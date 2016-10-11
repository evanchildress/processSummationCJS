library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
reconnect()
# coreData<-createCoreData(sampleType="electrofishing",columnsToAdd="pass") %>%
#   addTagProperties() %>%
#   dplyr::filter(species=="bkt") %>%
#   createCmrData() %>%
#   fillSizeLocation() %>%
#   addSampleProperties() %>%
#   addEnvironmental(sampleFlow=T) %>%
#   addKnownZ()

coreData<-readRDS("~/perform/vignettes/westBrook/results/corebkt.rds") %>%
  .[,trueObservedLength:=observedLength] %>%
  .[is.na(observedLength),observedLength:=predictedLength]
  
nPasses<-tbl(conDplyr,"data_tagged_captures") %>%
         filter(drainage=="west") %>%
         select(river,sample_number,pass) %>%
         distinct() %>%
         collect() %>%
         data.table() %>%
         .[,.(nPasses=length(unique(pass))),by=.(river,sample_number)] %>%
         setnames("sample_number","sampleNumber") %>%
         setkey(river,sampleNumber)

setkey(coreData,river,sampleNumber)
coreData<-nPasses[coreData]
setkey(coreData,tag,detectionDate)
coreData[is.na(nPasses)&proportionSampled==0,nPasses:=1] #when proportionSampled==0 no pass info
#5 of 13504 fish don't have length on first obs, so grab the mean for that age/sample/river 
getFirstLength<-function(riv,age,sampName){
    coreData[river==riv&ageInSamples %in% age&sampleName %in% sampName,mean(observedLength,na.rm=T),
         by=.(ageInSamples,sampleName)]$V1
  }

coreData[is.na(observedLength),observedLength:=getFirstLength(river,ageInSamples,sampleName),
         by=river]

coreData<-coreData %>%  
  data.frame() %>%
  addEnvironmental(sampleFlow=T)

jagsData <- createJagsData(coreData)

coreData<-data.table(coreData)

tempData<-tbl(conDplyr,"data_hourly_temperature") %>%
  # filter(river=="wb obear") %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[datetime>=min(coreData$detectionDate)&
      datetime<=max(coreData$detectionDate)] %>%
  .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime),
                                          river)] %>%
  setkey(river,date)

if(exists("temp")){rm(temp)}
time<-tempData[river=="west brook",date] 

coreData[,time:=which(get('time',envir=.GlobalEnv)==as.Date(detectionDate)),by=detectionDate]

flowData<-tbl(conDplyr,"data_daily_discharge") %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[date>=as.Date(min(coreData$detectionDate))&
      date<=as.Date(max(coreData$detectionDate))] %>%
  .[,discharge:=log(discharge)] %>%
  .[,.(date,river,discharge)] %>%
  .[,river:=as.numeric(factor(river,
                                 levels=c("west brook",
                                          "wb jimmy",
                                          "wb mitchell",
                                          "wb obear"),
                                 ordered=T))] %>%
  melt(id.vars=c("date","river")) %>%
  acast(date~river)

# flowData[,c(2,3,4)]<-flowData[,1]

tempData<-tempData  %>%
  .[,.(date,river,temperature)] %>%
  .[,river:=as.numeric(factor(river,
                              levels=c("west brook",
                                       "wb jimmy",
                                       "wb mitchell",
                                       "wb obear"),
                              ordered=T))] %>%
  melt(id.vars=c("date","river")) %>%
  acast(date~river)

scale2<-function(x){
  return(scale(x)[,1])
}

coreData[,nTimes:=c(NA,diff(time)+1),by=tag]

jagsData$stageDATA<-as.numeric(coreData$ageInSamples>3)+1
jagsData$flowForP<-scale(coreData$flowForP)[,1]
jagsData$z[jagsData$z==2]<-0
jagsData$lengthDATA<-coreData %>% 
                      group_by(river) %>%
                      transmute(length=scale(observedLength)[,1]) %>%
                      ungroup() %>%
                      data.table() %>%
                      .[,length]
jagsData$tempDATA<-apply(tempData,2,scale2)
jagsData$flowDATA<-apply(flowData,2,scale2)
jagsData$time<-coreData$time
jagsData$nTimes<-length(time)
jagsData$nTimesByRow<-coreData$nTimes
jagsData$sample<-coreData$sampleIndex
jagsData$nSamples<-max(coreData$sampleIndex)

timesByRow<-array(NA,dim=c(nrow(coreData),max(coreData$nTimes,na.rm=T)))
for(i in 1:nrow(coreData)){
  if(is.na(coreData$nTimes[i])){next}
  timesByRow[i,1:coreData$nTimes[i]]<-coreData$time[i-1]:coreData$time[i]
}
jagsData$timesByRow<-timesByRow
jagsData$nPasses<-coreData$nPasses

stds<-list(length=coreData %>% 
             group_by(river) %>%
             summarize(meanLength=mean(observedLength,na.rm=T),
                       sdLength=sd(observedLength,na.rm=T)),
           flowForP=coreData %>%
             summarize(meanFlow=mean(flowForP),
                       sdFlow=sd(flowForP)),
           flow=list(meanFlow=apply(flowData,2,mean),
                     sdFlow=apply(flowData,2,sd)),
           temp=list(meanTemp=apply(tempData,2,mean),
                     sdTemp=apply(tempData,2,sd)))

saveRDS(stds,"results/summationStandards.rds")

zInit<-jagsData$z+1
zInit[is.na(zInit)]<-0
zInit[jagsData$firstObsRows]<-NA
zInit[zInit %in% c(1,2)]<-NA
inits<- function(){
  list(z = zInit)      
}



# MCMC settings
na <- 500
nb <- 4000
ni <- 7000
nt <- 3
nc <- 3

varsToMonitor<-c('pBeta','phiBeta','phiSigma','phiEps')

gc()

  out <- jags(
    data=jagsData,
    inits=inits,
    model = "CjsProcessSummation.R",
    parameters.to.save = varsToMonitor,
    n.chains=nc,
    n.iter = ni,
    n.thin = nt,
    n.burnin=nb,
    parallel=T,
    codaOnly=c("phiEps"))

saveRDS(out,"results/processSummationOut.rds")
saveRDS(coreData,"results/processSummationCoreData.rds")
saveRDS(jagsData,"results/processSummationJagsData.rds")
