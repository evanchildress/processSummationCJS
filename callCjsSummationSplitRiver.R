library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)

coreData<-createCoreData(sampleType="electrofishing") %>% 
  addTagProperties() %>%
  dplyr::filter(species=="bkt") %>%
  createCmrData() %>%
  fillSizeLocation() %>%
  addSampleProperties() %>%
  addEnvironmental(sampleFlow=T) %>%
  addKnownZ()

jagsData <- createJagsData(data.frame(coreData))

coreData<-data.table(coreData)

tempData<-tbl(conDplyr,"data_hourly_temperature") %>%
  # filter(river=="wb obear") %>%
  collect(n = Inf) %>%
  data.table() %>%
  .[datetime>=min(coreData$detectionDate)&
      datetime<=max(coreData$detectionDate)] %>%
  .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime),
                                          river)] %>%
  setkey(river,date)
# 
# load("C:/Users/Evan/Desktop/Conte/perform/data/wbTemps.rData")
# tempData<-temp %>%
#   .[datetime>=min(coreData$detectionDate)&
#     datetime<=max(coreData$detectionDate)] %>%
#   .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime),
#                                           river)] %>%
#   setkey(river,date)

# load("C:/Users/Evan/Desktop/Conte/perform/data/wbTemps.rData")
# tempData<-temp %>%
#   .[datetime>=min(coreData$detectionDate)&
#     datetime<=max(coreData$detectionDate)] %>%
#   .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime),
#                                           river)] %>%
#   setkey(river,date)

if(exists("temp")){rm(temp)}
time<-tempData[river=="west brook",date] 


coreData[,time:=which(as.Date(detectionDate)==time),by=detectionDate]

flowData<-tbl(conDplyr,"data_daily_discharge") %>%
  collect(n = Inf) %>%
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

# flowData<-tbl(conDplyr,"data_flow_extension") %>%
#   collect() %>%
#   data.table() %>%
#   .[date>=min(coreData$detectionDate)&
#       date<=max(coreData$detectionDate)] %>%
#   .[,discharge:=log(qPredicted+0.74)] %>%
#   .[,.(date=as.Date(date),river,discharge)]
# 
# flowData<-rbind(flowData,flowData,flowData,flowData) %>%
#   .[,river:=rep(c("west brook","wb jimmy","wb mitchell","wb obear"),each=nrow(flowData))] %>%
#   .[,river:=as.numeric(factor(river,
#                               levels=c("west brook",
#                                        "wb jimmy",
#                                        "wb mitchell",
#                                        "wb obear"),
#                               ordered=T))] %>%
#   melt(id.vars=c("date","river")) %>%
#   acast(date~river)


# tempData<-tbl(conDplyr,"data_daily_temperature") %>%
#   # filter(river=="wb obear") %>%
#   collect(n = Inf) %>%
#   data.table() %>%
#   .[date>=min(coreData$detectionDate)&
#     date<=max(coreData$detectionDate)] %>%
#   .[,.(date=as.Date(date),river,temperature=daily_max_temp)]

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

tempData<-tempData[,c(1,1,1,2,3,4)]
flowData<-flowData[,c(1,1,1,2,3,4)]

coreData[,riverSplit:=as.numeric(factor(river,
                                           levels=c("west brook","wb2",
                                                    "wb3","wb jimmy",
                                                    "wb mitchell","wb obear"),
                                           ordered=T))] %>%
       .[river=="west brook",riverSplit:=as.numeric((section>=1)+
                                                      (section>=16)+
                                                      (section>=32)
                                                    )]
  
jagsData$riverDATA<-coreData$riverSplit
jagsData$nRivers<-6

jagsData$stageDATA<-as.numeric(coreData$ageInSamples>3)+1
jagsData$flowForP<-scale(coreData$flowForP)[,1]
jagsData$z[jagsData$z==2]<-0
# jagsData$lengthDATA<-coreData %>% 
#                       group_by(river) %>%
#                       transmute(length=scale(observedLength)[,1]) %>%
#                       ungroup() %>%
#                       data.table() %>%
#                       .[,length]
jagsData$tempDATA<-tempData
jagsData$flowDATA<-apply(flowData,2,scale2)
jagsData$time<-coreData$time
jagsData$nTimes<-length(time)

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
nb <- 20000
ni <- 23000
nt <- 3
nc <- 3

varsToMonitor<-c('pBeta','phiBeta')

gc()

  out <- jags(
    data=jagsData,
    inits=inits,
    model = "CjsProcessSummation.R",
    parameters.to.save = varsToMonitor,
    n.adapt=na,
    n.chains=nc,
    n.iter = ni,
    n.thin = nt,
    n.burnin=nb,
    parallel=T,
    codaOnly=c("pEps","phiEps"))

saveRDS(out,"processSummationOut.rds")
