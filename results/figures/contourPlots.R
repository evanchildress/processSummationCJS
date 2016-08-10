library(plotHacks)
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
  collect(n=Inf) %>%
  data.table() %>%
  .[datetime>=min(coreData$detectionDate)&
      datetime<=max(coreData$detectionDate)] %>%
  .[,.(temperature=max(temperature)),by=.(date=as.Date(datetime),
                                          river)] %>%
  setkey(river,date)

if(exists("temp")){rm(temp)}
time<-tempData[river=="west brook",date] 

coreData[,time:=which(as.Date(detectionDate)==time),by=detectionDate]

meanLength<-coreData[,mean(observedLength),.(river,ageInSamples<=3)] %>%
            melt(id.vars=c("river","ageInSamples")) %>%
            acast(river~ageInSamples)

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

tempData<-tbl(conDplyr,"data_daily_temperature") %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[date>=min(coreData$detectionDate)&
      date<=max(coreData$detectionDate)] %>%
  .[,.(date=as.Date(date),river,temperature=daily_max_temp)]

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

out<-readRDS("results/phiChains.rds")

phi<-apply(out,c(2,3,4),mean)
rm(out)
gc()

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

# rm(coreData)
rm(jagsData)

flowData<-apply(flowData,2,scale2)
tempData<-apply(tempData,2,scale2)

flowRange<-apply(flowData,2,range)
tempRange<-apply(tempData,2,range)

gc()

flowSim<-tempSim<-array(NA,dim=c(100,4))
for(r in 1:4){
  flowSim[,r]<-seq(flowRange[1,r],flowRange[2,r],length.out=100)
  tempSim[,r]<-seq(tempRange[1,r],tempRange[2,r],length.out=100)
}

survSim<-array(NA,dim=c(100,100,4,2))
for(g in 1:2){
  for(r in 1:4){
    for(f in 1:100){
      survSim[f,,r,g]<-phi[1,r,g]+phi[2,r,g]*flowSim[f,r]+phi[3,r,g]*tempSim[,r]+
        phi[4,r,g]*tempSim[,r]*flowSim[f,r]
    }
  }
}

survSim<-1/(1+exp(-survSim))
library(grDevices)

colors<-colorRampPalette(c(colorRampPalette(c("yellow","green"))(5),"blue"))(500)

for(g in 1:2){
  tiff.par(paste0("results/figures/contourPlots",c("Yoy","Adult")[g],".tif"),
           mfrow=c(2,2),mar=c(1.5,1.5,1,1))
  for(r in 1:4){
    image(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r],length.out=100),
            survSim[,,r,g],
            zlim=c(0.91,1.0),
            col=colors,
            xlab="",ylab="",
            main=paste(c("west brook","wb jimmy","wb mitchell","wb obear")[r],c("yoy","adults")[g]))
    contour(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r],length.out=100),
            survSim[,,r,g],add=T,lwd=2,nlevels=5)
    
  }
  dev.off()
}


