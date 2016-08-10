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

phiBeta<-readRDS("results/phiChains.rds")

phi<-apply(phiBeta,c(2,3,4),mean)
# out<-readRDS("results/processSummationOut.rds")
# 
# phi<-out$mean$phiBeta

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

flowData<-apply(flowData,2,scale2)
tempData<-apply(tempData,2,scale2)

flowRange<-apply(flowData,2,range)
tempRange<-apply(tempData,2,range)

survFlow<-survTemp<-surv<-array(NA,dim=c(dim(flowData),2))

for(g in 1:2){
  tiff.par(paste0("results/figures/flowEffectWithError",c("Yoy","Adult")[g],".tif"),
           mfrow=c(2,2),mar=c(1.5,1.5,1,1))
  for(r in 1:4){
    plot(NA,xlim=range(flowRange),ylim=c(0.8,1))
    for(i in 1:500){
      iter<-sample(1:dim(phiBeta)[1],1)
      surv<-phiBeta[iter,1,r,g]+
            phiBeta[iter,2,r,g]*seq(flowRange[1,r],flowRange[2,r],length.out=100)
      surv<-1/(1+exp(-surv))
      points(surv~seq(flowRange[1,r],flowRange[2,r],length.out=100),type='l',col=palette()[r])
    }
  }
  dev.off()
}

for(g in 1:2){
  tiff.par(paste0("results/figures/tempEffectWithError",c("Yoy","Adult")[g],".tif"),
           mfrow=c(2,2),mar=c(1.5,1.5,1,1))
  for(r in 1:4){
    plot(NA,xlim=range(0,25),ylim=c(0.8,1))
    for(i in 1:500){
      iter<-sample(1:dim(phiBeta)[1],1)
      surv<-phiBeta[iter,1,r,g]+
        phiBeta[iter,3,r,g]*seq(tempRange[1,r],tempRange[2,r],length.out=100)
      surv<-1/(1+exp(-surv))
      points(surv~seq(flowRange[1,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r],
                      flowRange[2,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r],length.out=100),
             type='l',col=palette()[r])
    }
  }
  dev.off()
}

surv<-array(dim=c(dim(tempData),2))
for(r in 1:4){
  for(g in 1:2){
    surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[2:nrow(flowData),r]+
      phi[3,r,g]*tempData[,r]+phi[4,r,g]*tempData[,r]*flowData[2:nrow(flowData),r]
  }
}
surv<-plogis(surv)
yoySurv<-data.table(surv[,,1]) %>%
  .[,date:=as.Date(rownames(tempData))]
adultSurv<-data.table(surv[,,2]) %>%
  .[,date:=as.Date(rownames(tempData))]


for(y in unique(year(adultSurv$date))){
n<-nrow(adultSurv[year(date)==y])
highs<-lows<-vector(mode="double",length=length(2:(n-1)))

for(i in 2:(n-1)){
  highs[i-1]<-adultSurv[year(date)==y][order(V3)][1:i,prod(V3)]
  lows[i-1]<-adultSurv[year(date)==y][order(V3)][(i+1):n,prod(V3)]
}

print(which.min(abs(highs-lows))/n)
}

plot(V1~date,data=yoySurv,type='l',col="black",ylim=c(0.90,1))
points(V2~date,data=yoySurv,type='l',col=palette()[2])
points(V3~date,data=yoySurv,type='l',col=palette()[3])
points(V4~date,data=yoySurv,type='l',col=palette()[4])


plot(V3~date,data=adultSurv[year(date)==2002],type='l')
par(new=T)
plot(V3~date,data=flow[year(date)==2002],type='l',col='blue')
par(new=T)
plot(V3~date,data=temp[year(date)==2002],type='l',col='red')
