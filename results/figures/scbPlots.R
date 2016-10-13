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

out<-readRDS("results/processSummationOutOld.rds")

phi<-out$mean$phiBeta

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

flowSim<-tempSim<-array(NA,dim=c(100,4))
for(r in 1:4){
  flowSim[,r]<-seq(flowRange[1,r],flowRange[2,r],length.out=100)
  tempSim[,r]<-seq(tempRange[1,r],tempRange[2,r],length.out=100)
}


survFlow<-survTemp<-surv<-array(NA,dim=c(dim(flowData),2))
for(g in 1:2){
  for(r in 1:4){
    survFlow[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]#+phi[3,r,g]*flowData[,r]^2+
    #phi[4,r,g]*mean(tempData[,r])+phi[5,r,g]*mean(tempData[,r])^2
    survTemp[,r,g]<-phi[1,r,g]+phi[3,r,g]*tempData[,r]+#+phi[5,r,g]*tempData[,r]^2+
    phi[2,r,g]*quantile(flowData[,r],0.55)+phi[4,r,g]*quantile(flowData[,r],0.55)*tempData[,r]
    surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+phi[4,r,g]*tempData[,r]*flowData[,r]
    #phi[5,r,g]*tempData[,r]^2
    #phi[5,r,g]*flowData[,r]*tempData[,r]
  }
}
survFlow<-melt(1/(1+exp(-survFlow))) %>% dcast(Var1~Var3+Var2) %>% data.table()
survTemp<-melt(1/(1+exp(-survTemp))) %>% dcast(Var1~Var3+Var2) %>% data.table()
surv<-melt(1/(1+exp(-surv))) %>% dcast(Var1~Var3+Var2) %>% data.table()

survFlow[,date:=as.Date(rownames(flowData))]
survTemp[,date:=as.Date(rownames(flowData))]
surv[,date:=as.Date(rownames(flowData))]

setnames(survFlow,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))
setnames(survTemp,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))
setnames(surv,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))

palette(c("#BEAED4","#7FC97F","#FFFF99"))

tiff.par("results/figures/responseDiffExamples.tif",
         mfrow=c(1,2),bg='black',col='gray90',lwd=2,
         width=7,height=4,mar=c(1.5,3.3,2,0))
start<-as.Date("2004-03-01")
end<-as.Date("2004-03-31")

plot(westBrookAdult~date,data=surv[date>start&date<=end],
     type='l',col=palette()[1],ylim=c(0.98,1.0),yaxt='n',
     ylab="",xlab="",xlim=c(start,end+5))
title(ylab="Daily Adult Survival",line=2.3)
legend(as.Date("2004-03-05"),0.99,c("Mainstem","Large Trib","Small Trib")
       ,col=palette()[1:3],lty=1,lwd=2,bty='n')
axis(2,seq(0.98,1,0.01))
points(jimmyAdult~date,data=surv[date>start&date<=end],
       type='l',col=palette()[2])
points(mitchellAdult~date,data=surv[date>start&date<=end],
       type='l',col=palette()[3])
text(x=end+3,y=surv[date==end,westBrookAdult],
     round(surv[date>start&date<=end,prod(westBrookAdult)],2),
     col=palette()[1])
text(end+3,surv[date==end,jimmyAdult],
     round(surv[date>start&date<=end,prod(jimmyAdult)],2),
     col=palette()[2])
text(end+3,surv[date==end,mitchellAdult],
     round(surv[date>start&date<=end,prod(mitchellAdult)],2),
     col=palette()[3])


survFlow[date>start&date<end,.(prod(westBrookAdult),
                           prod(jimmyAdult),
                           prod(mitchellAdult))]


start<-as.Date("2005-09-01")
end<-as.Date("2005-10-01")

plot(westBrookAdult~date,data=surv[date>start&date<=end],
     type='l',col=palette()[1],ylim=c(0.98,1.0),
     ylab="",yaxt='n',xlim=c(start,end+5),bty='n')
#title(ylab="Daily Adult Survival",line=2.3)
#axis(2,seq(0.98,1,0.01))
points(jimmyAdult~date,data=surv[date>start&date<=end],
       type='l',col=palette()[2])
points(mitchellAdult~date,data=surv[date>start&date<=end],
       type='l',col=palette()[3])

text(x=end+3,y=surv[date==end,westBrookAdult],
     round(surv[date>start&date<=end,prod(westBrookAdult)],2),
     col=palette()[1])
text(end+3,surv[date==end,jimmyAdult],
     round(surv[date>start&date<=end,prod(jimmyAdult)],2),
     col=palette()[2])
text(end+3,surv[date==end,mitchellAdult],
     round(surv[date>start&date<=end,prod(mitchellAdult)],2),
     col=palette()[3])



dev.off()


tiff.par("results/figures/flowEffectCompareStreams.tif",
         mfrow=c(1,2),bg='black',col='gray90',lwd=2,cex=1.3,
         mar=c(2.5,3.3,0,0),width=7,height=4)
plot(survFlow$westBrookYoy~flowData[,1],col=palette()[1],pch=19,ylim=c(0.91,1),
     ylab="",xlab="Scaled log(Discharge)",cex=0.75)
title(ylab="Juvenile Daily Survival",line=2.3)
points(survFlow$jimmyYoy~flowData[,2],col=palette()[2],pch=19,cex=0.75)
points(survFlow$mitchellYoy~flowData[,3],col=palette()[3],pch=19,cex=0.75)

legend(-1,0.97,c("Mainstem","Large Trib","Small Trib"),pch=19,col=palette()[1:3],
       bty='n')

plot(survFlow$westBrookAdult~flowData[,1],col=palette()[1],ylim=c(0.91,1),pch=19,
     ylab="",xlab="Scaled log(Discharge)",cex=0.75)
title(ylab="Adult Daily Survival",line=2.3)
points(survFlow$jimmyAdult~flowData[,2],col=palette()[2],pch=19,cex=0.75)
points(survFlow$mitchellAdult~flowData[,3],col=palette()[3],pch=19,cex=0.75)
dev.off()


tiff.par("results/figures/tempEffectCompareStreams.tif",
         mfrow=c(1,2),bg='black',col='gray90',lwd=2,cex=1.3,
         mar=c(2.5,3.3,0,0),width=7,height=4)
plot(survTemp$westBrookYoy~I(tempData[,1]*stds$temp$sdTemp[1]+stds$temp$meanTemp[1]),
     col=palette()[1],pch=19,ylim=c(0.91,1),
     ylab="",xlab=bquote(Temperature~(degree*C)),cex=0.75,xlim=c(0,25))
title(ylab="Juvenile Daily Survival",line=2.3)
points(survTemp$jimmyYoy~I(tempData[,2]*stds$temp$sdTemp[2]+stds$temp$meanTemp[2]),
       col=palette()[2],pch=19,cex=0.75)
points(survTemp$mitchellYoy~I(tempData[,3]*stds$temp$sdTemp[3]+stds$temp$meanTemp[3]),
       col=palette()[3],pch=19,cex=0.75)

legend(-1,0.97,c("Mainstem","Large Trib","Small Trib"),pch=19,col=palette()[1:3],
       bty='n')

plot(survTemp$westBrookAdult~I(tempData[,1]*stds$temp$sdTemp[1]+stds$temp$meanTemp[1]),
     col=palette()[1],ylim=c(0.91,1),pch=19,xlim=c(0,25),
     ylab="",xlab=bquote(Temperature~(degree*C)),cex=0.75)
title(ylab="Adult Daily Survival",line=2.3)
points(survTemp$jimmyAdult~I(tempData[,2]*stds$temp$sdTemp[2]+stds$temp$meanTemp[2]),
       col=palette()[2],pch=19,cex=0.75)
points(survTemp$mitchellAdult~I(tempData[,3]*stds$temp$sdTemp[3]+stds$temp$meanTemp[3]),
       col=palette()[3],pch=19,cex=0.75)
dev.off()