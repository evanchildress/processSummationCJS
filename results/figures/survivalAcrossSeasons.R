library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
library(plotHacks)

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")
out<-readRDS("results/processSummationOut.rds")
phi<-out$mean$phiBeta

flowData<-jagsData$flowDATA
tempData<-jagsData$tempDATA

surv<-array(dim=c(nrow(flowData),4,2))

for(g in 1:2){
  for(r in 1:4){
    surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+phi[4,r,g]*tempData[,r]*flowData[,r]
  }
}
surv<-melt(1/(1+exp(-surv))) %>% dcast(Var1~Var3+Var2) %>% data.table()

surv[,date:=as.Date(rownames(flowData))]

setnames(surv,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))

byDay<-surv[,.(westBrookYoy=mean(westBrookYoy),
               jimmyYoy=mean(jimmyYoy),
               mitchellYoy=mean(mitchellYoy),
               obearYoy=mean(obearYoy),
               westBrookAdult=mean(westBrookAdult),
               jimmyAdult=mean(jimmyAdult),
               mitchellAdult=mean(mitchellAdult),
               obearAdult=mean(obearAdult)),
            by=yday(date)] %>%
  setkey(yday)

byDayMedian<-surv[,.(westBrookYoy=median(westBrookYoy),
               jimmyYoy=median(jimmyYoy),
               mitchellYoy=median(mitchellYoy),
               obearYoy=median(obearYoy),
               westBrookAdult=median(westBrookAdult),
               jimmyAdult=median(jimmyAdult),
               mitchellAdult=median(mitchellAdult),
               obearAdult=median(obearAdult)),
            by=yday(date)] %>%
  setkey(yday)

coldTemp<-hotTemp<-matrix(nrow=nrow(tempData),ncol=ncol(tempData))
lowFlow<-highFlow<-matrix(nrow=nrow(flowData),ncol=ncol(flowData))
for(r in 1:4){
  hotTemp[,r]<-tempData[,r]>quantile(tempData[,r],0.95)
  coldTemp[,r]<-tempData[,r]<quantile(tempData[,r],0.05)
  lowFlow[,r]<-flowData[,r]<quantile(tempData[,r],0.05)
  highFlow[,r]<-flowData[,r]>quantile(tempData[,r],0.95)
}

labPos<-yday(seq.Date(as.Date("2000-01-01"),as.Date("2000-12-01"),"months"))
labs<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

tiff.par("results/figures/survivalAcrossSeasonsMedian.tif",mfcol=c(4,2),width=6.5,height=8,
         mar=c(2.5,3,1,0))
for(g in c("Yoy","Adult")){
  for(r in c("westBrook","jimmy","mitchell","obear")){
    plot(get(paste0(r,g))~yday(date),data=surv[year(date)>=2002],pch=19,col=gray(0.5,0.2),
         ylim=c(0.95,1),xaxt='n',
         ylab="",xlab="",main=paste(r,g))
    axis(1,labPos,labs)
    title(ylab="Daily Survival Probability",line=2.2)
    #points(get(paste0(r,g))~yday,data=byDay,type='l',lwd=2)
    points(get(paste0(r,g))~yday,data=byDayMedian,type='l',lwd=2)
  }
}
dev.off()

tiff.par("results/figures/survivalAcrossSeasonsMean.tif",mfcol=c(4,2),width=6.5,height=8,
         mar=c(2.5,3,1,0))
for(g in c("Yoy","Adult")){
  for(r in c("westBrook","jimmy","mitchell","obear")){
    plot(get(paste0(r,g))~yday(date),data=surv[year(date)>=2002],pch=19,col=gray(0.5,0.2),
         ylim=c(0.95,1),xaxt='n',
         ylab="",xlab="",main=paste(r,g))
    axis(1,labPos,labs)
    title(ylab="Daily Survival Probability",line=2.2)
    points(get(paste0(r,g))~yday,data=byDay,type='l',lwd=2)
    # points(get(paste0(r,g))~yday,data=byDayMedian,type='l',lwd=2)
  }
}
dev.off()




