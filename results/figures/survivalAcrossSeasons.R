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

coldTemp<-hotTemp<-matrix(nrow=nrow(tempData),ncol=ncol(tempData))
lowFlow<-highFlow<-matrix(nrow=nrow(flowData),ncol=ncol(flowData))
for(r in 1:4){
  hotTemp[,r]<-tempData[,r]>quantile(tempData[,r],0.95)
  coldTemp[,r]<-tempData[,r]<quantile(tempData[,r],0.05)
  lowFlow[,r]<-flowData[,r]<quantile(tempData[,r],0.05)
  highFlow[,r]<-flowData[,r]>quantile(tempData[,r],0.95)
}

tiff.par("results/figures/survivalAcrossSeasons.tif",mfcol=c(4,2),width=6.5,height=8,
         mar=c(2.5,2.5,1,0))
for(g in c("Yoy","Adult")){
  for(r in c("westBrook","jimmy","mitchell","obear")){
    plot(get(paste0(r,g))~yday(date),data=surv[year(date)>=2002],pch=19,col=gray(0.5,0.3),
         ylim=c(0.95,1),
         ylab="Daily Survival Probability",xlab="Day of Year",main=paste(r,g))
    points(get(paste0(r,g))~yday,data=byDay,type='l',lwd=2)
  }
}
dev.off()






