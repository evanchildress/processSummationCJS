library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
library(plotHacks)

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")
out<-readRDS("processSummationOut.rds")
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

cv<-function(x,na.rm=T){
  sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
}

extInf<-surv[,.(westBrookYoy=cv(westBrookYoy),
                jimmyYoy=cv(jimmyYoy),
                mitchellYoy=cv(mitchellYoy),
                obearYoy=cv(obearYoy),
                westBrookAdult=cv(westBrookAdult),
                jimmyAdult=cv(jimmyAdult),
                mitchellAdult=cv(mitchellAdult),
                obearAdult=cv(obearAdult)),
             ,by=year(date)]

palette(c("gray","red","green","blue"))

tiff.par("results/figures/survCV.tif",mfcol=c(1,2),width=6.5,height=4,
         mar=c(2.5,4,1,0),mgp=c(3,0.5,0))
for(g in c("Yoy","Adult")){
  plot(NA,ylim=c(0,0.03),xlim=c(2002,2015),
       ylab="CV of Daily Survival",xlab="",main=g)
  if(g=="Yoy"){
    legend(2005,0.15,c("west brook","jimmy","mitchell","obear"),
           col=palette()[1:4],lty=1,lwd=2,bty='n')
  }
  for(r in c("westBrook","jimmy","mitchell","obear")){
    points(get(paste0(r,g))~year,data=extInf[year>=2002],type='l',lwd=2,
           col=palette()[which(r==c("westBrook","jimmy","mitchell","obear"))])
  }
}
dev.off()