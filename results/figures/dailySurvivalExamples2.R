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
tempReal<-tempData
for(r in 1:4){
  tempReal[,r]<-tempData[,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]
}

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
par(mfrow=c(2,2))
for(y in 2004:2015){
plot(westBrookYoy~date,data=surv[year(date)==y],type='l',col=palette()[1],ylim=c(0.967,1))
points(jimmyYoy~date,data=surv[year(date)==y],type='l',col=palette()[2])
points(mitchellYoy~date,data=surv[year(date)==y],type='l',col=palette()[3])
points(obearYoy~date,data=surv[year(date)==y],type='l',col=palette()[4])
}
for(y in 2004:2015){
plot(westBrookAdult~date,data=surv[year(date)==y],type='l',col=palette()[1],ylim=c(0.85,1))
points(jimmyAdult~date,data=surv[year(date)==y],type='l',col=palette()[2])
points(mitchellAdult~date,data=surv[year(date)==y],type='l',col=palette()[3])
points(obearAdult~date,data=surv[year(date)==y],type='l',col=palette()[4])
}

tiff.par("results/figures/dailySurvivalExamples.tif",mfcol=c(2,2),mar=c(2.5,3,0.5,1),lwd=2)
plot(westBrookYoy~as.POSIXct(date),data=surv[year(date)==2008&month(date) %in% c(2,3,4)],
     type='l',col=palette()[1],ylim=c(0.967,1),xlab="",ylab="",yaxt='n')
axis(2,seq(0.97,1,0.01))
title(ylab=bquote(Juvenile~Daily~phi),line=2)
points(jimmyYoy~as.POSIXct(date),data=surv[year(date)==2008&month(date) %in% c(2,3,4)],type='l',col=palette()[2])
points(mitchellYoy~as.POSIXct(date),data=surv[year(date)==2008&month(date) %in% c(2,3,4)],type='l',col=palette()[3])
points(obearYoy~as.POSIXct(date),data=surv[year(date)==2008&month(date) %in% c(2,3,4)],type='l',col=palette()[4])
legend(as.POSIXct("2008-03-01"),0.98,c("west brook","jimmy","mitchell","obear"),
       col=palette(),lty=1,bty='n')


# plot(westBrookAdult~date,data=surv[year(date)==2008&month(date) %in% c(2,3,4)],
#      type='l',col=palette()[1],ylim=c(0.967,1),xlab="",ylab="")
# title(ylab=bquote(Adult~Daily~phi),line=2)
# points(jimmyAdult~date,data=surv[year(date)==2008&month(date) %in% c(2,3,4)],type='l',col=palette()[2])
# points(mitchellAdult~date,data=surv[year(date)==2008&month(date) %in% c(2,3,4)],type='l',col=palette()[3])
# points(obearAdult~date,data=surv[year(date)==2008&month(date) %in% c(2,3,4)],type='l',col=palette()[4])

plot(rep(1,89)~seq.POSIXt(as.POSIXct("2008-02-01"),as.POSIXct("2008-04-30"),by="day"),
     ylim=c(0,25),axes=T,xlab="",pch=NA,ylab=bquote(Temperature~(degree*C)),bty='u')
# for(r in 1:4){
#   temps<-tempData[as.Date(rownames(tempData)) %in% seq.Date(as.Date("2008-02-01"),as.Date("2008-04-30"),by="day"),r]*
#     stds$temp$sdTemp[r]+stds$temp$meanTemp[r]
#   points(temps~seq.POSIXt(as.POSIXct("2008-02-01"),as.POSIXct("2008-05-01"),by="day"),type='l',col=palette()[r])
# }
temps<-rowMeans(tempReal[as.Date(rownames(tempReal)) %in% 
                    seq.Date(as.Date("2008-02-01"),as.Date("2008-04-30"),by="day"),])
points(temps~seq.POSIXt(as.POSIXct("2008-02-01"),as.POSIXct("2008-05-01"),by="day"),type='l',col='gray40')

par(new=T)
plot(NA,xlim=as.POSIXct(c("2008-02-01","2008-04-30")),ylim=c(-3,3),axes=F,xlab="",ylab="")
axis(4,seq(-3,3,1))
#mtext("Scaled log(Discharge)",4,line=1.5,las=0,cex=0.666)

# for(r in 1:4){
#   flows<-flowData[as.Date(rownames(flowData)) %in% seq.Date(as.Date("2008-02-01"),as.Date("2008-04-30"),by="day"),r]
#   points(flows~seq.POSIXt(as.POSIXct("2008-02-01"),as.POSIXct("2008-05-01"),by="day"),type='l',col=palette()[r],lty=2)
# }
flows<-rowMeans(flowData[as.Date(rownames(flowData)) %in% 
                           seq.Date(as.Date("2008-02-01"),as.Date("2008-04-30"),by="day"),])
points(flows~seq.POSIXt(as.POSIXct("2008-02-01"),as.POSIXct("2008-05-01"),by="day"),type='l',lty=3,col='gray40')

par(mar=c(2.5,2.5,0.5,2.5))
plot(westBrookYoy~as.POSIXct(date),data=surv[year(date)==2010&month(date) %in% c(7,8,9)],
     type='l',col=palette()[1],ylim=c(0.967,1),xlab="",ylab="",yaxt='n')
axis(2,seq(0.97,1,0.01))
points(jimmyYoy~as.POSIXct(date),data=surv[year(date)==2010&month(date) %in% c(7,8,9)],type='l',col=palette()[2])
points(mitchellYoy~as.POSIXct(date),data=surv[year(date)==2010&month(date) %in% c(7,8,9)],type='l',col=palette()[3])
points(obearYoy~as.POSIXct(date),data=surv[year(date)==2010&month(date) %in% c(7,8,9)],type='l',col=palette()[4])

# plot(westBrookAdult~date,data=surv[year(date)==2010&month(date) %in% c(7,8,9)],
#      type='l',col=palette()[1],ylim=c(0.967,1),xlab="",ylab="")
# points(jimmyAdult~date,data=surv[year(date)==2010&month(date) %in% c(7,8,9)],type='l',col=palette()[2])
# points(mitchellAdult~date,data=surv[year(date)==2010&month(date) %in% c(7,8,9)],type='l',col=palette()[3])
# points(obearAdult~date,data=surv[year(date)==2010&month(date) %in% c(7,8,9)],type='l',col=palette()[4])

plot(rep(1,92)~seq.POSIXt(as.POSIXct("2010-07-01"),as.POSIXct("2010-09-30"),by="day"),
     ylim=c(0,25),axes=T,xlab="",pch=NA,ylab="",bty='u')

temps<-rowMeans(tempReal[as.Date(rownames(tempReal)) %in% 
                          seq.Date(as.Date("2010-07-01"),as.Date("2010-09-30"),by="day"),])
points(temps~seq.POSIXt(as.POSIXct("2010-07-01"),as.POSIXct("2010-09-30"),by="day"),type='l',col='gray40')

par(new=T)
plot(NA,xlim=as.POSIXct(c("2010-07-01","2010-09-30")),ylim=c(-3,3),bty='u',axes=F,xlab="",ylab="",bty='u')
axis(4,seq(-3,3,1))
mtext("Scaled log(Discharge)",4,line=1.4,las=0,cex=0.83)

flows<-rowMeans(flowData[as.Date(rownames(flowData)) %in% 
                  seq.Date(as.Date("2010-07-01"),as.Date("2010-09-30"),by="day"),])
points(flows~seq.POSIXt(as.POSIXct("2010-07-01"),as.POSIXct("2010-09-30"),by="day"),
       type='l',lty=3,col='gray40')

dev.off()