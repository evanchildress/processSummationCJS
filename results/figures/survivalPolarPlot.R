library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
library(plotHacks)
library(plotrix)

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
surv[,yday:=yday(date)]
surv[yday==366,yday:=365]

setnames(surv,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date","yday"))

byDay<-surv[,.(westBrookYoy=mean(westBrookYoy),
               jimmyYoy=mean(jimmyYoy),
               mitchellYoy=mean(mitchellYoy),
               obearYoy=mean(obearYoy),
               westBrookAdult=mean(westBrookAdult),
               jimmyAdult=mean(jimmyAdult),
               mitchellAdult=mean(mitchellAdult),
               obearAdult=mean(obearAdult)),
            by=yday] %>%
  setkey(yday)

coldTemp<-hotTemp<-matrix(nrow=nrow(tempData),ncol=ncol(tempData))
lowFlow<-highFlow<-matrix(nrow=nrow(flowData),ncol=ncol(flowData))
for(r in 1:4){
  hotTemp[,r]<-tempData[,r]>quantile(tempData[,r],0.95)
  coldTemp[,r]<-tempData[,r]<quantile(tempData[,r],0.05)
  lowFlow[,r]<-flowData[,r]<quantile(tempData[,r],0.05)
  highFlow[,r]<-flowData[,r]>quantile(tempData[,r],0.95)
}

marg<-rep(1.5,4)

labPos<-yday(seq.Date(as.Date("2000-01-01"),as.Date("2000-12-01"),"months"))/365*360
labs<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

surv<-surv[year(date)>=2002]
tiff.par("results/figures/survivalPolarPlot.tif",mfcol=c(4,2),width=5.5,height=8,
         mar=c(2.5,3,1,0))
for(g in c("Yoy","Adult")){
  for(r in c("westBrook","jimmy","mitchell","obear")){
#     polar.plot(surv[[paste0(r,g)]],
#          polar.pos=I((yday(surv$date)-1)/365*360),rp.type="l",
#          mar=c(0.5,0.5,0.5,0.5),line.col=rgb(1,0,0,0.4),lwd=3)
    polar.plot(lengths=surv[[paste0(r,g)]],
               polar.pos=surv$yday/365*360,rp.type="s",
               mar=marg,boxed.radial=F,
               point.symbols=19,point.col=gray(0.5,0.1),
               radial.lim=c(0.95,1),start=90,clockwise=T,
               label.pos=labPos,labels=labs,label.prop=1.2,
               grid.col="gray80",radial.labels=c("","0.96","","0.98","","1"))
    polar.plot(byDay[[paste0(r,g)]],
               polar.pos=byDay$yday/365*360,rp.type="l",boxed.radial=F,
               line.col='black',lwd=2,add=T,radial.lim=c(0.95,1),
               mar=marg,start=90,clockwise=T)
#     p <- plot_ly(r= ~surv[[paste0(r,g)]],
#                  t= ~I((yday(surv$date)-1)/365*360),
#                  color='blue', type = "scatter",mode="lines")
#     layout(p, title = "Mic Patterns", orientation = -90)
  }
}
dev.off()

