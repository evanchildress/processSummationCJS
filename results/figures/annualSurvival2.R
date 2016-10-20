library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
library(plotHacks)

colors<-c(gray(0.1,.1),rgb(1,0,0,0.1),rgb(0,1,0,0.1),rgb(0,0,1,0.1))
colors<-c("black","red","green","blue")

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")
out<-readRDS("results/processSummationOut.rds")

flowData<-jagsData$flowDATA
tempData<-jagsData$tempDATA

coreData[,stage:=as.numeric(ageInSamples>3)+1]

meanLength<-coreData[enc==1,mean(observedLength),by=.(river,stage)] %>%
  .[,river2:=match(river,c("west brook","wb jimmy","wb mitchell","wb obear"))] %>%
  .[,.(river2,stage,V1)] %>%
  .[,V1:=(V1-stds$length$meanLength[river2])/stds$length$sdLength[river2]] %>%
  melt(id.vars=c("river2","stage")) %>%
  acast(river2~stage)

iter<-sample(1:out$mcmc.info$n.samples,10)
iter<-1:out$mcmc.info$n.samples

surv<-array(dim=c(length(iter),nrow(flowData),4,2))

for(it in 1:length(iter)){
  cat("\n",it)
  i<-iter[it]
  
  phi<-out$sims.list$phiBeta[i,,,]

  
  for(g in 1:2){
    for(r in 1:4){
      surv[it,,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+phi[4,r,g]*tempData[,r]*flowData[,r]+
        phi[5,r,g]*meanLength[r,g]
    }
  }
}

surv<-1/(1+exp(-surv))
survUpper<-apply(surv,c(2,3,4),quantile,probs=0.975)
survLower<-apply(surv,c(2,3,4),quantile,probs=0.025)
survMean<-apply(surv,c(2,3,4),mean)

surv<-melt(survMean) %>% dcast(Var1~Var3+Var2) %>% data.table()
surv[,date:=as.Date(rownames(flowData))]
surv[,survivalYear:=year(date)+ifelse(month(date)<6,-1,0)]
setnames(surv,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date","survivalYear"))
annualSurv<-surv[,.(westBrookYoy=plogis(qlogis(prod(westBrookYoy))+phi[5,1,1]*meanLength[1,1]),
                    jimmyYoy=plogis(qlogis(prod(jimmyYoy))+phi[5,2,1]*meanLength[2,1]),
                    mitchellYoy=plogis(qlogis(prod(mitchellYoy))+phi[5,3,1]*meanLength[3,1]),
                    obearYoy=plogis(qlogis(prod(obearYoy))+phi[5,4,1]*meanLength[4,1]),
                    westBrookAdult=plogis(qlogis(prod(westBrookAdult))+phi[5,1,1]*meanLength[1,2]),
                    jimmyAdult=plogis(qlogis(prod(jimmyAdult))+phi[5,2,1]*meanLength[2,2]),
                    mitchellAdult=plogis(qlogis(prod(mitchellAdult))+phi[5,3,1]*meanLength[3,2]),
                    obearAdult=plogis(qlogis(prod(obearAdult))+phi[5,4,1]*meanLength[4,2])),
                 by=survivalYear]

survUpper<-melt(survUpper) %>% dcast(Var1~Var3+Var2) %>% data.table()
survUpper[,date:=as.Date(rownames(flowData))]
survUpper[,survivalYear:=year(date)+ifelse(month(date)<6,-1,0)]
setnames(survUpper,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date","survivalYear"))
annualSurvUpper<-survUpper[,.(westBrookYoy=plogis(qlogis(prod(westBrookYoy))+phi[5,1,1]*meanLength[1,1]),
                    jimmyYoy=plogis(qlogis(prod(jimmyYoy))+phi[5,2,1]*meanLength[2,1]),
                    mitchellYoy=plogis(qlogis(prod(mitchellYoy))+phi[5,3,1]*meanLength[3,1]),
                    obearYoy=plogis(qlogis(prod(obearYoy))+phi[5,4,1]*meanLength[4,1]),
                    westBrookAdult=plogis(qlogis(prod(westBrookAdult))+phi[5,1,1]*meanLength[1,2]),
                    jimmyAdult=plogis(qlogis(prod(jimmyAdult))+phi[5,2,1]*meanLength[2,2]),
                    mitchellAdult=plogis(qlogis(prod(mitchellAdult))+phi[5,3,1]*meanLength[3,2]),
                    obearAdult=plogis(qlogis(prod(obearAdult))+phi[5,4,1]*meanLength[4,2])),
                 by=survivalYear]

survLower<-melt(survLower) %>% dcast(Var1~Var3+Var2) %>% data.table()
survLower[,date:=as.Date(rownames(flowData))]
survLower[,survivalYear:=year(date)+ifelse(month(date)<6,-1,0)]
setnames(survLower,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                     "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date","survivalYear"))
annualSurvLower<-survLower[,.(westBrookYoy=plogis(qlogis(prod(westBrookYoy))+phi[5,1,1]*meanLength[1,1]),
                              jimmyYoy=plogis(qlogis(prod(jimmyYoy))+phi[5,2,1]*meanLength[2,1]),
                              mitchellYoy=plogis(qlogis(prod(mitchellYoy))+phi[5,3,1]*meanLength[3,1]),
                              obearYoy=plogis(qlogis(prod(obearYoy))+phi[5,4,1]*meanLength[4,1]),
                              westBrookAdult=plogis(qlogis(prod(westBrookAdult))+phi[5,1,1]*meanLength[1,2]),
                              jimmyAdult=plogis(qlogis(prod(jimmyAdult))+phi[5,2,1]*meanLength[2,2]),
                              mitchellAdult=plogis(qlogis(prod(mitchellAdult))+phi[5,3,1]*meanLength[3,2]),
                              obearAdult=plogis(qlogis(prod(obearAdult))+phi[5,4,1]*meanLength[4,2])),
                           by=survivalYear]

tiff.par("results/figures/annualSurvival2.tif",mfrow=c(1,2),
         mgp=c(1.7,0.5,0),width=6.5,height=4,mar=c(1.5,2.5,0,0.4))

for(stage in c("Yoy","Adult")){
  plot(NA,xlim=c(2002,2014),ylim=c(0,1),xlab="",
       ylab=paste(ifelse(stage=="Yoy","Juvenile","Adult"),"Annual Survival Probability"))
  panelLabel(c("a","b")[which(stage==c("Yoy","Adult"))])
  
  for(riv in c("westBrook","jimmy","mitchell","obear")){
#     g<-which(stage==c("Yoy","Adult"))
#     r<-which(riv==c("westBrook","jimmy","mitchell","obear"))
    points(get(paste0(riv,stage))~survivalYear,data=annualSurv[survivalYear>=2002&survivalYear<2015],type='l',
      col=colors[which(riv==c("westBrook","jimmy","mitchell","obear"))],lwd=2)
    points(get(paste0(riv,stage))~survivalYear,data=annualSurvUpper[survivalYear>=2002&survivalYear<2015],type='l',
           col=colors[which(riv==c("westBrook","jimmy","mitchell","obear"))],lty=2,lwd=0.5)
    points(get(paste0(riv,stage))~survivalYear,data=annualSurvLower[survivalYear>=2002&survivalYear<2015],type='l',
           col=colors[which(riv==c("westBrook","jimmy","mitchell","obear"))],lty=2,lwd=0.5)
  }
}
legend(2004.5,1.03,c("West Brook","Open Large","Open Small","Isolated Small"),
       col=colors[1:4],lty=1,lwd=2,bty='n')
dev.off()

streamSize<-tbl(conDplyr,"data_daily_discharge") %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[date>=as.Date(min(coreData$detectionDate))&
      date<=as.Date(max(coreData$detectionDate))] %>%
  .[,.(medianQ=median(discharge)),by=river] %>%
  setkey(river)

cv<-function(x,na.rm=F) sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
annualSurv<-annualSurv[survivalYear %in% 2002:2014]
annualCv<-annualSurv[,.(westBrookYoy=cv(westBrookYoy),
                        jimmyYoy=cv(jimmyYoy),
                        mitchellYoy=cv(mitchellYoy),
                        obearYoy=cv(obearYoy),
                        westBrookAdult=cv(westBrookAdult),
                        jimmyAdult=cv(jimmyAdult),
                        mitchellAdult=cv(mitchellAdult),
                        obearAdult=cv(obearAdult))]

surv<-surv[survivalYear %in% 2002:2014]
overallCv<-surv[,.(westBrookYoy=cv(westBrookYoy),
                        jimmyYoy=cv(jimmyYoy),
                        mitchellYoy=cv(mitchellYoy),
                        obearYoy=cv(obearYoy),
                        westBrookAdult=cv(westBrookAdult),
                        jimmyAdult=cv(jimmyAdult),
                        mitchellAdult=cv(mitchellAdult),
                        obearAdult=cv(obearAdult))]
survCv<-data.table(river=rep(c("west brook","wb jimmy","wb mitchell","wb obear"),2),
                      stage=rep(c("yoy","adult"),each=4),
                      overallCv=as.matrix(overallCv[1])[1,],
                      annualCv=as.matrix(annualCv[1])[1,]) %>%
  setkey(river)

survCv<-streamSize[survCv]
par(mfrow=c(2,2))
plot(log(overallCv)~log(medianQ),data=survCv[stage=="yoy"])
plot(log(overallCv)~log(medianQ),data=survCv[stage=="adult"])
plot(log(annualCv)~log(medianQ),data=survCv[stage=="yoy"])
plot(log(annualCv)~log(medianQ),data=survCv[stage=="adult"])
