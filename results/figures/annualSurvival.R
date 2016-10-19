library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
library(plotHacks)

colors<-c(gray(0.1,.1),rgb(1,0,0,0.1),rgb(0,1,0,0.1),rgb(0,0,1,0.1))

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

iter<-sample(1:out$mcmc.info$n.samples,300)

tiff.par("results/figures/annualSurvival.tif",mfrow=c(4,2))
for(riv in c("westBrook","jimmy","mitchell","obear")){
for(stage in c("Yoy","Adult")){
  plot(NA,xlim=c(2002,2014),ylim=c(0,1),xlab="",ylab="")
  
  g<-which(stage==c("Yoy","Adult"))
  r<-which(riv==c("westBrook","jimmy","mitchell","obear"))
  
  for(i in iter){
    phi<-out$sims.list$phiBeta[i,,,]
    surv<-array(dim=c(nrow(flowData),4,2))
    
    # for(g in 1:2){
      # for(r in 1:4){
        surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+phi[4,r,g]*tempData[,r]*flowData[,r]+
          phi[5,r,g]*meanLength[r,g]
      # }
    # }
    surv<-melt(1/(1+exp(-surv))) %>% dcast(Var1~Var3+Var2) %>% data.table()
    
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
    

      points(get(paste0(riv,stage))~survivalYear,data=annualSurv[survivalYear>=2002&survivalYear<2015],type='l',
             col=gray(0.2,0.1))
             # col=colors[which(r==c("westBrook","jimmy","mitchell","obear"))],lwd=1)
      axis(1,labPos,labs)
      title(ylab="Annual Survival Probability",line=2.2)
    }
  }
}
dev.off()