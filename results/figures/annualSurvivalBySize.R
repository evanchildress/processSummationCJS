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

iter<-sample(1:out$mcmc.info$n.samples,10)
iter<-1:out$mcmc.info$n.samples


lengths<-c(60,100,150,200)
for(size in lengths){
cat("\n")
pb<-txtProgressBar(1,length(iter),style=3)
surv<-array(dim=c(length(iter),nrow(flowData),4,2))
for(it in 1:length(iter)){
  setTxtProgressBar(pb,it)
  i<-iter[it]

  phi<-out$sims.list$phiBeta[i,,,]
  
  
  for(g in 1:2){
    for(r in 1:4){
      surv[it,,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+phi[4,r,g]*tempData[,r]*flowData[,r]
        
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
assign(paste0("annualSurv",size),
       surv[,.(westBrookYoy=plogis(qlogis(prod(westBrookYoy))+phi[5,1,1]*((size-stds$length$meanLength[1])/stds$length$sdLength[1])),
                    jimmyYoy=plogis(qlogis(prod(jimmyYoy))+phi[5,2,1]*((size-stds$length$meanLength[2])/stds$length$sdLength[2])),
                    mitchellYoy=plogis(qlogis(prod(mitchellYoy))+phi[5,3,1]*((size-stds$length$meanLength[3])/stds$length$sdLength[3])),
                    obearYoy=plogis(qlogis(prod(obearYoy))+phi[5,4,1]*((size-stds$length$meanLength[4])/stds$length$sdLength[4])),
                    westBrookAdult=plogis(qlogis(prod(westBrookAdult))+phi[5,1,1]*((size-stds$length$meanLength[1])/stds$length$sdLength[1])),
                    jimmyAdult=plogis(qlogis(prod(jimmyAdult))+phi[5,2,1]*((size-stds$length$meanLength[2])/stds$length$sdLength[2])),
                    mitchellAdult=plogis(qlogis(prod(mitchellAdult))+phi[5,3,1]*((size-stds$length$meanLength[3])/stds$length$sdLength[3])),
                    obearAdult=plogis(qlogis(prod(obearAdult))+phi[5,4,1]*((size-stds$length$meanLength[4])/stds$length$sdLength[4]))),
                 by=survivalYear]
)

survUpper<-melt(survUpper) %>% dcast(Var1~Var3+Var2) %>% data.table()
survUpper[,date:=as.Date(rownames(flowData))]
survUpper[,survivalYear:=year(date)+ifelse(month(date)<6,-1,0)]
setnames(survUpper,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                     "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date","survivalYear"))

assign(paste0("annualSurvUpper",size),
       survUpper[,.(westBrookYoy=plogis(qlogis(prod(westBrookYoy))+phi[5,1,1]*((size-stds$length$meanLength[1])/stds$length$sdLength[1])),
                    jimmyYoy=plogis(qlogis(prod(jimmyYoy))+phi[5,2,1]*((size-stds$length$meanLength[2])/stds$length$sdLength[2])),
                    mitchellYoy=plogis(qlogis(prod(mitchellYoy))+phi[5,3,1]*((size-stds$length$meanLength[3])/stds$length$sdLength[3])),
                    obearYoy=plogis(qlogis(prod(obearYoy))+phi[5,4,1]*((size-stds$length$meanLength[4])/stds$length$sdLength[4])),
                    westBrookAdult=plogis(qlogis(prod(westBrookAdult))+phi[5,1,1]*((size-stds$length$meanLength[1])/stds$length$sdLength[1])),
                    jimmyAdult=plogis(qlogis(prod(jimmyAdult))+phi[5,2,1]*((size-stds$length$meanLength[2])/stds$length$sdLength[2])),
                    mitchellAdult=plogis(qlogis(prod(mitchellAdult))+phi[5,3,1]*((size-stds$length$meanLength[3])/stds$length$sdLength[3])),
                    obearAdult=plogis(qlogis(prod(obearAdult))+phi[5,4,1]*((size-stds$length$meanLength[4])/stds$length$sdLength[4]))),
                           by=survivalYear]
)

survLower<-melt(survLower) %>% dcast(Var1~Var3+Var2) %>% data.table()
survLower[,date:=as.Date(rownames(flowData))]
survLower[,survivalYear:=year(date)+ifelse(month(date)<6,-1,0)]
setnames(survLower,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                     "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date","survivalYear"))
assign(paste0("annualSurvLower",size),
       survLower[,.(westBrookYoy=plogis(qlogis(prod(westBrookYoy))+phi[5,1,1]*((size-stds$length$meanLength[1])/stds$length$sdLength[1])),
                    jimmyYoy=plogis(qlogis(prod(jimmyYoy))+phi[5,2,1]*((size-stds$length$meanLength[2])/stds$length$sdLength[2])),
                    mitchellYoy=plogis(qlogis(prod(mitchellYoy))+phi[5,3,1]*((size-stds$length$meanLength[3])/stds$length$sdLength[3])),
                    obearYoy=plogis(qlogis(prod(obearYoy))+phi[5,4,1]*((size-stds$length$meanLength[4])/stds$length$sdLength[4])),
                    westBrookAdult=plogis(qlogis(prod(westBrookAdult))+phi[5,1,1]*((size-stds$length$meanLength[1])/stds$length$sdLength[1])),
                    jimmyAdult=plogis(qlogis(prod(jimmyAdult))+phi[5,2,1]*((size-stds$length$meanLength[2])/stds$length$sdLength[2])),
                    mitchellAdult=plogis(qlogis(prod(mitchellAdult))+phi[5,3,1]*((size-stds$length$meanLength[3])/stds$length$sdLength[3])),
                    obearAdult=plogis(qlogis(prod(obearAdult))+phi[5,4,1]*((size-stds$length$meanLength[4])/stds$length$sdLength[4]))),
                           by=survivalYear]
)
}

tiff.par("results/figures/annualSurvivalBySize.tif",mfrow=c(4,1),
         mgp=c(1.7,0.5,0),width=4,height=7.5,mar=c(1.5,2.5,0,0.4))

for(s in lengths){
  plot(NA,xlim=c(2002,2014),ylim=c(0,1),xlab="",ylab="")
       #ylab=paste(ifelse(stage=="Yoy","Juvenile","Adult"),"Annual Survival Probability"))
  panelLabel(c("a","b","c","d")[which(s==lengths)])
  
  stage<-ifelse(s<100,"Yoy","Adult")
  
  for(riv in c("westBrook","jimmy","mitchell","obear")){
    #     g<-which(stage==c("Yoy","Adult"))
    #     r<-which(riv==c("westBrook","jimmy","mitchell","obear"))
    points(get(paste0(riv,stage))~survivalYear,
           data=get(paste0("annualSurv",s))[survivalYear>=2002&survivalYear<2015],type='l',
           col=colors[which(riv==c("westBrook","jimmy","mitchell","obear"))],lwd=2)
    points(get(paste0(riv,stage))~survivalYear,
           data=get(paste0("annualSurvUpper",s))[survivalYear>=2002&survivalYear<2015],type='l',
           col=colors[which(riv==c("westBrook","jimmy","mitchell","obear"))],lty=2,lwd=0.5)
    points(get(paste0(riv,stage))~survivalYear,
           data=get(paste0("annualSurvLower",s))[survivalYear>=2002&survivalYear<2015],type='l',
           col=colors[which(riv==c("westBrook","jimmy","mitchell","obear"))],lty=2,lwd=0.5)
  }
}
legend(2004.5,1.03,c("West Brook","Open Large","Open Small","Isolated Small"),
       col=colors[1:4],lty=1,lwd=2,bty='n')
dev.off()

