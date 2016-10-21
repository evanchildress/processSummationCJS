library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
library(plotHacks)

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")
out<-readRDS("results/processSummationOut.rds")


flowData<-jagsData$flowDATA
tempData<-jagsData$tempDATA


coreData[,stage:=as.numeric(ageInSamples>3)+1]


iter<-sample(1:out$mcmc.info$n.samples,10)
iter<-1:out$mcmc.info$n.samples

for(size in lengths){
  surv<-array(dim=c(length(iter),nrow(flowData),4,2))
  cat("\n")
  pb<-txtProgressBar(1,length(iter),style=3,width=50)
  for(it in 1:length(iter)){
    setTxtProgressBar(pb,it)
    i<-iter[it]
    
    phi<-out$sims.list$phiBeta[i,,,]
    
    for(g in 1:2){
      for(r in 1:4){
        surv[it,,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+
          phi[4,r,g]*tempData[,r]*flowData[,r]+
          phi[5,r,g]*((size-stds$length$meanLength[r])/stds$length$sdLength[r])
      }
    }
        
  }
  
  surv<-1/(1+exp(-surv))
  survUpper<-apply(surv,c(2,3,4),quantile,probs=0.975)
  survLower<-apply(surv,c(2,3,4),quantile,probs=0.025)
  survMean<-apply(surv,c(2,3,4),mean)
    
  
  survUpper<-melt(survUpper) %>% 
             dcast(Var1~Var3+Var2) %>%
             data.table() %>%
             .[,date:=as.Date(rownames(flowData))] %>%
             setnames(c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
              "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))
  
  survLower<-melt(survLower) %>% 
    dcast(Var1~Var3+Var2) %>%
    data.table() %>%
    .[,date:=as.Date(rownames(flowData))] %>%
    setnames(c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
               "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))
  
  survMean<-melt(survMean) %>% 
    dcast(Var1~Var3+Var2) %>%
    data.table() %>%
    .[,date:=as.Date(rownames(flowData))] %>%
    setnames(c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
               "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))

  assign(paste0("byDayMean",size),
         survMean[,.(westBrookYoy=median(westBrookYoy),
                 jimmyYoy=median(jimmyYoy),
                 mitchellYoy=median(mitchellYoy),
                 obearYoy=median(obearYoy),
                 westBrookAdult=median(westBrookAdult),
                 jimmyAdult=median(jimmyAdult),
                 mitchellAdult=median(mitchellAdult),
                 obearAdult=median(obearAdult)),
              by=yday(date)] %>%
    setkey(yday)
  )
  
  assign(paste0("byDayLower",size),
         survLower[,.(westBrookYoy=median(westBrookYoy),
                     jimmyYoy=median(jimmyYoy),
                     mitchellYoy=median(mitchellYoy),
                     obearYoy=median(obearYoy),
                     westBrookAdult=median(westBrookAdult),
                     jimmyAdult=median(jimmyAdult),
                     mitchellAdult=median(mitchellAdult),
                     obearAdult=median(obearAdult)),
                  by=yday(date)] %>%
           setkey(yday)
  )
  
  assign(paste0("byDayUpper",size),
         survUpper[,.(westBrookYoy=median(westBrookYoy),
                     jimmyYoy=median(jimmyYoy),
                     mitchellYoy=median(mitchellYoy),
                     obearYoy=median(obearYoy),
                     westBrookAdult=median(westBrookAdult),
                     jimmyAdult=median(jimmyAdult),
                     mitchellAdult=median(mitchellAdult),
                     obearAdult=median(obearAdult)),
                  by=yday(date)] %>%
           setkey(yday)
  )
}

labPos<-yday(seq.Date(as.Date("2000-01-01"),as.Date("2000-12-01"),"months"))
labs<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

tiff.par("results/figures/survivalAcrossSeasonsBySize.tif",mfrow=c(4,1),width=3,height=7,
         mar=c(2.5,3,1,0))
for(s in lengths){

  yLow<-c(0.98,0.98,0.9745,0.92)[which(s==lengths)]
  plot(NA,xlim=c(0,365),ylim=c(yLow,1),xlab="",ylab="",xaxt='n',yaxt='n')
  panelLabel(bquote(bold(.(c("a","b","c","d")[which(s==lengths)]))),xadj=0.02)
  axis(2,seq(round(yLow,-2),1,0.01))
  axis(1,labPos,labs)
  g<-ifelse(s<100,"Yoy","Adult")
  for(r in c("westBrook","jimmy","mitchell","obear")){
    title(ylab="Daily Survival Probability",line=2.2)
    points(get(paste0(r,g))~yday,data=get(paste0("byDayMean",s)),
           type='l',lwd=1.5,
           col=palette()[which(r==c("westBrook","jimmy","mitchell","obear"))])
    points(get(paste0(r,g))~yday,data=get(paste0("byDayUpper",s)),
           type='l',lwd=0.5,lty=2,
           col=palette()[which(r==c("westBrook","jimmy","mitchell","obear"))])
    points(get(paste0(r,g))~yday,data=get(paste0("byDayLower",s)),
           type='l',lwd=0.5,lty=2,
           col=palette()[which(r==c("westBrook","jimmy","mitchell","obear"))])
  }
  if(s==60){
    legend(35,0.992,c("West Brook","Open Large","Open Small","Isolated Small"),lty=1,lwd=2,bty='n',
           col=palette()[1:4])
  }
}
dev.off()




