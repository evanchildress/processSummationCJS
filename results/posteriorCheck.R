##need to set up coreData and have scaled covariates, which can be done by sourcing contourPlots
source("results/figures/contourPlots.R")

coreData[,riverNum:=as.numeric(factor(river,ordered=T,
                                         levels=c("west brook","wb jimmy","wb mitchell","wb obear")))]
coreData[,scaledFlowForP:=scale(flowForP)[,1]]
coreData[,scaledLength:=scale(observedLength)[,1],by=river]
coreData[,nObs:=length(time),by=tag]
coreData<-coreData[nObs>1]
coreData[time==5681,time:=5680]

coreData[,firstObs:=detectionDate==min(detectionDate),by=tag]


predictObs<-function(pFlow,length,river,ageInSamples,time,propSampled,mcmcIter,nPasses,sampleNumber){
  stage<-as.numeric(ageInSamples>=4)+1
  
  alive<-obs<-rep(0,length(time))
  alive[1]<-obs[1]<-NA
  
  for(t in 2:length(time)){
    if(t>2){
      if(alive[t-1]==0) break
    }
    phiBeta<-out$sims.list$phiBeta[mcmcIter,,river[t-1],stage[t-1]]
    pBeta<-out$sims.list$pBeta[mcmcIter,,river[t]]
    pEps<-out$sims.list$pEps[mcmcIter,river[t],sampleNumber[t]]
    p<-pBeta[1]+pBeta[2]*pFlow[t]+pBeta[3]*length[t]+pBeta[4]*(nPasses[t]-1)+pEps

    p<-plogis(p)*propSampled[t]
    
    logitPhi<-phiBeta[1]+
          phiBeta[2]*flowData[time[t-1]:time[t],river[t-1]]+
          phiBeta[3]*tempData[time[t-1]:time[t],river[t-1]]+
          phiBeta[4]*flowData[time[t-1]:time[t],river[t-1]]*tempData[time[t-1]:time[t],river[t-1]]
    logitPhi<-qlogis(prod(plogis(logitPhi)))+phiBeta[5]*length[t-1]
    phi<-plogis(logitPhi)
    
    alive[t]<-rbinom(1,1,phi)
    obs[t]<-alive[t]*rbinom(1,1,p)
  }
  return(obs)
}

it<-sample(1:out$mcmc.info$n.samples,100)
predicted<-matrix(ncol=length(it),nrow=nrow(unique(coreData[firstObs!=TRUE&sampleNumber>28,.(sampleNumber,river)])))

for(i in 1:length(it)){
  coreData[,predictedObs:=predictObs(scaledFlowForP,scaledLength,
                                     riverNum,ageInSamples,time,proportionSampled,it[i],
                                     nPasses,sampleIndex),by=tag]
  
  predicted[,i]<-coreData[firstObs!=TRUE&sampleNumber>28,sum(predictedObs),by=.(sampleNumber,river)] %>%
                 melt(id.vars=c("sampleNumber","river")) %>%
                 .[,value]
  cat(i,"\n")
}

observed<-coreData[firstObs!=TRUE&sampleNumber>28,sum(enc),by=.(sampleNumber,river)] %>%
  melt(id.vars=c("sampleNumber","river")) %>%
  .[,value]

plot(NA,xlim=c(0,max(observed)),ylim=c(0,max(predicted)))
for(i in 1:length(it)){
  points(predicted[,i]~observed)
}
