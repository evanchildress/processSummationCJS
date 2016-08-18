library(data.table)
library(dplyr)
library(jagsUI)
library(plotHacks)

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")

out<-readRDS("processSummationOut.rds")
phi<-out$mean$phiBeta

flowData<-jagsData$flowDATA
tempData<-jagsData$tempDATA

coreData[,scaledLength:=scale(observedLength)[,1],by=river]
lengthRange<-coreData[enc==1,.(minLength=min(scaledLength),
                               maxLength=max(scaledLength)),
                      by=.(river)]

lengthEffect<-lengthSim<-lengthSimReal<-array(dim=c(100,4))

for(r in 1:4){
  riv<-c("west brook","wb jimmy","wb mitchell",'wb obear')[r]
  lengthSim[,r]<-seq(lengthRange[river==riv,minLength],
                     lengthRange[river==riv,maxLength],
                     length.out=100)
  lengthSimReal[,r]<-lengthSim[,r]*as.numeric(stds$length[stds$length$river==riv,"sdLength"])+
    as.numeric(stds$length[stds$length$river==riv,"meanLength"])
  
}

palette(c(gray(0.5,0.5),rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,1,0.5)))

tiff.par("results/figures/lengthEffect.tif",width=4,height=4,mar=c(2.5,2.6,0,0))
plot(NA,xlim=range(lengthSimReal),ylim=c(0,1),
     xlab="Fork Length (mm)",ylab="")
title(ylab="90 Day Survival Probability",line=1.7)
for(i in sample(1:out$mcmc.info$n.samples,100)){
  for(r in 1:4){
    
    int<-prod(rep(plogis(out$sims.list$phiBeta[i,1,r,2]),90))
    lengthEffect[,r]<-plogis(qlogis(int)+out$sims.list$phiBeta[i,5,r,2]*lengthSim[,r])
    
    points(lengthEffect[,r]~lengthSimReal[,r],type='l',col=palette()[r])
    
  }
}
dev.off()
