library(data.table)
library(dplyr)
library(jagsUI)
library(plotHacks)

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")

out<-readRDS("results/processSummationOut.rds")
p<-out$mean$phiBeta

coreData[,scaledLength:=scale(observedLength)[,1],by=river]
lengthRange<-coreData[enc==1,.(minLength=min(scaledLength),
                               maxLength=max(scaledLength)),
                      by=.(river)]
flowForPRange<-coreData[,.(minFlowForP=min(flowForP,na.rm=T),
                        maxFlowForP=max(flowForP,na.rm=T)),
                     by=.(river)]

lengthEffect<-lengthSim<-flowEffect<-flowForPSim<-lengthSimReal<-array(dim=c(100,4))

colors<-rep(gray(0.2,0.1),4)

for(r in 1:4){
  riv<-c("west brook","wb jimmy","wb mitchell",'wb obear')[r]
  lengthSim[,r]<-seq(lengthRange[river==riv,minLength],
                     lengthRange[river==riv,maxLength],
                     length.out=100)
  lengthSimReal[,r]<-lengthSim[,r]*as.numeric(stds$length[stds$length$river==riv,"sdLength"])+
    as.numeric(stds$length[stds$length$river==riv,"meanLength"])
  flowForPSim[,r]<-seq(flowForPRange[river==riv,minFlowForP],
                       flowForPRange[river==riv,maxFlowForP],
                       length.out=100)
  
}

# palette(c(gray(0.5,0.5),rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,1,0.5)))
iters<-sample(1:out$mcmc.info$n.samples,300)
tiff.par("results/figures/detectionEffects.tif",width=6.5,height=7.5,mar=c(2.5,2.6,0,0),
         mfcol=c(4,2))

for(r in 1:4){
  plot(NA,xlim=range(lengthSimReal),ylim=c(0,1),
       xlab="Fork Length (mm)",ylab="")
  panelLabel(bquote(bold(.(c("a","b","c","d")[r]))),xadj=0.025)
  title(ylab="Probability of Detection",line=1.7)
  for(i in iters){
    lengthEffect[,r]<-plogis(out$sims.list$pBeta[i,1,r]+out$sims.list$pBeta[i,3,r]*lengthSim[,r])
    
    points(lengthEffect[,r]~lengthSimReal[,r],type='l',col=colors[r])
    
    if(r==1){
      lengthEffect[,r]<-plogis(out$sims.list$pBeta[i,1,r]+out$sims.list$pBeta[i,3,r]*lengthSim[,r]+
                                 out$sims.list$pBeta[i,4,r])
      points(lengthEffect[,r]~lengthSimReal[,r],type='l',col=rgb(0,0,1,0.1))
    }
  }
}


for(r in 1:4){
  plot(NA,xlim=range(flowForPSim),ylim=c(0,1),
       xlab="Discharge During Sample",ylab="")
  panelLabel(bquote(bold(.(c("e","f","g","h")[r]))),xadj=0.025)
  title(ylab="Probability of Detection",line=1.7)
  for(i in iters){

  flowEffect[,r]<-plogis(out$sims.list$pBeta[i,1,r]+out$sims.list$pBeta[i,2,r]*flowForPSim[,r])
  points(flowEffect[,r]~flowForPSim[,r],type='l',col=colors[r])
  if(r==1){
    lengthEffect[,r]<-plogis(out$sims.list$pBeta[i,1,r]+out$sims.list$pBeta[i,2,r]*flowForPSim[,r]+
                               out$sims.list$pBeta[i,4,r])
    points(lengthEffect[,r]~flowForPSim[,r],type='l',col=rgb(0,0,1,0.1))
  }
  
  }
}

legend(80,0.3,c("west brook","jimmy","mitchell","obear"),
       lty=1,col=palette()[1:4],bty='n')
dev.off()
