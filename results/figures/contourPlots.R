library(plotHacks)
library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")
out<-readRDS("results/processSummationOut.rds")
phi<-out$mean$phiBeta

coreData[,stage:=as.numeric(ageInSamples>3)+1]
meanLength<-coreData[enc==1,mean(observedLength),by=.(river,stage)] %>%
  .[,river2:=match(river,c("west brook","wb jimmy","wb mitchell","wb obear"))] %>%
  .[,.(river2,stage,V1)] %>%
  .[,V1:=(V1-stds$length$meanLength[river2])/stds$length$sdLength[river2]] %>%
  melt(id.vars=c("river2","stage")) %>%
  acast(river2~stage)

flowData<-jagsData$flowDATA
tempData<-jagsData$tempDATA

flowRange<-apply(flowData,2,range)
tempRange<-apply(tempData,2,range)

flowSim<-tempSim<-array(NA,dim=c(100,4))
for(r in 1:4){
  flowSim[,r]<-seq(flowRange[1,r],flowRange[2,r],length.out=100)
  tempSim[,r]<-seq(tempRange[1,r],tempRange[2,r],length.out=100)
}

survSim<-array(NA,dim=c(100,100,4,2))
for(g in 1:2){
  for(r in 1:4){
    for(f in 1:100){
      survSim[f,,r,g]<-phi[1,r,g]+phi[2,r,g]*flowSim[f,r]+phi[3,r,g]*tempSim[,r]+
        phi[4,r,g]*tempSim[,r]*flowSim[f,r] + phi[5,r,g]*meanLength[r,g]
    }
  }
}

survSim<-1/(1+exp(-survSim))
library(grDevices)

colors<-colorRampPalette(c(colorRampPalette(c("red","yellow","green"))(5),"blue"))(500)

levels<-matrix(c(10,10,8,8,
                 10,10,30,30),
               nrow=2,ncol=4,byrow=T)

labels<-list(a12=matrix(c(-1.3,21,0.98,
                          -0.6,25,0.99,
                          4,5,0.99),
                        ncol=3,byrow=T))

borderAdjX<-0.5
borderAdjY<-2
for(g in 1:2){
  tiff.par(paste0("results/figures/contourPlots",c("Yoy","Adult")[g],".tif"),
           mfrow=c(2,2),mar=c(2.7,2.7,1,1))
  for(r in 1:4){
    image(seq(flowRange[1,r]-borderAdjX,flowRange[2,r]+borderAdjX,length.out=100),
            seq(tempRange[1,r]-borderAdjY,tempRange[2,r]*stds$temp$sdTemp[r]+
                  stds$temp$meanTemp[r]+borderAdjY,length.out=100),
            survSim[,,r,g],
            zlim=c(0.88,1.0),
            col=colors,
            xlab="Scaled log(Discharge)",ylab=bquote(Temperature~(degree*C)),
            main=paste(c("west brook","wb jimmy","wb mitchell","wb obear")[r],c("yoy","adults")[g]))

    hull<-chull(flowData[,r],tempData[,r])

    contour(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r],length.out=100),
            survSim[,,r,g],add=T,lwd=2,nlevels=levels[g,r],
            drawlabels = T,method="flattest")
    
    polygon(c(par("usr")[2],flowData[,r][hull],flowData[,r][hull[1]],par("usr")[c(2,2,1,1,2)]),
            c(par("usr")[4],I(tempData[hull,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),
              I(tempData[hull[1],r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),par("usr")[c(4,3,3,4,4)]),
            border=NA,col="gray")
    text(2.5,20,"0.996",cex=0.75)
#     polygon(flowData[,r][hull],
#             I(tempData[hull,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),
#             lty=2)
    box()
    
  }
  dev.off()
}

