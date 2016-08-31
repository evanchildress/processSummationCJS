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
        phi[4,r,g]*tempSim[,r]*flowSim[f,r]
    }
  }
}

survSim<-1/(1+exp(-survSim))
library(grDevices)

colors<-colorRampPalette(c(colorRampPalette(c("red","yellow","green"))(5),"blue"))(500)

for(g in 1:2){
  tiff.par(paste0("results/figures/contourPlots",c("Yoy","Adult")[g],".tif"),
           mfrow=c(2,2),mar=c(2.7,2.7,1,1))
  for(r in 1:4){
    image(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r],length.out=100),
            survSim[,,r,g],
            zlim=c(0.85,1.0),
            col=colors,
            xlab="Scaled log(Discharge)",ylab=bquote(Temperature~(degree*C)),
            main=paste(c("west brook","wb jimmy","wb mitchell","wb obear")[r],c("yoy","adults")[g]))
    contour(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r],length.out=100),
            survSim[,,r,g],add=T,lwd=2,nlevels=30)
    hull<-chull(flowData[,r],tempData[,r])
#     polygon(c(par("usr")[2],flowData[2:nrow(flowData),r][hull],
#               flowData[2:nrow(flowData),r][hull[1]],par("usr")[c(2,2,1,1,2)]),
#             c(par("usr")[4],I(tempData[hull,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),
#               I(tempData[hull[1],r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),par("usr")[c(4,3,3,4,4)]),
#             border=NA,col='white')
    polygon(flowData[,r][hull],
            I(tempData[hull,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),
            lty=2)
    box()
    
  }
  dev.off()
}