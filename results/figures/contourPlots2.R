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

labels<-list(a11=matrix(c(-2.8,12,0.995,5,
                          4.25,9,0.995,10,
                          -2.8,15,0.99,10,
                          4.35,5.75,0.99,15,
                          -2.85,18,0.98,10),
                        ncol=4,byrow=T),
             a12=matrix(c(-2.9,17.5,0.985,25,
                          -2.82,15,0.99,15,
                          -2.72,10.2,0.995,10),
                        ncol=4,byrow=T),
             a13=matrix(c(-2.55,16.3,0.994,70,
                          -2.35,11.5,0.995,70,
                          -2.13,7.2,0.996,70,
                          -1.9,1.7,0.997,70,
                          0.7,26.2,0.998,67,
                          2.2,25.2,0.999,65),
                        ncol=4,byrow=T),
             a14=matrix(c(-2.7,15.3,0.996,27,
                          -2.42,9.3,0.997,27,
                          -2,0.6,0.998,30,
                          2.05,-1.2,0.999,30),
                        ncol=4,byrow=T),
             a21=matrix(c(2.55,21.8,0.996,0,
                          3.65,-0.3,0.995,300,
                          2.1,-1.6,0.994,315,
                          0.6,-1.3,0.993,330,
                          -0.75,-1.3,0.992,330),
                        ncol=4,byrow=T),
             a22=matrix(c(4.5,3.5,0.998,350,
                          3.7,0.5,0.996,0,
                          -2.3,1,0.996,0,
                          -2.5,6,0.994,50,
                          -2.65,13,0.990,65),
                        ncol=4,byrow=T),
             a23=matrix(c(-0.65,-1.95,0.98,280,
                          -1.08,-1.95,0.94,280,
                          -2.45,14.3,0.96,320),
                        ncol=4,byrow=T),
             a24=matrix(c(-2.67,18.5,0.996,0,
                          4.35,18.85,0.996,0,
                          4.4,16.8,0.998,10,
                          -2.67,16,0.994,345,
                          -2.65,13.8,0.992,338,
                          -2.45,11.2,0.990,335),
                        ncol=4,byrow=T)
)
borderAdjX<-0.75
borderAdjY<-2.6
for(g in 1:2){
  tiff.par(paste0("results/figures/contourPlots",c("Yoy","Adult")[g],".tif"),
           mfrow=c(2,2),mar=c(2.7,2.7,1,1),oma=c(0,0,0,2))
  for(r in 1:4){
    image(seq(flowRange[1,r]-borderAdjX,flowRange[2,r]+borderAdjX,length.out=100),
            seq(tempRange[1,r]-borderAdjY,tempRange[2,r]*stds$temp$sdTemp[r]+
                  stds$temp$meanTemp[r]+borderAdjY,length.out=100),
            survSim[,,r,g],
            zlim=c(0.88,1.0),
            col=colors,
            xlab="Scaled log(Discharge)",ylab=bquote(Temperature~(degree*C)))
            #main=paste(c("west brook","wb jimmy","wb mitchell","wb obear")[r],c("yoy","adults")[g]))

    hull<-chull(flowData[,r],tempData[,r])

    contour(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r],length.out=100),
            survSim[,,r,g],add=T,lwd=2,nlevels=levels[g,r],
            drawlabels = F,method="flattest")
    
    polygon(c(par("usr")[2],flowData[,r][hull],flowData[,r][hull[1]],par("usr")[c(2,2,1,1,2)]),
            c(par("usr")[4],I(tempData[hull,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),
              I(tempData[hull[1],r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),par("usr")[c(4,3,3,4,4)]),
            border=NA,col="gray")
    a<-paste0("a",g,r)
    panelLabel(bquote(bold(.(c("a","b","c","d")[r]))))
    
    for(r in 1:nrow(labels[[a]])){
      text(labels[[a]][r,1],labels[[a]][r,2],labels[[a]][r,3],cex=0.75,
           srt=labels[[a]][r,4])
    }

#     polygon(flowData[,r][hull],
#             I(tempData[hull,r]*stds$temp$sdTemp[r]+stds$temp$meanTemp[r]),
#             lty=2)
  box()

  }
  
  par(xpd=NA)
  y.leg<-seq(22,35,length.out=500)
  for(i in 84:500){
    points(c(5.5,6),rep(y.leg[i],2),type='l',
           col=colors[i],lend='butt',lwd=0.5)
  }
  text(6.25,24.5,"0.9",cex=0.75)
  text(6.25,35,"1.0",cex=0.75)
  text(6.35,mean(c(24.45,35)),"0.95",cex=0.75)
  dev.off()
}

