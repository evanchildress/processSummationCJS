out<-readRDS("processSummationSplitRiverOut.rds")

phi<-out$mean$phiBeta

flowData<-apply(flowData,2,scale2)
#tempData<-apply(tempData,2,scale2)

flowRange<-apply(flowData,2,range)
tempRange<-apply(tempData,2,range)

meanTemp<-apply(tempData,2,mean)

flowSim<-tempSim<-array(NA,dim=c(100,6))
for(r in 1:6){
  flowSim[,r]<-seq(flowRange[1,r],flowRange[2,r],length.out=100)
  tempSim[,r]<-seq(tempRange[1,r],tempRange[2,r],length.out=100)
}

survSim<-array(NA,dim=c(100,100,6,2))
for(g in 1:2){
  for(r in 1:6){
    for(f in 1:100){
      survSim[f,,r,g]<-surv<-phi[1,r,g]+phi[2,r,g]*flowSim[f,r]+phi[3,r,g]*tempSim[f,r]+
        phi[4,r,g]*tempSim[,r]*flowSim[f,r]#+phi[5,r,g]*tempSim[,r]*flowSim[f,r]
    }
  }
}

survSim<-1/(1+exp(-survSim))

for(g in 1:2){
  for(r in 1:6){
    contour(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r],length.out=100),
            survSim[,,r,g],
            xlab="flow",ylab="temp",
            main=paste(c("west brook1","west brook2","west brook3","wb jimmy","wb mitchell","wb obear")[r],c("yoy","adults")[g]))
    points(tempData[,r]~flowData[,r],col=gray(0.5,0.5),pch=19)
    contour(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r],length.out=100),
            survSim[,,r,g],add=T,lwd=2)
  }
}


survFlow<-survTemp<-surv<-array(NA,dim=c(dim(flowData),2))
for(g in 1:2){
  for(r in 1:6){
    survFlow[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*meanTemp[r]#+
      #phi[4,r,g]*mean(tempData[,r])#+phi[5,r,g]*mean(tempData[,r])^2
    survTemp[,r,g]<-phi[1,r,g]+phi[3,r,g]*tempData[,r]#+phi[5,r,g]*tempData[,r]^2+
      #phi[2,r,g]*mean(flowData[,r])+phi[3,r,g]*mean(flowData[,r])^2
    surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+phi[4,r,g]*tempData[,r]*flowData[,r]
      #phi[5,r,g]*tempData[,r]^2
      #phi[5,r,g]*flowData[,r]*tempData[,r]
  }
}
survFlow<-melt(1/(1+exp(-survFlow))) %>% dcast(Var1~Var3+Var2) %>% data.table()
survTemp<-melt(1/(1+exp(-survTemp))) %>% dcast(Var1~Var3+Var2) %>% data.table()
surv<-melt(1/(1+exp(-surv))) %>% dcast(Var1~Var3+Var2) %>% data.table()

survFlow[,date:=as.Date(rownames(flowData))]
survTemp[,date:=as.Date(rownames(flowData))]
surv[,date:=as.Date(rownames(flowData))]

setnames(survFlow,c("index","westBrook1Yoy","westBrook2Yoy","westBrook3Yoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrook1Adult","westBrook2Adult","westBrook3Adult","jimmyAdult","mitchellAdult","obearAdult","date"))
setnames(survTemp,c("index","westBrook1Yoy","westBrook2Yoy","westBrook3Yoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrook1Adult","westBrook2Adult","westBrook3Adult","jimmyAdult","mitchellAdult","obearAdult","date"))
setnames(surv,c("index","westBrook1Yoy","westBrook2Yoy","westBrook3Yoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrook1Adult","westBrook2Adult","westBrook3Adult","jimmyAdult","mitchellAdult","obearAdult","date"))

# plot(westBrookYoy~date,data=surv,type='l')
# points(westBrookYoy~date,data=survFlow,type='l',col='blue')
# points(westBrookYoy~date,data=survTemp,type='l',col='red')
# 
# plot(jimmyYoy~date,data=surv,type='l')
# points(jimmyYoy~date,data=survFlow,type='l',col='blue')
# points(jimmyYoy~date,data=survTemp,type='l',col='red')
# 
# plot(mitchellYoy~date,data=surv,type='l')
# points(mitchellYoy~date,data=survFlow,type='l',col='blue')
# points(mitchellYoy~date,data=survTemp,type='l',col='red')
# 
# plot(westBrookAdult~date,data=surv,type='l')
# points(westBrookAdult~date,data=survFlow,type='l',col='blue')
# points(westBrookAdult~date,data=survTemp,type='l',col='red')
# 
# plot(jimmyAdult~date,data=surv,type='l')
# points(jimmyAdult~date,data=survFlow,type='l',col='blue')
# points(jimmyAdult~date,data=survTemp,type='l',col='red')
# 
# plot(mitchellAdult~date,data=surv,type='l')
# points(mitchellAdult~date,data=survFlow,type='l',col='blue')
# points(mitchellAdult~date,data=survTemp,type='l',col='red')
# 
# 
# plot(westBrookYoy~date,data=surv,type='l',col='blue',ylim=c(0.85,1))
# points(westBrookAdult~date,data=surv,type='l')
# 
# plot(jimmyYoy~date,data=surv,type='l',col='blue',ylim=c(0.85,1))
# points(jimmyAdult~date,data=surv,type='l')
# 
# plot(mitchellYoy~date,data=surv,type='l',col='blue',ylim=c(0.85,1))
# points(mitchellAdult~date,data=surv,type='l')

plot(NA,xlim=range(flowData),ylim=c(0.95,1))
  points(survFlow$westBrook1Yoy~flowData[,1],pch=19,col=palette()[1])
  points(survFlow$westBrook2Yoy~flowData[,1],pch=19,col=palette()[1])
  points(survFlow$westBrook3Yoy~flowData[,1],pch=19,col=palette()[1])
  points(survFlow$jimmyYoy~flowData[,4],pch=19,col=palette()[2])
  points(survFlow$mitchellYoy~flowData[,5],pch=19,col=palette()[3])
  #points(survFlow$obearYoy~flowData[,6],pch=19,col=palette()[4])

plot(NA,xlim=range(flowData),ylim=c(0.9,1))
  points(survFlow$westBrook1Adult~flowData[,1],pch=19,col=palette()[1])
  points(survFlow$westBrook2Adult~flowData[,1],pch=19,col=palette()[1])
  points(survFlow$westBrook3Adult~flowData[,1],pch=19,col=palette()[1])
  points(survFlow$jimmyAdult~flowData[,4],pch=19,col=palette()[2])
  points(survFlow$mitchellAdult~flowData[,5],pch=19,col=palette()[3])
  #points(survFlow$obearAdult~flowData[,6],pch=19,col=palette()[4])
  
plot(NA,xlim=range(tempData),ylim=c(0.95,1))
  points(survTemp$westBrook1Yoy~tempData[,1],pch=19,col=palette()[1])
  points(survTemp$westBrook2Yoy~tempData[,1],pch=19,col=palette()[1])
  points(survTemp$westBrook3Yoy~tempData[,1],pch=19,col=palette()[1])
  points(survTemp$jimmyYoy~tempData[,4],pch=19,col=palette()[2])
  points(survTemp$mitchellYoy~tempData[,5],pch=19,col=palette()[3])
  points(survTemp$obearYoy~tempData[,6],pch=19,col=palette()[4])
  
plot(NA,xlim=range(tempData),ylim=c(0.95,1))
  points(survTemp$westBrook1Adult~tempData[,1],pch=19,col=palette()[1])
  points(survTemp$westBrook2Adult~tempData[,1],pch=19,col=palette()[1])
  points(survTemp$westBrook3Adult~tempData[,1],pch=19,col=palette()[1])
  points(survTemp$jimmyAdult~tempData[,4],pch=19,col=palette()[2])
  points(survTemp$mitchellAdult~tempData[,5],pch=19,col=palette()[3])
  points(survTemp$obearAdult~tempData[,6],pch=19,col=palette()[4])
  

