#out<-readRDS("processSummationOut.rds")

phi<-out$mean$phiBeta

flowData<-apply(flowData,2,scale2)

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
      survSim[f,,r,g]<-surv<-phi[1,r,g]+phi[2,r,g]*flowSim[f,r]+phi[3,r,g]*flowSim[f,r]^2+
        phi[4,r,g]*tempSim[,r]#+phi[5,r,g]*tempSim[,r]*flowSim[f,r]
    }
  }
}

survSim<-1/(1+exp(-survSim))

for(g in 1:2){
  for(r in 1:4){
    contour(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r],length.out=100),
            survSim[,,r,g],
            xlab="flow",ylab="temp",
            main=paste(c("west brook","wb jimmy","wb mitchell","wb obear")[r],c("yoy","adults")[g]))
    points(tempData[,r]~flowData[,r],col=gray(0.5,0.5),pch=19)
    contour(seq(flowRange[1,r],flowRange[2,r],length.out=100),
            seq(tempRange[1,r],tempRange[2,r],length.out=100),
            survSim[,,r,g],add=T,lwd=2)
  }
}


survFlow<-survTemp<-surv<-array(NA,dim=c(dim(flowData),2))
for(g in 1:2){
  for(r in 1:4){
    survFlow[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*flowData[,r]^2+
      phi[4,r,g]*mean(tempData[,r])+phi[5,r,g]*mean(tempData[,r])^2
    survTemp[,r,g]<-phi[1,r,g]+phi[4,r,g]*tempData[,r]+phi[5,r,g]*tempData[,r]^2+
      phi[2,r,g]*mean(flowData[,r])+phi[3,r,g]*mean(flowData[,r])^2
    surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*flowData[,r]^2+phi[4,r,g]*tempData[,r]+
      phi[5,r,g]*tempData[,r]^2
      #phi[5,r,g]*flowData[,r]*tempData[,r]
  }
}
survFlow<-melt(1/(1+exp(-survFlow))) %>% dcast(Var1~Var3+Var2) %>% data.table()
survTemp<-melt(1/(1+exp(-survTemp))) %>% dcast(Var1~Var3+Var2) %>% data.table()
surv<-melt(1/(1+exp(-surv))) %>% dcast(Var1~Var3+Var2) %>% data.table()

survFlow[,date:=as.Date(rownames(flowData))]
survTemp[,date:=as.Date(rownames(flowData))]
surv[,date:=as.Date(rownames(flowData))]

setnames(survFlow,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))
setnames(survTemp,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))
setnames(surv,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                    "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))

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

plot(NA,xlim=c(-10,3),ylim=c(0,1))
  points(survFlow$westBrookYoy~flowData[,1],pch=19,col=palette()[1])
  points(survFlow$jimmyYoy~flowData[,2],pch=19,col=palette()[2])
  points(survFlow$mitchellYoy~flowData[,3],pch=19,col=palette()[3])
  points(survFlow$obearYoy~flowData[,4],pch=19,col=palette()[4])

plot(NA,xlim=c(-10,3),ylim=c(0,1))
  points(survFlow$westBrookAdult~flowData[,1],pch=19,col=palette()[1])
  points(survFlow$jimmyAdult~flowData[,2],pch=19,col=palette()[2])
  points(survFlow$mitchellAdult~flowData[,3],pch=19,col=palette()[3])
  points(survFlow$obearAdult~flowData[,4],pch=19,col=palette()[4])
  
plot(NA,xlim=c(0,25),ylim=c(0,1))
  points(survTemp$westBrookYoy~tempData[,1],pch=19,col=palette()[1])
  points(survTemp$jimmyYoy~tempData[,2],pch=19,col=palette()[2])
  points(survTemp$mitchellYoy~tempData[,3],pch=19,col=palette()[3])
  points(survTemp$obearYoy~tempData[,4],pch=19,col=palette()[4])
  
plot(NA,xlim=c(0,25),ylim=c(0,1))
  points(survTemp$westBrookAdult~tempData[,1],pch=19,col=palette()[1])
  points(survTemp$jimmyAdult~tempData[,2],pch=19,col=palette()[2])
  points(survTemp$mitchellAdult~tempData[,3],pch=19,col=palette()[3])
  points(survTemp$obearAdult~tempData[,4],pch=19,col=palette()[4])
