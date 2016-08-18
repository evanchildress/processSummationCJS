library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
library(plotHacks)

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")
out<-readRDS("processSummationOut.rds")
phi<-out$mean$phiBeta

flowData<-jagsData$flowDATA
tempData<-jagsData$tempDATA

surv<-array(dim=c(nrow(flowData),4,2))

for(g in 1:2){
  for(r in 1:4){
    surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+phi[4,r,g]*tempData[,r]*flowData[,r]
  }
}
surv<-melt(1/(1+exp(-surv))) %>% dcast(Var1~Var3+Var2) %>% data.table()

surv[,date:=as.Date(rownames(flowData))]

setnames(surv,c("index","westBrookYoy","jimmyYoy","mitchellYoy","obearYoy",
                "westBrookAdult","jimmyAdult","mitchellAdult","obearAdult","date"))

library(Rcpp)
cppFunction(
  'double extremeInfluence(NumericVector surv){
  int n = surv.size();
  
  NumericVector survSort(n);
  survSort = surv.sort();
  
  NumericVector lowerProd(n);
  NumericVector upperProd(n);
  
  for(int i = 0; i < n; ++i){
  double totLower = 1;
  double totUpper = 1;
  for(int l = 0; l < i+1; ++l){
  totLower *= survSort[l];
  }
  for(int u = i+1; u < n; ++u){
  totUpper *= survSort[u];
  }
  lowerProd[i] = totLower;
  upperProd[i] = totUpper;
  }
  
  NumericVector diffProd;
  diffProd = abs(lowerProd-upperProd);
  double whichMin;
  whichMin = which_min(diffProd) + 1;
  if((diffProd[whichMin-1] == diffProd[whichMin]) & (n%2 == 1)){
  whichMin = whichMin + 0.5;
  }
  double out;
  out = whichMin;
  out = out/n;
  
  return out;
  
  }')




extInf<-surv[,.(westBrookYoy=extremeInfluence(westBrookYoy),
                jimmyYoy=extremeInfluence(jimmyYoy),
                mitchellYoy=extremeInfluence(mitchellYoy),
                obearYoy=extremeInfluence(obearYoy),
                westBrookAdult=extremeInfluence(westBrookAdult),
                jimmyAdult=extremeInfluence(jimmyAdult),
                mitchellAdult=extremeInfluence(mitchellAdult),
                obearAdult=extremeInfluence(obearAdult)),
             ,by=year(date)]

tiff.par("results/figures/ExtremeInfluence.tif",mfcol=c(1,2),width=6.5,height=4,
         mar=c(2.5,2.5,1,0))
for(g in c("Yoy","Adult")){
  plot(NA,ylim=c(0,0.5),xlim=c(2002,2015),
       ylab="Proportion Days Accounting for 50% Mortality",xlab="",main=g)
  for(r in c("westBrook","jimmy","mitchell","obear")){
    points(get(paste0(r,g))~year,data=extInf[year>=2002],type='l',
           col=palette()[which(r==c("westBrook","jimmy","mitchell","obear"))])
  }
}
dev.off()