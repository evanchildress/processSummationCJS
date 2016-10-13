library(getWBData)
library(reshape2)
library(data.table)
library(jagsUI)
library(plotHacks)

coreData<-readRDS("results/processSummationCoreData.rds")
jagsData<-readRDS("results/processSummationJagsData.rds")
stds<-readRDS("results/summationStandards.rds")
out<-readRDS("results/processSummationOut.rds")
phi<-out$mean$phiBeta

flowData<-jagsData$flowDATA
tempData<-jagsData$tempDATA

surv<-array(dim=c(nrow(flowData),4,2))

for(g in 1:2){
  for(r in 1:4){
    surv[,r,g]<-phi[1,r,g]+phi[2,r,g]*flowData[,r]+phi[3,r,g]*tempData[,r]+
      phi[4,r,g]*tempData[,r]*flowData[,r]
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

extremeInfluence<-function(x){sd(x)/mean(x)}



extInf<-surv[,.(westBrookYoy=extremeInfluence(westBrookYoy),
                jimmyYoy=extremeInfluence(jimmyYoy),
                mitchellYoy=extremeInfluence(mitchellYoy),
                obearYoy=extremeInfluence(obearYoy),
                westBrookAdult=extremeInfluence(westBrookAdult),
                jimmyAdult=extremeInfluence(jimmyAdult),
                mitchellAdult=extremeInfluence(mitchellAdult),
                obearAdult=extremeInfluence(obearAdult)),
             ,by=year(date)] %>%
  melt(id.vars="year") %>%
  .[,stage:=as.numeric(grepl("Adult",variable))+1] %>%
  .[stage==1,river:=tstrsplit(variable,"Yoy")] %>%
  .[stage==2,river:=tstrsplit(variable,"Adult")] %>%
  .[,river:=c("west brook","wb jimmy","wb mitchell","wb obear")[
    match(river,c("westBrook","jimmy","mitchell","obear"))]] %>%
  setnames("value","extremeInfluence") %>%
  setkey(river)


streamSize<-tbl(conDplyr,"data_daily_discharge") %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[date>=as.Date(min(coreData$detectionDate))&
      date<=as.Date(max(coreData$detectionDate))] %>%
  .[,.(medianQ=median(discharge)),by=river] %>%
  setkey(river)


extInf<-streamSize[extInf]

tiff.par("results/figures/survCVVsStreamSize.tif",mfrow=c(1,2),
         mar=c(2.5,3,0,0),width=6.5,height=4)

  plot(log(extremeInfluence)~log(medianQ),data=extInf[stage==1],ylim=c(-7.1,-3.5),
       xlab="log(Median Discharge)",ylab="",pch=19,col=gray(0.5,0.5))
  title(ylab="log(CV) of Daily Survival (by year)",line=2.2)
  plot(log(extremeInfluence)~log(medianQ),data=extInf[stage==2],ylim=c(-7.1,-3.5),
       xlab="log(Median Discharge)",ylab="",pch=19,col=gray(0.5,0.5))
  #title(ylab="Proportion Events Accounting for 50% Mortality",line=2.2)

dev.off()
