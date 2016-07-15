library(data.table)
library(dplyr)
library(plotHacks)

res<-readRDS("results/cjsSimResults.rds") %>%
  data.table() %>%
  setnames(c("2.5%","50%","97.5%"),c("q2.5","q50","q97.5")) %>%
  .[,converged:=all(Rhat<1.1),by=simNum] %>%
  .[converged==T] %>%
  .[,sdPhi:=trueValue[which(parameter=="sdPhi")],by=simNum] %>%
  # .[parameter=="sdPhi",trueValue:=sdPhi] %>%
  .[,diffFrom0:=(q2.5<0&q97.5<0)|(q2.5>0&q97.5>0)] %>%
  .[,withinCI:=q2.5<=trueValue&q97.5>=trueValue] %>%
  .[,ciWidth:=q97.5-q2.5]


bias<-res[,.(bias=round(sum(mean-trueValue)/.N,4),
             relativeBias=round(mean(mean-trueValue)/mean(abs(trueValue)),3),
             rmse=round(sqrt(sum((mean-trueValue)^2)/.N),3),
             propWithinCI=round(sum(withinCI)/.N,3)),
          by=.(parameter)] %>%
  setnames(c("Parameter","Absolute Bias","Relative Bias","RMSE","Proportion in 95% CI"))

tiff.par("results/figures/simTrueVsEstimated.tif",mfrow=c(3,2))
for(p in c("pBeta","phiBeta1","phiBeta2","phiBeta3","phiBeta4","sdPhi")){
  whichP<-which(p==c("pBeta","phiBeta1","phiBeta2","phiBeta3","phiBeta4","sdPhi"))
  plot(mean~trueValue,data=res[parameter==p],xlab="",
       ylab="",pch=19,col=gray(0.5,0.5))
  abline(0,1)
  title(xlab=c(bquote("True logit(P)"),
               bquote("True"~beta[0]),
               bquote("True"~beta[Flow]),
               bquote("True"~beta[Temp]),
               bquote("True"~beta[Temp*Flow]),
               bquote("True"~sigma))[whichP],
  ylab=c(bquote("Estimated logit(P)"),
         bquote("Estimated"~beta[0]),
         bquote("Estimated"~beta[Flow]),
         bquote("Estimated"~beta[Temp]),
         bquote("Estimated"~beta[Temp*Flow]),
         bquote("Estimated"~sigma))[whichP])
}
dev.off()

write.csv(bias,"results/simSummaryTable.csv",row.names=F)
