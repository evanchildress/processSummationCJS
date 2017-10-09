library(data.table)
library(jagsUI)

load("envForSims.RData")
# simNum<- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
# set.seed(simNum)
simNum<-1

getSurvival<-function(startTime,endTime,flow,temp,beta,sigma=0){
  logitPhi<-phiBeta[1]+phiBeta[2]*flow[startTime:endTime]+
    phiBeta[3]*temp[startTime:endTime]+
    phiBeta[4]*flow[startTime:endTime]*temp[startTime:endTime]
  phi<-prod(1/(1+exp(-logitPhi)))
  
  phi<-plogis(qlogis(phi))#+rnorm(1,0,sigma))
  return(phi)
}

times<-round(seq(1,length(flows),length.out=63))
startTimes<-times[1:(length(times)-1)]
endTimes<-times[2:length(times)]-1

n.occasions<-length(times)
marked<-rep(34,n.occasions-1)

pMu<-runif(1,0,2.197225)
pSigma<-runif(1,0.1,1)
pBeta<-rnorm(n.occasions-1,pMu,pSigma)
phiBeta<-c(runif(1,5,9),runif(1,-2,2),runif(1,-2,2),runif(1,-2,2))

p<-plogis(pBeta)

r.var<-0#runif(1,0.1,2)#residual temporal variance

times<-round(seq(1,length(flows),length.out=63))
startTimes<-times[1:(length(times)-1)]
endTimes<-times[2:length(times)]-1

phi<-rep(NA,n.occasions-1)
for(t in 1:(n.occasions-1)){
  phi[t]<-getSurvival(startTimes[t],endTimes[t],flows,temps,phiBeta,r.var)
}

PHI<-matrix(phi,ncol=n.occasions-1,nrow=sum(marked),byrow=T)
P<-matrix(p,ncol=n.occasions-1,nrow=sum(marked),byrow = T)

simul.cjs<-function(PHI,P,marked){
  n.occasions<-dim(PHI)[2]+1
  CH<-matrix(0,ncol=n.occasions,nrow=sum(marked)) 
  
  #when was the fish marked?
  mark.occ<-rep(1:length(marked),marked[1:length(marked)])
  
  #fill the capture history
  for(i in 1:sum(marked)){        
    CH[i,mark.occ[i]]<-1
    if(mark.occ[i]==n.occasions)next
    for(t in (mark.occ[i]+1):n.occasions){
      #Bernoulli trial for survival
      sur<-rbinom(1,1,PHI[i,t-1])
      if(sur==0) break #if dead move to the next individual
      #Bernoulli trial for recaptures
      rp<-rbinom(1,1,P[i,t-1])
      if(rp==1) CH[i,t]<-1
    }
  }
  return(CH)
}

CH<-simul.cjs(PHI,P,marked)

get.first<-function(x) min(which(x!=0))
f<-apply(CH,1,get.first)




cjs.init.z<-function(ch,f){
  for(i in 1:nrow(ch)){
    if(sum(ch[i,])==1)next
    n2<-max(which(ch[i,]==1))
    ch[i,f[i]:n2]<-NA}
  for(i in 1:nrow(ch)){
    ch[i,1:f[i]]<-NA
  }
  return(ch)
}

knownZ<-matrix(NA,nrow=nrow(CH),ncol=ncol(CH))
for(i in 1:nrow(CH)){
  whichMax<-max(which(CH[i,]==1))
  knownZ[i,f[i]:whichMax]<-1
}

zInits<-cjs.init.z(CH,f)

jagsData<-list(y=CH,
               flows=flows,
               temps=temps,
               f=f,
               nTimes=length(flows),
               startTimes=startTimes,
               endTimes=endTimes,
               nOccasions=ncol(CH),
               nInd=nrow(CH),
               z=knownZ)

inits<-function(){list(z=zInits)}

na <- 100
nb <- 1
ni <- 3000
nt <- 3
nc <- 3

varsToMonitor<-c('phiBeta','pMu','pSigma','')

out <- jags(
  data=jagsData,
  inits=inits,
  model = "modelAsMatrix.R",
  parameters.to.save = varsToMonitor,
  n.chains=nc,
  n.iter = ni,
  n.thin = nt,
  n.burnin=nb,
  parallel=T)


results<-data.table(out$summary[1:6,c("mean","sd","2.5%","50%","97.5%","Rhat")])
results[,trueValue:=c(phiBeta,pMu,pSigma)]

# saveRDS(results,paste0("output/sim",simNum,".rds"))
