model{
  #model expected daily surival from covariates
  for(t in 1:nTimes){
    logitPhiExp[t]<- phiBeta[1]
                 +phiBeta[2]*flows[t]
                 # +phiBeta[3]*flows[t]^2
                 +phiBeta[3]*temps[t]
    phiExp[t]<-1/(1+(exp(-logitPhiExp[t])))
  }
  
  #aggregate daily survival between observations and model additional variation
  for(n in 1:(nOccasions-1)){
    phiOccExp[n]<-prod(phiExp[startTimes[n]:endTimes[n]])
    logitPhiOcc[n]~dnorm(log(phiOccExp[n]/(1-phiOccExp[n])),tauPhi)
    #logitPhiOcc[n]<-log(phiOccExp[n]/(1-phiOccExp[n]))
    phiOcc[n]<-1/(1+exp(-logitPhiOcc[n]))
  }

  #priors
  #survival
  tauPhi<-pow(sdPhi,-2)
  sdPhi~dunif(0,10)
  for(b in 1:4){
    phiBeta[b]~dnorm(0,0.001)T(-10,10)
  }
  
  #detection
  for(b in 1){
    pBeta[b]~dnorm(0,0.001)T(-10,10)
  }
  
  p<-1/(1+exp(-pBeta[1]))
  
  #Likelihood
  for(i in 1:nInd){
    for(t in (f[i]+1):nOccasions){
      #State process
      z[i,t]~dbern(surv[i,t])
      surv[i,t]<-phiOcc[t-1]*z[i,t-1]
      #Observation process
      y[i,t]~dbern(mu2[i,t])
      mu2[i,t]<-p*z[i,t]
    }
  }
}