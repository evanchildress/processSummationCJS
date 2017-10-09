model{
  #model expected daily surival from covariates
  for(t in 1:nTimes){
    logitPhiExp[t]<- phiBeta[1]
                 +phiBeta[2]*flows[t]
                 +phiBeta[3]*temps[t]
                 +phiBeta[4]*temps[t]*flows[t]
    phiExp[t]<-1/(1+(exp(-logitPhiExp[t])))
  }
  
  #aggregate daily survival between observations and model additional variation
  for(n in 1:(nOccasions-1)){
    phiOcc[n]<-prod(phiExp[startTimes[n]:endTimes[n]])
    # logitPhiOcc[n]~dnorm(log(phiOccExp[n]/(1-phiOccExp[n])),tauPhi)
    #logitPhiOcc[n]<-log(phiOccExp[n]/(1-phiOccExp[n]))
    # phiOcc[n]<-1/(1+exp(-logitPhiOcc[n]))
  }

  #priors
  #survival
  # tauPhi<-pow(sdPhi,-2)
  # sdPhi~dunif(0,10)
  for(b in 1:4){
    phiBeta[b]~dnorm(0,0.1)
  }
  
  #detection
  pMu~dnorm(0,0.1)
  pSigma~dunif(0,20)
  pTau<-1/pow(pSigma,2)
  
  for(t in 1:(nOccasions-1)){
    logitP[t]~dnorm(pMu,pTau)
    p[t]<-1/(1+exp(-logitP[t]))
  }
  #Likelihood
  for(i in 1:nInd){
    for(t in (f[i]+1):nOccasions){
      #State process
      z[i,t]~dbern(surv[i,t])
      surv[i,t]<-phiOcc[t-1]*z[i,t-1]
      #Observation process
      y[i,t]~dbern(mu2[i,t])
      mu2[i,t]<-p[t-1]*z[i,t]
    }
  }
}