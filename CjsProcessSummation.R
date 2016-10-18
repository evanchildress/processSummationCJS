model{

  for(i in 1:nEvalRows){
    logit( p[ evalRows[i] ] ) <- pBeta[1,riverDATA[evalRows[i]]]+
      pBeta[2,riverDATA[evalRows[i]]]*flowForP[evalRows[i]] +
      pBeta[3,riverDATA[evalRows[i]]]*lengthDATA[evalRows[i]] +
      pBeta[4,riverDATA[evalRows[i]]]*(nPasses[evalRows[i]]-1) +
      pEps[riverDATA[evalRows[i]],sample[evalRows[i]]]
  }
  
  ############## Recapture priors
  for( r in 1:(nRivers) ){
    pBeta[1,r]~dnorm(0,0.66)
    pBeta[2,r]~dnorm(0,0.66)
    pBeta[3,r]~dnorm(0,0.66)
    for(s in 1:nSamples){
      pEps[r,s]~dnorm(0,pTau)
    }
  }
  pBeta[4,1]~dnorm(0,0.66)
  for(r in 2:nRivers){
    pBeta[4,r]<-0
  }
  
  pTau<-1/pow(pSigma,2)
  pSigma~dunif(0,10)
  
  
  ##survival priors
  
  for(r in 1:nRivers){
    for(g in 1:2){
      for(b in 1:4){
        phiBeta[b,r,g]~dnorm(0,0.667)
      }
    }
    phiBeta[5,r,1]~dnorm(0,0.667)
    phiBeta[5,r,2]<-phiBeta[5,r,1]
  }
  
  # phiTau<-1/pow(phiSigma,2)
  # phiSigma~dunif(0,10)
  # for(r in 1:nRivers){
  #   for(s in 1:nSamples){
  #     phiEps[r,s]~dnorm(0,phiTau)
  #   }
  # }
  
  for(t in 1:nTimes){
    for(r in 1:nRivers){
      for(g in 1:2){
        logitPhi[t,r,g]<-phiBeta[1,r,g]+
          phiBeta[2,r,g]*flowDATA[t,r]+phiBeta[3,r,g]*tempDATA[t,r]+
          phiBeta[4,r,g]*flowDATA[t,r]*tempDATA[t,r]
        phi[t,r,g]<-1/(1+exp(-logitPhi[t,r,g]))
      }
    }
  }
  
  for(i in 1:nEvalRows){
    # State of survival
    z[ evalRows[i] ] ~ dbern( survProb[ evalRows[i] ] ) #Do or don't suvive to i
    
    #get product of daily survival probs and add in seasonal predictors and error
    survProbCov[evalRows[i]] <- prod(phi[time[evalRows[i]-1]:time[evalRows[i]],
                                         riverDATA[evalRows[i]-1],
                                         stageDATA[evalRows[i]-1]])
    logitSurvProb[evalRows[i]]<-logit(survProbCov[evalRows[i]])+
      
      phiBeta[5,
              riverDATA[evalRows[i]-1],
              stageDATA[evalRows[i]-1]]*
      lengthDATA[evalRows[i]-1] #+
      # phiEps[riverDATA[evalRows[i]-1],
      #        sample[evalRows[i]-1]]
      # 
    survProb[evalRows[i]]<-1/(1+exp(-logitSurvProb[evalRows[i]]))*z[evalRows[i]-1]
    
    # Observation of live encounters
    encDATA[ evalRows[i] ] ~ dbern( obsProb[ evalRows[i] ] )
    
    obsProb[ evalRows[i] ]<- p[ evalRows[i]] *proportionSampled[evalRows[i]] * z[ evalRows[i] ]
    #* availableDATA[ evalRows[i]+1 ]                 # Must be on the study site to be capturable.
  }
}