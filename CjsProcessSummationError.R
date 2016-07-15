model{
  ############## Recapture model
  for(i in 1:nEvalRows){
    logit( p[ evalRows[i] ] ) <- pBeta[1,
                                       season[evalRows[i]],
                                       riverDATA[evalRows[i]],
                                       year[evalRows[i]],
                                       stageDATA[evalRows[i]]]+
                                pBeta[2,
                                      season[evalRows[i]],
                                      riverDATA[evalRows[i]],
                                      year[evalRows[i]],
                                      stageDATA[evalRows[i]]]*
                                lengthDATA[evalRows[i]] #+
                                # pEps[sample[evalRows[i]],
                                #      riverDATA[evalRows[i]],
                                #      stageDATA[evalRows[i]]]
    
  }

  # for(g in 1:2){
  #   for(r in 1:nRivers){
  #     for(sam in 1:nSamples){
  #       pEps[sam,r,g]~dnorm(0,pSigma[r,g])
  #     }
  #   }
  # }
  
  ############## Recapture priors
  for(g in 1:2){
    for( s in 1:4 ){
      for( r in 1:(nRivers) ){
        for(y in 1:nYears){
         # pSigma[r,g]~dunif(0,10)

          pBeta[ 1,s,r,y,g ] ~ dnorm( 0,1.22 )
          pBeta[ 2,s,r,y,g]~dnorm(muPBeta2[g],tauPBeta2[g])
        }
      }
    }

  
    muPBeta2[g]~dnorm(0,1.22)
    sigmaPBeta2[g]~dgamma(2,0.1)
    tauPBeta2[g]<-1/pow(sigmaPBeta2[g],2)
  }
  

##survival priors

  for(r in 1:nRivers){
    for(g in 1:2){
        phiSigma[r,g]~dunif(0,10)
        phiTau[r,g]<-1/pow(phiSigma[r,g],2)
      
        for(b in 1:4){
          phiBeta[b,r,g]~dnorm(0,0.667)
      }
    }
  }
  
#   for(i in 1:nEvalRows){
#     for(t in 1:nTimesByRow[evalRows[i]]){ 
#     #for(t in time[evalRows[i]-1]:time[evalRows[i]]){
#       logitPhi[evalRows[i],t]<-phiBeta[1,riverDATA[evalRows[i]]]+
#         phiBeta[2,riverDATA[evalRows[i]]]*flowDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]+
#         phiBeta[3,riverDATA[evalRows[i]]]*flowDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]^2+
#         phiBeta[4,riverDATA[evalRows[i]]]*tempDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]+
#         phiBeta[5,riverDATA[evalRows[i]]]*lengthDATA[evalRows[i]]+
#         phiBeta[6,riverDATA[evalRows[i]]]*lengthDATA[evalRows[i]]*flowDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]+
#         phiBeta[7,riverDATA[evalRows[i]]]*lengthDATA[evalRows[i]]*flowDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]^2
#       phi[evalRows[i],t]<-1/(1+exp(-logitPhi[evalRows[i],t]))
#     }
#     
#   }
  
  for(t in 1:nTimes){
    for(r in 1:nRivers){
      for(g in 1:2){
        logitPhi[t,r,g]<-phiBeta[1,r,g]+
          phiBeta[2,r,g]*flowDATA[t,r]+phiBeta[3,r,g]*tempDATA[t,r]+
          phiBeta[4,r,g]*flowDATA[t,r]^2
        # +phiBeta[5,r,g]*tempDATA[t,r]*flowDATA[t,r]
        phi[t,r,g]<-1/(1+exp(-logitPhi[t,r,g]))
      }
    }
  }
  
  for(g in 1:2){
    for(r in 1:nRivers){
      for(y in 1:nYears){
        for(s in 1:4){
          phiEps[s,y,r,g]~dnorm(0,phiTau[r,g])
        }
      }
    }
  }
  
  for(i in 1:nEvalRows){
    # State of survival
    z[ evalRows[i] ] ~ dbern( survProb[ evalRows[i] ] ) #Do or don't suvive to i
    # survProb[evalRows[i]] <-prod(phi[evalRows[i],1:nTimesByRow[evalRows[i]]])*
    #   z[ evalRows[i]-1 ]
    
    # survProb[evalRows[i]] <- prod(phi[time[evalRows[i]-1]:time[evalRows[i]],
    #                                   riverDATA[evalRows[i]-1],
    #                                   stageDATA[evalRows[i]-1]])
    
    survProbCov[evalRows[i]] <- prod(phi[time[evalRows[i]-1]:time[evalRows[i]],
                                      riverDATA[evalRows[i]-1],
                                      stageDATA[evalRows[i]-1]])
    logitSurvProb[evalRows[i]]<-logit(survProbCov[evalRows[i]])+
                                phiEps[season[evalRows[i]],
                                       year[evalRows[i]],
                                       riverDATA[evalRows[i]],
                                       stageDATA[evalRows[i]]]
    survProb[evalRows[i]]<-1/(1+exp(-logitSurvProb[evalRows[i]]))*z[evalRows[i]-1]
    
    # Observation of live encounters
    encDATA[ evalRows[i] ] ~ dbern( obsProb[ evalRows[i] ] )
    
    obsProb[ evalRows[i] ]<- p[ evalRows[i]] *proportionSampled[evalRows[i]] * z[ evalRows[i] ]
    #* availableDATA[ evalRows[i]+1 ]                 # Must be on the study site to be capturable.
  }
}