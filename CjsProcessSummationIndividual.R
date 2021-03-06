model{
  ############## Recapture model
  for(i in 1:nEvalRows){
    logit( p[ evalRows[i] ] ) <- pBeta[1,
                                       season[evalRows[i]],
                                       riverDATA[evalRows[i]],
                                       stageDATA[evalRows[i]]]+
      pBeta[2,
            season[evalRows[i]],
            riverDATA[evalRows[i]],
            stageDATA[evalRows[i]]]*
      flowForP[evalRows[i]]
    
  }
  
  ############## Recapture priors
  for(b in 1:2){
    for( s in 1:4 ){
      for( r in 1:(nRivers) ){
        for(g in 1:2){
          pBeta[ b,s,r,g ] ~ dnorm( 0,0.667 )
        }
      }
    }
  }

  for(b in 1:7){
    for(r in 1:nRivers){
        phiBeta[b,r]~dnorm(0,0.667)
      }
    }
  
  for(i in 1:nEvalRows){
    for(t in 1:nTimesByRow[evalRows[i]]){ 
    #for(t in time[evalRows[i]-1]:time[evalRows[i]]){
      logitPhi[evalRows[i],t]<-phiBeta[1,riverDATA[evalRows[i]]]+
        phiBeta[2,riverDATA[evalRows[i]]]*flowDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]+
        phiBeta[3,riverDATA[evalRows[i]]]*flowDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]^2+
        phiBeta[4,riverDATA[evalRows[i]]]*tempDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]+
        phiBeta[5,riverDATA[evalRows[i]]]*lengthDATA[evalRows[i]]+
        phiBeta[6,riverDATA[evalRows[i]]]*lengthDATA[evalRows[i]]*flowDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]+
        phiBeta[7,riverDATA[evalRows[i]]]*lengthDATA[evalRows[i]]*flowDATA[timesByRow[evalRows[i],t],riverDATA[evalRows[i]]]^2
      phi[evalRows[i],t]<-1/(1+exp(-logitPhi[evalRows[i],t]))
    }
    
  }
  
#   for(t in 1:nTimes){
#     for(r in 1:nRivers){
#       for(g in 1:2){
#         logitPhi[t,r,g]<-phiBeta[1,r,g]+
#           phiBeta[2,r,g]*flowDATA[t,r]+phiBeta[3,r,g]*flowDATA[t,r]^2+
#           phiBeta[4,r,g]*tempDATA[t,r]
#         phi[t,r,g]<-1/(1+exp(-logitPhi[t,r,g]))
#       }
#     }
#   }
  
  
  for(i in 1:nEvalRows){
    # State of survival
    z[ evalRows[i] ] ~ dbern( survProb[ evalRows[i] ] ) #Do or don't suvive to i
    survProb[evalRows[i]] <-prod(phi[evalRows[i],1:nTimesByRow[evalRows[i]]])*
      z[ evalRows[i]-1 ]
    
#     survProb[evalRows[i]] <- prod(phi[time[evalRows[i]-1]:time[evalRows[i]],
#                                       riverDATA[evalRows[i]-1],
#                                       stageDATA[evalRows[i]-1]])*
#                                   z[ evalRows[i]-1 ]
    
    # Observation of live encounters
    encDATA[ evalRows[i] ] ~ dbern( obsProb[ evalRows[i] ] )
    
    obsProb[ evalRows[i] ]<- p[ evalRows[i]] * z[ evalRows[i] ]
    #* availableDATA[ evalRows[i]+1 ]                 # Must be on the study site to be capturable.
  }
}