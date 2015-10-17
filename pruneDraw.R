outcomeIdx = which(outcomeVect_train==0)
outcomeVect_train = outcomeVect_train[-outcomeIdx]
outcomevect_train_adj = outcomeVect_train*100
matchlist_final = matchlist_final[,-outcomeIdx]
numBoards = length(outcomeVect_train)

MUFIXED = c(1,2,.1,-1,2,1,2,-1.5)
set.seed(1)
TOTALITER = 100000
batchsize = 100
tau = 1
kappa = .9

pmatrixVI = matrix(rep(MUFIXED,numPlayers),ncol=8,nrow=numPlayers,byrow=T)
sigmatrixVI = list()
for(i in 1:numPlayers){
  sigmatrixVI[[i]] = diag(8)
}
muglobalVI = MUFIXED
sigmaglobalVI = diag(8)*1/(numPlayers+1)
elbo = rep(NA,TOTALITER)
PRIORGUESS = MUFIXED

#repeat until convergence
for(k in 1:TOTALITER){
  minibatch = sample(1:numPlayers,batchsize)
  for(n in 1:batchsize){
    i = minibatch[n]
    tmpinvpre = diag(8)
    tmpvect = muglobalVI
    
    gamelistwhite = which(matchlist_final[1,]==i)
    gamelistblack = which(matchlist_final[2,]==i)
    
    if(length(gamelistwhite)>0){
      for(j in 1:length(gamelistwhite)){
        tmpinvpre = tmpinvpre + pmatrixVI[matchlist_final[2,gamelistwhite[j]],]%*%t(pmatrixVI[matchlist_final[2,gamelistwhite[j]],])
        tmpvect = tmpvect + outcomevect_train_adj[gamelistwhite[j]]*pmatrixVI[matchlist_final[2,gamelistwhite[j]],]
      }
    }
    
    if(length(gamelistblack)>0){
      for(j in 1:length(gamelistblack)){
        tmpinvpre = tmpinvpre + pmatrixVI[matchlist_final[1,gamelistblack[j]],]%*%t(pmatrixVI[matchlist_final[1,gamelistblack[j]],])
        tmpvect = tmpvect + outcomevect_train_adj[gamelistblack[j]]*pmatrixVI[matchlist_final[1,gamelistblack[j]],]
      }
    }
    
    sigmatrixVI[[i]] = solve(tmpinvpre)
    pmatrixVI[i,] = sigmatrixVI[[i]]%*%tmpvect
  }
  
  muglobal_intermediate = 1/(batchsize+1)*(apply(pmatrixVI[minibatch,],2,sum)+MUFIXED)
  stepsize = (k+tau)^(-kappa)
  muglobalVI = (1-stepsize)*muglobalVI + stepsize*muglobal_intermediate
  
  #calculate ELBO
  if(k%%50==0){
    elboval = 0
    for(j in 1:numPlayers){
      elboval = elboval - .5*tr(sigmatrixVI[[j]]+pmatrixVI[j,]%*%t(pmatrixVI[j,]))
      elboval = elboval + t(muglobalVI)%*%pmatrixVI[j,]
      elboval = elboval - .5*log(det(sigmatrixVI[[j]]))
    }
    elboval = elboval - .5*(numPlayers+1)*tr(sigmaglobalVI+muglobalVI%*%t(muglobalVI))
    elboval = elboval + muglobalVI%*%MUFIXED
    for(j in 1:numBoards){
      i1 = matchlist_final[1,j]
      i2 = matchlist_final[2,j]
      elboval = elboval - .5*tr((sigmatrixVI[[i1]]+pmatrixVI[i1,]%*%t(pmatrixVI[i1,]))%*%(sigmatrixVI[[i2]]+pmatrixVI[i2,]%*%t(pmatrixVI[i2,])))
      elboval = elboval + outcomevect_train_adj[j]*(t(pmatrixVI[i2,])%*%pmatrixVI[i1,])
    }
    
    elbo[k] = elboval
    save(muglobalVI,file="m1_nodraw_muglobalVI.RData")
    save(pmatrixVI,file="m1_nodraw_pmatrixVI.RData")
    save(sigmaglobalVI,file="m1_nodraw_sigmaglobalVI.RData")
    save(elbo,file="m1_nodraw_elbo.RData")
    save(k,file="progress.RData")
  }
  
  print(k)
  #if(k%%floor(TOTALITER/10)==0) cat('*')
}