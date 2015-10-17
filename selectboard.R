#with gamedatMatrix, this selects the proper games that will be used and splits into the training and testing data set

playerList = gamedatMatrix[,5:6]
playerListuniq = sort(unique(as.vector(playerList)))
playerCount = rep(0,length(playerListuniq))
for(i in 1:dim(playerList)[1]){
  name1 = playerList[i,1]
  loc = which(playerListuniq==name1)
  playerCount[loc] = playerCount[loc]+1
  
  name2 = playerList[i,2]
  loc = which(playerListuniq==name2)
  playerCount[loc] = playerCount[loc]+1
  if(i%%floor(dim(playerList)[1]/10)==0) cat('*')
}

#save(playerListuniq,file="playerListuniq_large.RData")
#save(playerCount,file="playerCount.RData")
#length(playerCount[playerCount>1])

#now select the testset games
#three lists, 1) test set with players having more than 1 game each
#2) training set with players having more than 1 game each
#3) training set with players that might have only 1 game
maxgames = 500000
testGames = rep(NA,maxgames)
training1Games = rep(NA,maxgames)
training2Games = rep(NA,maxgames)
playerBool = rep(TRUE,length(playerListuniq))
playerCountActual = rep(0,length(playerListuniq))
testCounter = 1
training1Counter = 1
training2Counter = 1
for(i in 1:dim(playerList)[1]){
  name1 = playerList[i,1]
  name2 = playerList[i,2]
  if(!is.na(name1) & !is.na(name2)){
    loc1 = which(playerListuniq==name1)
    loc2 = which(playerListuniq==name2)
    count1 = playerCount[loc1]
    count2 = playerCount[loc2]
    if(testCounter<=maxgames & count1>1 & count2>1 & (playerBool[loc1]==TRUE | playerBool[loc2]==TRUE)){
      testGames[testCounter] = i
      testCounter = testCounter+1
      playerBool[loc1] = FALSE
      playerBool[loc2] = FALSE
      playerCountActual[loc1] = playerCountActual[loc1]+1
      playerCountActual[loc2] = playerCountActual[loc2]+1
    } else if(training1Counter<=maxgames & count1>1 & count2>1){
      training1Games[training1Counter] = i
      playerCountActual[loc1] = playerCountActual[loc1]+1
      playerCountActual[loc2] = playerCountActual[loc2]+1
      training1Counter = training1Counter+1
    } else if(training1Counter<=maxgames & (count1>1 | count2>1)){
      training2Games[training2Counter] = i
      playerCountActual[loc1] = playerCountActual[loc1]+1
      playerCountActual[loc2] = playerCountActual[loc2]+1
      training2Counter = training2Counter+1
    } 
  }

  if(i%%floor(dim(playerList)[1]/50)==0) {
    print(i)
    save(testGames,file="testGames_large.RData")
    save(training1Games,file="training1Games_large.RData")
    save(training2Games,file="training2Games_large.RData")
    save(playerCountActual,file="playerCountActual.RData")
    save(playerBool,file="playerBool_large.RData")
    save(i,file="progress.RData")
  }
}

##########
testGames = testGames[!is.na(testGames)]
training1Games = training1Games[!is.na(training1Games)]
training2Games = training2Games[!is.na(training2Games)]
allgames = c(testGames,training1Games)
playerList = gamedatMatrix[allgames,5:6]
playerListuniq = sort(unique(as.vector(playerList)))
playerCount_training = rep(0,length(playerListuniq))

playerList_training = gamedatMatrix[training1Games,5:6]
for(i in 9455:dim(playerList_training)[1]){
  name1 = playerList_training[i,1]
  loc = which(playerListuniq==name1)
  playerCount_training[loc] = playerCount_training[loc]+1
  
  name2 = playerList_training[i,2]
  loc = which(playerListuniq==name2)
  playerCount_training[loc] = playerCount_training[loc]+1
  
  if(i%%floor(dim(playerList_training)[1]/50)==0) print(i)
}

###########################
trainingGamesNew = rep(NA,maxgames)
tol = 6
counter = 1
playerListIdx = matrix(NA,nrow=dim(playerList_training)[1],ncol=2)
for(i in 1:dim(playerList_training)[1]){
  name1 = playerList_training[i,1]
  loc1 = which(playerListuniq==name1)
  name2 = playerList_training[i,2]
  loc2 = which(playerListuniq==name2)
  if(is.na(loc1)|is.na(loc2)) stop()
  playerListIdx[i,] = c(loc1,loc2)
#   
#   if(playerCount_training[loc1]>=tol & playerCount_training[loc2]>=tol){
#     trainingGamesNew[counter] = training1Games[i]
#     counter = counter+1
#   }
  
  if(i%%floor(dim(playerList_training)[1]/50)==0) {
    print(i)
    save(playerListIdx, file="playerListIdx.RData")
    save(trainingGamesNew, file = "trainingGamesNew.RData")
    save(counter, file = "progress.RData")
  }
}

########################
#same as above but using the newly constructed playerListIdx
trainingGamesNew = rep(NA,maxgames)
tol = 100
playerCount_new = rep(0,length(playerListuniq))
counter = 1
for(i in 1:dim(playerList_training)[1]){
  loc1 = playerListIdx[i,1]
  loc2 = playerListIdx[i,2]
  
  if(playerCount_training[loc1]>=tol & playerCount_training[loc2]>=tol){
    trainingGamesNew[counter] = training1Games[i]
    playerCount_new[loc1] = playerCount_new[loc1]+1
    playerCount_new[loc2] = playerCount_new[loc2]+1
    counter = counter+1
  }
  
  if(i%%floor(dim(playerList_training)[1]/10)==0) {
    print(i)
  }
}

##############################
#SOMETHING WENT WRONG, BUT IT'S OKAY. We now have a list of players so we can reverse-engineer the games
playerListuniq_final = playerListuniq[playerCount_new>0]

playerList = gamedatMatrix[,5:6]
playerCountTest_final = rep(0,length(playerListuniq_final))
playerCountTrain_final = rep(0,length(playerListuniq_final))
playerBool_final = rep(TRUE,length(playerListuniq_final))
testingGames_final = rep(NA,maxgames)
trainingGames_final = rep(NA,maxgames)
testCounter = 1
trainCounter = 1
for(i in 1:400000){
  name1 = playerList[i,1]
  loc1 = which(playerListuniq_final==name1)
  name2 = playerList[i,2]
  loc2 = which(playerListuniq_final==name2)
  
  if(length(loc1)>0 & length(loc2)>0){
    if(playerBool_final[loc1]==TRUE | playerBool_final[loc2]==TRUE){
      testingGames_final[testCounter] = i
      playerBool_final[loc1] = FALSE
      playerBool_final[loc2] = FALSE
      playerCountTest_final[loc1] = playerCountTest_final[loc1]+1
      playerCountTest_final[loc2] = playerCountTest_final[loc2]+1
      testCounter = testCounter+1
    } else {
      trainingGames_final[trainCounter] = i
      playerCountTrain_final[loc1] = playerCountTrain_final[loc1]+1
      playerCountTrain_final[loc2] = playerCountTrain_final[loc2]+1
      trainCounter = trainCounter+1
    }
  }
  
  if(i%%floor(dim(playerList_training)[1]/10)==0) {
    print(i)
    save(playerCountTest_final,file="playerCountTest_final.RData")
    save(playerCountTrain_final,file="playerCountTrain_final.RData")
    save(testingGames_final,file="testingGames_final.RData")
    save(trainingGames_final,file="trainingGames_final.RData")
  }
}

testingGames_final = testingGames_final[!is.na(testingGames_final)]
trainingGames_final = trainingGames_final[!is.na(trainingGames_final)]
save(testingGames_final,file="testingGames_final.RData")
save(trainingGames_final,file="trainingGames_final.RData")
allGames_final = c(testingGames_final,trainingGames_final)
allGames_final = sort(allGames_final)
save(allGames_final,file="allGames_final.RData")


##############
#construct /the/ gamedatMatrix that we will be using
gamedatMatrix_test = gamedatMatrix[testingGames_final,]
gamedatMatrix_train = gamedatMatrix[trainingGames_final,]
save(gamedatMatrix_test,file="gamedatMatrix_test.RData")
save(gamedatMatrix_train,file="gamedatMatrix_train.RData")

#with all 8 moveMatrix's, construct /the/ moveMatrix
game_num = 399999
smallgame_num = 50000
tmpgames = trainingGames_final
moveMatrix_train = matrix(NA,ncol=500,nrow=length(trainingGames_final))
counter = 1
for(i in 1:ceiling(game_num/smallgame_num)){
  load(paste("moveMatrix_final_",i,".RData",sep=""))
  tmp = tmpgames[tmpgames<=smallgame_num]
  moveMatrix_train[counter:(counter+length(tmp)-1),] = moveMatrix[tmp,]
  print(counter)
  counter = counter+length(tmp)
  tmpgames = tmpgames[tmpgames>smallgame_num]-smallgame_num
}
tmpgames = testingGames_final
moveMatrix_test = matrix(NA,ncol=500,nrow=length(testingGames_final))
counter = 1
for(i in 1:ceiling(game_num/smallgame_num)){
  load(paste("moveMatrix_final_",i,".RData",sep=""))
  tmp = tmpgames[tmpgames<=smallgame_num]
  if(length(tmp)>0){
    moveMatrix_test[counter:(counter+length(tmp)-1),] = moveMatrix[tmp,]
  }
  print(counter)
  counter = counter+length(tmp)
  tmpgames = tmpgames[tmpgames>smallgame_num]-smallgame_num
}
turnVect_train = moveVect[trainingGames_final]
outcomeVect_train = outcomeVect[trainingGames_final]
