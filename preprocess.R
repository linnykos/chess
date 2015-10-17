# setwd("C:/Users/Uikos/Dropbox/Powell - Kevin research/cos 597a/data")
# filenames = list.files()
setwd("C:/Users/Uikos/Dropbox/cos 597a/code")

all_dat = numeric(0)
for(i in 1:length(filenames)){
  all_dat = c(all_dat,readLines(filenames[i]))
  if(i%%floor(length(filenames)/10)==0) cat('*')
}

dim1 = length(all_dat)
header_line = grep("\\[",all_dat)
header_dat = all_dat[header_line]
tmp = 1:dim1
move_line = tmp[-header_line]
move_dat = all_dat[-header_line]

move_line_noempty = grep(".",move_dat)
tmp = 1:length(move_dat)
move_line_empty = tmp[-move_line_noempty]
#game_num = ceiling(length(move_line_empty)/2)
#game_num = 1859510

# take raw data and convert it into a moveMatrix
game_num = 399999
smallgame_num = 50000
idx = 1
stableidx = 1
moves = numeric(0)
moveVect = rep(NA,game_num)
outcomeVect = rep(NA,game_num)
moveMatrix = matrix(NA,ncol=500,nrow=smallgame_num)
num = 1
for(i in 1:length(move_dat)){
  if(nchar(move_dat[i])>1){
    lines = move_dat[i]
    tmp = strsplit(lines," ")
    tmp = tmp[[1]]
    tmp = unlist(strsplit(tmp,"\\."))
    #moves = c(moves,tmp[(1:length(tmp))%%3!=1])
    moves = c(moves,tmp)
  } else {
    if(length(moves)>1){
      len = length(moves)
      res = moves[len]
      moves = moves[-len]
      if(res=="*") { outcomeVect[idx] = -5} 
      else{
        res = unlist(strsplit(res,"-"))
        tmp = c(0,0)
        for(j in 1:2){
          if(res[j]=="1"){tmp[j] = 1}
        }
        outcomeVect[stableidx] = tmp[1] - tmp[2]
      }
      
      tmp = grep("[aA-zZ]",moves)
      moveVect[stableidx] = max(as.numeric(moves[-tmp]))
      moves = moves[tmp]
      moveMatrix[idx,1:length(moves)] = moves 
      stableidx = stableidx+1
      idx = idx+1
      if(stableidx > game_num) {
        save(moveMatrix,file=paste("moveMatrix_final_",num,".RData",sep=""))
        stop()
      }
      
      if(idx > smallgame_num){
        save(moveMatrix,file=paste("moveMatrix_final_",num,".RData",sep=""))
        num = num+1
        idx = 1
        moveMatrix = matrix(NA,ncol=500,nrow=smallgame_num)
      }
      #if(idx%%floor(game_num/10)==0) cat('*')
    }
    moves = numeric(0)
  }
  
  if(i%%floor(length(move_dat)/50)==0) {
    print(paste("i:",i,"//stableidx:",stableidx))
    save(moveMatrix,file="moveMatrix_final.RData")
    save(moveVect,file="moveVect_final.RData")
    save(outcomeVect,file="outcomeVect_final.RData")
  }
}

gamedatMatrix = matrix(NA,ncol=10,nrow=game_num)
header_datsplit = strsplit(header_dat, split = "\"*\"")
j=1
for(i in 1:game_num){
  boolvect = rep(1,10)
  bool = 1
  while(bool == 1){
    stringdat = header_datsplit[[j]]
    if(sum(grepl("Eve",stringdat))>0){
      if(boolvect[1] == 1){
        gamedatMatrix[i,1] = stringdat[length(stringdat)-1]
        boolvect[1] = 0
      } else {
        bool = 0
      }
      
    } else if(sum(grepl("Sit",stringdat))>0){
      if(boolvect[2] == 1){
        gamedatMatrix[i,2] = stringdat[length(stringdat)-1]
        boolvect[2] = 0
      } else {
        bool = 0
      }
      
    } else if(sum(grepl("Dat",stringdat))>0){
      if(boolvect[3] == 1){
        gamedatMatrix[i,3] = stringdat[length(stringdat)-1]
        boolvect[3] = 0
      } else {
        bool = 0
      }
      
    } else if(sum(grepl("Rou",stringdat))>0){
      if(boolvect[4] == 1){
        gamedatMatrix[i,4] = stringdat[length(stringdat)-1]
        boolvect[4] = 0
      } else {
        bool = 0
      }
      
    } else if(sum(grepl("WhiteElo",stringdat))>0){
      if(boolvect[8] == 1){
        gamedatMatrix[i,8] = stringdat[length(stringdat)-1]
        boolvect[8] = 0
      } else {
        bool = 0
      }
      
    } else if(sum(grepl("BlackElo",stringdat))>0){
      if(boolvect[9] == 1){
        gamedatMatrix[i,9] = stringdat[length(stringdat)-1]
        boolvect[9] = 0
      } else {
        bool = 0
      }
      
    } else if(sum(grepl("White",stringdat))>0){
      if(boolvect[5] == 1){
        gamedatMatrix[i,5] = stringdat[length(stringdat)-1]
        boolvect[5] = 0
      } else {
        bool = 0
      }
      
    } else if(sum(grepl("Black",stringdat))>0){
      if(boolvect[6] == 1){
        gamedatMatrix[i,6] = stringdat[length(stringdat)-1]
        boolvect[6] = 0
      } else {
        bool = 0
      }
      
    }else if(sum(grepl("Res",stringdat))>0){
      if(boolvect[7] == 1){
        gamedatMatrix[i,7] = stringdat[length(stringdat)-1]
        boolvect[7] = 0
      } else {
        bool = 0
      }
      
    } else if(sum(grepl("ECO",stringdat))>0){
      if(boolvect[10] == 1){
        gamedatMatrix[i,10] = stringdat[length(stringdat)-1]
        boolvect[10] = 0
      } else {
        bool = 0
      }
      
    }
    
    j = j+1
  }
  
  if(i%%floor(game_num/10)==0) cat('*')
}

tmp = unlist(tmp)
idx = grep("\\[",tmp)
gamedatlist = tmp[-idx]
colcounter = 1
rowcounter = 1
for(i in 1:length(gamedatlist)){
  tmp = gamedatlist[i]
  if(tmp=="]"){
    if(colcounter == 10){colcounter = 0; rowcounter = rowcounter + 1; 
                         if(rowcounter%%floor(game_num/10)==0) cat('*');}
    colcounter = colcounter + 1
    
    if(rowcounter > game_num) stop();
  } else {
    gamedatMatrix[rowcounter,colcounter] = tmp 
  }
}

openinglist = gamedatMatrix[,10]

ratioVect = rep(NA,game_num)
#the number of pieces captured by black within the first 10 captures
for(i in 1:game_num){
  game = moveMatrix[i,1:(moveVect[i]*2)]
  game = game[!is.na(game)]
  capture_turn = grep("x",game)
  if(length(capture_turn)>=10){
    capture_turn = capture_turn[1:10]
    ratioVect[i] = sum(capture_turn%%2==0)
  }
  if(i%%floor(game_num/10)==0) cat('*')
}