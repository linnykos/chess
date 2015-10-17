setwd("C:/Users/Uikos/Dropbox/cos 597a/code")
#setwd("C:/Users/UikosPC/Dropbox/cos 597a/code")

# #0 is draw, 1 is white wins, -1 is black wins
# outcomeVect_train = gamedatMatrix_train[,7]
# tmp = strsplit(outcomeVect_train,'/')
# for(i in 1:dim(gamedatMatrix_train)[1]){
#   if(length(tmp[[i]])>1 | tmp[[i]][1] == "*") {
#     outcomeVect_train[i] = 0
#   } else {
#     res = strsplit(tmp[[i]],'-')
#     if(as.numeric(res[[1]][1])==0){
#       outcomeVect_train[i] = -1
#     } else if(as.numeric(res[[1]][1])==1){
#       outcomeVect_train[i] = 1
#     } else {
#       stop('ERROR: UNKNOWN OUTCOME')
#     }
#   }
# }

# HOW TO USE: res = main_generateBoard(10000,10)
main_generateBoard <- function(numBoards,pieces){
  boardMatrixList = list()
  boardnumList = rep(NA,numBoards)
  counter = 1
  i = 1
  while(i<=numBoards & counter<=numBoards){
    print(paste(counter,i,sep=" "))
    tmp = generateBoard.pieces(counter,pieces)
    if (tmp$bad == FALSE) {
      boardMatrixList[[i]] = tmp$board
      boardnumList[i] = counter
      i = i+1
    } 
    counter = counter+1
    #if(counter==34905) counter = counter+1 #invalid queens
    #if(counter==56572) counter = counter+1 #invalid rook (should be able to move but doesn't)
    
    #if(i%%floor(numBoards/10)==0) cat('*')
  }
  
  return(list(boardMatrixList = boardMatrixList, boardnumList = boardnumList))
}

#pawn = 1, knight = 2, bishop = 3, rook = 4, queen = 5, king = 6
generateBoard.pieces <- function(game,pieces,verbose=FALSE){
  board = matrix(0,ncol=8,nrow=8)
  whitepieces = c(8,2,2,2,1)
  blackpieces = c(8,2,2,2,1)
  
  turnLength = sum(!is.na(moveMatrix[game,]))
  moveVect = moveMatrix[game,1:turnLength]
  tmp = grep("x",moveVect)
  if (length(tmp)>=pieces) moveVect = moveVect[1:tmp[pieces]]
  if (length(tmp)<pieces) {
    #print(paste("TOO FEW",game))
    return(list(board = board, whitepieces = whitepieces, blackpieces = blackpieces, bad = TRUE))
  }
  
  
  startVect = c(4,2,3,5,6,3,2,4)
  board[1,] = startVect
  board[2,] = rep(1,8)
  board[7,] = rep(-1,8)
  board[8,] = -startVect
  
  
  whiteturn = T

  for(i in 1:length(moveVect)){
    move = moveVect[i]
    if(verbose==TRUE) cat(paste(game, ": turn ", i,": ",move,'\n',sep=""))
    movesplit = unlist(strsplit(move,""))
    if(!grepl("O",move)){
      tmp = gregexpr(pattern ="[0-9]",move)
      tmp = tmp[[1]]
      tmp = tmp[length(tmp)]
      letterpos = match(movesplit[tmp-1],letters)
      location = c(letterpos,as.numeric(movesplit[tmp]))
    }
    
    if(grepl("x",move)){
      piece = board[location[2],location[1]]
      
      #account for en passant
      if(piece == 0){
        if(whiteturn) {indic = 1} else {indic = -1}
        if(board[location[2]-indic,location[1]]==-indic){
          if(whiteturn){
            blackpieces[1] = blackpieces[1] - 1
          } else {
            whitepieces[1] = whitepieces[1] - 1
          }
          board[location[2]-indic,location[1]] = 0
        }
        
      } else {
        if((sign(piece)+1)/2==whiteturn) stop("Attack: Invalid piece")
        if(whiteturn){
          blackpieces[abs(piece)] = blackpieces[abs(piece)] - 1
          if(blackpieces[abs(piece)]<0) stop("Attack: Black negative piece")
        } else{
          whitepieces[abs(piece)] = whitepieces[abs(piece)] - 1
          if(whitepieces[abs(piece)]<0) stop("Attack: White negative piece")
        }
        
        board[location[2],location[1]] = NA
      }
      
    }
    
    if(grepl("O",move)){
      board = castleFunc(board,movesplit,whiteturn)
      
    } else if (grepl("=",move)) {
      res = promoteFunc(board,location,move,movesplit,whiteturn,whitepieces,blackpieces)
      board = res$board
      whitepieces = res$whitepieces
      blackpieces = res$blackpieces
      
    } else if (grepl("N",move)) {
      board = knightFunc(board,location,move,movesplit,whiteturn)
      
    } else if (grepl("B",move)) {
      board = bishopFunc(board,location,move,movesplit,whiteturn)
      
    } else if (grepl("R",move)) {
      board = rookFunc(board,location,move,movesplit,whiteturn)
      
    } else if (grepl("Q",move)){
      board = queenFunc(board,location,move,movesplit,whiteturn)
      
    } else if (grepl("K",move)){
      board = kingFunc(board,location,move,movesplit,whiteturn)
      
    } else {
      board = pawnFunc(board,location,move,movesplit,whiteturn)
    }
    
    whiteturn = !whiteturn
    if(sum(is.na(board))>0) stop("ERROR")
    if(verbose==TRUE) print(board)
  }
  
  return(list(board = board, whitepieces = whitepieces, blackpieces = blackpieces, bad = FALSE))
}

###############################################

castleFunc <- function(board,movesplit,whiteturn){
  if(whiteturn){
    if(length(movesplit)>=5){
      if(sum(board[1,2:4]!=0)) stop("Castle 1");
      if(sum(board[1,1]!=4)) stop("Castle 2");
      if(sum(board[1,5]!=6)) stop("Castle 3");
      board[1,1] = 0
      board[1,5] = 0
      board[1,3] = 6
      board[1,4] = 4
    } else {
      if(sum(board[1,6:7]!=0)) stop("Castle 5");
      if(sum(board[1,8]!=4)) stop("Castle 6");
      if(sum(board[1,5]!=6)) stop("Castle 7");
      board[1,8] = 0
      board[1,5] = 0
      board[1,7] = 6
      board[1,6] = 4
    }
  } else {
    if(length(movesplit)>=5){
      if(sum(board[8,2:4]!=0)) stop("Castle 8");
      if(sum(board[8,1]!=-4)) stop("Castle 9");
      if(sum(board[8,5]!=-6)) stop("Castle 10");
      board[8,1] = 0
      board[8,5] = 0
      board[8,3] = -6
      board[8,4] = -4
    } else {
      if(sum(board[8,6:7]!=0)) stop("Castle 12");
      if(sum(board[8,8]!=-4)) stop("Castle 13");
      if(sum(board[8,5]!=-6)) stop("Castle 14");
      board[8,8] = 0
      board[8,5] = 0
      board[8,7] = -6
      board[8,6] = -4
    }
  }
  
  return(board)
}

promoteFunc <- function(board,location,move,movesplit,whiteturn,whitepieces,blackpieces){
  if(whiteturn) {indic = 1} else {indic = -1}
  
  if(whiteturn & location[2]!=8) stop("Promote: White invalid")
  if(!whiteturn & location[2]!=1) stop("Promote: Black invalid")
  
  #determine current location of pawn
  if(whiteturn){
    whitepieces[1] = whitepieces[1]-1
    if(grepl("x",move)){
      tmp = match(movesplit[1],letters)
      if(is.na(tmp)) stop("Promote: bad notation - white")
      if(board[7,tmp]!=1) stop("Promote: no pawn - white attack")
      board[7,tmp] = 0
    } else {
      if(board[7,location[1]]!=1) stop("Promote: no pawn - white")
      board[7,location[1]] = 0
    }
  } else {
    blackpieces[1] = blackpieces[1]-1
    if(grepl("x",move)){
      tmp = match(movesplit[1],letters)
      if(is.na(tmp)) stop("Promote: bad notation - black")
      if(board[2,tmp]!=-1) stop("Promote: no pawn - black attack")
      board[2,tmp] = 0
    } else {
      if(board[2,location[1]]!=-1) stop("Promote: no pawn - black")
      board[2,location[1]] = 0
    }
  }
  
  
  if (grepl("N",move)) {
    board[location[2],location[1]] = indic*2
    if(whiteturn){
      whitepieces[2] = whitepieces[2]+1
    } else {
      blackpieces[2] = blackpieces[2]+1
    }
    
  } else if (grepl("B",move)) {
    board[location[2],location[1]] = indic*3
    if(whiteturn){
      whitepieces[3] = whitepieces[3]+1
    } else {
      blackpieces[3] = blackpieces[3]+1
    }
    
  } else if (grepl("R",move)) {
    board[location[2],location[1]] = indic*4
    if(whiteturn){
      whitepieces[4] = whitepieces[4]+1
    } else {
      blackpieces[4] = blackpieces[4]+1
    }
    
  } else if (grepl("Q",move)){
    board[location[2],location[1]] = indic*5
    if(whiteturn){
      whitepieces[5] = whitepieces[5]+1
    } else {
      blackpieces[5] = blackpieces[5]+1
    }
    
  } else {
    stop("Promote: Invalid piece to promote to")
  }
  
  return(list(board=board,whitepieces = whitepieces,blackpieces = blackpieces))
}

knightFunc <- function(board,location,move,movesplit,whiteturn){
  if(whiteturn) {indic = 1} else {indic = -1}
  
  pos = rep(NA,8)
  
  locChangeMatrix = c(-1,-2,
                -2,-1,
                -1, 2,
                -2, 1,
                 1,-2,
                 2,-1,
                 1, 2,
                 2, 1)
  locChangeMatrix = matrix(locChangeMatrix,ncol=2,nrow=8,byrow=T)
  locMatrix = matrix(NA,ncol=2,nrow=8)
  
  for(i in 1:8){
    locMatrix[i,] = location + locChangeMatrix[i,]
    if(sum(locMatrix[i,]<1)>0) next()
    if(sum(locMatrix[i,]>8)>0) next()
    
    pos[i] = board[locMatrix[i,2],locMatrix[i,1]]
  }

  tmpidx = which(pos==indic*2)
  if(length(tmpidx)==0) stop("Knight: No pieces")
  if(length(tmpidx)>2) stop("Knight: Too many pieces")
  if(length(tmpidx)==2){
    # check to see if one knight is defending check
    defendcheckBool = rep(0,2)
    kingloc = which(board==indic*6, arr.ind=TRUE)
    for(i in 1:2){
      tmpcheckloc = kingloc
      passKnightBool = FALSE #a variable to indicator whether we have went past the knight yet
      #check the position of the knight to the king
      if (kingloc[1]==locMatrix[tmpidx[i],2]) { #share the same column
        if(kingloc[2] > locMatrix[tmpidx[i],1]){ 
          while(tmpcheckloc[2]>0){ #make sure it's within bounds of the board
            if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
              break()
            } else if(passKnightBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*4 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
              defendcheckBool[i] = -1 #flag to indicate knight is defending check
              break()
            } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*2 & !passKnightBool){
              passKnightBool = TRUE
            } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
              break()
            } 
            tmpcheckloc[2] = tmpcheckloc[2]-1 #check next square
          }
        } else {
          while(tmpcheckloc[2]<9){
            if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
              break()
            } else if(passKnightBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*4 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
              defendcheckBool[i] = -1
              break()
            } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*2 & !passKnightBool){
              passKnightBool = TRUE
            } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
              break()
            } 
            tmpcheckloc[2] = tmpcheckloc[2]+1
          }
        }
        
      } else if (kingloc[2]==locMatrix[tmpidx[i],1]){ #share the same row
        if (kingloc[1]>locMatrix[tmpidx[i],2]){
          while(tmpcheckloc[1]>0){
            if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
              break()
            } else if(passKnightBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*4 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
              defendcheckBool[i] = -1
              break()
            } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*2 & !passKnightBool){
              passKnightBool = TRUE
            } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
              break()
            } 
            tmpcheckloc[1] = tmpcheckloc[1]-1
          }
        } else {
          while(tmpcheckloc[1]<9){
            if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
              break()
            } else if(passKnightBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*4 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
              defendcheckBool[i] = -1
              break()
            } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*2 & !passKnightBool){
              passKnightBool = TRUE
            } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
              break()
            } 
            tmpcheckloc[1] = tmpcheckloc[1]+1
          }
        }
        
      } else if ((kingloc[1]<locMatrix[tmpidx[i],2] & kingloc[2]<locMatrix[tmpidx[i],1]) | (kingloc[1]>locMatrix[tmpidx[i],2] & kingloc[2]>locMatrix[tmpidx[i],1])){ 
        if (abs(kingloc[1]-locMatrix[tmpidx[i],2])==abs(kingloc[2]-locMatrix[tmpidx[i],1])){ #make sure the knight and king actually are on the same diagonal
          #knight is either topleft to king or bottomright
          if (kingloc[1]<locMatrix[tmpidx[i],2] & kingloc[2]<locMatrix[tmpidx[i],1]){
            while(tmpcheckloc[1]<9 & tmpcheckloc[2]<9){
              if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                break()
              } else if(passKnightBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*3 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                defendcheckBool[i] = -1
              } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*2 & !passKnightBool){
                passKnightBool = TRUE
              } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                break()
              } 
              tmpcheckloc[1] = tmpcheckloc[1]+1
              tmpcheckloc[2] = tmpcheckloc[2]+1
            }
          } else {
            while(tmpcheckloc[1]>0 & tmpcheckloc[2]>0){
              if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                break()
              } else if(passKnightBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*3 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                defendcheckBool[i] = -1
              } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*2 & !passKnightBool){
                passKnightBool = TRUE
              } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                break()
              } 
              tmpcheckloc[1] = tmpcheckloc[1]-1
              tmpcheckloc[2] = tmpcheckloc[2]-1
            }
          }
        } 
        
      } else if ((kingloc[1]>locMatrix[tmpidx[i],2] & kingloc[2]<locMatrix[tmpidx[i],1]) | (kingloc[1]<locMatrix[tmpidx[i],2] & kingloc[2]>locMatrix[tmpidx[i],1])){
        #knight is either topright to king or bottomleft
        if (abs(kingloc[1]-locMatrix[tmpidx[i],2])==abs(kingloc[2]-locMatrix[tmpidx[i],1])){
          if (kingloc[1]>locMatrix[tmpidx[i],2] & kingloc[2]<locMatrix[tmpidx[i],1]){
            while(tmpcheckloc[1]>0 & tmpcheckloc[2]<9){
              if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                break()
              } else if(passKnightBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*3 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                defendcheckBool[i] = -1
              } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*2 & !passKnightBool){
                passKnightBool = TRUE
              } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                break()
              } 
              tmpcheckloc[1] = tmpcheckloc[1]-1
              tmpcheckloc[2] = tmpcheckloc[2]+1
            }
          } else {
            while(tmpcheckloc[1]<9 & tmpcheckloc[2]>0){
              if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                break()
              } else if(passKnightBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*3 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                defendcheckBool[i] = -1
              } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*2 & !passKnightBool){
                passKnightBool = TRUE
              } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                break()
              } 
              tmpcheckloc[1] = tmpcheckloc[1]+1
              tmpcheckloc[2] = tmpcheckloc[2]-1
            }
          }
        }
        
      } else {
        stop("Knight: Detection of Position failed")
      }
    }
    tmpcorrectknight = which(defendcheckBool == 0)
    if(length(tmpcorrectknight)<1) stop("Knight: both knight seem to be defending check")
    if(length(tmpcorrectknight)==1){
      tmpidx = tmpidx[tmpcorrectknight]
      board[locMatrix[tmpidx,2],locMatrix[tmpidx,1]] = 0
      
    } else {
      # else look at notation
      tmplet = movesplit[2]
      tmp = match(tmplet,letters)
      if(is.na(tmp)){
        #is a number
        tmp = as.numeric(tmplet)
        tmpwhich = which(locMatrix[,2]==tmp)
        tmppos = which(pos[tmpwhich]==indic*2)
        if(length(tmppos)!=1) stop("Knight: Special notation - number")
      } else {
        #is a letter
        tmpwhich = which(locMatrix[,1]==tmp)
        tmppos = which(pos[tmpwhich]==indic*2)
        if(length(tmppos)!=1) stop("Knight: Special notation - letter")
      }
      board[locMatrix[tmpwhich[tmppos],2],locMatrix[tmpwhich[tmppos],1]] = 0
    } 
  } else {
    board[locMatrix[tmpidx,2],locMatrix[tmpidx,1]] = 0
  }
  
  board[location[2],location[1]] = indic*2
  
  return(board)
}

bishopFunc <- function(board,location,move,movesplit,whiteturn){
  if(whiteturn) {indic = 1} else {indic = -1}
  colSq = sum(location)%%2 #if 0, black. if 1, white
  
  tmpidx = which(board == indic*3, arr.ind = T)
  
  if(length(tmpidx)==0) stop("Bishop: No Bishop")
  if(dim(tmpidx)[1]>2) stop("Bishop: Too many bishops")
  if(dim(tmpidx)[1]==2){
    tmpcol = apply(tmpidx,1,modtwo)
    tmp1 = which(tmpcol==colSq)
    if(length(tmp1)>1) stop("Bishop: Multiple biships on same color")
    board[tmpidx[tmp1,1],tmpidx[tmp1,2]] = 0
  } else {
    tmp1 = 1
    board[tmpidx[1,1],tmpidx[1,2]] = 0
  }
  
  dist = abs(tmpidx[tmp1,1] - location[2]) #remember location is alpha then num
  xdir = sign(tmpidx[tmp1,2]-location[1])
  ydir = sign(tmpidx[tmp1,1]-location[2])
  
  tmploc = tmpidx[tmp1,]-c(ydir,xdir)
  if(dist>1){
    for(i in 1:(dist-1)){
      if(board[tmploc[1],tmploc[2]]!=0) { cat(tmploc); stop("Bishop: pieces in way")}
      tmploc = tmpidx[tmp1,]-c(ydir,xdir)
    }
  }
  
  board[location[2],location[1]] = indic*3
  
  return(board)
}

rookFunc <- function(board,location,move,movesplit,whiteturn){
  if(whiteturn) {indic = 1} else {indic = -1}
  
  tmpidx = which(board == indic*4, arr.ind = T)
  
  if(length(tmpidx)==0) stop("Rook: no rooks")
  if(dim(tmpidx)[1]==2){
    tmpbool = rep(0,2)
    for(i in 1:2){
      tmpbool[i] = (tmpidx[i,1]==location[2] || tmpidx[i,2]==location[1])
    }
    
    if(sum(tmpbool)==0) stop("Rook: no possible piece to move correctly")
    if(sum(tmpbool)>1){
      #check to see if both rooks can reach that square
      for(i in 1:2){
        hor = T
        if(tmpidx[i,1]!=location[2]) hor = F
        dist = abs(tmpidx[i,1]-location[2]) + abs(tmpidx[i,2]-location[1])
        if(dist==0) stop("Rook: distance 0")
        if(dist>1){
          if(hor){
            direct = sign(location[1]-tmpidx[i,2])
            tmpbool[i] = (length(which(board[location[2],location[1]:(tmpidx[i,2]+direct)]!=0))==0)
          } else {
            direct = sign(location[2]-tmpidx[i,1])
            tmpbool[i] = (length(which(board[location[2]:(tmpidx[i,1]+direct),location[1]]!=0))==0)
          }
        }
      }
      
      
      if(sum(tmpbool)>1){
        if(sum(tmpbool)>2) stop("Rook: Too many possible rooks")
        
        # check to see if one rook is defending check
        defendcheckBool = rep(0,2)
        kingloc = which(board==indic*6, arr.ind=TRUE)
        for(i in 1:2){
          tmpcheckloc = kingloc
          passRookBool = FALSE #a variable to indicator whether we have went past the rook yet
          #check the position of the rook to the king
          if (kingloc[1]==tmpidx[i,1]) { #share the same column
            if(kingloc[2] > tmpidx[i,2]){ 
              while(tmpcheckloc[2]>0){ #make sure it's within bounds of the board
                if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                  break()
                }else if(passRookBool & board[tmpcheckloc[1],tmpcheckloc[2]]== indic*4){
                  break()
                }else if(passRookBool &(board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*4 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                  defendcheckBool[i] = -1 #flag to indicate rook is defending check
                  break()
                } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*4 & !passRookBool){
                  passRookBool = TRUE
                } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                  break()
                } 
                tmpcheckloc[2] = tmpcheckloc[2]-1 #check next square
              }
            } else {
              while(tmpcheckloc[2]<9){
                if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                  break()
                } else if(passRookBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*4 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                  defendcheckBool[i] = -1
                  break()
                } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*4 & !passRookBool){
                  passRookBool = TRUE
                } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                  break()
                } 
                tmpcheckloc[2] = tmpcheckloc[2]+1
              }
            }
            
          } else if (kingloc[2]==tmpidx[i,2]){ #share the same row
            if (kingloc[1]>tmpidx[i,1]){
              while(tmpcheckloc[1]>0){
                if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                  break()
                } else if(passRookBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*4 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                  defendcheckBool[i] = -1
                  break()
                } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*4 & !passRookBool){
                  passRookBool = TRUE
                } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                  break()
                } 
                tmpcheckloc[1] = tmpcheckloc[1]-1
              }
            } else {
              while(tmpcheckloc[1]<9){
                if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                  break()
                } else if(passRookBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*4 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                  defendcheckBool[i] = -1
                  break()
                } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*4 & !passRookBool){
                  passRookBool = TRUE
                } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                  break()
                } 
                tmpcheckloc[1] = tmpcheckloc[1]+1
              }
            }
            
          } else if ((kingloc[1]<tmpidx[i,1] & kingloc[2]<tmpidx[i,2]) | (kingloc[1]>tmpidx[i,1] & kingloc[2]>tmpidx[i,2])){ 
            if (abs(kingloc[1]-tmpidx[i,1])==abs(kingloc[2]-tmpidx[i,2])){ #make sure the rook and king actually are on the same diagonal
              #rook is either topleft to king or bottomright
              if (kingloc[1]<tmpidx[i,1] & kingloc[2]<tmpidx[i,2]){
                while(tmpcheckloc[1]<9 & tmpcheckloc[2]<9){
                  if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                    break()
                  } else if(passRookBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*3 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                    defendcheckBool[i] = -1
                  } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*4 & !passRookBool){
                    passRookBool = TRUE
                  } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                    break()
                  } 
                  tmpcheckloc[1] = tmpcheckloc[1]+1
                  tmpcheckloc[2] = tmpcheckloc[2]+1
                }
              } else {
                while(tmpcheckloc[1]>0 & tmpcheckloc[2]>0){
                  if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                    break()
                  } else if(passRookBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*3 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                    defendcheckBool[i] = -1
                  } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*4 & !passRookBool){
                    passRookBool = TRUE
                  } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                    break()
                  } 
                  tmpcheckloc[1] = tmpcheckloc[1]-1
                  tmpcheckloc[2] = tmpcheckloc[2]-1
                }
              }
            } 
            
          } else if ((kingloc[1]>tmpidx[i,1] & kingloc[2]<tmpidx[i,2]) | (kingloc[1]<tmpidx[i,1] & kingloc[2]>tmpidx[i,2])){
            #rook is either topright to king or bottomleft
            if (abs(kingloc[1]-tmpidx[i,1])==abs(kingloc[2]-tmpidx[i,2])){
              if (kingloc[1]>tmpidx[i,1] & kingloc[2]<tmpidx[i,2]){
                while(tmpcheckloc[1]>0 & tmpcheckloc[2]<9){
                  if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                    break()
                  } else if(passRookBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*3 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                    defendcheckBool[i] = -1
                  } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*4 & !passRookBool){
                    passRookBool = TRUE
                  } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                    break()
                  } 
                  tmpcheckloc[1] = tmpcheckloc[1]-1
                  tmpcheckloc[2] = tmpcheckloc[2]+1
                }
              } else {
                while(tmpcheckloc[1]<9 & tmpcheckloc[2]>0){
                  if (is.na(board[tmpcheckloc[1],tmpcheckloc[2]])){
                    break()
                  } else if(passRookBool & (board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*3 | board[tmpcheckloc[1],tmpcheckloc[2]]==-indic*5)){
                    defendcheckBool[i] = -1
                  } else if (board[tmpcheckloc[1],tmpcheckloc[2]]==indic*4 & !passRookBool){
                    passRookBool = TRUE
                  } else if (board[tmpcheckloc[1],tmpcheckloc[2]]!=indic*6 & board[tmpcheckloc[1],tmpcheckloc[2]]!=0) {
                    break()
                  } 
                  tmpcheckloc[1] = tmpcheckloc[1]+1
                  tmpcheckloc[2] = tmpcheckloc[2]-1
                }
              }
            }
            
          } else {
            stop("Rook: Detection of Position failed")
          }
        }
        
        tmpcorrectrook = which(defendcheckBool == 0)
        if(length(tmpcorrectrook)<1) stop("Rook: both rook seem to be defending check")
        if(length(tmpcorrectrook)==1){
          board[tmpidx[tmpcorrectrook,1],tmpidx[tmpcorrectrook,2]] = 0
          
        } else {
          #both pieces can get there, so look at the notation
          tmplet = movesplit[2]
          
          tmp = match(tmplet,letters)
          if(is.na(tmp)){
            #is a number, specifies a row
            tmp = as.numeric(tmplet)
            tmppos = which(tmpidx[,1]==tmp)
          } else {
            #is a letter, specifies a col
            tmppos = which(tmpidx[,2]==tmp)
          }
          if(length(tmppos)==0) stop("Rook: special notation makes no sense")
          board[tmpidx[tmppos,1],tmpidx[tmppos,2]] = 0
        }
        
      } else {
        #only one of the pieces can actually move there
        tmppos = which(tmpbool==1)
        board[tmpidx[tmppos,1],tmpidx[tmppos,2]] = 0
      }
        
    } else {
    #only one of the rooks is on the same row/col
      tmppos = which(tmpbool==1)
      board[tmpidx[tmppos,1],tmpidx[tmppos,2]] = 0
    }
  } else {
    #only one rook on the board
    board[tmpidx[1,1],tmpidx[1,2]] = 0
  }
  
  board[location[2],location[1]] = indic*4
  
  return(board)
} 

queenFunc <- function(board,location,move,movesplit,whiteturn){
  if(whiteturn) {indic = 1} else {indic = -1}
  #determine if it's a bishop move or a rook move
  
  tmpidx = which(board == indic*5, arr.ind = T)
  if(length(tmpidx)==0) stop("Queen: no queen")
  if(dim(tmpidx)[1]>1) stop("Queen: too many queens")
  tmpbool = (tmpidx[1,1]==location[2] || tmpidx[1,2]==location[1])
  
  if(tmpbool){ 
    hor = T
    if(tmpidx[1,1]!=location[2]) hor = F
    dist = abs(tmpidx[1,1]-location[2]) + abs(tmpidx[1,2]-location[1])
    if(dist==0) stop("Queen: distance 0")
    if(dist>1){
      if(hor){
        direct = sign(location[1]-tmpidx[1,2])
        tmperror = (length(which(board[location[2],location[1]:(tmpidx[1,2]+direct)]!=0))>0)
      } else {
        direct = sign(location[2]-tmpidx[1,1])
        tmperror = (length(which(board[location[2]:(tmpidx[1,1]+direct),location[1]]!=0))>0)
      }
      if(tmperror) stop("Queen: pieces in way - rook")
    }
   
    
  } else {
    dist = abs(tmpidx[1,1] - location[2]) #remember location is alpha then num
    xdir = sign(tmpidx[1,2]-location[1])
    ydir = sign(tmpidx[1,1]-location[2])
    
    tmploc = tmpidx[1,]-c(ydir,xdir)
    if(dist>1){
      for(i in 1:(dist-1)){
        if(board[tmploc[1],tmploc[2]]!=0) { cat(tmploc); stop("Queen: pieces in way - bishop")}
        tmploc = tmpidx[1,]-c(ydir,xdir)
      }
    }
  }
  
  board[tmpidx[1],tmpidx[2]] = 0
  board[location[2],location[1]] = indic*5
  
  return(board)
}

kingFunc <- function(board,location,move,movesplit,whiteturn){
  if(whiteturn) {indic = 1} else {indic = -1}
  
  tmpidx = which(board == indic*6, arr.ind = T)
  
  if(abs(tmpidx[1,1]-location[2])>1) stop("King: Move too far 1")
  if(abs(tmpidx[1,2]-location[1])>1) stop("King: Move too far 2")
  
  board[tmpidx[1,1],tmpidx[1,2]] = 0
  board[location[2],location[1]] = indic*6
  
  return(board)
}

pawnFunc <- function(board,location,move,movesplit,whiteturn){
  if(whiteturn) {indic = 1} else {indic = -1}
  
  if(grepl("x",move)){
    tmplet = movesplit[1]
    tmp = match(tmplet,letters)
    tmploc = c(location[2]-indic,tmp)
    if(board[tmploc[1],tmploc[2]]!=indic) stop("Pawn: bad attack")
    board[tmploc[1],tmploc[2]] = 0
    board[location[2],location[1]] = indic
    
  } else {
    if(board[location[2]-indic,location[1]]==indic){
      board[location[2]-indic,location[1]] = 0
      board[location[2],location[1]] = indic
      
    } else if(board[location[2]-2*indic,location[1]]==indic){
      if(location[2]<4 || location[2]>5) stop("Pawn: invalid double move")
      board[location[2]-2*indic,location[1]] = 0
      board[location[2],location[1]] = indic
      
    } else {
      stop("Pawn: can't find")
    }
    
  }
  
  return(board)
}

################
modtwo <- function(array){
  return(sum(array)%%2)
}