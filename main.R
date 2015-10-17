setwd("C:/Users/Uikos/Dropbox/cos 597a/code")

game_num = 16
res_type = c(1,0,-1)
boardwinnames = rep(NA,16)
boarddrawnames = rep(NA,16)
boardlosenames = rep(NA,16)

boardwinidx = rep(NA,16)
boarddrawidx = rep(NA,16)
boardloseidx = rep(NA,16)

for(i in 1:16){
  boardwinnames[i] = paste("boardwin",i,sep="")
  boarddrawnames[i] = paste("boarddraw",i,sep="")
  boardlosenames[i] = paste("boardlose",i,sep="")
}

for(i in 1:3){
  gameidx = which(outcomeVect == res_type[i])
  gameidx = gameidx[1:16]
  for(j in 1:16){
    tmp = generateBoard.pieces(gameidx[j],8)
    if(i == 1){
      assign(boardwinnames[j],tmp)
      boardwinidx[j] = gameidx[j]
    } else if (i == 2){
      assign(boarddrawnames[j],tmp)
      boarddrawidx[j] = gameidx[j]
    } else if (i == 3){
      assign(boardlosenames[j],tmp)
      boardloseidx[j] = gameidx[j]
    }
  }
}

par(mfrow=c(4,4), pty="s", mar = c(0.1,0.1,0.1,0.1))
for(i in 1:16){
  image(t(get(boardwinnames[i])$board), col=cm.colors(20, alpha=1))
}
par(mfrow=c(4,4), pty="s", mar = c(0.1,0.1,0.1,0.1))
for(i in 1:16){
  image(t(get(boarddrawnames[i])$board), col=cm.colors(20, alpha=1))
}
par(mfrow=c(4,4), pty="s", mar = c(0.1,0.1,0.1,0.1))
for(i in 1:16){
  image(t(get(boardlosenames[i])$board), col=cm.colors(20, alpha=1))
}
