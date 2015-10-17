game = 1
pieces = 8


#######
#test board 1
x = c(0,-4, 0, 0, 0, 0, 0,-4,
      0, 0, 0, 0, 0,-1,-6, 0,
      0, 0, 0, 0, 0, 0, 0,-1,
      0, 0, 0, 0, 0, 0, 0, 0,
      2,-1, 0, 0, 0, 0, 1, 0,
      0, 1, 0, 4,-5, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 3, 1,
      0, 2, 0, 4, 0, 0, 0, 6)
board = matrix(x,nrow=8,ncol=8,byrow=T)

#testing Knight
location = c(3,6)
move="Nac6"
movesplit = unlist(strsplit(move,""))
whiteturn = T

#testing Rook
location = c(4,1)
move="Rbd1"
movesplit = unlist(strsplit(move,""))
whiteturn = F


#test board 2
x = c(0 ,0,-4, 0,-4, 0, 0, 0,
      0, 0, 0, 0, 0,-1,-6, 0,
      0, 0, 0, 0, 0, 0, 0,-1,
      0, 0, 0, 0, 0, 0, 0, 0,
      2,-1, 0, 0, 0, 0, 1, 0,
      0, 1, 0, 4,-5, 0, 1, 0,
      0, 0, 0, 0, 0, 3, 3, 1,
      0, 2, 0, 4, 0, 0, 0, 6)
board = matrix(x,nrow=8,ncol=8,byrow=T)
#testing bishop
location = c(4,4)
move="Bd4"
movesplit = unlist(strsplit(move,""))
whiteturn = T

#test board 3
x = c(0,-4, 0, 0,-3, 0, 0,-4,
      0, 0, 0, 0, 0,-1,-6, 0,
      0, 0, 0, 0, 0, 0, 0,-1,
      0, 0, 0, 0, 0, 0, 0, 0,
      2,-1, 0, 0, 0, 0, 1, 0,
      0, 1, 0, 4,-5, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 3, 1,
      0, 2, 0, 4, 0, 0, 0, 6)
board = matrix(x,nrow=8,ncol=8,byrow=T)

location = c(4,1)
move="Rd1"
movesplit = unlist(strsplit(move,""))
whiteturn = F

location = c(4,1)
move="Rd1"
movesplit = unlist(strsplit(move,""))
whiteturn = T

#test board 4
x = c(0,-4, 0, 0,-3, 0, 0, 0,
      0, 0, 0, 0, 0,-1,-6, 0,
      0, 0, 0, 0, 0, 0, 0,-1,
      0, 0, 0,-4, 0, 0, 0, 0,
      2,-1, 0, 0, 0, 0, 1, 0,
      0, 1, 0, 4,-5, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 3, 1,
      0, 2, 0, 4, 0, 0, 0, 6)
board = matrix(x,nrow=8,ncol=8,byrow=T)
location = c(4,1)
move="R4d1"
movesplit = unlist(strsplit(move,""))
whiteturn = F


#test board 5
x = c(0,-4, 0,-5,-3, 0, 0, 0,
      0, 0, 0, 0, 0,-1,-6, 0,
      0, 0, 0, 0, 0, 0, 0,-1,
      0, 0, 0, 0, 0, 0, 0, 0,
      2,-1, 0, 0, 0, 0, 1, 0,
      0, 1, 0, 4, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 3, 1,
      0, 2, 0, 4, 0, 0, 0, 6)
board = matrix(x,nrow=8,ncol=8,byrow=T)
location = c(4,4)
move="Qd4"
movesplit = unlist(strsplit(move,""))
whiteturn = F



