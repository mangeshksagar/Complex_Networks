#Q2
#Input
filename <- "lesmis.dat"
#filename <- "polbooks.dat"
data <- read.delim(filename,header=FALSE,sep=",")
data1 <- data[1] + rep(1,nrow(data))
data1[2] <- data[2] + rep(1,nrow(data))
data <- data1

n <- max(c(max(data[1]),max(data[2])))

#Adjacency matrix -> mat[]
mat <- matrix(0, nr=n, nc=n, byrow = TRUE)

for(k in 1:nrow(data))
{
  i <- data[k,1]
  j <- data[k,2]
  mat[i,j] <- 1
  mat[j,i] <- 1
}

#Avg degree -> avg.deg
deg.und <- function(mat)
{
  return(colSums(mat))
}

avg.deg <- mean(deg.und(mat))

#Density -> density.und
density.und <- avg.deg / n

#Local clustering coefficients -> local.cluster
local.cluster <- NULL
idx <- NULL
tot <- NULL

for(i in 1:n)
{
  k <- 0
  tot <- 0
  nbr <- sum(mat[i,])
  if(nbr > 1){
    idx <- which(mat[i,] == 1)
    for(j in 1:(nbr-1))
    {
      for(l in (j+1):nbr)
      {
        print(l)
          if(mat[idx[j],idx[l]] == 1){k <- k + 1} 
      }
    }
    tot <- nbr * (nbr - 1) / 2
  }
  lc <- 0
  if(tot != 0) {lc <- k/tot}
  local.cluster = c(local.cluster, lc)
}

#Degree distribution -> deg.dist[]
deg <- sort(unique(deg.und(mat)))
deg.dist <- matrix(0,nr=length(deg),nc=2,byrow = TRUE)

for(i in 1:length(deg))
{
  deg.dist[i,1] <- deg[i]
  deg.dist[i,2] <- sum(deg.und(mat) == deg[i]) / n
}

#Vertex similarity (cosine) -> sigma[]
mat2 <- mat %*% mat
degi <- deg.und(mat)
sigma <- matrix(0, nr=n, nc=n, byrow = TRUE)

for(i in 1:n)
{
  for(j in 1:n)
  {
    if((degi[i] != 0) && (degi[j] != 0)){
      sigma[i,j] <- mat2[i,j] / sqrt(degi[i] * degi[j])
    }
    if(i == j){
      sigma[i,j] <- 1
    }
  }
}
