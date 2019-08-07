lesmis <- read.delim("/nfs/cms/mtech18/mangesh.kshirsagar/Desktop/CN-1/CN Assignment-1/final/Final_Submission/Questions/lesmis.dat",header = F,sep = ",")
polbooks <- read.delim("/nfs/cms/mtech18/mangesh.kshirsagar/Desktop/CN-1/CN Assignment-1/final/Final_Submission/Questions/polbooks.dat", header = F, sep = ",")
d <- data.frame((lesmis[,1] + 1),(lesmis[,2] + 1)) 
n <- max(d) #No. of vertices
a <- matrix(0,n,n,dimnames = list(c(seq(1:77)),c(seq(1:77))))

#Creating adjacency matrix from given list
for (i in 1:nrow(d)){
      x <- d[i,1] 
      y <- d[i,2]
      a[x,y] <- 1
      a[y,x] <- 1
}
degree <- rowSums(a)
vertex <- seq(1,n)
show(data.frame(vertex,degree))

avg_degree <- sum(degree)/n
cat(sprintf("\nAverage degree is :"),avg_degree)
cat(sprintf("\nDensity is :"),avg_degree/n)

#Local Clustering coeff (LCC)
LCC <- NULL #LCC of each vertex
id <- NULL  #index of neighbours
kC2 <- NULL  #Possible connections between neighbours
for(i in 1:n){
  k <- 0
  kC2 <- 0
  nd <- sum(a[i,])  #degree of each vertex
  if(nd > 1){
    id <- which(a[i,] == 1)
    for(j in 1:(nd-1)){
      for(l in (j+1):nd){
        if(a[id[j],id[l]] == 1){k <- k + 1}  
      }
    }
    kC2 <- nd * (nd - 1) / 2
  }
  lc <- 0
  if(kC2 != 0) {lc <- k/kC2}
  LCC = c(LCC, lc)
}

#Degree Distribution
dist <- NULL
for (i in 0:36) {
  dist1 <- length(which(degree==i))/n
  dist <- c(dist,dist1)
}
cat(sprintf("\n"))
deg <- seq(0,36)
show(data.frame(deg,dist))

#Matrix for cosine similarities
m <- a %*% a  #Matrix for common neighbors between vertices
sig <- matrix(0,n,n,byrow = T,dimnames = list(c(seq(1:n)),c(seq(1:n))))
for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      sig[i,j] <- m[i,j]/(sqrt(rowSums(a)[i])*sqrt(rowSums(a)[j]))
    }
    if(i == j){
      sig[i,j] <- 1
    }
  }
}

#Matrix  for Katz similarities
I <- diag(1,n)
e <- eigen(a)
alpha <- (1/max(e$values))* 0.01   #alpha < 1/k1 where k1 is largerst eigen value of a
ks <- solve(I - (alpha*a))
