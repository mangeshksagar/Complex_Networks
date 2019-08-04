# Program For Computing Eigen Values Of Laplacian:-
lesmis <- read.delim("lesmis.dat",header = F,sep = ",")

d <- data.frame((lesmis[,1] + 1),(lesmis[,2] + 1)) 
n <- max(d) #No. of vertices
A <- matrix(0,n,n,dimnames = list(c(seq(1:77)),c(seq(1:77))))

#Making adjacency matrix from given edge list:-
for (i in 1:nrow(d)){
  x <- d[i,1] 
  y <- d[i,2]
  A[x,y] <- 1
  A[y,x] <- 1
}
degree <- rowSums(A)
vertex <- seq(1,n)
show(data.frame(vertex,degree))
D=diag(degree)
Laplacian=D-A
Eigen=eigen(Laplacian)
Eigen1=round(Eigen$values)
