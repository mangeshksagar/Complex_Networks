celegans <- as.matrix(read.delim("celegans.dat",header = F,sep = ","))
celegans_t <- t(celegans)
cocitation_mat <- celegans%*%elegans_t
bibiliographic_mat <- celegans_t%*%celegans

#Eigen vector centrality
n <- dim(celegans)[1]
old <- rep(1,n)
new <- old
eps <- 10^-4

while (sum(abs(old - new)) < eps) {
  old <- new
  for (i in 1:n) {
    x <- sum(celegans[i,]==1)
    new[i] <- x
  }
}
#repeat{
#  for (i in 1:n) {
#    x <- sum(celegans[i,]==1)
#    new[i] <- x
#  }
#  print(new)
#  print(old)
#  if(max(abs(new - old)) > eps) {break}
#  old <- new
#}

#Page rank centrality
o <- matrix(1,n)
I <- diag(n)
alpha <- 0.85
A <- as.matrix(celegans)
kout <- colSums(A)
kout[kout==0] <- 1
D <- diag(1/kout)
ADi <- A %*% solve(D)
xPR <- solve(I - alpha * ADi) %*% o
