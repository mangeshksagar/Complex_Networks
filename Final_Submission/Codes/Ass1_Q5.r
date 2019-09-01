dirneural <- read.delim("dirneural.dat",header = F,sep = ",")
celegans <- read.delim("celegans.dat",header = F,sep = ",")

#a <- matrix(c(0,0,0,0,0,1,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,1,0),
#            byrow = T,5,5,dimnames = list(c(seq(1,5)),c(seq(1,5))))
a <- dirneural
repeat{
  if(sum(dim(a))==0){
    print("Network is DAG")
    break
  }
  r <- as.numeric(which(colSums(a)==0))
  if(length(r)==0){
    print("Network is Cyclic")
    break
  }
  a <- a[-r,-r]
}
b <- celegans
repeat{
  if(sum(dim(b))==0){
    print("Network is DAG")
    break
  }
  r <- as.numeric(which(colSums(b)==0))
  if(length(r)==0){
    print("Network is Cyclic")
    break
  }
  b <- b[-r,-r]
}
