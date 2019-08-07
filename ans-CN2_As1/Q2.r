polbooks <- read.delim("/nfs/cms/mtech18/mangesh.kshirsagar/Desktop/CN-1/CN Assignment-1/final/Final_Submission/Questions/polbooks.dat", header = F, sep = ",")
d <- data.frame((polbooks[,1] + 1),(polbooks[,2] + 1)) 
n <- max(d)
a <- matrix(0,n,n)
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
no_steps <-100
nbn <- seq(1,no_steps,1) #No Of neighbours
vertexindex <- sample(seq(1,n,1),1,replace = F)   
length <- NULL
frequency <- rep(0, n)
for(i in 1:no_steps)
  {
  neighbours <- which(a[vertexindex,] == 1)
  rd <- sample(neighbours,1,replace = T)
  frequency[rd] <- frequency[rd] + 1    
  length <- c(length,rd)
  vertexindex <- rd
}
par(mfrow=c(1,2))
plot(nbn,length,type = "S")
plot(degree,frequency,main = "PLOT")