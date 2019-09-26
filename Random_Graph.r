net = read.csv("polbooks.dat",sep = "",header = FALSE)
net = net + 1
n = max(net)
adj = matrix(0,n,n,byrow = TRUE)

for (i in 1:nrow(net))
{
  adj[net[i,1],net[i,2]] <- 1
  adj[net[i,2],net[i,1]] <- 1
}

degree = rowSums(adj)
adj_list = matrix(0,n,n,byrow = TRUE)

for (i in 1:n)
{
  k = 1
  for (j in 1:n)
  {
    if ((adj[i,j] == 1) & (i != j)){
      adj_list[i,k] <- j
      k = k + 1
    }
  }
}

start_ver1 = sample(1:n,1)
ver_list1 = c(start_ver1)
k1 <- start_ver1

start_ver2 = sample(1:n,1)
ver_list2 = c(start_ver2)
k2 <- start_ver2

start_ver3 = sample(1:n,1)
ver_list3 = c(start_ver3)
k3 <- start_ver3

for (i in 1:5000)
{
  x1 = adj_list[k1,1:(degree[k1] - 1)]
  y1 = sample(x1,1)
  ver_list1 = c(ver_list1,y1)
  k1 <- y1
  
  x2 = adj_list[k2,1:(degree[k2] - 1)]
  y2 = sample(x2,1)
  ver_list2 = c(ver_list2,y2)
  k2 <- y2
  
  x3 = adj_list[k3,1:(degree[k3] - 1)]
  y3 = sample(x3,1)
  ver_list3 = c(ver_list3,y3)
  k3 <- y3
}

walker_visits = c(ver_list1,ver_list2,ver_list3)
walker_visits.df = as.data.frame(table(walker_visits))

op <- par ( mfrow = c ( 1 , 2 ) )
plot(1:50,ver_list1[1:50],type="b",col = "red", xlab = "Time", ylab = "Vertex index")
title(main = "Vertex index VS Time")
lines(1:50,ver_list2[1:50],type="b",col = "black")
lines(1:50,ver_list3[1:50],type="b",col = "blue")

plot(rowSums(adj[-c(103,105),]), as.vector(walker_visits.df[,2]) / sum(as.vector(walker_visits.df[,2])), col = "blue", xlab = "Degree of vertices", ylab = "Walker visit frequency (normalised)")
title(main = "Walker visit freqency VS Degree of vertices")
lines(rowSums(adj[-c(103,105),]), (rowSums(adj[-c(103,105),]) / sum(rowSums(adj))), type="b", col = "green")
