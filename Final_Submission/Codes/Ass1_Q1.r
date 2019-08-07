#Degree Of Each Vertex For Undirected Graph:-
#Storing The Given Network Into Adjancency Matrix:-
A=c(0,1,0,0,3,0,1,2,2,1,0,0,0,2,0,1,1,1,0,1,1,0,0,0,3,0,1,0,0,0,0,0,1,0,0,2)
Adj_Mat1=matrix(A,6,6,byrow = TRUE)
show(Adj_Mat1)
degree_Adj_Mat1=show(rowSums(Adj_Mat1))
#Degree Of Each Vertex For Directed Graph:-
B=c(0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0)
Adj_Mat2=matrix(B,6,6,byrow = TRUE)
show(Adj_Mat2)
#Finding Indegree & Outdegree Of each Vertex:-
degree_in_Adj_Mat2=show(rowSums(Adj_Mat2))
degree_out_Adj_Mat2=show(colSums(Adj_Mat2))