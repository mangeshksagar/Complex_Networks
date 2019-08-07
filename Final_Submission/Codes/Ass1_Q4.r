#Incidence matrix for given bipartite n/w:-
B <- matrix(c(1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1),byrow = T, 4, 7,
            dimnames = list(c('A','B','C','D'),c(seq(1:7))))


#Adjacency matrix for projections:-
#1.Vertex projection
At <- B %*% t(B)

#2.Group projection:-
Bt <- t(B) %*% B