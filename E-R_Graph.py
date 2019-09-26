#%%
#Importing Required Libraries
import numpy as np
import random as rn
import matplotlib.pyplot as plt
#%%

Avg_degree = [0.5, 1 ,2 ,4] #Average Degree 
n = 5000 #NO Of edges in the Graph
#%%
def ER_Graph(n, Avg_degree): 
    Adj_mat = np.zeros(shape = (n,n))
    prob = Avg_degree/(n-1)	
    for i in range(n):
        for j in range(i,n):
            if i != j:
                x = rn.random()
                if x <= prob:
                    Adj_mat[i, j] = 1 
                    Adj_mat[j, i] = 1 
    return Adj_mat
#%%
for k in Avg_degree:
    Adj_mat = ER_Graph(n, k) 
    degree = Adj_mat.sum(axis=1) 
    n_edges = int(max(degree))
    degree_dist = []
    for l in range(n_edges+1):
        dist = len(degree[degree==l])/n 
        degree_dist.append(dist)
#%% Plotting The Degree Distribution Of Given Graph:-
for l in Avg_degree:
  plt.plot(range(n_edges+1),degree_dist,color='red',linestyle='dashed', marker='o')
  plt.title("degree distribution of the E-R graph")
  plt.xlabel("Degree Sequence")
  plt.ylabel("No Of Fraction Of Edges")
  plt.grid(True)

#%%