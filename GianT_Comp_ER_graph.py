
#%%Importing Required Libraries
import numpy as np
import matplotlib.pyplot as plt

#%%
component = np.linspace(0,3,10)
size = []

#%%
def Giant_Comp(component,size): 
    return(1 - np.exp(-component*size))    

for i in component:
    size1, size2 = 0, 0.5
    while (abs(size1-size2) >= 10e-4):
        size1 = Giant_Comp(i,size2)
        size2 = Giant_Comp(i,size1)
    size.append(size2) 
    
#%%
plt.plot(component,size,color='red',linestyle='dashed', marker='o')
plt.title("The Size Of Giant Component")
plt.xlabel('Average Degree')
plt.ylabel('Size')
plt.grid(True)

#%%