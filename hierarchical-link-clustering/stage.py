from igraph import *
import time

t0 = time.time()

g = Graph.Read_Ncol("UsersPostGraph2.txt" , weights = True, directed=False)

d = time.time() - t0
print ("duration: %.2f s." % d)

print (g.summary())