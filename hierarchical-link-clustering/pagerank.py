import hlc
from igraph import *
import matplotlib.pyplot as plt

def plotStats(x,y,title,xtitle,ytitle):

    plt.xlabel(xtitle)
    plt.ylabel(ytitle)
    plt.title(title)
    plt.grid(True)

    plt.plot(x,y)
    plt.show()

g = Graph.Read_Ncol("UsersPosts2.txt" , weights = True, directed=False)

g_d = Graph.Read_Ncol("UsersPosts2.txt" , weights = True, directed=True)

c = g.clusters("strong")

sccIdx = c.sizes().index(max(c.sizes()))

print "c=", sccIdx

scc = c.subgraph(sccIdx)

scc.simplify(combine_edges={"weight": sum})

scc_d = g_d.subgraph(VertexSeq(scc))

plt.hist(scc_d.pagerank(), cumulative=-1, log=True, bins=100)

plt.savefig("pagerank_scc.png")