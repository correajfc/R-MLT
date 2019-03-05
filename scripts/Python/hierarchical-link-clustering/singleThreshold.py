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

#g = Graph.Read_Ncol("ZacharyKarateClub.txt" , weights = True, directed=False)
#
c = g.clusters("strong")

for i in c:
    print i

sccIdx = c.sizes().index(max(c.sizes()))

print "c=", sccIdx

scc = c.subgraph(sccIdx)

scc.simplify(combine_edges={"weight": sum})



alg = hlc.HLC(scc)

results = alg.run()

j= 0

for community in results:

    j = j + 1
    print j

    print "\t".join(scc.vs[community]["name"])


print("Threshold = %.6f" % alg.last_threshold)
print("Partition Density = %s" % alg.last_partition_density)


# print "Generating GraphML file..."

# communities.save("UsersPostsSCC.gml",format="gml")

