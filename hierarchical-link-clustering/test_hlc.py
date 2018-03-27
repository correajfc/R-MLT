import hlc
from igraph import *
import matplotlib.pyplot as plt
import operator
import heapq
import numpy

def plotStats(x,y,title,xtitle,ytitle):

    plt.xlabel(xtitle)
    plt.ylabel(ytitle)
    plt.title(title)
    plt.grid(True)

    plt.plot(x,y)
    plt.show()

g = Graph.Read_Ncol("grafoMLTsimplificado.txt" , weights = True, directed=False)

g_d = Graph.Read_Ncol("grafoMLTsimplificado.txt" , weights = True, directed=True)

c = g.clusters("strong")

sccIdx = c.sizes().index(max(c.sizes()))

for i in c:
    print(i)

print ("c=",c)
print ("len c=", len(c))
print "sccIdx=", sccIdx

scc = c.subgraph(sccIdx)

scc.simplify(combine_edges={"weight": sum})

alg = hlc.HLC(scc)

# x = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]

x = [x1 * 0.1 for x1 in range(0, 9)]

# x = [0.1]
y = []
z = []
w = []
d = []
d2 = []
p = []

p_community = []

for i in x:

    print "x=", i
    results = alg.run(i)

    v_community = []
    l_community = []


    l = []

    j = 0
    max_density = 0
    min_density = 1

    for community in results:

        # j = j + 1

        # print j
        #
        # print "\t".join(scc.vs[community]["name"])

        l_community.append(community)
        l.append(len(community))

        g_community = scc.subgraph(community)
        dc = g_community.density()

        if dc > max_density:
            max_density = dc

        if dc < min_density:
            min_density = dc

        v_community = list(set(v_community) | set(community))


    #find the N largest communities
    for i in zip(*heapq.nlargest(1, enumerate(l), key=operator.itemgetter(1)))[0]:
        print "index= ", i
        print "len=", l[i]
        print l_community[i]
        p_community.append(l_community[i])

    # y: length of largest community for a given threshold
    m = max(l_community, key=len) if l_community else [0]
    y.append(len(m))

    print "Size of largest community =", len(m)

    # z: Number of communities for a given threshold
    l = len(l_community)
    z.append(l)
    print "Number of communities =", l

    # w: Total number of vertexes all communities for a given threshold
    lc = len(v_community)
    w.append(lc)
    print "Community coverage =", lc

    # Calculates lowest and highest density of comm

    # d: Density of whole graph of all communities for a given threshold
    # all_communities = scc.subgraph(v_community)
    # dc = all_communities.density()
    # d.append(dc)

    d.append(max_density)
    d2.append(min_density)

    print "Max Density within all graph communities =", max_density


plotStats(x,y,'Largest Commnunity per Threshold','Threshold','Largest Comm')

plotStats(x,z,'Num of Commnunities per Threshold','Threshold','Communities')

plotStats(x,w,'Community Coverage per Threshold','Threshold','Community Coverage')

plotStats(x,d,'Communities max density per Threshold','Threshold','Communities Density')

plotStats(x,d2,'Communities min density per Threshold','Threshold','Communities Density')

#plotStats(x,p,'Pagerank average per Threshold','Threshold','Pagerank Average')

print("Threshold = %.6f" % alg.last_threshold)
# print("Partition Density = %s" % alg.last_partition_density)

print "len com= ", len(p_community)

bins_h = numpy.linspace(0, 0.1, 100)

for i in p_community:
    # print "p_com=", i
    # print "len p_com=", len(i)
    g_1 = g_d.subgraph(i)
    pagerank = g_1.pagerank()
    plt.hist(pagerank, cumulative=-1, log=True, bins=bins_h, alpha=0.5)

plt.savefig("pagerank.png")


# print "Generating GraphML file..."
# communities.save("UsersPostsSCC.gml",format="gml")

