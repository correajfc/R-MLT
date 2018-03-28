import hlc
import time
from igraph import *
import matplotlib.pyplot as plt
import operator
import heapq
import numpy
import gc


def plotStats(x,y,title,xtitle,ytitle):

    plt.xlabel(xtitle)
    plt.ylabel(ytitle)
    plt.title(title)
    plt.grid(True)

    plt.plot(x,y)
    plt.show()


print("reading ncol file and building network...")
t0 = time.time()

g = Graph.Read_Ncol("UsersPostGraph2.txt" , weights = True, directed=False)

delay = time.time() - t0
print ("duration: %.2f s." % delay)

# g_d = Graph.Read_Ncol("UsersPostGraph2.txt" , weights = True, directed=True)

print("Calculating SCC...")

c = g.clusters("strong")

del g

#

delay = time.time() - t0
print ("duration: %.2f s." % delay)

sccIdx = c.sizes().index(max(c.sizes()))

print "Idx=", sccIdx
print "len c=", len(c)

print("creating subgraph from SCC...")

#scc = c.subgraph(sccIdx, implementation="copy_and_delete")
scc = c.subgraph(sccIdx)

del c, sccIdx

delay = time.time() - t0
print ("duration: %.2f s." % delay)

print("simplifying SCC...")

scc.simplify(combine_edges={"weight": sum})

print "Num of vertexes=", scc.vcount()
print "Number of edges=", scc.ecount()

delay = time.time() - t0
print ("duration: %.2f s." % delay)

print("creating instance of hlc algorithm ...")

alg = hlc.HLC(scc)

delay = time.time() - t0
print ("duration: %.2f s." % delay)

x = [0.6]
# x = [x1 * 0.1 for x1 in range(0, 9)]

y = []
z = []
w = []
d = []
d2 = []
p = []

# p_community = []

print("running instance for threshold...")

gc.collect()

for i in x:

    print "x=", i
    results = alg.run(0.6)

    v_community = []
    #l_community = []

    communities_lengths = []

    #l = []

    j = 0

    max_length = 0
    largest_community = []
    max_density = 0
    min_density = 1

    for community in results:

        j = j + 1
        print j
        community_length = len(community)
        communities_lengths.append(community_length)

        if community_length > max_length:
            max_length = community_length
            largest_community = community

        #l_community.append(community)
        #l.append(len(community))

        # g_community = scc.subgraph(community)
        # dc = g_community.density()
        #
        # if dc > max_density:
        #     max_density = dc
        #
        # if dc < min_density:
        #     min_density = dc

        v_community = list(set(v_community) | set(community))

    delay = time.time() - t0
    print ("duration: %.2f s." % delay)

    # z: Number of communities for a given threshold
    print "Number of communities =", j

    # max_length: size of largest community for a given threshold
    print "Size of largest community =", max_length

    y.append(max_length)
    z.append(j)
    w.append(len(v_community))

    # d.append(max_density)
    # d2.append(min_density)

    # print "Max Density within all graph communities =", max_density


plotStats(x,y,'Largest Commnunity per Threshold','Threshold','Largest Comm')

plotStats(x,z,'Num of Commnunities per Threshold','Threshold','Communities')

plotStats(x,w,'Community Coverage per Threshold','Threshold','Community Coverage')

# plotStats(x,d,'Communities max density per Threshold','Threshold','Communities Density')
#
# plotStats(x,d2,'Communities min density per Threshold','Threshold','Communities Density')

#plotStats(x,p,'Pagerank average per Threshold','Threshold','Pagerank Average')

print("Threshold = %.6f" % alg.last_threshold)
# print("Partition Density = %s" % alg.last_partition_density)

#print "len com= ", len(p_community)

# bins_h = numpy.linspace(0, 0.1, 100)
#
#
# for i in p_community:
#     # print "p_com=", i
#     # print "len p_com=", len(i)
#     g_1 = g_d.subgraph(i)
#     pagerank = g_1.pagerank()
#     plt.hist(pagerank, cumulative=-1, log=True, bins=bins_h, alpha=0.5)
#
# plt.savefig("pagerank.png")


# print "Generating GraphML file..."
# communities.save("UsersPostsSCC.gml",format="gml")

