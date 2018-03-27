import hlc
from igraph import Graph
g = Graph.Read_Ncol("ZacharyKarateClub.txt" , weights=True, directed=False)
#g = Graph.Read_Ncol("UsersPosts2.txt" , weights=False, directed=False)
#print(g)
x = hlc.HLC(g)
results=x.run(.3)
#print(x.get_edge_similarity_graph(), text_file)

text_file2=open("OutputSimilarityGraph.txt","w")

line = x.get_edge_similarity_graph()
print >> text_file2, line
text_file2.close()

text_file3=open("similatiryScores.txt","w")
j=0
for i in line.es:
    j=j+1
    print >> text_file3, j,"\t",i["score"]
text_file3.close()

text_file=open("OutputCommunities.txt","w")
for community in results: 
    print >> text_file, ("\t".join(g.vs[community]["name"]))
text_file.close()

print("Last threshold: %.6f" % x.last_threshold)
 
g.vcount()
g.is_weighted()