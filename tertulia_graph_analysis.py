import igraph
from igraph import *
import louvain

graph_art_art = Graph.Read_Ncol('grafoMLTsimplificado.txt', weights = True, directed=False)

#plot(graph_art_art)

#layout = graph_art_art.layout("kk")
#plot(graph_art_art, layout = layout)

summary(graph_art_art)


part = louvain.find_partition(graph_art_art, louvain.ModularityVertexPartition);
print(part)

#closeness(graph_art_art, mode="all")

#print(part[0])

g=graph_art_art.subgraph(part[0])

layout = g.layout("large")


#Plot largest community
visual_style = {}
visual_style["vertex_size"] = 2
#visual_style["vertex_color"] = [color_dict[gender] for gender in g.vs["gender"]]
visual_style["vertex_label"] = g.vs["name"]
visual_style["label_size"] = 3
#visual_style["edge_width"] = [1 + 2 * int(is_formal) for is_formal in g.es["is_formal"]]
visual_style["layout"] = layout
visual_style["edge_color"] = 66666675
visual_style["bbox"] = (3000, 3000)
#visual_style["margin"] = 20
plot(g, **visual_style)