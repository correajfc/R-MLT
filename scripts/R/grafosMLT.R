#cargar librerias 

# # DATASETS 
# 
# #todos los nodos
# nodosMLT <- read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - nodos.csv", na.strings="n/a")
# enlacesMLT <- read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - enlaces-MLT.csv", na.strings="n/a")
# #Exposiciones y ...
# nodosExArt<-read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - nodosExArt.csv", na.strings="n/a")
# nodosExCur<-read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - nodosExCur.csv", na.strings="n/a")
# nodosExAus<-  read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - nodosExAus.csv", na.strings="n/a")
# nodosExPre<-read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - nodosExPre.csv", na.strings="n/a")
# nodosExObras<-  read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - nodosExObras.csv", na.strings="n/a")
# 
# nodosArtCur<-  read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - nodosArtCur.csv", na.strings="n/a")
# 
#   
# Expos.Artistas <- read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - Expos-Artistas.csv", na.strings="n/a")
# Expos.Curadores <- read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - Expos-Curadores.csv", na.strings="n/a")
# Expos.Auspiciadores <- read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - Expos-Auspiciadores.csv", na.strings="n/a")
# Expos.Presentadores <- read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - Expos-Presentadores.csv", na.strings="n/a")
# Expos.Obras <- read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - Expos-Obras.csv", na.strings="n/a")
# Artista.Curador<-  read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/MLT_DB_07_2016 - Artista-Curador.csv", na.strings="n/a")
# Artista.Curador.nombre<-read.csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/data/Continua-MLT_DB_07_2016 - Artista-Curador-Expo.csv", na.strings="n/a")


# Examine the data:
# head(nodes)
# head(links)
# nrow(nodes); length(unique(nodes$id))
# nrow(links); nrow(unique(links[,c("from", "to")]))

head(nodosMLT)
head(enlacesMLT)
nrow(unique(nodosMLT)); length(unique(nodosMLT$id_nodo))
nrow(enlacesMLT); nrow(unique(enlacesMLT[,c("id_agente", "id_expo")]))

#eliminar nodos y enlaces repetido

enlacesMLT[duplicated(enlacesMLT), ]
nodosMLT[duplicated(nodosMLT), ]
enlacesMLT<-enlacesMLT[!duplicated(enlacesMLT), ]
nodosMLT<-nodosMLT[!duplicated(nodosMLT), ]
nodosExObras[duplicated(nodosExObras), ]
nodosExObras<- nodosExObras[!duplicated(nodosExObras), ]
# nodosExObras[!(nodosExObras$id_nodo %in% as.vector(unique(Expos.Obras$id_expo))) , ]$id_nodo
# nodosExObras[(nodosExObras$id_nodo %in% as.vector(unique(Expos.Obras$id_obra))) , ]$id_nodo
# length(nodosExObras[(nodosExObras$id_nodo %in% as.vector(unique(Expos.Obras$id_obra))), ]$id_nodo)
# Expos.Obras[!(Expos.Obras$id_obra %in% as.vector(unique(nodosExObras$id_nodo))),]$id_obra
 enlacesMLT[!(enlacesMLT$id_agente %in% as.vector(unique(nodosMLT$id_nodo))),]
 enlacesMLT<-enlacesMLT[(enlacesMLT$id_agente %in% as.vector(unique(nodosMLT$id_nodo))),]


#crear grafos 

gMLT <- graph_from_data_frame(d=enlacesMLT, vertices=nodosMLT, directed=F)
gExArt<-graph_from_data_frame(d=Expos.Artistas, vertices=nodosExArt, directed=F)
gExCur<-graph_from_data_frame(d=Expos.Curadores, vertices=nodosExCur, directed=F)
gExAus<-graph_from_data_frame(d=Expos.Auspiciadores, vertices=nodosExAus, directed=F)
gExPre<-graph_from_data_frame(d=Expos.Presentadores, vertices=nodosExPre, directed=F)
gExObras<-graph_from_data_frame(d=Expos.Obras, vertices=nodosExObras, directed=F)
gArtCur<-graph_from_data_frame(d=Artista.Curador, vertices=nodosArtCur, directed=F)

gArtCur.nombre<-graph.edgelist(as.matrix(Artista.Curador.nombre[,c("curador","artista")]),directed = TRUE)
#metricas de los grafos
gArtCur.nombre$grado<-degree(gArtCur.nombre, mode = "all")
gArtCur.nombre$grado_in<-degree(gArtCur.nombre,mode = "in")
gArtCur.nombre$grado_out<-degree(gArtCur.nombre,mode = "out")

gArtCur.nombre$intermediacion_nod<-betweenness(graph = gArtCur.nombre,directed = F)
gArtCur.nombre$eigenvectores<-evcent(gArtCur.nombre,directed = F)$vector
gArtCur.nombre$rank<-page_rank(gArtCur.nombre)
gArtCur.nombre$cercania<-closeness(gArtCur.nombre)

#algunas graficas
hist(gArtCur.nombre$grado, breaks=1:vcount(gArtCur.nombre)-1, main="Histogram of node degree")
sort(gArtCur.nombre$grado,decreasing = T)[1:20]
sort(gArtCur.nombre$grado_in,decreasing = T)[1:30]
sort(gArtCur.nombre$grado_out,decreasing = T)[1:20]
sort(gArtCur.nombre$eigenvector,decreasing = T)[1:20]
sort(gArtCur.nombre$rank,decreasing = T)[1:20]
sort(gArtCur.nombre$intermediacion,decreasing = T)[1:20]


#examinar enlaces y vertices(nodos)
E(gExArt)       # The edges of the "net" object
V(gExArt)       # The vertices of the "net" object
E(gExArt)$nombre_expo       # The edges of the "net" object
V(gExArt)$class_nodo       # The vertices of the "net" object


# Generate colors based on media type:
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
colrs <- c("tomato", "blue","green","orange","purple","pink")
#asicolor y tamaño de vertice
V(gMLT)$color <- colrs[as.integer(factor(V(gMLT)$class_nodo))]
V(gMLT)$size<-sqrt(degree(gMLT))+2

V(gArtCur)$color <- colrs[as.integer(factor(V(gArtCur)$class_nodo))]
V(gArtCur)$size<-sqrt(degree(gArtCur))+2

V(gExArt)$color <- colrs[as.integer(factor(V(gExArt)$class_nodo))]
#V(gExArt)$size<- degree(gExArt)/max(degree(gExArt))*10+2
V(gExArt)$size<-sqrt(degree(gExArt))+2

V(gExCur)$color <- colrs[as.integer(factor(V(gExCur)$class_nodo))]
#V(gExCur)$size<- degree(gExCur)/max(degree(gExCur))*10+2
V(gExCur)$size<-sqrt(degree(gExCur))+2

V(gExAus)$color <- colrs[as.integer(factor(V(gExAus)$class_nodo))]
#V(gExAus)$size<- degree(gExCur)/max(degree(gExCur))*10+2
V(gExAus)$size<-sqrt(degree(gExAus))+2

V(gExPre)$color <- colrs[as.integer(factor(V(gExPre)$class_nodo))]
V(gExPre)$size<-sqrt(degree(gExPre))+2

V(gExObras)$color <- colrs[as.integer(factor(V(gExObras)$class_nodo))]
V(gExObras)$size<-sqrt(degree(gExObras))+2

#plot pelado
plot.igraph(gMLT,vertex.label=NA,vertex.frame.color='white')

#layout mds
plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
legend(x=-1.5, y=-0.5, levels(factor(V(gMLT)$class_nodo)), pch=21,col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)

plot(gExArt, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
plot(gExCur, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
plot(gExAus, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
plot(gExPre, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
plot(gExObras, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
plot(gArtCur, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)

plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_kk)
plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_graphopt)
legend(x=-1.5, y=-0.5, levels(factor(V(gMLT)$class_nodo)), pch=21,col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)
plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_on_grid)
plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_gem)


plot(gArtCur, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_kk)
plot(gArtCur, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_graphopt(gArtCur, start = NULL, niter = 1000, charge = 0.1, mass = 30, spring.length = 2, spring.constant = 0.1, max.sa.movement = 5))

plot(gArtCurSimple, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)


#constrir dataframes
head(Artista.Curador.nombre)



#estadisticas basicas

#crear grafo vacio

#calcular metricas del grafo


