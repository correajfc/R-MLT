#cargar/instalar librerias 

install.packages('igraph')
install.packages('network') 
install.packages('sna')
install.packages('ndtv')
install.packages('visNetwork')
devtools::install_github("analyxcompany/ForceAtlas2")


library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(igraph)
library(network)
library(sna)
library(ndtv)
library(visNetwork)
library(ForceAtlas2)

#mapas de color
library(viridis)
library(RColorBrewer)

#cargar datos 
#rm(list = ls()) # Remove all the objects we created so far.

# DATASETS 
head(MLTExpos1956_2016)
head(agente_agente_relacion)
head(artistas_artistas_acumulado)
head(curador_artistas_acumulado)
head(expo_agentes_curadore_artistas)
head(grafoMLTar_art_1956_2016)
head(grafoMLTcur_art_1956_2016)
head(expo_agentes)
head(expo_agentes_expandido)
head(MLT_expos)


#estadisticas 

#grafica de expos por tipo de participacion
p_ex <- ggplot(MLT_expos, aes( x=ano ) ) 
p_ex + geom_histogram(aes(fill=factor(tipo_participacion)), color="white",binwidth=1)+
  labs(title ="Exposiciones por año por tipo de participacion",x="años", y="cantidad")

#expos curadas por año
curadas<- expo_curadores %>% 
  mutate(curada=if_else(!is.na(nombre_registro_agente),"curada","no-curada",NA_character_)) %>%
  select(id_expo,curada) %>%
  group_by(id_expo,curada) %>%
  summarise(num_expos=n()) %>%
  left_join(MLT_expos,., by="id_expo" ) %>%
  #select(id_expo,ano,tipo_participacion,curada) %>%
  #group_by(ano,tipo_participacion,curada) %>%
  summarise(num_expos=n()) 


#grafica de expos por tipo de participacion
p_ex_curaduria_ano<-ggplot(curadas, aes(x=ano))
p_ex_curaduria_ano + geom_histogram(aes(fill=factor(curada)), color="white",binwidth=1)+
  labs(title ="Exposiciones por año con curaduria explicita",x="años", y="cantidad")



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




