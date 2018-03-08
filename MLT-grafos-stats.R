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
#expos curadas con año
expos_curadas<- expo_curadores %>% 
  mutate(curada=if_else(!is.na(nombre_registro_agente),"curada","no-curada",NA_character_)) %>%
  select(id_expo,curada) %>%
  group_by(id_expo,curada) %>%
  summarise(num_expos=n()) %>%
  left_join(MLT_expos,., by="id_expo" )%>% 
  #select(id_expo,ano,tipo_participacion,curada) %>%
  #group_by(ano,tipo_participacion,curada) %>%
  #summarise(num_curadores=if_el) %>% 
  rename(num_curadores=num_expos)  %>%
  transform(num_curadores=if_else(curada=="no-curada",0,1))

#calculamos solo los que han participado más de 4 veces
ex_curadores_ano<-inner_join( MLT_expos,expo_curadores,by="id_expo")

stats_curadores<-ex_curadores_ano %>%group_by(nombre_registro_agente) %>%
  summarise(num_expos=n()) %>% arrange(desc(num_expos)) 

stats_curadores$num_expos<-as.numeric(stats_curadores$num_expos)  

curtop<-filter(stats_curadores,num_expos>4) %>% select(nombre_registro_agente) 

expo_curadores_importancia<-expo_curadores %>% 
  mutate(cur_top=if_else(
    nombre_registro_agente %in% as.vector(curtop$nombre_registro_agente),
    nombre_registro_agente,"Otros"))


ex_curadores_ano_imp<-inner_join( MLT_expos,expo_curadores_importancia,by="id_expo")

#grafica de expos por tipo de participacion
p_ex <- ggplot(MLT_expos, aes( x=ano ) ) 
p_ex + geom_histogram(aes(fill=factor(tipo_participacion)), color="white",binwidth=1)+
  labs(title ="Exposiciones por año por tipo de participacion",x="años", y="cantidad")


    


#grafica de expos con sin curador
p_ex_curaduria_ano<-ggplot(expos_curadas, aes(x=ano))
p_ex_curaduria_ano + geom_histogram(aes(fill=factor(curada)), color="white",binwidth=1)+
  labs(title ="Exposiciones por año con curador(es)",x="años", y="cantidad")


#grafica curadores

p_ex_curadores_ano<-ggplot(ex_curadores_ano, aes(x=ano))
#todos los que han participado
p_ex_curadores_ano + geom_histogram(aes(fill=factor(nombre_registro_agente)),color="white",binwidth=1)+
   theme(legend.position="none")+
  labs(title ="Exposiciones no curadas y curadores que participaron en el año",x="años", y="cantidad")


p_ex_curadores_ano_imp<-ggplot(ex_curadores_ano_imp, aes(x=ano))
#todos los que han participado
p_ex_curadores_ano_imp + geom_histogram(aes(fill=factor(nombre_registro_agente)),color="white",binwidth=1)+
  theme(legend.position="none")+
  labs(title ="Exposiciones no curadas y curadores que participaron en el año",x="años", y="cantidad")
#solo los que han particpado más de 4 veces
p_ex_curadores_ano_imp + geom_histogram(aes(fill=factor(cur_top)),color="white",binwidth=1)+
  labs(title ="Exposiciones no curadas y curadores que participaron en el año",x="años", y="cantidad")



##############################################
#Preparar datos para grafos con igraph

enlacesMLT<-MLTExpos1956_2016 %>% 
  rename(from = nombre_expo_order_ano,to=nombre_registro_agente) %>%
  select(from,to,ano,rol_expo) %>% na.omit()

#enlacesMLT %>% filter(is.na(rol_expo))

nodosExpos<-MLT_expos %>% 
  mutate(tipo_agente="expocisión") %>%
  select(nombre_expo_order_ano,ano,tipo_agente,nombre_espacio,id_expo) %>%
  rename(nombre_registro_agente=nombre_expo_order_ano,ano_agente_inicia=ano,
         nacionalidad_agente = nombre_espacio,id_agente=id_expo)

#funcio para evaluar si hay NA en una columna
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

nodosAgentes<-MLTExpos1956_2016 %>%
  select(nombre_registro_agente,ano_agente_inicia,tipo_agente,nacionalidad_agente) %>%
  distinct(nombre_registro_agente,.keep_all = T) 

nodosAgentes<-completeFun(nodosAgentes,c("nombre_registro_agente"))


id_agente<-length(nodosExpos$id_agente)+1:length(nodosAgentes$nombre_registro_agente)
nodosAgentes$id_agente<-id_agente

nodosMLT<-bind_rows(nodosExpos,nodosAgentes) 

#buscar duliplicados
nodosMLT%>%
  group_by(nombre_registro_agente) %>% 
  filter(n()>1)

#curadores y artistas
enlacesArtCur<-curador_artistas_acumulado %>% 
  rename(from=a1,to=a2) %>%
  select(from,to,veces,relacion)

nodosArtCur<-MLTExpos1956_2016 %>%
  select(nombre_registro_agente,rol_expo, ano_agente_inicia,tipo_agente,nacionalidad_agente) %>%
  filter(rol_expo %in% c("curador","artista")) %>%
  distinct(nombre_registro_agente,.keep_all = T) 

#revisar que las listas de nodos y enlaces esten bien formadas
nrow(nodosMLT);length(unique(nodosMLT$id_agente))  
nrow(enlacesMLT); nrow(unique(enlacesMLT[,c("from", "to")]))

nodosverificacion<-unique(c(enlacesMLT$from,enlacesMLT$to))
verificacion<-nodosverificacion %in% nodosMLT$nombre_registro_agente
nodosverificacion[!verificacion]

nrow(nodosArtCur);length(unique(nodosArtCur$nombre_registro_agente))  
nrow(enlacesArtCur); nrow(unique(enlacesArtCur[,c("from", "to")]))

nodosverificacion<-unique(c(enlacesArtCur$from,enlacesArtCur$to))
verificacion<-nodosverificacion %in% nodosArtCur$nombre_registro_agente
nodosverificacion[!verificacion]



#crear grafos con igraph
gMLT <- graph_from_data_frame(d=enlacesMLT, vertices=nodosMLT, directed=F)
gArtCur<- graph_from_data_frame(d=enlacesArtCur,vertices = nodosArtCur,directed = T)
#gArtCur.el<-graph_(as.matrix(enlacesArtCurAcumulado[,c("from","to")]), directed = T)
# gExArt<-graph_from_data_frame(d=Expos.Artistas, vertices=nodosExArt, directed=F)
# gExCur<-graph_from_data_frame(d=Expos.Curadores, vertices=nodosExCur, directed=F)
# gExAus<-graph_from_data_frame(d=Expos.Auspiciadores, vertices=nodosExAus, directed=F)
# gExPre<-graph_from_data_frame(d=Expos.Presentadores, vertices=nodosExPre, directed=F)
# gExObras<-graph_from_data_frame(d=Expos.Obras, vertices=nodosExObras, directed=F)
# gArtCur<-graph_from_data_frame(d=Artista.Curador, vertices=nodosArtCur, directed=F)


#metricas de los grafos

V(gArtCur)$grado_in<-deg_in<-degree(gArtCur,mode = "in")
V(gArtCur)$grado_out<-deg_out<-degree(gArtCur,mode = "out")
V(gArtCur)$grado_total<-deg_total<-degree(gArtCur,mode = "total")

# 
V(gArtCur)$intermediacion_undir<-betweenness(graph = gArtCur , directed = F)
V(gArtCur)$intermediacion_dir<-betweenness(graph = gArtCur , directed = T)
V(gArtCur)$eigenvectores_undir<-evcent(gArtCur,directed = F)$vector
V(gArtCur)$eigenvectores_dir<-evcent(gArtCur,directed = T)$vector

V(gArtCur)$rank_undir<-page_rank(gArtCur,directed = F)$vector
V(gArtCur)$rank_dir<-page_rank(gArtCur,directed = T)$vector
V(gArtCur)$rank<-page_rank(gArtCur)$vector

V(gArtCur)$rank_undir_weighted<-page_rank(gArtCur,weights = E(gArtCur)$veces)$vector
V(gArtCur)$cercania_total<-closeness(gArtCur,mode = "total")
V(gArtCur)$cercania_out<-closeness(gArtCur,mode = "out")
V(gArtCur)$cercania_in<-closeness(gArtCur,mode = "in")

V(gMLT)$grado_total<-degree(gMLT,mode = "total")
V(gMLT)$intermediacion_undir<-betweenness(graph = gMLT , directed = F)
V(gMLT)$eigenvectores_undir<-evcent(gMLT,directed = F)$vector
V(gMLT)$rank_undir<-page_rank(gMLT,directed = F)$vector

#explorar datos graficas
hist(V(gArtCur)$grado_out, breaks=1:vcount(gArtCur)-1, main="Histogram of node degree")
sort(V(gArtCur)$grado,decreasing = T)[1:20]
sort(V(gArtCur)$grado_in,decreasing = T)[1:30]
sort(V(gArtCur)$grado_out,decreasing = T)[1:20]
sort(V(gArtCur)$eigenvectores_undir,decreasing = T)[1:20]
sort(V(gArtCur)$rank_dir,decreasing = T)[1:20]
sort(V(gArtCur)$intermediacion,decreasing = T)[1:20]


#examinar enlaces y vertices(nodos)
E(gMLT)       # The edges of the "net" object
V(gMLT)       # The vertices of the "net" object

E(gArtCur)       # The edges of the "net" object
V(gArtCur)       # The vertices of the "net" object


# Generate colors based on media type:
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
colrs <- c("tomato", "blue","green","orange","purple","pink")
#asicolor y tamaño de vertice
V(gMLT)$color <- colrs[as.integer(factor(V(gMLT)$tipo_agente))]
V(gMLT)$size<-sqrt(V(gMLT)$grado_total+1)

V(gArtCur)$color <- colrs[as.integer(factor(V(gArtCur)$rol_expo))]
V(gArtCur)$size<-sqrt(V(gArtCur)$grado_total)
E(gArtCur)$edge.color <- "gray80" 
E(gArtCur)$width <- 1+E(gArtCur)$veces/12
E(gArtCur)$weigtht<-E(gArtCur)$veces
# 
# V(gExArt)$color <- colrs[as.integer(factor(V(gExArt)$class_nodo))]
# #V(gExArt)$size<- degree(gExArt)/max(degree(gExArt))*10+2
# V(gExArt)$size<-sqrt(degree(gExArt))+2
# 
# V(gExCur)$color <- colrs[as.integer(factor(V(gExCur)$class_nodo))]
# #V(gExCur)$size<- degree(gExCur)/max(degree(gExCur))*10+2
# V(gExCur)$size<-sqrt(degree(gExCur))+2
# 
# V(gExAus)$color <- colrs[as.integer(factor(V(gExAus)$class_nodo))]
# #V(gExAus)$size<- degree(gExCur)/max(degree(gExCur))*10+2
# V(gExAus)$size<-sqrt(degree(gExAus))+2
# 
# V(gExPre)$color <- colrs[as.integer(factor(V(gExPre)$class_nodo))]
# V(gExPre)$size<-sqrt(degree(gExPre))+2
# 
# V(gExObras)$color <- colrs[as.integer(factor(V(gExObras)$class_nodo))]
# V(gExObras)$size<-sqrt(degree(gExObras))+2

#plot pelado
plot.igraph(gMLT,vertex.label=NA,vertex.frame.color='white')

#layout mds
plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  
     vertex.frame.color='white',layout=layout_with_mds)
legend(x=-1.5, y=-0.5, levels(factor(V(gMLT)$tipo_agente)), pch=21,col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)

# plot(gExArt, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
# plot(gExCur, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
# plot(gExAus, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
# plot(gExPre, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
# plot(gExObras, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)
# plot(gArtCur, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_mds)

plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_kk)

plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_graphopt)
legend(x=-1.5, y=-0.5, levels(factor(V(gMLT)$tipo_agente)), pch=21,col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)

plot(gMLT, edge.arrow.size=.4,vertex.label=NA,  vertex.frame.color='white',layout=layout_on_grid)

l<-layout_with_graphopt(gArtCur, start = NULL, niter = 900, charge = 0.1,
                        mass = 30, spring.length = 2, spring.constant = 0.1,
                        max.sa.movement = 5)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(gArtCur, edge.arrow.size=.1,vertex.label=NA, rescale=F, vertex.frame.color='white',layout=l*2)
plot(gArtCur, edge.arrow.size=.1,vertex.label=NA,  vertex.frame.color='white',layout=layout_with_kk)

## filtros

MLTExpos1956_2016 %>% 
  dplyr::filter(rol_expo=="artista") %>%
  dplyr::filter(nombre_registro_agente != "Varios")%>%
  group_by(nombre_registro_agente) %>%
  summarise(num_expos=n()) %>%
  filter(num_expos>4) %>%
  arrange(desc(num_expos)) %>%
  .$nombre_registro_agente %>% as.vector()->artistas_sel1

MLTExpos1956_2016 %>% 
  dplyr::filter(rol_expo=="artista") %>%
  dplyr::filter(nombre_registro_agente != "Varios")%>%
  dplyr::filter(obra_en_coleccion_MTL>0) %>%
  group_by(nombre_registro_agente) %>%
  summarise(num_expos=n()) %>%
  arrange(desc(num_expos)) %>%
  .$nombre_registro_agente %>% as.vector()->artistas_sel2

MLTExpos1956_2016 %>% 
  dplyr::filter(rol_expo=="curador") %>%
  group_by(nombre_registro_agente) %>%
  summarise(num_expos=n()) %>%
  arrange(desc(num_expos)) %>%
  .$nombre_registro_agente %>% as.vector()->curadores_sel


artistas_sel<-c(artistas_sel1,artistas_sel2) %>% unique()


curador_artistasel<-artistas_sel[artistas_sel %in% curadores_sel]
artistas_selu<-artistas_sel[!(artistas_sel %in% curador_artistasel)]
curadores_sel<-curadores_sel[!(curadores_sel %in% curador_artistasel)]

MLTExpos1956_2016 %>% 
  dplyr::filter(obra_en_coleccion_MTL>0) %>%
  dplyr::filter(nombre_registro_agente %in% artistas_sel)%>%
  group_by(nombre_registro_agente) %>%
  summarise(num_expos=n()) %>%
  arrange(desc(num_expos)) %>%
  .$nombre_registro_agente %>% as.vector()->artistas_obrasMLT



MLTExpos1956_2016 %>% 
  dplyr::filter(rol_expo=="artista", 
                  (id_expo %in% expos_curadas[expos_curadas$curada=="curada",]$id_expo)) %>% 
  dplyr::filter(nombre_registro_agente %in% artistas_selu) %>%
  mutate(nombre_s=stringi::stri_paste(nombre_expo," - ",ano),tipo_s="expo",
         nombre_t=nombre_registro_agente, tipo_t="artista") %>%
  select(nombre_s,tipo_s,nombre_t,tipo_t,ano)->enlacesMLTsel1


MLTExpos1956_2016 %>% 
  dplyr::filter(rol_expo=="artista", 
                (id_expo %in% expos_curadas[expos_curadas$curada=="curada",]$id_expo)) %>% 
  dplyr::filter(nombre_registro_agente %in% curador_artistasel) %>%
  mutate(nombre_s=stringi::stri_paste(nombre_expo," - ",ano),tipo_s="expo",
         nombre_t=nombre_registro_agente, tipo_t="artista-curador") %>%
  select(nombre_s,tipo_s,nombre_t,tipo_t,ano)->enlacesMLTsel2

MLTExpos1956_2016 %>% 
  dplyr::filter(rol_expo=="curador", 
                (id_expo %in% expos_curadas[expos_curadas$curada=="curada",]$id_expo)) %>% 
  dplyr::filter(nombre_registro_agente %in% curador_artistasel) %>%
  mutate(nombre_s=stringi::stri_paste(nombre_expo," - ",ano),tipo_s="expo",
         nombre_t=nombre_registro_agente, tipo_t="artista-curador") %>%
  select(nombre_s,tipo_s,nombre_t,tipo_t,ano)->enlacesMLTsel3

MLTExpos1956_2016 %>% 
  dplyr::filter(rol_expo=="curador", 
                (id_expo %in% expos_curadas[expos_curadas$curada=="curada",]$id_expo)) %>% 
  dplyr::filter(nombre_registro_agente %in% curadores_sel) %>%
  mutate(nombre_s=stringi::stri_paste(nombre_expo," - ",ano),tipo_s="expo",
         nombre_t=nombre_registro_agente, tipo_t="curador") %>%
  select(nombre_s,tipo_s,nombre_t,tipo_t,ano)->enlacesMLTsel4


MLT_obras %>% filter(nombre_artista_registro_std %in% artistas_obrasMLT) %>%
  transmute(nombre_s=nombre_artista_registro_std, 
            tipo_s=if_else(nombre_artista_registro_std %in% curador_artistasel,"artista-curador","artista"),
            nombre_t=stringi::stri_paste(titulo_obra," - ",cod_registro),
            tipo_t="obra", ano=ano_creacion)->enlacesMLTsel5

enlacesMLTsel<-bind_rows(enlacesMLTsel1,
                         enlacesMLTsel2,
                         enlacesMLTsel3,
                         # enlacesMLTsel5
                         enlacesMLTsel4) %>% distinct()

write_csv(enlacesMLTsel,"enlacesMTLsel.csv")
