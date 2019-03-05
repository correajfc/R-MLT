
# librerias ------
library(tidyr)
library(ggplot2)
library(lubridate)
# library(visdat)
library(stringr)

library(googledrive)
library(googlesheets)
library(igraph)
library(ggraph)
library(dplyr)
library(purrr)
library(wesanderson)



dbMTL60<-gs_key("1kjWlQAyLL9LRVpuVJsHtbvEhazSYxJvXlKDfS5k5JtQ")
MLT_Exposiciones_agentes<-dbMTL60 %>%
  gs_read(ws ="Exposiciones-agentes",range = cell_cols("A:AH"))


#funcion para evaluar el tipo de exposicion colectiva e individual
participacion_expo<-function(art){
  if(is.na(art) )
    return(NA)
  sp<-strsplit(as.character(art), ",")
  
  if(lengths(sp)==1 & !str_detect(sp,regex("varios", ignore_case = T)))
    return("individual")
  
  if(lengths(sp)==1 & str_detect(sp,regex("varios", ignore_case = T)))
    return("colectiva")
  
  if(lengths(sp) >1 )
    return("colectiva")
  
}

contar_artsitas<-function(art){
  if(is.na(art) )
    return(NA)
  sp<-strsplit(as.character(art), ",")
  
  if(lengths(sp)==1 & trimws(sp[1])=="Varios")
    return(NA)
  
  return(lengths(sp))
  
}


MLT_expos<- MLT_Exposiciones_agentes %>% rowwise() %>%
  mutate(tipo_participacion = participacion_expo(artistas) ) %>% 
  select(id_expo,expo_order,nombre_expo,fecha_ini,fecha_fin,duracion_dias,
         nombre_espacio,descripcion_expo, tipo_participacion)

#añadir campo identificacion de order-expo-año y campo del año
MLT_expos<-MLT_expos %>% 
  mutate(nombre_expo_order_ano= paste(expo_order,
                                      nombre_expo,
                                      year(fecha_ini),
                                      sep = "-"),
         ano=year(fecha_ini),ano_decimal = decimal_date(fecha_ini)) 

# construir grafo curador artistas ----

MLT_Exposiciones_agentes %>%
  select(id_expo,presentadores,curadores,artistas) %>%
  gather(key = "tipo", value = "curador_presentador", presentadores,curadores) %>%
  filter(!is.na(curador_presentador))%>%
  filter(!is.na(artistas)) %>%
  separate_rows(curador_presentador,sep = ",") %>% 
  separate_rows(artistas,sep = ",") %>%
  mutate_if( is.character, funs(trimws(.)))%>%
  filter(!str_detect(artistas,regex("varios",ignore_case = T))) -> tmp_enlaces

tmp_enlaces %>%
  select(agente=artistas) %>%
  # mutate(rol="artista")%>%
  distinct()->nodos_artistas

tmp_enlaces %>%
  select(rol=tipo,agente=curador_presentador) %>%
  distinct() ->nodos_pres_cur

nodos_pres_cur<- nodos_pres_cur %>%
  mutate(rol=case_when(rol =="presentadores" & agente %in% nodos_artistas$agente ~ "presentador-artista",
                       rol =="curadores" & agente %in% nodos_artistas$agente ~ "curador-artista",
                       rol =="presentadores" & !(agente %in% nodos_artistas$agente) ~ "presentador",
                       rol =="curadores" & !(agente %in% nodos_artistas$agente) ~ "curador")) 



nodos_artistas %>%
  left_join(nodos_pres_cur, by ="agente") %>%
  mutate(rol = if_else(is.na(rol),"artista",rol))%>%
  bind_rows(nodos_pres_cur) %>%
  distinct()->nodos_pres_cur_art

enlaces_pres_cur_art<-tmp_enlaces %>% 
  select(source=curador_presentador,target=artistas,id_expo) %>%
  left_join(select(MLT_expos,id_expo,nombre_expo_order_ano,ano,fecha_ini,ano_decimal), by ="id_expo")


# escribir en archivo de enlaces y nodos -------
write.csv(file = "output_data/MLT_pres_cur_art_links.csv",enlaces_pres_cur_art)
write.csv(file = "output_data/MLT_pres_cur_art_nodes.csv",nodos_pres_cur_art)


# filtrar por intervalos de tiempo definidos -------
# enlaces_pres_cur_art %>%
#   group_by(source,target) %>%
#   summarise(peso_enlace=n()) ->enlaces_pres_cur_art_all
enlaces_pres_cur_art %>%
  # filter(ano<=1965) %>%
  group_by(source,target) %>%
  summarise(peso_enlace=n()) %>% 
  arrange(desc(peso_enlace)) %>% 
  filter(str_detect(source,regex("[\\s\\S]*"))) %>% 
  filter(str_detect(target,regex("[\\s\\S]*"))) %>% 
  filter(str_detect(source,regex("alejandro",ignore_case = T)))


enlaces_pres_cur_art %>%
  filter(ano<=1965) %>%
  group_by(source,target) %>%
  summarise(peso_enlace=n()) ->enlaces_pres_cur_art_1956_1965

nodos_pres_cur_art_1956_1965<-nodos_pres_cur_art %>%
  filter(agente %in% c(enlaces_pres_cur_art_1956_1965$source,enlaces_pres_cur_art_1956_1965$target)) 



enlaces_pres_cur_art %>%
  filter(ano>1965 & ano<=1982) %>%
  group_by(source,target) %>%
  summarise(peso_enlace=n()) ->enlaces_pres_cur_art_1966_1982

nodos_pres_cur_art_1966_1982<-nodos_pres_cur_art %>%
  filter(agente %in% c(enlaces_pres_cur_art_1966_1982$source,enlaces_pres_cur_art_1966_1982$target)) 


enlaces_pres_cur_art %>%
  filter(ano>1982 & ano<=1992) %>%
  group_by(source,target) %>%
  summarise(peso_enlace=n()) ->enlaces_pres_cur_art_1983_1992

nodos_pres_cur_art_1983_1992<-nodos_pres_cur_art %>%
  filter(agente %in% c(enlaces_pres_cur_art_1983_1992$source,enlaces_pres_cur_art_1983_1992$target)) 


enlaces_pres_cur_art %>%
  filter(ano>1992 & ano<=2006) %>%
  group_by(source,target) %>%
  summarise(peso_enlace=n()) ->enlaces_pres_cur_art_1993_2006

nodos_pres_cur_art_1993_2006<-nodos_pres_cur_art %>%
  filter(agente %in% c(enlaces_pres_cur_art_1993_2006$source,enlaces_pres_cur_art_1993_2006$target)) 


enlaces_pres_cur_art %>%
  filter(ano>1982 & ano<=2006) %>%
  group_by(source,target) %>%
  summarise(peso_enlace=n()) ->enlaces_pres_cur_art_1983_2006

enlaces_pres_cur_art %>%
  filter(ano>2006 ) %>%
  group_by(source,target) %>%
  summarise(peso_enlace=n()) ->enlaces_pres_cur_art_2007_2016

nodos_pres_cur_art_2007_2016<-nodos_pres_cur_art %>%
  filter(agente %in% c(enlaces_pres_cur_art_2007_2016$source,enlaces_pres_cur_art_2007_2016$target))

# construir objetos de igraph -------

grafoPCA.1956_1965<-graph_from_data_frame(enlaces_pres_cur_art_1956_1965,
                                          directed = T, 
                                          vertices = nodos_pres_cur_art_1956_1965)

grafoPCA.1966_1982<-graph_from_data_frame(enlaces_pres_cur_art_1966_1982,
                                          directed = T,
                                          vertices = nodos_pres_cur_art_1966_1982)
grafoPCA.1983_1992<-graph_from_data_frame(enlaces_pres_cur_art_1983_1992,
                                          directed = T,
                                          vertices = nodos_pres_cur_art_1983_1992)
 grafoPCA.1993_2006<-graph_from_data_frame(enlaces_pres_cur_art_1993_2006,
                                           directed = T,
                                           vertices = nodos_pres_cur_art_1993_2006)
grafoPCA.2007_2016<-graph_from_data_frame(enlaces_pres_cur_art_2007_2016,directed = T,
                                          vertices = nodos_pres_cur_art_2007_2016)
# grafoPCA<-graph_from_data_frame(enlaces_pres_cur_art_all,directed = T,vertices = nodos_pres_cur_art)


# graficar  ------
roles<-nodos_pres_cur_art_1956_1965$rol
label_nodes <-if_else(degree(grafoPCA.1956_1965)>1,V(grafoPCA.1956_1965)$name,"")
p <- ggraph(grafoPCA.1956_1965, layout = "fr",niter = 1500) + 
  geom_edge_link(aes(width=peso_enlace),
                 arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(1, 'mm'),
                 start_cap=circle(1, 'mm'),
                 edge_colour= "grey60") + 
  geom_node_point(aes(color = factor(rol), size = degree(grafoPCA.1956_1965))) +
  geom_node_text(aes(label = label_nodes),
                 repel = F 
                 # nudge_x = nchar(label_nodes)/10
                 )+
  coord_fixed()+
  scale_edge_width(range = c(0.2,1), guide = "none")+
  scale_color_manual(values = wes_palette("Darjeeling1", n = length(roles%>%unique())))+
  scale_size(name = "grado")+
  ggtitle('Artistas, Presentadores y Curadores 1956-1965',
          subtitle = "Los nodos con nombre tienen 2 o más conexiones")
  

p+theme_graph()

 ggsave("output_data/g-mlt-pca-1956-1965.svg",device = "svg", units = "cm", width = 40 )
 ggsave("output_data/g-mlt-pca-1956-1965.png",device = "png", units = "cm", dpi = 300, width = 20  )


label_nodes <-if_else(degree(grafoPCA.1966_1982)>5,V(grafoPCA.1966_1982)$name,"")
g<-layout_with_fr(grafoPCA.1966_1982, niter = 1500)
# g<-layout_with_drl(grafoPCA.1966_1982,  options=drl_defaults$final)
p <- ggraph(grafoPCA.1966_1982,layout = "fr") + 
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(1, 'mm'),
                 start_cap=circle(1, 'mm'),
                 edge_colour= "grey60") + 
  geom_node_point(aes(color = factor(rol), size = degree(grafoPCA.1966_1982))) +
  geom_node_text(aes(label = label_nodes),
                 repel = T 
                 # nudge_x = nchar(label_nodes)/10
  )+
  coord_fixed()+
  scale_size(name = "grado")+
  scale_color_manual(values = wes_palette("Darjeeling1", 
                                          n = length(nodos_pres_cur_art_1966_1982$rol%>%unique())))+
  ggtitle('Artistas, Presentadores y Curadores 1966-1982',
          subtitle = "Los nodos con nombre tienen 5 o más conexiones")

p+theme_graph()

 ggsave("output_data/g-mlt-pca-1966-1982.svg",device = "svg", units = "cm", width = 40 )
 ggsave("output_data/g-mlt-pca-1966-1982.png",device = "png", units = "cm", width = 20,dpi = 300 )
 

label_nodes <-if_else(degree(grafoPCA.1983_1992)>5,V(grafoPCA.1983_1992)$name,"")
# g<-layout_with_fr(grafoPCA.1983_1992, niter = 1500)
# g<-layout_with_drl(grafoPCA.1983_1992,  options=drl_defaults$final)
p <- ggraph( grafoPCA.1983_1992,
            layout = "fr"
) + 
  geom_edge_link(aes(edge_width = peso_enlace,                
                     edge_alpha = peso_enlace
  ),
  arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(1, 'mm'),
                 start_cap=circle(1, 'mm'),
                 edge_colour= "grey60") + 
  geom_node_point(aes(color = factor(rol), size = degree(grafoPCA.1983_1992))) +
  geom_node_text(aes(label = label_nodes),
                 repel = T 
                 # nudge_x = nchar(label_nodes)/10
  )+
  coord_fixed()+
  scale_size(name = "grado")+
  scale_edge_alpha(range = c(1,0.2),guide = "none")+
  scale_edge_width(range = c(0.2,3), guide = "none")+
  scale_color_manual(values = wes_palette("Darjeeling1", 
                                          n = length(nodos_pres_cur_art_1983_1992$rol%>%unique())))+
  ggtitle('Artistas, Presentadores y Curadores 1983-1992',
          subtitle = "Los nodos con nombre tienen 5 o más conexiones")

p+theme_graph()

ggsave("output_data/g-mlt-pca-1983-1992.svg",device = "svg", units = "cm", width = 40 )
ggsave("output_data/g-mlt-pca-1983-1992.png",device = "png", units = "cm", width = 20, dpi = 300 )


label_nodes <-if_else(degree(grafoPCA.1993_2006)>15,V(grafoPCA.1993_2006)$name,"")
# g<-layout_with_fr(grafoPCA.1983_1992, niter = 1500)
# g<-layout_with_drl(grafoPCA.1983_1992,  options=drl_defaults$final)
p <- ggraph( grafoPCA.1993_2006,
             layout = "fr"
) + 
  geom_edge_link( aes(edge_width = peso_enlace,                
                       edge_alpha = peso_enlace
  ),
  arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(1, 'mm'),
                 start_cap=circle(1, 'mm'),
                 edge_colour= "grey60") + 
  geom_node_point(aes(color = factor(rol), size = degree(grafoPCA.1993_2006))) +
  geom_node_text(aes(label = label_nodes),
                 repel = T 
                 # nudge_x = nchar(label_nodes)/10
  )+
  coord_fixed()+
  scale_size(name = "grado")+
  scale_edge_width(range = c(0.2,3), guide = "none")+
  scale_edge_alpha(range = c(1,0.2),guide = "none")+
  scale_color_manual(values = wes_palette("Darjeeling1", 
                                          n = length(nodos_pres_cur_art_1993_2006$rol%>%unique())))+
  ggtitle('Artistas, Presentadores y Curadores 1993-2006',
          subtitle = "Los nodos con nombre tienen 15 o más conexiones")

p+theme_graph()

ggsave("output_data/g-mlt-pca-1993-2006.svg",device = "svg", units = "cm", width = 40 )
ggsave("output_data/g-mlt-pca-1993-2006.png",device = "png", units = "cm", width = 20, dpi = 300 )


label_nodes <-if_else(degree(grafoPCA.2007_2016)>15,V(grafoPCA.2007_2016)$name,"")
# g<-layout_with_fr(grafoPCA.2007_2016, niter = 1500)
# g<-layout_with_drl(grafoPCA.1983_1992,  options=drl_defaults$final)
p <- ggraph( grafoPCA.2007_2016,
             layout = "fr",niter=2000
) + 
  geom_edge_link(
    aes(edge_width = peso_enlace#,                
     # edge_alpha = peso_enlace
                     ),
arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(1, 'mm'),
                 start_cap=circle(1, 'mm'),
                 edge_colour= "grey60"
                 ) + 
  geom_node_point(aes(color = factor(rol), size = degree(grafoPCA.2007_2016))) +
  geom_node_text(aes(label = label_nodes),
                 repel = T 
                 # nudge_x = nchar(label_nodes)/10
  )+
  coord_fixed()+
  scale_size(name = "grado")+
  scale_color_manual(values = wes_palette("Darjeeling1", 
                                          n = length(nodos_pres_cur_art_2007_2016$rol%>%unique())))+
  scale_edge_width(range = c(0.2,1), guide = "none")+
  # scale_edge_alpha(range = c(1,0.2),guide = "none")+
  ggtitle('Artistas, Presentadores y Curadores 2007-2016',
          subtitle = "Los nodos con nombre tienen 15 o más conexiones")

p+theme_graph()

 ggsave("output_data/g-mlt-pca-2007-2016.svg",device = "svg", units = "cm", width = 40 )
 ggsave("output_data/g-mlt-pca-2007-2016.png",device = "png", units = "cm", width = 20, dpi = 300 )
 

 # contar la cantidad de curadores que ha tenido un artista -----
 
 enlaces_pres_cur_art_1983_1992 %>% 
   group_by(target) %>%
   summarise(curs=paste0(source,collapse= ","), num_curs=n()) %>%
   ungroup() %>%
   filter(num_curs > 1) 
