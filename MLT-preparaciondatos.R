
#Script para preparara los datos 
Sys.setlocale("LC_ALL", "ES_ES.UTF-8")

#librerias
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(visdat)
library(stringr)

library(googledrive)
library(googlesheets)

mishojas<-gs_ls()
drive_ls("MLT_DB_07_2016")

mishojas %>% filter(str_detect(sheet_title,"Continua-MLT_DB_07_2016") ) -> gsMLT
gsMLT$sheet_key

dbMTL60<-gs_key(gsMLT$sheet_key)
MLT_Exposiciones_agentes<-dbMTL60 %>%
  gs_read(ws ="Exposiciones-agentes",range = cell_cols("A:AH"))

MLT_personas <- dbMTL60 %>%
  gs_read(ws ="personas",range = cell_cols("A:Q"))

MLT_organizaciones <-  dbMTL60 %>%
  gs_read(ws ="organizaciones",range = cell_cols("A:H"))

MLT_presentadores <-   dbMTL60 %>%
  gs_read(ws ="presentadores",range = cell_cols("A:C"))


MLT_curadores <- dbMTL60 %>%
  gs_read(ws ="curadores",range = cell_cols("A:E"))
MLT_auspiciadores <- dbMTL60 %>%
  gs_read(ws ="auspiciadores",range = cell_cols("A:E"))

MLT_documentos <- dbMTL60 %>%
  gs_read(ws ="documentos",range = cell_cols("A:T"))
MLT_obras <- dbMTL60 %>%
  gs_read(ws ="obras",range = cell_cols("A:AB"))



#examinar datos y convertir tipos de datos
#buscar duliplicados
MLT_personas %>%
  mutate(nombre_registro_creador=trimws(nombre_registro_creador)) %>%
group_by(nombre_registro_creador) %>% 
  filter(n()>1)

#eliminar duliplicados
MLT_personas<-MLT_personas %>%
  mutate(nombre_registro_creador=trimws(nombre_registro_creador)) %>%
  distinct(nombre_registro_creador,.keep_all=TRUE)

#buscar duliplicados
MLT_organizaciones %>%
  mutate(nombre_institucion=trimws(nombre_institucion)) %>%
  group_by(nombre_institucion) %>% 
  filter(n()>1)

#eliminar duliplicados
MLT_organizaciones<-MLT_organizaciones %>%
  mutate(nombre_institucion=trimws(nombre_institucion)) %>%
  distinct(nombre_institucion,.keep_all=TRUE)

#funcion para evaluar el tipo de exposicion colectiva e individual
participacion_expo<-function(art){
  if(is.na(art) )
    return(NA)
  sp<-strsplit(as.character(art), ",")
  
  if(lengths(sp)==1 & trimws(sp[1])!="Varios")
    return("individual")
  
  if(lengths(sp)==1 & trimws(sp[1])=="Varios")
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
  mutate(nombre_expo_order_ano= paste(expo_order,nombre_expo,format(fecha_ini,"%Y"),sep = "-"),
         ano=as.integer(format(fecha_ini,"%Y"))) 



#exposiciones y artistas
expo_artistas<-select(MLT_Exposiciones_agentes,id_expo,artistas) %>% 
  rename(nombre_registro_agente=artistas) %>%
  mutate(nombre_registro_agente=strsplit(as.character(nombre_registro_agente), ","),rol_expo="artista") %>%
  unnest(nombre_registro_agente) %>% 
  mutate(nombre_registro_agente=trimws(nombre_registro_agente))
  

expo_presentadores<-select(MLT_Exposiciones_agentes,id_expo,presentadores) %>%
  rename(nombre_registro_agente=presentadores) %>%
  mutate(nombre_registro_agente=strsplit(as.character(nombre_registro_agente), ","),rol_expo="presentador") %>%
  unnest(nombre_registro_agente) %>% 
  mutate(nombre_registro_agente=trimws(nombre_registro_agente))

expo_curadores<-select(MLT_Exposiciones_agentes,id_expo,curadores) %>%
  rename(nombre_registro_agente=curadores) %>%
  mutate(nombre_registro_agente=strsplit(as.character(nombre_registro_agente), ","),rol_expo="curador") %>%
  unnest(nombre_registro_agente) %>% 
  mutate(nombre_registro_agente=trimws(nombre_registro_agente))

expo_auspiciadores<-select(MLT_Exposiciones_agentes,id_expo,apoyan_auspicia) %>%
  rename(nombre_registro_agente=apoyan_auspicia) %>%
  mutate(nombre_registro_agente=strsplit(as.character(nombre_registro_agente), ","),rol_expo="auspiciador") %>%
  unnest(nombre_registro_agente)%>% 
  mutate(nombre_registro_agente=trimws(nombre_registro_agente))

expo_agentes<-bind_rows(expo_artistas,expo_curadores) %>%
  bind_rows(expo_auspiciadores) %>% 
  bind_rows(expo_presentadores) %>% na.omit()

#buscar duliplicados
expo_agentes%>%
  group_by(id_expo,rol_expo,nombre_registro_agente) %>% 
  filter(n()>1)
# %>% View()
#eliminar duliplicados
expo_agentes<-expo_agentes %>%
  distinct(id_expo,rol_expo,nombre_registro_agente,.keep_all=TRUE)

#completar datos de la tabla de personas
expo_agentes_personas<- expo_agentes %>% 
  inner_join(MLT_personas,by=c("nombre_registro_agente"="nombre_registro_creador")) %>%
  rename(ano_agente_inicia=ano_nacimiento,ano_agente_fin=ano_muerte,nacionalidad_agente=nacionalidad) %>%
  select(-ocupaciones,-id_img__persona_fk,-(url_wikipedia_persona:observaciones_personas)) %>% 
  mutate(tipo_agente="persona")

#completar datos de la tabla de organizaciones
expo_agentes_organizaciones<- expo_agentes %>% 
  inner_join(MLT_organizaciones,by=c("nombre_registro_agente"="nombre_institucion")) %>%
  rename(ano_agente_inicia=ano_constitucion,ano_agente_fin=ano_cierre,nacionalidad_agente=pais) %>%
  select(-financiacion,-sectores,-id_imagen_logo_fk) %>% 
  mutate(tipo_agente="organizacion")


#unir en una tabla
expo_agentes_expandido<-bind_rows(expo_agentes_personas,expo_agentes_organizaciones) 

  #buscar duliplicados
  expo_agentes_expandido%>%
  group_by(id_expo,rol_expo,nombre_registro_agente) %>% 
  filter(n()>1) 
# %>% View()
  #añador las exposiciones a las relaciones y guardar en archivo
  MLTExpos1956_2016<-full_join(MLT_expos,expo_agentes_expandido,by="id_expo")
  write_csv(MLTExpos1956_2016,"MLTexpos1956-2016.csv")
  
###############################################  
#generacion de relaciones artistas artistas

    expo_artista_artista<-select(MLT_Exposiciones_agentes,id_expo,artistas) %>% 
    mutate(artistas_c=artistas)%>%
    transform(artistas_c = strsplit(as.character(artistas_c),","))
    # %>%
    #   transform(artistas_c=trimws(artistas_c))

# Itera por cada cada una de las listas de artistas    
  x<-expo_artista_artista$artistas_c
  idex<-expo_artista_artista$id_expo
  relation_df<- data.frame(id_expo = integer(0),a1=character(0),a2=character(0))
  for(i in 1:length(idex)){
    if(length(x[[i]])>1 ){
      relation_matrix<-combn(trimws(x[[i]]),2)
      ids<-rep(i, times = dim(relation_matrix)[2])
      a1<-as.vector(relation_matrix[1,])
      a2<-as.vector(relation_matrix[2,])
      tmp_df<-data.frame(id_expo=ids,a1=a1,a2=a2)
      relation_df<-bind_rows(relation_df,tmp_df)
    }
  }
  
  #
    
#buscar duliplicados
relation_df%>%
  group_by(id_expo,a1,a2) %>% 
  filter(n()>1)
# %>% View()
#eliminar duliplicados
expo_artista_artista<-relation_df %>%
  distinct(id_expo,a1,a2,.keep_all=TRUE) %>%
  mutate(rol_a1="artistas",rol_a2="artista")


#generacion de relaciones curador artistas

    expo_curador_artista<-select(MLT_Exposiciones_agentes,id_expo,curadores,artistas) %>% 
    mutate(curadores_c=curadores, artistas_c=artistas)%>%
    transform(curadores_c = strsplit(as.character(curadores_c),",")) %>%
      transform(artistas_c = strsplit(as.character(artistas_c),",")) 
    #   transform(artistas_c=trimws(artistas_c))

# Itera por cada cada una de las listas de artsitas    
  x<-expo_curador_artista$curadores_c
  y<-expo_curador_artista$artistas_c
  idex<-expo_curador_artista$id_expo
  relation_df2<- data.frame(id_expo = integer(0),a1=character(0),a2=character(0))
  for(i in 1:length(idex)){
    #i<-196
    if(length(x[[i]])==1 & length(y[[i]])==1 && !is.na(x[[i]]) && !is.na(y[[i]]) && trimws(y[[i]])!="Varios")
    {
      tmp_df<-data.frame(id_expo=idex[[i]],a1=x[[i]],a2=y[[i]])
      bind_rows(relation_df2,tmp_df)
    }
    if(length(x[[i]])==1 & length(y[[i]])>1 && !is.na(x[[i]]) )
      {
      
      ids<-rep(i, times = length(y[[i]]))
      a1<-rep(trimws(x[[i]]), times =length(y[[i]]))
      a2<-trimws(y[[i]])
      tmp_df<-data.frame(id_expo=ids,a1=a1,a2=a2)
      relation_df2<-bind_rows(relation_df2,tmp_df)
    }
    
    if(length(x[[i]])>1 & length(y[[i]])>1 && !is.na(x[[i]]) )
      {
      
      for(j in 1:length(x[[i]])){
      ids<-rep(i, times = length(y[[i]]))  
      a1<-rep(trimws(x[[i]][j]), times =length(y[[i]]))
      a2<-trimws(y[[i]])
      tmp_df<-data.frame(id_expo=ids,a1=a1,a2=a2)
      relation_df2<-bind_rows(relation_df2,tmp_df)
      }
    }
  }
    
#buscar duliplicados
relation_df2%>%
  group_by(id_expo,a1,a2) %>% 
  filter(n()>1)
# %>% View()
#eliminar duliplicados
expo_curador_artista<-relation_df2 %>%
  distinct(id_expo,a1,a2,.keep_all=TRUE)%>%
  mutate(rol_a1="curador",rol_a2="artista")

#relaciones entre artistas-artistas y artistas curadores
expo_agentes_curadore_artistas<-bind_rows(expo_artista_artista,expo_curador_artista)

#añador las exposiciones a las relaciones y guardar en archivo
grafoMLTcur_art_art_1956_2016<-full_join(MLT_expos,expo_agentes_curadore_artistas,by="id_expo")
  write_csv(grafoMLTcur_art_art_1956_2016,"grafoMLTcur_art_art_1956_2016.csv")

grafoMLTcur_art_1956_2016<-full_join(MLT_expos,expo_curador_artista,by="id_expo")
  write_csv(grafoMLTcur_art_1956_2016,"grafoMLTcur_art_1956_2016.csv")
  
grafoMLTar_art_1956_2016<-full_join(MLT_expos,expo_artista_artista,by="id_expo")
  write_csv(grafoMLTar_art_1956_2016,"grafoMLTart_art_1956_2016.csv")  
  
#relaciones (parejas) agrupadas por relacion acumulado en la historia de exposiciones
#artista-artista  
  artistas_artistas_acumulado<-expo_artista_artista%>%
    group_by(a1,a2)%>%summarise(veces=n()) %>%
    arrange(desc(veces))%>%
    mutate(relacion="artista-artista")
#curador-artista  
  curador_artistas_acumulado<-expo_curador_artista%>%
    group_by(a1,a2)%>%summarise(veces=n()) %>%
    arrange(desc(veces))  %>%
    mutate(relacion="curador-artista")

#grafo total simplificado relaciones curador-artista y artista-artista 
  agente_agente_relacion<-bind_rows(curador_artistas_acumulado,artistas_artistas_acumulado)
#guardar en archivos
  write_csv(agente_agente_relacion,"grafoMLTsimplificado_agente_agente_1956_2016.csv")
    write_csv(artistas_artistas_acumulado ,"grafoMLTsimplificado_art_art_1956_2016.csv")
        write_csv(curador_artistas_acumulado ,"grafoMLTsimplificado_cur_art_1956_2016.csv")


  
###### Ver histogramas 
  #artistas -artistas
qplot(main="Histograma de parejas de artistas",veces, data = artistas_artistas_acumulado[artistas_artistas_acumulado$veces>0,], geom = "histogram")
qplot(main="Histograma de parejas de artistas con 2 o más exposiciones",veces, data = artistas_artistas_acumulado[artistas_artistas_acumulado$veces>=2,], geom = "histogram")
qplot(main="Histograma de parejas de artistas con 3 o más exposiciones",veces, data = artistas_artistas_acumulado[artistas_artistas_acumulado$veces>=3,], geom = "histogram")
qplot(main="Histograma de parejas de artistas con 4 o más exposiciones",veces, data = artistas_artistas_acumulado[artistas_artistas_acumulado$veces>=4,], geom = "histogram")
qplot(main="Histograma de parejas de artistas con 5 o más exposiciones",veces, data = artistas_artistas_acumulado[artistas_artistas_acumulado$veces>=5,], geom = "histogram")
#curador-artistas
qplot(main="Histograma de parejas de curador-artistas",veces, data = curador_artistas_acumulado[curador_artistas_acumulado$veces>0,], geom = "histogram")
qplot(main="Histograma de parejas de curador-artistas con 2 o más exposiciones",veces, data = curador_artistas_acumulado[curador_artistas_acumulado$veces>=2,], geom = "histogram")
qplot(main="Histograma de parejas de curador-artistas con 3 o más exposiciones",veces, data = curador_artistas_acumulado[curador_artistas_acumulado$veces>=3,], geom = "histogram")
qplot(main="Histograma de parejas de curador-artistas con 4 o más exposiciones",veces, data = curador_artistas_acumulado[curador_artistas_acumulado$veces>=4,], geom = "histogram")
qplot(main="Histograma de parejas de curador-artistas con 5 o más exposiciones",veces, data = curador_artistas_acumulado[curador_artistas_acumulado$veces>=5,], geom = "histogram")


####################################


    
#Documentos tratamiento a parte

select(MLT_Exposiciones_agentes,id_expo,documento_ppal) %>%
  rename(nombre_registro_agente=documento_ppal) %>%
  mutate(nombre_registro_agente=strsplit(as.character(nombre_registro_agente), ","),rol_expo="documento") %>%
  unnest(nombre_registro_agente) -> expo_documentos_ppal




