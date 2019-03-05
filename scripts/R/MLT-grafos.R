# grafo artistas curadores + artistas-artistas

names(MLT_Exposiciones_agentes)

MLT_expos_art_cur<- MLT_Exposiciones_agentes %>%
  select(id_expo,expo_order,nombre_expo,fecha_ini,artistas,curadores)


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

MLT_expos_art_cur<- MLT_expos_art_cur %>% rowwise() %>%
  mutate(tipo_participacion = participacion_expo(artistas) ) 

#añadir campo identificacion de order-expo-año y campo del año
MLT_expos_art_cur<-MLT_expos_art_cur %>% 
  mutate(nombre_expo_order_ano= paste(expo_order,nombre_expo,format(fecha_ini,"%Y"),sep = "-"),
         ano=as.integer(format(fecha_ini,"%Y"))) 

