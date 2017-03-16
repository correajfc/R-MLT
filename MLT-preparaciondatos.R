
#Script para preparara los datos 

#librerias
library(readr)
library(dplyr)
library(tidyr)
#lectura de archivos
#matriz inicial de los datos de exposiciones y agentes
MLT_Exposiciones_agentes <- read_csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/Continua-MLT_DB_07_2016 - Exposiciones-agentes.csv",
col_types = cols(duracion_dias = col_integer(),
fecha_fin = col_date(format = "%Y-%m-%d"),
fecha_ini = col_date(format = "%Y-%m-%d")))

MLT_personas <- read_csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/Continua-MLT_DB_07_2016 - personas.csv")
MLT_organizaciones <- read_csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/Continua-MLT_DB_07_2016 - organizaciones.csv")
MLT_presentadores <- read_csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/Continua-MLT_DB_07_2016 - presentadores.csv")
MLT_curadores <- read_csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/Continua-MLT_DB_07_2016 - curadores.csv",
col_types = cols(id_organizacion = col_integer(),
id_persona = col_integer()))
MLT_auspiciadores <- read_csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/Continua-MLT_DB_07_2016 - auspiciadores.csv",
col_types = cols(id_organizacion = col_integer(),
id_persona = col_integer()))
MLT_documentos <- read_csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/Continua-MLT_DB_07_2016 - documentos.csv")
MLT_obras <- read_csv("~/Documents/Proyectos/Javeriana-Tertulia/R-MLT/Continua-MLT_DB_07_2016 - obras.csv")


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




MLT_expos<-select(MLT_Exposiciones_agentes,id_expo,expo_order,nombre_expo,fecha_ini,fecha_fin,duracion_dias,nombre_espacio,descripcion_expo)


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

expo_agentes_personas<- expo_agentes %>% 
  inner_join(MLT_personas,by=c("nombre_registro_agente"="nombre_registro_creador")) %>%
  rename(ano_agente_inicia=ano_nacimiento,ano_agente_fin=ano_muerte,nacionalidad_agente=nacionalidad) %>%
  select(-ocupaciones,-id_img__persona_fk,-(url_wikipedia_persona:tipo_creador)) %>% 
  mutate(tipo_agente="persona")

# expo_agentes %>% 
#   left_join(MLT_personas,by=c("nombre_registro_agente"="nombre_registro_creador")) %>%
#   rename(ano_agente_inicia=ano_nacimiento,ano_agente_fin=ano_muerte,nacionalidad_agente=nacionalidad) %>%
#   select(-ocupaciones,-id_img__persona_fk,-(url_wikipedia_persona:tipo_creador)) %>% 
#   mutate(tipo_agente="persona")%>% View()

expo_agentes_organizaciones<- expo_agentes %>% 
  inner_join(MLT_organizaciones,by=c("nombre_registro_agente"="nombre_institucion")) %>%
  rename(ano_agente_inicia=ano_constitucion,ano_agente_fin=ano_cierre,nacionalidad_agente=pais) %>%
  select(-financiacion,-sectores,-id_imagen_logo_fk) %>% 
  mutate(tipo_agente="organizacion")

# expo_agentes %>% 
#   left_join(MLT_organizaciones,by=c("nombre_registro_agente"="nombre_institucion")) %>%
#   rename(ano_agente_inicia=ano_constitucion,ano_agente_fin=ano_cierre,nacionalidad_agente=pais) %>%
#   select(-financiacion,-sectores,-id_imagen_logo_fk) %>% 
#   mutate(tipo_agente="organizacion")%>% View()

expo_agentes_expandido<-bind_rows(expo_agentes_personas,expo_agentes_organizaciones) 

  #buscar duliplicados
  expo_agentes_expandido%>%
  group_by(id_expo,rol_expo,nombre_registro_agente) %>% 
  filter(n()>1) 
# %>% View()
  
  MLTExpos1956_2106<-full_join(MLT_expos,expo_agentes_expandido,by="id_expo")
  write_csv(MLTExpos1956_2106,"MLTexpos1956-2016.csv")

#Documentos tratamiento a parte

expo_documentos<-select(MLT_Exposiciones_agentes,id_expo,documento_ppal) %>%
  rename(nombre_registro_agente=documento_ppal) %>%
  mutate(nombre_registro_agente=strsplit(as.character(nombre_registro_agente), ","),rol_expo="documento") %>%
  unnest(nombre_registro_agente) 




