

MLT_Exposiciones_agentes %>% glimpse()
MLT_expos %>% glimpse()
id_expos_colectivas <- filter(MLT_expos,tipo_participacion=="colectiva") %>% pull(id_expo)
# solo los artistas de exposiciones colectivas 

enlace_art_art <- MLT_Exposiciones_agentes %>%  
  select(id_expo,artistas) %>% 
  filter(id_expo %in% id_expos_colectivas) %>% 
  separate_rows(artistas,sep = ",") %>% 
  filter(!str_detect(artistas,regex("varios",ignore_case = T))) %>%
  mutate(artistas = str_trim(artistas)) %>% 
  group_by(id_expo) %>% 
  do(t(combn(.$artistas, 2)) %>% as_data_frame()) %>% 
  ungroup() %>% 
  rename(source = V1,target = V2) %>% 
  left_join(select(MLT_expos, id_expo,nombre_expo_order_ano,ano_decimal), by = "id_expo")
  

write.csv(file = "MLT_art_art_links.csv",enlaces_pres_cur_art)
