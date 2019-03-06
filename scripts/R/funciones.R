

# funciones ---------------------------------------------------------------



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

#funcion para evaluar si hay NA en una columna
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


