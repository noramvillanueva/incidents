##########################################
# Funciones
# Calcula ano, mes, dia, hora, minuto,...
PreparaFechas <- function(datos) {
  hola=as.character(datos[,1])
  aux1=matrix(unlist(strsplit(hola, " ")),ncol=2,byrow=T)
  aux2=matrix(unlist(strsplit(aux1[,1], "/")),ncol=3,byrow=T)
  aux3=matrix(unlist(strsplit(aux1[,2], ":")),ncol=3,byrow=T)
  
  datos$ano=as.numeric(aux2[,3])
  datos$mes=as.numeric(aux2[,2])
  datos$dia=as.numeric(aux2[,1])
  datos$hora=as.numeric(aux3[,1])
  datos$minuto=as.numeric(aux3[,2])
  datos$horamin=paste0(aux3[,1],".",aux3[,2])
  datos$horamin=as.numeric(datos$horamin)
  datos$fecha=aux1[,1]
  datos}


