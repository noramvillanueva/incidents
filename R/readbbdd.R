# LECTURA AUTOMATIZADA BBDD y UNION
readbbdd <- function(file = file, file2 = NULL){
  dat=NULL
  res = c()
  lista = as.vector(read.table(paste0( "", file, "" )))  #
  lista = as.vector(lista[,1])
  nlista = length(lista)
  source("fpreparafechas.R")

  if(!is.null(file2)){
    # lectura de otra variable 
    lista2 = as.vector(read.table(paste0( "", file2, "" )))
    lista2 = as.vector(lista2[,1])
    nlista2 = length(lista2)
  }
  
for (i in 1:nlista) { 
    
    file = lista[i]
    aux = unlist(strsplit(file, "_"))
    tienda = aux[1]; maquina = unlist( strsplit( aux[2], "[.]" ) )[1]
    datos = read.csv2(file)
    
    
    if(!is.null(file2)){
          
      for (j in 1:nlista2) {
      
        file2 = lista2[j]
        aux2 = unlist(strsplit(file2, "_"))

        if(aux2[1] == tienda){
          tienda2 <- aux2[1]    
          datos2 <- read.csv2(file2) #paste0("/Users/Nora/Dropbox/Compartidas/ITMATI_ecomt/Correo\ 4\ -\ Temperaturas\ Exteriores/TaExterior/",fichero2,"")
          alldat <- merge( datos, datos2, by.x = "Fecha", by.y = "Fecha", all.x = TRUE )
        }
        
      }
      
      datos2 = PreparaFechas(alldat)
      datos2$tienda <- tienda
      datos2$maquina <- maquina
      dat = rbind(dat,datos2)
    
    }else{ 
      datos = PreparaFechas(datos)
      datos$tienda <- tienda
      datos$maquina <- maquina
      dat <- datos }  
}
  
  
  # ordeno los datos
  ii=order(dat$tienda, dat$maquina,dat$ano,dat$mes,dat$dia,dat$hora,dat$minuto)
  dat=dat[ii,]
  
  # creo variable tipo (calle y cc)
  tcalle <- c("T127", "T152", "T241", "T307", "T536", "T710", "T718", 
              "T760", "T773", "T837", "T905", "T919", "T920", "T990", "T992", 
              "T995", "T996", "T1001")
  dat$tipo <- rep("cc", length(dat[, 1]))
  ii <- dat$tienda %in% tcalle
  dat$tipo[ii] <- "calle"
  
 
  
  # creo variable ciudad
  tt<-unique(dat$tienda)
  nt<-length(tt)
  ciudad<-c("sevilla", "ourense", "madrid", "ourense", "ourense", "lugo", 
            "pontevedra", "alicante", "pontevedra", "zaragoza", "valladolid",
            "zaragoza", "zaragoza", "barcelona", "sevilla", "finestrat", "valladolid",
            "ourense", "madrid", "madrid", "barcelona", "barcelona",
            "madrid", "madrid", "madrid", "huelva", "madrid",
            "sevilla", "valladolid", "valencia")
  dat$ciudad <- rep(NA, length(dat[,1]))
  for(j in 1:nt){ 
    ii <- dat$tienda==tt[j] 
    dat$ciudad[ii] <- ciudad[j]  
  }
  
  # cambio nombre variables -> minusculas
  dat<-rename(dat, consigna = Consigna, ambiente = Ambiente, impulsion.aire = Impulsion.aire,
              retorno.aire =  Retorno.aire, impulsion.agua = Impulsion.agua, 
              impulsion.agua.fria = Impulsion.agua.fria, 
              impulsion.agua.caliente = Impulsion.agua.caliente, 
             retorno.agua = Retorno.agua, estado.maquina = Estado.maquina, 
              apertura.tienda =  Apertura.tienda, general = General, clima = Clima,
              exterior = T.exterior)
 
  
 dat
}

