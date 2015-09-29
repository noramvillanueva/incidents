# Funcion que crea nueva variable con 1 o 0
# indicando si hubo incidencia o no
# periodos superiores a 2 horas
#################################################
cumul_ones_fast <- function(x){
  rl <- rle(x)
  aux <- as.matrix(data.frame(rl$value,rl$lengths))
  fun <- function(x){
    if(x[1] == 1 & x[2] > 8){
      rep(1, x[2])
    }else{
      rep(0, x[2])
    } 
  }
  return(unlist(apply(aux,1,fun)))
}

# Funcion para crear variable inci1 e inci2
##########################################################
fnuminci12 <- function(data){
  data$preinci <- NULL
  data$preinci1 <- NULL
  data$preinci2 <- NULL
  # preincidencia cuando la TA es !=  TC-2 < TA < TC + 2
  data$preinci<-ifelse(data$apertura.tienda == 1 &
                         data$estado.maquina >= 1 &
                         (data$ambiente > data$newcon + 2 |
                            data$newcon - 2 > data$ambiente),  1 , 0)  
  #  preincidencia1 cuando la TA > TC + 2
  #  en regimen ENFRIAMIENTO VERANO
  data$preinci1<-ifelse(data$apertura.tienda == 1 &
                         data$estado.maquina >= 1 &
                         data$ambiente > data$newcon + 2,  1 , 0) 
  # preincidencia2 cuando  TA < TC-2 
  #  en regimen CALENTAMIENTO INVIERNO
  data$preinci2<-ifelse(data$apertura.tienda == 1 &
                          data$estado.maquina >= 1 &
                          data$newcon - 2 > data$ambiente,  1 , 0)
  
  # incidencia cuando la TC no se alcanza en periodos de > 2 horas
  data$inci <- cumul_ones_fast(data$preinci)
  data$inci1 <- cumul_ones_fast(data$preinci1)
  data$inci2 <- cumul_ones_fast(data$preinci2)

 result<-list(inci = data$inci, inci1 = data$inci1, inci2 = data$inci2)
  return(result)
}

