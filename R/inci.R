#' Creación de la variable Inci1 e Inci2
#' 
#' @description La función permite crear las variables respuesta
#' inci1 (incidencias en régimen de enfriamiento) e inci2 (inci-
#' dencias en régimen de calentamiento). 
#' 
#' Régimen enfriamiento: existe incidencia si 
#' Temperatura ambiente > Temperatura consigna + 2 C, siendo la 
#' temperatura de consigna = 22 C, si la temperatura de impulsión 
#' del aire es menor que la temperatura ambiente − 5 C.
#' 
#' Régimen calentamiento: existe incidencia si 
#' Temperatura ambiente < Temperatura consigna - 2 C, siendo la 
#' temperatura de consigna = 22 C, si la temperatura de impulsión 
#' del aire es mayor que la temperatura ambiente + 5 C.
#' 
#' @param datos La base de datos que se leyó y unificó utilizando la 
#' función readbbdd. 
#' 
#' @author Nora M. Villanueva y Javier Roca Pardinas
#' 
#' @examples
#' library(incidents)
#' ## Ojo, tarda unos minutos
#' # misdatos <- readbbdd(file = "lista_all.txt", file2 = "lista.txt)
#' # misdatos2<-inci(misdatos)
#' # elimino las variables: impulsion.agua.fria e impulsion.agua.caliente
#' # misdatos2 <-misdatos2[,-c(7,8)] 
#' @export


inci <- function(datos){
dat <- datos
dat$newcon <- dat$consigna
enf <- dat$estado.maquina == 1 &
  dat$apertura.tienda == 1 &
  dat$impulsion.aire <= (dat$ambiente - 5) 
cal <- dat$estado.maquina == 1 &
  dat$apertura.tienda == 1 &
  dat$impulsion.aire > (dat$ambiente + 5)

dat$newcon[enf] <- 22
dat$newcon[cal] <- 20

#source("fnuminci12.R")
resul <- fnuminci12(data = dat) 
dat$inci <- resul$inci
dat$inci1 <- resul$inci1
dat$inci2 <- resul$inci2
res <- dat
res

}





