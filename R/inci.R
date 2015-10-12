#' Creacion de la variable Inci1 e Inci2
#' 
#' @description La funcion permite crear las variables respuesta
#' inci1, incidencias en regimen de enfriamiento, e inci2, 
#' incidencias en regimen de calentamiento. 
#' 
#' Regimen enfriamiento, existe incidencia si 
#' Temperatura ambiente mayor que  Temperatura consigna mas  2 C, 
#' siendo la temperatura de consigna igual a  22 C, si la 
#' temperatura de impulsion del aire es menor que la 
#' temperatura ambiente menos 5 C.
#' 
#' Regimen calentamiento, existe incidencia si 
#' Temperatura ambiente menor que la  Temperatura consigna
#' menos 2 C, siendo la temperatura de consigna 
#' igual a 20 C, si la temperatura de impulsion del aire
#'  es mayor que la  temperatura ambiente mas 5 C.
#' 
#' @param datos La base de datos que se leyo y unifico utilizando la 
#' funcion readbbdd. 
#' 
#' @author Nora M. Villanueva y Javier Roca Pardinas
#' 
#' @examples
#' library(incidents)
#' ## Tarda unos minutos
#' # misdatos <- readbbdd(file = "lista_all.txt", file2 = "lista.txt)
#' # misdatos2<-inci(misdatos)
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

resul <- fnuminci12(data = dat) 
dat$inci <- resul$inci
dat$inci1 <- resul$inci1
dat$inci2 <- resul$inci2
res <- dat
res

}