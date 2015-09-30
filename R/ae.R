#' Análisis exploratorio de los datos
#' 
#' @description La función lleva a cabo un pequeño análisis
#' exploratorio de los datos. Calcula el número total de 
#' incidencias, las incidencias por tipo, las incidencias 
#' por mes, y por tipo y por mes, tanto para las incidencias
#' provocadas en régimen de enfriamiento como de calentamiento.
#' 
#' @param datos La base de datos que se creó utilizando la 
#' función inci. 
#' 
#' @author Nora M. Villanueva y Javier Roca Pardinas
#' 
#' @examples
#' library(incidents)
#' ## Ojo, tarda unos minutos
#' # misdatos <- readbbdd(file = "lista_all.txt", file2 = "lista.txt)
#' # misdatos2<-inci(misdatos)
#' # misdatos2 <-misdatos2[,-c(7,8)] 
#' # mianalisis <- ae(misdatos2)
#' 
#' @export


ae <- function(datos){
dat <- datos  

  inc2 <- filter(dat, inci1 == 1, ano == 2014)
  mis2 <- distinct(inc2, maquina, fecha)
  ntot2 <- length(mis2[ ,1])
 
  inctipoenf <- group_by(inc2, tipo)
  inctipo1enf <- distinct(inctipoenf, maquina, fecha)
  incidenciastipoenf <- summarise(inctipo1enf, ntipo = n())
  
  t1 <- table(mis2$mes)
  f1 <- prop.table(t1)*100
  tenf <- table(inctipo1enf$tipo,inctipo1enf$mes) #enf


  inc3 <- filter(dat, inci2 == 1, ano == 2014)
  mis3 <- distinct(inc3, maquina, fecha)
  ntot3 <- length(mis3[ ,1])
  
  inctipocal <- group_by(inc3, tipo)
  inctipo1cal <- distinct(inctipocal, maquina, fecha)
  incidenciastipocal <- summarise(inctipo1cal, ntipo = n())
  t2 <- table(mis3$mes)
  f2 <- prop.table(t2)*100
  tcal <- table(inctipo1cal$tipo,inctipo1cal$mes) #cal
  cat("---------------------", "\n")
  cat("Régimen Enfriamiento", "\n")
  cat("--------------------", "\n")
  cat("Número incidencias totales:", ntot2, "\n")
  cat("", "\n")
  cat("Número incidencias por tipo:", "\n")
  print(incidenciastipoenf)
  cat("", "\n")
  cat("Número incidencias por mes:", "\n")
  print(rbind(t1,f1))
  cat("", "\n")
  cat("Número incidencias por tipo y por mes:", "\n")
  print(tenf)
  cat("", "\n")
  cat("----------------------", "\n")
  cat("Régimen Calentamiento", "\n")
  cat("----------------------", "\n")
  cat("Número incidencias totales:", ntot3, "\n")
  cat("Número incidencias por tipo:", "\n")
  print(incidenciastipocal)
  cat("", "\n")
  cat("Número incidencias por mes:",  "\n")
  print(rbind(t2,f2))
  cat("", "\n")
  cat("Número incidencias por tipo y por mes:", "\n")
  print(tcal)
  cat("", "\n")
}