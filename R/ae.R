
ae <- function(dat){
  
  # ENFRIAMIENTO
  inc2 <- filter(dat, inci1 == 1, ano == 2014)
  mis2 <- distinct(inc2, maquina, fecha)
  ntot2 <- length(mis2[ ,1])
 
  inctipoenf <- group_by(inc2, tipo)
  inctipo1enf <- distinct(inctipoenf, maquina, fecha)
  incidenciastipoenf <- summarise(inctipo1enf, ntipo = n())
  
  t <- table(mis2$mes)
  f <- prop.table(t)*100
  tenf <- table(inctipo1enf$tipo,inctipo1enf$mes) #enf

  
  # CALENTAMIENTO
  inc3 <- filter(dat, inci2 == 1, ano == 2014)
  mis3 <- distinct(inc3, maquina, fecha)
  ntot3 <- length(mis3[ ,1])
  
  inctipocal <- group_by(inc3, tipo)
  inctipo1cal <- distinct(inctipocal, maquina, fecha)
  incidenciastipocal <- summarise(inctipo1cal, ntipo = n())
  t3 <- table(mis3$mes)
  f3 <- prop.table(t3)*100
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
  print(rbind(t,f))
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
  print(rbind(t3,f3))
  cat("", "\n")
  cat("Número incidencias por tipo y por mes:", "\n")
  print(tcal)
  cat("", "\n")
}