fmonitoringnew <- function(dia, mes, año, tienda, maquina, dat, mititulo){
  
  x <- dat$horamin[ dat$dia == dia & dat$mes == mes & 
                      dat$ano == año & dat$tienda == tienda &
                      dat$maquina == maquina]
  
  y1 <- dat$ambiente[ dat$dia == dia & dat$mes == mes & 
                        dat$ano == año & dat$tienda == tienda &
                        dat$maquina == maquina]
  color <- dat$inci2[dat$dia == dia & dat$mes == mes & 
                      dat$ano == año & dat$tienda == tienda &
                      dat$maquina == maquina]+1
  y1.1 <- dat$consigna[ dat$dia == dia & dat$mes == mes & 
                          dat$ano == año & dat$tienda == tienda &
                          dat$maquina == maquina]
  y1.2 <- dat$newcon[ dat$dia == dia & dat$mes == mes & 
                        dat$ano == año & dat$tienda == tienda &
                        dat$maquina == maquina]
  y2 <- dat$exterior[ dat$dia == dia & dat$mes == mes & 
                          dat$ano == año & dat$tienda == tienda &
                          dat$maquina == maquina]
  y3 <- dat$general[ dat$dia == dia & dat$mes == mes & 
                       dat$ano == año & dat$tienda == tienda &
                       dat$maquina == maquina]
  y4 <- dat$clima[ dat$dia == dia & dat$mes == mes & 
                     dat$ano == año & dat$tienda == tienda &
                     dat$maquina == maquina]
  y5 <- dat$impulsion.aire[ dat$dia == dia & dat$mes == mes & 
                              dat$ano == año & dat$tienda == tienda &
                              dat$maquina == maquina]
  y6 <- dat$retorno.aire[ dat$dia == dia & dat$mes == mes & 
                            dat$ano == año & dat$tienda == tienda &
                            dat$maquina == maquina]
  y7 <- dat$impulsion.agua[ dat$dia == dia & dat$mes == mes & 
                              dat$ano == año & dat$tienda == tienda &
                              dat$maquina == maquina]
  y8 <- dat$retorno.agua[ dat$dia == dia & dat$mes == mes & 
                            dat$ano == año & dat$tienda == tienda &
                            dat$maquina == maquina]  
  
  apert <- min(na.omit(dat$horamin[dat$dia == dia & dat$mes == mes & 
                                     dat$ano == año & dat$tienda == tienda &
                                     dat$maquina == maquina & dat$apertura.tienda == 1]) )
  yfinmin<-min(y1.1,y1.2,na.rm=TRUE)
  yfinmax<-max(y1.1,y1.2,na.rm=TRUE)
  
 #quartz()
  par(mfrow=c(2,3))
  
  
  if(sum(!is.na(y1))!=0){
    plot(x, y1, cex = 0.5, pch = 16, xlab = "Hora", ylab = "Ambiente",
         ylim = c(min(y1, yfinmin, na.rm = TRUE), max(y1,yfinmax, na.rm = TRUE)), col=color)
    points(x, y1.2, cex = 0.5, pch = 16, col="darkgreen")
    abline(v=apert, lty = 2)
    mtext(paste(mititulo),  side = 3)
  } 
  
  
  if(sum(!is.na(y2))!=0){
    plot(x, y2, cex = 0.5, pch = 16, xlab = "Hora", ylab = "Exterior",
         ylim = c(min(y2, na.rm = TRUE), max(y2, na.rm = TRUE)))
    mtext(paste(tienda,maquina),  side = 3)
  }
  # CONSUMOS ENERGETICOS
  if(sum(!is.na(y3))!=0){
    plot(x, y3, cex = 0.5, pch = 16, xlab = "Hora",main="Consumo", ylab = "Consumo",
         ylim = c(min(y3, na.rm = TRUE), max(y3, na.rm = TRUE)))
    points(x, y4, cex = 0.5, pch = 16,
           ylim = c(min(y3,y4, na.rm = TRUE), max(y3,y4, na.rm = TRUE)), col="blue")
    legend("topleft",col=c(1,4), legend=c("General","Clima"), pch=16,  bty = "n")
  }
  #AGUA
  if(sum(!is.na(y7))!=0){
    plot(x, y7, cex = 0.5, pch = 16, main='Agua',  xlab = "Hora", ylab = "Agua",
         ylim = c(min(y7,y8, na.rm = TRUE), max(y7,y8, na.rm = TRUE)))
    points(x, y8, cex = 0.5, pch = 16,
           ylim = c(min(y7,y8, na.rm = TRUE), max(y7,y8, na.rm = TRUE)), col="blue")
    legend("topleft",col=c(1,4), legend=c("Impulsion","Retorno"), pch=16,  bty = "n")
  }
  
  # AIRE
  if(sum(!is.na(y5))!=0&sum(!is.na(y6))!=0){
    plot(x, y5, cex = 0.5, pch = 16,  main='Aire', xlab = "Hora", ylab = "Aire",
         ylim = c(min(y5,y6, na.rm = TRUE), max(y5,y6, na.rm = TRUE)))
    points(x, y6, cex = 0.5, pch = 16, 
           ylim = c(min(y5,y6, na.rm = TRUE), max(y5,y6, na.rm = TRUE)), col="blue")
    legend("topleft",col=c(1,4), legend=c("Impulsion","Retorno"), pch=16,  bty = "n")
  }else if(sum(!is.na(y5))!=0){
    plot(x, y5, cex = 0.5, pch = 16,main='Aire', xlab = "Hora", ylab = "Impulsion.aire",
         ylim = c(min(y5,y6, na.rm = TRUE), max(y5,y6, na.rm = TRUE)))
    
  }else if(sum(!is.na(y6))!=0){
    plot(x, y6, cex = 0.5, pch = 16, xlab = "Hora", ylab = "Retorno.aire",
         ylim = c(min(y5,y6, na.rm = TRUE), max(y5,y6, na.rm = TRUE)),col="blue")
  }
  
 
}