### CREACION DE LAS VARIABLES INCI1 e INCI2
inci <- function(dat){

dat$newcon <- dat$consigna
enf <- dat$estado.maquina == 1 &
  dat$apertura.tienda == 1 &
  dat$impulsion.aire <= (dat$ambiente - 5) 
cal <- dat$estado.maquina == 1 &
  dat$apertura.tienda == 1 &
  dat$impulsion.aire > (dat$ambiente + 5)

dat$newcon[enf] <- 22
dat$newcon[cal] <- 20

source("fnuminci12.R")
resul <- fnuminci12(data = dat) 
dat$inci <- resul$inci
dat$inci1 <- resul$inci1
dat$inci2 <- resul$inci2
res <- dat
res

}





