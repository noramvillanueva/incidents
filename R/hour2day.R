#' Creación de la base de datos diaria
#' 
#' @description La función crea una nueva base de datos diaria, es
#' decir, resume la información horaria en información diaria. 
#' Para ello se han creado nuevas variables. Por ejemplo, temperatura
#' ambiente mínima, máxima o media. 
#' 
#' @param datos La base de datos creada con la función inci.
#' 
#' @author Nora M. Villanueva y Javier Roca Pardinas
#' 
#' @examples
#' library(incidents)
#' ## Ojo, tarda unos minutos
#' # misdatos <- readbbdd(file = "lista_all.txt", file2 = "lista.txt)
#' # misdatos2<-inci(misdatos)
#' # datosdia <- hour2day(misdatos2)
#' ## Elimino las variables de impulsion y retorno tanto de agua,
#' ## como de aire.  
#' # datosdia <- datosdia[,c(1:22, 32,33)]
#' 
#' @import dplyr
#' @export


hour2day <- function(datos){
dat <- datos
# max, min, mean t.ambiente tienda cerrada. 
aux0 <- filter(dat, ano == 2014, apertura.tienda == 0)

damb<-summarise(group_by(aux0, tienda, maquina, mes, dia, tipo,ciudad), 
             amb.max = max(ambiente, na.rm = TRUE),  
             amb.min = min(ambiente, na.rm = TRUE),
             amb.mean = mean(ambiente, na.rm = TRUE))
 
damb$amb.max[damb$amb.max=="NaN"]=NA; damb$amb.min[damb$amb.min=="NaN"]=NA
damb$amb.mean[damb$amb.mean=="NaN"]=NA


aux00 <- filter(dat, ano == 2014, apertura.tienda == 1)

damb.on<-summarise(group_by(aux00, tienda, maquina, mes, dia, tipo,ciudad), 
                amb.max.on = max(ambiente, na.rm = TRUE),  
                amb.min.on = min(ambiente, na.rm = TRUE),
                amb.mean.on = mean(ambiente, na.rm = TRUE))

damb.on$amb.max.on[damb.on$amb.max.on=="NaN"]=NA; damb.on$amb.min.on[damb.on$amb.min.on=="NaN"]=NA
damb.on$amb.mean.on[damb.on$amb.mean.on=="NaN"]=NA

#mean t.exterior durante horario comercial  
aux1 <- filter(dat, ano == 2014, apertura.tienda == 1 , estado.maquina == 1)

xx <- group_by(aux1, tienda, maquina, mes, dia, tipo,ciudad)
dext<-summarise(xx, ext.max = max(exterior, na.rm = FALSE),
                ext.min = min(exterior, na.rm = FALSE),
                ext.mean = mean(exterior, na.rm = FALSE))
dext$ext.max[dext$ext.max=="NaN"]=NA;dext$ext.min[dext$ext.min=="NaN"]=NA;
dext$ext.mean[dext$ext.mean=="NaN"]=NA;


#mean, median general  horario comercial
dgen<-summarise(xx, gen.mean = mean(general, na.rm = TRUE),
                gen.med = median(general, na.rm = TRUE))
dgen$gen.mean[dgen$gen.mean=="NaN"]=NA;dgen$gen.med[dgen$gen.med=="NaN"]=NA;


#mean, median  clima  horario comercial
dcli<-summarise(xx, cli.mean = mean(clima, na.rm = TRUE),
                cli.med = median(clima, na.rm = TRUE))
dcli$cli.mean[dcli$cli.mean=="NaN"]=NA;dcli$cli.med[dcli$cli.med=="NaN"]=NA;


#mean, median impulsion.aire  horario comercial
diaire<-summarise(xx, iaire.mean = mean(impulsion.aire, na.rm = TRUE),
                  iaire.med = median(impulsion.aire, na.rm = TRUE))
diaire$iaire.mean[diaire$iaire.mean=="NaN"]=NA;
diaire$iaire.med[diaire$iaire.med=="NaN"]=NA

#mean, median retorno.aire  horario comercial
draire<-summarise(xx, raire.mean = mean(retorno.aire, na.rm = TRUE),
                  raire.med = median(retorno.aire, na.rm = TRUE))
draire$raire.mean[draire$raire.mean=="NaN"]=NA;
draire$raire.med[draire$raire.med=="NaN"]=NA


#mean, median impulsion.agua  horario comercial
diagua<-summarise(xx, iagua.mean = mean(impulsion.agua, na.rm = TRUE),
                  iagua.med = median(impulsion.agua, na.rm = TRUE))
diagua$iagua.mean[diagua$iagua.mean=="NaN"]=NA;
diagua$iagua.med[diagua$iagua.med=="NaN"]=NA


#mean, median retorno.agua  horario comercial
dragua<-summarise(xx, ragua.mean = mean(retorno.agua, na.rm = TRUE),
                  ragua.med = median(retorno.agua, na.rm = TRUE))
dragua$ragua.mean[dragua$ragua.mean=="NaN"]=NA;
dragua$ragua.med[dragua$ragua.med=="NaN"]=NA


#mean consigna horario comercial
dcon<-summarise(xx, con.mean = mean(newcon, na.rm = TRUE))
dcon$con.mean[dcon$con.mean == "NaN"]=NA



#inci1(enfriamiento, "verano"), inci2(calentamiento, "invierno") al dia
dinci<-summarise(group_by(aux1, tienda, maquina, mes, dia, tipo), 
                 inci = max(inci), inci1 = max(inci1), inci2 = max(inci2))  

#---> uno bbdd para crear inci.5d
dd <- full_join(damb,dext)
dd<-full_join(dd, dinci)

# num. incidencias 5 dias antes.
num_inci_5d <- function(ii, var){
  sum(lead(var,ii)[1:5], na.rm = TRUE)
}

inci5d <- c(); ii <- unique(dd$maquina)
for (m in ii){
  aux.1 <- filter(dd, maquina == m)
  aux.1 <-data.frame(aux.1)
  aux.2 <- sapply(c(0:(length(aux.1[ ,1])-5)), num_inci_5d, var = aux.1$inci)
  aux.2 <- c(rep(NA,4),aux.2)
  inci5d <- c(inci5d, aux.2) 
}

# numero horas tienda abierta
aux2 <- filter(dat, ano == 2014, apertura.tienda == 1)
dhor<-summarise(group_by(aux2, tienda, maquina, mes, dia, tipo, ciudad), 
                count = n())
dhor <- mutate(dhor, horamin =  count/4)
dhor<-dhor[,-6]
dhor<-dhor[,-6]

#############################
## UNION BASES DE DATOS
dd<-full_join(dd, damb.on)
dd<-full_join(dd,dgen)
dd<-full_join(dd,dcli)
dd<-full_join(dd,diaire)
dd<-full_join(dd,draire)
dd<-full_join(dd,diagua)
dd<-full_join(dd,dragua)
dd<-full_join(dd,dcon)
dd<-full_join(dd,dhor)
inci5d[is.na(dd$inci)]=NA
dfin<-mutate(data.frame(dd),inci.5d = inci5d)
}

