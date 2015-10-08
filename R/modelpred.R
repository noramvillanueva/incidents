#' Construccion de los modelos de prediccion de incidencias
#' 
#' @description La funcion construye dos modelos de prediccion 
#' de incidencias. Uno para incidencias provocadas en regimen de 
#' enfriamiento y el otro para aquellas provocadas  en regimen 
#' de calentamiento. Los modelos incluyen las covariables 
#' seleccionadas:   
#' 
#' amb.max: es la temperatura ambiente maxima (diaria) antes de que abra 
#'          la tienda, es decir,el mismo dia que se quiere hacer la 
#'          prediccion.
#'          
#' amb.mean.on: es la temperatura ambiente media (diaria) durante el horario
#'          comercial, es decir, el dia antes de hacer la prediccion. 
#'          
#' cli.mean: consumos de aire acondicionado (diario) durante el horario 
#'          comercial, es decir, el dia antes de hacer la prediccion. 
#'          
#' inci.5: numero de incidencias los 5 dias antes de hacer la prediccion.
#'        Es decir, si el dia anterior hubo incidencia, dicha variable 
#'        seria igual a 1, si los 5 dias antes ha habido incidencia, seria 
#'        igual a 5. Si ha habido incidencia durante 10 dias consecutivos, 
#'        el valor maximo de dicha variable es de 5. 
#' 
#' @param data  La base de datos creada con la funcion hour2day.
#' 
#' @param newdata Los nuevos valores a predecir incidencias. 
#' Nota: tiene que ser un objeto de la clase data.frame. 
#' 
#' @param pred.days Indica que la prediccion se hara para el dia
#'  siguiente. Por defecto es 1. Por ejemplo, si se pretende hacer
#'    prediccion a dos  dias, se sustituye el 1 por el dos. 
#' 
#' @param cut.point Vector con el punto de corte (o probabilidad)
#'  seleccionado a partir de la cual entendemos que existe incidencia.
#'  El primer valor es el punto de corte para el modelo de 
#'  enfriamiento mientras que el segundo valor para el modelo de
#'  calentamiento.
#' 
#' 
#' @author Nora M. Villanueva y Javier Roca Pardinas
#'
#' @examples
#' library(incidents)
#' # Tarda unos minutos
#' # misdatos <- readbbdd(file = "lista_all.txt", file2 = "lista.txt")
#' # misdatos2<-inci(misdatos)
#' # misdatos2 <-misdatos2[,-c(7,8)]
#' # datosdia <- hour2day(misdatos2)
#' ## Elimino las variables de impulsion y retorno tanto de agua,
#' ## como de aire.  
#' # datosdia <- datosdia[,c(1:22, 32,33)]
#' ## Por ejemplo: queremos predecir si va a haber incidencia 
#' ## en una tienda con los siguientes valores:
#' # datos2pred <-data.frame (amb.max = 40, amb.mean.on = 5, 
#' #              cli.mean = 150, inci.5d = 3)
#' # modelpred(data = datosdia, pred.days = 1, newdata = datos2pred, 
#' #  cut.point = c(0.1,0.1))
#' 
#' 
#' @import dplyr
#' @importFrom mgcv gam
#' @export


modelpred <- function(data,  newdata, pred.days = 1, cut.point = c(0.1, 0.1)){
dat <- data
dat$inci    <- lead(dat$inci, pred.days)
dat$inci1   <- lead(dat$inci1, pred.days)
dat$inci2   <- lead(dat$inci2, pred.days)
dat$amb.max <- lead(dat$amb.max, pred.days)
dat$amb.mean<- lead(dat$amb.mean, pred.days)
dat$amb.min <- lead(dat$amb.min, pred.days)
dat$mes     <- lead(dat$mes, pred.days)

ii <- seq(365,6570, 365)
dat$inci[ii]    <-NA
dat$inci1[ii]   <-NA
dat$inci2[ii]   <-NA
dat$amb.max[ii] <-NA
dat$amb.mean[ii]<-NA
dat$amb.min[ii] <-NA
dat$mes[ii]     <-NA

dat=na.omit(dat)



##############################
### MODELO ENFRIAMIENTO

m1 <- gam(inci1 ~ s(amb.max) +s(cli.mean)+  s(inci.5d, k = 5) ,
family = "binomial", data = dat)

muhat1 <- predict(m1, newdata = newdata, type = "response")

pred1 <- ifelse(muhat1 >= cut.point[1] ,1,0)



cat("---------------------", "\n")
cat("Modelo enfriamiento", "\n")
cat("---------------------", "\n")

print(summary(m1))

cat("", "\n")




###############################
# MODELO CALENTAMIENTO

m2=gam(inci2~s(amb.mean.on)+ s(inci.5d, k=5)  + s(cli.mean) ,
       family="binomial", data=dat)

muhat2=predict(m2, newdata = newdata, type="response")
pred2=ifelse(muhat2 >= cut.point[2],1,0)


cat("---------------------", "\n")
cat("Modelo calentamiento", "\n")
cat("---------------------", "\n")

print(summary(m2))

cat("", "\n")

cat("---------------------", "\n")
cat("Resultados de prediccion", "\n")
cat("---------------------", "\n")


print(data.frame(newdata, prob1 = round(muhat1, 3), prob2 = round(muhat2, 3),
           predict1 = pred1, predict2 = pred2))


}