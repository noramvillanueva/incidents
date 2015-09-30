#' Construcción de los modelos de predicción de incidencias
#' 
#' @description La función construye dos modelos de predicción 
#' de incidencias. Uno para incidencias provocadas en régimen de 
#' enfriamiento y el otro para aquellas provocadas  en régimen 
#' de calentamiento. Los modelos incluyen las covariables 
#' seleccionadas:   
#' amb.max: es la temperatura ambiente máxima (diaria) antes de que abra 
#'          la tienda, es decir,el mismo día que se quiere hacer la 
#'          predicción.
#' amb.mean.on: es la temperatura ambiente media (diaria) durante el horario
#'          comercial, es decir, el día antes de hacer la predicción. 
#' cli.mean: consumos de aire acondicionado (diario) durante el horario 
#'          comercial, es decir, el día antes de hacer la predicción. 
#' inci.5: número de incidencias los 5 días antes de hacer la predicción.
#'        Es decir, si el día anterior hubo incidencia, dicha variable 
#'        sería igual a 1, si los 5 días antes ha habido incidencia, sería 
#'        igual a 5. Si ha habido incidencia durante 10 días consecutivos, 
#'        el valor máximo de dicha variable es de 5. 
#' 
#' @param data La base de datos creada con la función hour2day.
#' 
#' @param newdata Los nuevos valores a predecir incidencias. 
#' Nota: tiene que ser un objeto de la clase data.frame. 
#' 
#' @param pred.days Por defecto es 1. Indica que la predicción se
#' hará para el día siguiente. Por ejemplo, si se pretende hacer 
#' predicción a dos días, se sustituye el 1 por el dos. 
#' 
#' @author Nora M. Villanueva y Javier Roca Pardinas
#'
#' @examples
#' library(incidents)
#' # Ojo, tarda unos minutos
#' # misdatos <- readbbdd(file = "lista_all.txt", file2 = "lista.txt)
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
#' # modelpred(data = datosdia, pred.days = 1, newdata = datos2pred)
#' 
#' 
#' @import dplyr
#' @importFrom mgcv gam
#' @export


modelpred <- function(data,  newdata, pred.days = 1){
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

pred1 <- ifelse(muhat1 >= 0.5,1,0)



cat("---------------------", "\n")
cat("Modelo enfriamiento", "\n")
cat("---------------------", "\n")

print(summary(m1))

cat("", "\n")




###############################
# MODELO CALENTAMIENTO

#251 # 0.9385
m2=gam(inci2~s(amb.mean.on)+ s(inci.5d, k=5)  + s(cli.mean) ,
       family="binomial", data=dat)

muhat2=predict(m2, newdata = newdata, type="response")
pred2=ifelse(muhat2 >= 0.5,1,0)


cat("---------------------", "\n")
cat("Modelo calentamiento", "\n")
cat("---------------------", "\n")

print(summary(m2))

cat("", "\n")

cat("---------------------", "\n")
cat("Resultados de predicción", "\n")
cat("---------------------", "\n")


data.frame(newdata, prob1 = round(muhat1, 3), prob2 = round(muhat2, 3),
           predict1 = pred1, predict2 = pred2)


}










