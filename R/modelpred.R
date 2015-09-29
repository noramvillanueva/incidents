modelpred <- function(data, pred.days = 1, newdata){
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

#pred.tiendas<-data.frame(dat$tienda, round(muhat1,2))
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
#data.frame(test$inci1,round(muhat1c,2))
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










