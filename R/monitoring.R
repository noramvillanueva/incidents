#' Analisis exploratorio de los datos
#' 
#' @description La funcion hace un grafico, para una tienda, una 
#' maquina, un anho, un mes y un dia. En el se representan todas 
#' las variables como: temperatura ambiente, temperatura exterior,
#'  consumos general y clima, temperatura de impulsion y retorno 
#'  de agua, y temperatura de impulsion y retorno de aire. 
#' 
#' @param datos La base de datos que se creo utilizando la 
#' funcion inci. 
#' @param ano El anho que se quiere representar.
#' 
#' @param mes El mes que se quiere representar.
#' 
#' @param dia El dia que se quiere representar.
#' 
#' @param tienda La tienda que se quiere representar,
#' entre comillas. 
#' 
#' @param maquina La maquina que se quiere representar,
#' entre comillas. 
#' 
#' @author Nora M. Villanueva y Javier Roca Pardinas
#' 
#' @examples
#' library(incidents)
#' ##  Tarda unos minutos
#' # misdatos <- readbbdd(file = "lista_all.txt", 
#' #              file2 = "lista.txt)
#' # misdatos2<-inci(misdatos)
#' # misdatos2 <-misdatos2[,-c(7,8)] 
#' # monitoring(misdatos2, ano = 2014, mes = 5,
#' #    dia = 23, tienda = "T1001", maquina = "A61702")
#' 
#' 
#' 
#' @export
#' 
#' 





monitoring<-function(datos, ano = ano, mes = mes, dia = dia, 
                     tienda = tienda, maquina = maquina){
  
  
  for (i in dia) {
    fmonitoringnew(datos, dia = i, mes = mes, ano = ano, 
                     tienda = tienda, maquina = maquina, 
                     mititulo = paste0(i, "/", mes, "/", ano))
  }
}