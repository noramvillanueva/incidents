#' Análisis exploratorio de los datos
#' 
#' @description La función hace un gráfico, para una tienda, una 
#' máquina, un año, un mes y un día. En él se representan todas 
#' las variables como: temperatura ambiente, temperatura exterior,
#'  consumos general y clima, temperatura de impulsión y retorno 
#'  de agua, y temperatura de impulsión y retorno de aire. 
#' 
#' @param datos La base de datos que se creó utilizando la 
#' función inci. 
#' @param año El año que se quiere representar.
#' 
#' @param mes El mes que se quiere representar.
#' 
#' @param dia El día que se quiere representar.
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
#' # Ojo, tarda unos minutos
#' # misdatos <- readbbdd(file = "lista_all.txt", file2 = "lista.txt)
#' # misdatos2<-inci(misdatos)
#' # misdatos2 <-misdatos2[,-c(7,8)] 
#' # monitoring(misdatos2, año = 2014, mes = 5,
#' #    dia = 23, tienda = "T1001", maquina = "A61702")
#' 
#' 
#' 
#' @export
#' 
#' 















monitoring<-function(dat, año = año, mes = mes, dia = dia, 
                            tienda = tienda, maquina = maquina){
#  source("fmonitoringnew.R")
  
 for (i in dia) {
    fmonitoringnew(dia = i, mes = mes, año = año, 
                   tienda = tienda, maquina = maquina, 
                   dat = dat, mititulo = paste0(i, "/", mes, "/", año))
  }
}