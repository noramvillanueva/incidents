monitoring<-function(dat, año = 2014, mes = 5, dia = 23, 
                            tienda = "T1001", maquina = "A61702"){
  source("fmonitoringnew.R")
  
 for (i in dia) {
    # names(incidencias2$num$T1002)[1:length(names(incidencias2$num$T1002))-1]
    fmonitoringnew(dia = i, mes = mes, año = año, 
                   tienda = tienda, maquina = maquina, 
                   dat = dat, mititulo = paste0(i, "/", mes, "/", año))
  }
}