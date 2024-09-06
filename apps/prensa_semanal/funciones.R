redactar_fecha <- function(x) { 
  mes = month(x)
  mes_t = recode(mes, 
                 "1" = "enero",
                 "2" = "febrero",
                 "3" = "marzo",
                 "4" = "abril",
                 "5" = "mayo",
                 "6" = "junio",
                 "7" = "julio",
                 "8" = "agosto",
                 "9" = "septiembre",
                 "10" = "octubre",
                 "11" = "noviembre",
                 "12" = "diciembre")
  
  fecha_etiqueta = paste(day(x), "de", mes_t)
  return(fecha_etiqueta)
}