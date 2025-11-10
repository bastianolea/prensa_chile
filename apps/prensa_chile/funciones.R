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


recodificar_fuentes <- function(data) {
  data |> 
    mutate(fuente = case_match(fuente,
                               "24horas" ~ "24 Horas",
                               "adnradio" ~ "ADN Radio",
                               "agricultura" ~ "Agricultura",
                               "biobio" ~ "Radio BíoBío",
                               "chvnoticias" ~ "CHV Noticias",
                               "ciper" ~ "Ciper",
                               "cnnchile" ~ "CNN Chile",
                               "cooperativa" ~ "Cooperativa",
                               "diariofinanciero" ~ "D. Financiero",
                               "elciudadano" ~ "El Ciudadano",
                               "eldinamo" ~ "El Dínamo",
                               "elmostrador" ~ "El Mostrador",
                               "elsiglo" ~ "El Siglo",
                               "emol" ~ "Emol",
                               "exante" ~ "Ex-Ante",
                               "lacuarta" ~ "La Cuarta",
                               "lahora" ~ "La Hora",
                               "lanacion" ~ "La Nación",
                               "latercera" ~ "La Tercera",
                               "meganoticias" ~ "Meganoticias",
                               "publimetro" ~ "Publimetro",
                               "radiouchile" ~ "Radio U. de Ch.",
                               "t13" ~ "T13",
                               "theclinic" ~ "The Clinic", 
                               "redgol" ~ "RedGol",
                               "lasegunda" ~ "La Segunda",
                               "eldesconcierto" ~ "El Desconcierto",
                               "quintopoder" ~ "El Quinto Poder",
                               .default = fuente))
}


sentimiento_calcular <- function(data) {
  data |> 
    summarize(n = n(),
              n_positivo = sum(sentimiento == 1, na.rm = T),
              # n_neutral = sum(sentimiento == 0, na.rm = T),
              n_negativo = sum(sentimiento == -1, na.rm = T),
              sentimiento = mean(sentimiento, na.rm = T),
              fecha = min(fecha), 
              .groups = "drop") |> 
    mutate(p_negativas = n_negativo/n,
           p_positivas = n_positivo/n)
}


js_get_vertical_position <- function() {
  # get vertical scroll position using javascript
  tags$head(
    tags$script("
    var posicion_y = [0]; 
      $(document).on('scroll', function() {
        posicion_y[0] = window.pageYOffset;
        Shiny.onInputChange('posicion_y', posicion_y);
      });
      ")
  ) 
}