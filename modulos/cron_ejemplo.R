# Ejemplo de scraping

# cargar paquetes
library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)

source("funciones.R")

# enlaces ----
# crea un vector con enlaces web de páginas donde los medios tienen enlaces a sus noticias
enlaces <- paste0("https://elsiglo.cl/category/pais/page/", 1:3); hist = ""


# obtener enlaces de noticias ----
# luego, hacemos scraping a cada enlace para obtener los enlaces a las noticias
# individuales que existen en cada página

resultados_enlaces <- map(enlaces, \(enlace) {
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  sesion <- bow(enlace) |> scrape()
  
  noticias_sesion <- sesion |> 
    html_elements(".entry-title") |>
    html_elements("a") |> 
    html_attr("href")
  
  message(glue("Se obtuvieron {length(noticias_sesion)} noticias en {enlace}"))
  
  return(noticias_sesion)
})

enlaces_elsiglo <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
# por cada enlace de noticia obtenido, se entra a la noticia y se obtienen sus textos
resultados_elsiglo <- map(enlaces_elsiglo, \(enlace) {
  
  # enlace de prueba
  # enlace <- "https://elsiglo.cl/estacion-central-desafios-para-crecimiento-con-equidad-de-genero/"    
  
  # revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  # envolver en try para que no eche a perder el loop si hay un error
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    
    #elementos
    x_titulo <- noticia |> html_elements("main") |>  html_elements(".entry-title") |> html_text2()
    
    x_bajada <- noticia |> html_elements("main") |> html_elements("blockquote") |> html_text2()
    
    x_fecha_texto <- noticia |> html_elements("main") |> html_elements(".entry-meta") |> html_elements(".date") |> html_text2()
    
    mes <- x_fecha_texto |> str_extract("\\w+") |> tolower() |> 
      recode("enero" = "1", "febrero" = "2", "marzo" = "3",
             "abril" = "4", "mayo" = "5", "junio" = "6",
             "julio" = "7", "agosto" = "8", "septiembre" = "9",
             "octubre" = "10", "noviembre" = "11", "diciembre" = "12")
    
    dia_año <-  x_fecha_texto |> 
      str_extract("\\d+, \\d{4}") |> 
      str_remove(",")
    
    año <- dia_año |> str_extract("\\d{4}$")
    
    dia <- dia_año |> str_extract("^\\d+")
    
    x_fecha <- paste(año, mes, dia)
    
    x_cuerpo <- noticia |> html_elements(".entry-content") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    # unir elementos en un dataframe
    noticia_data <- tibble("titulo" = x_titulo |> validar_elementos(),
                           "bajada" = x_bajada |> validar_elementos(),
                           "fecha" = x_fecha |> validar_elementos(),
                           "cuerpo" = x_cuerpo |>  validar_elementos(),
                           "fuente" = "elsiglo",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping elsiglo: ", e)
    return(NULL)}
  )
}) |> 
  list_rbind()


# guardar ----
# crea una carpeta para guardar los resultados y los guarda con la fecha de hoy
dir.create("resultados/elsiglo/", showWarnings = F)

readr::write_rds(resultados_elsiglo, 
                 glue("resultados/elsiglo/elsiglo_cron_{today()}.rds"))

message(glue("listo cron elsiglo {now()}"))
