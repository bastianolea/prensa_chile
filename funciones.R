ejecutar <- function(script = "modulos/cron_elsiglo.r", 
                     esperar = FALSE) {
  
  ruta_log = script |> 
    stringr::str_replace("\\.r$", ".log") |> 
    stringr::str_replace("modulos/", "logs/")
  
  invisible(suppressWarnings(file.remove(ruta_log)))
  
  comando <- paste0("nohup Rscript ", script, " >> ", ruta_log, " 2>&1")
  
  Sys.sleep(0.1)
  system(comando, wait = esperar)
}


scraping_prensa <- function(script = "modulos/cron_radiopaulina.r") {
  message(glue::glue("iniciando {script} - {lubridate::now()}"))
  
  # con source
  # glue("scraping/{f}") |> source() |> intentar(f)
  
  # en subproceso (sesión) interactivo
  # rstudioapi::jobRunScript(script, workingDir = "/Users/baolea/R/prensa")
  
  # # en subproceso (callr) no interactivo
  # proc <- callr::r_bg(\(script) source(script), 
  #                     supervise = TRUE,
  #                     stderr = stringr::str_replace(script, "\\.r$", "\\.log"),
  #                     stdout = stringr::str_replace(script, "\\.r$", "\\.log")
  #                     )
  
  # ejecutar script en el fondo no interactivo
  ejecutar(script)
}



beep_n <- function(x = 3) {
  walk(1:3, ~{beep(1); Sys.sleep(0.15)}) 
}

# 
# iniciar_selenium <- function(espera = 10, puerto = 4445L) {
#   message("iniciando contenedor en puerto ", puerto)
#   ##detener contenedores
#   #system("docker stop $(docker ps -a -q)", wait = T, timeout = 10, ignore.stdout = T)
#   #Sys.sleep(espera)
#   #iniciar nuevo contenedor
#   system(glue::glue('docker run -d -p {puerto}:4444 selenium/standalone-firefox'), 
#          wait = T, timeout = 10)#, ignore.stdout = T)
#   Sys.sleep(espera)
# }
# 
# 
# reiniciar_selenium <- function(espera = 5) {
#   message("reiniciando contenedores")
#   #detener contenedores
#   system("docker stop $(docker ps -a -q)", wait = T, timeout = 10, ignore.stdout = T)
#   Sys.sleep(espera)
#   #iniciar nuevo contenedor
#   system('docker run -d -p 4445:4444 selenium/standalone-firefox', wait = T, timeout = 10, ignore.stdout = T)
#   Sys.sleep(espera)
# }
# 
# 
# ver_contenedores <- function() {
#   system("docker ps")
# }
# 
# cerrar_contenedores <- function() {
#   message("cerrando todos los contenedores")
#   system("docker stop $(docker ps -a -q)", ignore.stderr = T)
#   Sys.sleep(5)
#   system("docker rm $(docker ps -a -q)", ignore.stderr = T)
# }
# #cerrar_contenedores()


#cambiar elementos sin contenido por missing
validar_elementos <- function(input, colapsar = FALSE) {
  
  if (colapsar == TRUE) {
    input2 <- paste(input, collapse = "\n")
  } else {
    input2 <- input
  }
  
  output <- ifelse(length(input) == 0, NA_character_, input2)
  
  return(output)
}



intentar <- function(x, nombre = "prueba") {
  tryCatch(x, 
           error = function(e) message("Error en ", nombre, ": ", e), 
           finally = message("OK ", nombre))
}

# scraping_prensa <- function(f = "cron_radiopaulina.r") {
#   message("iniciando ", f, " - ", lubridate::now())
#   #here(glue("scraping/{f}")) |> source() |> intentar(f) 
#   glue("scraping/{f}") |> source() |> intentar(f) 
#   #intentar(source(here("scraping/cron_tarapacaonline.r")), "tarapacaonline")
# }




revisar_resultados <- function(ruta) {
  walk(list.dirs(ruta, full.names = T, recursive = F), ~{
    Sys.sleep(0.05)
    #x_carpeta <- carpetas[1]
    x_carpeta <- .x
    
    revision <- x_carpeta |> 
      list.files(full.names = T) |> 
      file.info() |> 
      tibble::tibble() |> 
      filter(size > 10000)
    
    #mensaje
    if (max(revision$ctime) |> as.Date() == lubridate::today()) {
      message(x_carpeta |> stringr::str_extract("\\w+$"), " OK")
    } else {
      message("ERROR ", x_carpeta |> stringr::str_extract("\\w+$"))  
    }
  })
}
# revisar_resultados("resultados")


continuar_si_hay_enlaces <- function(enla, num = 3) {
  #continuar sólo si hay enlaces
  if (length(enla) <= num) {
    message("enlaces insuficientes, terminando")
    return(FALSE)
    #q()
  } else {
    message(glue("{length(enla)} enlaces obtenidos"))
    return(TRUE)
  } 
}

revisar_url <- function(url) {
  # url <- "https://www.eldinamo.cl/pais/2023/03/14/empresarios-agroindustriales-forman-consejo-empresarial-sectorial-para-contribuir-a-la-formacion-tp-y-la-empleabilidad-del-rubro/"
  estado <- url |> 
    httr::GET() |> 
    httr::status_code() |> 
    try()
  
  if (class(estado) != "integer") return(NULL)
  
  message(url, " (estado: ", estado, ")") |> try()
  
  if (estado != 200) {
    message(glue("error http en {url}"))
    return(NULL)
  } else {
    return(estado)
  }
}

# 
# #ejecutar procesos en el fondo
# source_bg <- function(file) {
#   log <- stringr::str_remove(file, "\\w+$") |> paste0("log")
#   system(glue::glue("Rscript '{file}' >> '{log}' 2>&1"), wait = F)
# }
# 
# 
# 
# #definir si es local o nacional
# 
# definir_escala <- function(x) {
#   prensa_nacional = c("adnradio", "agricultura", "cnnchile",
#                       "cooperativa_pais", "elciudadano", "elmostrador",
#                       "latercera_pais", "t13", "biobio_pais",
#                       "lahora",
#                       "emol_pais", "diariofinanciero_pais")
#   
#   if (x %in% prensa_nacional) {
#     y = "nacional" 
#   } else {
#     y = "local"
#   }
#   return(y)
# }
# 
# selenium_crear_driver <- function(puerto) {
#   remoteDriver(remoteServerAddr = "localhost", port = puerto, browserName = "firefox")
# }

# 



tramitaciones_limpiar_fechas <- function(data) {
  data |> 
    mutate(año = stringr::str_extract(fecha, "\\d{4}"),
           mes = stringr::str_extract(fecha, "\\w{3}") |> tolower(),
           dia = stringr::str_extract(fecha, "\\d{2}")) |> 
    mutate(mes = recode(mes,
                        "ene" = "1",
                        "jan" = "1",
                        "feb" = "2",
                        "mar" = "3",
                        "abr" = "4",
                        "apr" = "4",
                        "may" = "5",
                        "jun" = "6",
                        "jul" = "7",
                        "ago" = "8",
                        "aug" = "8",
                        "sep" = "9",
                        "oct" = "10",
                        "nov" = "11",
                        "dic" = "12",
                        "dec" = "12")) |>
    mutate(fecha_o = fecha,
           fecha = lubridate::ymd(paste(año, mes, dia)))
}



limpiar_texto <- function(x) {
  x |> 
    tolower() |> 
    str_replace_all("[[:punct:]]", " ") |> 
    str_replace_all("[0-9]", " ") |> 
    str_replace_all("\\||\\<|\\>|@|-|—|\\{|\\}|\\[|\\]|\\=|“", " ") |> 
    str_trim() |> 
    str_squish()
}


limpiar_texto_poquito <- function(x) {
  x |> 
    str_replace_all("dfp:|\\n|\\r", " ") |> 
    str_trim() |> 
    str_squish()
}


revisar_scraping <- function(data) {
  try({
  message(paste("listo", deparse(substitute(data)), "-", lubridate::now()))
  if ("tbl" %in% class(data)) message(paste(nrow(data), "noticias obtenidas"))
  })
}


# palabras ----
stopwords <- readr::read_lines("~/R/lira_popular/datos/stopwords_español.txt") #tidytext::get_stopwords("es") |> pull(word)

# palabras irrelevantes ----
palabras_irrelevantes = c("chile", "publicar", "comunidad", "personas",
                          "año", "años", "añosa", "añosen",
                          "país", "persona", "comunicación", "señor",
                          "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre",
                          "leer", "artículo", "completo", "articular", "completar", # cooperatva ("leer articulo completo")
                          "relacionadasdetalle", "null", # emol
                          "publicación", # elmostrador
                          "mercer", #cnnchile y otros
                          "detallar" # meganoticias
)

palabras_eliminar = c("right", "left", "top", "align", "gnews", "px", "twitter", "com", "pic", "font", "height", "width",
                      "pred", "fs", "us", "april", "flickr", "datawrapper", "data", "fried", "ftx", "medium", "exante", "server", "family", "loc", "lon", "mag", "prof", "lat", "gpt", "banner", "donación",
                      "style", 
                      "aton", "emolmlt", "font", "border", "margin", #emol
                      "rectangle", "container", "img", "display", "sans", "end", "weight", "content", "rem", "flex", "border", "bottom", "margin", "padding", "center", 
                      "radius", "text", "síguenos", "solid", "items", "dadada", "droidsans", "justify", "serif", "push", "function", "cmd", "div", "googletag", "ad",
                      "protected", "email",
                      palabras_irrelevantes)


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
