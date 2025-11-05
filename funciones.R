
# palabras ----
stopwords <- readr::read_lines("datos/stopwords_es.txt") #tidytext::get_stopwords("es") |> pull(word)

# palabras irrelevantes ----
palabras_irrelevantes = c("chile", "publicar", "comunidad", "personas",
                          "región",
                          "año", "años", "añosa", "añosen",
                          "país", "persona", "comunicación", "señor",
                          "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre",
                          "youtube", "aaron",
                          "leer", "artículo", "completo", "articular", "completar", # cooperatva ("leer articulo completo")
                          "publicación", # elmostrador
                          "mercer", #cnnchile y otros
                          "detallar", # meganoticias
                          "cabe", "pese", "abc", "abcdin"
)

palabras_basura = c("aaa", "aba", "aaisa", "aap", "aas",
                    "RelacionadasDetalle", "sd", "hd",
                    "TradingView", "Widget BEGIN", "Widget END",
                    "jpg", "like", "new", "child", "https", "length", "https", "http", "domcontentloaded", "flexdirection", "firstdiv", "pointer", "addeventlistener", "queryselector", "marginbottom", "containers", "lastdiv", "foreach", "innerwidth",
                    "right", "left", "top", "align", "gnews", "px", "twitter", "com", "pic", "font", "height", "width",
                    "pred", "fs", "us", "april", "flickr", "datawrapper", "data", "fried", "ftx", "medium", "exante", "server", "family", "loc", "lon", "mag", "prof", "lat", "gpt", "banner", "donación",
                    "style", "relacionadasdetalle", "null",
                    "containerstyleflexdirection", "firstdivstylemarginbottom", "first", "div", "formstyleflexdirection", "formulario", "style", "flex", "last", "cursor", 
                    "document", "deventlistenerdomloedevent", "query", "selector", "all", "responsive", "firstchild",
                    "tablaennotici", "notatablaemol", "public",
                    "demr", "detalleicon", "important", "notatablaemol", "tablaennotici", "tdnthchildn", "arial",
                    "updatelayout", "function", "formulario", "transmision", "containerstyleflexdirection", "firstdivstylemarginbottom", 
                    "fontsize", "backgroundcolor", "display", "padding", "width", "lineheight", "ifslength", "kok", "queryselectorall", "queryselectorinputtyp",
                    "elements", "submit", "www", "ficon",
                    "alignitems", "margin", "left", "sans", "serif", "height", "margin", "top",
                    "aton", "emolmlt", "font", "border", "margin", #emol
                    "rectangle", "container", "img", "display", "sans", "end", "weight", "content", "rem", "flex", "border", "bottom", "margin", "padding", "center", 
                    "radius", "text", "síguenos", "solid", "items", "dadada", "droidsans", "justify", "serif", "push", "function", "cmd", "div", "googletag", "ad",
                    "protected", "email",
                    "aach", "aafp", "aafdf") |> unique()

palabras_eliminar = c(palabras_irrelevantes,
                      palabras_basura)



# —----

ejecutar <- function(script = "modulos/cron_elsiglo.r", 
                     esperar = TRUE) {
  # browser()
  ruta_log = script |> 
    stringr::str_replace("\\.r$", ".log") |> 
    stringr::str_replace("modulos/", "logs/")
  
  invisible(suppressWarnings(file.remove(ruta_log)))
  
  comando <- paste0("/usr/local/bin/Rscript ", script, " >> ", ruta_log, " 2>&1")
  # comando <- paste0("nohup Rscript ", script, " >> ", ruta_log, " 2>&1")
  
  Sys.sleep(0.1)
  system(comando, wait = esperar)
}


scraping_prensa <- function(script = "cron_radiopaulina.r",
                            ruta = "scraping/fuentes/", 
                            ejecucion = "asinc") {
  message(glue::glue("iniciando {script} - {format(now(), '%d/%m/%y %H:%M:%S')}"))
  
  if (ejecucion == "asinc") {
  ### en subproceso (sesión) interactivo (RStudio Background Job, compatible con cualquier sistema operativo)
  rstudioapi::jobRunScript(paste0(ruta, script), workingDir = getwd())
    
  } else if (ejecucion == "secuencial") {
    source(paste0(ruta, script))  
  }
  
  ###
  # # en subproceso (callr) no interactivo
  # proc <- callr::r_bg(\(script) source(script), 
  #                     supervise = TRUE,
  #                     stderr = stringr::str_replace(script, "\\.r$", "\\.log"),
  #                     stdout = stringr::str_replace(script, "\\.r$", "\\.log")
  #                     )
  
  ### 
  # ejecutar script en el fondo no interactivo (sólo en macOS, probablemente en Linux)
  # ejecutar(paste0("/Users/baolea/R/prensa/", script))
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




limpiar_texto_poquito <- function(x) {
  x |> 
    str_replace_all("dfp:|\\n|\\r", " ") |> 
    str_trim() |> 
    str_squish()
}


limpiar_texto <- function(x) {
  
  palabras_basura <- paste("\\b", palabras_basura, "\\b", sep = "") |> 
    paste(collapse = "|")
  
  x |> 
    # eliminar código
    str_replace_all("\\{.*\\}", " ") |>
    # eliminar hashtags
    str_replace_all("\\#.*\\b", " ") |>
    # eliminar código ciper
    str_replace_all("var divElement.*\\);", " ") |> 
    str_remove_all("\\{\\{.*\\}\\}") |> 
    # es mejor convertir a espacios que eliminar, porque así se separan de la anterior/siguiente palabra
    str_replace_all("[[:punct:]]", " ") |>
    str_replace_all("[[:digit:]]", " ") |>
    str_replace_all("\\||\\<|\\>|@|-|—|\\{|\\}|\\[|\\]|\\=|“", " ") |>
    textclean::strip() |> 
    # tolower() |> 
    str_replace_all(palabras_basura, " ") |> 
    str_squish() |> 
    str_trim()
}



revisar_scraping <- function(data) {
  try({
    message(paste("listo", deparse(substitute(data)), "-", lubridate::now()))
    if ("tbl" %in% class(data)) message(paste(nrow(data), "noticias obtenidas"))
  })
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
                               "izquierdadiario" ~ "La Izquierda Diario",
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

mes_a_numero <- function(x) {
  recode(x, 
         "enero" = "1",
         "febrero" = "2",
         "marzo" = "3",
         "abril" = "4",
         "mayo" = "5",
         "junio" = "6",
         "julio" = "7",
         "agosto" = "8",
         "septiembre" = "9",
         "octubre" = "10",
         "noviembre" = "11",
         "diciembre" = "12")
}

# sólo funciona en macOS
notificacion <- function(titulo = "Título", texto = "texto") {
  # system("osascript -e 'display notification \"Datos de noticias descargados\" with title \"Scraping de prensa\"'")
  message(titulo, ": ", texto)
  
  system(
    paste0("osascript -e 'display notification \"", texto, "\" with title \"", titulo, "\"'")
  ) |> try()
}

# notificacion("Scraping de prensa", "Datos de noticias descargados")


rng <- function() {
  sample(1111:9999, 1)
}


ruta_resultado <- function(fuente = "latercera", hist = "", formato = "rds") {
  if (class(hist)[1] == "function") hist <- ""
  glue::glue("scraping/datos/{fuente}/{fuente}_cron_{rng()}_{lubridate::today()}{hist}.{formato}")
}


modulos_n <- function() {
  fs::dir_ls("scraping/datos") |> length()
}

sin_cambios_hoy <- function() {
  directorios <- fs::dir_info("scraping/datos") |> 
    arrange(desc(modification_time))
  
  # directorios sin cambios hoy
  sin_cambios <- directorios |> 
    filter(modification_time < lubridate::today()) |> 
    mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
    select(fuente, size, modification_time)
  
  return(sin_cambios)
}


estimar_tiempo <- function(muestra, estimacion = 4.9) {
  message(paste("tiempo aproximado de procesamiento:", round((muestra * estimacion)/60/60, 1), "horas")) 
}

detencion_manual <- function() {
  read.delim("otros/stop.txt", header = FALSE)[[1]] == "stop"
}
# if detencion_manual() return(NULL)

# mensaje_segundos <- function(...) {
#   message("(", seconds(round(..., 1)) |> as.numeric(), " segundos)")
# }

mensaje_segundos <- function(palabras, tiempo) {
  segundos = seconds(round(tiempo, 1)) |> as.numeric()
  palabras_segundos = round(palabras/segundos, 0)
  
  message("    ",  segundos, " segundos, ",
          palabras_segundos, " palabras/seg.")
}


fecha_limite <- function() {
  floor_date(today(), unit = "week", week_start = 7) # domingo que termina la semana, para prensa semanal
}



# para no scrapear si ya se obtuvo
revisar_scrapeado <- function(enlace) {
  
  # datos generados en p1
  scrapeados <- readRDS("otros/urls.rds")
  
  # comparar
  veredicto <- enlace %in% scrapeados
  
  if (veredicto) {
    message("url repetida")
  }
  
  return(veredicto)
}
