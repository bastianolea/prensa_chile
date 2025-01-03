# PRENSA: PASO 1
# carga todos los resultados individuales en una lista por fuente, donde cada elemento de la lista es un dataframe con una noticia por fila,
# luego revisa cada elemento de la lista y separa los con demasiadas filas en partes más pequeñas, 
# y finalmente procesa esas partes paralelamente, donde se interpretan las fechas y se limpia el texto, y los resultados se guardan al disco.
# finalmente, se cargan las partes preprocesadas y se escriben como una sola base.
# input: archivos rds individuales de la carpeta resultados
# output: prensa_datos.feather

tictoc::tic()

library(fs)
library(future)
library(purrr)
library(furrr)
library(dplyr) |> suppressPackageStartupMessages()
library(readr)
library(stringr)
library(lubridate) |> suppressPackageStartupMessages()
source("funciones.R")

plan(multisession, workers = 7)


# datos ----
carpetas_modulos <- dir_ls("resultados")
archivos_modulos <- map(carpetas_modulos, ~dir_ls(.x, regexp = ".rds"))

# archivos_modulos <- list(archivos_modulos[[11]], 
#                          archivos_modulos[[25]])
# archivos_modulos <- archivos_modulos[[3]]

# carga ----
# cargar resultados de módulos 
modulos_cargados <- future_map(archivos_modulos, \(archivo_modulo) {
  # archivo_modulo <- archivos_modulos[[3]]
  # archivo_modulo <- archivos_modulos[["resultados/emol"]]
  # archivo_modulo <- "resultados/agricultura/agricultura_cron_6913_2024-12-30.rds"
  
  # cargar todos los archivos de la carpeta del módulo
  resultados_modulo <- map(unlist(archivo_modulo), read_rds)
  
  # por cada pieza de datos del módulo, convertir a dataframe si es necesario
  modulo_cargado <- map(resultados_modulo, \(resultado_modulo) {
    # resultado_modulo <- resultados_modulo[[3]]
    
    # revisar si datos son válidos
    if ("list" %in% class(resultado_modulo)) {
      if (length(resultado_modulo) == 0) return(NULL)
    }
    if ("tbl" %in% class(resultado_modulo)) {
      if (nrow(resultado_modulo) == 0) return(NULL)
    }
    
    # convertir los que son lista a dataframes
    if ("list" %in% class(resultado_modulo)) {
      # pegar partes de la lista, si fueran más de 1 por columna
      resultado_modulo3 <- map(1:length(resultado_modulo), ~lapply(resultado_modulo[[.x]], paste, collapse ="\n"))
      
      resultado_modulo4 <- map(resultado_modulo3, bind_rows)
      resultado_modulo <- resultado_modulo4 |> list_rbind() |> mutate(across(everything(), as.character))
    }
    
    if (!"tbl" %in% class(resultado_modulo)) return(NULL) #solo retornar si funciona
    
    resultado_modulo_pre <- resultado_modulo |> mutate(across(everything(), as.character))
    
    return(resultado_modulo_pre)
  })
  
  datos <- modulo_cargado |> 
    list_rbind()
  
  return(datos)
})


# map(modulos_cargados, nrow)

# separar fuentes con muchos datos en partes mas pequeñas para luego procesarlas
modulos_cargados <- map(modulos_cargados, \(modulo) {
  # modulo <- modulos_cargados[[2]]
  # map(modulos_cargados, nrow)

  filas <- nrow(modulo)
  grupos <- filas %/% 20000 # un grupo cada 10000 observaciones

  if (filas >= 20000) {
    modulo <- modulo |>
      mutate(grupos = (row_number()-1) %/% (n()/grupos)) |>
      group_by(grupos) |>
      group_split()
  } else {
    modulo <- modulo |>
      mutate(grupos = 0)
  }

  return(modulo)
}) |> list_flatten()



# limpieza ----
# future_walk(modulos_cargados, \(modulo) {
modulos_limpios <- future_map(modulos_cargados, \(modulo) {
  # modulo <- modulos_cargados[[2]]
  # modulo <- modulos_cargados[[23]]
  # modulo <- modulos_cargados[["resultados/agricultura/agricultura_cron_6913_2024-12-30.rds"]]
  # modulo <- modulos_cargados[["resultados/agricultura/agricultura_cron_2024-09-24.rds"]]
  
  if (length(modulo) <= 4) return(NULL)
  if (nrow(modulo) == 0) return(NULL)
  
  datos_2 <- modulo |>
    # noticias únicas
    distinct(url, .keep_all = TRUE) |> 
    # limpiar fuentes
    mutate(fuente = str_remove(fuente, "_pais")) |> 
    # noticias con título
    filter(nchar(titulo) > 20) |> 
    # noticias con cuerpo
    filter(nchar(cuerpo) > 100)
  
  if (nrow(datos_2) == 0) return(NULL)
  
  # que tengan todas las columnas necesarias
  if (!"bajada" %in% names(datos_2)) datos_2 <- datos_2 |> mutate(bajada = NA_character_)
  if (!"fecha_scraping" %in% names(datos_2)) datos_2 <- datos_2 |> mutate(fecha_scraping = NA_Date_)
  # if (!"url" %in% names(datos_2)) 
  
  ## eliminar textos ----
  datos_4 <- datos_2 |> 
    mutate(titulo = limpiar_texto_poquito(titulo),
           bajada = limpiar_texto_poquito(bajada),
           cuerpo = limpiar_texto_poquito(cuerpo)) |> 
    mutate(cuerpo_limpio = limpiar_texto(cuerpo),
           bajada_limpia = limpiar_texto(bajada),
           titulo_limpio = limpiar_texto(titulo))
  
  # finalizar
  datos_prensa <- datos_4 |> 
    select(fuente, fecha, titulo, bajada, cuerpo, cuerpo_limpio, fecha_scraping, url) |> 
    # crear ids únicos por noticia
    rowwise() |>
    mutate(id = rlang::hash(url)) |> 
    ungroup()

  return(datos_prensa)
})



# fechas ----
modulos_limpios_fechas <- future_map(modulos_limpios, \(modulo) {
  
  # modulo <- modulos_limpios[["resultados/agricultura/agricultura_cron_6913_2024-12-30.rds"]]
  # names(modulos_limpios)
  # modulo <- modulos_limpios[[28]]
  
  if (is.null(modulo)) return(NULL)
  
  # instrucciones especiales para fuentes específicas
  resultado_1 <- modulo |> 
    # extracción de fecha por fuente
    mutate(fecha2 = as_date(fecha)) |> 
    relocate(fecha2, .after = fecha) |> 
    # mutate(fecha2 = if_else(fuente == "cnnchile", str_extract(url, "\\d{8}") |> as_date(), fecha2)) |> 
    # mutate(fecha2 = if_else(fuente == "adnradio", str_extract(url, "(?<=(\\/))(\\d{4}\\/\\d{2}\\/\\d{2})(?=\\/)") |> ymd(), fecha2)) |> 
    # mutate(fecha2 = if_else(fuente == "24horas", dmy(fecha), fecha2)) |> 
    # mutate(fecha2 = if_else(fuente == "exante", ymd(fecha), fecha2)) |> 
    # mutate(fecha2 = if_else(fuente == "meganoticias" & is.na(fecha2), str_extract(url, "(?<=(-))(\\d{2}-\\d+-\\d{4})(?=\\.html)") |> dmy(), fecha2)) |> 
    mutate(fecha2 = case_when(fuente == "agricultura" & fecha == "2024-05-31" ~ str_extract(url, "\\d{8}") |> as_date(), # corregir fecha incorrecta scrapeada
                              fuente == "cnnchile" ~ str_extract(url, "\\d{8}") |> as_date(),
                              fuente == "adnradio" ~ str_extract(url, "(?<=(\\/))(\\d{4}\\/\\d{2}\\/\\d{2})(?=\\/)") |> ymd(),
                              fuente == "24horas" ~ dmy(fecha),
                              fuente == "exante" ~ ymd(fecha),
                              fuente == "meganoticias" & is.na(fecha2) ~ str_extract(url, "(?<=(-))(\\d{2}-\\d+-\\d{4})(?=\\.html)") |> dmy(), 
                              .default = fecha2))
  
  # extracción general
  resultado_2 <- resultado_1 |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(fecha, "\\d{4}.\\d{2}.\\d{2}") |> ymd(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(fecha, "\\d{2}.\\d{2}.\\d{4}") |> dmy(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(fecha, "\\d{2}.\\d{1}.\\d{4}") |> dmy(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(url, "\\d{4}-\\d{2}-\\d{2}") |> ymd(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(url, "\\d{2}-\\d{2}-\\d{4}") |> dmy(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(url, "\\d{4}\\/\\d{2}\\/\\d{2}") |> ymd(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(url, "\\d{2}\\/\\d{2}\\/\\d{4}") |> dmy(), fecha2))
  
  resultado_3 <- resultado_2 |> 
    # excepciones por fuente
    mutate(fecha2 = if_else(fuente == "elciudadano" & is.na(fecha2), paste("2024", str_extract(url, "(?<=(\\/))\\d{2}\\/\\d{2}(?=\\/)")) |> ymd(), fecha2)) |>
    # correcciones de fechas improbables
    mutate(fecha2 = if_else(fecha2 < "1990-01-01", as_date(fecha_scraping), fecha2)) |> 
    # ordenar
    mutate(fecha_original = fecha, 
           fecha = fecha2,
           año = year(fecha)) |> 
    select(-fecha2, -fecha_original) |> 
    mutate(fecha_scraping = as_date(fecha_scraping))
  
  resultado_4 <- resultado_3 |> 
    arrange(desc(fecha))
  
  return(resultado_4)
})

# resultado |> 
#   filter(fecha < "2000-01-01")

# plan(multisession)



# unir ----

# descomentar esto para usar paso intermedio que guarda resultados por piezas en el disco duro, para evitar que se caiga el proceso por falta de memoria
# datos_prensa <- map(dir_ls("datos/preprocesados_datos"), read_rds) |> 
#   list_rbind() |> 
#   arrange(desc(fecha)) |> 
#   distinct(url, .keep_all = TRUE)

# segunda forma de hacerlo, directamente desde memoria
datos_prensa <- modulos_limpios_fechas |> 
  list_rbind() |>
  distinct(url, .keep_all = TRUE)



# revisar ----
# noticias por fuente
# datos_prensa |>
#   count(fuente) |>
#   arrange(desc(n))
# 
# # noticias por año
# datos_prensa |>
#   mutate(año = year(fecha)) |>
#   count(año) |>
#   arrange(desc(año))
# 
# # noticias sin fecha por fuente
# datos_prensa |>
#   filter(is.na(fecha)) |>
#   count(fuente)
# 
# # noticias por mes
# datos_prensa |>
#   mutate(fecha = floor_date(fecha, "month")) |>
#   count(fecha) |>
#   arrange(desc(fecha))



# guardar ----
arrow::write_parquet(datos_prensa, "datos/prensa_datos.parquet")



# guardar cantidad de noticias
options(scipen=9999)
n_noticias <- datos_prensa |> 
  nrow() |> 
  signif(digits = 3) 

n_noticias |> write("datos/prensa_n_noticias.txt")
n_noticias |> write("apps/prensa_chile/prensa_n_noticias.txt")

# list("n_fuentes" = length(unique(datos_prensa$fuente)),
#      "max_fecha" = max(datos_prensa$fecha, na.rm = T),
#      "n_noticias" = length(unique(datos_prensa$id))
# )
# 
# datos_prensa |>
#   filter(fecha < "2000-01-01")

plan(multisession)
remove(modulos_cargados, 
       modulos_limpios, 
       modulos_limpios_fechas
       # datos_prensa
       )
invisible(gc())

# beepr::beep()
tictoc::toc()
