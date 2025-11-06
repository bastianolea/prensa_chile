library(tidyverse)
source("funciones.R")

library(furrr)
library(future)
plan(multisession, workers = 7)

# locale -a
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# datos_prensa


# preparar datos ----
prensa_palabras_conteo_2 <- prensa_palabras_conteo |> 
  # agregar metadatos
  left_join(datos_prensa |> 
              select(id, fuente, fecha),
            by = "id") 

rm(prensa_palabras_conteo,
   datos_prensa)


# fechas ----
prensa_palabras_conteo_3 <- prensa_palabras_conteo_2 |> 
  # rango de fechas 
  # filter(fecha >= today() - months(4)) |> 
  filter(fecha >= today() %m-% months(4)) |> 
  filter(fecha <= fecha_limite()) |> # fecha límite, para no incluir días de la semana siguiente
  mutate(semana = week(fecha),
         fecha = floor_date(fecha, unit = "week", week_start = 1),
         # para primera semana del año
         fecha = case_when(semana == 1 & month(fecha) != 1 ~ ymd(paste(year(fecha)+1, "01 01")), #fecha + days(1),
                           .default = fecha) 
         ) |> 
  group_split(fuente)

# prensa_palabras_conteo_3

# conteo por semana y fuente ----
palabras_semana_fuente <- future_map(prensa_palabras_conteo_3, \(parte) {
  # conteo por semanas y fuente multiprocesador
  parte |> 
    group_by(fuente, semana, fecha, palabra) |> 
    # conteo por semanas
    summarize(n = sum(n),
              # semana = first(semana),
              # fecha = first(fecha)
              .groups = "drop")
}) |> 
  list_rbind() |> 
  recodificar_fuentes() |> 
  mutate(fecha_texto = redactar_fecha(fecha),
         fecha_texto = fct_reorder(fecha_texto, fecha))

# palabras_semana_fuente |> 
#   filter(fecha >= "2024-11-01") |> 
#   distinct(fecha, semana, fecha_texto)

# top palabras por semana
palabras_semana_fuente_2 <- palabras_semana_fuente |> 
  group_by(fuente, semana) |> 
  slice_max(n, n = 2000) |> # palabras por fuente
  # calcular palabras por fuente
  group_by(fuente) |> 
  mutate(n_total_fuente = sum(n)) |> 
  ungroup()

# palabras_semana_fuente_2 |> 
#   filter(palabra == "migración")

# guardar ----
arrow::write_parquet(palabras_semana_fuente_2,"datos/app/palabras_semana_fuente.parquet")
# palabras_semana_fuente <- arrow::read_parquet("apps/prensa_chile/palabras_semana_fuente.parquet")

plan(multisession)
rm(prensa_palabras_conteo_2, prensa_palabras_conteo_3,
   palabras_semana_fuente, palabras_semana_fuente_2)
invisible(gc())
