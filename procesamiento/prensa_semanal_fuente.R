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


# fechas ----
prensa_palabras_conteo_3 <- prensa_palabras_conteo_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(6)) |> 
  mutate(semana = week(fecha),
         fecha = floor_date(fecha, unit = "week", week_start = 1)) |> 
  group_split(fuente)


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
  mutate(fecha = redactar_fecha(fecha),
         fecha = fct_reorder(fecha, semana))


palabras_semana_fuente_2 <- palabras_semana_fuente |> 
  # top palabras por semana
  group_by(fuente, semana) |> 
  slice_max(n, n = 60) |> # palabras por fuente
  # calcular palabras por fuente
  group_by(fuente) |> 
  mutate(n_total_fuente = sum(n)) |> 
  ungroup()


# guardar ----
arrow::write_parquet(palabras_semana_fuente_2,"apps/prensa_chile/palabras_semana_fuente.parquet")
# palabras_semana_fuente <- arrow::read_parquet("apps/prensa_chile/palabras_semana_fuente.parquet")

plan(multisession)
rm(prensa_palabras_conteo,
   prensa_palabras_conteo_2, prensa_palabras_conteo_3,
   palabras_semana_fuente, palabras_semana_fuente_2)
invisible(gc())