library(tidyverse)
source("funciones.R")

# locale -a
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
# if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# datos_prensa


# preparar datos ----
prensa_conteo_2 <- prensa_palabras_conteo |> 
  # agregar metadatos
  left_join(datos_prensa |> 
              select(id, fecha),
            by = "id")


# fechas ----
prensa_conteo_3 <- prensa_conteo_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(6)) |> 
  mutate(fecha = floor_date(fecha, unit = "week", week_start = 1),
         semana = week(fecha))


# conteo por semanas ----
palabras_semana <- prensa_conteo_3 |> 
  group_by(semana, fecha, palabra) |> 
  # conteo por semanas
  summarize(n = sum(n),
            fecha = min(fecha)) |> 
  # mínimo
  filter(n > 2)


# guardar ----
arrow::write_parquet(palabras_semana, 
                     "apps/prensa_semanal/palabras_semana.parquet")
rm(prensa_conteo_2, prensa_conteo_3)
invisible(gc())