library(tidyverse)
source("funciones.R")

# locale -a
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
# if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")
# if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

sentimiento <- arrow::read_parquet("datos/prensa_llm_sentimiento.parquet")
clasificacion <- arrow::read_parquet("datos/prensa_llm_clasificar.parquet")
# datos_prensa


# preparar datos ----
datos_prensa_b <- datos_prensa |> 
  # rango de fechas 
  filter(fecha >= today() - months(4)) |> 
  select(id, fecha, fuente)

sentimiento_b <- sentimiento |> 
  select(id, sentimiento) |> 
  mutate(sentimiento = case_match(sentimiento, 
                                  "positivo" ~ 1,
                                  "neutral" ~ 0,
                                  "negativo" ~ -1))
clasificacion_b <- clasificacion |> 
  select(id, clasificacion)

# unir
prensa_sentimiento_2 <- datos_prensa_b |> 
  left_join(sentimiento_b, by = "id") |> 
  left_join(clasificacion_b, by = "id") |> 
  filter(!is.na(sentimiento))

# prensa_sentimiento_2

rm(datos_prensa)

# fechas ----
prensa_sentimiento_3 <- prensa_sentimiento_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(4)) |> 
  filter(fecha <= fecha_limite) |> # fecha límite, para no incluir días de la semana siguiente
  mutate(fecha = floor_date(fecha, unit = "week", week_start = 1),
         semana = week(fecha)) |> 
  filter(!is.na(sentimiento)) |> 
  distinct(id, .keep_all = TRUE)




# guardar ----
arrow::write_parquet(prensa_sentimiento_3, 
                     "apps/prensa_chile/prensa_sentimiento.parquet")

rm(datos_prensa_b,
   palabras_semana,
   palabras_semana_2,
   prensa_sentimiento_2)
invisible(gc())