library(tidyverse)
source("funciones.R")

# cargar datos 
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
sentimiento <- arrow::read_parquet("datos/prensa_llm_sentimiento.parquet")
clasificacion <- arrow::read_parquet("datos/prensa_llm_clasificar.parquet")


# preparar datos ----
datos_prensa_b <- datos_prensa |> 
  # rango de fechas 
  filter(fecha >= today() - months(5)) |> 
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

# rm(datos_prensa)

# fechas ----
prensa_sentimiento_3 <- prensa_sentimiento_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(4)) |> 
  filter(fecha <= fecha_limite) |> # fecha límite, para no incluir días de la semana siguiente
  mutate(fecha = floor_date(fecha, unit = "week", week_start = 1),
         semana = week(fecha)) |> 
  filter(!is.na(sentimiento)) |> 
  distinct(id, .keep_all = TRUE)



# estado por fuentes ----

# estado de cálculos
procesamiento <- datos_prensa_b |> 
  mutate(calculado = ifelse(id %in% sentimiento_b$id & id %in% clasificacion$id, "calculado", "calcular")) |> 
  count(fuente, calculado) |> 
  mutate(total = sum(n), .by = fuente) |> 
  mutate(p = n/sum(n), .by = fuente)

# fuentes con un 60% de sus noticias procesadas
procesadas <- procesamiento |> 
  filter(calculado == "calculado") |> 
  filter(p > 0.5) |> 
  arrange(desc(p))

# procesadas$fuente

procesar <- procesamiento |> 
  filter(calculado == "calculado") |> 
  filter(p <= 0.6) |> 
  arrange(desc(p))

# procesar$fuente


# guardar ----
arrow::write_parquet(prensa_sentimiento_3, 
                     "apps/prensa_chile/prensa_sentimiento.parquet")

readr::write_rds(procesamiento,
                 "apps/prensa_chile/prensa_sentimiento_status.rds")