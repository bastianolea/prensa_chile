library(tidyverse)
source("funciones.R")

# cargar datos 
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
sentimiento <- arrow::read_parquet("datos/prensa_llm_sentimiento.parquet")
# clasificacion <- arrow::read_parquet("datos/prensa_llm_clasificar.parquet")
clasificacion <- arrow::read_parquet("datos/prensa_stm_clasificar.parquet")


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
  filter(fecha <= fecha_limite()) |> # fecha límite, para no incluir días de la semana siguiente
  mutate(fecha = floor_date(fecha, unit = "week", week_start = 1),
         semana = week(fecha)) |> 
  filter(!is.na(sentimiento)) |> 
  distinct(id, .keep_all = TRUE)



# # estado por fuentes ----
# 
# estado de cálculos
procesamiento <- datos_prensa_b |>
  mutate(calculado = ifelse(id %in% sentimiento_b$id & id %in% clasificacion$id, "calculado", "calcular")) |>
  count(fuente, calculado) |>
  mutate(total = sum(n), .by = fuente) |>
  mutate(p = n/sum(n), .by = fuente)
# 
# # fuentes con un 60% de sus noticias procesadas
# procesadas <- procesamiento |> 
#   filter(calculado == "calculado") |> 
#   filter(p > 0.5) |> 
#   arrange(desc(p))
# 
# # procesadas$fuente
# 
# procesar <- procesamiento |> 
#   filter(calculado == "calculado") |> 
#   filter(p <= 0.6) |> 
#   arrange(desc(p))
# 
# # procesar$fuente


# guardar ----
arrow::write_parquet(prensa_sentimiento_3, 
                     "apps/prensa_chile/prensa_sentimiento.parquet")

readr::write_rds(procesamiento,
                 "apps/prensa_chile/prensa_sentimiento_status.rds")



# palabras por sentimiento/clasificacion ----
# para nubes de palabra por tópico y sentimiento

library(tidyverse)
source("funciones.R")

# locale -a
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
# if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

datos_prensa_2 <- datos_prensa |> 
  filter(fecha >= today() %m-% months(4))

prensa_conteo_2 <- prensa_palabras_conteo |> 
  # filtrar fechas
  filter(id %in% datos_prensa_2$id) |> 
  # agregar metadatos
  left_join(datos_prensa |> select(id, fecha),
            by = "id")

# fechas ----
prensa_conteo_3 <- prensa_conteo_2 |> 
  filter(fecha <= fecha_limite()) |> # fecha límite, para no incluir días de la semana siguiente
  mutate(fecha = floor_date(fecha, unit = "week", week_start = 1),
         semana = week(fecha))

# topicos ----
prensa_conteo_4 <- prensa_conteo_3 |> 
  left_join(sentimiento_b, by = "id") |> 
  left_join(clasificacion_b, by = "id") |> 
  filter(!is.na(clasificacion),
         !is.na(sentimiento))

# conteo por semanas ----
palabras_semana_topico <- prensa_conteo_4 |> 
  group_by(semana, fecha, sentimiento, clasificacion, palabra) |> 
  # conteo por semanas
  summarize(n = sum(n),
            fecha = min(fecha), .groups = "drop") |> 
  # mínimo
  # filter(n > 2) |> 
  arrange(fecha, desc(n))

# palabras_semana_topico

# # probar
# palabras_semana_topico |> 
#   filter(clasificacion == "política",
#          sentimiento == -1)

# reducir
palabras_semana_topico_2 <- palabras_semana_topico |> 
  group_by(semana, fecha, sentimiento, clasificacion) |> 
  slice_max(n, n = 400)

palabras_semana_topico_2

arrow::write_parquet(palabras_semana_topico_2, 
                     "apps/prensa_chile/palabras_semana_topico.parquet")
