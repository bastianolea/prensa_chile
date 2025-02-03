library(dplyr)
library(arrow)
library(ggplot2)
library(tidyr)
library(lubridate)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")

# cargar bases unificadas de resultados llm
sentimiento <- read_parquet("datos/prensa_llm_sentimiento.parquet")
clasificacion <- read_parquet("datos/prensa_llm_clasificar.parquet")
resumen <- read_parquet("datos/prensa_llm_resumen.parquet")


# estado ca

# unir resultados llm
datos <- bind_rows(sentimiento |> mutate(tipo = "sentimiento", orden = row_number()),
                   clasificacion |> mutate(tipo = "clasificacion", orden = row_number()),
                   resumen |> mutate(tipo = "resumen", orden = row_number()))




# —----



datos_2 <- datos |>
  arrange(id) |> 
  select(id, sentimiento, clasificacion, resumen) |> 
  add_count(id)

datos_3 <- datos_2 |> 
  # filter(n > 1) |> 
  group_by(id) |> 
  fill(sentimiento, clasificacion, resumen, .direction = "updown") |> 
  distinct()



datos_prensa_2 <- datos_prensa |> 
  filter(id %in% datos$id) |> 
  select(-fecha2, -fecha_original, -cuerpo_limpio)


datos_prensa_3 <- datos_prensa_2 |> 
  left_join(datos_3, by = "id")


reciente <- datos_prensa_3 |> 
  # filter(fecha > (today() - weeks(5)))
  filter(fecha > (today() - months(4)))


reciente |> 
  arrange(desc(fecha)) |> 
  # filter(!is.na(sentimiento)) |> 
  print(n=300)

reciente |> count(sentimiento)

reciente |> count(clasificacion)

reciente |> 
  filter(clasificacion == "policial")

reciente |> 
  filter(clasificacion == "farándula") |> 
  select(titulo, resumen) |> 
  print(n=20)

reciente |> 
  filter(clasificacion == "deporte") |> 
  select(titulo, resumen) |> 
  print(n=20)

# sentimiento de policiales
reciente |> 
  filter(clasificacion == "policial") |> 
  count(sentimiento)

# sentimiento de política
reciente |> 
  filter(clasificacion == "política") |> 
  count(sentimiento)

# resumen de noticias negativas
reciente |> 
  filter(sentimiento == "negativo") |> 
  filter(!is.na(resumen)) |> 
  select(titulo, resumen)


reciente |> 
  mutate(mes = floor_date(fecha, unit = "month"),
         semana = floor_date(fecha, unit = "week")) |> 
  count(semana)

# sentimiento, por fuente
reciente |> 
  mutate(mes = floor_date(fecha, unit = "month")) |> 
  count(mes, sentimiento, fuente) |> 
  ggplot() +
  aes(mes, n) +
  geom_line(aes(color = fuente, linetype = sentimiento)) +
  facet_wrap(~sentimiento)


# sentimiento de política, por fuente
reciente |> 
  filter(clasificacion == "política") |> 
  filter(!is.na(sentimiento)) |> 
  count(sentimiento, fuente)

# clasificación, por fuente
reciente |> 
  filter(!is.na(clasificacion)) |> 
  filter(!clasificacion %in% c("farándula", "deporte")) |> 
  mutate(mes = floor_date(fecha, unit = "month")) |> 
  count(mes, clasificacion, fuente) |> 
  ggplot() +
  aes(mes, n) +
  geom_line(aes(color = fuente)) +
  facet_wrap(~clasificacion)


# gráfico sentimiento
reciente |> 
  filter(fecha >= today() - weeks(4)) |> 
  filter(clasificacion == "policial") |> 
  mutate(semana = floor_date(fecha, unit = "day", week_start = 1)) |> 
  group_by(semana) |> 
  count(sentimiento) |> 
  filter(!is.na(sentimiento),
         sentimiento != "neutral") |> 
  group_by(semana) |> 
  mutate(p = n/sum(n)) |> 
  mutate(p = if_else(sentimiento == "negativo", -p, p)) |> 
  ggplot() +
  aes(semana, p) +
  geom_col(aes(fill = sentimiento))
