library(tidyverse)
sentimiento <- arrow::read_parquet("prensa_sentimiento.parquet")

sentimiento_calcular <- function(data) {
  data |> 
    summarize(n = n(),
              n_positivo = sum(sentimiento == 1, na.rm = T),
              n_neutral = sum(sentimiento == 0, na.rm = T),
              n_negativo = sum(sentimiento == -1, na.rm = T),
              sentimiento = mean(sentimiento, na.rm = T),
              fecha = min(fecha), .groups = "drop") |> 
    mutate(p_negativas = n_negativo/n)
}



# conteo por semanas ----
sentimiento_semana <- sentimiento |>
  group_by(semana, fecha) |>
  # conteo por semanas
  sentimiento_calcular()

sentimiento_semana |>
  ggplot() +
  aes(fecha, sentimiento) +
  geom_col() +
  geom_text(aes(label = scales::percent(p_negativas, 1)), vjust = 1) +
  scale_y_continuous(limits = c(-1, 1))



## por fuente ----

# if fuente != "Todas"

sentimiento_semana_fuente <- sentimiento |>
  group_by(fuente, semana, fecha) |>
  # conteo por semanas
  sentimiento_calcular()

lista_fuentes <- unique(sentimiento$fuente)

sentimiento_semana_fuente |>
  filter(fuente %in% lista_fuentes[1:3]) |> 
  ggplot() +
  aes(fecha, sentimiento) +
  geom_col() +
  facet_wrap(~fuente) +
  scale_y_continuous(limits = c(-1, 1))


## por clasificación ----

# if tema != "Todos"

sentimiento_semana_clasificacion <- sentimiento |>
  filter(!is.na(clasificacion)) |>
  group_by(clasificacion, semana, fecha) |>
  # conteo por semanas
  sentimiento_calcular()

# sentimiento_semana_fuente_clasificacion |>
#   group_by(clasificacion, semana, fecha) |>
#   summarize(sentimiento = mean(sentimiento)) |>
sentimiento_semana_clasificacion |>
  ggplot() +
  aes(fecha, sentimiento) +
  geom_col() +
  facet_wrap(~clasificacion) +
  scale_y_continuous(limits = c(-1, 1))


## por fuente y clasificación ----
# if tema != "Todos" & fuente != "Todos"

sentimiento_semana_fuente_clasificacion <- sentimiento |>
  filter(!is.na(clasificacion)) |>
  # filter(clasificacion %in% c("economía", "policial", "política")) |>
  group_by(fuente, clasificacion, semana, fecha) |>
  # conteo por semanas
  sentimiento_calcular()

sentimiento_semana_fuente_clasificacion |>
  # filter(clasificacion == "política") |>
  filter(fuente %in% lista_fuentes[1:3]) |>
  # filter(clasificacion == "policial") |>
  ggplot() +
  aes(fecha, sentimiento) +
  geom_col() +
  # geom_point() +
  # geom_segment(aes(xend = fecha, yend = 0)) +
  facet_wrap(vars(fuente)) +
  scale_y_continuous(limits = c(-1, 1))

