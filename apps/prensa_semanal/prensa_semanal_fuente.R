library(tidyverse)
source("funciones.R")

library(furrr)
library(future)
plan(multisession, workers = 7)

# locale -a
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

if (!exists("datos_prensa")) datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")
# if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

datos_prensa

# prensa_palabras_conteo <- prensa_palabras |> 
#   slice(1:2000000) |> 
#   group_by(id) |> 
#   count(palabra) |> 
#   filter(n > 3) |> 
#   arrange(id, desc(n))



# preparar datos ----
prensa_palabras_conteo_2 <- prensa_palabras_conteo |> 
  # palabras que se repitan n veces dentro de su noticia
  # filter(n > 2) |> 
  # excluir palabras
  # filter(!palabra %in% palabras_irrelevantes) |> 
  # agregar metadatos
  left_join(datos_prensa |> 
              select(id, fuente, fecha),
            by = "id") 
# recodificar_fuentes()

# fechas ----
prensa_palabras_conteo_3 <- prensa_palabras_conteo_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(6)) |> 
  mutate(semana = week(fecha),
         fecha = floor_date(fecha, unit = "week", week_start = 1)) |> 
  group_split(fuente)

# conteo ----
palabras_semana <- future_map(prensa_palabras_conteo_3, \(parte) {
  # conteo por semanas multiprocesador
  parte |> 
    group_by(fuente, semana, fecha, palabra) |> 
    # conteo por semanas
    summarize(n = sum(n),
              # semana = first(semana),
              # fecha = first(fecha)
              .groups = "drop")
}) |> 
  list_rbind()


# top palabras por semana ----
.n_fuentes = 10
.palabras_por_fuente = 50
.palabras_por_semana = 15
.semanas = 29:32

datos_1 <- palabras_semana |>
  # top 10 palabras 
  group_by(fuente, semana) |> 
  slice_max(n, n = .palabras_por_fuente) |> # palabras por fuente
  # calcular palabras por fuente
  group_by(fuente) |> 
  mutate(n_total_fuente = sum(n)) |> 
  filter(semana %in% .semanas) |> 
  ungroup() |> 
  # agrupar fuentes chicas
  recodificar_fuentes() |> 
  mutate(fuente = fct_reorder(fuente, n_total_fuente, .desc = TRUE)) |>
  mutate(fuente = fct_lump(fuente, w = n_total_fuente, n = .n_fuentes, ties.method = "first", 
                           other_level = "Otros")) |>
  group_by(fuente, semana, fecha, palabra) |>
  summarize(n = sum(n)) |> 
  
  # maximo palabras por semana
  group_by(semana, palabra) |> 
  mutate(n_semana = sum(n)) |>
  group_by(semana) |> 
  mutate(rank2 = dense_rank(desc(n_semana))) |> 
  filter(rank2 <= .palabras_por_semana) |> # cantidad de palabras por semana
  # ordenar palabras
  group_by(semana, palabra) |> 
  mutate(n_palabra_semana = sum(n)) |> 
  group_by(semana) |> 
  mutate(palabra = tidytext::reorder_within(palabra, n_palabra_semana, semana)) |> 
  ungroup()

datos_1 |> 
  mutate(fecha = redactar_fecha(fecha),
         fecha = fct_reorder(fecha, semana)) |> 
  # gráfico
  ggplot(aes(x = n, y = palabra, fill = fuente)) +
  geom_col(linewidth = .2, color = "white") +
  guides(fill = guide_legend(position = "bottom", nrow = 3)) +
  tidytext::scale_y_reordered() +
  scale_x_continuous(expand = expansion(c(0, .1))) +
  facet_wrap(~fecha, 
             scales = "free", nrow = 1)  +
  theme_minimal() +
  labs(title = "Palabras más mencionadas en medios, semanalmente", 
       subtitle = "(el número corresponde al número de la semana en el año)",
       y = "palabras más mencionadas por semana", x = "frecuencia de mención, por fuente",
       fill = "fuentes\nescritas",
       caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile")


# palabra específica por fuente ----
# .palabra = "macaya"
.palabra = "hermosilla"
.semanas = 33:36

datos_2 <- palabras_semana |>
  # excluir palabras
  filter(palabra == .palabra) |> 
  # filtrar semanas
  filter(semana %in% .semanas) |> 
  # # ranking de fuentes con mayor cantidad de palabras
  group_by(fuente) |>
  mutate(n_total_fuente = sum(n)) |>
  ungroup() |>
  # agrupar fuentes chicas
  recodificar_fuentes() |>
  mutate(fuente = fct_lump(fuente, w = n_total_fuente,
                           n = .n_fuentes, other_level = "Otros")) |>
  mutate(fuente = fct_reorder(fuente, n_total_fuente, .desc = T),
         fuente = fct_relevel(fuente, "Otros", after = 0)) |> 
  group_by(fuente, semana, fecha, palabra) |>
  summarize(n = sum(n)) |>
  # ordenar palabras
  group_by(semana, fuente) |> 
  mutate(n_palabra_fuente = sum(n)) |> 
  group_by(semana) |> 
  mutate(fuente2 = tidytext::reorder_within(fuente, n_palabra_fuente, semana)) |> 
  ungroup()

# gráfico
datos_2 |> 
  mutate(fecha = redactar_fecha(fecha),
         fecha = fct_reorder(fecha, semana)) |> 
  ggplot(aes(x = n, y = fuente2, fill = fuente)) +
  geom_col(position = position_dodge()) +
  guides(fill = guide_none()) +
  tidytext::scale_y_reordered() +
  scale_x_continuous(expand = expansion(c(0, 0.01))) +
  facet_wrap(~fecha, 
             scales = "free", nrow = 1)  +
  theme_minimal() +
  labs(title = paste("Cantidad de noticias que mencionan:", .palabra), 
       subtitle = "Sólo incluyendo mayores medios de comunicación escritos",
       y = "fuentes ordenadas por menciones", x = "frecuencia de mención, por semanas",
       caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile")
