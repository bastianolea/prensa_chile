library(tidyverse)

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
prensa_conteo_2 <- prensa_palabras_conteo |> 
  # palabras que se repitan n veces dentro de su noticia
  # filter(n > 2) |> 
  # excluir palabras
  # filter(!palabra %in% palabras_irrelevantes) |> 
  # agregar metadatos
  left_join(datos_prensa |> 
              select(id, fuente, fecha, titulo),
            by = "id")

# fechas ----
prensa_conteo_3 <- prensa_conteo_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(6)) |> 
  mutate(semana = week(fecha))

# conteo ----
palabras_semana <- prensa_conteo_3 |> 
  group_by(semana, fuente, palabra) |> 
  # conteo por semanas
  summarize(n = sum(n),
            fecha = min(fecha)) |> 
  ungroup() |> 
  # mínimo
  filter(n > 2)

# palabras irrelevantes ----
palabras_irrelevantes = c("chile", "publicar", 
                          "año", "años", "añosa", "añosen",
                          "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre",
                          "leer", "articular", "completar", # cooperatva ("leer articulo completo")
                          "relacionadasdetalle", "null", # emol
                          "detallar", # meganoticias
                          "comunidad", # puede ser relacionado a comentarios
                          "país", "persona", "comunicación"
)

# top palabras por semana ----
.n_fuentes = 17
.palabras_por_fuente = 50
.palabras_por_semana = 15
.semanas = 29:32

palabras_semana |>
  # excluir palabras
  filter(!palabra %in% palabras_irrelevantes) |>
  # filter(semana >= max(semana) - 6) |> 
  # top 10 palabras 
  group_by(fuente, semana) |> 
  slice_max(n, n = .palabras_por_fuente) |> # palabras por fuente
  group_by(fuente) |> 
  mutate(total_fuente = sum(n)) |> 
  # ranking de fuentes con mayor cantidad de palabras
  group_by(semana) |> 
  mutate(rank = dense_rank(desc(total_fuente))) |> 
  filter(semana %in% .semanas) |> 
  filter(rank <= .n_fuentes) |> # cantidad de fuentes
  # # agrupar fuentes chicas
  # mutate(fuente = fct_lump(fuente, w = total_fuente, n = .n_fuentes, other_level = "otros_medios")) |>
  # group_by(semana, fuente, palabra) |>
  # summarize(n = sum(n)) |>
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
  print() |> 
  # gráfico
  ggplot(aes(x = n, y = palabra, fill = fuente)) +
  geom_col(linewidth = .2, color = "white") +
  guides(fill = guide_legend(position = "bottom", nrow = 3)) +
  tidytext::scale_y_reordered() +
  scale_x_continuous(expand = expansion(c(0, .1))) +
  facet_wrap(~semana, 
             scales = "free", nrow = 1)  +
  theme_minimal() +
  labs(title = "Palabras más mencionadas en medios, semanalmente", 
       subtitle = "(el número corresponde al número de la semana en el año)",
       y = "palabras más mencionadas por semana", x = "frecuencia de mención, por fuente",
       fill = "fuentes\nescritas",
       caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile")


# palabra específica por fuente ----
.palabra = "macaya"

palabras_semana |>
  # excluir palabras
  filter(palabra == .palabra) |> 
  filter(semana >= max(semana) - 6) |> 
  # top 10 palabras 
  group_by(fuente, semana) |> 
  slice_max(n, n = .palabras_por_fuente) |> # palabras por fuente
  # # ranking de fuentes con mayor cantidad de palabras
  group_by(fuente) |>
  mutate(total_fuente = sum(n)) |>
  ungroup() |>
  mutate(rank = dense_rank(desc(total_fuente))) |>
  filter(rank <= .n_fuentes) |> # cantidad de fuentes
  # # agrupar fuentes chicas
  # mutate(fuente = fct_lump(fuente, w = total_fuente,
  #                          n = 8, other_level = "otros_medios")) |>
  # group_by(fuente, palabra) |>
  # summarize(n = sum(n)) |>
  # # maximo palabras por semana
  # group_by(semana, palabra) |> 
  # mutate(n_semana = sum(n)) |>
  # group_by(semana) |> 
  # mutate(rank2 = dense_rank(desc(n_semana))) |> 
  # filter(rank2 <= .palabras_por_semana) |> # cantidad de palabras por semana
  # # ordenar palabras
  # group_by(semana, palabra) |>
  # mutate(n_palabra_semana = sum(n)) |>
  # group_by(semana) |>
  # mutate(palabra = tidytext::reorder_within(palabra, n_palabra_semana, semana)) |>
  # print() |>
  # gráfico
  ggplot(aes(x = n, y = fuente, fill = fuente)) +
  geom_col(position = position_dodge()) +
  guides(fill = guide_none()) +
  # tidytext::scale_y_reordered() +
  scale_x_continuous(expand = expansion(c(0, 0.01)),
                     # limits = c(NA, 500)
                     ) +
  theme_minimal() +
  labs(title = paste("Cantidad de noticias que mencionan:", .palabra), 
       subtitle = "Sólo incluyendo mayores medios de comunicación escritos",
       y = "fuentes", x = "frecuencia de mención",
       caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile")
