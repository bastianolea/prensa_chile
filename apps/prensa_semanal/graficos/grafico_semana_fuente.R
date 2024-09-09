
# top palabras por semana ----
.n_fuentes = 10
.palabras_por_fuente = 50
.palabras_por_semana = 15
.semanas = 29:32

datos_1 <- palabras_semana_fuente |>
  # top 10 palabras 
  group_by(fuente, semana) |> 
  slice_max(n, n = .palabras_por_fuente) |> # palabras por fuente
  # calcular palabras por fuente
  group_by(fuente) |> 
  mutate(n_total_fuente = sum(n)) |> 
  filter(semana %in% .semanas) |> 
  ungroup() |> 
  # agrupar fuentes chicas
  
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

datos_2 <- palabras_semana_fuente |>
  # excluir palabras
  filter(palabra == .palabra) |> 
  # filtrar semanas
  filter(semana %in% .semanas) |> 
  # # ranking de fuentes con mayor cantidad de palabras
  group_by(fuente) |>
  mutate(n_total_fuente = sum(n)) |>
  ungroup() |>
  # agrupar fuentes chicas
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
