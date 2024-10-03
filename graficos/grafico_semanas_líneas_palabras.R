library(tidyverse)
library(arrow)

palabras_semana <- arrow::read_parquet("apps/prensa_semanal/palabras_semana.parquet")


# ajustes ----
data <- palabras_semana |> 
  # límite de fecha
  filter(fecha >= floor_date(today() - weeks(9), "month")) |>
  # calcular frecuencia total de cada palabra
  group_by(palabra) |> 
  mutate(freq_total_palabra = sum(n)) |> 
  # calcular total de palabras por semana
  group_by(semana) |> 
  mutate(palabras_semana = sum(n)) |> 
  # sacar semanas chicas
  ungroup() |> 
  filter(palabras_semana > mean(palabras_semana)*.6) |> 
  # eliminar palabras chicas en contexto
  ungroup() |> 
  filter(freq_total_palabra > mean(freq_total_palabra)*2) |>
  # calcular porcentaje de palabra por semana
  group_by(semana) |> 
  mutate(p_palabra_semana = n/palabras_semana) |> 
  # dejar solo top palabras semana por porcentaje de semana
  filter(p_palabra_semana > 0.003) |> 
  ungroup() |> 
  # # dejar solo top palabras total (maximo de palabras visibles)
  # mutate(palabra_lump = fct_lump(palabra, w = freq_total_palabra, 
  #                                n = 25, other_level = "otras")) |> 
  # filter(palabra_lump != "otras") |> 
  # sacar palabras que salen una sola vez
  # add_count(palabra, name = "palabra_n") |>
  # filter(palabra_n > 1) |>
  # sacar semanas donde hayan pocos términos (indicio de error)
  group_by(semana) |> 
  mutate(semana_n = n()) |> 
  filter(semana_n > 2) |> 
  # dejar solo top 10 palabras por semana
  group_by(semana) |>
  slice_max(n, n = 10)
# dejar solo palabras con cierta frecuencia
# filter(n > 00)
# ungroup() |>
# filter(n > mean(n)*0.5)




# gráfico ----
# opciones gráfico
.dodge = 3
.angulo = 40
.espaciado_y = 0.08
.espaciado_x = 0.05

data |> 
  # ordenar palabras por frecuencia
  ungroup() |> 
  mutate(palabra = fct_reorder(palabra, freq_total_palabra)) |> 
  # etiquetas hacia la izquierda
  mutate(inv = ifelse(semana == min(semana) | n < mean(n)*0.8, TRUE, FALSE)) |> 
  # por porcentaje o por frecuencia
  group_by(semana) |>
  mutate(n = n/sum(n)) |>
  # mutate(n = p_palabra_semana) |>
  # gráfico
  ggplot(aes(fecha, n)) +
  geom_step(aes(color = palabra),
            linewidth = .9, alpha = .5,
            direction = "mid", 
            position = position_dodge(.dodge),
            show.legend = F) +
  geom_point(aes(group = palabra),
             size = 3, color = "white", 
             position = position_dodge(.dodge)) +
  geom_point(aes(color = palabra),
             size = 2, position = position_dodge(.dodge)) +
  # texto
  shadowtext::geom_shadowtext(
    aes(label = ifelse(inv, paste(palabra, "  "), paste("  ", palabra)),
        hjust = ifelse(inv, 1, 0),
        color = palabra),
    bg.colour = "white", bg.r = 0.3, angle = .angulo, size = 2.8, vjust = 0.3, 
    position = position_dodge(.dodge), check_overlap = T, show.legend = F) +
  # escalas
  scale_y_continuous(expand = expansion(c(.espaciado_y*0.7, .espaciado_y))) +
  scale_x_date(date_breaks = "weeks", date_labels = "%d de %B", expand = expansion(c(.espaciado_x, .espaciado_x))) +
  # paletteer::scale_color_paletteer_d("rcartocolor::Antique", dynamic = FALSE) +
  # scale_color_viridis_d(end = .8, option="magma") +
  guides(color = guide_none()) +
  theme_classic() +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major.x = element_line(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(hjust = 1, angle = .angulo),
        plot.caption = element_text(color = "grey80")) +
  labs(y = "frecuencia de palabras por semana",
       x = "semanas", 
       title = "Conceptos principales en prensa, por semana", 
       caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile")


# guardar
ggsave(paste0("graficos/noticias_semana_", today(), ".jpg"), 
       width = 6, height = 5, scale = 1.5)








# —----

palabras_semana <- arrow::read_parquet("apps/prensa_semanal/palabras_semana.parquet")

palabra_select = c("delincuencia", "corrupción", "hermosilla", "enel")

palabras_semana |> 
  filter(fecha > today() - weeks(12)) |> 
  filter(palabra %in% palabra_select) |> 
  group_by(palabra) |> 
  mutate(freq_total_palabra = sum(n)) |> 
  ungroup() |> 
  mutate(palabra = fct_reorder(palabra, freq_total_palabra)) |> 
  ggplot(aes(fecha, n, color = palabra)) +
  geom_line(linewidth = .9, alpha = .7, show.legend = F) +
  geom_point(size = 3, color = "white") +
  geom_point(size = 2) +
  scale_color_viridis_d(begin = .2, end = .7, option = "magma") +
  theme_classic() +
  guides(color = guide_legend(reverse = T, override.aes = list(size = 4))) +
  theme(legend.text = element_text(margin = margin(l = 2))) +
  labs(color = "Palabras", y = "Frecuencia de palabras", x = "Semanas")


