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

# opciones gráfico
.dodge = 3
.angulo = 40
.espaciado_y = 0.08
.espaciado_x = 0.05

# palabras irrelevantes ----
palabras_irrelevantes = c("chile", "publicar", 
                          "año", "añosa", "añosen",
                          "país", "persona", "comunicación"
)

# preparar datos ----
prensa_conteo_2 <- prensa_palabras_conteo |> 
  # palabras que se repitan n veces dentro de su noticia
  filter(n > 2) |> 
  # excluir palabras
  filter(!palabra %in% palabras_irrelevantes) |> 
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
  group_by(semana, palabra) |> 
  # conteo por semanas
  summarize(n = sum(n),
            fecha = min(fecha)) |> 
  ungroup() |> 
  # mínimo
  filter(n > 10)

# guardar
arrow::write_parquet(palabras_semana, "apps/prensa_semanal/palabras_semana.parquet")

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
data |> 
  # ordenar palabras por frecuencia
  ungroup() |> 
  mutate(palabra = fct_reorder(palabra, freq_total_palabra)) |> 
  # etiquetas hacia la izquierda
  mutate(inv = ifelse(semana == min(semana) | n < mean(n)*0.8, TRUE, FALSE)) |> 
  # por porcentaje o por frecuencia
  # group_by(semana) |>
  # mutate(n = n/sum(n)) |>
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
