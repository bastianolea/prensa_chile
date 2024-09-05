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

# palabras irrelevantes ----
palabras_irrelevantes = c("chile", "publicar", "año", "añosa", 
                          "país", "persona", "año", "comunicación")

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

# conteo ----
palabras_top_semana <- prensa_conteo_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(6)) |> 
  mutate(semana = week(fecha)) |> 
  group_by(semana, palabra) |> 
  # conteo por semanas
  summarize(n = sum(n),
            fecha = min(fecha)) |> 
  # dejar solo top 10 palabras por semana
  group_by(semana) |> 
  slice_max(n, n = 10)

# detalles ----
palabras_top_semana_2 <- palabras_top_semana |> 
  # ordenar palabras por frecuencia
  group_by(palabra) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(palabra = fct_reorder(palabra, total)) |> 
  # eliminar palabras chicas en contexto
  filter(n > mean(n)*0.7) |>
  # dejar solo top palabras total (maximo de palabras visibles)
  mutate(palabra_lump = fct_lump(palabra, w = total, 
                                 n = 30, other_level = "otras")) |> 
  filter(palabra_lump != "otras") |> 
  # límite de fecha
  filter(fecha >= floor_date(today() - weeks(9), "month")) |>
  # sacar palabras que salen una sola vez
  add_count(palabra, name = "palabra_n") |> 
  filter(palabra_n > 1) |> 
  # sacar semanas donde hayan pocos términos (indicio de error)
  group_by(semana) |> 
  mutate(semana_n = n()) |> 
  filter(semana_n > 2)


# gráfico ----
palabras_top_semana_2 |> 
  # group_by(semana) |>
  # mutate(n = n/sum(n)) |>
  ggplot(aes(fecha, n)) +
  geom_step(aes(color = palabra),
            linewidth = .8,
            position = position_dodge(3),
            direction = "mid", show.legend = F) +
  geom_point(aes(group = palabra),
             size = 3, color = "white", 
             position = position_dodge(3)) +
  geom_point(aes(color = palabra),
             size = 2, position = position_dodge(3)) +
  shadowtext::geom_shadowtext(aes(label = paste("  ", palabra), 
                                  color = palabra),
                              bg.colour = "white", bg.r = 0.3, #color = "black",
            angle = 30, size = 2.4, hjust = 0, vjust = 0.3, 
            # nudge_x = 0.06, nudge_y = max(palabras_top_semana$n)*0.02, 
            position = position_dodge(3),
            check_overlap = T, show.legend = F) +
  scale_y_continuous(expand = expansion(c(0.05, 0.1))) +
  scale_x_date(date_breaks = "weeks", date_labels = "%d de %B", 
               expand = expansion(c(0.02, 0.07))) +
  guides(color = guide_none()) +
  theme_classic() +
  theme(panel.grid.major.x = element_line(),
        axis.text.x = element_text(hjust = 1, angle = 30)) +
  labs(y = "frecuencia de palabra por semana",
       x = "semanas")

