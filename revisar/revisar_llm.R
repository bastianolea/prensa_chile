library(dplyr)
library(arrow)
library(ggplot2)

# cargar bases unificadas
sentimiento <- read_parquet("datos/prensa_llm_sentimiento.parquet")
clasificacion <- read_parquet("datos/prensa_llm_clasificar.parquet")
resumen <- read_parquet("datos/prensa_llm_resumen.parquet")

# # # cargar sólo ultimo procesamiento
sentimiento <- fs::dir_info("datos/prensa_llm/sentimiento/") |> slice_max(modification_time) |> pull(path) |> read_parquet()
clasificacion <- fs::dir_info("datos/prensa_llm/clasificar/") |> slice_max(modification_time) |> pull(path) |> read_parquet()
resumen <- fs::dir_info("datos/prensa_llm/resumen/") |> slice_max(modification_time) |> pull(path) |> read_parquet()

# unir
datos <- bind_rows(sentimiento |> mutate(tipo = "sentimiento", orden = row_number()),
                   # clasificacion |> mutate(tipo = "clasificacion", orden = row_number()),
                   # resumen |> mutate(tipo = "resumen", orden = row_number())
                   )

# tiempo promedio
datos |> 
  summarize(mean(tiempo), 
            n(), 
            .by = tipo)

# tema
theme_set(theme_linedraw() +
          theme(panel.background = element_rect(fill = "transparent", colour = NA),  
                plot.background = element_rect(fill = "transparent", colour = NA)))



# por palabras y tiempo de ejecución
datos |> 
  ggplot() +
  aes(tiempo, n_palabras) +
  geom_jitter(size = 0.4, alpha = .3, width = 1) +
  facet_wrap(~tipo) +
  coord_cartesian(xlim = c(0, 15),
                  ylim = c(0, 1000), 
                  expand = F) +
  labs(subtitle = "por palabras y tiempo de ejecución")

# ggsave(filename = "plot_a.png", width = 4, height = 2.5, scale = 1.8)

# por palabras y orden de ejecución
datos |> 
  ggplot() +
  aes(orden, n_palabras) +
  geom_point(size = 0.4, alpha = .3) +
  geom_smooth(method = "lm", se = F, color = "purple2") +
  facet_wrap(~tipo, scales = "free_x") +
  coord_cartesian(expand = F) +
  labs(subtitle = "por palabras y orden de ejecución")

# ggsave(filename = "plot_b.png", width = 4, height = 2.5, scale = 1.8)

# por tiempo de ejecución y orden de ejecución
datos |> 
  ggplot() +
  aes(orden, tiempo) +
  geom_jitter(size = 0.4, alpha = .3, height = .7) +
  geom_smooth(method = "lm", se = F, color = "purple2") +
  facet_wrap(~tipo, scales = "free_x") +
  coord_cartesian(ylim = c(0, 15),
                  expand = F) +
  labs(subtitle = "por tiempo de ejecución y orden de ejecución")

# ggsave(filename = "plot_c.png", width = 4, height = 2.5, scale = 1.8)


