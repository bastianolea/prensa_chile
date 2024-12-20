library(ggplot2)

sentimiento <- read_parquet("datos/prensa_llm_sentimiento.parquet")
clasificacion <- read_parquet("datos/prensa_llm_clasificar.parquet")
resumen <- read_parquet("datos/prensa_llm_resumen.parquet")

datos <- bind_rows(sentimiento |> mutate(tipo = "sentimiento", orden = row_number()),
                   clasificacion |> mutate(tipo = "clasificacion", orden = row_number()),
                   resumen |> mutate(tipo = "resumen", orden = row_number()))


# por palabras y tiempo de ejecución
datos |> 
  ggplot() +
  aes(tiempo, n_palabras) +
  geom_jitter(size = 0.4, alpha = .3, width = 1) +
  facet_wrap(~tipo) +
  coord_cartesian(xlim = c(0, 15),
                  ylim = c(0, 1000), 
                  expand = F) +
  theme_linedraw() +
  theme(panel.spacing.x = unit(4, "mm")) +
  labs(subtitle = "por palabras y tiempo de ejecución")

# por palabras y orden de ejecución
datos |> 
  ggplot() +
  aes(orden, n_palabras) +
  geom_point(size = 0.4, alpha = .3) +
  geom_smooth(method = "lm", se = F, color = "purple2") +
  theme_linedraw() +
  facet_wrap(~tipo, scales = "free_x") +
  coord_cartesian(expand = F) +
  theme(panel.spacing.x = unit(4, "mm")) +
  labs(subtitle = "por palabras y orden de ejecución")

# por tiempo de ejecución y orden de ejecución
datos |> 
  ggplot() +
  aes(orden, tiempo) +
  geom_jitter(size = 0.4, alpha = .3, height = .7) +
  geom_smooth(method = "lm", se = F, color = "purple2") +
  theme_linedraw() +
  facet_wrap(~tipo, scales = "free_x") +
  coord_cartesian(ylim = c(0, 15),
                  expand = F) +
  theme(panel.spacing.x = unit(4, "mm")) +
  labs(subtitle = "por tiempo de ejecución y orden de ejecución")
