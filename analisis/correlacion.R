library(dplyr)
library(arrow)
library(ggplot2)

source("funciones.R")

correlacion <- arrow::read_parquet("datos/prensa_correlacion.parquet")
correlacion_fuente <- arrow::read_parquet("datos/prensa_correlacion_fuente.parquet")

# correlación de un concepto en todo el corpus
correlacion |>
  filter(item1 == "delincuencia") |>
  arrange(desc(correlation)) |>
  head(10)

# correlación de un concepto, por fuente
correlacion_fuente |>
  filter(item1 == "delincuencia") |>
  group_by(fuente) |>
  slice_max(correlation, n = 5) |>
  arrange(fuente, desc(correlation)) |>
  print(n=Inf)

# palabras únicas correlacionadas con un concepto
correlacion |>
  filter(item1 == "delincuencia") |>
  slice_max(correlation, n = 30) |>
  print(n=Inf)

# palabras únicas de la correlación por fuentes
correlacion_fuente |>
  filter(item1 == "delincuencia") |>
  group_by(fuente) |>
  slice_max(correlation, n = 20) |>
  ungroup() |> 
  count(item2) |> 
  slice_max(n, n = 30) |> 
  print(n=Inf)



correlacion |>
  filter(item1 == "hermosilla") |>
  arrange(desc(correlation)) |>
  head(5)

correlacion_fuente |>
  filter(item1 == "hermosilla") |>
  group_by(fuente) |>
  slice_max(correlation, n = 5) |>
  arrange(fuente, desc(correlation)) |>
  print(n=Inf)

correlacion |>
  filter(item1 == "robo") |>
  arrange(desc(correlation)) |>
  head(10)

correlacion |>
  filter(item1 == "arma") |>
  arrange(desc(correlation)) |>
  head(10)

correlacion |>
  filter(item1 == "corrupción") |>
  arrange(desc(correlation)) |>
  head(10)

correlacion |>
  filter(item1 == "fraude") |>
  arrange(desc(correlation)) |>
  head(10)

correlacion |>
  filter(item1 == "jadue") |>
  arrange(desc(correlation)) |>
  head(10)

correlacion |>
  filter(item1 == "kast") |>
  arrange(desc(correlation)) |>
  head(10)

correlacion |>
  filter(correlation > .9) |>
  arrange(desc(correlation)) |>
  head(20)


# graficar ----

# círculo de términos correlacionados ----
.palabra = "hermosilla"
.palabras_excluir = c("luis")
.n_terminos = 5
.tamaño_correlacion = (35) / (1 + (log(.n_terminos-4)*0.4))
# .tamaño_correlacion = 30

cor_filt <- correlacion |>
  rename(palabra1 = item1, palabra2 = item2, correlacion = correlation) |> 
  filter(palabra1 == .palabra)

cor_filt_max <- cor_filt |> 
  filter(!palabra2 %in% .palabras_excluir) |>
  slice_max(correlacion, n = .n_terminos)
  # filter(correlacion >= .3)

cor_filt_max |> 
  mutate(palabra2 = forcats::fct_reorder(palabra2, correlacion, .desc = TRUE)) |> 
  ggplot(aes(x = palabra2, y = 1, 
             color = correlacion)) +
  geom_point(size = .tamaño_correlacion, alpha = .2) +
  geom_point(aes(size = correlacion), alpha = 1) +
  shadowtext::geom_shadowtext(aes(label = palabra2),
                              bg.colour = "white", bg.r = 0.3, color = "black") +
  scale_size_continuous(range = c(.tamaño_correlacion*0.1, .tamaño_correlacion*0.85),
                        limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, 2)) +
  guides(color = guide_none(),
         size = guide_none()) +
  theme(axis.text = element_blank()) +
  labs(x = "términos más correlacionados con x", y = NULL)





# círculo de términos por fuente ----
.palabra = "hermosilla"
.palabras_excluir = c("luis")
.n_terminos = 6
.n_fuentes = 4
.tamaño_correlacion_fuente = (35) / (1 + (log(.n_terminos-4)*0.1)) - ((.n_fuentes-1)*3)
# .tamaño_correlacion_fuente = 15

cor_filt_fuente <- correlacion_fuente |>
  rename(palabra1 = item1, palabra2 = item2, correlacion = correlation) |> 
  filter(palabra1 == .palabra)

cor_filt_max_fuente <- cor_filt_fuente |> 
  # palabras excluidas
  filter(!palabra2 %in% .palabras_excluir) |>
  # maximo de términos por fuente
  group_by(fuente) |> 
  slice_max(correlacion, n = .n_terminos) |> 
  # ranking de fuentes
  group_by(fuente) |> 
  mutate(cor_total = sum(correlacion)) |> 
  ungroup() |> 
  mutate(rank_fuente = dense_rank(cor_total)) |> 
  filter(rank_fuente <= .n_fuentes)

cor_filt_max_fuente |> 
  group_by(fuente) |> 
  mutate(orden = dense_rank(desc(correlacion))) |> 
  recodificar_fuentes() |> 
  mutate(palabra2 = forcats::fct_reorder(palabra2, correlacion, .desc = TRUE)) |> 
  ggplot(aes(x = orden, y = 1, 
             color = correlacion)) +
  geom_point(size = .tamaño_correlacion_fuente, alpha = .2) +
  geom_point(aes(size = correlacion), alpha = 1) +
  shadowtext::geom_shadowtext(aes(label = palabra2),
                              bg.colour = "white", bg.r = 0.3, color = "black", size = 3) +
  scale_size_continuous(range = c(.tamaño_correlacion_fuente*0.1, .tamaño_correlacion_fuente*0.85),
                        limits = c(0, NA)) +
  guides(color = guide_none(), size = guide_none()) +
  theme(strip.background = element_blank(), strip.text = element_text(vjust = 1),
        strip.text.x = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(), panel.background = element_blank()) +
  coord_cartesian(clip = "off") +
  # facet_wrap(~fuente, scales = "free", ncol = 1) +
  facet_grid(rows = vars(fuente), cols = vars(orden), drop = T,
             scales = "free", switch = "y",
             ) +
  labs(x = "términos más correlacionados con x", y = NULL)



# red ----
correlacion |>
  filter(!is.na(correlation),
         correlation <= .95, correlation >= .9) |>
  graph_from_data_frame() |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


