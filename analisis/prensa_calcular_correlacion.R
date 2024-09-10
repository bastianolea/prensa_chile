# a partir del conteo de palabras por noticia, calcular correlación entre términos, indicando qué términos aparecen cercanos a otros
# https://uc-r.github.io/word_relationships#corr

library(dplyr)
library(purrr)
library(widyr)
library(beepr)

# cargar ----
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")
if (!exists("datos_prensa")) datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")

# correlación ----
# calcular correlación entre palabras, por noticia

tictoc::tic()

correlacion <- prensa_palabras_conteo |> 
  filter(palabra_freq_total >= 100) |> #solo palabras que aparezcan más de n veces a través del corpus
  pairwise_cor(palabra, id)  |> 
  filter(!is.na(correlation)); beep()

tictoc::toc()
# 60 segundos

## guardar ----
arrow::write_parquet(correlacion, "datos/prensa_correlacion.parquet")


# correlación por fuente ----
prensa_palabras_conteo_fuente <- prensa_palabras_conteo |> 
  filter(palabra_freq_total >= 50) |> #solo palabras que aparezcan más de n veces a través del corpus
  left_join(datos_prensa |> 
              select(id, fuente),
            by = "id")

# remove(datos_prensa)
fuentes <- datos_prensa$fuente |> unique() |> set_names()

tictoc::tic()

correlacion_fuente <- map(fuentes, \(.fuente) {
  message(.fuente)
  
  datos_fuente <- prensa_palabras_conteo_fuente |> 
    filter(fuente == .fuente) |> 
    group_by(palabra) |> 
    mutate(palabra_freq_total = sum(n)) |> 
    ungroup() |> 
    filter(palabra_freq_total > quantile(palabra_freq_total, 0.2)) # n de palabra por sobre el x% de la distribución
  
  corr_fuente <- datos_fuente |> 
    select(palabra, id) |> 
    pairwise_cor(palabra, id) |> 
    filter(!is.na(correlation)) |> 
    mutate(fuente = .fuente)
  
  return(corr_fuente)
}) |> 
  list_rbind()

tictoc::toc()

## guardar ----
arrow::write_parquet(correlacion_fuente, "datos/prensa_correlacion_fuente.parquet")


# pruebas ----

# correlacion |>
#   filter(item1 == "delincuencia") |>
#   arrange(desc(correlation)) |>
#   head(5)
# 
# correlacion_fuente |>
#   filter(item1 == "delincuencia") |>
#   group_by(fuente) |> 
#   slice_max(correlation, n = 5) |> 
#   arrange(fuente, desc(correlation)) |>
#   print(n=Inf)
# 
# correlacion |>
#   filter(item1 == "hermosilla") |>
#   arrange(desc(correlation)) |>
#   head(5)
# 
# correlacion_fuente |>
#   filter(item1 == "hermosilla") |>
#   group_by(fuente) |> 
#   slice_max(correlation, n = 5) |> 
#   arrange(fuente, desc(correlation)) |>
#   print(n=Inf)

# correlacion |> 
#   filter(item1 == "robo") |> 
#   arrange(desc(correlation)) |> 
#   head(10)
# 
# correlacion |> 
#   filter(item1 == "arma") |> 
#   arrange(desc(correlation)) |> 
#   head(10)
# 
# correlacion |>
#   filter(item1 == "corrupción") |>
#   arrange(desc(correlation)) |>
#   head(10)
# 
# correlacion |> 
#   filter(item1 == "fraude") |> 
#   arrange(desc(correlation)) |> 
#   head(10)
# 
# correlacion |> 
#   filter(item1 == "jadue") |> 
#   arrange(desc(correlation)) |> 
#   head(10)
# 
# correlacion |> 
#   filter(item1 == "kast") |> 
#   arrange(desc(correlation)) |> 
#   head(10)
# 
# correlacion |> 
#   filter(correlation > .9) |> 
#   arrange(desc(correlation)) |> 
#   head(20)
# 
# # graficar ----
# 
# correlacion |> 
#   filter(!is.na(correlation),
#          correlation <= .95, correlation >= .9) |> 
#   graph_from_data_frame() |> 
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
#   geom_node_point(color = "lightblue", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_void()