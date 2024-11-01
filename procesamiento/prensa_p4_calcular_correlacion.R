# a partir del conteo de palabras por noticia, calcular correlación entre términos, indicando qué términos aparecen cercanos a otros
# https://uc-r.github.io/word_relationships#corr

library(dplyr)
library(purrr)
library(widyr)
library(beepr)

# cargar ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")


# opciones ----
corr_min <- 0.08

prensa_palabras_conteo_2 <- prensa_palabras_conteo |> 
  ungroup() |> 
  add_count(palabra, name = "palabra_freq_total")


# correlación ----
# calcular correlación entre palabras, por noticia

tictoc::tic()

correlacion <- prensa_palabras_conteo_2 |> 
  filter(palabra_freq_total >= nrow(prensa_palabras_conteo_2)*0.000005) |> #solo palabras que aparezcan más de n veces a través del corpus
  select(-palabra_freq_total) |> 
  pairwise_cor(palabra, id)  |> 
  filter(!is.na(correlation)) |> 
  filter(correlation > corr_min)

tictoc::toc()
# 60 segundos


## guardar ----
arrow::write_parquet(correlacion, "datos/prensa_correlacion.parquet")
file.copy(from = "datos/prensa_correlacion.parquet", 
          "apps/prensa_chile/prensa_correlacion.parquet", overwrite = TRUE)
# correlacion <- arrow::read_parquet("datos/prensa_correlacion.parquet")

rm(correlacion)


# —----


# correlación por fuente ----
prensa_palabras_conteo_fuente <- prensa_palabras_conteo_2 |> 
  filter(palabra_freq_total >= nrow(prensa_palabras_conteo_2)*0.000001) |> #solo palabras que aparezcan más de n veces a través del corpus
  select(-palabra_freq_total) |> 
  left_join(datos_prensa |> 
              select(id, fuente),
            by = "id") |> 
  group_by(fuente) |> 
  group_split()

# remove(datos_prensa)

message("correlación por fuentes: total de palabras a correlacionar: ", map(prensa_palabras_conteo_fuente, nrow) |> unlist() |> sum())

tictoc::tic()

# filtrar palabras menores
prensa_palabras_conteo_fuente_2 <- map(prensa_palabras_conteo_fuente, \(datos_fuente) {
  datos_fuente |> 
  group_by(palabra) |> 
  mutate(palabra_freq_total = sum(n)) |> 
  ungroup() |> 
  filter(palabra_freq_total > quantile(palabra_freq_total, 0.2)) # n de palabra por sobre el x% de la distribución
})

message("correlación por fuentes: total de palabras  a correlacionar luego de filtrar: ", map(prensa_palabras_conteo_fuente_2, nrow) |> unlist() |> sum())

# correlación por fuentes
correlacion_fuente <- map(prensa_palabras_conteo_fuente_2, \(datos_fuente) {
  .fuente <- datos_fuente$fuente[1]
  message("correlación ", .fuente)

  corr_fuente <- datos_fuente |> 
    select(palabra, id) |> 
    pairwise_cor(palabra, id) |> 
    filter(!is.na(correlation)) |> 
    mutate(fuente = .fuente) |> 
    filter(correlation > corr_min*1)
  
  return(corr_fuente)
}) |> 
  list_rbind()

tictoc::toc()

message("correlación por fuentes: total de palabras únicas: ", unique(correlacion_fuente$item1) |> length())

## guardar ----
arrow::write_parquet(correlacion_fuente, "datos/prensa_correlacion_fuente.parquet")
file.copy(from = "datos/prensa_correlacion_fuente.parquet", 
          "apps/prensa_chile/prensa_correlacion_fuente.parquet", overwrite = TRUE)

rm(correlacion,
   correlacion_fuente,
   datos_prensa,
   prensa_palabras,
   prensa_palabras_conteo_fuente, 
   prensa_palabras_conteo_fuente_2,
   prensa_palabras_conteo,
   prensa_palabras_conteo_2)
invisible(gc())