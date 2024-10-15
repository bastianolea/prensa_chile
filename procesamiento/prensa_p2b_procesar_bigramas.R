library(dplyr)
library(purrr)
library(furrr)
library(tidytext)
source("funciones.R")

plan(multisession, workers = 7)
# options(future.globals.maxSize = 2048*1024^2)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")


# dividir dataframe en partes para poder procesar en paralelo
datos_prensa_split <- datos_prensa |>
  filter(año >= 2018) |>
  select(id, cuerpo_limpio) |> 
  mutate(grupos = (row_number()-1) %/% (n()/30)) |> # n grupos de igual cantidad de filas
  group_split(grupos)


# bigramas ----
tictoc::tic()
prensa_bigramas <- future_map(datos_prensa_split, \(parte) {
  
  bigramas <- parte |> 
    select(-grupos) |> 
    unnest_tokens(output = bigrama, 
                  input = cuerpo_limpio,
                  token = "ngrams", 
                  n = 2L, stopwords = stopwords,
                  drop = TRUE)
  return(bigramas)
})
tictoc::toc()


# palabras en bigramas ----
tictoc::tic()
prensa_bigramas_palabras <- future_map(prensa_bigramas, \(parte) {
  
  palabras <- parte |> 
    unnest_tokens(output = palabra,
                  input = bigrama,
                  token = "words", drop = FALSE) |> 
    # eliminar palabras irrelevantes
    filter(!palabra %in% palabras_eliminar)
  return(palabras)
}) |> 
  list_rbind()
tictoc::toc()

rm(datos_prensa_split, prensa_bigramas); invisible(gc())


## guardar ----
arrow::write_parquet(prensa_bigramas_palabras, "datos/prensa_bigramas.parquet")

rm(prensa_bigramas_palabras); invisible(gc())


# 
# # conteo de bigramas con la palabra caso ----

if (!exists("prensa_bigramas_palabras")) prensa_bigramas_palabras <- arrow::read_parquet("datos/prensa_bigramas.parquet")
# conteo_casos <- future_map(prensa_bigramas_palabras, \(parte) {
#   
#   parte_orden <- parte |> 
#     # orden de ocurrencia
#     group_by(id, bigrama) |>
#     mutate(orden = row_number()) |> 
#     ungroup()
#   
#   conteo <- parte_orden |> 
#     filter(orden == 1,
#            palabra == "caso") |> 
#     count(bigrama)
#   return(conteo)
# }) |> 
#   list_rbind()
# 
# 
# 
# ## recontar ----
# conteo_casos |> 
#   arrange(desc(n)) |> 
#   print(n=30)
# 
# conteo_casos |> 
#   group_by(bigrama) |> 
#   summarize(n = sum(n)) |> 
#   arrange(desc(n)) |> 
#   print(n=30)
