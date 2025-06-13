# PRENSA: PASO 2
# en este script se cargan los datos de prensa y se tokenizan, es decir, se transforma hacia una estructura donde cada fila contiene una sola palabra
# luego se eliminan stopwords y palabras inútiles, y se guarda el resultado
# input: prensa_datos.feather
# output: prensa_palabras.feather
# tiempo aproximado: 7 minutos (ahora se demora 1 minuto!)

library(dplyr)
library(purrr)
library(furrr)
library(tidytext)
source("funciones.R")

plan(multisession, workers = 8)
# options(future.globals.maxSize = 2048*1024^2)

tictoc::tic()

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")

# dividir dataframe en partes para poder procesar en paralelo
datos_prensa_split <- datos_prensa |>
  filter(año >= 2018) |>
  select(id, cuerpo_limpio) |> 
  mutate(grupos = (row_number()-1) %/% (n()/16)) |> # n grupos de igual cantidad de filas
  group_split(grupos)


# tokenización de texto por partes, multiprocesador
prensa_palabras <- future_map(datos_prensa_split, \(parte) {
  parte |> 
    select(-grupos) |> 
    unnest_tokens(output = palabra, 
                  input = cuerpo_limpio,
                  token = "words", stopwords = stopwords,
                  drop = TRUE) |> 
    # eliminar stopwords
    # filter(!palabra %in% stopwords) |> 
    filter(palabra != "") |> 
    # eliminar palabras irrelevantes
    filter(!palabra %in% palabras_eliminar)
})

# conteo total de palabras
message(paste("total de palabras:", map(prensa_palabras, nrow) |> unlist() |> sum() |> format(big.mark = ".", decimal.mark = ",")))

# mínimo de repeticiones y largo de de palabras
prensa_palabras_filt <- future_map(prensa_palabras, \(parte) {
    parte |> 
    add_count(palabra, name = "palabra_freq_total") |> 
    # excluir palabra si se repite menos de x veces
      filter(palabra_freq_total > 10) |> 
    # excluir por longitud
      filter(nchar(palabra) >= 3, # palabra más corta
             nchar(palabra) < 23) # palabra más larga del español según rae
  })
  
message(paste("total de palabras post filtro:", map(prensa_palabras_filt, nrow) |> unlist() |> sum() |> format(big.mark = ".", decimal.mark = ",")))

prensa_palabras <- prensa_palabras_filt |> 
  list_rbind()

## guardar ----
arrow::write_parquet(prensa_palabras, "datos/prensa_palabras.parquet")
prensa_palabras <- arrow::read_parquet("datos/prensa_palabras.parquet")

# guardar cantidad de palabras
n_palabras <- prensa_palabras |> 
  nrow() |> 
  signif(digits = 3)

n_palabras |> write("datos/prensa_n_palabras.txt")
n_palabras |> write("apps/prensa_chile/prensa_n_palabras.txt")

plan(multisession)
rm(datos_prensa_split,
   prensa_palabras_filt)
invisible(gc())

# beepr::beep()
tictoc::toc()
