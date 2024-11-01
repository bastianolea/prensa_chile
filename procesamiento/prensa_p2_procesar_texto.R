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

plan(multisession, workers = 7)
# options(future.globals.maxSize = 2048*1024^2)

tictoc::tic()

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")

# dividir dataframe en partes para poder procesar en paralelo
datos_prensa_split <- datos_prensa |>
  filter(año >= 2018) |>
  select(id, cuerpo_limpio) |> 
  mutate(grupos = (row_number()-1) %/% (n()/30)) |> # n grupos de igual cantidad de filas
  group_split(grupos)


# tokenización de texto por partes, multiprocesador
# future_walk(datos_prensa_split, \(parte) {
prensa_palabras <- future_map(datos_prensa_split, \(parte) {
  
  palabras <- parte |> 
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
  
  # descomentar esto para usar paso intermedio que guarda resultados por piezas en el disco duro, para evitar que se caiga el proceso por falta de memoria
  # # guardar resultados individuales
  # readr::write_rds(palabras, 
  #                  file = paste0("datos/preprocesados_palabras/palabras_", parte$grupos[1], ".rds"), compress = "gz")
  return(palabras)
}) |> 
  list_rbind()

# descomentar esto para usar paso intermedio que guarda resultados por piezas en el disco duro, para evitar que se caiga el proceso por falta de memoria
# cargar piezas individuales
# prensa_palabras <- map(fs::dir_ls("datos/preprocesados_palabras/"), readr::read_rds) |> 
#   list_rbind()

message(paste("total de palabras:", 
              nrow(prensa_palabras) |> format(big.mark = ".", decimal.mark = ",")))


## guardar ----
arrow::write_parquet(prensa_palabras, "datos/prensa_palabras.parquet")

plan(multisession)
rm(datos_prensa_split
   # prensa_palabras
   )
invisible(gc())

beepr::beep()
tictoc::toc()
