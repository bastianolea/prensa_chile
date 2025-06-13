library(dplyr)
library(stringr)
library(tidyr)
library(corpus)
library(purrr)
library(furrr)
library(beepr)

if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_parquet("datos/prensa_palabras.parquet")
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")

prensa_palabras_raiz <- arrow::read_parquet("datos/prensa_palabras_raiz.parquet")


filtro <- prensa_palabras_raiz |> 
  filter(raiz == "est")

filtro_b <- filtro |> 
  distinct(palabra, .keep_all = TRUE)

prensa_palabras_filt <- prensa_palabras |> 
  filter(id %in% unique(filtro_b$id))

prensa_palabras_filt

datos_prensa_filt <- datos_prensa |> 
  filter(id %in% unique(filtro_b$id))


datos_prensa_filt |> pull(cuerpo_limpio)
