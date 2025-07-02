library(dplyr)
library(purrr)
library(furrr)
library(arrow)
library(stm)
library(tictoc)

if (!exists("modelo_stm")) modelo_stm <- readr::read_rds("otros/analisis/tema_delincuencia/modelos/modelo_stm_5.rds")
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")

modelo_stm

names(modelo_stm)

topicos <- labelTopics(modelo_stm, n = 8)

topicos

names(topicos)

topicos$frex
topicos$prob


