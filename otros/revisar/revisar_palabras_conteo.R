library(dplyr)
library(purrr)
library(furrr)
library(arrow)
library(stm)
library(tidytext)
library(tictoc)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")
# if (!exists("prensa_palabras_raiz")) prensa_palabras_raiz <- read_parquet("datos/prensa_palabras_raiz.parquet")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")


prensa_conteo <- prensa_palabras_conteo |> 
  summarize(n = sum(n), .by = palabra)

prensa_conteo |> 
  arrange(n) |> print(n=300)

prensa_conteo |> 
  filter(str_detect(palabra, "width"))

# "linewidth", "groupwidth", "offsetwidth", "maxwidth", "minwidth", 

datos_prensa_filt <- datos_prensa |> 
  filter(str_detect(cuerpo_limpio, "offsetwidth"))

datos_prensa_filt |> 
  slice(5) |> 
  pull(cuerpo) |> 
  str_remove_all("var divElement.*\\);") |> 
  str_remove_all("\\{\\{.*\\}\\}") |> 
  str_remove_all("\\{.*\\}") |>
  str_remove_all("\\#.*\\b") |>
  str_remove("RelacionadasDetalle")
