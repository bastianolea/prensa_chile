library(dplyr)
library(stringr)
library(tidyr)

source("funciones.R")

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# datos_prensa


# noticias con la palabra x
prensa_palabras_conteo_2 <- prensa_palabras_conteo |> 
  filter(palabra == "público")

# agregar metadatos
prensa_palabras_conteo_3 <- prensa_palabras_conteo_2 |> 
  left_join(datos_prensa |> 
              select(id, cuerpo_limpio, fuente, fecha),
            by = "id") 

# conteo de fuentes con la palabra x
prensa_palabras_conteo_3 |> 
  group_by(fuente) |> 
  summarize(n=sum(n)) |> 
  arrange(desc(n))


# ver qué dicen las noticias de una fuente que contiene la palabra x
prensa_palabras_conteo_3 |> 
  filter(fuente == "emol") |> 
  arrange(desc(fecha)) |> 
  slice(1:10) |> 
  pull(cuerpo_limpio)


library(tidytext)

prensa_palabras_conteo_3 |> 
  filter(fuente == "emol") |> 
  arrange(desc(fecha)) |> 
  slice(1:10) |> 
  unnest_tokens(output = "palabra", input = cuerpo_limpio) |> 
  count(palabra, sort = TRUE) |> 
  print(n=49)
