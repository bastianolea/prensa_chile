# PRENSA: PASO 3
# se cargan los datos tokenizados en el paso anterior, se cuenta la frecuencia de palabras por noticia, y se guarda el resultado
# input: prensa_palabras.feather (paso 2)
# output: prensa_palabras_conteo.parquet
# tiempo aprox: 55 minutos (ahora sólo 10 minutos!) (8 min con 7 cores) (AHORA 4)

library(dplyr)
library(stringr)
library(tidyr)
library(corpus)
library(purrr)
library(furrr)
library(beepr)

source("funciones.R")

tictoc::tic()

plan(multisession, workers = 7)

# cargar texto procesado ----
# source("prensa_procesar_texto.R") #(ejecutar si hay datos nuevos)
if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_parquet("datos/prensa_palabras.parquet")

# excluir palabras ----
# palabras para excluir de la lematización, porque alterar su sufijo cambia el sentido del concepto, o bien, hace que se agurpe con palabras erróneas
palabras_excluir_stem = c("caso", "casar", "juzgado", "proyecto",
                          "añosa", "años", "salud", "maduro",
                          "suministro", "calama", "abogado",
                          "luis", "luisa",
                          "público", "pública",
                          # sospechas
                          "calle", "calles", "sujeto", "sujetos"
)
# corpus::stem_snowball("calles", algorith = "es")

# calcular ----
# conteo de palabras por sus raíces, por noticia, multiprocesador
tictoc::tic()

## limpieza ----
prensa_palabras_pre <- prensa_palabras |> 
  add_count(id, name = "total_palabras_id") |>
  filter(total_palabras_id < 9999, total_palabras_id > 10) |>
  select(-total_palabras_id) |> 
  filter(nchar(palabra) >= 4)

# separar por grupos para cálculo multiprocesador
prensa_palabras_split <- prensa_palabras_pre |>
  mutate(grupos = (row_number()-1) %/% (n()/30)) |> # n grupos de igual cantidad de filas
  group_split(grupos)

## raiz ----
# obtener raíz de palabras
prensa_palabras_raiz <- prensa_palabras_split |>
  future_map(~mutate(.x,
                     # raiz = corpus::text_tokens(palabra, stemmer = "es")) |> 
                     raiz = corpus::stem_snowball(palabra, algorithm = "es")) |> # más rápido
               unnest(raiz) |> 
               # deshacer stemming de palabras que producen problemas (coinciden con otras)
               mutate(raiz = ifelse(palabra %in% palabras_excluir_stem, palabra, raiz)) |> 
               # privilegiar verbos en infinitivo
               group_by(raiz) |> 
               mutate(orden = ifelse(str_detect(palabra, "ar$|er$|ir$"),
                                     TRUE, FALSE)) |> 
               mutate(tiene_orden = any(orden)) |> 
               # desprivilegiar plurales
               group_by(raiz) |> 
               mutate(orden = ifelse(tiene_orden == FALSE, 
                                     !str_detect(palabra, "as$|es$|is$|os$|us$"), 
                                     orden)) |> 
               # desprivilegiar si es la menos recurrente
               add_count(palabra, name = "palabra_freq_id") |> 
               mutate(orden = ifelse(palabra_freq_id == min(palabra_freq_id), FALSE, orden)) |> 
               # reemplazar raíz de palabra por palabra más apropiada para representar varias conjugaciones (infinitivo, singular)
               arrange(raiz, desc(orden)) |>
               group_by(raiz) |> 
               mutate(palabra = first(palabra)) |>
               ungroup() |> 
               select(-contains("orden"), raiz) |> 
               # agrupar manualmente palabras (conjugaciones, palabras no lematizadas) para que coincidan en una sola
               mutate(palabra = case_match(palabra,
                                           c("venezolano", "venezolana") ~ "venezuela",
                                           c("delincuente", "delictual") ~ "delincuencia",
                                           "abogada" ~ "abogado",
                                           "policial" ~ "policía",
                                           "antisociales" ~ "antisocial",
                                           "saqueo" ~ "saquear",
                                           "calles" ~ "calle",
                                           "sujetos" ~ "sujeto",
                                           c("municipal", "municipio") ~ "municipalidad",
                                           "alcaldesa" ~ "alcalde",
                                           c("pública", "públicas", "públicos") ~ "público",
                                           .default = palabra))
  )


## conteo ----
prensa_palabras_conteo <- prensa_palabras_raiz |> 
  future_map(~count(.x, 
                    id, palabra)) |> 
  list_rbind() |> 
  # excluir palabra si solo aparece una vez entre ids (palabra no se repite entre noticias)
  add_count(palabra, name = "palabra_freq_total") |> 
  filter(palabra_freq_total > 1) |> 
  select(-palabra_freq_total)


# guardar ----
arrow::write_parquet(prensa_palabras_conteo, "datos/prensa_palabras_conteo.parquet")

plan(multisession)
rm(prensa_palabras,
   prensa_palabras_pre,
   prensa_palabras_split,
   prensa_palabras_raiz
   # prensa_palabras_conteo
   )
invisible(gc())

tictoc::toc()
beep()
