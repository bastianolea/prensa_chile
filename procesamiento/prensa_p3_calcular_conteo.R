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

plan(multisession, workers = 8)

# cargar texto procesado ----
# source("prensa_procesar_texto.R") #(ejecutar si hay datos nuevos)
if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_parquet("datos/prensa_palabras.parquet")

# excluir palabras ----
# palabras para excluir de la lematización, porque alterar su sufijo cambia el sentido del concepto, o bien, hace que se agrupe con palabras erróneas
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

# ## limpieza ----
# prensa_palabras_pre <- prensa_palabras |> 
#   # cantidad de palabras por id
#   add_count(id, name = "total_palabras_id") |>
#   filter(#total_palabras_id < 9999, 
#          total_palabras_id > 100) |>
#   select(-total_palabras_id)
# innecesario porque se hace en pasos 1 y 2

# separar por grupos para cálculo multiprocesador
prensa_palabras_split <- prensa_palabras |>
  select(-palabra_freq_total) |> 
  mutate(grupo = (row_number()-1) %/% (n()/16)) |> # n grupos de igual cantidad de filas
  group_split(grupo)

rm(prensa_palabras)

## raiz ----
# obtener raíz de palabras
# el objetivo de esto es que, como un mismo concepto puede tener varias conjugaciones, se obtiene la raíz de cada palabra,
# lo que hace que múltiples conjugaciones de una palabra compartan una raíz; luego se agrupan las palabras por su raíz,
# y se aplica un algoritmo para privilegiar la palabra más apropiada de todas las conjugaciones (infinitivo, etc.), y se
# usa esa palabra para reemplazar las conjugaciones posibles. Así, delincuente, delincuencia y delinquir quedan como "delincuencia".

prensa_palabras_raiz <- future_map(prensa_palabras_split, \(parte) {
  # parte <- prensa_palabras_split[[1]]
  resultado <- parte |> 
    mutate(raiz = corpus::stem_snowball(palabra, algorithm = "es")) |> 
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
    select(-palabra_freq_id) |> 
    # reemplazar raíz de palabra por palabra más apropiada para representar varias conjugaciones (infinitivo, singular)
    arrange(raiz, desc(orden)) |>
    group_by(raiz) |> 
    mutate(palabra = first(palabra)) |>
    ungroup() |> 
    select(-contains("orden"), raiz) |> 
    # agrupar manualmente palabras (conjugaciones, palabras no lematizadas) para que coincidan en una sola
    mutate(palabra = case_match(palabra,
                                c("venezolano", "venezolana") ~ "venezuela",
                                c("inmigrantes", "inmigrante", "migrantes", "migrante", "migrar", "inmigrar", "migración", "inmigración", "migratorio", "migratoria", "emigrar") ~ "migración",
                                c("delincuencia", "delincuente", "delincuencial", "delinquir", "delictual") ~ "delincuencia",
                                c("seguridad", "inseguridad", "inseguro") ~ "seguridad",
                                "abogada" ~ "abogado",
                                "partida" ~ "partido",
                                "policial" ~ "policía",
                                "antisociales" ~ "antisocial",
                                "saqueo" ~ "saquear",
                                "calles" ~ "calle",
                                "sujetos" ~ "sujeto",
                                c("unidad", "unido", "unir", "unid") ~ "unidad",
                                c("municipal", "municipio") ~ "municipalidad",
                                "alcaldesa" ~ "alcalde",
                                c("ministro", "ministra", "ministros", "ministras") ~ "ministro/a",
                                c("presidenta", "presidencial") ~ "presidente",
                                c("pública", "públicas", "públicos") ~ "público",
                                .default = palabra)) |> 
    # corregir palabras lematizadas
    mutate(palabra = case_match(palabra,
                                "pensionar" ~ "pensiones",
                                "reformar" ~ "reforma",
                                "ministrar" ~ "ministro/a",
                                .default = palabra)) |> 
    select(-grupo)
  return(resultado)
})

rm(prensa_palabras_split)
plan(multisession, workers = 8)


## conteo ----
# conteo de palabras por id
prensa_palabras_conteo <- prensa_palabras_raiz |> 
  future_map(~count(.x, id, palabra)) |> 
  list_rbind()
  # excluir palabra si solo aparece una vez entre ids (palabra no se repite entre noticias)
  # add_count(palabra, name = "palabra_freq_total") |> 
  # filter(palabra_freq_total > 20) |> 
  # select(-palabra_freq_total)


# guardar ----
prensa_palabras_raiz <- list_rbind(prensa_palabras_raiz)

arrow::write_parquet(prensa_palabras_raiz, "datos/prensa_palabras_raiz.parquet")
arrow::write_parquet(prensa_palabras_conteo, "datos/prensa_palabras_conteo.parquet")
# arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# limpieza
rm(prensa_palabras,
   prensa_palabras_pre,
   prensa_palabras_split)
   # prensa_palabras_raiz
   # prensa_palabras_conteo
plan(multisession)
invisible(gc())

tictoc::toc()
# beep()
