library(dplyr)
library(purrr)
library(furrr)
library(arrow)
library(stm)
library(tictoc)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")
if (!exists("prensa_palabras_raiz")) prensa_palabras_raiz <- read_parquet("datos/prensa_palabras_raiz.parquet")

# preparar datos ----
sample_n = 100000

datos_prensa_filt <- datos_prensa |> 
  select(id, año) |> 
  filter(año >= 2018) |> 
  slice_sample(n = sample_n)

prensa_palabras_raiz_filt <- prensa_palabras_raiz |> 
  filter(id %in% datos_prensa_filt$id)

plan(multisession, workers = 8)

prensa_palabras_stem <- prensa_palabras_raiz_filt |> 
  mutate(grupo = (row_number()-1) %/% (n()/16)) |> # n grupos de igual cantidad de filas
  group_split(grupo) |> 
  future_map(~summarise(.x, cuerpo_limpio_stem = paste(raiz, collapse = " "), .by = id)) |> 
  list_rbind()

# asumiendo que el orden de las palabras es irrelevante

# rm(datos_prensa)

# # lematizar palabras del cuerpo o título
# datos_stem <- datos_prensa_filt |> 
#   rowwise() |> 
#   mutate(cuerpo_limpio_stem = corpus::text_tokens(cuerpo_limpio, stemmer = "es") |> unlist() |> paste(collapse = " ")) |> 
#   ungroup()

# procesamiento de texto necesario para el modelamiento
# en teoría no es necesario de usar, pero te genera los outputs necesarios para la siguiente función
processed <- stm::textProcessor(documents = prensa_palabras_stem$cuerpo_limpio_stem,
                                metadata = prensa_palabras_stem,
                                lowercase = F, removestopwords = F, removepunctuation = F, removenumbers = F, verbose = F,
                                stem = FALSE, 
                                language = "spanish")

out <- stm::prepDocuments(processed$documents, processed$vocab, processed$meta,
                          upper.thresh = sample_n*0.7,
                          lower.thresh = sample_n*0.01)


# buscar K ----
# tic()
# findingk_ver2 <- searchK(documents = out$documents,
#                          vocab = out$vocab,
#                          K = c(16, 18, 19, 20), # probar cantidades de k
#                          max.em.its = 100,
#                          data = out$meta,
#                          init.type = "Spectral")
# toc(); beepr::beep() # 3335 segundos


# plot(findingk_ver2)
# maximizar coherencia y helf-out likelihood, minimizar residuales

# definir k en base a lo anterior
.k = 18


# calcular modelo ----
tic()
modelo_stm <- stm(documents = out$documents, 
                  vocab = out$vocab,
                  K = .k, 
                  max.em.its = 300,
                  data = out$meta,
                  init.type = "Spectral")
toc() # 425 segundos


# plot(modelo_stm, n = 4)

labelTopics(modelo_stm)

# topico 18

# guardar ----
readr::write_rds(modelo_stm, "otros/analisis/tema_delincuencia/modelos/modelo_stm.rds")

out$meta
