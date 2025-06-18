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

# preparar datos ----
sample_n = 100000

# ideas: sacar noticias sobre sismos, incendio y meteorología

datos_prensa <- datos_prensa |> 
  select(id, año) |> 
  filter(año >= 2020) |> 
  filter() |> 
  slice_sample(n = sample_n)

# muestra de los documentos con palabras contadas
prensa_palabras_conteo <- prensa_palabras_conteo |> 
  filter(id %in% datos_prensa$id)

rm(datos_prensa)

# crear document-term matrix desde datos de palabras contadas
prensa_dtm <- prensa_palabras_conteo |> 
  tidytext::cast_dtm(id, palabra, n)

# convertir a formato de stm
prensa_corpus <- readCorpus(prensa_dtm, type = "slam")

rm(prensa_dtm)

# preparar documentos
out <- stm::prepDocuments(prensa_corpus$documents, prensa_corpus$vocab, prensa_corpus$meta,
                          upper.thresh = sample_n*0.9,
                          lower.thresh = sample_n*0.005)


# buscar K ----
tic()
findingk_ver2 <- searchK(documents = out$documents,
                         vocab = out$vocab,
                         K = c(17, 18, 19, 20), # probar cantidades de k
                         data = out$meta, 
                         cores = 7,
                         init.type = "Spectral")
toc(); beepr::beep() # 3335 segundos
# 15696 segundos con 200000 y 0.9 y 0.005
# 594 segs con 50000 y 8 cores
# 2215 s con 100000 y 7 cores

plot(findingk_ver2)
# maximizar coherencia y held-out likelihood, minimizar residuales

# definir k en base a lo anterior
.k = 19


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

# 2 ~ "economía",
# 3 ~ "delincuencia",
# 4 ~ "gobierno",
# 5 ~ "internacional",
# 6 ~ "parlamento",
# 11 ~ "justicia",
# 12 ~ "social",
# 13 ~ 

# topico 12

# agregar ids de noticias usadas para entrenar
modelo_stm$id <- unique(prensa_palabras_conteo$id)
message(paste(length(prensa_palabras_conteo$id), "palabras"))
message(paste(length(unique(prensa_palabras_conteo$id)), "documentos"))

# guardar ----
readr::write_rds(modelo_stm, "otros/analisis/tema_delincuencia/modelos/modelo_stm_5.rds")

# out$meta$id
# names(modelo_stm)

