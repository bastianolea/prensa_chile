library(dplyr)
library(purrr)
library(furrr)
library(arrow)
library(stm)
library(tidytext)
library(tictoc)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")
if (!exists("prensa_palabras_raiz")) prensa_palabras_raiz <- read_parquet("datos/prensa_palabras_raiz.parquet")
# if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# preparar datos ----
sample_n = 120000

# # ideas: sacar noticias sobre sismos, incendio y meteorología
# datos_prensa_irrelevante <- datos_prensa |> 
#   filter(stringr::str_detect(tolower(titulo), "sismo|temblor|clima|meteorolo|incendio"))
# 
# datos_prensa_irrelevante |> 
#   slice_sample(n = 10) |> 
#   select(titulo)

datos_prensa <- datos_prensa |> 
  select(id, año) |> 
  filter(año >= 2020) |> 
  filter() |> 
  slice_sample(n = sample_n)

ids_entrenamiento <- datos_prensa$id

prensa_palabras_raiz_filt <- prensa_palabras_raiz |> 
  filter(id %in% ids_entrenamiento)

rm(datos_prensa)
rm(prensa_palabras_raiz)

# de el dataset tokenizado, volver a unir por documento
prensa_palabras_stem <- prensa_palabras_raiz_filt |> 
  group_by(id) |> 
  summarise(cuerpo_limpio_stem = paste(raiz, collapse = " "))
# asumiendo que el orden de las palabras es irrelevante
  
rm(prensa_palabras_raiz_filt)

# procesamiento de texto necesario para el modelamiento
# en teoría no es necesario de usar, pero te genera los outputs necesarios para la siguiente función
processed <- stm::textProcessor(documents = prensa_palabras_stem$cuerpo_limpio_stem,
                                metadata = prensa_palabras_stem,
                                lowercase = F, removestopwords = F, removepunctuation = F, removenumbers = F, verbose = F,
                                stem = FALSE, 
                                language = "spanish")

# preparar documentos
out <- stm::prepDocuments(processed$documents, processed$vocab, processed$meta,
                          upper.thresh = sample_n*0.95,
                          lower.thresh = sample_n*0.003)

rm(processed)

# buscar K ----
tic()
findingk_ver2 <- searchK(documents = out$documents,
                         vocab = out$vocab,
                         K = c(17, 18, 19, 20), # probar cantidades de k
                         data = out$meta, 
                         cores = 4,
                         init.type = "Spectral")
toc(); beepr::beep() # 3335 segundos
# 15696 segundos con 200000 y 0.9 y 0.005
# 594 segs con 50000 y 8 cores
# 2215 s con 100000 y 7 cores
# 3880 con 150000 y 6
plot(findingk_ver2)
# maximizar coherencia y held-out likelihood, minimizar residuales

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
modelo_stm$id <- ids_entrenamiento

# agregar metadatos
modelo_stm$out <- out 

# guardar ----
readr::write_rds(modelo_stm, "analisis/modelos/modelo_stm_8.rds")

# out$meta$id
# names(modelo_stm)

