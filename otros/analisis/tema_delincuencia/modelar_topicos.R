library(dplyr)
library(purrr)
library(arrow)
# library(ggplot2)
# library(lubridate)
# library(slider)
# library(furrr)
# library(future)

library(stm)
library(tictoc)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")

# preparar datos ----
sample_n = 10000

datos_prensa_filt <- datos_prensa |> 
  filter(año >= 2023) |> 
  slice_sample(n = sample_n)

# rm(datos_prensa)

# lematizar palabras del cuerpo o título
datos_stem <- datos_prensa_filt |> 
  rowwise() |> 
  mutate(cuerpo_limpio_stem = corpus::text_tokens(cuerpo_limpio, stemmer = "es") |> unlist() |> paste(collapse = " ")) |> 
  ungroup()

# procesamiento de texto necesario para el modelamiento
processed <- stm::textProcessor(documents = datos_stem$cuerpo_limpio_stem,
                                metadata = datos_stem,
                                # opciones
                                lowercase = FALSE,
                                removestopwords = TRUE,
                                removenumbers = FALSE,
                                removepunctuation = FALSE, 
                                stem = FALSE, 
                                wordLengths = c(3, 20),
                                sparselevel = 1,
                                language = "spanish",
                                verbose = FALSE, 
                                onlycharacter = FALSE,
                                striphtml = FALSE, 
                                customstopwords = NULL)

out <- stm::prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 30)

# # buscar K ----
# tic()
# findingk_ver2 <- searchK(documents = out$documents, 
#                          vocab = out$vocab,
#                          # K = c(5, 10, 15, 20, 30, 40), #specify K to try
#                          K = c(5, 10, 16, 17, 18, 19, 20, 30), #specify K to try
#                          # K = 0, # para que use algoritmo qeu encuentra topicos
#                          # N = 500,
#                          # proportion = 0.5,
#                          # M = 10,
#                          # prevalence = ~s(date),
#                          max.em.its = 200,
#                          data = out$meta,
#                          init.type = "Spectral")
# toc()
# 
# plot(findingk_ver2)
# # maximizar coherencia y helf-out likelihood, minimizar residuales
.k = 18



# calcular modelo ----
tic()
modelo_stm <- stm(documents = out$documents, vocab = out$vocab,
                  K = .k, 
                  # prevalence = ~ media + s(date),
                  # prevalence = ~ s(date),
                  max.em.its = 175,
                  # max.em.its = 75, #converge aprox al 76
                  data = out$meta,
                  init.type = "Spectral")
toc()


# plot(modelo_stm, n = 4)

labelTopics(modelo_stm)

# topico 8

# ajustar a datos nuevos ----
# preparar data en datos nuevos
# aquí podría ser en loop por cantidades mas chicas de datos

datos_prensa_test <- datos_prensa |> 
  filter(año >= 2018) |> 
  # dejar solo los que no fueron considerados en el modelo
  filter(!id %in% out$meta$id) |> 
  #muestra
  # slice_sample(n = 10000) |> 
  # filtro básico de calidad
  filter(!is.na(cuerpo_limpio), nchar(cuerpo_limpio) > 300) |> 
  # separar
  mutate(grupos = (row_number()-1) %/% (n()/8)) |> # n grupos de igual cantidad de filas
  group_split(grupos)

library(furrr)
plan(multisession, workers = 6)

# procesar columnas de texto, especificar la columna que se va a usar como documentos (variable) y si se van a stem las palabras
datos_prensa_test_stem <- datos_prensa_test |> 
  future_map(
    \(partes) {
      partes |> 
        rowwise() |> 
        mutate(cuerpo_limpio_stem = corpus::text_tokens(cuerpo_limpio, stemmer = "es") |> unlist() |> paste(collapse = " "))
    }) |> 
  list_rbind()

datos_prensa_test_stem 

# procesamiento de texto necesario para el modelamiento
corpus_nuevo <- stm::textProcessor(documents = datos_prensa_test_stem$cuerpo_limpio,
                                   metadata = datos_prensa_test_stem,
                                   # opciones
                                   lowercase = FALSE,
                                   removestopwords = TRUE,
                                   removenumbers = FALSE,
                                   removepunctuation = FALSE, 
                                   stem = FALSE, 
                                   wordLengths = c(3, Inf),
                                   sparselevel = 1,
                                   language = "spanish",
                                   verbose = FALSE, 
                                   onlycharacter = FALSE,
                                   striphtml = FALSE, 
                                   customstopwords = NULL)

# alinear el corpus generado en base al vocabulario del modelo entrenado
corpus_alineado <- alignCorpus(corpus_nuevo, modelo_stm$vocab)

# ajustar nuevos documentos (argumentos documents, newData) en base a modelo y corpus previamente entrenados (argumentos model, origData)
documentos_ajustados <- fitNewDocuments(model = modelo_stm, #modelo previamente entrenado
                                        documents = corpus_alineado$documents, #documentos nuevos
                                        newData = corpus_alineado$meta, #metadata de los documentos nuevos
                                        origData = out$meta #metadata del modelo entrenado
)

# obtener los valores theta del modelo
thetas <- modelo_stm$theta |> 
  as.data.frame() |> 
  mutate(documento = 1:n())

# transformar a dataframe long
thetas_max <- thetas |>
  # pivotar a long
  tidyr::pivot_longer(cols = starts_with("V"), names_to = "topic", values_to = "theta") |>
  # filtrar el top x de thetas
  group_by(documento) |>
  slice_max(n = 3, order_by = theta) |>
  # dejar otros topicos solo si son mayores que la mitad del primero
  mutate(theta = if_else(theta >= max(theta)/3, theta, NA)) |>
  filter(!is.na(theta))

# crear etiquetas
thetas_max_etiqueta <- thetas_max |>
  group_by(documento) |>
  # crear variables ranking del topico
  mutate(topico_n = 1:n(),
         topico_n = paste0("topico_", topico_n), #nombre
         topic = stringr::str_remove(topic, "V") #numero
  )

# transformar a data frames wide
topicos_numero <- thetas_max_etiqueta |> 
  tidyr::pivot_wider(id_cols = documento, names_from = topico_n, values_from = topic)

topicos_theta <- thetas_max_etiqueta |> 
  mutate(topico_n = stringr::str_replace(topico_n, "topico_", "topico_theta_")) |> 
  tidyr::pivot_wider(id_cols = documento, names_from = topico_n, values_from = theta)


datos_2 <- tibble(out$meta) |> 
  bind_cols(topicos_numero) |> 
  select(-documento) |> 
  bind_cols(topicos_theta) |> 
  select(-documento)

datos_2 |> 
  # filter(topico_1 == 17 | topico_2 == 17) |> 
  filter(topico_1 == 17) |> 
  pull(titulo) |> 
  sample(size = 10)
