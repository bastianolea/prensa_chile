library(dplyr)
library(purrr)
library(arrow)
library(stm)
library(tictoc)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")

# preparar datos ----
sample_n = 30000

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
                                stem = FALSE, 
                                language = "spanish")

out <- stm::prepDocuments(processed$documents, processed$vocab, processed$meta,
                          upper.thresh = sample_n*0.3, lower.thresh = 40)


# buscar K ----
tic()
findingk_ver2 <- searchK(documents = out$documents,
                         vocab = out$vocab,
                         K = c(10, 16, 18, 19, 20, 25), #specify K to try
                         max.em.its = 200,
                         data = out$meta,
                         init.type = "Spectral")
toc() # 1748 segundos

plot(findingk_ver2)
# maximizar coherencia y helf-out likelihood, minimizar residuales

# definir k en base a lo anterior
.k = 18


# calcular modelo ----
tic()
modelo_stm <- stm(documents = out$documents, 
                  vocab = out$vocab,
                  K = .k, 
                  max.em.its = 100,
                  data = out$meta,
                  init.type = "Spectral")
toc() # 191 segundos


# plot(modelo_stm, n = 4)

labelTopics(modelo_stm)

# topico 9

# guardar ----

# ajustar a datos nuevos ----
# preparar data en datos nuevos
# aquí podría ser en loop por cantidades mas chicas de datos

datos_prensa_filt <- datos_prensa |> 
  filter(año >= 2018) |> 
  # dejar solo los que no fueron considerados en el modelo
  # filter(!id %in% out$meta$id) |> 
  # filtro básico de calidad
  filter(!is.na(cuerpo_limpio), nchar(cuerpo_limpio) > 300) |> 
  # simplificar
  select(id, titulo, cuerpo_limpio) |> 
  # separar
  mutate(grupos = (row_number()-1) %/% (n()/30)) |> # n grupos de igual cantidad de filas
  group_split(grupos)



library(furrr)
plan(multisession, workers = 8)


walk(datos_prensa_filt, \(datos_parte) {
  # datos_parte <- datos_prensa_filt[[1]]
  
  # procesar columnas de texto, especificar la columna que se va a usar como documentos (variable) y si se van a stem las palabras
  datos_parte_stem <- datos_parte |> 
    rowwise() |> 
    mutate(cuerpo_limpio_stem = corpus::text_tokens(cuerpo_limpio, stemmer = "es") |> unlist() |> paste(collapse = " "))
  
  # procesamiento de texto necesario para el modelamiento
  corpus_nuevo <- stm::textProcessor(documents = datos_parte_stem$cuerpo_limpio,
                                     metadata = datos_parte_stem,
                                     # opciones
                                     stem = FALSE, 
                                     language = "spanish")
  
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
    # dejar otros tópicos sólo si son mayores que la mitad del primero
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
  
  # datos_2 |> 
  #   # filter(topico_1 == 17 | topico_2 == 17) |> 
  #   filter(topico_1 == 17) |> 
  #   pull(titulo) |> 
  #   sample(size = 10)
})