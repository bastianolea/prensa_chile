library(dplyr)
library(purrr)
library(furrr)
library(arrow)
library(stm)
library(tictoc)

tic()

modelo_stm <- readr::read_rds("analisis/modelos/modelo_stm_7.rds")

# ajustar a datos nuevos ----
# preparar data en datos nuevos
# aquí podría ser en loop por cantidades mas chicas de datos

datos_prensa <- read_parquet("datos/prensa_datos.parquet")

# cargar resultados anteriores
prensa_topicos <- fs::dir_ls("analisis/resultados/partes") |> 
  map(arrow::read_parquet) |> 
  list_rbind()

datos_prensa_filt <- datos_prensa |> 
  filter(año >= 2018) |> 
  # sacar los usados para entrenar el modelo
  filter(!id %in% modelo_stm$id) |>
  # sacar los que ya fueron procesados antes
  # filter(!id %in% prensa_topicos$id) |>
  # filtro básico de calidad
  filter(!is.na(cuerpo_limpio), nchar(cuerpo_limpio) > 300) |> 
  # simplificar
  select(id, titulo, cuerpo_limpio)

prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

prensa_palabras_conteo <- prensa_palabras_conteo |> 
  # sacar los usados para entrenar el modelo
  filter(!id %in% modelo_stm$id) #|>
  # sacar los que ya fueron procesados antes
  # filter(!id %in% prensa_topicos$id)

# crear document-term matrix desde datos de palabras contadas
prensa_dtm <- prensa_palabras_conteo |> 
  tidytext::cast_dtm(id, palabra, n)

# convertir a formato de stm
corpus_nuevo <- readCorpus(prensa_dtm, type = "slam")

# plan(multisession, workers = 7)
# resultados <- map(datos_prensa_filt, \(datos_parte) {
#   # datos_parte <- datos_prensa_filt[[1]]
#   message("iniciando")
  # procesar columnas de texto, especificar la columna que se va a usar como documentos (variable) y si se van a stem las palabras
  # datos_parte_stem <- datos_parte |> 
  # rowwise() |> 
  # mutate(cuerpo_limpio_stem = corpus::text_tokens(cuerpo_limpio, stemmer = "es") |> unlist() |> paste(collapse = " "))
  
  # # procesamiento de texto necesario para el modelamiento
  # corpus_nuevo <- stm::textProcessor(documents = datos_parte$cuerpo_limpio,
  #                                    metadata = datos_parte,
  #                                    # opciones
  #                                    stem = T, lowercase = F, removenumbers = F, removepunctuation = F,
  #                                    language = "spanish")
  
  # 
  # out <- stm::prepDocuments(corpus_nuevo$documents, corpus_nuevo$vocab, corpus_nuevo$meta,
  #                           # upper.thresh = nrow(datos_parte)*0.7,
  #                           lower.thresh = nrow(datos_parte)*0.01)
  
  # alinear el corpus generado en base al vocabulario del modelo entrenado
  corpus_alineado <- alignCorpus(corpus_nuevo, modelo_stm$vocab)
  
  # out <- stm::prepDocuments(corpus_alineado$documents, corpus_alineado$vocab, corpus_alineado$meta,
  #                           # upper.thresh = nrow(datos_parte)*0.7,
  #                           lower.thresh = 3)
  
  # ajustar nuevos documentos (argumentos documents, newData) en base a modelo y corpus previamente entrenados (argumentos model, origData)
  documentos_ajustados <- fitNewDocuments(model = modelo_stm, # modelo previamente entrenado
                                          documents = corpus_alineado$documents, # documentos nuevos
                                          newData = corpus_alineado$meta, # metadata de los documentos nuevos
                                          origData = modelo_stm$out$meta # metadata del modelo entrenado
  )
  
  # obtener los valores theta del modelo
  thetas <- documentos_ajustados$theta |> 
    as.data.frame() |> 
    mutate(documento = 1:n()) |> 
    tibble()
  
  # transformar a dataframe long
  thetas_max <- thetas |>
    # pivotar a long
    tidyr::pivot_longer(cols = starts_with("V"), names_to = "topic", values_to = "theta") |>
    # filtrar el top x de thetas
    group_by(documento) |>
    slice_max(n = 3, order_by = theta) |>
    # dejar otros tópicos sólo si son mayores que la mitad del primero
    # mutate(theta = if_else(theta >= max(theta)/3, theta, NA)) |>
    filter(!is.na(theta))
  
  # crear etiquetas
  thetas_max_etiqueta <- thetas_max |>
    group_by(documento) |>
    # crear variables ranking del topico
    mutate(topico_n = 1:n(),
           topico_n = paste0("topico_", topico_n), #nombre
           topic = stringr::str_remove(topic, "V")) |> #numero
    ungroup()
  
  # transformar a dataframes wide
  topicos_numero <- thetas_max_etiqueta |> 
    tidyr::pivot_wider(id_cols = documento, names_from = topico_n, values_from = topic) |> 
    select(-documento)
  
  topicos_theta <- thetas_max_etiqueta |> 
    mutate(topico_n = stringr::str_replace(topico_n, "topico_", "topico_theta_")) |> 
    tidyr::pivot_wider(id_cols = documento, names_from = topico_n, values_from = theta) |> 
    select(-documento)
  
  # agregar topicos y probabilidades a datos
  datos_parte_topico <- tibble(corpus_alineado$meta) |> 
    bind_cols(topicos_numero) |> 
    bind_cols(topicos_theta) |> 
    select(-cuerpo_limpio)
  
  # datos_2 |> filter(topico_1 == 3) |> select(titulo)
  
  # # guardar ----
  # archivo <- paste0("otros/analisis/tema_delincuencia/resultados/partes/ajustado_", Sys.Date(), "_", sample(10000:99999, 1), ".parquet")
  # message(paste("guardando", archivo))
  # 
  # arrow::write_parquet(datos_parte_topico |> select(-titulo, -cuerpo_limpio), 
  #                      archivo)
  message("ok")
#   return(datos_parte_topico)
# }) |> 
#   list_rbind()

# resultados |> 
#   select(-titulo)
# 
resultados





# guardar ----
# arrow::write_parquet(resultados_recod |> select(-titulo),
#                      "datos/prensa_topicos.parquet")

# guardar pieza ----
arrow::write_parquet(resultados, 
                     paste("analisis/tema_delincuencia/resultados/partes/stm_ajustados_", today(), "_", sample(1000:9999, 1), ".parquet", sep = "")
)


# datos_prensa_topicos

toc()


# unir todos ----

partes <- fs::dir_ls("analisis/tema_delincuencia/resultados/partes/")

ajustados <- map(partes, read_parquet) |> list_rbind()


labelTopics(modelo_stm, n = 12)


ajustados_recod <- ajustados |>
  # recodificar tópicos
  mutate(across(c(topico_1, topico_2, topico_3),
                ~case_match(.x,
                            "1" ~ "sismos",
                            "2" ~ "medioambiente",
                            "3" ~ "corrupción",
                            "4" ~ "salud",
                            "5" ~ "otros1",
                            "6" ~ "educación",
                            "7" ~ "clima",
                            "8" ~ "pensiones",
                            "9" ~ "corrupción", # 2
                            "10" ~ "incendios",
                            "11" ~ "economía",
                            "12" ~ "otros2",  # 2
                            "13" ~ "delincuencia",
                            "14" ~ "social",
                            "15" ~ "política",
                            "16" ~ "política",  # 2
                            "17" ~ "internacional",
                            "18" ~ "otros3", # 3
                            "19" ~ "economía"  # 2
                            ),
                .names = "{.col}_t"))

ajustados_recod |> 
  filter(topico_1_t == "otros3") |> 
  select(titulo) |> 
  print(n=30)
  
ajustados_recod |> 
  filter(topico_1_t == "otros3") |> 
  # select(titulo)
  slice(c(15)) |> 
  pull(id)

datos_prensa |> 
  # filter(id == "edb34c8db53e8ec866a9c459355d90f4") |> 
  # filter(id == "718583f469e25538c80b6cfcc7bb16ea") |> 
  filter(id  == "bcf4db5211d6627de8012f1c0f35b75c") |> 
  pull(cuerpo, url)

datos_prensa_topicos <- datos_prensa |>
  # sólo clasificados
  filter(id %in% ajustados$id) |>
  # agregar tópicos
  left_join(ajustados_recod |> select(-titulo),
            join_by(id))


#
#
# # agregar a datos completos
# datos_prensa_topicos <- datos_prensa |>
#   # sólo clasificados
#   filter(id %in% resultados$id) |>
#   # agregar tópicos
#   left_join(resultados_recod |> select(-titulo),
#             join_by(id))
#
# glimpse(datos_prensa_topicos)
#
# # buscar
# datos_prensa_topicos |>
#   filter(topico_1 == "delincuencia") |>
#   filter(topico_theta_1 > 0.17) |> # umbral
#   slice_sample(n = 10) |>
#   select(titulo, topico_1, topico_2, topico_theta_1)
