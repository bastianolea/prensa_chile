library(dplyr)
library(purrr)
library(furrr)
library(arrow)
library(stm)
library(tictoc)

# este script carga los datos de noticias y los ajusta al modelo de tópicos
# al excluir los documentos ya procesados, solamente ajusta los documentos nuevos

tic()

modelo_stm <- readr::read_rds("analisis/modelos/modelo_stm_8.rds")

# ajustar a datos nuevos ----
# preparar data en datos nuevos
# aquí podría ser en loop por cantidades mas chicas de datos

datos_prensa <- read_parquet("datos/prensa_datos.parquet")

# cargar resultados anteriores
prensa_topicos <- fs::dir_ls("analisis/resultados/partes") |>
  map(arrow::read_parquet) |>
  list_rbind()
# si se empieza de cero, omitir

datos_prensa <- datos_prensa |> 
  filter(año >= 2018) |> 
  # sacar los que ya fueron procesados antes
  filter(!id %in% prensa_topicos$id) |>
  # filtro básico de calidad
  filter(!is.na(cuerpo_limpio), nchar(cuerpo_limpio) > 300) |> 
  # simplificar
  select(id, titulo, cuerpo_limpio)


# datos ----
# prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")
prensa_palabras_raiz <- read_parquet("datos/prensa_palabras_raiz.parquet")

prensa_palabras_raiz <- prensa_palabras_raiz |> 
  filter(id %in% datos_prensa$id) #|>
# sacar los que ya fueron procesados antes
# filter(!id %in% prensa_topicos$id)

# rm(datos_prensa)

# de el dataset tokenizado, volver a unir por documento
prensa_palabras_stem <- prensa_palabras_raiz |> 
  group_by(id) |> 
  summarise(cuerpo_limpio_stem = paste(raiz, collapse = " "))

rm(prensa_palabras_raiz)


# procesar ----
prensa_palabras_stem <- prensa_palabras_stem |> 
  mutate(grupos = (row_number()-1) %/% (n()/16)) |> # n grupos de igual cantidad de filas
  group_split(grupos)

walk(prensa_palabras_stem, \(prensa_palabras_stem) {
  
  # procesamiento de texto necesario para el modelamiento
  corpus_nuevo <- stm::textProcessor(documents = prensa_palabras_stem$cuerpo_limpio_stem,
                                     metadata = prensa_palabras_stem,
                                     lowercase = F, removestopwords = F, removepunctuation = F, removenumbers = F, verbose = F,
                                     stem = FALSE, 
                                     language = "spanish")
  
  out <- stm::prepDocuments(corpus_nuevo$documents, corpus_nuevo$vocab, corpus_nuevo$meta,
                            # upper.thresh = nrow(datos_parte)*0.7,
                            lower.thresh = 5)
  
  # alinear el corpus generado en base al vocabulario del modelo entrenado
  corpus_alineado <- alignCorpus(corpus_nuevo, modelo_stm$vocab)
  
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
    select(-cuerpo_limpio_stem)
  
  # datos_2 |> filter(topico_1 == 3) |> select(titulo)
  
  # guardar pieza ----
  archivo <- paste0("analisis/resultados/partes/ajustado_", 
                    Sys.Date(), "_", 
                    first(prensa_palabras_stem$grupos), "_",
                    sample(100:999, 1), ".parquet")
  message(paste("guardando", archivo))
  
  arrow::write_parquet(datos_parte_topico,
                       archivo)
  message("ok ", first(prensa_palabras_stem$grupos))
})

toc()


# unir todos ----

partes <- fs::dir_ls("analisis/resultados/partes/")

ajustados <- map(partes, read_parquet) |> list_rbind()


# recodificar ----
labelTopics(modelo_stm, n = 6)


ajustados_recod <- ajustados |>
  # recodificar tópicos
  mutate(across(c(topico_1, topico_2, topico_3),
                ~case_match(.x,
                            "1" ~ "otros", #sismos
                            "2" ~ "política",
                            "3" ~ "otros", #clima
                            "4" ~ "delincuencia", #sexual
                            "5" ~ "salud",
                            "6" ~ "economía",
                            "7" ~ "internacional",
                            "8" ~ "política",
                            "9" ~ "educación",
                            "10" ~ "judicial",
                            "11" ~ "judicial", #(duda)
                            "12" ~ "otros", 
                            "13" ~ "política", #parlamentarios
                            "14" ~ "economía",
                            "15" ~ "corrupción",
                            "16" ~ "política", #gobierno
                            "17" ~ "otros", #incendios
                            "18" ~ "delincuencia",
                ),
                .names = "{.col}_t"))

# revisar ----
# glimpse(ajustados_recod)
# 
# ajustados_recod |> 
#   filter(topico_1_t == "otros") |> 
#   left_join(datos_prensa |> select(id, titulo)) |> 
#   select(titulo) |> 
#   print(n=30)
# 
# ajustados_recod |> 
#   filter(topico_1_t == "delincuencia",,
#          topico_theta_1 > 0.4) |> 
#   left_join(datos_prensa |> select(id, titulo)) |> 
#   select(titulo) |> 
#   slice(c(10:25))
# 
# ajustados_recod |> 
#   filter(topico_1_t == "judicial",
#          topico_theta_1 > 0.4) |> 
#   left_join(datos_prensa |> select(id, titulo)) |> 
#   select(titulo) |> 
#   slice(c(1:10))

# datos_prensa |> 
#   # filter(id == "edb34c8db53e8ec866a9c459355d90f4") |> 
#   # filter(id == "718583f469e25538c80b6cfcc7bb16ea") |> 
#   filter(id  == "bcf4db5211d6627de8012f1c0f35b75c") |> 
#   pull(cuerpo, url)


datos_prensa_topicos <- datos_prensa |>
  # sólo clasificados
  filter(id %in% ajustados_recod$id) |>
  # agregar tópicos
  left_join(ajustados_recod,
            join_by(id))


# # buscar
# datos_prensa_topicos |>
#   filter(topico_1 == "delincuencia") |>
#   filter(topico_theta_1 > 0.17) |> # umbral
#   slice_sample(n = 10) |>
#   select(titulo, topico_1, topico_2, topico_theta_1)

# datos_prensa_topicos |> 
#   select(titulo, ends_with("_t"), contains("theta")) |> 
#   mutate(topico_1_t = if_else(topico_theta_1 < 0.3, NA, topico_1_t),
#          topico_2_t = if_else(topico_theta_2 < 0.3, NA, topico_2_t),
#          topico_3_t = if_else(topico_theta_3 < 0.3, NA, topico_3_t)) |> 
#   print(n=100)

# márgen mínimo de certeza, o si no se pasa a otros
clasificacion <- datos_prensa_topicos |> 
  select(id, topico_1_t, topico_theta_1) |> 
  mutate(topico_1_t = if_else(topico_theta_1 < 0.3, "otros", topico_1_t)) |> 
  select(id, clasificacion = topico_1_t)

# clasificacion

# guardar ----
arrow::write_parquet(clasificacion, "datos/prensa_stm_clasificar.parquet")
