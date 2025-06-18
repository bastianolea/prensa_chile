library(dplyr)
library(purrr)
library(furrr)
library(arrow)
library(stm)
library(tictoc)

if (!exists("modelo_stm")) modelo_stm <- readr::read_rds("otros/analisis/tema_delincuencia/modelos/modelo_stm_5.rds")

# ajustar a datos nuevos ----
# preparar data en datos nuevos
# aquí podría ser en loop por cantidades mas chicas de datos

if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")

datos_prensa_filt <- datos_prensa |> 
  filter(año >= 2018) |> 
  # dejar solo los que no fueron considerados en el modelo
  # filter(!id %in% out$meta$id) |> 
  # filtro básico de calidad
  filter(!is.na(cuerpo_limpio), nchar(cuerpo_limpio) > 300) |> 
  # simplificar
  select(id, titulo, cuerpo_limpio) |> 
  # separar
  split(1:16)

# # eliminar resultados anteriores
# fs::dir_ls("~/R/prensa/otros/analisis/tema_delincuencia/resultados/partes/") |> 
#   fs::file_delete()

plan(multisession, workers = 8)

resultados <- map(datos_prensa_filt, \(datos_parte) {
  # datos_parte <- datos_prensa_filt[[1]]
  message("iniciando")
  # procesar columnas de texto, especificar la columna que se va a usar como documentos (variable) y si se van a stem las palabras
  # datos_parte_stem <- datos_parte |> 
  # rowwise() |> 
  # mutate(cuerpo_limpio_stem = corpus::text_tokens(cuerpo_limpio, stemmer = "es") |> unlist() |> paste(collapse = " "))
  
  # procesamiento de texto necesario para el modelamiento
  corpus_nuevo <- stm::textProcessor(documents = datos_parte$cuerpo_limpio,
                                     metadata = datos_parte,
                                     # opciones
                                     stem = T, lowercase = F, removenumbers = F, removepunctuation = F,
                                     language = "spanish")
  
  
  out <- stm::prepDocuments(corpus_nuevo$documents, corpus_nuevo$vocab, corpus_nuevo$meta,
                            # upper.thresh = nrow(datos_parte)*0.7,
                            lower.thresh = nrow(datos_parte)*0.01)
  
  # alinear el corpus generado en base al vocabulario del modelo entrenado
  corpus_alineado <- alignCorpus(corpus_nuevo, modelo_stm$vocab)
  
  # ajustar nuevos documentos (argumentos documents, newData) en base a modelo y corpus previamente entrenados (argumentos model, origData)
  documentos_ajustados <- fitNewDocuments(model = modelo_stm, # modelo previamente entrenado
                                          documents = corpus_alineado$documents, # documentos nuevos
                                          newData = corpus_alineado$meta, # metadata de los documentos nuevos
                                          origData = out$meta # metadata del modelo entrenado
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
    mutate(theta = if_else(theta >= max(theta)/3, theta, NA)) |>
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
  return(datos_parte_topico)
}) |> 
  list_rbind()

resultados |> 
  select(-titulo)

resultados

labelTopics(modelo_stm)

# datos_prensa_topicos <- datos_prensa |> 
#   # sólo clasificados
#   filter(id %in% resultados$id) |> 
#   # agregar tópicos
#   left_join(resultados |> select(-titulo),
#             join_by(id)) |> 

resultados_recod <- resultados |> 
  # recodificar tópicos
  mutate(across(c(topico_1, topico_2, topico_3),
                ~case_match(.x,
                            "1" ~ "temblores, emergencias",
                            "2" ~ "salud",
                            "3" ~ "educación",
                            "4" ~ "elecciones",
                            "5" ~ "corrupción",
                            "6" ~ "nada",
                            "7" ~ "transporte, clima",
                            "8" ~ "economía",
                            "9" ~ "incendios",
                            "10" ~ "internacional",
                            "11" ~ "entretenimiento",
                            "12" ~ "delincuencia",
                            "13" ~ "inversión verde?",
                            "14" ~ "municipios",
                            "15" ~ "gobierno",
                            "16" ~ "legislativo",
                            "17" ~ "constitución",
                            "18" ~ "judicial",
                            "19" ~ "bonos")))


glimpse(datos_prensa_topicos)

# agregar a datos completos
datos_prensa_topicos <- datos_prensa |>
  # sólo clasificados
  filter(id %in% resultados$id) |>
  # agregar tópicos
  left_join(resultados_recod |> select(-titulo),
            join_by(id))

# buscar
datos_prensa_topicos |> 
  filter(topico_1 == "delincuencia") |> 
  filter(topico_theta_1 > 0.17) |> # umbral 
  slice_sample(n = 10) |> 
  select(titulo, topico_1, topico_2, topico_theta_1)



# guardar ----
arrow::write_parquet(resultados_recod |> select(-titulo),
                     "datos/prensa_topicos.parquet")


# datos_prensa_topicos
