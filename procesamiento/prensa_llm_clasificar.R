library(dplyr)
library(purrr)
library(furrr)
library(stringr)
library(mall)
library(lubridate)
library(arrow)
library(beepr)

source("funciones.R")

# configurar multiprocesador
plan(multisession, workers = 7)

# configurar LLM
llm_use("ollama", "llama3.1:8b", 
        .cache = "", temperature = 0)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")

# cargar resultados anteriores
anterior <- read_parquet("datos/prensa_llm_clasificar.parquet")

# extraer muestra
muestra = 2000

datos_muestra <- datos_prensa |> 
  filter(año >= 2024) |> 
  filter(!id %in% anterior$id) |> 
  slice_sample(n = muestra)

# estimar tiempo
message(paste("tiempo aproximado de procesamiento:", round((muestra * 5)/60/60, 1), "horas"))


# separar en piezas ----
# el dataframe se separa en una lista de igual cantidad de filas para facilitar su procesamiento multiprocesador
filas <- nrow(datos_muestra)
grupos <- filas %/% 100 #un grupo cada 10000 observaciones

datos_muestra_split <- datos_muestra |> 
  mutate(grupos = (row_number()-1) %/% (n()/grupos)) |>
  group_by(grupos) |>
  group_split()


# limpiar ----
datos_limpios <- future_map(datos_muestra_split, 
                                    \(datos) {
                                      datos |> 
                                        select(id, titulo, bajada, cuerpo) |> 
                                        mutate(texto = paste(bajada, cuerpo),
                                               texto = textclean::strip(texto, digit.remove = FALSE, char.keep = c(".", ",")),
                                               texto = str_trunc(texto, 5000, side = "center")) |> 
                                        mutate(n_palabras = str_count(texto, "\\w+"))
                                    })

# separar por id
datos_limpios_split <- datos_limpios |> 
  list_rbind() |> 
  group_by(id) |>
  group_split()

categorias = c("política", "economía", "policial",
               "cultura", "farándula", "deporte", "ciencias", "tecnología"
               )


# loop ----
# obtener sentimientos de todos los textos
clasificacion <- map(datos_limpios_split, 
                     \(dato) {
                       inicio <- now()
                       message(paste("procesando", dato$id))
                       
                       # clasificar
                       clasificacion <- dato$texto |> llm_vec_classify(labels = categorias)
                       
                       # reintentar 1 vez
                       if (is.na(clasificacion)) {
                         clasificacion <- dato$texto |> llm_vec_classify(labels = categorias)
                       }
                       final <- now()
                       
                       if (is.na(clasificacion)) return(NULL)
                       
                       # resultado
                       resultado <- tibble(id = dato$id,
                                           clasificacion,
                                           tiempo = final - inicio,
                                           n_palabras = dato$n_palabras
                       )
                       
                       return(resultado)
                     }); beep()

# tiempo total
clasificacion |> 
  list_rbind() |> 
  summarize(tiempo_total = sum(tiempo),
            tiempo_prom = mean(tiempo))

clasificacion |> list_rbind() |>
  left_join(datos_limpios |> list_rbind()) |>
  select(titulo, clasificacion)


# guardar avance ----
clasificacion |> 
  list_rbind() |> 
  write_parquet(paste0("datos/prensa_llm/clasificar/prensa_llm_clasificar_", sample(111:999, 1), "_", today(), ".parquet"))


# unir resultados ----
resultados <- map(dir("datos/prensa_llm/clasificar/", full.names = T),
                  read_parquet) |> 
  list_rbind()

# guardar resultados unidos
resultados |> write_parquet("datos/prensa_llm_clasificar.parquet")
