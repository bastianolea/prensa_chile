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
# llm_use("ollama", "deepseek-r1:8b", .cache = "", temperature = 0)
llm_use("ollama", "llama3.1:8b", .cache = "", temperature = 0)
# llm_use("ollama", "llama3.2:3b", .cache = "", temperature = 0)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")

# cargar resultados anteriores
anterior <- read_parquet("datos/prensa_llm_clasificar.parquet")

# extraer muestra
if (!exists("muestra_llm")) muestra_llm = 2600 # definir cantidad de noticias a procesar

# estimar tiempo
estimar_tiempo(muestra_llm, 5.2)

# datos_muestra <- datos_prensa |> 
#   filter(año >= 2024) |> 
#   filter(fecha > (today() - weeks(6))) |> 
#   filter(!id %in% anterior$id) |> 
#   slice_sample(n = muestra)

# todas las noticias, partiendo por las más recientes
datos_muestra <- datos_prensa |>
  filter(año >= 2023) |> 
  select(id, bajada, cuerpo) |> 
  filter(!id %in% anterior$id) |> 
  slice(1:muestra_llm)

rm(datos_prensa)


# separar en piezas ----
# el dataframe se separa en una lista de igual cantidad de filas para facilitar su procesamiento multiprocesador
filas <- nrow(datos_muestra)
grupos <- filas %/% 500 # un grupo cada x observaciones

datos_muestra_split <- datos_muestra |> 
  mutate(grupos = (row_number()-1) %/% (n()/grupos)) |>
  group_by(grupos) |>
  group_split()


# limpiar ----
datos_limpios <- future_map(datos_muestra_split, 
                            \(datos) {
                              datos |> 
                                mutate(texto = paste(bajada, cuerpo),
                                       texto = textclean::strip(texto, digit.remove = FALSE, char.keep = c(".", ",")),
                                       texto = str_trunc(texto, 7000, side = "center")) |> 
                                mutate(n_palabras = str_count(texto, "\\w+"))
                            })

# separar por id
datos_limpios_split <- datos_limpios |> 
  list_rbind() |> 
  distinct(id, .keep_all = TRUE) |> 
  mutate(orden = row_number()) |> 
  group_by(orden) |>
  group_split()

categorias = c("política", "economía", "policial",
               "cultura", "farándula", "deporte", "ciencias", "tecnología"
)


# loop ----
message(paste("iniciando loop clasificación para", length(datos_limpios_split), "noticias"))

# obtener sentimientos de todos los textos
clasificacion <- map(datos_limpios_split, 
                     \(dato) {
                       inicio <- now()
                       message(paste("procesando", dato$id))
                       
                       tryCatch({
                         # detener operación externamente
                         if (detencion_manual()) return(NULL)
                         
                         # clasificar
                         clasificacion <- dato$texto |> llm_vec_classify(labels = categorias)
                         
                         if (is.na(clasificacion)) {
                           clasificacion <- dato$texto |> llm_vec_classify(labels = c(categorias, "otros"))
                           
                           if (is.na(clasificacion)) clasificacion <- "Sin interpretación"
                         }
                         final <- now()
                         
                         if (is.na(clasificacion)) return(NULL)
                         
                         mensaje_segundos(dato$n_palabras, final - inicio)
                         
                         # resultado
                         resultado <- tibble(id = dato$id, clasificacion,
                                             tiempo = final - inicio,
                                             tiempo_1 = inicio, tiempo_2 = final,
                                             n_palabras = dato$n_palabras
                         )
                         return(resultado)
                       },
                       error = function(e) {
                         warning(paste("error:", e))
                         return(NULL)
                       })
                     }); beep()

# tiempo total
clasificacion |> 
  list_rbind() |> 
  summarize(tiempo_total = sum(tiempo),
            tiempo_prom = mean(tiempo),
            n_noticias = n()) |> glimpse()

# clasificacion |> list_rbind() |>
#   left_join(datos_limpios |> list_rbind()) |>
#   select(titulo, clasificacion)


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
