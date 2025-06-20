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
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")

# cargar resultados anteriores
anterior <- read_parquet("datos/prensa_llm_sentimiento.parquet")

# extraer muestra
if (!exists("muestra_llm")) muestra_llm = 2600 # definir cantidad de noticias a procesar

# estimar tiempo
estimar_tiempo(muestra_llm, 4.9)

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
# se separa en una lista de igual cantidad de filas para facilitar su procesamiento multiprocesador

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


# loop ----
message(paste("iniciando loop sentimiento para", length(datos_limpios_split), "noticias"))

# obtener sentimientos de todos los textos
sentimientos <- map(datos_limpios_split, 
                    \(dato) {
                      inicio <- now()
                      message(paste("procesando", dato$id), appendLF = F)
                      
                      tryCatch({
                        # detener operación
                        if (detencion_manual()) return(NULL)
                        
                        # obtener sentimiento
                        sentimiento <- dato$texto |> llm_vec_sentiment(options = c("positivo", "neutral", "negativo"))
                        
                        # reintentar 1 vez
                        if (is.na(sentimiento)) {
                          message(" ...reintentando...")
                          sentimiento <- dato$texto |> llm_vec_sentiment(options = c("positivo", "neutral", "negativo"))
                          
                          if (is.na(sentimiento)) sentimiento <- "Sin interpretación"
                        }
                        final <- now()
                        
                        if (is.na(sentimiento)) return(NULL)
                        
                        mensaje_segundos(dato$n_palabras, final - inicio)
                        
                        # resultado
                        resultado <- tibble(id = dato$id, sentimiento,
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
sentimientos |> 
  list_rbind() |> 
  summarize(tiempo_total = sum(tiempo),
            tiempo_promedio = mean(tiempo),
            n_noticias = n()) |> glimpse()



# guardar avance ----
sentimientos |> 
  list_rbind() |> 
  write_parquet(paste0("datos/prensa_llm/sentimiento/prensa_llm_sentimiento_", sample(111:999, 1), "_", today(), ".parquet"))


# unir resultados ----
resultados <- map(dir("datos/prensa_llm/sentimiento/", full.names = T),
                  read_parquet) |> 
  list_rbind()


# guardar resultados unidos
resultados |> write_parquet("datos/prensa_llm_sentimiento.parquet")
