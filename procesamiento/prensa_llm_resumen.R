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
llm_use("ollama", "llama3.1:8b", .cache = "", temperature = 0.5)
# llm_use("ollama", "qwen2.5:14b", .cache = "", temperature = 0.5)

# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")

# cargar resultados anteriores
anterior <- read_parquet("datos/prensa_llm_resumen.parquet")

# extraer muestra
if (!exists("muestra_llm")) muestra_llm = 3000 # definir cantidad de noticias a procesar

# estimar tiempo
estimar_tiempo(muestra_llm, 8.1)

datos_muestra <- datos_prensa |> 
  filter(año >= 2024) |> 
  filter(fecha > (today() - weeks(6))) |> 
  filter(!id %in% anterior$id) |> 
  slice_sample(n = muestra_llm)


# separar en piezas ----
# se separa en una lista de igual cantidad de filas para facilitar su procesamiento multiprocesador

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
                                select(id, bajada, cuerpo) |> 
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
# obtener sentimientos de todos los textos
resumenes <- map(datos_limpios_split, 
                 \(dato) {
                   inicio <- now()
                   message(paste("procesando", dato$id))
                   
                   tryCatch({
                     # detener operación externamente
                     detencion_manual()
                     
                     # obtener sentimiento
                     resumen <- dato$texto |> llm_vec_summarize(max_words = 30, additional_prompt = "en español")
                     
                     # reintentar 1 vez
                     if (is.na(resumen)) {
                       resumen <- dato$texto |> llm_vec_summarize(max_words = 30, additional_prompt = "en español")
                     }
                     final <- now()
                     
                     if (is.na(resumen)) return(NULL)
                     
                     mensaje_segundos(final - inicio)
                     
                     # resultado
                     resultado <- tibble(id = dato$id,
                                         resumen,
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
resumenes |> 
  list_rbind() |> 
  summarize(tiempo_total = sum(tiempo),
            tiempo_prom = mean(tiempo))

# resumenes_b <- resumenes |>
#   list_rbind() |>
#   # slice_sample(n = 10) |>
#   pull(resumen)
# 
# resumenes_a #llama 3.2:3b
# resumenes_b #qwen2.5:14b

# guardar avance ----
resumenes |> 
  list_rbind() |> 
  write_parquet(paste0("datos/prensa_llm/resumen/prensa_llm_resumen_", sample(111:999, 1), "_", today(), ".parquet"))


# unir resultados ----
resultados <- map(dir("datos/prensa_llm/resumen/", full.names = T),
                  read_parquet) |> 
  list_rbind()

# guardar resultados unidos
resultados |> write_parquet("datos/prensa_llm_resumen.parquet")
