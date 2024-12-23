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
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")

# cargar resultados anteriores
anterior <- read_parquet("datos/prensa_llm_resumen.parquet")

# extraer muestra
muestra = 100 # definir cantidad de noticias a procesar

datos_muestra <- datos_prensa |> 
  filter(año >= 2024) |> 
  filter(fecha > (today() - weeks(2))) |> 
  filter(!id %in% anterior$id) |> 
  slice_sample(n = muestra)

# estimar tiempo
message(paste("tiempo aproximado de procesamiento:", round((muestra * 8)/60/60, 1), "horas"))


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
                                               texto = str_trunc(texto, 5000, side = "center")) |> 
                                        mutate(n_palabras = str_count(texto, "\\w+"))
                                    })

# separar por id
datos_limpios_split <- datos_limpios |> 
  list_rbind() |> 
  group_by(id) |>
  group_split()


# loop ----
# obtener sentimientos de todos los textos
resumenes <- map(datos_limpios_split, 
                 \(dato) {
                   inicio <- now()
                   message(paste("procesando", dato$id))
                   
                   # obtener sentimiento
                   resumen <- dato$texto |> llm_vec_summarize(max_words = 30, additional_prompt = "en español")
                   
                   # reintentar 1 vez
                   if (is.na(resumen)) {
                     resumen <- dato$texto |> llm_vec_summarize(max_words = 30, additional_prompt = "en español")
                   }
                   final <- now()
                   
                   if (is.na(resumen)) return(NULL)
                   
                   # resultado
                   resultado <- tibble(id = dato$id,
                                       resumen,
                                       tiempo = final - inicio,
                                       tiempo_1 = inicio, tiempo_2 = final,
                                       n_palabras = dato$n_palabras
                   )
                   
                   return(resultado)
                 }); beep()

# tiempo total
resumenes |> 
  list_rbind() |> 
  summarize(tiempo_total = sum(tiempo),
            tiempo_prom = mean(tiempo))

resumenes |> 
  list_rbind() |> 
  slice_sample(n = 10) |> 
  pull(resumen)


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
