# ejecutar todos los pasos de procesamiento, post scraping (prensa_obtener_datos.R)
library(future)
library(lubridate)
source("funciones.R")

# revisar que esté conectado el disco externo
stopifnot("disco externo desconectado" = file.exists("/Volumes/Externo/R/prensa/"))

# cantidad de textos a procesar con LLM
muestra_llm = 2600

# memoria por thread
plan(multisession, workers = 8)
options(future.globals.maxSize = 1.0 * 3e9)

inicio <- now()


# ajustar documentos nuevos al modelo de tópicos
source("analisis/stm_ajustar_topicos.R", echo = T)

# sentimiento de noticias usando modelos de lenguaje
source("procesamiento/prensa_llm_sentimiento.R", echo = T)
# output: prensa_llm_sentimiento.parquet

# resumen de noticias usando modelos de lenguaje
source("procesamiento/prensa_llm_resumen.R", echo = T)
# output: prensa_llm_resumen.parquet

# # tópico de noticias usando modelos de lenguaje
# source("procesamiento/prensa_llm_clasificar.R", echo = T)
# # output: prensa_llm_clasificar.parquet


# finalizar ----
final <- now()

tiempo = final - inicio
print(round(tiempo, 1))

beep()