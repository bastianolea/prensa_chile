# ejecutar todos los pasos de procesamiento, post scraping (prensa_obtener_datos.R)
library(future)
library(lubridate)
source("funciones.R")

# revisar que esté conectado el disco externo
stopifnot("disco externo desconectado" = file.exists("/Volumes/Externo/R/prensa/"))

# setup ----

# memoria por thread
plan(multisession, workers = 8)
options(future.globals.maxSize = 1.0 * 3e9)

inicio <- now()


# procesamiento ----

# unión de scraping en una sola base de datos
# cargar datos scrapeados, los limpia y los guarda en una sola base, una noticia por fila
source("procesamiento/prensa_p1_cargar_datos.R", echo = T)
# output: datos/prensa_datos.parquet
# 
# tokenización de textos en palabras
# transformar datos de prensa en base tokenizada por palabras para análisis de texto
source("procesamiento/prensa_p2_procesar_texto.R", echo = T)
# output: datos/prensa_palabras.parquet

# tokenización por bigramas
# source("procesamiento/prensa_p2b_procesar_bigramas.R", echo = T)
# output: datos/prensa_bigramas.parquet

# conteo de frecuencia de palabras por noticia
source("procesamiento/prensa_p3_calcular_conteo.R", echo = T)
# output: datos/prensa_palabras_conteo.parquet

# conteos para app de noticias semanales
source("procesamiento/prensa_semanal.R", echo = T)
source("procesamiento/prensa_semanal_fuente.R", echo = T)

# correlación entre palabras dentro de noticias, retorna base con palabras y sus pares correlacionados
source("procesamiento/prensa_correlacion.R", echo = T)
# output: datos/prensa_correlacion.parquet, datos/prensa_correlacion_fuente.parquet

# datos para análisis de sentimiento
source("procesamiento/prensa_semanal_sentimiento.R", echo = T)


# finalizar ----
final <- now()

tiempo = final - inicio
print(round(tiempo, 1))

beep()