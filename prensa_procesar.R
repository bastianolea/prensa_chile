# ejecutar todos los pasos de procesamiento, post scraping (prensa_obtener_datos.R)

inicio <- Sys.time()

# unión de scraping en una sola base de datos
# cargar datos scrapeados y guardarlos en una sola base, una noticia por fila
source("procesamiento/prensa_p1_cargar_datos.R", echo = TRUE)
# output: datos/prensa_datos.parquet

# tokenización de textos en palabras
# transformar datos de prensa en base tokenizada por palabras para análisis de texto
source("procesamiento/prensa_p2_procesar_texto.R", echo = TRUE)
# output: datos/prensa_palabras.parquet

# tokenización por bigramas
# source("procesamiento/prensa_p2b_procesar_bigramas.R", echo = TRUE)
# output: datos/prensa_bigramas.parquet

# conteo de frecuencia de palabras por noticia
source("procesamiento/prensa_p3_calcular_conteo.R", echo = TRUE)
# output: datos/prensa_palabras_conteo.parquet

# conteos para app de noticias semanales
source("apps/prensa_semanal/prensa_semanal.R", echo = TRUE)
source("apps/prensa_semanal/prensa_semanal_fuente.R", echo = TRUE)

# correlación entre palabras dentro de noticias, retorna base con palabras y sus pares correlacionados
source("procesamiento/prensa_p4_calcular_correlacion.R", echo = TRUE)
# output: datos/prensa_correlacion.parquet, datos/prensa_correlacion_fuente.parquet

# procesamiento de noticias para identificar topicos mediante machine learning
# source("analisis/prensa_calcular_topicos.R")

final <- Sys.time()

tiempo = final - inicio
tiempo
# 13 minutos en total

beep_n()
