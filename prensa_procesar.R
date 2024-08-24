# ejecutar todos los pasos de procesamiento, post scraping (prensa_obtener_datos.R)
# para producir 

# unión de scraping en una sola base de datos
source("procesamiento/prensa_p1_cargar_datos.R")
# output: datos/prensa_datos.feather

# tokenización de textos en palabras
source("procesamiento/prensa_p2_procesar_texto.R")
# output: datos/prensa_palabras.feather

# conteo de frecuencia de palabras
source("procesamiento/prensa_p3_calcular_conteo.R")
# output: datos/prensa_palabras_conteo.parquet