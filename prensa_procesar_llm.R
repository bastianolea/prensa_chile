# sentimiento de noticias usando modelos de lenguaje
source("procesamiento/prensa_llm_sentimiento.R", echo = T)
# output: prensa_llm_sentimiento.parquet

# tópico de noticias usando modelos de lenguaje
source("procesamiento/prensa_llm_clasificar.R", echo = T)
# output: prensa_llm_clasificar.parquet

# resumen de noticias usando modelos de lenguaje
# source("procesamiento/prensa_llm_resumen.R", echo = T)
# output: prensa_llm_resumen.parquet