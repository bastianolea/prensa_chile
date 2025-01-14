library(dplyr)
library(arrow)

# cargar datos ----
if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")

# revisar
cat("total de palabras:", scales::comma(length(prensa_palabras$palabra)))

cat("total de palabras únicas:", scales::comma(length(unique(prensa_palabras$palabra))))
