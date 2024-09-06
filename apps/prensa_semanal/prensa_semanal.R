library(tidyverse)

# locale -a
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

if (!exists("datos_prensa")) datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")
# if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

datos_prensa

# prensa_palabras_conteo <- prensa_palabras |> 
#   slice(1:2000000) |> 
#   group_by(id) |> 
#   count(palabra) |> 
#   filter(n > 3) |> 
#   arrange(id, desc(n))



# palabras irrelevantes ----
palabras_irrelevantes = c("chile", "publicar", 
                          "año", "añosa", "añosen",
                          "país", "persona", "comunicación"
)

# preparar datos ----
prensa_conteo_2 <- prensa_palabras_conteo |> 
  # palabras que se repitan n veces dentro de su noticia
  # filter(n > 2) |> 
  # excluir palabras
  filter(!palabra %in% palabras_irrelevantes) |> 
  # agregar metadatos
  left_join(datos_prensa |> 
              select(id, fuente, fecha, titulo),
            by = "id")

# fechas ----
prensa_conteo_3 <- prensa_conteo_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(6)) |> 
  mutate(semana = week(fecha))

# conteo ----
palabras_semana <- prensa_conteo_3 |> 
  group_by(semana, palabra) |> 
  # conteo por semanas
  summarize(n = sum(n),
            fecha = min(fecha)) |> 
  ungroup() |> 
  # mínimo
  filter(n > 10)

# guardar
arrow::write_parquet(palabras_semana, "apps/prensa_semanal/palabras_semana.parquet")
