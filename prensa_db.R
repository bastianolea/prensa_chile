source("funciones.R")

# usethis::edit_r_environ(scope = "project")
# agregar a .gitignore

library(DBI)
library(dplyr)

# conexión ----
db_con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres",
  host = Sys.getenv("db_host"),
  port = Sys.getenv("db_port"),
  user = Sys.getenv("db_user"),
  password = Sys.getenv("db_pass")
)


# crear tablas ----
library(arrow)


palabras_semana <- arrow::read_parquet("datos/app/palabras_semana.parquet")

dbWriteTable(conn = db_con, 
             name = "palabras_semana",
             palabras_semana, 
             indexes = list(c("semana", "fecha")),
             overwrite = TRUE)



palabras_semana_fuente <- read_parquet("datos/app/palabras_semana_fuente.parquet")

dbWriteTable(conn = db_con, 
             name = "palabras_semana_fuente",
             palabras_semana_fuente, 
             indexes = list(c("fuente", "semana", "fecha")),
             overwrite = TRUE)



correlacion <- read_parquet("datos/prensa_correlacion.parquet")

dbWriteTable(conn = db_con, 
             name = "correlacion",
             correlacion, 
             indexes = list(c("item1", "item2")),
             overwrite = TRUE)



correlacion_fuente <- read_parquet("datos/prensa_correlacion_fuente.parquet")

dbWriteTable(conn = db_con, 
             name = "correlacion_fuente",
             correlacion_fuente, 
             indexes = list(c("item1", "item2", "fuente")),
             overwrite = TRUE)



sentimiento <- read_parquet("datos/app/prensa_sentimiento.parquet")

dbWriteTable(conn = db_con, 
             name = "sentimiento",
             sentimiento, 
             indexes = list(c("fecha", "fuente")),
             overwrite = TRUE)



palabras_semana_topico <- read_parquet("datos/app/palabras_semana_topico.parquet")

dbWriteTable(conn = db_con, 
             name = "palabras_semana_topico",
             palabras_semana_topico, 
             indexes = list(c("semana", "fecha", "clasificacion")),
             overwrite = TRUE)


# otros ----
n_noticias <- readLines("datos/prensa_n_noticias.txt") |> 
  as.numeric() |> 
  format(scientific = FALSE, big.mark = ".", decimal.mark = ",")

n_palabras <- readLines("datos/prensa_n_palabras.txt") |> 
  as.numeric()

# crear tabla 
prensa_otros <- tibble(n_noticias = n_noticias,
                       n_palabras = n_palabras,
                       fecha = lubridate::today()
                       )

# guardar 
dbWriteTable(conn = db_con, 
             name = "prensa_otros",
             prensa_otros, 
             overwrite = TRUE)



# otros preprocesar ----
# procesar datos extra
palabras_posibles <- palabras_semana |> 
  group_by(palabra) |> 
  summarize(n = sum(n)) |> 
  collect() |> 
  arrange(desc(n)) |> 
  filter(n > 100) |> 
  pull(palabra)

dbWriteTable(conn = db_con, 
             name = "palabras_posibles",
             tibble(palabras_posibles), 
             overwrite = TRUE)


fuentes <- palabras_semana_fuente |> pull(fuente) |> unique() |> sort()

dbWriteTable(conn = db_con, 
             name = "fuentes",
             tibble(fuentes), 
             overwrite = TRUE)


topicos <- sentimiento |> 
  pull(clasificacion) |> unique() |> 
  na.exclude() |> 
  stringr::str_subset("Sin", negate = T) |> 
  sort()

dbWriteTable(conn = db_con, 
             name = "topicos",
             tibble(topicos), 
             overwrite = TRUE)


lista_semanas <- palabras_semana |> 
  distinct(fecha, semana) |> 
  arrange(desc(fecha)) |> 
  collect() |> 
  mutate(fecha_t = redactar_fecha(fecha),
         fecha_t = paste("Semana del", fecha_t)) |> 
  select(fecha_t, fecha) #|>
  # tibble::deframe()

dbWriteTable(conn = db_con, 
             name = "lista_semanas",
             lista_semanas, 
             overwrite = TRUE)




# probar ----

tbl(db_con, "correlacion_fuente")