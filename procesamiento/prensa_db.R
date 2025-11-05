
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


palabras_semana <- arrow::read_parquet("apps/prensa_chile/palabras_semana.parquet")

dbWriteTable(conn = db_con, 
             name = "palabras_semana",
             palabras_semana, 
             indexes = list(c("semana", "fecha")),
             overwrite = TRUE)



palabras_semana_fuente <- read_parquet("apps/prensa_chile/palabras_semana_fuente.parquet")

dbWriteTable(conn = db_con, 
             name = "palabras_semana_fuente",
             palabras_semana_fuente, 
             indexes = list(c("fuente", "semana", "fecha")),
             overwrite = TRUE)



correlacion <- read_parquet("apps/prensa_chile/prensa_correlacion.parquet")

dbWriteTable(conn = db_con, 
             name = "correlacion",
             correlacion, 
             indexes = list(c("item1", "item2")),
             overwrite = TRUE)



correlacion_fuente <- read_parquet("apps/prensa_chile/prensa_correlacion_fuente.parquet")

dbWriteTable(conn = db_con, 
             name = "correlacion_fuente",
             correlacion_fuente, 
             indexes = list(c("item1", "item2", "fuente")),
             overwrite = TRUE)



sentimiento <- read_parquet("apps/prensa_chile/prensa_sentimiento.parquet")

dbWriteTable(conn = db_con, 
             name = "sentimiento",
             sentimiento, 
             indexes = list(c("fecha", "fuente")),
             overwrite = TRUE)



palabras_semana_topico <- read_parquet("apps/prensa_chile/palabras_semana_topico.parquet")

dbWriteTable(conn = db_con, 
             name = "palabras_semana_topico",
             palabras_semana_topico, 
             indexes = list(c("semana", "fecha", "clasificacion")),
             overwrite = TRUE)


# probar ----

tbl(db_con, "correlacion_fuente")