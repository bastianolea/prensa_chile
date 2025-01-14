library(arrow)
library(readr)

tictoc::tic()
palabras_semana <- read_parquet("apps/prensa_semanal/palabras_semana.parquet")
palabras_semana_fuente <- read_parquet("apps/prensa_semanal/palabras_semana_fuente.parquet")
correlacion <- read_parquet("apps/prensa_semanal/prensa_correlacion.parquet")
correlacion_fuente <- read_parquet("apps/prensa_semanal/prensa_correlacion_fuente.parquet")
tictoc::toc()

write_rds(palabras_semana, "apps/prensa_semanal/palabras_semana.rds")
write_rds(palabras_semana_fuente, "apps/prensa_semanal/palabras_semana_fuente.rds")
write_rds(correlacion, "apps/prensa_semanal/prensa_correlacion.rds")
write_rds(correlacion_fuente, "apps/prensa_semanal/prensa_correlacion_fuente.rds")


tictoc::tic()
palabras_semana <- read_rds("apps/prensa_semanal/palabras_semana.rds")
palabras_semana_fuente <- read_rds("apps/prensa_semanal/palabras_semana_fuente.rds")
correlacion <- read_rds("apps/prensa_semanal/prensa_correlacion.rds")
correlacion_fuente <- read_rds("apps/prensa_semanal/prensa_correlacion_fuente.rds")
tictoc::toc()
