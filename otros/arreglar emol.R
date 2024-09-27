library(fs)
library(readr)

temp <- fs::dir_info("resultados/emol") |> 
  arrange(desc(modification_time)) |> 
  slice(2:500)

# archivos <- fs::dir_ls("resultados/emol")
archivos <- temp$path

datos <- map(archivos, read_rds)

datos |> 
  list_flatten() |> 
  # map(~mutate(.x, fecha_scraping = as.character(fecha_scraping))) |> 
  list_rbind() |> 
  distinct(url, .keep_all = TRUE)
