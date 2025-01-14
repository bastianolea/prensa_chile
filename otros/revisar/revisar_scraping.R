# ejecutar para obtener información sobre scrapings realizados con prensa_obtener_datos.R

library(fs)
library(dplyr)
library(lubridate)
library(purrr)

directorios <- dir_info("scraping/datos") |> 
  arrange(desc(modification_time))

# directorios sin cambios hoy
sin_cambios <- directorios |> 
  filter(modification_time < today()) |> 
  mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
  select(fuente, size, modification_time)

# directorios con cambios hoy
con_cambios <- map(directorios$path, ~{
  directorio <- dir_info(.x)
  
  directorio |> 
    filter(modification_time >= today()) |> 
    mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
    select(fuente, size, modification_time)
}) |> 
  list_rbind()


map(directorios$path, ~dir_info(.x)) |> 
  list_rbind() |> 
  mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
  select(fuente, everything()) |> 
  summarize(n = n(), .by = fuente) |> 
  arrange(desc(n)) |> 
  print(n=Inf)

cat("\nfuentes con datos guardados hoy:\n"); print(con_cambios, n = Inf)

cat("\nfuentes sin datos guardados hoy:\n"); print(sin_cambios, n = Inf)





