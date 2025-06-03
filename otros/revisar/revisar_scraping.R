# ejecutar para obtener información sobre scrapings realizados con prensa_obtener_datos.R

library(fs)
library(dplyr)
library(lubridate)
library(purrr)

directorios <- dir_info("scraping/datos") |> 
  arrange(desc(modification_time))

# # directorios sin cambios hoy
# sin_cambios <- directorios |> 
#   filter(modification_time < today()) |> 
#   mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
#   select(fuente, size, modification_time)

sin_cambios <- map_df(directorios$path, ~{
  # .x <- directorios$path[5]
  directorio <- dir_info(.x)
  
  fuente <- stringr::str_extract(.x, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")
  
  archivos <- directorio |> 
    filter(modification_time >= today()) |>
    filter(size >= 1024)
  
  ultimo <- directorio |> 
    filter(size >= 1024) |> 
    slice_max(modification_time) |> 
    select(size, modification_time)
    
  if (nrow(archivos) == 0) {
    return(tibble(fuente,
                  ultimo))
  } else {
    return(tibble())
  }
})


# directorios con cambios hoy
con_cambios <- map(directorios$path, ~{
  directorio <- dir_info(.x)
  
  directorio |> 
    filter(modification_time >= today()) |> 
    mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
    select(fuente, size, modification_time)
}) |> 
  list_rbind()


cantidad_archivos <- map(directorios$path, ~dir_info(.x)) |> 
  list_rbind() |> 
  mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
  select(fuente, everything()) |> 
  summarize(n = n(), .by = fuente) |> 
  arrange(desc(n)) |> 
  print(n=Inf)



# directorios con cambios hoy
ultimos_cambios <- map(directorios$path, ~{
  # .x <- directorios$path[2]
  directorio <- dir_info(.x)
  
  directorio |> 
    slice_max(modification_time) |> 
    mutate(fuente = stringr::str_extract(path, "scraping/datos/\\w+") |> stringr::str_remove("scraping/datos/")) |> 
    select(fuente, size, modification_time)
}) |> 
  list_rbind() |> 
  arrange(modification_time)


cat("\ncantidad de archivos por fuente:\n"); print(cantidad_archivos, n = Inf)

cat("\nfuentes con datos guardados hoy:\n"); print(con_cambios, n = Inf)

cat("\nfuentes sin datos guardados hoy:\n"); print(sin_cambios, n = Inf)


ultimos_cambios |> print(n=Inf)


