# ejecutar para obtener información sobre scrapings realizados con prensa_obtener_datos.R

library(fs)
library(dplyr)
library(lubridate)
library(purrr)

directorios <- dir_info("resultados") |> 
  arrange(desc(modification_time))

# directorios con cambios hoy
con_cambios <- directorios |> 
  filter(modification_time >= today())

# directorios sin cambios hoy
sin_cambios <- directorios |> 
  filter(modification_time < today())

# datos guardados hoy
map(directorios$path, ~{
  # directorio <- directorios |>
  #   slice(1) |>
  #   pull(path) |>
  #   dir_info()
  
  directorio <- dir_info(.x)
  
  directorio |> 
    filter(modification_time >= today()) |> 
    mutate(fuente = stringr::str_extract(path, "resultados/\\w+") |> stringr::str_remove("resultados/")) |> 
    select(fuente, size, modification_time)
}) |> 
  list_rbind()


# fuentes sin guardados hoy
map(directorios$path, ~{
  directorio <- dir_info(.x)
  
  datos <- directorio |> 
    filter(modification_time >= today())
  
  if (nrow(datos) == 0) {
    
    fuente <- stringr::str_extract(.x, "resultados/\\w+") |> stringr::str_remove("resultados/")
    tibble(fuente, "fecha" = today())
  } else {
    return(NULL)
  }
  
}) |> 
  list_rbind()

