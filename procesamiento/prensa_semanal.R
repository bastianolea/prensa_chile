library(tidyverse)
source("funciones.R")

# locale -a
Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")
# if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# datos_prensa


# preparar datos ----
prensa_conteo_2 <- prensa_palabras_conteo |> 
  # agregar metadatos
  left_join(datos_prensa |> 
              select(id, fecha),
            by = "id")


# fechas ----
prensa_conteo_3 <- prensa_conteo_2 |> 
  # rango de fechas 
  filter(fecha >= today() - months(6)) |> 
  mutate(fecha = floor_date(fecha, unit = "week", week_start = 1),
         semana = week(fecha))


# conteo por semanas ----
palabras_semana <- prensa_conteo_3 |> 
  group_by(semana, fecha, palabra) |> 
  # conteo por semanas
  summarize(n = sum(n),
            fecha = min(fecha)) |> 
  # mínimo
  filter(n > 2)


# filtrar ----
palabras_semana_2 <- palabras_semana |> 
  # límite de fecha
  # filter(fecha >= today() - weeks(input$semanas)) |>
  # calcular frecuencia total de cada palabra
  group_by(palabra) |> 
  mutate(freq_total_palabra = sum(n)) |> 
  # calcular total de palabras por semana
  group_by(semana) |> 
  mutate(palabras_semana = sum(n)) |> 
  # sacar semanas chicas
  ungroup() |> 
  filter(palabras_semana > mean(palabras_semana)*.6) |> 
  # eliminar palabras chicas en contexto
  # ungroup() |> 
  # filter(freq_total_palabra > mean(freq_total_palabra)*input$frecuencia_total) |>
  # calcular porcentaje de palabra por semana
  group_by(semana) |> 
  mutate(p_palabra_semana = n/palabras_semana) |> 
  ungroup()


# guardar ----
arrow::write_parquet(palabras_semana_2, 
                     "apps/prensa_chile/palabras_semana.parquet")

rm(palabras_semana,
   palabras_semana_2,
   prensa_conteo_2,
   prensa_palabras_conteo,
   prensa_conteo_3)
invisible(gc())