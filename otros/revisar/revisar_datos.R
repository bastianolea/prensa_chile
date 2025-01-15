library(dplyr)
library(lubridate)
library(ggplot2)

# cargar datos ----
# source("prensa_cargar_datos.R")

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")

# revisar
cat("total de noticias únicas:", scales::comma(nrow(datos_prensa)))

# noticias por fuente ----
datos_prensa |>
  count(fuente) |>
  arrange(desc(n))

datos_prensa_semana <- datos_prensa |> 
  filter(fecha >= today() - days(7))

# noticias por fuente en la semana
datos_prensa_semana |> 
  count(fuente, sort = TRUE) |> 
  print(n=Inf)
# 
# # noticias por año ----
# datos_prensa |>
#     count(año) |>
#     arrange(desc(año))
# 
# # noticias por fuente ----
# datos_prensa |>
#   count(fuente) |>
#   arrange(desc(n))
# 
# # noticias por mes ----
# datos_prensa |>
#   mutate(fecha = floor_date(fecha, "month")) |>
#   count(fecha) |>
#   arrange(desc(fecha))
# 
# # noticias por año y por fuente ----
# datos_prensa |>
#   count(año, fuente) |>
#   arrange(fuente, desc(año)) |> 
#   print(n=Inf)
# 
# 
# #fuentes sin noticias en años ----
# conteo_años <- datos_prensa |> 
#   mutate(año = as.factor(año),
#          fuente = as.factor(fuente)) |> 
#   count(año, fuente, .drop = F) |> 
#   print(n=Inf)
# 
# conteo_años |> 
#   filter(año == 2020, n == 0)
# 
# conteo_años |> 
#   filter(as.numeric(as.character(año)) >= 2019) |> 
#   filter(!fuente %in% c("24horas", "chvnoticias", "lacuarta", 
#                         "biobio", "cooperativa", "diariofinanciero", "agricultura")) |> 
#   ggplot(aes(año, n, fill = fuente)) +
#   geom_col(color = "white")
# 
# # noticias sin fecha por fuente ----
# datos_prensa |>
#   filter(is.na(fecha)) |>
#   count(fuente)
# 
# # fechas minimas y maximas por fuente ----
# datos_prensa |> 
#   group_by(fuente) |> 
#   summarize(min = min(fecha, na.rm = T),
#             max = max(fecha, na.rm = T)) |> 
#   print(n=Inf)
# 
# max(datos_prensa$fecha, na.rm = T)
# 
# datos_prensa |> filter(is.na(fecha))
# 
# # fuentes con mas de 5 años de noticias ----
# datos_prensa |>
#   count(año, fuente) |>
#   arrange(fuente, desc(año)) |> 
#   group_by(fuente) |> 
#   mutate(n_años = n()) |> 
#   filter(n_años >= 6) |> 
#   pull(fuente) |> 
#   unique() |> dput()
# 
