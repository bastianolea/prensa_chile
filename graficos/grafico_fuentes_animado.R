library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(tidyr)

Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

# cargar datos ----
# source("prensa_cargar_datos.R")

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")


# datos ----
# gráfico noticias por fuente y mes
datos_prensa_grafico_scraping <- datos_prensa |>
  select(fuente, fecha, año, fecha_scraping) |> 
  filter(año >= 2019) |>
  # filter(año == 2024) |>
  filter(!is.na(fecha_scraping)) |> 
  mutate(fecha = floor_date(fecha, "month"),
         fecha_scraping = floor_date(fecha_scraping, "month"),
         mes = month(fecha) |> as.integer() |> as.factor(),
         mes_scraping = month(fecha_scraping) |> as.integer())

# conteo inicial de noticias
datos_prensa_grafico_scraping_conteo <- datos_prensa_grafico_scraping |> 
  group_by(fecha, mes, año, mes_scraping, fecha_scraping, fuente) |> 
  count() #|> 
# # resumir fuentes chicas
# ungroup() |>
# mutate(fuente = forcats::fct_lump_prop(fuente, w = n, prop = 0.01, other_level = "otros")) |>
# group_by(fecha, mes, año, fuente) |>
# summarize(n = sum(n))


# relleno de fechas
datos_prensa_grafico_scraping_conteo_rellenado <- datos_prensa_grafico_scraping_conteo |> 
  group_by(fuente) |> 
  complete(fecha = seq.Date(min(datos_prensa_grafico_scraping$fecha), 
                            max(datos_prensa_grafico_scraping$fecha), 
                            by = "months"),
           fecha_scraping = seq.Date(min(datos_prensa_grafico_scraping$fecha_scraping), 
                                     max(datos_prensa_grafico_scraping$fecha_scraping), 
                                     by = "months")
  ) |> 
  mutate(año = year(fecha) |> as.factor(),
         mes = month(fecha) |> as.factor(),
         mes_scraping = month(fecha_scraping) |> as.factor()) |> 
  filter(fecha_scraping <= max(datos_prensa$fecha_scraping, na.rm=T))


# conteo de suma acumulada
datos_prensa_grafico_scraping_conteo_rellenado_sum <- datos_prensa_grafico_scraping_conteo_rellenado |> 
  mutate(n = replace_na(n, 0)) |> 
  group_by(fuente, fecha) |> 
  mutate(sum = cumsum(n)) |> 
  recodificar_fuentes()


# gráfico ----
plot <- datos_prensa_grafico_scraping_conteo_rellenado_sum |> 
  ggplot(aes(mes, sum, fill = fuente)) +
  geom_col(position = position_stack(), color = "white", linewidth = .2) +
  scale_y_continuous(expand = c(0, 0), labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  guides(fill = guide_legend(position = "bottom", nrow = 3, title = NULL)) +
  facet_grid(~año, space = "free_x", scales = "free_x", switch = "x") +
  scale_fill_viridis_d(begin = 0.1, end = 0.6, option = "plasma") +
  theme(strip.text = element_text(margin = margin(t=20)),
        axis.text.x = element_text(margin = margin(t=-25, b = 14)),
        plot.subtitle = element_text(margin = margin(t = 0)),
        plot.title = element_text(margin = margin(t = 6, b = 6)),
        plot.caption = element_text(margin = margin(t = 10)),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(4, "mm"),
        legend.text = element_text(margin = margin(l = 2, r = 4)),
        legend.margin = margin(t=15), 
        panel.spacing.x = unit(4, "mm")) +
  guides(fill = guide_none()) +
  labs(y = "Noticias por mes", x = NULL, 
       title = "Web scraping de medios digitales chilenos",
       subtitle = "Cantidad de noticias obtenidas hasta {format(as_date(closest_state), '%B de %Y')}",
       caption = "Elaboración propia. Bastián Olea Herrera")


# animación ----
anim <- plot +
  transition_states(fecha_scraping, 
                    transition_length = 2, state_length = 0.5,
                    wrap = FALSE) +
  enter_grow()
  # ggtitle()


# render ----
library(future)
plan(multicore, workers = 7)

animate(anim, 
        renderer = av_renderer(paste0("graficos/resultados/datos_prensa_scraping_", lubridate::today(), ".mov")),
        fps = 60, end_pause = 60, duration = 16, 
        width = 1080, height = 800, units = "px", res = 90)
