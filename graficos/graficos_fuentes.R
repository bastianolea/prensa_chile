library(dplyr)
library(lubridate)
library(ggplot2)

Sys.setlocale("LC_TIME", "es_ES.UTF-8") # meses en español

# cargar datos ----
# source("prensa_cargar_datos.R")

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")


# gráfico noticias por fuentes ----

# gráfico noticias por fuente y mes
datos_prensa_grafico <- datos_prensa |>
  filter(año >= 2019) |> 
  mutate(fecha = floor_date(fecha, "month"),
         mes = month(fecha) |> as.integer() |> as.factor())

# conteo por fuente y mes
datos_prensa_grafico_conteo <- datos_prensa_grafico |> 
  group_by(fecha, mes, año, fuente) |> 
  count() |> 
  # resumir fuentes chicas
  ungroup() |>
  mutate(fuente = forcats::fct_lump_prop(fuente, w = n, prop = 0.01, other_level = "otros")) |>
  group_by(fecha, mes, año, fuente) |>
  summarize(n = sum(n))

# gráfico
datos_prensa_grafico_conteo |> 
  ggplot(aes(mes, n, fill = fuente)) +
  geom_col(position = position_stack(), color = "white") +
  scale_y_continuous(expand = c(0, 0), labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  guides(fill = guide_legend(position = "bottom", nrow = 3, title = NULL)) +
  labs(y = "noticias", x = NULL, 
       title = "Noticias en medios digitales chilenos",
       subtitle = "Cantidad total de noticias obtenidas por mes, según fuente",
       caption = "Fuente: elaboración propia. Bastián Olea Herrera") +
  facet_grid(~año, space = "free_x", scales = "free_x", switch = "x") +
  theme(strip.text = element_text(margin = margin(t=20)),
        axis.text.x = element_text(margin = margin(t=-25)),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(4, "mm"),
        legend.text = element_text(margin = margin(l = 2, r = 4)),
        legend.margin = margin(t=15), 
        panel.spacing.x = unit(4, "mm"))

ggsave(glue::glue("graficos/resultados/datos_prensa_scraping_{today()}.png"), 
       width = 11, height = 8, bg = "white")


# gráfico noticias mensuales, por fuentes y por año ----
datos_prensa_grafico |> 
  filter(año == 2022) |> 
  ggplot(aes(fecha, fill = fuente)) +
  geom_bar() +
  scale_x_date(date_breaks = "months", date_labels = "%m", expand = c(0, 0), minor_breaks = NULL) +
  facet_wrap(~fuente, ncol = 3, axes = "all_x") +
  guides(fill = guide_none()) +
  theme_minimal() +
  theme(axis.text = element_text(size = 7),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(panel.spacing.x = unit(0.4, "cm")) +
  labs(title = "Noticias mensuales, por fuente, 2024",
       caption = "Fuente: elaboración propia. Bastián Olea Herrera")

ggsave(glue::glue("graficos/resultados/datos_prensa_fuentes_2024_{today()}.png"), 
       width = 11, height = 8, bg = "white")



conteo_prensa_años <- datos_prensa |>
  filter(year(fecha) >= 2019) |> 
  group_by(fuente, año) |> 
  summarize(n = n()) |> 
  group_by(fuente) |> 
  mutate(total = sum(n))

conteo_prensa_años |> 
  filter(total >= 8000) |>
  # filter(total < 10000) |>
  ggplot(aes(año, n, color = fuente)) +
  geom_line(linewidth = 1.5, alpha = .3) +
  geom_point(size = 4, alpha = .8) +
  guides(color = guide_legend(position = "bottom")) +
  theme_minimal()







# —----

# versión animada ----
# install.packages("gganimate")
library(gganimate)
library(tidyr)

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
  mutate(sum = cumsum(n))


plot <- datos_prensa_grafico_scraping_conteo_rellenado_sum |> 
  ggplot(aes(mes, sum, fill = fuente)) +
  geom_col(position = position_stack(), color = "white") +
  scale_y_continuous(expand = c(0, 0), labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  guides(fill = guide_legend(position = "bottom", nrow = 3, title = NULL)) +
  facet_grid(~año, space = "free_x", scales = "free_x", switch = "x") +
  theme(strip.text = element_text(margin = margin(t=20)),
        axis.text.x = element_text(margin = margin(t=-25)),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(4, "mm"),
        legend.text = element_text(margin = margin(l = 2, r = 4)),
        legend.margin = margin(t=15), 
        panel.spacing.x = unit(4, "mm"))

# animación
anim <- plot +
  transition_states(fecha_scraping, 
                    transition_length = 3, state_length = 1, 
                    wrap = FALSE) +
  enter_grow() +
  ggtitle(label = "Web scraping de medios digitales chilenos",
          subtitle = "Cantidad total de noticias obtenidas 
          hasta {format(as_date(closest_state), '%B de %Y')}") +
  labs(y = "noticias", x = NULL, 
       caption = "Fuente: elaboración propia. Bastián Olea Herrera")

# render
animate(anim, 
        renderer = av_renderer("graficos/resultados/datos_prensa_scraping.mov"),
        fps = 60, end_pause = 30, duration = 16, 
        width = 1080, height = 800, units = "px", res = 90)


