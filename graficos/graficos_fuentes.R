library(dplyr)
library(lubridate)
library(ggplot2)

# cargar datos ----
# source("prensa_cargar_datos.R")

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")


# gráfico noticias por fuentes ----

# gráfico noticias por fuente y mes
datos_prensa_grafico <- datos_prensa |>
  filter(year(fecha) >= 2019) |> 
  mutate(fecha = floor_date(fecha, "month"),
         mes = month(fecha) |> as.integer())

datos_prensa_grafico |> 
  # group_by(fecha) |> 
  # mutate(fuente = forcats::fct_lump_prop(fuente, prop = 0.03, other_level = "otros")) |> 
  ggplot(aes(as.factor(mes), fill = fuente)) +
  # geom_bar(position = position_dodge2(preserve = "single")) +
  geom_bar(position = position_stack(), color = "white") +
  # scale_x_date(date_breaks = "months", date_labels = "%m", expand = c(0, 0), minor_breaks = NULL) +
  # scale_x_date(date_breaks = "months", labels = datos_prensa_grafico$mes, expand = c(0, 0), minor_breaks = NULL) +
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
        legend.margin = margin(t=15), 
        panel.spacing.x = unit(4, "mm"))

ggsave(glue::glue("graficos/datos_prensa_scraping_{today()}_a.png"), 
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

ggsave(glue::glue("graficos/datos_prensa_fuentes_2024_{today()}.png"), 
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
