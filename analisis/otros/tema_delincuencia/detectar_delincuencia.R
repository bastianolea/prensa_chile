library(dplyr)
library(purrr)
library(arrow)
library(ggplot2)
library(lubridate)
library(slider)
library(furrr)
library(future)

plan(multisession, workers = 8)


# cargar datos ----
if (!exists("datos_prensa")) datos_prensa <- read_parquet("datos/prensa_datos.parquet")

# guardar resultados unidos
clasificacion <- read_parquet("datos/prensa_llm_clasificar.parquet")
resumen <- read_parquet("datos/prensa_llm_resumen.parquet")


# preparar datos ----
datos_prensa_filt <- datos_prensa |> 
  filter(año >= 2018)
# slice_sample(n = 200000)

datos_prensa_filt_split <- datos_prensa_filt |> 
  mutate(grupos = (row_number()-1) %/% (n()/8)) |> # n grupos de igual cantidad de filas
  group_split(grupos)


# temas ----
palabras_delincuencia = c("homicid",
                          "asesin",
                          "hurt", "robo\\b",
                          "asalto|asalta",
                          "arma.*fuego", "pistola",
                          "secuestr",
                          "violaci|violad",
                          "delito", "delincuen",
                          "crimen|crimin",
                          "narco", "droga",
                          "saqueo|saquea",
                          "portonazo", "turba")

palabras_delincuencia_extra <- c(palabras_delincuencia,
                                 "víctim", "delito", "delincuen", "crimen|crimin")

palabras_homicidios = c("homicid",
                        "asesin", 
                        "balead", "acribill")

palabras_homicidios_extra <- c(palabras_homicidios,
                               "fallec", "muert", "víctim",
                               "arma.*fuego", "cuchill", "punzante")


# procesar ----

datos_detect <- future_map(datos_prensa_filt_split, \(datos_parte) {
  datos_parte |> 
    # # clasificación por LLM
    # mutate(clasificacion = id %in% clasificacion$id,
    #        clasificacion = ifelse(clasificacion, "policial", "otros")) |>  
    # # estos datos no están para todas las noticias
    # detectar tema a partir de palabras presentes 
    mutate(tema = case_when(
      str_detect(cuerpo_limpio, str_flatten(palabras_homicidios, collapse = "|")) ~ "homicidios",
      str_detect(cuerpo_limpio, str_flatten(palabras_delincuencia, collapse = "|")) ~ "delincuencia",
      .default = "otros")) |> 
    # cantidad de palabras presentes
    mutate(n_term = case_when(tema == "homicidios" ~ str_count(cuerpo, str_flatten(palabras_homicidios_extra, collapse = "|")),
                              tema == "delincuencia" ~ str_count(cuerpo, str_flatten(palabras_delincuencia_extra, collapse = "|"))))
})


# datos_detect |> 
#   list_rbind() |> 
#   filter(clasificacion == "policial" & tema == "delincuencia") |> 
#   slice_sample(n = 100)

# conteo de clasificaciones
datos_detect |> 
  list_rbind() |> 
  count(clasificacion, tema)

# obtener resúmenes de noticias sobre homicidios
datos_detect |> 
  list_rbind() |> 
  # filter(tema == "homicidios") |> 
  # filter(clasificacion == "policial" & tema == "homicidios") |> 
  filter(clasificacion == "policial" & tema == "delincuencia") |>
  filter(n_term >= 3) |> 
  left_join(resumen, join_by(id)) |> 
  filter(!is.na(resumen)) |> 
  slice_sample(n = 8) |> 
  select(id, resumen)

# revisar texto de noticias
datos_prensa |> 
  filter(id == "00ed98e3fb5776d55f6602790cd804b1") |> 
  pull(cuerpo) |> 
  str_extract_all(str_flatten(palabras_delincuencia, collapse = "|"))



# bases de conteo ----
noticias_delincuencia <- datos_detect |> 
  list_rbind() |> 
  # filter(clasificacion == "policial" & tema == "delincuencia") |> 
  filter(tema == "delincuencia") |> 
  filter(n_term >= 4) |> 
  select(fecha, titulo, año, id) |> 
  left_join(resumen |> select(id, resumen), join_by(id))

noticias_homicidios <- datos_detect |> 
  list_rbind() |> 
  # filter(clasificacion == "policial" & tema == "homicidios") |>
  filter(tema == "homicidios") |>
  filter(n_term > 3) |> 
  select(fecha, titulo, año, id) |> 
  left_join(resumen |> select(id, resumen), join_by(id))

# noticias_homicidios$id


# datos de casos ----

### datos homicidios ----
homicidios_consumados <- readxl::read_xlsx("otros/analisis/tema_delincuencia/BASE-VHC-2018-2024-OFICIAL.xlsx") |> 
  janitor::clean_names()
# https://prevenciondehomicidios.cl/#informes

homicidios_consumados_n <- homicidios_consumados |> 
  mutate(fecha = dmy(paste(15, mes, id_ano))) |> 
  group_by(fecha) |> 
  summarize(n = n())


### datos delincuencia ----
delincuencia_chile <- arrow::read_parquet("otros/analisis/tema_delincuencia/cead_delincuencia_chile.parquet")
# https://github.com/bastianolea/delincuencia_chile/blob/main/datos_procesados/cead_delincuencia_chile.parquet

delincuencia_chile_n <- delincuencia_chile |> 
  # delitos de mayor connotación sociañ
  filter(delito %in% c("Homicidios", "Hurtos", "Lesiones menos graves, graves o gravísimas",
                       "Violaciones", "Robo con violencia o intimidación", "Robo de objetos de o desde vehículo",
                       "Robo de vehículo motorizado", "Robo en lugar habitado", "Otros robos con fuerza", "Robo por sorpresa")) |>
  group_by(fecha) |> 
  summarize(n = sum(delito_n))



# gráficos ----

## noticias ----

### gráfico noticias homicidios ----
noticias_homicidios_conteo <- noticias_homicidios |> 
  group_by(fecha) |> 
  summarize(n = n()) |> 
  complete(fecha = seq.Date(min(noticias_homicidios$fecha), 
                            max(noticias_homicidios$fecha), by="days"),
           fill = list(n = 0)) |> 
  arrange(fecha) |> 
  mutate(n2 = slide_dbl(n, .before = 14, mean))

g_noticias_homicidios <- noticias_homicidios_conteo |> 
  ggplot() +
  aes(fecha, n2) +
  geom_line(lwd = .2) +
  geom_smooth(method = "lm", se = T, color = "black", alpha = 0.15, lwd = .7) +
  theme_linedraw() +
  labs(title = "noticias sobre homicidios",
       subtitle = "detección de conceptos en prensa escrita digital",
       y = "noticias diarias",
       caption = "fuente: web scraping de prensa digital") 

g_noticias_homicidios


### gráfico noticias delincuencia ----
noticias_delincuencia_conteo <- noticias_delincuencia |> 
  group_by(fecha) |> 
  summarize(n = n()) |> 
  complete(fecha = seq.Date(min(noticias_delincuencia$fecha), max(noticias_delincuencia$fecha), by="days"),
           fill = list(n = 0)) |> 
  arrange(fecha) |> 
  mutate(n2 = slide_dbl(n, .before = 14, mean))

g_noticias_delincuencia <- noticias_delincuencia_conteo |> 
  ggplot() +
  aes(fecha, n2) +
  geom_line(lwd = .2) +
  geom_smooth(method = "lm", se = T, color = "black", alpha = 0.15, lwd = .7) +
  theme_linedraw() +
  labs(title = "noticias sobre delincuencia",
       subtitle = "detección de conceptos en prensa escrita digital",
       y = "noticias diarias",
       caption = "fuente: web scraping de prensa digital")

g_noticias_delincuencia


## casos ----
### gráfico casos homicidios ----
g_casos_homicidios <- homicidios_consumados_n |> 
  ggplot() +
  aes(fecha, n) +
  geom_line(lwd = .3) +
  geom_smooth(method = "lm", se = T, color = "black", alpha = 0.15, lwd = .7) +
  theme_linedraw() +
  labs(y = "víctimas homicidios consumados",
       title = "casos de homicidios",
       subtitle = "homicidios consumados con validación interinstitucional",
       caption = "fuente: observatorio de homicidios, ministerio de sguridad pública")

g_casos_homicidios


###  gráfico casos delincuencia ----

g_casos_delincuencia <- delincuencia_chile_n |> 
  ggplot() +
  aes(fecha, n) +
  geom_line(lwd = .3) +
  geom_smooth(method = "lm", se = T, color = "black", alpha = 0.15, lwd = .7) +
  theme_linedraw() +
  labs(y = "casos policiales mensuales",
       title = "casos de delincuencia",
       subtitle = "casos policiales de mayor connotación social",
       caption = "fuente: centro de estudios y análisis del delito") +
  scale_y_continuous(labels = label_comma(big.mark = "."))



## unir gráficos ----
# g_noticias_delincuencia
# g_casos_delincuencia
# 
# g_noticias_homicidios
# g_casos_homicidios

library(patchwork)
library(scales)

g_casos_delincuencia + g_noticias_delincuencia &
  scale_x_date(date_breaks = "years", date_labels = "%Y") &
  labs(x = NULL)

g_casos_homicidios + g_noticias_homicidios &
  scale_x_date(date_breaks = "years", date_labels = "%Y") &
  labs(x = NULL)
