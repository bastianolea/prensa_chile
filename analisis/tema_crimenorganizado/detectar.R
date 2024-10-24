library(dplyr)
library(lubridate)
library(arrow)

# cargar datos ----
if (!exists("prensa_palabras")) prensa_palabras <- read_parquet("datos/prensa_palabras.parquet") |> collect()
if (!exists("prensa_bigramas")) prensa_bigramas <- read_parquet("datos/prensa_bigramas.parquet")

# definir conceptos para temas ----
comunas <- readr::read_csv2("datos/comunas_chile_cut.csv")

lista_comunas <- c(comunas$comuna, unique(comunas$region))

# en base a correlaciones
bigramas_crimenorganizado = c("tren aragua", "crimen organizado", 
                              "criminales organizados", "banda delincuentes",
                              "banda delictual", "organización delictual", "organización ilícita")

palabras_crimenorganizado = c("aragua", "organizado")

length(unique(prensa_palabras$id))

prensa_bigramas |> 
  slice(1:1e5) |> 
  group_by(id) |> 
  mutate(noticia_ubicacion = palabra %in% tolower(lista_comunas)) |> 
  print(n = 1000)

# detectar localizaciones ----
prensa_bigramas_2 <- prensa_bigramas |> 
  # slice(1:1e6) |> 
  mutate(ubicacion = palabra %in% tolower(lista_comunas)) |> # true para palabras que son ubicaciones 
  group_by(id) |> 
  mutate(noticia_ubicacion = any(ubicacion)) |> 
  filter(noticia_ubicacion) |> 
  ungroup()


# detectar tema ----
prensa_bigramas_3 <- prensa_bigramas_2 |> 
  mutate(crimen = bigrama %in% tolower(bigramas_crimenorganizado)) |>
  group_by(id) |> 
  mutate(noticia_crimen = any(crimen)) |> 
  filter(noticia_crimen) |> 
  ungroup()


length(unique(prensa_bigramas_3$id))

library(tidyr)

# prensa_bigramas_4 <- prensa_bigramas_3 |> 
#   slice(1:1e6) |>
#   # adjuntar información de ubicación
#   left_join(comunas |> 
#               mutate(palabra = tolower(comuna)), 
#             by = join_by(palabra)) |> 
#   # propagar comuna a toda la noticia
#   group_by(id) |> 
#   fill(names(comunas), .direction = "updown")

prensa_bigramas_4 <- prensa_bigramas_3 |> 
  filter(ubicacion) |> 
  distinct(id, ubicacion, .keep_all = TRUE) |> 
  mutate(ubicacion = palabra) |> 
  select(id, ubicacion, noticia_crimen)

prensa_bigramas_5 <- prensa_bigramas_4 |> 
  count(ubicacion, sort = TRUE) |> 
  left_join(comunas |>
              mutate(ubicacion = tolower(comuna)),
            by = join_by(ubicacion))
  
  
prensa_bigramas_5b <- prensa_bigramas_5 |> 
  filter(is.na(cut_comuna)) |> 
  select(1, 2) |> 
  left_join(comunas |>
              distinct(region, .keep_all = TRUE) |> 
              mutate(ubicacion = tolower(region)),
            by = join_by(ubicacion))

prensa_bigramas_6 <- prensa_bigramas_5 |> 
  filter(!is.na(cut_comuna)) |> 
  bind_rows(prensa_bigramas_5b) |> 
  arrange(desc(n))

library(ggplot2)
library(forcats)

prensa_bigramas_6 |> 
  mutate(comuna = fct_lump_n(comuna, w = n, 25, other_level = "Otras")) |> 
  group_by(comuna, region) |> 
  summarize(n = sum(n)) |> 
  ungroup() |> 
  mutate(comuna = fct_reorder(comuna, n),
         comuna = fct_relevel(comuna, "Otras", after = 0)) |> 
  group_by(region) |> 
  mutate(n_region = sum(n)) |> 
  ungroup() |> 
  mutate(region = fct_reorder(region, n_region, .desc = T)) |> 
  ggplot(aes(n, y = comuna, fill = region)) +
  geom_col(width = .6) +
  geom_text(aes(label = ifelse(comuna != "Otras", n, "")), hjust = 0, nudge_x = 50) +
  scale_x_continuous(expand = expansion(c(0, 0.25))) +
  theme_minimal() +
  guides(fill = guide_legend(position = "right")) +
  labs(title = "Conteo de menciones de ubicaciones \nen noticias sobre crimen organizado",
       x = "Menciones de ubicaciones en noticias",
       y = "Comunas mencionadas en noticias",
       fill = "Regiones")
