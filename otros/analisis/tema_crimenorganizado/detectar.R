library(dplyr)
library(lubridate)
library(arrow)
library(tidytext)
library(stringr)

# cargar datos ----
if (!exists("prensa_bigramas")) prensa_bigramas <- read_parquet("datos/prensa_bigramas.parquet")

# definir conceptos para temas ----
comunas <- readr::read_csv2("datos/comunas_chile_cut.csv")

lista_comunas <- c(comunas$comuna, unique(comunas$region))

# lista de comunas para match
lista_comunas_match <- tibble(comunas = lista_comunas) |> 
  mutate(palabras = corpus::text_tokens(comunas, 
                                        corpus::text_filter(drop = c(stopwords::stopwords("es"), "san"))
  )) |> 
  rowwise() |> 
  mutate(n_palabras = length(palabras)) |> 
  mutate(comunas_1p = palabras |> unlist() |> tolower() |> first(),
         comunas_1p = if_else(nchar(comunas_1p) > 4, comunas_1p, NA)) |> 
  mutate(comunas_2p = if_else(n_palabras == 2,
                              unlist(palabras) |> paste(collapse = " "),
                              NA)) |> 
  mutate(comunas_xp = if_else(n_palabras > 2,
                              purrr::keep(palabras, ~nchar(.x) > 2) |> list(),
                              NA),
         comunas_xp = if_else(length(comunas_xp) > 2,
                              comunas_xp[1:2] |> list(),
                              comunas_xp |> list()),
         comunas_xp = if_else(length(comunas_xp) > 0,
                              comunas_xp |> paste(collapse = " "),
                              NA)
         
  ) |> 
  mutate(comunas_2p = if_else(is.na(comunas_2p) & !is.na(comunas_xp), comunas_xp, comunas_2p)) |> 
  mutate(comunas_1p = if_else(is.na(comunas_1p) & is.na(comunas_2p) & is.na(comunas_xp), unlist(palabras)[1], comunas_1p))


# en base a correlaciones
bigramas_crimenorganizado = c("tren aragua", "crimen organizado", 
                              "criminales organizados", "banda delincuentes",
                              "banda delictual", "organización delictual", "organización ilícita")

organizaciones <- c("Tren de Aragua", 
                    "Los Gallegos",
                    "Tren del Coro", 
                    "Los Pulpos", 
                    "Los Valencianos", 
                    "Pills King", 
                    "Los Espartanos", 
                    "Los Restrojos", 
                    "Los Orquídea Blanca",
                    "Nueva Portugal", 
                    "Laberinto", 
                    "Mara Salvatrucha", 
                    "Primeiro Comando", # "Primeiro Comando da Capital (PCC)",
                    "Cartel de Jalisco", # "Cartel de Jalisco Nueva Generación (CJNG)", 
                    "Cartel de Sinaloa", 
                    "Los Piratas", 
                    "Los Orientales", 
                    "Fausta", 
                    "Ejército de Liberación", # "Ejército de Liberación Nacional (ELN)", 
                    "Los Trinitarios", 
                    "Los Hijos del Sol", 
                    "El Silencio", 
                    "Los Capitalinos", 
                    "Chatarreros", 
                    "Bang de Fujian", 
                    "Los Ciudad de Dios")

# limpiar nombres de organizaciones
organizaciones_2 <- tibble(organizaciones) |> 
  mutate(palabras = corpus::text_tokens(organizaciones, 
                                        corpus::text_filter(drop = stopwords::stopwords("es"))
  )) |> 
  # tidyr::nest(data = palabras) |> 
  rowwise() |> 
  mutate(organizacion_limpio = unlist(palabras) |> paste(collapse = " "))

organizaciones_2


# length(unique(prensa_palabras$id))

prensa_bigramas |> 
  slice(1:1e5) |> 
  group_by(id) |> 
  mutate(noticia_ubicacion = palabra %in% tolower(lista_comunas)) |> 
  print(n = 1000)


# detectar localizaciones ----

test_vector <- c("san", "pedro", "atacama")

test_vector |> purrr::p(2)
purrr::keep(test_vector, \(x) nchar(x) > 4)

positive_vector


lista_comunas_1_palabra <- lista_comunas_match |> filter(!is.na(comunas_1p)) |> pull(comunas_1p)
lista_comunas_2_palabras <- lista_comunas_match |> filter(!is.na(comunas_2p)) |> pull(comunas_2p)

prensa_bigramas_2 <- prensa_bigramas |> 
  # slice(1:1e8) |>
  mutate(ubicacion = palabra %in% lista_comunas_1_palabra) # true para palabras que son ubicaciones 

prensa_bigramas_3 <- prensa_bigramas_2 |> 
  mutate(ubicacion = if_else(!ubicacion, bigrama %in% lista_comunas_2_palabras, ubicacion)) |> # lo mismo pero por bigramas
  mutate(confirmar_ubicacion = palabra %in% c("comuna", "región", "comunas", "regiones",
                                              "ubicado", "ubica", "lugar", "zona", "área"))

prensa_bigramas_4 <- prensa_bigramas_3 |> 
  group_by(id) |> 
  mutate(noticia_ubicacion = any(ubicacion)) |> 
  mutate(noticia_confirmar_ubicacion = any(confirmar_ubicacion))

prensa_bigramas_5 <- prensa_bigramas_4 |> 
  ungroup() |> 
  filter(noticia_ubicacion)

prensa_bigramas_5 |> filter(ubicacion & noticia_confirmar_ubicacion)

rm(prensa_bigramas_2, prensa_bigramas_3, prensa_bigramas_4)



# detectar tema ----
prensa_bigramas_6 <- prensa_bigramas_5 |> 
  mutate(crimen = bigrama %in% bigramas_crimenorganizado,
         bandas = bigrama %in% organizaciones_2$organizacion_limpio)

prensa_bigramas_7 <- prensa_bigramas_6 |> 
  group_by(id) |> 
  mutate(noticia_crimen = any(crimen),
         noticia_bandas = any(bandas)) |> 
  filter(noticia_crimen | noticia_bandas) |> 
  ungroup()


length(unique(prensa_bigramas_7$id))



# prensa_bigramas_4 <- prensa_bigramas_3 |> 
#   slice(1:1e6) |>
#   # adjuntar información de ubicación
#   left_join(comunas |> 
#               mutate(palabra = tolower(comuna)), 
#             by = join_by(palabra)) |> 
#   # propagar comuna a toda la noticia
#   group_by(id) |> 
#   fill(names(comunas), .direction = "updown")

library(tidyr)


# gráficos por comunas ----
prensa_bigramas_8 <- prensa_bigramas_7 |> 
  filter(ubicacion & noticia_confirmar_ubicacion) |> 
  distinct(id, ubicacion, .keep_all = TRUE) |> 
  # mutate(ubicacion = palabra) |> 
  select(id, palabra, bigrama, noticia_crimen, noticia_bandas)


prensa_bigramas_8a <- prensa_bigramas_8 |> 
  # count(palabra, sort = TRUE) |> 
  left_join(lista_comunas_match |>
              select(comunas, bigrama = comunas_2p),
            by = join_by(bigrama))

prensa_bigramas_8b <- prensa_bigramas_8a |> 
  filter(is.na(comunas)) |> 
  select(-comunas) |> 
  # count(ubicacion, sort = TRUE) |> 
  left_join(lista_comunas_match |>
              select(comunas, palabra = comunas_1p),
            by = join_by(palabra), 
            multiple = "first")

prensa_bigramas_9 <- bind_rows(prensa_bigramas_8a |> filter(!is.na(comunas)),
                               prensa_bigramas_8b)


prensa_bigramas_10 <- prensa_bigramas_9 |> 
  left_join(comunas,
            by = join_by(comunas == comuna), 
            multiple = "first") |> 
  rename(comuna = comunas) |> 
  mutate(region = if_else(is.na(region) & !is.na(comuna), comuna, region))


prensa_bigramas_11 <- prensa_bigramas_10 |> 
  count(comuna, cut_comuna, region, cut_region, sort = TRUE)


readr::write_csv2(prensa_bigramas_11,
                  "analisis/tema_crimenorganizado/prensa_crimenorganizado_comuna.csv")

# 
# prensa_bigramas_5b <- prensa_bigramas_5 |> 
#   filter(is.na(cut_comuna)) |> 
#   select(1, 2) |> 
#   left_join(comunas |>
#               distinct(region, .keep_all = TRUE) |> 
#               mutate(ubicacion = tolower(region)),
#             by = join_by(ubicacion))
# 
# prensa_bigramas_6 <- prensa_bigramas_5 |> 
#   filter(!is.na(cut_comuna)) |> 
#   bind_rows(prensa_bigramas_5b) |> 
#   arrange(desc(n))

library(ggplot2)
library(forcats)

prensa_bigramas_11 |> 
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
       y = "Comunas o regiones mencionadas en noticias",
       fill = "Regiones")


prensa_bigramas_11 |> 
  filter(region == "Tarapacá") |> 
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
  geom_text(aes(label = ifelse(comuna != "Otras", n, "")), hjust = 0, nudge_x = 5) +
  scale_x_continuous(expand = expansion(c(0, 0.25))) +
  theme_minimal() +
  guides(fill = guide_none()) +
  labs(title = "Conteo de menciones de ubicaciones \nen noticias sobre crimen organizado",
       x = "Menciones de ubicaciones en noticias",
       y = "Comunas o regiones mencionadas en noticias",
       fill = "Regiones")


# gráficos por bandas ----


prensa_bigramas_bandas_2 <- prensa_bigramas_7 |> 
  filter(ubicacion & noticia_confirmar_ubicacion) |> 
  # distinct(id, ubicacion, crimen, bandas, .keep_all = TRUE) |> 
  # mutate(ubicacion = palabra) |> 
  select(id, palabra, bigrama, ubicacion, crimen, bandas)

prensa_bigramas_bandas_3 <- prensa_bigramas_7 |> 
  filter(bandas) |> 
  mutate(bandas = bigrama) |> 
  distinct(id, bandas)

prensa_bigramas_bandas_4 <- prensa_bigramas_bandas_3 |> 
  left_join(prensa_bigramas_10 |> 
              select(id, comuna, cut_comuna, region, cut_region),
            by = join_by(id),
            multiple = "first")

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/prensa_datos.parquet")

datos_prensa_2 <- datos_prensa |> 
  select(id, titulo, fuente, url, fecha)

rm(datos_prensa)

prensa_bigramas_bandas_5 <- prensa_bigramas_bandas_4 |> 
  left_join(datos_prensa_2, 
            join_by(id), 
            multiple = "first")

organizaciones_2

prensa_bigramas_bandas_6 <- prensa_bigramas_bandas_5 |> 
  left_join(organizaciones_2 |> select(bandas_nombre = organizaciones, organizacion_limpio),
            by = join_by(bandas == organizacion_limpio))

readr::write_csv2(prensa_bigramas_bandas_6,
                  "analisis/tema_crimenorganizado/prensa_crimenorganizado_comuna_banda.csv")

prensa_bigramas_bandas_6 |>           
  filter(year(fecha) >= 2020,
         fecha <= today() - months(1)) |> 
  mutate(fecha = floor_date(fecha, "month")) |> 
  ungroup() |> 
  count(fecha, bandas_nombre) |> 
  mutate(bandas_nombre = fct_reorder(bandas_nombre, n, .desc = T)) |> 
  # filter(!is.na(region)) |> 
  ggplot(aes(x = fecha, y = n, color = bandas_nombre)) +
  geom_line() +
  theme_minimal() +
  guides(fill = guide_legend(position = "right")) +
  labs(title = "Conteo de menciones de organizaciones criminales\nen noticias sobre crimen organizado",
       x = "Fecha",
       y = "Menciones de organizaciones criminales en noticias",
       color = "Organizaciones criminales")


# gráfico por regiones y bandas ----

prensa_bigramas_bandas_6 |>           
  filter(year(fecha) >= 2020,
         fecha <= today() - months(1)) |> 
  ungroup() |> 
  count(region, cut_region, bandas_nombre) |> 
  filter(!is.na(region)) |> 
  mutate(bandas_nombre = fct_reorder(bandas_nombre, n, .desc = T)) |> 
  group_by(region) |> 
  mutate(sum = sum(n)) |> 
  ungroup() |> 
  mutate(region = fct_reorder(region, sum, .desc = F)) |> 
  ggplot(aes(x = n, y = region, fill = bandas_nombre)) +
  geom_col(color = "white", linewidth = .1) +
  scale_x_continuous(expand = expansion(c(0, 0))) +
  theme_minimal() +
  guides(fill = guide_legend(position = "bottom", nrow = 3)) +
  labs(title = "Conteo de menciones de organizaciones criminales\nen noticias sobre crimen organizado",
       x = "Menciones de organizaciones criminales en noticias",
       y = "Regiones",
       fill = "Organizaciones criminales")




