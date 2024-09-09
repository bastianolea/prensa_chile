library(shiny)
library(bslib)
library(thematic)
library(showtext)
library(htmltools)
library(shinyjs)
library(arrow)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(lubridate)
library(shadowtext)
library(shinycssloaders)
library(ragg)

# colores ----
color_fondo = "#EEDABF"
color_texto = "#866C53"
color_detalle = "#A5876A"
color_destacado = "#C7392B"

# configuraciones ----

# register_font(name = "Lato")

source("funciones.R")

options(shiny.useragg = TRUE)

showtext::showtext_opts(dpi = 180)
font_add_google("Lato", "Lato")
font_add_google("Libre Baskerville", "Libre Baskerville")
showtext_auto()

options(spinner.type = 8, spinner.color = color_detalle)

thematic_shiny(
  font = "auto",
  # font = font_spec(families = c("Lato", "Libre Baskerville"), 
  #                  install = TRUE),
  # session = session,
  bg = color_fondo, fg = color_texto, accent = color_destacado)


# cargar datos ----
# palabras_semana <- read_parquet("apps/prensa_semanal/palabras_semana.parquet")
# palabras_semana_fuente <- read_parquet("apps/prensa_semanal/palabras_semana_fuente.parquet")
palabras_semana <- read_parquet("palabras_semana.parquet")
palabras_semana_fuente <- read_parquet("palabras_semana_fuente.parquet")

palabras_posibles <- palabras_semana |> 
  group_by(palabra) |> 
  summarize(n = sum(n)) |> 
  arrange(desc(n)) |> 
  # filter(str_detect(palabra, "corru"))
  # slice(1:500) |> 
  filter(n > 100) |> 
  pull(palabra)

fuentes <- palabras_semana_fuente$fuente |> unique() |> sort()

# ui ----
ui <- page_fluid(
  title = "Prensa en Chile", 
  lang = "es",
  
  ## tema ----
  theme = bslib::bs_theme(
    font_scale = 1.2,
    bg = color_fondo, fg = color_texto, primary = color_destacado, 
    # tipografías 
    heading_font = font_google("Libre Baskerville", wght = c(400, 700),
                               ital = c(0, 1), local = TRUE),
    base_font = font_google("Lato",  wght = c(300, 400, 700, 900),
                            ital = c(0, 1), local = TRUE)
    # base_font = font_link(
    #   "IBM Plex Mono",
    #   href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,400;0,500;0,700;1,400&display=swap"
    # )
  ),
  
  shinyjs::useShinyjs(),
  
  ## css ----
  tags$style(
    HTML("a { color: ", color_detalle, "}")
  ),
  tags$style(
    HTML(".selectize-input.items.not-full { color: red !important;}")),
  
  
  # —----
  
  # header ----
  fluidRow(
    column(12,
           
           div(style = css(margin_top = "12px", margin_bottom = "12px"),
               h1("Análisis de prensa en Chile",
                  style = css(font_style = "italic")),
               
               em(tags$a("Bastián Olea Herrera", href = "https://bastianolea.github.io/shiny_apps/",
                         style = css("color!" = color_detalle,
                                     opacity = "60%",
                                     text_decoration = "none")),
               )
           ),
           
           
           markdown("Proyecto de análisis de texto de noticias chilenas. Las noticias se obtienen regularmente mediante [web scraping](https://github.com/bastianolea/prensa_chile)."),
           markdown("Actualmente, el corpus obtenido supera las **600 mil noticias** individuales, las cuales suman un total de **105 millones de palabras**, abarcando más de 21 fuentes periodísticas distintas."),
           
           hr()
    )
  ),
  
  
  
  # semanas ----
  fluidRow(
    column(12,
           h3("Palabras más frecuentes, por semana"),
           
           markdown("Gráfico que presenta las palabras más frecuentes en cada semana de la prensa escrita chilena. Una línea conecta palabras que han sido relevantes por más de una semana, para seguir su tendencia. Cada palabra tiene un color, pero puedes usar las opciones para destacar una palabra por sobre el resto."),
    )
  ),
  fluidRow(
    column(9,
           plotOutput("g_semanas", 
                      width = "100%", height = "640px") |> withSpinner(),
           
           markdown("Éste gráfico se genera automáticamente, a partir de un proceso automatizado de obtención de textos y procesamiento de datos.
                    A partir de todas las noticias publicadas por los medios comunicacionales escritos online, semana a semana, se transforman todas las noticias en palabras separadas, y se cuenta la repetición de cada palabra, tomando como una repetición las distintas conjugaciones de cada palabra (por ejemplo, _delincuente_ y _delincuencia_ cuentan como una sola palabra repetida 2 veces). Luego, se eliminan palabras irrelevantes (como artículos, pronombres y otras), y se genera un ranking de las palabras más frecuentes para cada semana."),
           
    ),
    column(3,
           ## opciones ----
           h5("Opciones del gráfico"),
           
           sliderInput("semanas",
                       "Rango de semanas",
                       min = 2, max = 4*5,
                       value = 9),
           
           selectizeInput("destacar_palabra",
                          "Destacar palabras",
                          choices = NULL, #c("Ninguna", palabras_posibles),
                          multiple = FALSE,
                          options = list(search = TRUE,
                                         create = TRUE,
                                         placeholder = "")),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Escribir o elegir de la lista una palabra para destacarla con color en el gráfico. Como es un gráfico de palabras principales, puede que hayan palabras que no aparezcan dado que no fueron las principales dentro del rango de fechas.")
           ),
           
           sliderInput("frecuencia_min",
                       "Proporción mínima",
                       min = 0.002*100, max = 0.005*100,
                       value = 0.0036*100, ticks = F,
                       step = 0.0001*100),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Proporción mínima que tiene que tener una palabra en el contexto de todas las palabras; por defecto, las palabras deben aparecer al menos en un 0,3% del total de palabras.")
           ),
           
           
           
           
           div(id = "opciones_avanzadas",
               
               selectizeInput("palabras_excluir",
                              "Excluir palabras",
                              choices = NULL,
                              multiple = TRUE,
                              options = list(create = TRUE,
                                             placeholder = "")),
               div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                   em("Palabras a remover del gráfico. Deben separarse con comas y escribirse tal como aparecen.")
               ),
               
               sliderInput("palabras_semana_max",
                           "Máximo de palabras por semana",
                           min = 4, max = 20,
                           value = 10,
                           step = 1),
               div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                   em("Cantidad máxima de palabras por cada semana; limita la cantidad de palabras por semana dejando sólo las x mayores; por defecto son 10.")
               ),
               
               selectInput("texto_tamaño",
                           "Tamaño del texto",
                           choices = c("Normal" = 2.3, "Mediano" = 2.8, "Grande" = 3.2),
                           selected = c("Mediano" = 2.8)
               ),
               
               sliderInput("angulo",
                           "Ángulo de etiquetas",
                           min = 0, max = 60,
                           value = 40,
                           step = 10),
               div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                   em("Ángulo del texto de las etiquetas; alterarlo puede hacer que aparezcan más etiquetas de palabras, dado que las que se sobreponen son ocultadas.")
               )
           ) |> shinyjs::hidden(),
           
           div(
             style = css(opacity = "30%"),
             actionButton("mostrar_opciones", "Opciones avanzadas")
           )
           
    )
  ),
  
  # palabras ----
  fluidRow(
    column(12,
           br(),
           hr(),
           h3("Frecuencia de palabras, por semana"),
           
           markdown("Seleccione una o varias palabras para comparar su frecuencia semanal."),
    )
  ),
  
  fluidRow(
    column(4, style = css(max_width = "600px"),
           selectizeInput("selector_palabras",
                          "Conceptos que desea incluir",
                          choices = NULL, #c("corrupción", "delincuencia", palabras_posibles),
                          # choices = NULL,
                          selected = c("delincuencia", "corrupción", "hermosilla", "enel"),
                          multiple = TRUE,
                          width = "100%",
                          options = list(search = TRUE,
                                         create = TRUE,
                                         placeholder = "")),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Elija palabras de la lista para incluirlas en el gráfico. La lista está ordenada por frecuencia de palabras. Puede escribir para buscar o incluir otras palabras.")
           ),
    ),
    column(4,  
           style = css(max_width = "600px"),
           selectInput("tipo_grafico",
                       "Tipo de gráfico",
                       choices = c("Barras", "Líneas"),
                       selected = "Barras",
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cambie el tipo de gráfico que se usará para visualizar los datos. Por defecto, se cambia a visualización de líneas si se seleccionan muchas palabras, y a barras si se seleccionan pocas.")
           ),
    ),
    
    column(4,  style = css(max_width = "600px"),
           sliderInput("semanas_palabras",
                       "Rango de semanas",
                       min = 2, max = 4*6,
                       value = 9,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Personalice el rango de tiempo que abarcará la visualización. Por defecto, si el rango es muy amplio, se cambia a barras.")
           ),
    )
    
  ),
  fluidRow(
    column(12,
           plotOutput("g_palabras", height = "640px", width = "100%") |> withSpinner()
    )
  ),
  
  
  # semana fuente ----
  fluidRow(
    column(12,
           
           br(),
           hr(),
           h3("Palabras más mencionadas en medios, semanalmente"),
           
           markdown("Gráfico que expresa, por cada semana, las palabras más repetidas en el texto de las noticias, indicando también la proporción de las menciones de la palabra que correspondena a medios de comunicación específicos. Por ejemplo, si una barra muestra un color prevalente en ella, significa que el medio de comunicación correspondiente a ese color usó más frecuentemente el termino que los demás.")
    )
  ),
  fluidRow(
    column(4,  
           style = css(max_width = "600px"),
           sliderInput("semanas_fuentes",
                       "Rango de semanas",
                       min = 1, max = 8,
                       value = 4,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Personalice el rango de tiempo que abarcará la visualización. Por defecto, si el rango es muy amplio, se cambia a barras.")
           )
    ),
    
    column(4,  
           style = css(max_width = "600px"),
           sliderInput("semana_fuentes_fuentes",
                       "Cantidad de medios",
                       min = 3, max = 10,
                       value = 5,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cantidad de medios comunicacionales a identificar. Se mostrarán los nombres de las n fuentes con mayor cantidad de palabras. El resto se agrupará en como ”Otros”.")
           )
    ),
    
    column(4,  
           style = css(max_width = "600px"),
           sliderInput("semana_fuentes_palabras_n",
                       "Cantidad de palabras",
                       min = 5, max = 20,
                       value = 15,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cantidad de palabras a mostrar por semana. Aumentar este valor aumenta la cantidad de barras, y podría permitir ver conceptos menos comunes.")
           )
    )
  ),
  fluidRow(
    column(12,
           plotOutput("g_semana_fuente", height = "640px", width = "100%") |> withSpinner()
    )
  ),
  
  
  # semana fuente palabras ----
  fluidRow(
    column(12,
           
           # title = paste("Cantidad de noticias que mencionan:", .palabra), 
           # subtitle = "Sólo incluyendo mayores medios de comunicación escritos",
           br(),
           hr(),
           h3("Cantidad menciones de un concepto específico"),
           h5("Concepto:", em(textOutput("texto_selector_palabras_fuente", inline = TRUE))),
           
           markdown("Seleccione un concepto, palabra, o nombre para comparar las menciones del concepto elegido entre los distintos medios de comunicación escritos.")
    )
  ),
  fluidRow(
    column(3, style = css(max_width = "600px"),
           selectizeInput("selector_palabras_fuente",
                          "Seleccione el concepto a comparar",
                          choices = NULL, #c("hermosilla", "macaya", "corrupción", "delincuencia", palabras_posibles),
                          # choices = NULL,
                          # selected = "hermosilla",
                          multiple = FALSE,
                          width = "100%",
                          options = list(search = TRUE,
                                         create = TRUE, placeholder = "")),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Elija una palabra de la lista para usarla en el gráfico. La lista está ordenada por frecuencia de palabras. Puede escribir para buscar o incluir otras palabras.")
           )
    ),
    
    column(3,  
           style = css(max_width = "600px"),
           sliderInput("semanas_fuentes_palabras",
                       "Rango de semanas",
                       min = 1, max = 8,
                       value = 5,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Personalice el rango de tiempo que abarcará la visualización. Por defecto, si el rango es muy amplio, se cambia a barras.")
           )
    ),
    
    column(3,  
           style = css(max_width = "600px"),
           sliderInput("semana_fuentes_palabras_fuentes",
                       "Cantidad de medios",
                       min = 3, max = 15,
                       value = 10,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cantidad de medios comunicacionales a identificar. Se mostrarán los nombres de las n fuentes con mayor cantidad de palabras. El resto se agrupará en ”Otros”.")
           )
    ),
    column(3, style = css(max_width = "600px"),
           selectizeInput("destacar_medio",
                          "Destacar un medio",
                          choices = c("Ninguno", fuentes),
                          selected = "Ninguno",
                          multiple = FALSE,
                          width = "100%",
                          options = list(search = TRUE,
                                         create = TRUE, placeholder = "")),
           div(style = css(font_family = "Libre Baskerville", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Seleccione un medio de comunicación para destacarlo en el gráfico por sobre el resto de los medios disponibles.")
           )
    )
  ),
  fluidRow(
    column(12,
           
           
           
           plotOutput("g_semana_fuente_palabra", height = "480px", width = "100%") |> withSpinner()
    )
  ),
  
  
  
  
  # firma ----
  fluidRow(
    column(12, style = css(padding = "28px"),
           hr(),
           
           markdown("Desarrollado por [Bastián Olea Herrera.](https://bastian.olea.biz)"),
           
           markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
           
           markdown("Código de fuente de esta app y del procesamiento de los datos [disponible en GitHub.](https://github.com/bastianolea/prensa_chile)"),
           
           div(style = "height: 40px")
           
    )
  )
)

# —----

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # interacciones ----
  
  observeEvent(input$mostrar_opciones, {
    shinyjs::toggle("opciones_avanzadas")
  })
  
  
  # selectores ----
  
  observe(
  updateSelectizeInput(session, 'destacar_palabra', 
                       # choices = c("Ninguna", palabras_posibles),
                       choices = c("Ninguna", selector_semanas_palabras()),
                       server = TRUE)
  )
  
  # observe(
  #   updateSelectizeInput(session, 'palabras_excluir', 
  #                        # choices = c("Ninguna", palabras_posibles),
  #                        choices = c(rev(selector_semanas_palabras())),
  #                        server = TRUE)
  # )
  
  
  updateSelectizeInput(session, 'selector_palabras', 
                       choices = c("corrupción", "delincuencia", palabras_posibles),
                       selected = c("delincuencia", "corrupción", "hermosilla", "enel"),
                       server = TRUE)
  
  updateSelectizeInput(session, 'selector_palabras_fuente', 
                       choices = c("Hermosilla", "Macaya", "corrupción", "delincuencia", palabras_posibles),
                       selected = "Hermosilla",
                       server = TRUE)
  
  
  # datos ----
  ## semanas ----
  datos_conteo_semanas <- reactive({
    datos1 <- palabras_semana |> 
      # límite de fecha
      filter(fecha >= today() - weeks(input$semanas)) |>
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
      # dejar solo top palabras semana por porcentaje de semana
      filter(p_palabra_semana > (input$frecuencia_min)/100) |> 
      ungroup() |> 
      # # dejar solo top palabras total (maximo de palabras visibles)
      # mutate(palabra_lump = fct_lump(palabra, w = freq_total_palabra, 
      #                                n = 25, other_level = "otras")) |> 
      # filter(palabra_lump != "otras") |> 
      # sacar palabras que salen una sola vez
      # add_count(palabra, name = "palabra_n") |>
      # filter(palabra_n > 1) |>
      # sacar semanas donde hayan pocos términos (indicio de error)
      group_by(semana) |> 
      mutate(semana_n = n()) |> 
      filter(semana_n > 2) |> 
      # dejar solo top 10 palabras por semana
      group_by(semana) |>
      slice_max(n, n = input$palabras_semana_max)
    
    # datos1 |> 
    #   count(semana, fecha)
    
    # browser()
    datos2 <- datos1 |> 
      filter(!palabra %in% input$palabras_excluir) |> 
      # ordenar palabras por frecuencia
      ungroup() |> 
      mutate(palabra = fct_reorder(palabra, freq_total_palabra)) |> 
      # etiquetas hacia la izquierda
      mutate(inv = ifelse(semana == min(semana) | n < mean(n)*0.8, TRUE, FALSE))
    
    # datos |> slice(1) |> pull(fecha) |> 
    # redactar_fecha()
    
    # por porcentaje o por frecuencia
    # group_by(semana) |>
    # mutate(n = n/sum(n)) |>
    
    
    
    # browser()
    return(datos2)
  })
  
  # para el selector de palabras destacadas, cosa que solo contenga palabras que aparecen en el gráfico en vez de todas
  selector_semanas_palabras <- reactive({
    # browser()
    datos_conteo_semanas() |> 
      select(palabra, freq_total_palabra) |> 
      arrange(desc(freq_total_palabra)) |> 
      pull(palabra) |> 
      unique() |> 
      as.character()
    
  })
  
  ## palabras ----
  
  datos_conteo_semanas_palabras <- reactive({
    req(input$selector_palabras != "")
    
    palabras_semana |> 
      filter(fecha > today() - weeks(input$semanas_palabras)) |> 
      filter(palabra %in% input$selector_palabras) |> 
      group_by(palabra) |> 
      mutate(freq_total_palabra = sum(n)) |> 
      ungroup() |> 
      mutate(palabra = fct_reorder(palabra, freq_total_palabra, .desc = T)) |> 
      # otros datos
      group_by(semana) |> 
      mutate(n_semana = sum(n),
             rank = dense_rank(desc(n))) |> 
      ungroup() |> 
      mutate(prom = mean(n)) |> 
      mutate(chico = ifelse(n > prom*0.8, FALSE, TRUE)) |> 
      # fechas
      group_by(semana) |> 
      mutate(fecha_etiqueta = redactar_fecha(min(fecha)),
             fecha_etiqueta = fct_reorder(fecha_etiqueta, semana)) |> 
      ungroup()
  })
  
  
  ## semana fuente ----
  # top palabras por semana
  datos_semana_fuente <- reactive({
    .n_fuentes = input$semana_fuentes_fuentes # 5
    .palabras_por_fuente = 60
    .palabras_por_semana = input$semana_fuentes_palabras_n #15
    .semanas = (week(today())-(input$semanas_fuentes-1)):week(today()) #29:32
    
    palabras_semana_fuente |>
      # top 10 palabras 
      group_by(fuente, semana) |> 
      slice_max(n, n = .palabras_por_fuente) |> # palabras por fuente
      # calcular palabras por fuente
      group_by(fuente) |> 
      mutate(n_total_fuente = sum(n)) |> 
      filter(semana %in% .semanas) |> 
      ungroup() |> 
      # agrupar fuentes chicas
      
      mutate(fuente = fct_reorder(fuente, n_total_fuente, .desc = TRUE)) |>
      mutate(fuente = fct_lump(fuente, w = n_total_fuente, n = .n_fuentes, ties.method = "first", 
                               other_level = "Otros")) |>
      group_by(fuente, semana, fecha, palabra) |>
      summarize(n = sum(n)) |> 
      
      # maximo palabras por semana
      group_by(semana, palabra) |> 
      mutate(n_semana = sum(n)) |>
      group_by(semana) |> 
      mutate(rank2 = dense_rank(desc(n_semana))) |> 
      filter(rank2 <= .palabras_por_semana) |> # cantidad de palabras por semana
      # ordenar palabras
      group_by(semana, palabra) |> 
      mutate(n_palabra_semana = sum(n)) |> 
      group_by(semana) |> 
      mutate(palabra = tidytext::reorder_within(palabra, n_palabra_semana, semana)) |> 
      ungroup()
  })
  
  ## semana fuente palabra ----
  # palabra específica por fuente
  datos_semana_fuente_palabra <- reactive({
    req(input$selector_palabras_fuente != "")
    
    .palabra = tolower(input$selector_palabras_fuente) #"hermosilla"
    .semanas = (week(today())-(input$semanas_fuentes_palabras-1)):week(today()) #29:32
    .n_fuentes = input$semana_fuentes_palabras_fuentes
    
    dato1 <- palabras_semana_fuente |>
      # excluir palabras
      filter(palabra == .palabra) |> 
      # filtrar semanas
      filter(semana %in% .semanas) |> 
      # # ranking de fuentes con mayor cantidad de palabras
      group_by(fuente) |>
      mutate(n_total_fuente = sum(n)) |>
      ungroup() |>
      # agrupar fuentes chicas
      mutate(fuente = fct_lump(fuente, w = n_total_fuente,
                               n = .n_fuentes, other_level = "Otros")) |>
      mutate(fuente = fct_reorder(fuente, n_total_fuente, .desc = T),
             fuente = fct_relevel(fuente, "Otros", after = 0)) |> 
      group_by(fuente, semana, fecha, palabra) |>
      summarize(n = sum(n)) |>
      # ordenar palabras
      group_by(semana, fuente) |> 
      mutate(n_palabra_fuente = sum(n)) |> 
      group_by(semana) |> 
      mutate(fuente2 = tidytext::reorder_within(fuente, n_palabra_fuente, semana)) |> 
      ungroup()
    
    # browser()
    
    if (input$destacar_medio == "Ninguno") {
      dato2 <- dato1 |> 
        mutate(destacado = "Ninguno")
    } else if (input$destacar_medio != "Ninguno") {
      dato2 <- dato1 |> 
        mutate(destacado = ifelse(fuente == input$destacar_medio, 
                                  "Destacado", "Otros"))
    }
    
    return(dato2)
  })
  
  
  # gráficos ----
  
  ## semanas ----
  output$g_semanas <- renderPlot({
    req(input$destacar_palabra != "")
    
    # opciones gráfico
    .dodge = 3
    .espaciado_y = 0.08
    .espaciado_x = 0.05
    
    # hace que los colores del gráfico sean por palabras, o por palabra destacada vs otras
    variable <- ifelse(input$destacar_palabra != "Ninguna",
                       "destacar", "palabra")
    
    if (input$destacar_palabra != "Ninguna") {
      # crea una variable dicotómica con la palabra destacada
      datos3 <- datos_conteo_semanas() |> 
        mutate(destacar = ifelse(palabra == tolower(input$destacar_palabra), 
                                 tolower(input$destacar_palabra), "otras"),
               destacar = fct_relevel(destacar, "otras", after = 0))
    } else {
      datos3 <- datos_conteo_semanas()
    }
    
    #gráfico 
    plot <- datos3 |> #datos_conteo_semanas() |> 
      ggplot(aes(fecha, n)) +
      geom_step(aes(color = !!sym(variable), group = palabra),
                linewidth = .9, alpha = .5,
                direction = "mid", 
                position = position_dodge(.dodge),
                show.legend = F) +
      # texto
      shadowtext::geom_shadowtext(
        aes(label = ifelse(inv, paste(palabra, "  ."), paste("  ", palabra)),
            hjust = ifelse(inv, 1, 0),
            color = !!sym(variable), group = palabra),
        bg.colour = color_fondo, bg.r = 0.3, angle = input$angulo, size = as.numeric(input$texto_tamaño), vjust = 0.3, 
        position = position_dodge(.dodge), check_overlap = T, show.legend = F) +
      geom_point(aes(group = palabra),
                 size = 3, color = color_fondo, 
                 position = position_dodge(.dodge)) +
      geom_point(aes(color = !!sym(variable), group = palabra),
                 size = 2, position = position_dodge(.dodge)) +
      # escalas
      scale_y_continuous(expand = expansion(c(.espaciado_y*0.7, .espaciado_y))) +
      scale_x_date(date_breaks = "weeks", 
                   labels = redactar_fecha,
                   # date_labels = "%d de %B", 
                   expand = expansion(c(.espaciado_x, .espaciado_x))) +
      guides(color = guide_none()) +
      # theme_classic() +
      coord_cartesian(clip = "off") +
      theme(panel.grid.major.x = element_line(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(family = "Lato"),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.text.x = element_text(family = "Lato", hjust = 1, angle = input$angulo),
            # panel.background = element_rect(fill = color_destacado),
            plot.caption = element_text(color = color_detalle)) +
      labs(y = "frecuencia de palabras por semana",
           x = NULL,
           # title = "Conceptos principales en prensa, por semana", 
           caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile"
      )
    
    # paleta de colores si se destaca una palabra
    if (input$destacar_palabra != "Ninguna") {
      plot <- plot +
        scale_color_manual(values = c(color_texto, color_destacado))
    } else {
      # browser()
      plot <- plot +
        # scale_color_brewer(palette = "Set1")
        # scale_color_manual(values = colorRampPalette(brewer.pal(Inf, "Dark2"))(length(unique(datos$palabra))))
        scale_color_viridis_d(begin = .2, end = .7, option="magma")
    }
    
    
    return(plot)
    
  }, res = 100)
  
  
  ## palabras ----
  
  observeEvent(input$selector_palabras, {
    if (length(input$selector_palabras) > 4) {
      updateSelectInput(session, "tipo_grafico", 
                        selected = "Líneas")
    } else if (length(input$selector_palabras) <= 4) {
      updateSelectInput(session, "tipo_grafico", 
                        selected = "Barras")
    }
  })
  
  observeEvent(input$semanas_palabras, {
    if (input$semanas_palabras > 16) {
      updateSelectInput(session, "tipo_grafico", 
                        selected = "Barras")
    }
  })
  
  output$g_palabras <- renderPlot({
    
    # datos_conteo_semanas_palabras()
    
    # if (length(input$selector_palabras) > 4) {
    if (input$tipo_grafico == "Líneas") {
      ### líneas ----
      plot <- datos_conteo_semanas_palabras() |> 
        ggplot(aes(fecha_etiqueta, n, color = palabra, group = palabra)) +
        geom_line(linewidth = 1.2, alpha = .7, show.legend = F) +
        geom_point(size = 4, color = color_fondo) +
        geom_point(size = 3) +
        geom_text(data = ~group_by(.x, palabra) |> slice_max(n),
                  aes(label = palabra,
                      y = n + (prom*0.25)),
                  size = 3) +
        ggrepel::geom_text_repel(data = ~filter(.x, semana == max(semana)),
                                 aes(label = palabra, color = palabra),
                                 size = 3, 
                                 hjust = 0, 
                                 # xlim = c(0, ~pull(.x, fecha) |> max()),
                                 nudge_x = 0.3, segment.alpha = .3,
                                 show.legend = F, direction = "y") +
        # scale_x_date(expand = expansion(c(0.02, 0.1))) +
        scale_x_discrete(expand = expansion(c(0.02, 0.1))) +
        scale_color_viridis_d(begin = .2, end = .7, option = "magma") +
        guides(color = guide_legend(reverse = T, override.aes = list(size = 4)))
      
      
    } else if (input$tipo_grafico == "Barras") {
      ### barras ----
      # browser()
      plot <- datos_conteo_semanas_palabras() |>
        # filter(rank == 1)
        ggplot(aes(fecha_etiqueta, n, fill = palabra)) +
        # geom_col(width = .9, color = color_fondo,
        #          position = position_dodge2(preserve = c("single"))) +
        geom_col(width = .7, color = color_fondo) +
        geom_point(aes(color = palabra), alpha = 0) +
        geom_text(aes(label = ifelse(!chico, as.character(palabra), ""),
                      y = n - (prom*0.06),
        ),
        position = position_stack(),
        color = "white", angle = 90, 
        hjust = 1,
        size = 3) +
        geom_text(data = ~filter(.x, rank == 1),
                  aes(label = ifelse(chico, as.character(palabra), ""),
                      y = n_semana + (prom*0.09),
                      color = palabra),
                  vjust = 0, show.legend = F, check_overlap = T,
                  size = 2.8) +
        scale_fill_viridis_d(begin = .2, direction = -1, end = .7, option = "magma", aesthetics = c("color", "fill")) +
        scale_y_continuous(expand = expansion(c(0, 0.05))) +
        guides(fill = guide_none(),
               color = guide_legend(reverse = FALSE, position = "right", 
                                    override.aes = list(alpha = 1, size = 4)))
    }
    
    plot <- plot +
      theme(legend.text = element_text(margin = margin(l = 2))) +
      theme(panel.grid.major.x = element_line(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(family = "Lato"),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.text.x = element_text(family = "Lato", hjust = 1),
            legend.title = element_text(family = "Libre Baskerville", face = "italic"),
            plot.caption = element_text(color = color_detalle)) +
      theme(axis.text.x = element_text(family = "Lato", angle = 40)) + #, hjust = 1, angle = 40))
      labs(color = "Palabras", y = "frecuencia de palabras", x = NULL,
           caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile"
      )
    
    
    return(plot)
  }, res = 100)
  
  
  ## semana fuente ----
  output$g_semana_fuente <- renderPlot({
    
    plot <- datos_semana_fuente() |> 
      ggplot(aes(x = n, y = palabra, fill = fuente)) +
      geom_col(width = .7, color = color_fondo) +
      geom_point(aes(color = fuente), alpha = 0) +
      tidytext::scale_y_reordered() +
      scale_x_continuous(expand = expansion(c(0, .1))) +
      scale_fill_viridis_d(begin = .2, direction = -1, end = .7, option = "magma", aesthetics = c("color", "fill")) +
      facet_wrap(~fecha, 
                 scales = "free", nrow = 1)  +
      guides(fill = guide_none(),
             color = guide_legend(position = "bottom", 
                                  nrow = ifelse(input$semana_fuentes_fuentes > 5, 2, 1), # nrow = 1, 
                                  reverse = TRUE,
                                  override.aes = list(alpha = 1, size = 4))) +
      labs(y = "palabras más mencionadas por semana", x = "frecuencia de mención, por fuente",
           fill = "fuentes\nescritas",
           caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile")
    
    plot <- plot +
      theme(legend.text = element_text(margin = margin(l = 2))) +
      theme(panel.grid.major.x = element_line(),
            panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(family = "Lato"),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.title.x = element_text(family = "Libre Baskerville", face = "italic",
                                        margin = margin(t=6, b=-10)),
            axis.text.x = element_text(family = "Lato", hjust = 1),
            legend.title = element_text(family = "Libre Baskerville", face = "italic"),
            plot.caption = element_text(color = color_detalle)) +
      theme(axis.text.x = element_text(family = "Lato", angle = 40))
    
    return(plot)
  }, res = 100)
  
  
  ## semana fuente palabra ----
  
  output$texto_selector_palabras_fuente <- renderText({
    str_to_sentence(input$selector_palabras_fuente)
  })
  
  output$g_semana_fuente_palabra <- renderPlot({
    
    plot <- datos_semana_fuente_palabra() |> 
      ggplot(aes(x = n, y = fuente2,
                 color = destacado)) +
      # geom_col(width = .6, fill = color_destacado) +
      geom_segment(aes(xend = 0, yend = fuente2),
                   linewidth = 2, color = color_fondo) +
      geom_segment(aes(xend = 0, yend = fuente2),
                   linewidth = 1, alpha = .5) +
      geom_point(size = 4, color = color_fondo) +
      geom_point(size = 3) +
      guides(color = guide_none()) +
      coord_cartesian(clip = "off") +
      tidytext::scale_y_reordered() +
      scale_x_continuous(expand = expansion(c(0, 0.1))) +
      # scale_fill_viridis_d(begin = .2, direction = -1, end = .7, option = "magma", aesthetics = c("color", "fill")) +
      scale_color_manual(values = c(color_destacado, color_texto)) +
      facet_wrap(~fecha, 
                 scales = "free", nrow = 1)  +
      # theme_minimal() +
      labs(y = "fuentes ordenadas por menciones", x = "frecuencia de mención, por semanas",
           caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile")
    
    plot <- plot +
      theme(legend.text = element_text(margin = margin(l = 2))) +
      theme(panel.grid.major.x = element_line(),
            panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(family = "Lato"),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.title.x = element_text(family = "Libre Baskerville", face = "italic",
                                        margin = margin(t=6)),
            axis.text.x = element_text(family = "Lato", hjust = 1),
            legend.title = element_text(family = "Libre Baskerville", face = "italic"),
            plot.caption = element_text(color = color_detalle)) +
      theme(axis.text.x = element_text(family = "Lato", angle = 90, vjust = .5))
    
    return(plot)
  }, res = 100)
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
