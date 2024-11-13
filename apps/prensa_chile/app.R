library(shiny) |> suppressPackageStartupMessages()
library(htmltools)
library(shinyjs) |> suppressPackageStartupMessages()

library(dplyr) |> suppressPackageStartupMessages()
# library(arrow) |> suppressPackageStartupMessages()
library(arrow) |> suppressPackageStartupMessages()
# library(readr) |> suppressPackageStartupMessages()
library(ggplot2)
library(forcats)
library(stringr)
library(lubridate) |> suppressPackageStartupMessages()
library(shadowtext)
library(shinycssloaders)

library(thematic)
library(showtext)
library(sysfonts)
library(curl) |> suppressPackageStartupMessages()
library(bslib) |> suppressPackageStartupMessages()
library(ragg)

source("funciones.R")


# colores ----
color_fondo = "#EEDABF"
color_texto = "#866C53"
color_negro = "#694E34"
color_detalle = "#A5876A"
color_destacado = "#C7392B"


# configuraciones ----
options(spinner.type = 8, spinner.color = color_detalle)
options(shiny.useragg = TRUE)
showtext::showtext_opts(dpi = 180)
thematic_shiny(font = "auto", accent = color_destacado)
.texto_ejes = 9

# tipografías para ragg/sysfonts
sysfonts::font_add_google("Lato", "Lato", db_cache = TRUE)
sysfonts::font_add_google("Libre Baskerville", "Libre Baskerville", db_cache = TRUE)
showtext_auto()


# cargar datos ----
# setwd("apps/prensa_semanal")
palabras_semana <- read_parquet("palabras_semana.parquet")
palabras_semana_fuente <- read_parquet("palabras_semana_fuente.parquet")
correlacion <- read_parquet("prensa_correlacion.parquet")
correlacion_fuente <- read_parquet("prensa_correlacion_fuente.parquet")

# setwd("apps/prensa_semanal")
# palabras_semana <- read_rds("palabras_semana.rds")
# palabras_semana_fuente <- read_rds("palabras_semana_fuente.rds")
# correlacion <- read_rds("prensa_correlacion.rds")
# correlacion_fuente <- read_rds("prensa_correlacion_fuente.rds")


# vectores ----
palabras_posibles <- palabras_semana |> 
  group_by(palabra) |> 
  summarize(n = sum(n)) |> 
  arrange(desc(n)) |> 
  filter(n > 100) |> 
  pull(palabra)

fuentes <- palabras_semana_fuente$fuente |> unique() |> sort()


# ui ----
ui <- page_fluid(
  title = "Prensa en Chile", 
  lang = "es",
  
  # tipografías en html
  fresh::use_googlefont("Lato"),
  fresh::use_googlefont("Libre Baskerville"),
  fresh::use_googlefont("Libre Baskerville Italic"),
  
  ## tema ----
  # tags$head(HTML('<link rel="preconnect" href="https://fonts.googleapis.com">')),
  # tags$head(HTML('<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>')),
  
  theme = bslib::bs_theme(
    font_scale = 1.2,
    bg = color_fondo, fg = color_texto, primary = color_destacado, 
    # tipografías 
    base_font = "Lato",
    heading_font = "Libre Baskerville Italic"
    # heading_font = font_link("Libre Baskerville", "https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,300;1,400;1,700;1,900&family=Libre+Baskerville:ital,wght@0,400;0,700;1,400&display=swap"),
    # base_font = font_link("Lato", "https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,300;1,400;1,700;1,900&display=swap")
    # base_font = font_link("Libre Baskerville", "https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,300;1,400;1,700;1,900&family=Libre+Baskerville:ital,wght@0,400;0,700;1,400&display=swap"),
    # heading_font = font_google("Libre Baskerville", wght = "400 700", #c(400, 700),
    #                            ital = c(0, 1), 
    #                            local = FALSE),
    # base_font = font_google("Lato",  
    #                         # wght = "300 400 700 900", #
    #                         wght = c(300, 400, 700, 900),
    #                         ital = c(0, 1), 
    #                         # cache = sass_file_cache(dir = "cache"))
    #                         local = FALSE)
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
           
           
           markdown("Proyecto de _análisis de texto_ de noticias publicadas por medios de comunicación digitales de Chile. _En construcción._"),
           markdown("Un sistema automatizado obtiene y procesa grandes cantidades de datos de noticias, creando una base de datos de noticias con su texto (título, cuerpo, bajada) y metadatos (fecha, fuente, dirección), a partir de la cual es posible realizar distintos análisis sobre el texto. Inicialmente, se puede analizar _sobre qué_ hablan las noticias en determinadas fechas (análisis descriptivo). Posteriormente se pueden hacer análisis más sofisticados, como agrupar temáticamente las noticias de forma automatizada, detectar si las noticias sobre cierto tema son positivas o negativas, o correlacionar qué se dice cuando se mencionan otros conceptos."),
           markdown("Actualmente, el material obtenido supera las **600 mil noticias** individuales, las cuales suman un total de **105 millones de palabras**, abarcando más de 21 fuentes periodísticas distintas. Para más información técnica sobre este proyecto, [visite el repositorio](https://github.com/bastianolea/prensa_chile)."),
           
           hr()
    )
  ),
  
  
  
  # líneas semanas ----
  fluidRow(
    column(12,
           h3("Palabras más frecuentes, por semana"),
           
           markdown("Gráfico que presenta las palabras más frecuentes en cada semana de la prensa escrita chilena. El eje horizontal representa el tiempo, y el vertical la frecuencia del concepto. Una línea conecta palabras que han sido relevantes por más de una semana, para seguir su tendencia. Cada palabra tiene un color, pero puedes usar las opciones para destacar una palabra por sobre el resto."),
    )
  ),
  fluidRow(
    column(9,
           plotOutput("g_semanas", 
                      width = "100%", height = "640px") |> withSpinner(),
           
           markdown("Todos los gráficos se generan automáticamente, mediante de un proceso automatizado de obtención de textos y procesamiento de datos.
                    A partir de todas las noticias publicadas por los medios comunicacionales escritos online, semana a semana, se transforman todas las noticias en palabras separadas, y se cuenta la repetición de cada palabra, tomando como una repetición las distintas conjugaciones de cada palabra (por ejemplo, _delincuente_ y _delincuencia_ cuentan como una sola palabra repetida 2 veces). Luego, se eliminan palabras irrelevantes (como artículos, pronombres y otras), y se genera un ranking de las palabras más frecuentes para cada semana."),
           
    ),
    column(3,
           ## opciones
           # h5("Opciones del gráfico"),
           
           sliderInput("semanas",
                       "Rango de semanas",
                       min = 2, max = 4*3,
                       value = 4*2, width = "100%",),
           
           selectizeInput("destacar_palabra",
                          "Destacar palabras",
                          choices = NULL, #c("Ninguna", palabras_posibles),
                          multiple = FALSE, width = "100%",
                          options = list(search = TRUE,
                                         create = TRUE,
                                         placeholder = "")),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Escribir o elegir de la lista una palabra para destacarla con color en el gráfico. Como es un gráfico de palabras principales, puede que hayan palabras que no aparezcan dado que no fueron las principales dentro del rango de fechas.")
           ),
           
           sliderInput("frecuencia_min",
                       "Proporción mínima",
                       min = 0.002*100, max = 0.005*100,
                       value = 0.003*100, ticks = F, width = "100%",
                       step = 0.0001*100),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Proporción mínima que tiene que tener una palabra en el contexto de todas las palabras; por defecto, las palabras deben aparecer al menos en un 0,3% del total de palabras.")
           ),
           
           selectInput("palabras_semanas_tipo",
                       label = "Medida de frecuencia",
                       choices = c("Porcentaje", "Frecuencia")
           ),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Medida de posición vertical de las palabras. Se calculan después de eliminar palabras vacías y poco frecuentes, por lo que representan medidas meramente comparativas de la prevalencia de términos semanales.")
           ),
           
           
           div(id = "opciones_avanzadas",
               
               selectizeInput("palabras_excluir",
                              "Excluir palabras",
                              choices = NULL,
                              multiple = TRUE,  width = "100%",
                              options = list(create = TRUE,
                                             placeholder = "")),
               div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                   em("Palabras a remover del gráfico. Deben separarse con comas y escribirse tal como aparecen.")
               ),
               
               sliderInput("palabras_semana_max",
                           "Máximo de palabras por semana",
                           min = 4, max = 20,
                           value = 10,
                           step = 1, width = "100%",),
               div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                   em("Cantidad máxima de palabras por cada semana; limita la cantidad de palabras por semana dejando sólo las x mayores; por defecto son 10.")
               ),
               
               selectInput("texto_tamaño",
                           "Tamaño del texto",
                           choices = c("Normal" = 2.3, "Mediano" = 2.8, "Grande" = 3.2),
                           selected = c("Mediano" = 2.8), width = "100%",
               ),
               
               sliderInput("angulo",
                           "Ángulo de etiquetas",
                           min = 0, max = 60,
                           value = 40,
                           step = 10, width = "100%",),
               div(style = css(font_family = "Libre Baskervill Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                   em("Ángulo del texto de las etiquetas; alterarlo puede hacer que aparezcan más etiquetas de palabras, dado que las que se sobreponen son ocultadas.")
               )
           ) |> shinyjs::hidden(),
           
           div(
             style = css(opacity = "30%"),
             actionButton("mostrar_opciones", "Opciones avanzadas")
           )
           
    )
  ),
  
  # palabras semana ----
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
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
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
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cambie el tipo de gráfico que se usará para visualizar los datos. Por defecto, se cambia a visualización de líneas si se seleccionan muchas palabras, y a barras si se seleccionan pocas.")
           ),
    ),
    
    column(4,  style = css(max_width = "600px"),
           sliderInput("semanas_palabras",
                       "Rango de semanas",
                       min = 4*1, max = 4*4,
                       value = 4*3,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Personalice el rango de tiempo que abarcará la visualización. Por defecto, si el rango es muy amplio, se cambia a barras.")
           ),
    )
    
  ),
  fluidRow(
    column(12,
           plotOutput("g_palabras", height = "640px", width = "100%") |> withSpinner()
    )
  ),
  
  
  # barras semana fuente ----
  fluidRow(
    column(12,
           br(),
           hr(),
           h3("Palabras más mencionadas en medios, semanalmente"),
           
           markdown("Gráfico que expresa, por cada semana, las palabras más repetidas en el texto de las noticias. Las barras más largas representan palabras más frecuentes. Cada barra se forma por la proporción de las menciones de la palabra que correspondena a medios de comunicación específicos. Por ejemplo, si una barra muestra un color prevalente en ella, significa que el medio de comunicación correspondiente a ese color usó más frecuentemente el termino que los demás.")
    )
  ),
  fluidRow(
    column(4,  
           style = css(max_width = "600px"),
           sliderInput("semanas_fuentes",
                       "Rango de semanas",
                       min = 1, max = 4*2,
                       value = 4,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
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
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
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
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cantidad de palabras a mostrar por semana. Aumentar este valor aumenta la cantidad de barras, y podría permitir ver conceptos menos comunes.")
           )
    )
  ),
  fluidRow(
    column(12,
           plotOutput("g_semana_fuente", height = "640px", width = "100%") |> withSpinner()
    )
  ),
  
  
  # puntos fuente palabras ----
  fluidRow(
    column(12,
           br(),
           hr(),
           h3("Cantidad menciones de un concepto específico"),
           h5("Concepto:", em(textOutput("texto_selector_palabras_fuente", inline = TRUE))),
           
           markdown("Frecuencia total de menciones de una palabra, por semana, y separado por medios de comunicación. Seleccione un concepto, palabra, o nombre para comparar las menciones del concepto elegido entre los distintos medios de comunicación escritos. De este modo, es posible identificar si hay ciertos medios que mencionan más determinados conceptos, o medios que los evitan; o bien, la popularidad de un concepto a través del tiempo, comparada a entre distintos medios.")
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
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Elija una palabra de la lista para usarla en el gráfico. La lista está ordenada por frecuencia de palabras. Puede escribir para buscar o incluir otras palabras.")
           )
    ),
    
    column(3,  
           style = css(max_width = "600px"),
           sliderInput("semanas_fuentes_palabras",
                       "Rango de semanas",
                       min = 1, max = 4*2,
                       value = 4,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
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
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
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
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Seleccione un medio de comunicación para destacarlo en el gráfico por sobre el resto de los medios disponibles.")
           )
    )
  ),
  fluidRow(
    column(12,
           
           plotOutput("g_semana_fuente_palabra", height = "480px", width = "100%") |> withSpinner()
    )
  ),
  
  
  
  # correlación ----
  
  ## correlación general ----
  fluidRow(
    column(12,
           br(),
           hr(),
           h3("Correlación entre términos"),
           
           markdown("En este gráfico podemos elegir un concepto y obtener las palabras que son mencionadas más frecuentemente junto a ese concepto dentro de cada noticia. Por ejemplo, si una noticia habla del _presidente,_ es muy probable que también diga _Boric_ por sobre otras palabras. En este sentido, la correlación es una relación recíproca entre términos; es decir, términos que co-ocurren frecuentemente dentro de las noticias.")
    )
  ),
  fluidRow(
    column(6, #style = css(max_width = "600px"),
           selectizeInput("cor_total_palabra",
                          "Concepto principal",
                          choices = NULL, 
                          width = "100%",
                          options = list(search = TRUE, create = TRUE, placeholder = "")),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Elija una palabra de la lista para calcular la correlación de otras palabras con ella. La lista está ordenada por frecuencia de palabras. Puede escribir para buscar o incluir otras palabras.")
           ),
    ),
    
    column(6, #style = css(max_width = "600px"),
           sliderInput("cor_total_palabra_n",
                       "Cantidad de palabras",
                       min = 3, max = 10,
                       value = 5,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cantidad de palabras correlacionadas a mostrar. Aumentar este valor aumenta la cantidad de círculos, mostrando conceptos menos correlacionados.")
           )
    )
  ),
  
  fluidRow(
    column(12,
           plotOutput("g_cor_total", height = "260px", width = "100%") |> withSpinner()
    )
    
  ),
  
  
  ## correlación por fuentes ----
  fluidRow(
    column(12,
           br(),
           hr(),
           h3("Correlación de palabras por medios de comunicación"),
           
           markdown("Al igua que el gráfico anterior, en éste se expresan las palabras más correlacionadas al concepto elegido, pero desagregado por medio de comunicación. De este modo es posible comparar las palabras que más co-ocurren con el término elegido a través de los distintos medios de prensa escrita, evidenciando posibles diferencias en la forma de tratar las temáticas noticiosas.")
    )
  ),
  fluidRow(
    column(4, style = css(max_width = "600px"),
           selectizeInput("cor_fuente_palabra",
                          "Concepto principal",
                          choices = NULL, 
                          width = "100%",
                          options = list(search = TRUE, create = TRUE, placeholder = "")),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Elija una palabra de la lista para calcular la correlación de otras palabras con ella. La lista está ordenada por frecuencia de palabras. Puede escribir para buscar o incluir otras palabras.")
           ),
    ),
    
    column(4,  
           style = css(max_width = "600px"),
           sliderInput("cor_fuente_palabra_n",
                       "Cantidad de palabras",
                       min = 3, max = 10,
                       value = 5,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cantidad de palabras correlacionadas máximas a mostrar. Aumentar este valor aumenta la cantidad de círculos, mostrando conceptos menos correlacionados.")
           )
    ),
    
    column(4,  
           style = css(max_width = "600px"),
           sliderInput("cor_fuente_fuente_n",
                       "Cantidad de medios",
                       min = 2, max = 8,
                       value = 5,
                       width = "100%"),
           div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
               em("Cantidad de medios de comunicación a mostrar. Los medios se ordenan de mayor a menor de acuerdo al nivel de correlación de sus palabras.")
           )
    )
  ),
  fluidRow(
    column(12,
           uiOutput("ui_g_cor_fuente") |> withSpinner(proxy.height = 300)
    )
  ),
  
  
  
  
  
  
  # firma ----
  fluidRow(
    column(12, style = css(padding = "28px", font_size = "70%", font_family = "Libre Baskerville"),
           hr(),
           
           markdown("Desarrollado por [Bastián Olea Herrera.](https://bastian.olea.biz) usando el lenguaje de programación R. Puedes [hacerme llegar](https://x.com/bastimapache) cualquier duda, consulta, sugerencia o comentario."),
           
           markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
           
           markdown("Código de fuente de esta app y del procesamiento de los datos [disponible en GitHub.](https://github.com/bastianolea/prensa_chile)"),
           
           # cafecito ----
           div(
             style = css(max_width = "380px", margin = "auto", padding = "28px"),
             
             tags$style(HTML(".cafecito:hover {opacity: 70%; transition: 0.3s; color: black !important;} .cafecito a:hover {color: black}")),
             
             div(class = "cafecito",
                 style = paste("width: 100%; background-color: #FFDD04; transform:scale(0.7); border: 1.2px", color_detalle, "solid; border-radius: 13px;"),
                 tags$body(HTML('<script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="bastimapache" data-color="#FFDD00" data-emoji=""  data-font="Bree" data-text="Regálame un cafecito" data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></script>'))
             )
           )
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
                       selected = c("delincuencia", "corrupción", "hermosilla", "boric"),
                       server = TRUE)
  
  updateSelectizeInput(session, 'selector_palabras_fuente', 
                       # choices = c("Hermosilla", "Cubillos", "corrupción", "delincuencia", palabras_posibles),
                       choices = c(palabras_posibles),
                       # selected = "Hermosilla",
                       server = TRUE)
  
  
  updateSelectizeInput(session, 'cor_total_palabra', 
                       choices = c("Hermosilla", "Cubillos", "corrupción", "delincuencia", palabras_posibles),
                       selected = "Hermosilla",
                       server = TRUE)
  
  updateSelectizeInput(session, 'cor_fuente_palabra', 
                       choices = c("Hermosilla", "Cubillos", "corrupción", "delincuencia", palabras_posibles),
                       selected = "Hermosilla",
                       server = TRUE)
  
  
  # —----
  
  
  # cálculos ----
  # todos los bloques reactivos se intentan particionar de forma lógica según lo que los usuarios puedan seleccionar,
  # de modo que se realice la menor cantidad de cálculos si un usuario cambia algo
  
  ## líneas semanas ----
  datos_conteo_semanas_1 <- reactive({
    message("datos líneas semanas 1")
    palabras_semana |> 
      # límite de fecha
      filter(fecha >= today() - weeks(input$semanas)) |>
      # dejar solo top palabras semana por porcentaje de semana
      filter(p_palabra_semana > (input$frecuencia_min)/100)
  })
  
  datos_conteo_semanas_2 <- reactive({
    req(datos_conteo_semanas_1())
    message("datos líneas semanas 2")
    # # dejar solo top palabras total (maximo de palabras visibles)
    # mutate(palabra_lump = fct_lump(palabra, w = freq_total_palabra, 
    #                                n = 25, other_level = "otras")) |> 
    # filter(palabra_lump != "otras") |> 
    # sacar palabras que salen una sola vez
    # add_count(palabra, name = "palabra_n") |>
    # filter(palabra_n > 1) |>
    datos_conteo_semanas_1() |> 
      # sacar semanas donde hayan pocos términos (indicio de error)
      group_by(semana) |> 
      mutate(semana_n = n()) |> 
      filter(semana_n > 2) |> 
      # dejar solo top 10 palabras por semana
      group_by(semana) |>
      slice_max(n, n = input$palabras_semana_max)
  })
  
  datos_conteo_semanas_3 <- reactive({
    req(datos_conteo_semanas_2())
    message("datos líneas semanas 3")
    
    datos_conteo_semanas_2() |> 
      filter(!palabra %in% input$palabras_excluir) |> 
      # ordenar palabras por frecuencia
      ungroup() |> 
      mutate(palabra = fct_reorder(palabra, freq_total_palabra)) |> 
      # etiquetas hacia la izquierda
      mutate(inv = ifelse(semana == min(semana) | n < mean(n)*0.8, TRUE, FALSE))
  })
  
  datos_conteo_semanas_4 <- reactive({
    req(datos_conteo_semanas_3())
    message("datos líneas semanas 4")
    
    if (input$palabras_semanas_tipo == "Porcentaje") {
      datos_conteo_semanas_3() |> 
        group_by(semana) |>
        mutate(n = n/sum(n)) |>
        ungroup()
    } else {
      datos_conteo_semanas_3()
    }
  })
  
  # para el selector de palabras destacadas, cosa que solo contenga palabras que aparecen en el gráfico en vez de todas
  selector_semanas_palabras <- reactive({
    message("selector líneas semanas")
    datos_conteo_semanas_3() |> 
      select(palabra, freq_total_palabra) |> 
      arrange(desc(freq_total_palabra)) |> 
      pull(palabra) |> 
      unique() |> 
      as.character()
  })
  
  
  ## palabras semana ----
  datos_conteo_semanas_palabras_1 <- reactive({
    palabras_semana |> 
      filter(fecha > today() - weeks(input$semanas_palabras))
  })
  
  datos_conteo_semanas_palabras_2 <- reactive({
    req(input$selector_palabras != "")
    
    datos_conteo_semanas_palabras_1() |> 
      filter(palabra %in% input$selector_palabras) |> 
      group_by(palabra) |> 
      mutate(freq_total_palabra = sum(n)) |> 
      ungroup() |> 
      mutate(palabra = fct_reorder(palabra, freq_total_palabra, .desc = T)) |> 
      # otros datos
      group_by(semana) |> 
      mutate(n_semana = sum(n),
             # rank = dense_rank(desc(n))) |> 
             rank = row_number(desc(n))) |> 
      ungroup() |> 
      mutate(prom = mean(n)) |> 
      mutate(chico = ifelse(n > prom*0.8, FALSE, TRUE)) |> 
      # fechas
      group_by(semana) |> 
      mutate(fecha_etiqueta = redactar_fecha(min(fecha)),
             fecha_etiqueta = fct_reorder(fecha_etiqueta, semana)) |> 
      ungroup()
  })
  
  
  ## barras semana fuente ----
  # top palabras por semana
  # datos_semana_fuente_1 <- reactive({
  #   palabras_semana_fuente |>
  #     group_by(fuente, semana) |>
  #     slice_max(n, n = 60) # palabras por fuente
  # })
  
  datos_semana_fuente_2 <- reactive({
    .semanas = (week(today())-(input$semanas_fuentes-1)):week(today()) #29:32
    
    palabras_semana_fuente |> 
      filter(semana %in% .semanas) |> 
      ungroup()
  })
  
  datos_semana_fuente_3 <- reactive({
    datos_semana_fuente_2() |> 
      # agrupar fuentes chicas
      mutate(fuente = fct_reorder(fuente, n_total_fuente, .desc = FALSE)) |>
      mutate(fuente = fct_lump(fuente, w = n_total_fuente, n = input$semana_fuentes_fuentes, ties.method = "first", 
                               other_level = "Otros")) |>
      group_by(fuente, semana, fecha, palabra) |>
      summarize(n = sum(n), .groups = "drop")
  })
  
  datos_semana_fuente_4 <- reactive({
    datos_semana_fuente_3() |> 
      # maximo palabras por semana
      group_by(semana, palabra) |> 
      mutate(n_semana = sum(n)) |>
      group_by(semana) |>
      mutate(rank = dense_rank(desc(n_semana))) |>
      # mutate(rank2 = row_number(desc(n_semana))) |>
      filter(rank <= input$semana_fuentes_palabras_n) |> # cantidad de palabras por semana
      distinct(semana, rank, fuente, .keep_all = TRUE) |>
      # ordenar palabras
      group_by(semana, palabra) |> 
      mutate(n_palabra_semana = sum(n)) |> 
      group_by(semana) |> 
      mutate(palabra = tidytext::reorder_within(palabra, n_palabra_semana, semana)) |> 
      ungroup()
  })
  
  ## puntos fuente palabra ----
  # palabra específica por fuente
  datos_semana_fuente_palabra_1 <- reactive({
    req(input$selector_palabras_fuente != "")
    
    palabras_semana_fuente |>
      filter(palabra == tolower(input$selector_palabras_fuente))
  })
  
  datos_semana_fuente_palabra_2 <- reactive({
    # browser()
    .semanas = (week(today())-(input$semanas_fuentes_palabras-1)):week(today()) #29:32
    
    datos_semana_fuente_palabra_1() |> 
      # filtrar semanas
      filter(semana %in% .semanas) |> 
      # # ranking de fuentes con mayor cantidad de palabras
      group_by(fuente) |>
      mutate(n_total_fuente = sum(n)) |>
      ungroup() 
  })
  
  datos_semana_fuente_palabra_3 <- reactive({
    .n_fuentes = input$semana_fuentes_palabras_fuentes
    
    datos_semana_fuente_palabra_2() |>
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
  })
  
  datos_semana_fuente_palabra_4 <- reactive({
    if (input$destacar_medio == "Ninguno") {
      datos_semana_fuente_palabra_3() |> 
        mutate(destacado = "Ninguno")
      
    } else if (input$destacar_medio != "Ninguno") {
      datos_semana_fuente_palabra_3() |> 
        mutate(destacado = ifelse(fuente == input$destacar_medio, 
                                  "Destacado", "Otros"))
    }
  })
  
  
  ## correlación ----
  
  ### correlación general ----
  cor_total_dato_1 <- reactive({
    req(input$cor_total_palabra != "")
    
    cor_filt <- correlacion |>
      rename(palabra1 = item1, palabra2 = item2, correlacion = correlation) |> 
      filter(palabra1 == tolower(input$cor_total_palabra))
    
    return(cor_filt)
  })
  
  cor_total_dato_2 <- reactive({
    req(cor_total_dato_1() |> nrow() > 1)
    
    .palabras_excluir = c("luis")
    
    cor_total_dato_1() |> 
      filter(!palabra2 %in% .palabras_excluir) |>
      ungroup() |> 
      slice_max(correlacion, n = input$cor_total_palabra_n)
  })
  
  
  ### correlación fuentes ----
  cor_fuente_dato_1 <- reactive({
    req(input$cor_fuente_palabra != "")
    
    correlacion_fuente |>
      rename(palabra1 = item1, palabra2 = item2, correlacion = correlation) |> 
      filter(palabra1 == tolower(input$cor_fuente_palabra))
  })
  
  cor_fuente_dato_2 <- reactive({
    req(input$cor_fuente_palabra != "")
    .palabras_excluir = c("luis")
    
    cor_fuente_dato_1() |> 
      # palabras excluidas
      filter(!palabra2 %in% .palabras_excluir) |>
      # maximo de términos por fuente
      group_by(fuente) |> 
      slice_max(correlacion, n = input$cor_fuente_palabra_n)
  })
  
  cor_fuente_dato_3 <- reactive({
    cor_fuente_dato_2() |> 
      # ranking de fuentes
      group_by(fuente) |> 
      mutate(cor_total = sum(correlacion)) |> 
      ungroup() |> 
      mutate(rank_fuente = dense_rank(desc(cor_total))) |>
      filter(rank_fuente <= input$cor_fuente_fuente_n)
  })
  
  
  # —----
  # gráficos ----
  # la idea es que los datos pasen ya procesados acá y esta sección sólo se enfoque en las visualizaciones
  
  ## líneas semanas ----
  output$g_semanas <- renderPlot({
    req(input$destacar_palabra != "")
    req(datos_conteo_semanas_4())
    # browser()
    message("gráfico líneas semanas")
    
    datos <- datos_conteo_semanas_4()
    
    # opciones gráfico
    .dodge = 3
    .espaciado_y = 0.08
    .espaciado_x = 0.05
    
    # hace que los colores del gráfico sean por palabras, o por palabra destacada vs otras
    variable <- ifelse(input$destacar_palabra != "Ninguna",
                       "destacar", "palabra")
    
    if (input$destacar_palabra != "Ninguna") {
      # crea una variable dicotómica con la palabra destacada
      datos3 <- datos |> 
        mutate(destacar = ifelse(palabra == tolower(input$destacar_palabra), 
                                 tolower(input$destacar_palabra), "otras"),
               destacar = fct_relevel(destacar, "otras", after = 0))
    } else {
      datos3 <- datos
    }
    
    #gráfico 
    plot <- datos3 |> #datos_conteo_semanas() |> 
      # ggplot(aes(fecha, n)) +
      ggplot(aes(fecha, n)) +
      geom_step(aes(color = !!sym(variable), group = palabra),
                linewidth = .9, alpha = .5,
                direction = "mid", 
                position = position_dodge(.dodge),
                show.legend = F) +
      geom_point(aes(group = palabra),
                 size = 3, color = color_fondo, 
                 position = position_dodge(.dodge)) +
      geom_point(aes(color = !!sym(variable), group = palabra),
                 size = 2, position = position_dodge(.dodge)) +
      # texto
      shadowtext::geom_shadowtext(
        aes(label = ifelse(inv, paste(palabra, "  "), paste("  ", palabra)),
            hjust = ifelse(inv, 1, 0),
            color = !!sym(variable), group = palabra),
        bg.colour = color_fondo, bg.r = 0.3, angle = input$angulo, size = as.numeric(input$texto_tamaño), vjust = 0.3, 
        position = position_dodge(.dodge), check_overlap = T, show.legend = F) +
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
            axis.text.y = element_text(family = "Lato", size = .texto_ejes),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.text.x = element_text(family = "Lato", size = .texto_ejes, hjust = 1, angle = input$angulo),
            # panel.background = element_rect(fill = color_destacado),
            plot.caption = element_text(color = color_detalle)) +
      labs(y = "frecuencia de palabras por semana",
           x = NULL)
    # title = "Conceptos principales en prensa, por semana", 
    # caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile"
    
    # escala porcentual
    if (input$palabras_semanas_tipo == "Porcentaje") {
      plot <- plot +
        scale_y_continuous(expand = expansion(c(.espaciado_y*0.7, .espaciado_y)),
                           labels = scales::label_percent()) +
        labs(y = "porcentaje de palabras más frecuentes por semana")
    }
    
    # paleta de colores si se destaca una palabra
    if (input$destacar_palabra != "Ninguna") {
      plot <- plot +
        scale_color_manual(values = c(color_texto, color_destacado))
    } else {
      plot <- plot +
        scale_color_viridis_d(begin = .2, end = .7, option="magma")
    }
    
    return(plot)
    
  }, res = 100)
  
  
  ## palabras semana ----
  
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
    # if (length(input$selector_palabras) > 4) {
    if (input$tipo_grafico == "Líneas") {
      ### líneas ----
      plot <- datos_conteo_semanas_palabras_2() |> 
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
      plot <- datos_conteo_semanas_palabras_2() |>
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
        scale_fill_viridis_d(begin = .2, end = .7, direction = -1, option = "magma", aesthetics = c("color", "fill")) +
        scale_y_continuous(expand = expansion(c(0, 0.05))) +
        guides(fill = guide_none(),
               color = guide_legend(reverse = FALSE, position = "right", 
                                    override.aes = list(alpha = 1, size = 4)))
    }
    
    plot <- plot +
      theme(legend.text = element_text(margin = margin(l = 2))) +
      theme(panel.grid.major.x = element_line(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(family = "Lato", size = .texto_ejes),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.text.x = element_text(family = "Lato", size = .texto_ejes, hjust = 1),
            legend.title = element_text(family = "Libre Baskerville", face = "italic"),
            plot.caption = element_text(color = color_detalle)) +
      theme(axis.text.x = element_text(family = "Lato", angle = 40)) + #, hjust = 1, angle = 40))
      labs(color = "Palabras", y = "frecuencia de palabras", x = NULL
           # caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile"
      )
    
    
    return(plot)
  }, res = 100)
  
  
  ## barras semana fuente ----
  output$g_semana_fuente <- renderPlot({
    
    plot <- datos_semana_fuente_4() |> 
      ggplot(aes(x = n, y = palabra, fill = fuente)) +
      geom_col(width = .7, color = color_fondo) +
      geom_point(aes(color = fuente), alpha = 0) +
      tidytext::scale_y_reordered() +
      scale_x_continuous(expand = expansion(c(0, .1))) +
      scale_fill_viridis_d(begin = .2, direction = 1, end = .7, option = "magma", aesthetics = c("color", "fill")) +
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
            axis.text.y = element_text(family = "Lato", size = .texto_ejes),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.title.x = element_text(family = "Libre Baskerville", face = "italic",
                                        margin = margin(t = 6, b = -10)),
            axis.text.x = element_text(family = "Lato", size = .texto_ejes, hjust = 1, angle = 40),
            legend.title = element_text(family = "Libre Baskerville", face = "italic"),
            plot.caption = element_text(color = color_detalle))
    
    return(plot)
  }, res = 100)
  
  
  ## puntos fuente palabra ----
  
  output$texto_selector_palabras_fuente <- renderText({
    str_to_sentence(input$selector_palabras_fuente)
  })
  
  output$g_semana_fuente_palabra <- renderPlot({
    plot <- datos_semana_fuente_palabra_4() |> 
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
            axis.text.y = element_text(family = "Lato", size = .texto_ejes),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.title.x = element_text(family = "Libre Baskerville", face = "italic",
                                        margin = margin(t=6)),
            axis.text.x = element_text(family = "Lato", size = .texto_ejes, hjust = 1, vjust = .5, angle = 90),
            legend.title = element_text(family = "Libre Baskerville", face = "italic"),
            plot.caption = element_text(color = color_detalle))
    
    return(plot)
  }, res = 100)
  
  
  ## correlación ----
  ### correlación general ----
  output$g_cor_total <- renderPlot({
    
    dato <- cor_total_dato_2() |> 
      mutate(tamaño = scales::rescale(correlacion, to = c(1.2, 2), 
                                      from = range(min(correlacion), .7)),
             tamaño = ifelse(tamaño > 2, 2, tamaño) # from = range(correlacion, na.rm = TRUE, finite = TRUE))
      ) |> 
      mutate(orden = dense_rank(desc(correlacion))) |>
      # mutate(orden = row_number(desc(correlacion))) |> 
      distinct(orden, .keep_all = TRUE) |> 
      mutate(palabra2 = forcats::fct_reorder(palabra2, correlacion, .desc = TRUE))
    
    plot <- dato |> 
      ggplot(aes(x = 1, y = 1, 
                 fill = correlacion, color = correlacion)) +
      ggforce::geom_circle(aes(x0 = 1, y0 = 1, r = 2), alpha = .2, linewidth = .1) +
      ggforce::geom_circle(aes(x0 = 1, y0 = 1, r = tamaño), alpha = .9) +
      shadowtext::geom_shadowtext(aes(label = palabra2),
                                  bg.colour = color_fondo, bg.r = 0.2, color = color_negro, size = 3.2) +
      geom_text(aes(label = round(correlacion, 3), y = -1.5), vjust = 1, size = 2.6, alpha = .5) +
      guides(color = guide_none(), fill = guide_none(), size = guide_none()) +
      theme(strip.background = element_blank(), strip.text = element_blank(),
            axis.title.x = element_text(family = "Libre Baskerville", face = "italic", margin = margin(t = 8)),
            axis.ticks = element_blank(), axis.text = element_blank(),
            panel.grid = element_blank(), panel.background = element_blank()) +
      coord_equal(clip = "off") +
      scale_y_continuous(limits = c(-1.5, 3)) +
      facet_grid(rows = NULL, cols = vars(orden), drop = T) +
      labs(x = paste("términos más correlacionados con:", str_to_sentence(input$cor_total_palabra)), y = NULL)
    
    
    return(plot)
  }, res = 100)
  
  
  ### correlación fuentes ----
  output$g_cor_fuente <- renderPlot({
    req(cor_fuente_dato_3())
    message("gráfico correlación fuentes")
    
    dato <- cor_fuente_dato_3() |> 
      mutate(tamaño = scales::rescale(correlacion, to = c(1.2, 2), 
                                      from = range(min(correlacion), .7)),
             tamaño = ifelse(tamaño > 2, 2, tamaño) # from = range(correlacion, na.rm = TRUE, finite = TRUE))
      ) |> 
      group_by(fuente) |> 
      mutate(orden = dense_rank(desc(correlacion))) |>
      distinct(fuente, orden, .keep_all = TRUE) |> 
      recodificar_fuentes() |> 
      mutate(palabra2 = forcats::fct_reorder(palabra2, correlacion, .desc = TRUE))
    
    plot <- dato |> 
      ggplot(aes(x = 1, y = 1, 
                 fill = correlacion, color = correlacion)) +
      ggforce::geom_circle(aes(x0 = 1, y0 = 1, r = 2), alpha = .2, linewidth = .1) +
      ggforce::geom_circle(aes(x0 = 1, y0 = 1, r = tamaño), alpha = .9) +
      shadowtext::geom_shadowtext(aes(label = palabra2),
                                  bg.colour = color_fondo, bg.r = 0.2, color = color_negro, size = 3.2) +
      geom_text(aes(label = round(correlacion, 3), y = -1.5), vjust = 1, size = 2.6, alpha = .5) +
      guides(color = guide_none(), fill = guide_none(), size = guide_none()) +
      theme(strip.background = element_blank(), strip.text = element_text(face = "bold", vjust = 1),
            strip.text.x = element_blank(),
            axis.ticks = element_blank(), axis.text = element_blank(),
            axis.title.x = element_text(family = "Libre Baskerville", face = "italic", margin = margin(t = 8)),
            panel.grid = element_blank(), panel.background = element_blank()) +
      coord_equal(clip = "off") +
      scale_y_continuous(limits = c(-1.5, 3)) +
      facet_grid(rows = vars(fuente), cols = vars(orden), drop = T, switch = "y") +
      labs(x = paste("términos más correlacionados con:", str_to_sentence(input$cor_fuente_palabra)), y = NULL)
    
    return(plot)
  }, res = 100)
  
  output$ui_g_cor_fuente <- renderUI({
    req(cor_fuente_dato_3())
    message("ui correlación fuentes")
    
    div(style = css(margin_bottom = "0px"),
        plotOutput("g_cor_fuente",
                   height = 60 + (input$cor_fuente_fuente_n*150),
                   width = "100%") |> withSpinner(proxy.height = 300)
    )
    # height = reactive(60 + (input$cor_fuente_fuente_n*150))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
