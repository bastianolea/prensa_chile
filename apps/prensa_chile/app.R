library(shiny) |> suppressPackageStartupMessages()

library(dplyr) |> suppressPackageStartupMessages()
library(arrow) |> suppressPackageStartupMessages()

library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(lubridate) |> suppressPackageStartupMessages()
library(shadowtext)

library(bslib) |> suppressPackageStartupMessages()
library(htmltools)
library(shinyjs) |> suppressPackageStartupMessages()
library(thematic)
library(shinyWidgets)
library(shinycssloaders)
library(sysfonts)
library(showtext)
library(ragg)

source("funciones.R")


# colores ----
color_fondo = "#EEDABF"
color_texto = "#866C53"
color_negro = "#694E34"
color_detalle = "#A5876A"
color_destacado = "#C7392B"


# tipografías ----
sysfonts::font_add("Lato",
                   regular = "www/fonts/lato-v24-latin-regular.ttf",
                   bold = "www/fonts/lato-v24-latin-900.ttf",
                   italic = "www/fonts/lato-v24-latin-italic.ttf",
                   bolditalic = "www/fonts/lato-v24-latin-900italic.ttf",
)
sysfonts::font_add("Libre Baskerville",
                   regular = "www/fonts/libre-baskerville-v14-latin-regular.ttf",
                   bold = "www/fonts/libre-baskerville-v14-latin-700.ttf",
                   italic = "www/fonts/libre-baskerville-v14-latin-italic.ttf",
                   bolditalic = "www/fonts/libre-baskerville-v14-latin-italic.ttf",
)

# para gráficos
tipografia <- list(sans = "Lato",
                   # serif = "Libre Baskerville"
                   # serif = "Libre Baskerville Italic" # no aparece en shinyapps
                   serif = "Lato"
)

# showtext_auto() # esto echaba a perder la resolución


# configuraciones ----
thematic_shiny(font = c("Lato", "Libre Baskerville"), 
               accent = color_destacado)
options(spinner.type = 8, spinner.color = color_detalle)
options(shiny.useragg = TRUE)
# showtext::showtext_opts(dpi = 180) # esto junto al showtext_auto() echa a perder la resolución
.texto_ejes = 9
resolucion = 110


# cargar datos ----
palabras_semana <- read_parquet("palabras_semana.parquet")
palabras_semana_fuente <- read_parquet("palabras_semana_fuente.parquet")
correlacion <- read_parquet("prensa_correlacion.parquet")
correlacion_fuente <- read_parquet("prensa_correlacion_fuente.parquet")
sentimiento <- read_parquet("prensa_sentimiento.parquet")

n_noticias <- readLines("prensa_n_noticias.txt") |> as.numeric() |> format(big.mark = ".", decimal.mark = ",")
n_palabras <- readLines("prensa_n_palabras.txt") |> as.numeric()


# vectores ----
palabras_posibles <- palabras_semana |> 
  group_by(palabra) |> 
  summarize(n = sum(n)) |> 
  arrange(desc(n)) |> 
  filter(n > 100) |> 
  pull(palabra)

fuentes <- palabras_semana_fuente$fuente |> unique() |> sort()

topicos <- sentimiento$clasificacion |> unique() |> na.exclude()



# ui ----
ui <- page_fluid(
  title = "Análisis de Prensa en Chile", 
  lang = "es",
  
  # tipografías en html
  # gfonts::use_font("lato", "www/css/lato.css"),
  # gfonts::use_font("libre-baskerville", "www/css/libre-baskerville.css"),
  # fresh::use_googlefont("Lato"),
  # fresh::use_googlefont("Libre Baskerville"),
  # fresh::use_googlefont("Libre Baskerville Italic"),
  
  # aplicar tipografías al texto del sitio
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/libre-baskerville.css")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/libre-baskerville.css"),
    tags$style("* {font-family:'Libre Baskerville' !important;}")
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/lato.css")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/lato.css"),
    tags$style("p, label, item {font-family:'Lato' !important;}")
  ),
  
  ## tema ----
  # tags$head(HTML('<link rel="preconnect" href="https://fonts.googleapis.com">')),
  # tags$head(HTML('<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>')),
  
  theme = bslib::bs_theme(
    font_scale = 1.2,
    bg = color_fondo, fg = color_texto, primary = color_destacado, 
    # tipografías 
    base_font = "Lato",
    heading_font = "Libre Baskerville"
  ),
  
  shinyjs::useShinyjs(),
  
  ## css ----
  tags$style(
    HTML("a { color: ", color_detalle, "}")
  ),
  tags$style(
    HTML(".selectize-input.items.not-full { color: red !important;}")),
  
  # color del texto placeholder de pickerInput
  tags$style(
    HTML(".dropdown-toggle.bs-placeholder {
    color: unset !important;}")),
  
  # —----
  
  # header ----
  div(style = css(margin_top = "12px", margin_bottom = "12px"),
      h1("Análisis de prensa en Chile",
         style = css(font_style = "italic")),
      
      div(style = css("color!" = color_detalle,
                      opacity = "60%",
                      font_size = "80%",
                      text_decoration = "none"),
          
          em(tags$a("Bastián Olea Herrera", href = "https://bastianolea.rbind.io"),
          ),
          br(),
          
          em("Última actualización de datos:", textOutput("ultimos_datos_fecha", inline = TRUE)
          )
      )
  ),
  
  
  # texto introducción
  div(
    markdown(
      paste0("Proyecto de _análisis de texto_ de noticias publicadas por medios de comunicación digitales de Chile. Actualmente, el material obtenido supera las **", n_noticias, " noticias** individuales, 
                    las cuales suman un total de **", n_palabras/1000000, " millones de palabras**, abarcando más de 20 fuentes periodísticas distintas.")),
    markdown("Un sistema automatizado obtiene y procesa grandes cantidades de datos de noticias diariamente, creando una base de datos de noticias con su texto (título, cuerpo, bajada) y metadatos (fecha, fuente, dirección web). A partir de esta información es posible realizar distintos análisis sobre el texto. Este visualizador permite describir _sobre qué_ hablan las noticias en determinadas fechas, en intervalos de semanas, y fuentes periodísticas."),
    markdown("Para más información técnica sobre este proyecto, [visite el repositorio](https://github.com/bastianolea/prensa_chile)."),
    hr()
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
           div(style = css(overflow_x = "scroll"),
               div(style = css(min_width = "600px"),
                   plotOutput("g_semanas", 
                              width = "100%", height = "640px") |> withSpinner()
               )
           ),
           
           markdown("Todos los gráficos se generan automáticamente, mediante de un proceso automatizado de obtención de textos y procesamiento de datos.
                    A partir de todas las noticias publicadas por los medios comunicacionales escritos online, semana a semana, se transforman todas las noticias en palabras separadas, y se cuenta la repetición de cada palabra, tomando como una repetición las distintas conjugaciones de cada palabra (por ejemplo, _delincuente_ y _delincuencia_ cuentan como una sola palabra repetida 2 veces). Luego, se eliminan palabras irrelevantes (como artículos, pronombres y otras), y se genera un ranking de las palabras más frecuentes para cada semana."),
           
    ),
    column(3,
           ## opciones
           # h5("Opciones del gráfico"),
           
           sliderInput("semanas",
                       "Rango de semanas",
                       min = 2, max = 4*3,
                       value = 8, width = "100%",),
           
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
                       choices = c("Porcentaje", "Frecuencia"),
                       selected = "Frecuencia"
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
           div(style = css(overflow_x = "scroll"),
               div(style = css(min_width = "600px"),
                   plotOutput("g_palabras", height = "640px", width = "100%") |> withSpinner()
               ))
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
           div(style = css(overflow_x = "scroll"),
               div(style = css(min_width = "900px"),
                   plotOutput("g_semana_fuente", height = "640px", width = "100%") |> withSpinner()
               ))
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
           div(style = css(overflow_x = "scroll"),
               div(style = css(min_width = "900px"),
                   plotOutput("g_semana_fuente_palabra", height = "480px", width = "100%") |> withSpinner()
               ))
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
           div(style = css(overflow_x = "scroll"),
               div(style = css(min_width = "600px"),
                   plotOutput("g_cor_total", height = "260px", width = "100%") |> withSpinner()
               ))
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
           div(style = css(overflow_x = "scroll"),
               div(style = css(min_width = "600px"),
                   uiOutput("ui_g_cor_fuente") |> withSpinner(proxy.height = 300)
               ))
    )
  ),
  
  hr(),

  # sentimiento ----
  div(
    h3("Análisis de sentimiento"),

    markdown("Para esta visualización, se utilizó un [modelo extenso de lenguaje (LLM) de inteligencia artificial](https://bastianolea.rbind.io/blog/analisis_sentimiento_llm/) para procesar el contenido de cada noticia, y en base a su texto, asignarle un sentimiento y un tema.
             Por _sentimiento_ nos referimos a si el contenido semántico del texto describe un suceso positivo, neutro o negativo; por ejemplo, una noticia sobre un suceso trágico será negativa. Por _tema_ nos referimos a la clasificación de los textos noticiosos en distintas categorías temáticas o tópicos, como pueden ser noticias sobre política, economía, policial, etc."),

    layout_columns(col_widths = c(4, 4, 4),
                   div(
                     pickerInput("sentimiento_tema",
                                 "Seleccionar temas",
                                 choices = c("Todos", topicos),
                                 selected = "Todos",
                                 multiple = FALSE,
                                 width = "100%",
                                 options = pickerOptions(maxOptions = 1,
                                                         maxOptionsText = "Máximo 1",
                                                         noneSelectedText = "Todos")
                     ),
                     div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                         em("Visualice noticias sin distinguir entre sus temáticas, o seleccione temáticas para filtrar los resultados sólo considerando noticias clasificadas en temas específicos.")
                     )
                   ),
                   div(
                     pickerInput("sentimiento_fuente",
                                 "Seleccionar medios",
                                 choices = c(fuentes),
                                 selected = NULL,
                                 multiple = TRUE,
                                 width = "100%",
                                 options = pickerOptions(maxOptions = 3,
                                                         maxOptionsText = "Máximo 3",
                                                         noneSelectedText = "Todos")
                     ),
                     div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                         em("Visualice todas las noticias, o seleccione algunos medios de comunicación para filtrar y separar la visualización. Des-seleccione los medios para volver a ver los resultados de todas las noticias.")
                     )
                   ),
                   div(
                     sliderInput("sentimiento_semanas",
                                 "Rango de semanas",
                                 min = 4*1, max = 4*4,
                                 value = 4*2,
                                 width = "100%"),
                     div(style = css(font_family = "Libre Baskerville Italic", font_size = "70%", margin_top = "-8px", margin_bottom = "16px"),
                         em("Rango de tiempo que abarcará la visualización.")
                     )
                   )
    ),

    # interpretación
    div(
      markdown("Las barras indican el promedio de sentimiento semanal de las noticias: si se acerca a la parte superior significa que la mayoría de las noticias fueron positivas, y si se acerca a la inferior, que fueron negativas. Si la barra está cerca a la línea central, significa que las noticias fueron en promedio neutras (ni buenas ni malas), o bien, que hubo un balance entre noticias positivas y negativas.")
      ),
    
    # gráfico
    div(
      div(style = css(overflow_x = "scroll"),
          div(style = css(min_width = "600px"),
              plotOutput("g_sentimiento", height = "480px", width = "100%") |> withSpinner()
          ))
    ),
    
    
    # disclaimer ia
    div(style = css(font_size = "70%", margin_top = "6px"),
        markdown("_Recordar que los modelos de inteligencia artificial pueden cometer errores y clasificar textos de forma incorrecta. Si en este procesamiento de datos no han supervisado, las pruebas manuales que hemos realizado indican que los resultados suelen ser altamente certeros, pero ciertas noticias complejas o ambiguas pueden confundir al modelo._")
    )
  ),
  
  
  
  # firma ----
  fluidRow(
    column(12, style = css(padding = "28px", font_size = "70%", font_family = tipografia$serif),
           hr(),
           
           markdown("Desarrollado por [Bastián Olea Herrera.](https://bastianolea.rbind.io) usando el lenguaje de programación R. Puedes [hacerme llegar](https://bastianolea.rbind.io/contact/) cualquier duda, consulta, sugerencia o comentario."),
           
           markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
           
           markdown("Código de fuente de esta app y del procesamiento de los datos [disponible en GitHub.](https://github.com/bastianolea/prensa_chile)"),
           
           # cafecito ----
           div(style = css(overflow_x = "scroll"),
               div(style = css(width = "350px", margin = "auto", padding = "0px"),
                   
                   tags$style(HTML(".cafecito:hover {opacity: 70%; transition: 0.3s; color: black !important;} .cafecito a:hover {color: black}")),
                   
                   div(class = "cafecito",
                       style = paste("width: 100%; background-color: #FFDD04; transform:scale(0.5); border: 1.2px", color_detalle, "solid; border-radius: 13px;"),
                       tags$body(HTML('<script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="bastimapache" data-color="#FFDD00" data-emoji=""  data-font="Bree" data-text="Regálame un cafecito" data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></script>'))
                   )
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
                       choices = c("delincuencia", "corrupción", "Hermosilla", "Boric", palabras_posibles),
                       selected = c("delincuencia", "corrupción", "Hermosilla", "Boric"),
                       server = TRUE)
  
  updateSelectizeInput(session, 'selector_palabras_fuente', 
                       # choices = c("Hermosilla", "Cubillos", "corrupción", "delincuencia", palabras_posibles),
                       choices = c(palabras_posibles),
                       # selected = "Hermosilla",
                       server = TRUE)
  
  
  updateSelectizeInput(session, 'cor_total_palabra', 
                       choices = c("Hermosilla", "Boric", "corrupción", "delincuencia", palabras_posibles),
                       selected = "Hermosilla",
                       server = TRUE)
  
  updateSelectizeInput(session, 'cor_fuente_palabra', 
                       choices = c("Hermosilla", "Boric", "corrupción", "delincuencia", palabras_posibles),
                       selected = "Hermosilla",
                       server = TRUE)
  
  
  # —----
  
  
  output$ultimos_datos_fecha <- renderText({
    
    format(file.info("palabras_semana.parquet")$mtime,
           "%d/%m/%Y")
  })
  
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
    # browser()
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
      filter(palabra %in% tolower(input$selector_palabras)) |> 
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
    # retorna vector con numero de las semanas a mostrar
    # .semanas = (week(today())-(input$semanas_fuentes-1)):week(today())
    
    # browser()
    # # bug: no puede filtrar por semanas de distintos años, porque nada indica el año
    # # entonces sería como desde semana 50 a semana 1
    # # también la resta está mal hecha en consideración de semanas entre dos años
    # # tampoco viene con fecha, habría que filtrar por fecha
    semanas_atras = weeks(input$semanas_fuentes+1)
    fecha_min = today() - semanas_atras
    fecha_max = today()
    
    # palabras_semana_fuente |> 
    #   filter(semana %in% .semanas) |> 
    #   ungroup()
    # 
    # palabras_semana_fuente |> 
    #   distinct(semana, fecha, fecha_texto)
    
    palabras_semana_fuente |> 
      filter(fecha >= fecha_min,
             fecha <= fecha_max)
  })
  
  datos_semana_fuente_3 <- reactive({
    # browser()
    datos_semana_fuente_2() |> 
      # agrupar fuentes chicas
      mutate(fuente = fct_reorder(fuente, n_total_fuente, .desc = FALSE)) |>
      mutate(fuente = fct_lump(fuente, w = n_total_fuente, n = input$semana_fuentes_fuentes, ties.method = "first", 
                               other_level = "Otros")) |>
      group_by(fuente, semana, fecha, fecha_texto, palabra) |>
      summarize(n = sum(n), .groups = "drop")
  })
  
  # 
  # datos_semana_fuente_4 <- reactive({
  #   browser()
  #   
  #   datos_1 <- datos_semana_fuente_3() |> 
  #     # filter(semana == 4) |> 
  #     group_by(semana, fecha_texto, palabra) |> 
  #     summarize(fuente = list(c(fuente, n)),
  #               n_semana = sum(n)) |> 
  #     group_by(semana) |> 
  #     arrange(semana, desc(n_semana)) |> 
  #     mutate(rank = row_number(desc(n_semana))) |> 
  #     filter(rank <= input$semana_fuentes_palabras_n)
  #     
  #   # 
  #   # datos_1 <- datos_semana_fuente_3() |> 
  #   #   # maximo palabras por semana
  #   #   group_by(semana, palabra) |> 
  #   #   mutate(n_semana = sum(n)) |>
  #   #   arrange(semana, desc(n_semana)) |> 
  #   #   group_by(semana) |>
  #   #   mutate(rank = dense_rank(desc(n_semana))) |> 
  #   #   ungroup() |> 
  #   #   # mutate(rank2 = row_number(desc(n_semana))) |>
  #   #   filter(rank <= input$semana_fuentes_palabras_n) |> # cantidad de palabras por semana
  #   #   # distinct(semana, rank) |> 
  #   #   # add_count(semana) |> print(n=Inf)
  #   #   arrange(semana, rank)
  #   
  #  
  #   # el filtro tiene que ser contando 15 palabras por cada semana, 
  #   # indiferente a si la palabra sale en multiples fuentes
  #   
  #   # palabras <- datos_1 |> 
  #   #   arrange(semana, rank) |> 
  #   #   distinct(semana, palabra, rank) |> 
  #   #   rename(palabra_rank = palabra) |> 
  #   #   select(-rank) |> 
  #   #   tidyr::nest(palabra_rank = palabra_rank)
  #   
  #   # palabras <- datos_1 |> 
  #   #   arrange(semana, rank) |> 
  #   #   distinct(semana, palabra, rank) |> 
  #   #   rename(palabra_rank = palabra) |> 
  #   #   select(-rank) |> 
  #   #   group_by(semana) |> 
  #   #   summarize(palabra_rank = list(palabra_rank))
  #   
  #   # palabras <-  tibble(semana = 4, 
  #   #        palabra_rank = list("presidente"))
  #   
  #   datos_2 <- datos_1 |> 
  #     # ordenar palabras
  #     group_by(semana, rank) |> 
  #     mutate(n_palabra_semana = sum(n_semana)) |> 
  #     group_by(semana) |> 
  #     mutate(palabra = tidytext::reorder_within(palabra,
  #                                               # semana,
  #                                               n_palabra_semana,
  #                                               semana)) |> 
  #     ungroup()
  #   
  #   # datos_2
  #   # datos_2 |> 
  #   #   print(n=100)
  #   
  #   datos_3 <- datos_2 |> 
  #     tidyr::unnest_longer(fuente)
  #   
  #   datos_3
  # })
  
  datos_semana_fuente_4 <- reactive({
    # browser()
    
    datos1 <- datos_semana_fuente_3() |> 
      # maximo palabras por semana
      group_by(semana, palabra) |> 
      mutate(n_semana = sum(n)) |>
      group_by(semana) |>
      mutate(rank = dense_rank(desc(n_semana)))
    
    ranking <- datos1 |> 
      filter(rank <= input$semana_fuentes_palabras_n) |> 
      group_by(palabra) |> 
      summarize(n = sum(n)) |> 
      arrange(desc(n)) |> 
      mutate(rank = 1:n()) |> 
      filter(rank <= input$semana_fuentes_palabras_n) # cantidad de palabras por semana
    
    # browser()
    
    datos1 |> 
      filter(palabra %in% ranking$palabra) |> 
      # print(n=100)
    # están malas las semanas
      group_by(fecha_texto, fuente, palabra) |> 
      # ungroup() |> 
      # distinct(semana, fecha, fecha_texto)
      summarize(n = sum(n),
                fecha = min(fecha), 
                fecha_texto = first(fecha_texto)) |> 
      # distinct(semana, fecha, rank, fuente, .keep_all = TRUE) |>
      # ordenar palabras
      group_by(fecha_texto, palabra) |> 
      mutate(n_palabra_semana = sum(n)) |> 
      group_by(fecha_texto) |> 
      mutate(palabra = tidytext::reorder_within(palabra, n_palabra_semana, fecha_texto)) |> 
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
    
    # .semanas = (week(today())-(input$semanas_fuentes_palabras-1)):week(today()) #29:32
    # # bug: no puede filtrar por semanas de distintos años, porque nada indica el año
    # # entonces sería como desde semana 50 a semana 1
    # # también la resta está mal hecha en consideración de semanas entre dos años
    # # tampoco viene con fecha, habría que filtrar por fecha
    semanas_atras = weeks(input$semanas_fuentes_palabras+1)
    fecha_min = today() - semanas_atras
    fecha_max = today()
    
    datos_semana_fuente_palabra_1() |> 
      # filtrar semanas
      # filter(semana %in% .semanas) |> 
      filter(fecha >= fecha_min,
             fecha <= fecha_max) |> 
      # # ranking de fuentes con mayor cantidad de palabras
      group_by(fuente) |>
      mutate(n_total_fuente = sum(n)) |>
      ungroup() 
  })
  
  datos_semana_fuente_palabra_3 <- reactive({
    .n_fuentes = input$semana_fuentes_palabras_fuentes-1
    
    datos_semana_fuente_palabra_2() |>
      # agrupar fuentes chicas
      mutate(fuente = fct_lump(fuente, w = n_total_fuente,
                               n = .n_fuentes, other_level = "Otros")) |>
      mutate(fuente = fct_reorder(fuente, n_total_fuente, .desc = T),
             fuente = fct_relevel(fuente, "Otros", after = 0)) |> 
      group_by(fuente, fecha, fecha_texto, palabra) |>
      summarize(n = sum(n)) |>
      # ordenar palabras
      group_by(fecha_texto, fuente) |> 
      mutate(n_palabra_fuente = sum(n)) |> 
      group_by(fecha_texto) |> 
      mutate(fuente2 = tidytext::reorder_within(fuente, n_palabra_fuente, fecha_texto)) |> 
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
  
  
  ## sentimiento ----
  sentimiento_semanas <- reactive({
    semanas_atras = weeks(input$sentimiento_semanas)
    fecha_min = today() - semanas_atras
    fecha_max = today()

    sentimiento |>
      filter(fecha >= fecha_min,
             fecha <= fecha_max) |>
      recodificar_fuentes()
  })
  
  
  # —----
  # gráficos ----
  # la idea es que los datos pasen ya procesados acá y esta sección sólo se enfoque en las visualizaciones
  
  ## líneas semanas ----
  output$g_semanas <- renderPlot({
    req(length(input$destacar_palabra) > 0)
    req(input$destacar_palabra != "")
    req(datos_conteo_semanas_4())
    # browser()
    message("gráfico líneas semanas")
    
    datos <- datos_conteo_semanas_4()
    
    # opciones gráfico
    .dodge = 3
    .espaciado_y = 0.08
    .espaciado_x = 0.06
    
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
        family = tipografia$sans,
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
            axis.text.y = element_text(family = tipografia$sans, size = .texto_ejes),
            axis.title.y = element_text(family = tipografia$serif, face = "italic"),
            axis.text.x = element_text(family = tipografia$sans, size = .texto_ejes, hjust = 1, angle = input$angulo),
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
                           labels = label_percent()) +
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
    
  }, res = resolucion)
  
  
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
    req(datos_conteo_semanas_palabras_2())
    # browser()
    
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
                  family = tipografia$sans, size = 3) +
        ggrepel::geom_text_repel(data = ~filter(.x, semana == max(semana)),
                                 aes(label = palabra, color = palabra),
                                 size = 3, family = tipografia$sans,
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
        family = tipografia$sans,
        hjust = 1,
        size = 3) +
        geom_text(data = ~filter(.x, rank == 1),
                  aes(label = ifelse(chico, as.character(palabra), ""),
                      y = n_semana + (prom*0.09),
                      color = palabra),
                  family = tipografia$sans,
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
            axis.text.y = element_text(family = tipografia$sans, size = .texto_ejes),
            axis.title.y = element_text(family = tipografia$serif, face = "italic"),
            axis.text.x = element_text(family = tipografia$sans, size = .texto_ejes, hjust = 1),
            legend.title = element_text(family = tipografia$serif, face = "italic"),
            legend.text =  element_text(family = tipografia$sans),
            plot.caption = element_text(color = color_detalle)) +
      theme(axis.text.x = element_text(family = tipografia$sans, angle = 40)) + #, hjust = 1, angle = 40))
      labs(color = "Palabras", y = "frecuencia de palabras", x = NULL
           # caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile"
      )
    
    
    return(plot)
  }, res = resolucion)
  
  
  ## barras semana fuente ----
  output$g_semana_fuente <- renderPlot({
    req(datos_semana_fuente_4())
    # browser()
    # dev.new()
    # datos_semana_fuente_4() |> 
    #   distinct(semana, fecha, fecha_texto)
    
    plot <- datos_semana_fuente_4() |>
      ggplot(aes(x = n, y = palabra, fill = fuente)) +
      geom_col(width = .7, color = color_fondo) +
      geom_point(aes(color = fuente), alpha = 0) +
      tidytext::scale_y_reordered() +
      scale_x_continuous(expand = expansion(c(0, .1))) +
      scale_fill_viridis_d(begin = .2, direction = 1, end = .7, option = "magma", aesthetics = c("color", "fill")) +
      facet_wrap(~fecha_texto, 
                 scales = "free", nrow = 1)  +
      guides(fill = guide_none(),
             color = guide_legend(position = "bottom", 
                                  nrow = ifelse(input$semana_fuentes_fuentes > 5, 2, 1), # nrow = 1, 
                                  reverse = TRUE,
                                  override.aes = list(alpha = 1, size = 4))) +
      labs(y = "palabras más mencionadas por semana", x = "frecuencia de mención, por fuente",
           fill = "fuentes\nescritas"
           # caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile"
      )
    
    plot <- plot +
      theme(legend.text = element_text(margin = margin(l = 2))) +
      theme(panel.grid.major.x = element_line(),
            panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(family = tipografia$sans, size = .texto_ejes),
            axis.title.y = element_text(family = tipografia$serif, face = "italic"),
            axis.title.x = element_text(family = tipografia$serif, face = "italic",
                                        margin = margin(t = 6, b = -10)),
            axis.text.x = element_text(family = tipografia$sans, size = .texto_ejes, hjust = 1, angle = 40),
            legend.title = element_blank(), #element_text(family = tipografia$serif, face = "italic"),
            legend.text =  element_text(family = tipografia$sans), 
            plot.caption = element_text(color = color_detalle)) +
      theme(strip.text = element_text(family = tipografia$sans))
    
    return(plot)
  }, res = resolucion)
  
  
  ## puntos fuente palabra ----
  
  output$texto_selector_palabras_fuente <- renderText({
    str_to_sentence(input$selector_palabras_fuente)
  })
  
  output$g_semana_fuente_palabra <- renderPlot({
    req(datos_semana_fuente_palabra_4())
    # browser()
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
      facet_wrap(~fecha_texto, 
                 scales = "free", nrow = 1)  +
      # theme_minimal() +
      labs(y = "fuentes ordenadas por menciones", x = "frecuencia de mención, por semanas"
           # caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile"
      )
    
    plot <- plot +
      theme(legend.text = element_text(margin = margin(l = 2))) +
      theme(panel.grid.major.x = element_line(),
            panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(family = tipografia$sans, size = .texto_ejes),
            axis.title.y = element_text(family = tipografia$serif, face = "italic"),
            axis.title.x = element_text(family = tipografia$serif, face = "italic",
                                        margin = margin(t=6)),
            axis.text.x = element_text(family = tipografia$sans, size = .texto_ejes, hjust = 1, vjust = .5, angle = 90),
            legend.title = element_text(family = tipografia$serif, face = "italic"),
            plot.caption = element_text(color = color_detalle)) +
      theme(strip.text = element_text(family = tipografia$sans))
    
    return(plot)
  }, res = resolucion)
  
  
  ## correlación ----
  ### correlación general ----
  output$g_cor_total <- renderPlot({
    
    dato <- cor_total_dato_2() |> 
      mutate(tamaño = rescale(correlacion, to = c(1.2, 2), 
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
                                  family = tipografia$sans,
                                  bg.colour = color_fondo, bg.r = 0.2, color = color_negro, size = 3.2) +
      geom_text(aes(label = round(correlacion, 3), y = -1.5), vjust = 1, size = 2.6, alpha = .5) +
      guides(color = guide_none(), fill = guide_none(), size = guide_none()) +
      theme(strip.background = element_blank(), strip.text = element_blank(),
            axis.title.x = element_text(family = tipografia$serif, face = "italic", margin = margin(t = 8)),
            axis.ticks = element_blank(), axis.text = element_blank(),
            panel.grid = element_blank(), panel.background = element_blank()) +
      coord_equal(clip = "off") +
      scale_y_continuous(limits = c(-1.5, 3)) +
      facet_grid(rows = NULL, cols = vars(orden), drop = T) +
      labs(x = paste("términos más correlacionados con:", str_to_sentence(input$cor_total_palabra)), y = NULL)
    
    
    return(plot)
  }, res = resolucion)
  
  
  ### correlación fuentes ----
  output$g_cor_fuente <- renderPlot({
    req(cor_fuente_dato_3())
    message("gráfico correlación fuentes")
    
    dato <- cor_fuente_dato_3() |> 
      mutate(tamaño = rescale(correlacion, to = c(1.2, 2), 
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
                                  family = tipografia$sans,
                                  bg.colour = color_fondo, bg.r = 0.2, color = color_negro, size = 3.2) +
      geom_text(aes(label = round(correlacion, 3), y = -1.5), vjust = 1, size = 2.6, alpha = .5) +
      guides(color = guide_none(), fill = guide_none(), size = guide_none()) +
      theme(strip.background = element_blank(), strip.text = element_text(face = "bold", vjust = 1),
            strip.text.x = element_blank(),
            axis.ticks = element_blank(), axis.text = element_blank(),
            axis.title.x = element_text(family = tipografia$serif, face = "italic", margin = margin(t = 8)),
            panel.grid = element_blank(), panel.background = element_blank()) +
      coord_equal(clip = "off") +
      scale_y_continuous(limits = c(-1.5, 3)) +
      facet_grid(rows = vars(fuente), cols = vars(orden), drop = T, switch = "y") +
      labs(x = paste("términos más correlacionados con:", str_to_sentence(input$cor_fuente_palabra)), y = NULL)
    
    return(plot)
  }, res = resolucion)
  
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
  
  
  
  # sentimiento ----
  output$g_sentimiento <- renderPlot({
    # browser()
    
    sentimiento <- sentimiento_semanas()
    
    if (is.null(input$sentimiento_fuente) & input$sentimiento_tema == "Todos") {
      # sin fuente ni tema
      
      sentimiento_2 <- sentimiento |>
        group_by(semana, fecha) |>
        sentimiento_calcular()
      
    } else if (!is.null(input$sentimiento_fuente) & input$sentimiento_tema == "Todos") {
      # sólo fuente
      # browser()
      sentimiento_2 <- sentimiento |>
        filter(fuente %in% input$sentimiento_fuente) |> 
        group_by(fuente, semana, fecha) |>
        sentimiento_calcular()
      
      faceta <- facet_wrap(~fuente)
      
    } else if (is.null(input$sentimiento_fuente) & input$sentimiento_tema != "Todos") {
      # sólo tema
      
      sentimiento_2 <- sentimiento |>
        filter(clasificacion == input$sentimiento_tema) |>
        group_by(clasificacion, semana, fecha) |>
        sentimiento_calcular()
      
      faceta <- facet_wrap(vars(clasificacion))
      
    } else if (!is.null(input$sentimiento_fuente) & input$sentimiento_tema != "Todos") {
      # fuente y tema
      
      sentimiento_2 <- sentimiento |>
        filter(clasificacion %in% input$sentimiento_tema) |>
        filter(fuente %in% input$sentimiento_fuente) |> 
        group_by(fuente, clasificacion, semana, fecha) |>
        sentimiento_calcular()
      
      faceta <- facet_wrap(vars(fuente))
    }
    
    # browser()
    
    sentimiento_3 <- sentimiento_2 |>
      mutate(tipo = ifelse(sentimiento > 0, "Mayormente\npositivo", "Mayormente\nnegativo")) |> 
      mutate(tipo_texto = ifelse(sentimiento > 0, "% de noticias\npositivas", "% de noticias\nnegativas"))
    
    
    # fechas
    # browser()
    if (input$sentimiento_semanas <= 8) {
      sentimiento_4 <- sentimiento_3 |> 
        group_by(semana) |> 
        mutate(fecha_etiqueta = redactar_fecha(min(fecha))) |> 
        ungroup() |> 
        mutate(fecha_etiqueta = fct_reorder(fecha_etiqueta, fecha))
    } else {
      sentimiento_4 <- sentimiento_3 |> 
        mutate(fecha_etiqueta = fecha)
    }
    
    # ancho texto
    if (input$sentimiento_semanas > 10 | !is.null(input$sentimiento_fuente)) {
      ancho_texto = 2.2
    } else {
      ancho_texto = 3
    }
    
    # browser()
    # gráfico
    plot <- sentimiento_4 |> 
      ggplot() +
      aes(fecha_etiqueta, sentimiento, fill = tipo) +
      geom_col(color = color_fondo, linewidth = 0.6) +
      geom_text(aes(color = tipo_texto,
                    label = ifelse(sentimiento <= 0, percent(p_negativas, 1), "")), 
                vjust = 1, size = ancho_texto, nudge_y = -0.04) +
      geom_text(aes(color = tipo_texto,
                    label = ifelse(sentimiento > 0, percent(p_positivas, 1), "")), 
                vjust = 0, size = ancho_texto, nudge_y = 0.04,
                show.legend = F) +
      geom_hline(yintercept = 0, linewidth = 0.7, color = color_texto) +
      scale_y_continuous(limits = c(-1.1, 1.1),
                         # expand = expansion(c(0.1, 0.1)),
                         breaks = c(1, 0, -1),
                         # labels = label_percent(accuracy = 1)
                         labels = c("Positivo", "Neutro", "Negativo")
      ) +
      scale_fill_manual(values = c("Mayormente\npositivo" = "#B0987E", 
                                   "Mayormente\nnegativo" = color_destacado)) +
      scale_color_manual(values = c("% de noticias\npositivas" = "#B0987E", 
                                    "% de noticias\nnegativas" = color_destacado)) +
      coord_cartesian(clip = "off") +
      guides(fill = guide_legend(reverse = TRUE, title = NULL, direction = "vertical",
                                 override.aes = list(label = "")),
             color = guide_legend(reverse = TRUE, title = NULL, direction = "vertical",
                                  override.aes = list(label = "%", vjust = .5, size = 3.3)),
             ) +
      labs(y = "sentimiento promedio de noticias semanales",
           x = NULL) +
      theme(legend.key.spacing.y = unit(2, "mm"))
    
    # si el gráfico tiene faceta
    if (!is.null(input$sentimiento_fuente) | input$sentimiento_tema != "Todos") {
      plot <- plot + faceta +
        guides(fill = guide_legend(position = "top", reverse = TRUE, direction = "horizontal", title = NULL,
                                   override.aes = list(label = "")),
               color = guide_legend(position = "top", reverse = TRUE, direction = "horizontal", title = NULL,
                                    override.aes = list(label = "%", vjust = .5, size = 3.3)))
    }
    
    # si el gráfico tiene faceta y muchas semanas
    # if (!is.null(input$sentimiento_fuente) | input$sentimiento_tema != "Todos") {
    #   if (input$sentimiento_semana > 6) {
    #     plot <- plot +
    #       
    #   }
    # }
    
    # si se cambia el eje x porque tiene muchas semanas
    if (input$sentimiento_semanas > 8) {
      plot <- plot +
        scale_x_date(date_labels = "%d/%m/%y")
    } else {
      plot <- plot +
      theme(axis.text.x = element_text(family = tipografia$sans, size = .texto_ejes, hjust = 1, angle = 40))
    }
    
    # tema
    plot <- plot +
      theme(axis.ticks.x = element_blank(),
            axis.text.y = element_text(family = tipografia$sans, size = .texto_ejes, angle = 90, hjust = .5),
            axis.title.y = element_text(family = tipografia$serif, face = "italic")
      )
    
    return(plot)
  }, res = resolucion)
}

# Run the application 
shinyApp(ui = ui, server = server)
