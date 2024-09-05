library(shiny)
library(bslib)
library(thematic)
library(showtext)
library(htmltools)
library(arrow)
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(shadowtext)
library(shinycssloaders)
library(ragg)
options(shiny.useragg = TRUE)
showtext::showtext_opts(dpi = 180)

color_fondo = "#EED0A8"
color_texto = "#866C53"
color_detalle = "#A5876A"
color_destacado = "#C7392B"

thematic_shiny(
  # font = "auto", 
  font = font_spec(families = c("Lato", "Libre Baskerville")),
  # session = session,
  bg = color_fondo, fg = color_texto, accent = color_destacado)


# datos ----
# palabras_semana <- read_parquet("apps/prensa_semanal/palabras_semana.parquet")
palabras_semana <- read_parquet("palabras_semana.parquet")

palabras_posibles <- palabras_semana |> 
  group_by(palabra) |> 
  summarize(n = sum(n)) |> 
  arrange(desc(n)) |> 
  slice(1:100) |> 
  pull(palabra)


ui <- page_fluid(
  title = "Prensa en Chile", 
  lang = "es",
  
  # tema ----
  theme = bslib::bs_theme(
    font_scale = 1.2,
    bg = color_fondo, fg = color_texto, primary = color_destacado, 
    # tipografías
    heading_font = font_google("Libre Baskerville", wght = c(400, 700),
                               ital = c(0, 1)),
    base_font = font_google("Lato",  wght = c(300, 400, 700, 900),
                            ital = c(0, 1))
    # base_font = font_link(
    #   "IBM Plex Mono",
    #   href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,400;0,500;0,700;1,400&display=swap"
    # )
  ),
  
  # css ----
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
                                     text_decoration = "none")),
               )
           ),
           
           
           markdown("Proyecto de análisis de texto de noticias chilenas. Las noticias se obtienen regularmente mediante [web scraping](https://github.com/bastianolea/prensa_chile)."),
           markdown("Actualmente, el corpus obtenido supera las **600 mil noticias** individuales, las cuales suman un total de **105 millones de palabras**, abarcando más de 21 fuentes periodísticas distintas."),
           
           hr()
    )
  ),
  
  
  
  ## gráfico semanas ----
  fluidRow(
    column(9,
           plotOutput("g_semanas", 
                      width = "100%", height = "600px") |> withSpinner()
    ),
    column(3,
           ### configuración ----
           h2("config"),
           
           sliderInput("semanas",
                       "semanas",
                       min = 2, max = 4*5,
                       value = 9),
           
           # sliderInput("frecuencia_total",
           #             "frecuencia_total",
           #             min = -4,
           #             max = 4,
           #             value = 2,
           #             step = 0.5),
           
           sliderInput("frecuencia_min",
                       "frecuencia_min",
                       min = 0.001, max = 0.010,
                       value = 0.003,
                       step = 0.001),
           
           sliderInput("palabras_semana_max",
                       "palabras_semana_max",
                       min = 4, max = 20,
                       value = 10,
                       step = 1),
           
           selectizeInput("palabras_excluir",
                          "palabras_excluir",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(create = TRUE,
                                         placeholder = "")),
           
           sliderInput("angulo",
                       "angulo",
                       min = 0, max = 60,
                       value = 40,
                       step = 10),
           
           selectizeInput("destacar_palabra",
                          "destacar_palabra",
                          choices = c("Ninguna", palabras_posibles),
                          multiple = FALSE,
                          options = list(search = TRUE,
                                         create = TRUE,
                                         placeholder = "Ninguna")),
           
           
           
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
  # datos ----
  ## datos conteo semanas ----
  datos_conteo_semanas <- reactive({
    palabras_semana |> 
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
      filter(p_palabra_semana > input$frecuencia_min) |> 
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
  })
  
  # gráficos ----
  
  ## gráfico semanas ----
  output$g_semanas <- renderPlot({
    # opciones gráfico
    .dodge = 3
    .angulo = input$angulo
    .espaciado_y = 0.08
    .espaciado_x = 0.05
    
    datos <- datos_conteo_semanas() |> 
      filter(!palabra %in% input$palabras_excluir) |> 
      # ordenar palabras por frecuencia
      ungroup() |> 
      mutate(palabra = fct_reorder(palabra, freq_total_palabra)) |> 
      # etiquetas hacia la izquierda
      mutate(inv = ifelse(semana == min(semana) | n < mean(n)*0.8, TRUE, FALSE))
    
    # por porcentaje o por frecuencia
    # group_by(semana) |>
    # mutate(n = n/sum(n)) |>
    
    if (input$destacar_palabra != "Ninguna") {
      # crea una variable dicotómica con la palabra destacada
      datos <- datos |> 
        mutate(destacar = ifelse(palabra == tolower(input$destacar_palabra), 
                                 tolower(input$destacar_palabra), "otras"))
    }
    
    # hace que los colores del gráfico sean por palabras, o por palabra destacada vs otras
    variable <- ifelse(input$destacar_palabra != "Ninguna",
                       "destacar", "palabra")
    
    #gráfico 
    plot <- datos |> 
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
        bg.colour = color_fondo, bg.r = 0.3, angle = .angulo, size = 2.8, vjust = 0.3, 
        position = position_dodge(.dodge), check_overlap = T, show.legend = F) +
      # escalas
      scale_y_continuous(expand = expansion(c(.espaciado_y*0.7, .espaciado_y))) +
      scale_x_date(date_breaks = "weeks", date_labels = "%d de %B", expand = expansion(c(.espaciado_x, .espaciado_x))) +
      guides(color = guide_none()) +
      # theme_classic() +
      coord_cartesian(clip = "off") +
      theme(panel.grid.major.x = element_line(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(family = "Lato"),
            axis.title.y = element_text(family = "Libre Baskerville", face = "italic"),
            axis.text.x = element_text(family = "Lato", hjust = 1, angle = .angulo),
            plot.caption = element_text(color = color_detalle)) +
      labs(y = "frecuencia de palabras por semana",
           x = NULL,
           # title = "Conceptos principales en prensa, por semana", 
           caption = "Elaboración: Bastián Olea Herrera. https://github.com/bastianolea/prensa_chile"
      )
    library(RColorBrewer)
    # paleta de colores si se destaca una palabra
    if (input$destacar_palabra != "Ninguna") {
      plot <- plot +
        scale_color_manual(values = c(color_destacado, color_texto))
    } else {
      # browser()
      plot <- plot +
        # scale_color_brewer(palette = "Set1")
        # scale_color_manual(values = colorRampPalette(brewer.pal(Inf, "Dark2"))(length(unique(datos$palabra))))
        scale_color_viridis_d(end = .75, option="magma")
    }
    
    
    return(plot)
    
  }, res = 100)
}

# Run the application 
shinyApp(ui = ui, server = server)
