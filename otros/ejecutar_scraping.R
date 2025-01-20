# este script ejecuta paralelamente el scraping usando furrr, preferible si se quiere automatizar y agendar el proceso usando cron o launchd

setwd("~/R/prensa/")
source("funciones.R")

Sys.setenv(LANG = "en_US.UTF-8")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(furrr)
library(glue)
library(dplyr)
library(stringr)
plan(multisession, workers = 8)


# cat("prueba")
# R.version.string
# sessionInfo()$locale

# realizar scraping 
notificacion("Scraping de prensa", "Iniciando scripts…")

modulos <- c(
  "scraping/fuentes/scraping_diariofinanciero.r",
  "scraping/fuentes/scraping_eldesconcierto.r",
  "scraping/fuentes/scraping_emol.r",
  "scraping/fuentes/scraping_lacuarta.r",
  "scraping/fuentes/scraping_latercera.r",
  "scraping/fuentes/scraping_meganoticias.r",
  "scraping/fuentes/scraping_eldinamo.r",
  "scraping/fuentes/scraping_lanacion.r",
  "scraping/fuentes/scraping_publimetro.r",
  "scraping/fuentes/scraping_theclinic.r",
  "scraping/fuentes/scraping_elciudadano.r",
  "scraping/fuentes/scraping_radiouchile.r",
  "scraping/fuentes/scraping_24horas.r",
  "scraping/fuentes/scraping_cnnchile.r",
  "scraping/fuentes/scraping_exante.r",
  "scraping/fuentes/scraping_elsiglo.r",
  "scraping/fuentes/scraping_ciper.r",
  "scraping/fuentes/scraping_agricultura.r",
  "scraping/fuentes/scraping_redgol.r",
  "scraping/fuentes/scraping_quintopoder.r",
  "scraping/fuentes/scraping_cooperativa.r",
  "scraping/fuentes/scraping_elmostrador.r",
  "scraping/fuentes/scraping_chvnoticias.r",
  "scraping/fuentes/scraping_t13.r",
  "scraping/fuentes/scraping_biobio.r",
  "scraping/fuentes/scraping_lahora.r",
  "scraping/fuentes/scraping_adnradio.r",
  "scraping/fuentes/scraping_lasegunda.r"
  )

modulos <- paste0("/Users/baolea/R/prensa/", modulos)

# ejecutar paralelamente
future_walk(modulos, ~{
  # .x <- sample(modulos, 1)
  source(.x, local = TRUE, echo = FALSE)
  
  modulo <- str_extract(.x, '\\w+\\.(r|R)')
  
  notificacion("Scraping de prensa", 
               glue("{modulo} terminado.
                    Listos {modulos_n() - nrow(sin_cambios_hoy())} de {modulos_n()}."))
  
  print(paste(nrow(sin_cambios_hoy()), "de", modulos_n()))
  })

notificacion("Scraping de prensa", "Datos de noticias descargados")

plan(multisession)

source("otros/revisar/revisar_scraping.R")

