# este script ejecuta paralelamente el scraping usando furrr, preferible si se quiere automatizar y agendar el proceso usando cron o launchd

setwd("~/R/prensa/")
source("funciones.R")

Sys.setenv(LANG = "en_US.UTF-8")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(furrr)
library(glue)
plan(multisession, workers = 8)


# cat("prueba")
# R.version.string
# sessionInfo()$locale

# realizar scraping 
notificacion("Scraping de prensa", "Iniciando scripts…")

modulos <- c(
  "modulos/cron_diariofinanciero.r",
  "modulos/cron_eldesconcierto.r",
  "modulos/cron_emol.r",
  "modulos/cron_lacuarta.r",
  "modulos/cron_latercera.r",
  "modulos/cron_meganoticias.r",
  "modulos/cron_eldinamo.r",
  "modulos/cron_lanacion.r",
  "modulos/cron_publimetro.r",
  "modulos/cron_theclinic.r",
  "modulos/cron_elciudadano.r",
  "modulos/cron_radiouchile.r",
  "modulos/cron_24horas.r",
  "modulos/cron_cnnchile.r",
  "modulos/cron_exante.r",
  "modulos/cron_elsiglo.r",
  "modulos/cron_ciper.r",
  "modulos/cron_agricultura.r",
  "modulos/cron_redgol.r",
  "modulos/cron_quintopoder.r",
  "modulos/cron_cooperativa.r",
  "modulos/cron_elmostrador.r",
  "modulos/cron_chvnoticias.r",
  "modulos/cron_t13.r",
  "modulos/cron_biobio.r",
  "modulos/cron_lahora.r",
  "modulos/cron_adnradio.r",
  "modulos/cron_lasegunda.r"
  )

modulos <- paste0("/Users/baolea/R/prensa/", modulos)

# ejecutar paralelamente
future_walk(modulos, ~{
  source(.x, local = TRUE, echo = FALSE)
  
  notificacion("Scraping de prensa", 
               glue("{.x} terminado.
                    Listos {modulos_n() - nrow(sin_cambios_hoy())} de {modulos_n()}."))
  
  print(paste(nrow(sin_cambios_hoy()), "de", modulos_n()))
  })

notificacion("Scraping de prensa", "Datos de noticias descargados")

plan(multisession)

source("revisar/revisar_scraping.R")

