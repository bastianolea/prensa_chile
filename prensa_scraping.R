# ejecuta scripts de web scraping en paralelo (usando RStudio jobs) para 
# obtener datos recientes de todas las fuentes, que se guardan en la carpeta /resultados

# hay un script alternativo que ejecuta los scrapings usando furrr multiproceso en /otros/ejecutar_scraping.R

# output: resultados/{fuente}/...

library(dplyr) |> suppressPackageStartupMessages()
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate) |> suppressPackageStartupMessages()

source("funciones.R")


# realizar scraping ----
notificacion("Scraping de prensa", "Iniciando scripts…")

scraping_prensa("modulos/cron_latercera.r") #hist

scraping_prensa("modulos/cron_meganoticias.r") #hist

scraping_prensa("modulos/cron_eldinamo.r") #hist

scraping_prensa("modulos/cron_lanacion.r") #hist

scraping_prensa("modulos/cron_publimetro.r") #hist

scraping_prensa("modulos/cron_theclinic.r") #hist

scraping_prensa("modulos/cron_elciudadano.r") #hist

scraping_prensa("modulos/cron_radiouchile.r") #hist

scraping_prensa("modulos/cron_24horas.r") #hist

scraping_prensa("modulos/cron_cnnchile.r") #hist

scraping_prensa("modulos/cron_exante.r") #hist

scraping_prensa("modulos/cron_elsiglo.r") #hist

scraping_prensa("modulos/cron_ciper.r") #hist

scraping_prensa("modulos/cron_agricultura.r") #hist

scraping_prensa("modulos/cron_redgol.r") #hist

scraping_prensa("modulos/cron_eldesconcierto.r") #hist

scraping_prensa("modulos/cron_quintopoder.r") #hist

scraping_prensa("modulos/cron_emol.r") #hist en otro script

scraping_prensa("modulos/cron_diariofinanciero.r") #histórico solo hasta página 20

scraping_prensa("modulos/cron_lacuarta.r") #histórico solo hasta página 9

scraping_prensa("modulos/cron_cooperativa.r") #hist en otro script

scraping_prensa("modulos/cron_elmostrador.r") # (requiere selenium para hist)

scraping_prensa("modulos/cron_chvnoticias.r")

scraping_prensa("modulos/cron_t13.r")

scraping_prensa("modulos/cron_biobio.r")

scraping_prensa("modulos/cron_lahora.r")

scraping_prensa("modulos/cron_adnradio.r")

scraping_prensa("modulos/cron_lasegunda.r") #sólo obtiene titulares, fecha y palabras clave

# scraping_prensa("modulos/cron_cooperativa_h.r") #este es sólo histórico


# revisión ----
# Sys.sleep(30)

# chequear si se guardaron los archivos
source("revisar/revisar_scraping.R")

notificacion("Scraping de prensa", "Datos de noticias descargados")