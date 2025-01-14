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

scraping_prensa("scraping_latercera.r") #hist

scraping_prensa("scraping_meganoticias.r") #hist

scraping_prensa("scraping_eldinamo.r") #hist

scraping_prensa("scraping_lanacion.r") #hist

scraping_prensa("scraping_publimetro.r") #hist

scraping_prensa("scraping_theclinic.r") #hist

scraping_prensa("scraping_elciudadano.r") #hist

scraping_prensa("scraping_radiouchile.r") #hist

scraping_prensa("scraping_24horas.r") #hist

scraping_prensa("scraping_cnnchile.r") #hist

scraping_prensa("scraping_exante.r") #hist

scraping_prensa("scraping_izquierdadiario.R") #hist

scraping_prensa("scraping_elsiglo.r") #hist

scraping_prensa("scraping_ciper.r") #hist

scraping_prensa("scraping_agricultura.r") #hist

scraping_prensa("scraping_redgol.r") #hist

scraping_prensa("scraping_eldesconcierto.r") #hist

scraping_prensa("scraping_quintopoder.r") #hist

scraping_prensa("scraping_emol.r") #hist en otro script

scraping_prensa("scraping_diariofinanciero.r") #histórico solo hasta página 20

scraping_prensa("scraping_lacuarta.r") #histórico por búsquedas

scraping_prensa("scraping_cooperativa.r") #hist en otro script

scraping_prensa("scraping_elmostrador.r") # (requiere selenium para hist)

scraping_prensa("scraping_chvnoticias.r")

scraping_prensa("scraping_t13.r") #hist por búsquedas

scraping_prensa("scraping_biobio.r")

scraping_prensa("scraping_lahora.r")

scraping_prensa("scraping_adnradio.r")

scraping_prensa("scraping_lasegunda.r") #sólo obtiene titulares, fecha y palabras clave

# scraping_prensa("scraping_cooperativa_h.r") #este es sólo histórico


# revisión ----
# Sys.sleep(30)

# chequear si se guardaron los archivos
source("otros/revisar/revisar_scraping.R")

notificacion("Scraping de prensa", "Datos de noticias descargados")