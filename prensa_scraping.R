# ejecuta scripts de webscraping en paralelo para obtener datos recientes de todas las fuentes
# output: resultados/{fuente}/...

library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones.R")


# realizar scraping ----

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

scraping_prensa("modulos/cron_emol_h.r") #hist pero por meses

scraping_prensa("modulos/cron_diariofinanciero.r") #histórico solo hasta página 20

scraping_prensa("modulos/cron_elmostrador.r") #(requiere selenium)

scraping_prensa("modulos/cron_lacuarta.r") #histórico solo hasta página 9

scraping_prensa("modulos/cron_cooperativa.r") #hist en otro script

scraping_prensa("modulos/cron_agricultura.r") #hist

scraping_prensa("modulos/cron_chvnoticias.r")

scraping_prensa("modulos/cron_t13.r")

scraping_prensa("modulos/cron_biobio.r")

scraping_prensa("modulos/cron_lahora.r")

scraping_prensa("modulos/cron_adnradio.r") 

# scraping_prensa("modulos/cron_cooperativa_h.r")







#revisión ----
#chequear si se guardaron los archivos
# revisar_resultados("resultados")
#revisa si hay archivos nuevos el día actual
