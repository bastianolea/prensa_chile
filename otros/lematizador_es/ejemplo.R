library(data.table)
library(fastmatch)
source("otros/lematizador_es/lematizador.R")

head(spmorphemes) # You can modify this list. See http://www.datsi.fi.upm.es/~coes/
head(spdictionary) # You can modify this data.frame with your own words!
head(spcommonwords) # You can modify this data.frame with your own words!

system.time(lematizador("grandullón")) # First time it takes a lot of time (see ?fmatch)
system.time(lematizador("grandullón"))

lematizador( "des" ) # dar
lematizador( "anduve" ) # andar
lematizador( "casitas" ) ## NA : Try  http://www.datanalytics.com/blog/2011/12/13/un-lematizador-para-el-espanol-con-r-¿cutre-¿mejorable/
## Or modify the spdictionary!
lematizador( "comimos" ) # comer
lematizador( "queremos") # querer
lematizador( "patatas" ) # patata

bench::mark(
  map(palabras, lematizador)
)
