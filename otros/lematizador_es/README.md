#+TITLE: Un lematizador en español para R 
#+AUTHOR: Emilio Torres Manzanera
#+DATE: Time-stamp: <2020-08-26 13:43 emilio on emilio-XPS-15-9570>
#+TAGS: 
#+PROPERTY: header-args :results output :exports both :session :eval no

Public domain. 2020. Emilio Torres Manzanera
https://torres.epv.uniovi.es/centon/lematizador-espanol-r.html


Se presenta un diccionario de español y unos lematizadores que se desarrollaron en el año 2011. Por aquella época apenas había nada para lematizar en español. Imagino que después de casi diez años habrá herramientas potentes que realicen esta tarea. 


** Un poco de historia

Este trabajo se desarrolló en el año 2011. Por aquella época apenas había nada para lematizar en español. Imagino que después de casi diez años habrá herramientas potentes que realicen esta tarea. 


Como labor propia de un enano minero, ha diez años que creé un listado de palabras a partir de documentos bajados del [[http://www.gutenberg.org/][proyecto Gutenberg]], del diccionario de [[http://www.datsi.fi.upm.es/~coes/][espa~nol.dicc Release 1.11]] (COES), que cuenta con 63 000 registros, y de contrastarlas con la versión del año 2001 del diccionario de la [[https://dle.rae.es/][Real Acadamia de la Lengua Española]] (DRAE), que a fecha de 2020 cuenta con 93 000 lemas.


Descargue [[file:graphics/200918.tar.gz][este fichero]].

#+begin_src r
library(data.table)
library(fastmatch)
source("lematizador.R")

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

#+end_src



** Licencia sobre el lenguaje español.

Como todos los años recibo alguna petición o sugerencia sobre este lematizador para R, lo pongo al día.  A raíz de
una intervención pública en una [[https://twitter.com/JaimeObregon/status/1284783285512605697][red social de internet]] se comentó la necesidad de disponer de un diccionario con las palabras en español en formato libre.

En su momento creé un listado de 55 000 palabras con su correspondiente forma canónica. Identifico como palabra canónica si dicho lema aparece tal cual en el DRAE. Aparecen recogidas en el fichero =spdictionary.csv=. Este fichero incluye palabras del proyecto Gutenberg, de licencia libre, del diccionario COES, que publicada bajo licencia GNU, pero que a su vez contrastó si las palabras aparecían en el DRAE, e incluye información sobre la clasificación de la palabra. Dado que solo he usado palabras disponibles en documentos de libre acceso, no publico la información adicional de la clasificación, y además, creé y comprobé el listado de palabras canónicas a partir de todas las fuentes de información, creo que no es necesario mantener la licencia GNU, sino que dispongo de la suficiente autoría como para decidir la licencia con que la publico a fecha de hoy: [[https://wiki.creativecommons.org/wiki/public_domain][Public Domain]]. 

Respecto a las palabras comunes, el fichero =spcommonwords.csv=, es de Public Domain, mientras que el =spmorphemes.rds=, al ser una transformación del COES, se publica bajo licencia GNU.

** Una alternativa es
 Un lematizador para el español con R... ¿cutre? ¿mejorable? [[http://www.datanalytics.com/blog/2011/12/13/un-lematizador-para-el-espanol-con-r-¿cutre-¿mejorable/][Carlos J. Gil Bellosta]]














