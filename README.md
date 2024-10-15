
# Análisis de prensa chilena

Proyecto de ciencia de datos desarrollado en R para analizar texto de noticias chilenas. Comprende módulos para realizar web scraping de sitios web de prensa para obtener el texto de sus noticias, procesos para transformar ese texto en palabras (tokens), y procesos para analizar dicho corpus de palabras usando distintas técnicas estadísticas.

Actualmente, el corpus de noticias obtenido supera las 590.000 noticias individuales, las cuales suman un total de 105 millones (!) de palabras, abarcando más de 33 fuentes periodísticas distintas.

![Gráfico resumen de resultados del scraping de prensa](graficos/resultados/datos_prensa_scraping_2024-09-29.png)

----

## Aplicaciones web

### Análisis de prensa chilena por semanas

[Aplicación web](https://bastianoleah.shinyapps.io/prensa_chile/) desarrollada en R que muestra varios gráficos que cuantifican el contenido de las noticias de prensa escrita de Chile. Los gráficos permiten identificar qué palabras son las más usadas a través del tiempo, lo cual a su vez permite ver cómo va variando el acontecer nacional. Esta aplicación es una muestra de la utilidad del análisis de texto para resumir gran cantidad de información por medio de visualizaciones interactivas.

[Accede a la aplicación en este enlace](https://bastianoleah.shinyapps.io/prensa_chile/)

<a href = "https://bastianoleah.shinyapps.io/prensa_chile/">
<img src="apps/prensa_chile/pantallazos/analisis_prensa_chile_semanal.jpg" height="900px" />
</a>

![](apps/prensa_chile/pantallazos/p1.png)
![](apps/prensa_chile/pantallazos/p2b.png)
![](apps/prensa_chile/pantallazos/p3.png)
![](apps/prensa_chile/pantallazos/p2c.png)
![](apps/prensa_chile/pantallazos/p4.png)
![](apps/prensa_chile/pantallazos/p5b.png)

----

## Principales hallazgos

![Resultados preliminares: conceptos más frecuentes en noticias por semana](graficos/resultados/noticias_semana_2024-09-30.jpg)

![Resultados preliminares: noticias sobre delincuencia versus estadísticas de delitos reportados](graficos/resultados/prensa_delincuencia_vs_reporte3b.jpg)

_próximamente..._

----

## Estructura del código

El script `prensa_procesar.R` es un orquestador desde el que se pueden realizar todos los pasos de obtención, procesamiento y análisis de datos. Muchos de estos procesos son altamente demandantes en términos de hardware, dado que estamos trabajando con bases de datos de cientos de miles de observaciones con columnas que contienen miles de palabras por cada fila, o bien, tablas con cientos de millones de filas que contienen los términos de todo el corpus. Por lo anterior, todos estos procesos han sido optimizados para usar las funciones más eficientes (luego de realizar varios benchmarks), reducir el costo de memoria de las operaciones (dado que la magnitud de los datos es imposible de mantener en memoria en ordenadores comunes), y aprovechar al máximo el procesamiento paralelo en múltiples núcleos de CPU.

### Scraping
- `prensa_scraping.R` ejecuta múltiples procesos simultáneos (usando _background jobs_ de RStudio), que corresponden a scripts que realizan scraping de un sitio web en específico, y guarda sus resultados en un archivo individual en `resultados/{modulo}/`. Se puede ejecutar diariamente para mantener actualizadas las fuentes de datos con las noticias recientes. El web scraping se realiza usando los paquetes `{rvest}` y `{polite}`, los cuales en conjunción realizan un scraping "ético", en el sentido de que realizan solicitudes de datos eficientes y respetando los tiempos de carga de los servidores, con el objetivo de no sobrecargarlos y respetar sus políticas sobre obtención de datos.

En la carpeta `modulos` se encuentran decenas de scripts individuales, uno por cada medio de comunicación o fuente de prensa, cada uno de los cuales realiza scraping de las noticias de dicho medio y las guarda. **Actualización:** los scripts de scraping fueron removidos de este repositorio por razones laborales y de seguridad. En su lugar, dejé un script de ejemplo, que puede servir para entender cómo se integran scripts de scraping a este sistema. Este script permite hacer web scraping de un sitio de noticias nacional, y puedes basarte en él para obtener datos de otros sitios. Si estás realizando un proyecto de código abierto o no remunerado y requieres acceder a todos los módulos de web scraping, por favor contactarme directamente.

Es posible realizar el scraping de uno de los medios ejecutando estos scripts individualmente, o bien, usarlos todos de forma simultánea y paralela (todos al mismo tiempo) ejecutando las funciones del script `prensa_scraping.R`. Por ejemplo, para scrapear las noticias del diario CNN Chile, se cargan las librerías del script `prensa_scraping.R` y se cargan las funciones del proyecto con `source("funciones.R")`, para así poder ejecutar la función `scraping_prensa("modulos/cron_cnnchile.r")`. `scraping_prensa` va a ejecutar el scraping en segundo plano; es decir, un proceso que se ejecuta de forma independiente y en el fondo (sin bloquear la consola de R). Dependiendo de la línea que comentes/descomentes en la función, los scripts se ejecutarán como _background job_ de RStudio (muestra el progreso de cada scraping en RStudio y funciona en cualquier sistema operativo) o como subprocesos de R (compatible con macOS y Linux, no muestra el progreso pero funciona sin necesidad de tener RStudio ejecutándose).

Los resultados del scraping se guardarán en la carpeta con el nombre del medio, dentro de `resultados/`. 

Los scripts que en `prensa_scraping.R` dicen "hist" son "históricos", es decir, dentro de cada script están comentadas líneas que permiten obtener noticias hacia atrás, y no sólo las más recientes. La capacidad de obtener noticias pasadas depende de cada fuente, ya que algunas limitan el acceso a noticias antiguas, y otras simplemente no las tienen disponibles. La mayoría de los scripts tienen comentadas líneas que permiten usar el mismo script para obtener noticias hacia atrás.


### Procesamiento
Luego del scraping se realiza el procesamiento de los datos. Todos los scripts de procesamiento se pueden ejecutar secuencialmente desde `prensa_procesar.R`. 

- `procesamiento/prensa_p1_cargar_datos.R` carga los datos scrapeados obtenidos por el paso anterior, los une, limpia y guarda en una sola base de datos, una noticia por fila. La carga y unión de los resultados individuales se realiza de forma paralela usando `{furrr}`. La limpieza de texto es un procedimiento computacionalmente intenso (dado que implica detectar patrones en el texto para eliminar símbolos e interpretar fechas), por lo que los datos cargados son divididos en piezas más pequeñas, y cada pieza se procesa en un proceso paralelo para aprovechar múltiples procesadores, y el resultado de cada limpieza se guarda individualmente en la carpeta `datos/preprocesados/` para evitar mantener los datos en memoria. Finalmente, estas piezas procesadas son vueltas a cargar y unidas como un solo archivo, `datos/prensa_datos.parquet`, que es la base principal que contiene todas las noticias ordenadas, limpiadas, e individualizadas con un ID único.
- `procesamiento/prensa_p2_procesar_texto.R` es el proceso en el que se tokenizan los textos de las noticias; es decir, se transforma una cadena de texto en todas las palabras individuales que conforman el cuerpo de la noticia, eliminando las _stopwords_ o palabras que son irrelevantes para el significado del texto. Para procesar la tokenización de forma eficiente, la base de datos se divide en piezas pequeñas de igual tamaño para ser procesadas en paralelo y guardadas individualmente, igual que en el paso anterior. Esto es para reducir la cantidad de información que se debe mantener cargada en la memoria, al relevar los resultados al disco duro. De este proceso se obtiene `datos/prensa_palabras.parquet`, una base de datos con más de 90 millones de filas, donde cada fila es una palabra asociada al ID único de la noticia de la cual proviene.
- `procesamiento/prensa_p3_calcular_conteo.R` carga la base de datos por palabras, y cuenta la frecuencia de palabras por noticia de forma paralela, para obtener `datos/prensa_palabras_conteo.parquet`, una base similar a la anterior, pero cuyas palabras son únicas por cada noticia, y contiene la frecuencia de cada término por noticia. El conteo de palabras se realiza primero agrupando las palabras según su raíz (por ejemplo, "notici" es la raíz de "noticia"), de modo que palabras que comparten una misma raíz son contadas como una misma palabra (noticia, noticias, noticiario, noticioso), y luego se aplica un algoritmo que reconstruye la palabra completa a partir de la raíz, privilegiando la elección de verbos singulares, en infinitivo, y frecuentes.
- `procesamiento/prensa_p4_calcular_correlacion.R` calcula la correlación entre términos, indicando qué términos aparecen cercanos a otros a partir del conteo de palabras por noticia. Calcula tanto la correlación de las palabras en general, como la correlación de las palabras por cada fuente.
- Los scripts `apps/prensa_semanal/prensa_semanal.R` y `apps/prensa_semanal/prensa_semanal_fuente.R` realizan cálculos sobre los datos que producen dataframes usados por la aplicación de visualización de datos, de modo que la aplicación en sí misma realice la menor cantidad de cálculos en tiempo real.


### Análisis
- `analisis/prensa_calcular_correlacion.R` para calcular correlación entre palabras dentro de noticias, retornando una base con palabras y sus pares correlacionados.
- `analisis/prensa_calcular_topicos.R` donde se procesan las noticias para identificar tópicos de forma automática y no supervisada mediante machine learning.
- `analisis/prensa_detectar_temas.R` donde se evalúa la presencia de términos específicos en cada noticia, y en base al porcentaje de términos coincidentes con respecto a las palabras totales, etiquetar cada noticia con la temática que engloba a los términos; por ejemplo, noticias donde más de 3% de las palabras tienen que ver con robos, asaltos, etc., serán categorizadas como noticias sobre delincuencia.
- Y más...

----

## Licencia
Este código se rige por la licencia **GNU General Public License**, una licencia de código abierto, por lo que puedes usar el código libremente y modificarlo, pero debes compartir tus modificaciones bajo la misma licencia. Esto permite a los usuarios usar y modificar el software, garantizando que el código resultante seguirá siendo libre (copyleft) al imponer que subsecuentes usos o modificaciones del código deban tamnbién ser compartidas de forma abierta. Por favor, citar este repositorio, y su autor, Bastián Olea Herrera.


----
 
Puedes ver mis otros proyectos de ciencia de datos en R [en mi portafolio de **visualizadores de datos sociales en R**](https://bastianolea.github.io/shiny_apps/)
