## #+TITLE: Lematizador.R
## #+AUTHOR: Emilio Torres Manzanera
## #+DATE: Time-stamp: <2020-08-26 13:42 emilio on emilio-XPS-15-9570>
## #+TAGS:
## #+PROPERTY: header-args :results output :exports both :session
## https://torres.epv.uniovi.es/centon/lematizador-espanol-r.html

## Public domain 2020 Emilio Torres Manzanera
## https://torres.epv.uniovi.es/centon/lematizador-espanol-r.html

## There are well known English stemmers in R. However, there is not a R
## function that stemms Spanish words. So a stemmer function advocated to
## Spanish language has been built. It runs as follows. First, it checks
## whether the word is a common Spanish word. Second, it verifies whether
## the word is in a dictionary. Third, try to find a similar word (with the
## same lexeme) that appears in the dictionary. Finally, if previous steps
## are not successfully, it connects to a Internet site to find out the
## lexeme.





## library(data.table)
## library(fastmatch)
## source("lematizador.R")

## head(spmorphemes) # You can modify this list. See http://www.datsi.fi.upm.es/~coes/
## head(spdictionary) # You can modify this data.frame with your own words!
## head(spcommonwords) # You can modify this data.frame with your own words!

## system.time(lematizador("grandullón")) # First time it takes a lot of time (see ?fmatch)
## system.time(lematizador("grandullón"))

## lematizador( "des" ) # dar
## lematizador( "anduve" ) # andar
## lematizador( "casitas" ) ## NA : Try  http://www.datanalytics.com/blog/2011/12/13/un-lematizador-para-el-espanol-con-r-¿cutre-¿mejorable/
##                          ## Or modify the spdictionary!
## lematizador( "comimos" ) # comer
## lematizador( "queremos") # querer
## lematizador( "patatas" ) # patata

library(data.table)
library(fastmatch)

spmorphemes <- readRDS("otros/lematizador_es/spmorphemes.rds")
spdictionary <- fread("otros/lematizador_es/spdictionary.csv")
spcommonwords <- fread("otros/lematizador_es/spcommonwords.csv")


lematizador <- function(word,
                        all.words = FALSE,
                        commonwords =  spcommonwords,
                        dictionary = spdictionary,
                        morphemes = spmorphemes,
                        ...) {

    word <- tolower(as.character(word))
    getcanonicalword <- function(words, database, all.words = FALSE ) {
        pos <- fmatch(words, database$word,nomatch=0L )
        pos <- pos[pos>0L]
        if( all.words ) database$canonical[pos]
        else database$canonical[ pos[1] ]
    }

    ## Is it a spanish common word?
    canonical <-  getcanonicalword(word, commonwords, all.words)
    if( any(!is.na(canonical)) ) return(canonical)

    ## Is this word in the dictionary?
    canonical <-  getcanonicalword(word, dictionary, all.words)
    if( any(!is.na(canonical)) ) return(canonical)

    ## No. So we have to find out 'similar' words from the dictionary
    ## We split the word in root + desinence.
    ## And we paste root with other possibles desinences.

    ## Divide the word into root+desinence
    nch <- nchar(word)
    listroots <- lapply(1:(nch-1), function(i, word, nch) {
        root <- substring(word,1,i)
        desinence <- substring(word,i+1,nch)
        c(root, desinence)
    }, word,nch)
    listroots <- do.call(rbind, listroots)
    colnames(listroots) <- c("root","desinence")

    getderivational <- function(x, mylist) {
        pos <- fmatch(x, names(mylist))
        tmp <- mylist[[pos]]
        if(is.null(tmp) ) {NA}
        else {tmp}
    }

    ## Get the derivational morphemes that correspond to each desinence
    derivational <- lapply(listroots[,"desinence"], getderivational , spmorphemes)
    names(derivational) <- listroots[,"root"]
    derivational <- Filter(Negate(anyNA), derivational)

    ## Build the possible words: root + derivational morphemes
    possiblewords <-  unlist(lapply(seq_along(derivational), function(i){
        root <- names(derivational)[i]
        paste0(root, derivational[[i]])
    }))
    possiblewords <- unique(possiblewords)

    ## Get the canonical words!
    canonical <- getcanonicalword(possiblewords, dictionary, all.words )
    f <- !is.na(canonical)
    if( any(f))  return(canonical[f])

    ## No words until here.
    return(NA)

}


