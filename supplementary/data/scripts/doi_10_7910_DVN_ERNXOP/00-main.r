# The data can be obtained from the GESIS: http://www.gesis.org/en/allbus/allbus-home/

# You need..
# 1. Data on ALLBUS 2008: ZA4600_A08.txt
# 2. Data on left-right open-ended questions data: za4605_a08.dta
# 3. Data on categories: KATEGORIEN_100212_Master_deutsch_sort_modified.tx

# 1. is publicly available here https://dbk.gesis.org/DBKSearch/SDESC2.asp?no=4600&tab=3&db=E&dab=0 but
# 2. and 3. have to be requested from the GESIS directly.

# Data citations
  # 1. GESIS - Leibniz Institute for the Social Sciences (2011): ALLBUS/GGSS 2008 
  # (Allgemeine Bevölkerungsumfrage der Sozialwissenschaften/German General Social Survey 2008). 
  # GESIS Data Archive, Cologne. ZA4600 Data file Version 2.0.0.

  # 2. GESIS - Leibniz Institute for the Social Sciences (2009): ALLBUS/GGSS 2008 
  # (Allgemeine Bevölkerungsumfrage der Sozialwissenschaften/German General Social Survey 2008): 
  # Open answers to the question on associations with the terms "left" and "right"
  # GESIS Data Archive, Cologne. ZA4605 Data file Version 1.0.0.

  # 3. Züll C, Scholz E (2012) Assoziationen mit den politischen Richtungsbegriff en "links" und "rechts" im 
  # internationalen Vergleich: Kategorienschema für die Codierung offener Angaben. GESIS


# set cache=TRUE to load cached data
cache <- FALSE
# set bauer=TRUE to run R script as Paul (Windows vs. OS)
bauer <- TRUE
# set install=TRUE to re-install R packages
install <- TRUE


###################################
    ## PAUL
if (bauer) setwd("~/analysis")
    ## PABLO
if (!bauer) setwd("~/git/left-right-pobe")



  # PACKAGES
    packages <- c("dplyr", "stringr", "stargazer", "ggplot2", "scales",
                  "psych", "tm", "wordcloud", "car", "devtools", "Hmisc",
                  "glmnet", "emIRT", "slam", "ggplot2", "matrixStats", "lda", "quanteda")
    if (install) for (i in packages){install.packages(i)}
    if (install) devtools::install_github("kbenoit/quanteda")
    if (install) install.packages("https://cran.r-project.org/src/contrib/Archive/stm/stm_1.0.8.tar.gz",
      repo=NULL, type="source")
    library(foreign) 
    library(plyr)
    library(dplyr) 
    library(stm) 
    library(stringr) 
    library(stargazer) 
    library(ggplot2) 
    library(scales) 
    library(psych) 
    library(tm) 
    library(wordcloud) 
    library(car) 
    library(quanteda)
    library(glmnet)
    library(emIRT)
    library(slam)
    library(ggplot2)



############ MANUALLY DEFINED FUNCTIONS ############
    getTDM <- function (text, sparsity=0.001, stem=TRUE)
    {
      require(tm)
      myCorpus <- Corpus(VectorSource(text))
      if (Sys.info()["sysname"] == "Darwin") {
        myCorpus <- tm_map(myCorpus, function(x) iconv(x, from = "latin1",
                                                       to = "UTF-8"))
      }
      if (Sys.info()["sysname"] == "Windows") {
        myCorpus <- tm_map(myCorpus, function(x) iconv(x, from = "latin1",
                                                       to = "UTF-8"))
      }
      cat("Removing stopwords... ")
      myCorpus <- tm_map(myCorpus, removeWords, stopwords("german"))
      cat("done!\n")
      if (stem){
        cat("Stemming words... ")
        myCorpus <- tm_map(myCorpus, stemDocument, "german")
        cat("done!\n")
      }
      cat("Removing punctuation... ")
      myCorpus <- tm_map(myCorpus, removePunctuation)
      cat("done!\n")
      cat("Converting to lowercase... ")
      myCorpus <- tm_map(myCorpus, tolower)
      cat("done!\n")
      cat("Removing digits... ")
      myCorpus <- tm_map(myCorpus, removeNumbers)
      cat("done!\n")
      cat("Counting words of 3 of more characters that appear in at least",
          sparsity*100, "% of documents... ")
      myTdm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
      myTdm <- removeSparseTerms(myTdm, 1-sparsity)
      # words that appear in at least 0.1% of documents
      cat("done!\n")
      return(myTdm)
    }


############ running all analyses in paper  ###########

source("01-clean-data.r")
if (!cache) source("02-topic-models.r") # This script takes 1-2 hours; Use cache (+ results change for every run)
source("03-tab-3.r")
source("04-fig-4-5-14.r")
source("05-fig-6-7.r")
source("06-fig-8-table-5.r")
source("07-fig-9-table-2.r")
source("08-fig-15-16.r")
source("09-effect_on_party_placements.r")
source("10-fig-10-11-12-appendix-analysis.r")
source("11-irt-scaling-factor-analysis.r")



