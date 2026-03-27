 # calculo promedio precio de vacuna de influenza

library(tidyverse)
library(readr)

setwd("set directory")

influenza <- read_delim("set directory/Datos_vacunas_cenabast.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

influenza <- influenza %>% 
  rename(product = 3)

influenza <- influenza %>% 
  filter(product == "VAC ANTIINFLUENZ TRI O TETRAVAL 1DO JRP")

mean(influenza$`Precio unitario`)
