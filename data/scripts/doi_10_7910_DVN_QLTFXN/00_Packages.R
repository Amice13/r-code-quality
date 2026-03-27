## This file cleans the workspace
## sets a seed and loads all R packages 
## used in analyses. It must be run prior to 
## any command file (file numbers 01-08)


rm(list=ls())

#setwd() #set a working directory here as needed 

set.seed(20210518)

# install packages if not already installed ---------------------------

# install.packages("stats")
# install.packages("list")      
# install.packages("tidyverse")  
# install.packages("broom")
# install.packages("MASS")
# install.packages("glm.predict")
# install.packages("dotwhisker")
# install.packages("gridExtra")
# install.packages("grid")
# install.packages("car")
# install.packages("ggplot2")
# install.packages("misreport")
# install.packages("stargazer")
# install.packages("Hmisc")
# install.packages("quadprog")
# install.packages("kableExtra")
# install.packages("table1")
# install.packages("knitr")
# install.packages("zoo")

# load packages ----------------------------------------------------------------

library(stats)
library(list)      
library(tidyverse)  
library(broom)
library(MASS)
library(glm.predict)
library(dotwhisker)
library(gridExtra)
library(grid)
library(car)
library(ggplot2)
library(misreport)
library(stargazer)
library(Hmisc)
library("quadprog")
library(kableExtra)
library(table1)
library(knitr)
library(zoo)
options(knitr.kable.NA = "")

