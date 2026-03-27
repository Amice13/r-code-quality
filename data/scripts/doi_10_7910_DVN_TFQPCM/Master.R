############# Run before all replication scripts for the paper
## Contagion, Confounding, and Causality: Confronting the Three C’s of Observational
##  Political Networks Research by Medha Uppala and Bruce A Desmarais


install.packages("devtools")
library("devtools")

install_version("ergm", version = "4.2.1")
install_version("ggplot2", version = "3.3.6")
install_version("dplyr", version = "1.0.9")
install_version("tidyr", version = "1.2.0")
install_version("sna", version = "2.6")

install.packages("CADFtest")      ### Necessary for punitroots pkg
install.packages("fUnitRoots")    ### Necessary for punitroots pkg
install.packages("Packages/punitroots_0.0-2.tar.gz", 
                  repos = NULL, 
                  lib = "Packages")

install_version("collapse", version= "1.7.6")
install_version("tseries", version="0.10-51")

library(ergm)
library(dplyr)
library(ggplot2)
library(tidyr)
library(punitroots, lib.loc = "Packages")
library(sna)
library(tseries)
library(collapse)


