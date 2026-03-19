rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
install.packages(packageurl, repos=NULL, type="source")


# *NOTE*: install RATest version 0.1.9 as indicated below
# *NOTE*: 'gridExtra', 'quantreg' need to be installed before installing RATest
install.packages('gridExtra')
install.packages('quantreg')
packageurl <- "https://cran.r-project.org/src/contrib/Archive/RATest/RATest_0.1.9.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
