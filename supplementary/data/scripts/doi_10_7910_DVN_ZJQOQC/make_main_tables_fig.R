remove(list=ls())

install.packages("devtools")
install.packages("fixest")
install.packages("AER")
install.packages("pBracket")
install.packages("rdrobust")
install.packages("rddensity")
install.packages("survival")
install.packages("cmprsk")
install.packages("showtext")


require(devtools);require(fixest);require(AER);require(pBrackets);
require(rdrobust);require(rddensity);require(survival);require(cmprsk);require(showtext)
#devtools::install_version("dummies",version="1.5.6")
library(dummies)
library(showtext)
sessionInfo()
Sys.info()


#Please first copy and paste "helvetica-light-normal.ttf" into your font files if it doesn't already exist. 
#Then change the directory (regular ="") to your font directory.
#Usually the font files are located in some “standard” directories in the system.
#You can use font_paths() to check the current search path.
#The file "helvetica-light-normal.ttf" is provided in the replication file.
font_add("helvetica_light", regular = "/Library/Fonts/Microsoft/helvetica-light-normal.ttf")


# Set the working directory to the parent directory
# For replication, change the directory as needed
setwd("~/Dropbox/Spanish Civil War/replication_material/")


# Create subfolders within the parent directory
subfolders <- c("data", "code","code/paper","code/appendix","figure_output")
for (sub in subfolders) {
  dir.create(sub, recursive = TRUE, showWarnings = FALSE)
}



### Table 1 ####

source("code/paper/table_1.R",echo=TRUE)


### Table 2 ####

source("code/paper/table_2.R",echo=TRUE)


### Table 3 ####

source("code/paper/table_3.R",echo=TRUE)


### Table 4 ####

source("code/paper/table_4.R",echo=TRUE)


### Figure 4 ####

source("code/paper/figure_4.R",echo=TRUE)





