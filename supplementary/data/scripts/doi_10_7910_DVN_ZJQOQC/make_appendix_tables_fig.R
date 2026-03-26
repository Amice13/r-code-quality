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
require(rdrobust);require(rddensity);require(survival);require(cmprsk)

#devtools::install_version("dummies",version="1.5.6")
library(dummies)

# Set the working directory to the parent directory
# For replication, change the directory as needed
setwd("~/Dropbox/Spanish Civil War/replication_material/")


# Create subfolders within the parent directory
subfolders <- c("data", "code","code/paper","code/appendix","figure_output")
for (sub in subfolders) {
  dir.create(sub, recursive = TRUE, showWarnings = FALSE)
}


## Table A1 ##

source("code/appendix/table_A1.R",echo=TRUE)


## Figure A1 ##

source("code/appendix/figure_A1.R",echo=TRUE)


## Figure A2 ##

source("code/appendix/figure_A2.R",echo=TRUE)


## Table A2 ##

source("code/appendix/table_A2.R",echo=TRUE)


## Table A3 ##

source("code/appendix/table_A3.R",echo=TRUE)


## Table A4 ##

source("code/appendix/table_A4.R",echo=TRUE)


## Table A5 ##

source("code/appendix/table_A5.R",echo=TRUE)


## Figures A3-5 ##

source("code/appendix/figure_A345.R",echo=TRUE)


## Table A6 ##

source("code/appendix/table_A6.R",echo=TRUE)


## Table A7 Figure A6 ##

source("code/appendix/table_A7_fig_A6.R",echo=TRUE)


## Table A8 A9 A10 ##

source("code/appendix/table_A8910.R",echo=TRUE)


## Table A11 ##

source("code/appendix/table_A11.R",echo=TRUE)


## Table A12 ##

source("code/appendix/table_A12.R",echo=TRUE)


## Table A13 ##

source("code/appendix/table_A13.R",echo=TRUE)


## Figure A7 ##

source("code/appendix/figure_A7.R",echo=TRUE)


## Table A14 Figure A8##

source("code/appendix/table_A14_figA8.R",echo=TRUE)



