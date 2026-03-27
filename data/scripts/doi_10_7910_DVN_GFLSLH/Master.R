################################################################################
 #----------------------------------------------------------------------------#
 #                      Master  R replication file for                        #
 #  Decoupling Visualization and Testing when Presenting Confidence Intervals #
 #                                     by                                     #
 #                  David A. Armstrong II and William Poirier                 #
 #----------------------------------------------------------------------------# 
################################################################################

#### 00. Load all packages and declare conflicted function preferences ####
## !!! Make sure to intall the required packages (see Readme.md for instructions) !!! ##
source("code/00_setup.R")

#### 01. Process Gibson Data ####
source("code/01_process_gibson_data.R")

#### 02. Process Iyengar and Westwood Data ####
source("code/02_process_iyengar_data.R")

#### 03. Process Muraoka and Rosas Data ####
source("code/03_process_muraoka_data.R")

#### 04. Make Figure 1 ####
source("code/04_Figure1.R")

#### 05. Refer to data from Gibson ####
## We discuss a few numbers from the data in the text around Figure 1.  
## These values appear in results/gibson_text_discussion.txt
## The system() command below should work on Unix-like systems or 
## perhaps Windows with the R build tools installed. 
system("R CMD batch code/05_gibson_text_discussion.R results/gibson_text_discussion.txt")

## Alternatively, the code below will print the results to the console. 
# source("code/05_gibson_text_discussion.R")

#### 06. Make Figure 2 ####
source("code/06_Figure2.R")

#### 07. Make Figure A1 ####
source("code/07_FigureA1.R")

#### 08. Make Figure A2 ####
source("code/08_FigureA2.R")

#### 09. Make Figure A3 ####
source("code/09_FigureA3.R")

#### 10. Make Figure A4 ####
source("code/10_FigureA4.R")

#### 11. Make Figure A5 ####
source("code/11_FigureA5.R")

#### 12. Make Figure A6, Table A1 and Table A2 ####
## File 12 does all the analysis of the Retail Sales 
## Data.  This file runs a model in Stan and then 
## returns TableA1.tex, TableA2.tex as well as FigureA6.png
source("code/12_Gill_Retail_Sales_Analysis.R")

 #### 13. Make Figure A7 ####
source("code/13_FigureA7.R")

#### 14. Make Figure A8 ####
source("code/14_FigureA8.R")

#### 15. Make Figure A9 ####
source("code/15_FigureA9.R")

#### 16. Make Figure A10 ####
source("code/16_FigureA10.R")

#### 17. Make Figure A11 ####
# execute 17_FigureA11.do in Stata

#### 18. Make Figure A12 ####
# execute 18_FigureA12.do in Stata

#### 19. To clean up, uncomment and run ####
# the code below to remove all objects from
# your workspace. 
# rm(list=ls())
