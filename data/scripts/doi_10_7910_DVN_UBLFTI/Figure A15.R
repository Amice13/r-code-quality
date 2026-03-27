##############################################################################
#                 School Breakfast and Academic Achievement                  #
#                Published in Economics of Education Review                  #
#                 Cuadros-Menaca, Thomsen and Nayga (2022)                   #
##############################################################################

rm(list=ls()) #start clean
setwd('Your Directory')
library(ggplot2)
library(ggpubr)
library("cowplot")
library(DRDID)
library(did)
library(stargazer)

#Figure A15

Some_poor <- read.dta("Some_poor.dta")

set.seed(1814)
Att_Math_Unconditional_SP_NT <- att_gt(yname = "ZMath", 
                                       tname = "GradeLevel", 
                                       idname = "ID", 
                                       gname = "Effgrade", 
                                       data = Some_poor, 
                                       xformla = NULL,
                                       est_method = "dr", 
                                       control_group = "nevertreated", 
                                       bstrap = TRUE, 
                                       biters = 1000,
                                       print_details = FALSE, 
                                       clustervars = "SchoolLEA", 
                                       panel = TRUE) 

ggdid(Att_Math_Unconditional_SP_NT, ylim=c(-.2,.5)) 