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

#Figure A31

Always_poor <- read.dta("Always_poor.dta")

set.seed(1814)
Att_Math_Conditional_AP <- att_gt(yname = "ZMath", 
                                  tname = "GradeLevel", 
                                  idname = "ID", 
                                  gname = "Effgrade",
                                  data = Always_poor, 
                                  xformla = ~ Boy + Minority + Bus + TotalStudents + CertifiedTeacherTotal,
                                  est_method = "dr", 
                                  control_group = "notyettreated",  
                                  bstrap = TRUE, 
                                  biters = 1000, 
                                  print_details = FALSE, 
                                  clustervars = "SchoolLEA", 
                                  panel = TRUE) 

ggdid(Att_Math_Conditional_AP, ylim=c(-.3,.4)) 