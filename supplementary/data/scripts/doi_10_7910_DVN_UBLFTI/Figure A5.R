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

#Figure A5

Always_poor <- read.dta("Always_poor.dta")

set.seed(1814)
Att_Math_conditional_AP_NT <- att_gt(yname = "ZMath",
                                     tname = "GradeLevel", 
                                     idname = "ID", 
                                     gname = "Effgrade", 
                                     data = Always_poor, 
                                     xformla = ~ Boy + Minority  + Bus + TotalStudents + CertifiedTeacherTotal,
                                     est_method = "dr", 
                                     control_group = "nevertreated",  
                                     bstrap = TRUE, 
                                     biters = 1000, 
                                     print_details = FALSE, 
                                     clustervars = "SchoolLEA", 
                                     panel = TRUE) 
ggdid(Att_Math_conditional_AP_NT, ylim=c(-.2,.5)) 