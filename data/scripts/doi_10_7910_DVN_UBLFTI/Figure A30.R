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

#Figure A30

Some_poor <- read.dta("Some_poor.dta")

set.seed(1814)
Att_ELA_Conditional_SP <- att_gt(yname = "ZELA", 
                                 tname = "GradeLevel", 
                                 idname = "ID", 
                                 gname = "Effgrade", 
                                 data = Some_poor,
                                 xformla = ~ Boy + Minority + Ecodis + Bus + TotalStudents + CertifiedTeacherTotal,
                                 est_method = "dr", 
                                 control_group = "notyettreated",
                                 bstrap = TRUE, 
                                 biters = 1000, 
                                 print_details = FALSE, 
                                 clustervars = "SchoolLEA", 
                                 panel = TRUE) 

ggdid(Att_ELA_Conditional_SP, ylim=c(-.2,.4)) 