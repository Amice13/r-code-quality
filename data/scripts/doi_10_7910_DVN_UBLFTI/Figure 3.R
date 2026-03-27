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

#Figure 3

data <- read.dta("Working_Data.dta")

set.seed(1814)
Att_ELA_conditional_NT <- att_gt(yname = "ZELA", 
                                 tname = "GradeLevel", 
                                 idname = "ID", 
                                 gname = "Effgrade", 
                                 data = data, 
                                 xformla = ~ Boy + Minority + Ecodis + Bus + TotalStudents + CertifiedTeacherTotal,
                                 est_method = "dr", 
                                 control_group = "nevertreated", 
                                 bstrap = TRUE, 
                                 biters = 1000, 
                                 print_details = FALSE, 
                                 clustervars = "SchoolLEA", 
                                 panel = TRUE) 

ggdid(Att_ELA_conditional_NT, ylim=c(-.2,.4))