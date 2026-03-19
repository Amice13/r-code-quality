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

#Figure A36

Some_poor <- read.dta("Some_poor.dta")

#Math

set.seed(1814)
Att_Math_Conditional_SP <- att_gt(yname = "ZMath",
                                  tname = "GradeLevel", 
                                  idname = "ID", 
                                  gname = "Effgrade", 
                                  data = Some_poor, 
                                  xformla = ~ Boy + Minority + Bus + TotalStudents + CertifiedTeacherTotal,
                                  est_method = "dr", 
                                  control_group = "notyettreated", 
                                  bstrap = TRUE, 
                                  biters = 1000, 
                                  print_details = FALSE, 
                                  clustervars = "SchoolLEA", 
                                  panel = TRUE)
Math_es_Conditional_SP0 <- aggte(Att_Math_Conditional_SP, type = "dynamic")
FigureA36A<-ggdid(Math_es_Conditional_SP0, ylim=c(-.4,.4))
Math_balance_Conditional_SP2 <- aggte(Att_Math_Conditional_SP, type = "dynamic", balance_e=2)
FigureA36c<-ggdid(Math_balance_Conditional_SP2, ylim=c(-.4,.4))

#ELA

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
ELA_es_Conditional_SP0 <- aggte(Att_ELA_Conditional_SP, type = "dynamic")
FigureA36B<-ggdid(ELA_es_Conditional_SP0, ylim=c(-.4,.4))
ELA_balance_Conditional_SP2 <- aggte(Att_ELA_Conditional_SP, type = "dynamic", balance_e=2)
FigureA36D<-ggdid(ELA_balance_Conditional_SP2, ylim=c(-.4,.4))

ggdraw() +
  draw_plot(FigureA36A, x = 0, y = 0.5, width = .5, height = 0.45) +
  draw_plot(FigureA36B, x = 0.5, y = 0.5, width = .5, height = 0.45) +
  draw_plot(FigureA36C, x = 0, y = 0, width = .5, height = 0.45) +
  draw_plot(FigureA36D, x = 0.5, y = 0, width = .5, height = 0.45) +
  draw_plot_label(label = c("Panel a.", "Panel b."), size = 12,
                  x = c(0, 0), y = c(1,0.5))