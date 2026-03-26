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

#Figure A23

Always_poor <- read.dta("Always_poor.dta")

#Math

set.seed(1814)
Att_Math_Unconditional_AP_NT <- att_gt(yname = "ZMath", 
                                       tname = "GradeLevel", 
                                       idname = "ID", 
                                       gname = "Effgrade", 
                                       data = Always_poor, 
                                       xformla = NULL,
                                       est_method = "dr", 
                                       control_group = "nevertreated", 
                                       bstrap = TRUE, 
                                       biters = 1000, 
                                       print_details = FALSE, 
                                       clustervars = "SchoolLEA", 
                                       panel = TRUE) 
Math_es_Unconditional_AP_NT0 <- aggte(Att_Math_Unconditional_AP_NT, type = "dynamic")
FigureA23A<-ggdid(Math_es_Unconditional_AP_NT0, ylim=c(-.4,.4))
Math_balance_Unconditional_AP_NT2 <- aggte(Att_Math_Unconditional_AP_NT, type = "dynamic", balance_e=2)
FigureA23C<-ggdid(Math_balance_Unconditional_AP_NT2, ylim=c(-.4,.4))

#ELA

set.seed(1814)
Att_ELA_Unconditional_AP_NT <- att_gt(yname = "ZELA", 
                                      tname = "GradeLevel", 
                                      idname = "ID", 
                                      gname = "Effgrade", 
                                      data = Always_poor, 
                                      xformla = NULL,
                                      est_method = "dr", 
                                      control_group = "nevertreated",  
                                      bstrap = TRUE, 
                                      biters = 1000, 
                                      print_details = FALSE, 
                                      clustervars = "SchoolLEA", 
                                      panel = TRUE) 
ELA_es_Unconditional_AP_NT0 <- aggte(Att_ELA_Unconditional_AP_NT, type = "dynamic")
FigureA23B<-ggdid(ELA_es_Unconditional_AP_NT0, ylim=c(-.4,.4))
ELA_balance_Unconditional_AP_NT2 <- aggte(Att_ELA_Unconditional_AP_NT, type = "dynamic", balance_e=2)
FigureA23D<-ggdid(ELA_balance_Unconditional_AP_NT2, ylim=c(-.4,.4))

ggdraw() +
  draw_plot(FigureA23A, x = 0, y = 0.5, width = .5, height = 0.45) +
  draw_plot(FigureA23B, x = 0.5, y = 0.5, width = .5, height = 0.45) +
  draw_plot(FigureA23C, x = 0, y = 0, width = .5, height = 0.45) +
  draw_plot(FigureA23D, x = 0.5, y = 0, width = .5, height = 0.45) +
  draw_plot_label(label = c("Panel a.", "Panel b."), size = 12,
                  x = c(0, 0), y = c(1,0.5))