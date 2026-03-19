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

#Figure A34

data <- read.dta("Working_Data.dta")

#Math

set.seed(1814)
Att_Math_Condicional <- att_gt(yname = "ZMath", 
                               tname = "GradeLevel", 
                               idname = "ID", 
                               gname = "Effgrade", 
                               data = data, 
                               xformla = ~ Boy + Minority + Ecodis + Bus + TotalStudents + CertifiedTeacherTotal,
                               est_method = "dr", 
                               control_group = "notyettreated",  
                               bstrap = TRUE, 
                               biters = 1000, 
                               print_details = FALSE, 
                               clustervars = "SchoolLEA",
                               panel = TRUE) 
Math_balance_Condicional0 <- aggte(Att_Math_Condicional, type = "dynamic")
FigureA34A<-ggdid(Math_balance_Condicional0, ylim=c(-.2,.2))
Math_balance_Condicional2 <- aggte(Att_Math_Condicional, type = "dynamic", balance_e=2)
FigureA34C<-ggdid(Math_balance_Condicional2, ylim=c(-.2,.2))

#ELA

set.seed(1814)
Att_ELA_Condicional <- att_gt(yname = "ZELA", 
                              tname = "GradeLevel", 
                              idname = "ID", 
                              gname = "Effgrade", 
                              data = data, 
                              xformla = ~ Boy + Minority + Ecodis + Bus + TotalStudents + CertifiedTeacherTotal,
                              est_method = "dr", 
                              control_group = "notyettreated", 
                              bstrap = TRUE, 
                              biters = 1000, 
                              print_details = FALSE, 
                              clustervars = "SchoolLEA", 
                              panel = TRUE) 

ELA_balance_Condicional0 <- aggte(Att_ELA_Condicional, type = "dynamic")
FigureA34B<-ggdid(ELA_balance_Condicional0, ylim=c(-.2,.2))
ELA_balance_Condicional2 <- aggte(Att_ELA_Condicional, type = "dynamic", balance_e=2)
FigureA34D<-ggdid(ELA_balance_Condicional2, ylim=c(-.2,.2))

ggdraw() +
  draw_plot(FigureA34A, x = 0, y = 0.5, width = .5, height = 0.45) +
  draw_plot(FigureA34B, x = 0.5, y = 0.5, width = .5, height = 0.45) +
  draw_plot(FigureA34C, x = 0, y = 0, width = .5, height = 0.45) +
  draw_plot(FigureA34D, x = 0.5, y = 0, width = .5, height = 0.45) +
  draw_plot_label(label = c("Panel a.", "Panel b."), size = 12,
                  x = c(0, 0), y = c(1,0.5))
