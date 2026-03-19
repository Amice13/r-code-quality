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

#Figure A24

data <- read.dta("Working_Data.dta")

set.seed(1814)
Att_Attendance_Unconditional_NT <- att_gt(yname = "Attendance", 
                                          tname = "GradeLevel", 
                                          idname = "ID", 
                                          gname = "Effgrade", 
                                          data = data, 
                                          xformla = NULL,
                                          est_method = "dr", 
                                          control_group = "nevertreated",  
                                          bstrap = TRUE, 
                                          biters = 1000, 
                                          print_details = FALSE, 
                                          clustervars = "SchoolLEA", 
                                          panel = TRUE) 
Attendance_es_Unconditional_NT0 <- aggte(Att_Attendance_Unconditional_NT, type = "dynamic")
FigureA24A<-ggdid(Attendance_es_Unconditional_NT0, ylim=c(-.01,.02))
Attendance_balance_Unconditional_NT1 <- aggte(Att_Attendance_Unconditional_NT, type = "dynamic", balance_e=1)
FigureA24B<-ggdid(Attendance_balance_Unconditional_NT1, ylim=c(-.01,.02))
Attendance_balance_Unconditional_NT2 <- aggte(Att_Attendance_Unconditional_NT, type = "dynamic", balance_e=2)
FigureA24C<-ggdid(Attendance_balance_Unconditional_NT2, ylim=c(-.01,.02))

ggdraw() +
  draw_plot(FigureA24A, x = 0, y = 0.65, width = 1, height = 0.31) +
  draw_plot(FigureA24B, x = 0, y = 0.32, width = 1, height = 0.31) +
  draw_plot(FigureA24C, x = 0, y = 0, width = 1, height = 0.31) +
  draw_plot_label(label = c("Panel a.", "Panel b.","Panel c."), size = 12,
                  x = c(0, 0, 0), y = c(1,0.66,0.33))