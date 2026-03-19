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

#Figure A38

data <- read.dta("Working_Data.dta")

set.seed(1814)
Att_Attendance_Conditional <- att_gt(yname = "Attendance", 
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
Attendance_es_Conditional0 <- aggte(Att_Attendance_Conditional, type = "dynamic")
FigureA38A<-ggdid(Attendance_es_Conditional0, ylim=c(-.01,.02))
Attendance_balance_Conditional1 <- aggte(Att_Attendance_Conditional, type = "dynamic", balance_e=1)
FigureA38B<-ggdid(Attendance_balance_Conditional1, ylim=c(-.01,.02))
Attendance_balance_Conditional2 <- aggte(Att_Attendance_Conditional, type = "dynamic", balance_e=2)
FigureA38C<-ggdid(Attendance_balance_Conditional2, ylim=c(-.01,.02))

ggdraw() +
  draw_plot(FigureA38A, x = 0, y = 0.65, width = 1, height = 0.31) +
  draw_plot(FigureA38B, x = 0, y = 0.32, width = 1, height = 0.31) +
  draw_plot(FigureA38C, x = 0, y = 0, width = 1, height = 0.31) +
  draw_plot_label(label = c("Panel a.", "Panel b.","Panel c."), size = 12,
                  x = c(0, 0, 0), y = c(1,0.66,0.33))