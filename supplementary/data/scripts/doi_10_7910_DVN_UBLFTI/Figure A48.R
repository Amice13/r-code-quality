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

#Figure A48

data <- read.dta("Working_Data.dta")

#Math

set.seed(1814)
Att_Math_Unconditional <- att_gt(yname = "ZMath", 
                                 tname = "GradeLevel", 
                                 idname = "ID", 
                                 gname = "Effgrade", 
                                 data = data, 
                                 xformla = NULL,
                                 est_method = "dr", 
                                 control_group = "notyettreated", 
                                 bstrap = TRUE, 
                                 biters = 1000, 
                                 print_details = FALSE, 
                                 clustervars = "SchoolLEA", 
                                 panel = TRUE) 
Math_balance_Unconditional0 <- aggte(Att_Math_Unconditional, type = "dynamic")
FigureA48A<-ggdid(Math_balance_Unconditional0, ylim=c(-.2,.2))
Math_balance_Unconditional2 <- aggte(Att_Math_Unconditional, type = "dynamic", balance_e=2)
FigureA48C<-ggdid(Math_balance_Unconditional2, ylim=c(-.2,.2))

#ELA

set.seed(1814)
Att_ELA_Unconditional <- att_gt(yname = "ZELA", 
                                tname = "GradeLevel",
                                idname = "ID", 
                                gname = "Effgrade",
                                data = data, 
                                xformla = NULL,
                                est_method = "dr", 
                                control_group = "notyettreated", 
                                bstrap = TRUE, 
                                biters = 1000, 
                                print_details = FALSE, 
                                clustervars = "SchoolLEA", 
                                panel = TRUE) 
ELA_balance_Unconditional0 <- aggte(Att_ELA_Unconditional, type = "dynamic")
FigureA48B<-ggdid(ELA_balance_Unconditional0, ylim=c(-.2,.2))
ELA_balance_Unconditional2 <- aggte(Att_ELA_Unconditional, type = "dynamic", balance_e=2)
FigureA48D<-ggdid(ELA_balance_Unconditional2, ylim=c(-.2,.2))

ggdraw() +
  draw_plot(FigureA48A, x = 0, y = 0.5, width = .5, height = 0.45) +
  draw_plot(FigureA48B, x = 0.5, y = 0.5, width = .5, height = 0.45) +
  draw_plot(FigureA48C, x = 0, y = 0, width = .5, height = 0.45) +
  draw_plot(FigureA48D, x = 0.5, y = 0, width = .5, height = 0.45) +
  draw_plot_label(label = c("Panel a.", "Panel b."), size = 12,
                  x = c(0, 0), y = c(1,0.5))



