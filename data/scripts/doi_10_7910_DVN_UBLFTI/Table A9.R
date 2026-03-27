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

# Function

results<-function(object){
  df<-data.frame(g=c("Overall", object$egt),
                 att=c(object$overall.att,object$att.egt),
                 se=c(object$overall.se,object$se.egt),
                 crt=c(1.96,rep(object$crit.val.egt,length(object$egt))))
  df$lower95<-df$att-df$crt*df$se
  df$upper95<-df$att+df$crt*df$se
  df$sig<-ifelse(abs(df$att/df$se) >= df$crt,"*","")
  return(df[,names(df)!="crt"]) 
}

#Table A9

data <- read.dta("Working_Data.dta")

set.seed(1814)
Att_Attendance_Unconditional<- att_gt(yname = "Attendance", 
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
Attendance_agg_Unconditional <- aggte(Att_Attendance_Unconditional, type = "group")

stargazer(results(Attendance_agg_Unconditional),summary=F,type="text", out="TableA9.htm")