
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

#Table 2

data <- read.dta("Working_Data.dta")

#Math

set.seed(1814)
Att_Math_conditional_NT <- att_gt(yname = "ZMath",
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

Math_agg_Conditional_NT <- aggte(Att_Math_conditional_NT, type = "group")
summary(Math_agg_Conditional_NT)

#ELA

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

ELA_agg_Conditional_NT <- aggte(Att_ELA_conditional_NT, type = "group")
summary(ELA_agg_Conditional_NT)

stargazer(cbind(results(Math_agg_Conditional_NT),results(ELA_agg_Conditional_NT)),summary=F,type="text", out="Table2.htm")
