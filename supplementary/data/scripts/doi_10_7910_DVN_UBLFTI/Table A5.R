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

#Table A5

No_poor <- read.dta("No_poor.dta")

#Panel A

set.seed(1814)
Att_Math_Conditional_NP <- att_gt(yname = "ZMath", 
                                  tname = "GradeLevel",
                                  idname = "ID", 
                                  gname = "Effgrade",
                                  data = No_poor, 
                                  xformla = ~ Boy + Minority + Bus + TotalStudents + CertifiedTeacherTotal,
                                  est_method = "dr", 
                                  control_group = "notyettreated",  
                                  bstrap = TRUE, 
                                  biters = 1000, 
                                  print_details = FALSE, 
                                  clustervars = "SchoolLEA", 
                                  panel = TRUE) 
Math_agg_Conditional_NP <- aggte(Att_Math_Conditional_NP, type = "group")

set.seed(1814)
Att_ELA_Conditional_NP <- att_gt(yname = "ZELA", 
                                 tname = "GradeLevel", 
                                 idname = "ID", 
                                 gname = "Effgrade", 
                                 data = No_poor,
                                 xformla = ~ Boy + Minority + Bus + TotalStudents + CertifiedTeacherTotal,
                                 est_method = "dr", 
                                 control_group = "notyettreated", 
                                 bstrap = TRUE, 
                                 biters = 1000, 
                                 print_details = FALSE, 
                                 clustervars = "SchoolLEA", 
                                 panel = TRUE) 
ELA_agg_Conditional_NP <- aggte(Att_ELA_Conditional_NP, type = "group")

stargazer(cbind(results(Math_agg_Conditional_NP),results(ELA_agg_Conditional_NP)),summary=F,type="text", out="TableA5A.htm")

#Panel B

Some_poor <- read.dta("Some_poor.dta")

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
Math_agg_Conditional_SP <- aggte(Att_Math_Conditional_SP, type = "group")

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
ELA_agg_Conditional_SP <- aggte(Att_ELA_Conditional_SP, type = "group")

stargazer(cbind(results(Math_agg_Conditional_SP),results(ELA_agg_Conditional_SP)),summary=F,type="text", out="TableA5B.htm")

#Panel C

Always_poor <- read.dta("Always_poor.dta")

set.seed(1814)
Att_Math_Conditional_AP <- att_gt(yname = "ZMath", 
                                  tname = "GradeLevel", 
                                  idname = "ID", 
                                  gname = "Effgrade",
                                  data = Always_poor, 
                                  xformla = ~ Boy + Minority + Bus + TotalStudents + CertifiedTeacherTotal,
                                  est_method = "dr", 
                                  control_group = "notyettreated",  
                                  bstrap = TRUE, 
                                  biters = 1000, 
                                  print_details = FALSE, 
                                  clustervars = "SchoolLEA", 
                                  panel = TRUE) 
Math_agg_Conditional_AP <- aggte(Att_Math_Conditional_AP, type = "group")

set.seed(1814)
Att_ELA_Conditional_AP <- att_gt(yname = "ZELA", 
                                 tname = "GradeLevel", 
                                 idname = "ID", 
                                 gname = "Effgrade", 
                                 data = Always_poor, 
                                 xformla = ~ Boy + Minority + Bus + TotalStudents + CertifiedTeacherTotal,
                                 est_method = "dr", 
                                 control_group = "notyettreated",  
                                 bstrap = TRUE, 
                                 biters = 1000, 
                                 print_details = FALSE, 
                                 clustervars = "SchoolLEA", 
                                 panel = TRUE) 
ELA_agg_Conditional_AP <- aggte(Att_ELA_Conditional_AP, type = "group")

stargazer(cbind(results(Math_agg_Conditional_AP),results(ELA_agg_Conditional_AP)),summary=F,type="text", out="TableA5C.htm")





