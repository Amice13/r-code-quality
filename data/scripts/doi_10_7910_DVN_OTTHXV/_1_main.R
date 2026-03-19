################################################################################
# REPLICATION FILE FOR CLIMATE CRISIS AND POLICY INACTION IN INDONESIA##########
# ALLAN HSIAO AND NICHOLAS KUIPERS##############################################
# AMERICAN JOURNAL OF POLITICAL SCIENCE#########################################
################################################################################

rm(list = ls())

#set working directory to downloaded replication file
setwd("~/Dropbox/environment_attitudes/hk_ajps_replication_upload")


sink("./hk_code_log.txt")

library(gtsummary)
library(estimatr)
library(foreign)
library(tidyverse)
library(stargazer)
library(lmtest)
library(sandwich)
library(haven)
library(ggh4x)
library(gtable)
library(gt)
library(magrittr)
library(ggpubr)

#load helper functions
source("./_2_code/_functions.R")

#LOAD OBJECT NAMES TO KEEP AFTER EACH SCRIPT
keep_objs = c("estimation_data", "sikap_df", "sikap_36", "write_latex", "write_html", "keep_objs")

#make data
source("./_2_code/1_make_data.R")
rm(list=setdiff(ls(), keep_objs)) #clean up environment

################################################################################
#MAKE MAIN PAPER FIGURES AND TABLES#############################################
################################################################################

source("./_2_code/2_figure_1.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/2_figure_2.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/2_table_1.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/2_table_2.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/2_table_3.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/2_table_4.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/2_table_5.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/2_figure_3.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/2_figure_4.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

################################################################################
#MAKE APPENDIX PAPER FIGURES AND TABLES#########################################
################################################################################

source("./_2_code/3_figure_a1.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_figure_a2.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_figure_a5.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_figure_a6.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_figure_a7.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a1.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a2.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a3_a4_a5.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a6.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a7.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a8.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a9.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a10.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a11.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a12.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a13.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a14.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a15.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a16.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a17.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a18.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a19.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a20.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a21.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a22.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a23.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a24.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a25.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a26.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a27.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a28.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a29.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a30.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a31_32.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a33.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a34.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a35_a36_a37.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a38.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a39.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

source("./_2_code/3_table_a40.R",echo=T)
rm(list=setdiff(ls(), keep_objs)) #clean up environment

print(sessionInfo())
sink()
