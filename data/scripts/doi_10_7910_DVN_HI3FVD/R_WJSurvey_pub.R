# Author: C. Smith (cd.smith@usm.edu)
# Title: Waterways Journal Survey Analysis
# Last Edited: July 2025

############################################################################
############# LIBRARIES & PACKAGES #########################################
############################################################################

# Useful Packages
require(nnet)
require(foreign)
require(ggplot2)
require(reshape2)
require(survey)
require(jtools)
require(tidyr)
require(dplyr)
require(haven)
require(stargazer)
require(flextable)
require(crosstable)

# Useful Packages
library(tidyr)
library(dplyr)
library(tibble)
library(survey)
library(svyVGAM)
library(jtools)
library(remotes)
library(haven)
library(stargazer)
library(flextable)
library(crosstable)

########################################################################
####### LOAD ANONYMIZED & CODED SURVEY DATA FILE #######################
########################################################################

wjs<-data.frame(read.csv('wjs.csv'))

#Preview Data
head(wjs)

#Create fields for survey weighting
#wjs$fpc<-20000
#wjs$weights1<-20000/nrow(wjs)

#survey design information data frame
wjs.design<-svydesign(ids=~1, data=wjs, fpc=~fpc, weights=~weights1)
#show survey design information
summary(wjs.design)
 
#designate the 'northeast' as the reference level for the 'Region' variable
wjs.design$variables$Region <- relevel(as.factor(wjs.design$variables$Region), ref = "Northeast")

#designate 'financial feasibility' as the reference level for the 'Q9_Score' variable
wjs.design$Q9_Score<- relevel(wjs.design$Q9_Score, ref = 1)



###############################################################################
##*******************************************************************##########
####### START OF SURVEY ANALYSIS ##############################################
##*******************************************************************##########
###############################################################################





########################################################################
####### WJS Models - Alt ##############################################
########################################################################

# Regressions of scores for Q3 - Q8 on other covariates

wjs.m1<-svyglm(Q3_Score~FortyAbove+Employ_Code+Occ_Code2+south+midwest+west, design=wjs.design)

wjs.m2<-svyglm(Q4_Score~FortyAbove+Employ_Code+Occ_Code2+south+midwest+west, design=wjs.design)

wjs.m3<-svyglm(Q5_Score~FortyAbove+Employ_Code+Occ_Code2+south+midwest+west, design=wjs.design)

wjs.m4<-svyglm(Q6_Score~FortyAbove+Employ_Code+Occ_Code2+Q3_Score+Q4_Score+Q5_Score+south+midwest+west, design=wjs.design)

wjs.m5<-svyglm(Q7_Score~FortyAbove+Employ_Code+Occ_Code2+Q3_Score+Q4_Score+Q5_Score+south+midwest+west, design=wjs.design)

wjs.m6<-svyglm(Q8_Score~FortyAbove+Employ_Code+Occ_Code2+Q3_Score+Q4_Score+Q5_Score+south+midwest+west, design=wjs.design)


#Q9 multinomial regression using 'multinom' function - NO INTERACTION TERMS
wjs.mmodel2 <- multinom(Q9_Text ~ FortyAbove+Employ_Code+Occ_Code2+south+midwest+west+Q3_Score+Q4_Score+Q5_Score+Q6_Score+Q7_Score+Q8_Score, data = wjs.design)
summary(wjs.mmodel2)
export_summs(wjs.mmodel2, to.file="html", file.name="wjs_Q9model_2.html", robust=TRUE, scale=TRUE)


##############################################################################
####### WJS MODEL2 - COEFFICIENTS PLOT #######################################
##############################################################################

#Plot of Model2 Coefficients

wjs.plots_mmodel2 <- plot_summs(wjs.mmodel2, exp=TRUE, coefs=c("Age40plus" = "FortyAbove", 
                                                    "Employed in Waterways" = "Employ_Code",
                                                    "Management Occupation" = "Occ_Code2",
                                                    "Q3 Response" = "Q3_Score",
                                                    "Q4 Response" = "Q4_Score",
                                                    "Q5 Response" = "Q5_Score",
                                                    "Q6 Response" = "Q6_Score",
                                                    "Q7 Response" = "Q7_Score",
                                                    "Q8 Response" = "Q8_Score",
                                                    "Region: South" = "south",
                                                    "Region: West" = "west",
                                                    "Region: Midwest" = "midwest"),
                             scale = TRUE, robust=TRUE)

wjs.plots_mmodel2


#####################################################################
####### MODEL OUTPUT TABLES #########################################
#####################################################################


export_summs(wjs.m1, wjs.m2, wjs.m3, wjs.m4, wjs.m5, wjs.m6, to.file="html", file.name="wjs_m1m6.html", robust=TRUE, scale=TRUE)

export_summs(wjs.mmodel2, to.file="html", file.name="wjs_m7_Q9.html", robust=TRUE, scale=TRUE)


#####################################################################
####### CONTINGENCY TABLES ##########################################
#####################################################################
#Data Frames for Descriptive Statistics

#contingency table 1: Region

wjs %>%
  mutate(FortyAbove_long=set_label(FortyAbove,"Age 40 & Above"),
         Employ_Code_long=set_label(Employ_Code,"Employed in Inland Waterway Sector"),
         Occ_Code2_long=set_label(Occ_Code2,"Management Occupation")) %>%
crosstable(cols = c(FortyAbove_long, Employ_Code_long, Occ_Code2_long), by = "Region", total="both", percent_digits=1, showNA="always") %>% 
  as_flextable(fontsizes = list(body=9,subheaders=9,header=10))



###### CROSSTABS: Q1 / PART1 #####################################


wjsq1q2 <- group_by(wjs,FortyAbove) %>%
  summarise( cnt= across(
    c("Q1_Financial", "Q1_PortUser", "Q1_GovReg", "Q1_Security", "Q1_Workforce", "Q1_ExistingPort"),sum), n=n())%>%
   mutate(pct = (cnt /n))  %>% unnest(cnt)

set_flextable_defaults(
  font.family = "Arial",
  font.size = 9,
  padding = 2,
  border.color = "#000000",
  line_spacing = 1.3,
  digits=2,
  theme_fun=theme_vanilla
)



ft.wjsq1q2 <- flextable(
                        data=wjsq1q2,
                        col_keys=c("FortyAbove","Q1_Financial","Q1_PortUser","Q1_GovReg","Q1_Security",
                                   "Q1_Workforce","Q1_ExistingPort","n"))

ft.wjsq1q2 <- set_header_labels(ft.wjsq1q2, 
                                FortyAbove="Age 40+",
                                Q1_Financial="Financial Feasibility",
                                Q1_PortUser="Port User Demand",
                                Q1_GovReg="Government Regulation",
                                Q1_Security="Security Risks",
                                Q1_Workforce="Workforce Issues",
                                Q1_ExistingPort="Existing Port Status",
                                n="Total")
ft.wjsq1q2 <- set_table_properties(ft.wjsq1q2 , layout = "autofit", width = .75)
ft.wjsq1q2 <- mk_par(x=ft.wjsq1q2, j=~. -FortyAbove -n,
                     part="body",
                    value=as_paragraph(fmt_n_percent(wjsq1q2[,2:7],wjsq1q2$pct)))
ft.wjsq1q2 

###### CROSSTABS: Q1 / PART2 ##################################### 

wjsq1q2.2 <- group_by(wjs,Employ_Code) %>%
  summarise( cnt= across(
    c("Q1_Financial", "Q1_PortUser", "Q1_GovReg", "Q1_Security", "Q1_Workforce", "Q1_ExistingPort"),sum), n=n())%>%
  mutate(pct = (cnt /n))  %>% unnest(cnt, names_repair = "universal") %>% unnest(pct, names_repair = "universal") 

wjsq1q2.2.alt <- group_by(wjs,Employ_Code) %>%
  summarise( cnt= across(
    c("Q1_Financial", "Q1_PortUser", "Q1_GovReg", "Q1_Security", "Q1_Workforce", "Q1_ExistingPort"),sum), n=n())%>%
  mutate(pct = (cnt /n)) %>% unnest(cnt)

wjsq1q2.2.alt 

ft.wjsq1q2.2.alt <- flextable(
  data=wjsq1q2.2.alt,
  col_keys=c("Employ_Code","Q1_Financial","Q1_PortUser","Q1_GovReg","Q1_Security","Q1_Workforce","Q1_ExistingPort","n"))

ft.wjsq1q2.2.alt <- set_header_labels(ft.wjsq1q2.2.alt, 
                                Employ_Code="Employed in Waterways",
                                Q1_Financial="Financial Feasibility",
                                Q1_PortUser="Port User Demand",
                                Q1_GovReg="Government Regulation",
                                Q1_Security="Security Risks",
                                Q1_Workforce="Workforce Issues",
                                Q1_ExistingPort="Existing Port Status",
                                n="Total")

ft.wjsq1q2.2.alt <- set_table_properties(ft.wjsq1q2.2.alt , layout = "autofit", width = .75)
ft.wjsq1q2.2.alt <- mk_par(x=ft.wjsq1q2.2.alt, j=~. -Employ_Code -n,
                     part="body",
                     value=as_paragraph(fmt_n_percent(wjsq1q2.2.alt[,2:7],wjsq1q2.2.alt$pct)))
 

###### CROSSTABS: Q1 / PART3 ##################################### 

wjsq1q2.3 <- group_by(wjs,Occ_Code2) %>%
  summarise( cnt= across(
    c("Q1_Financial", "Q1_PortUser", "Q1_GovReg", "Q1_Security", "Q1_Workforce", "Q1_ExistingPort"),sum), n=n())%>%
  mutate(pct = (cnt /n)) %>% unnest(cnt)


ft.wjsq1q2.3<- flextable(
  data=wjsq1q2.3,
  col_keys=c("Occ_Code2","Q1_Financial","Q1_PortUser","Q1_GovReg","Q1_Security","Q1_Workforce","Q1_ExistingPort","n"))


df.n.test<-ft.wjsq1q2.3$body$dataset$n/sum(ft.wjsq1q2.3$body$dataset$n)



ft.wjsq1q2.3 <- set_header_labels(ft.wjsq1q2.3, 
                                Employ_Code="Employed in Waterways",
                                Q1_Financial="Financial Feasibility",
                                Q1_PortUser="Port User Demand",
                                Q1_GovReg="Government Regulation",
                                Q1_Security="Security Risks",
                                Q1_Workforce="Workforce Issues",
                                Q1_ExistingPort="Existing Port Status",
                                n="Total")

ft.wjsq1q2.3 <- set_table_properties(ft.wjsq1q2.3 , layout = "autofit", width = .75)
ft.wjsq1q2.3 <- mk_par(x=ft.wjsq1q2.2.alt, j=~. -Employ_Code -n,
                     part="body",
                     value=as_paragraph(fmt_n_percent(wjsq1q2.3[,2:7],wjsq1q2.3$pct)))

df.pct1 <- rbind(wjsq1q2$pct,wjsq1q2.2.alt$pct,wjsq1q2.3$pct)
df.cnt1 <- rbind(ft.wjsq1q2$body$dataset[,2:8],ft.wjsq1q2.2.alt$body$dataset[,2:8],ft.wjsq1q2.3$body$dataset[,2:8])


head(df.pct1)

df.col1 <- data.frame(Variable=c("Age 40 & Above","Age 40 & Above","Employed in Inland Waterway Sector","Employed in Inland Waterway Sector","Management Occupation","Management Occupation"),Category=c("0","1","0","1","0","1"))
print(df.col1)

ft.wjsq1q2.2.combo1 <-0

ft.wjsq1q2.2.combo1 <- flextable(cbind(df.col1,rbind(ft.wjsq1q2$body$dataset[,2:8],ft.wjsq1q2.2.alt$body$dataset[,2:8],ft.wjsq1q2.3$body$dataset[,2:8])))

ft.wjsq1q2.2.combo1 <- set_header_labels(ft.wjsq1q2.2.combo1, 
                                  Variable="Characteristic",
                                  Category="Response Type",
                                  Q1_Financial="Financial Feasibility",
                                  Q1_PortUser="Port User Demand",
                                  Q1_GovReg="Government Regulation",
                                  Q1_Security="Security Risks",
                                  Q1_Workforce="Workforce Issues",
                                  Q1_ExistingPort="Existing Port Status",
                                  n="Total")

ft.wjsq1q2.2.combo1 <- set_table_properties(ft.wjsq1q2.2.combo1 , layout = "autofit", width = .8)
ft.wjsq1q2.2.combo1 <- mk_par(x=ft.wjsq1q2.2.combo1, j=~. -Variable -Category -n,
                       part="body",
                       value=as_paragraph(fmt_n_percent(df.cnt1[,1:6],df.pct1)))

print(ft.wjsq1q2.2.combo1)
ft.wjsq1q2.2.combo1 %>% 
  merge_v(
    j = ~ Variable)


###### CROSSTABS: Q1 / PART4 ##################################### 

########### wjsq1q2.999 ##############################
# This section of code creates a bottom row of column totals and binds it to
# the rest of the table

wjsq1q2.999 <-0

wjsq1q2.999 <- group_by(wjs,Occ_Code2) %>%
  summarise( cnt= across(
    c("Q1_Financial", "Q1_PortUser", "Q1_GovReg", "Q1_Security", "Q1_Workforce", "Q1_ExistingPort"),sum), n=n())%>%
  mutate(pct = (cnt /n)) %>%  unnest(cnt)

wjsq1q2.999$Occ_Code2 <- as.character(wjsq1q2.999$Occ_Code2)

wjsq1q2.999_with_total <- wjsq1q2.999 %>%
  bind_rows(summarise(., Occ_Code2="Total", Q1_Financial=sum(Q1_Financial), Q1_PortUser=sum(Q1_PortUser), Q1_GovReg=sum(Q1_GovReg),  Q1_Security=sum(Q1_Security), Q1_Workforce=sum(Q1_Workforce),  Q1_ExistingPort=sum(Q1_ExistingPort), n=sum(n)))

wjsq1q2.999_with_total

ft.wjsq1q2.3.999<- flextable(
  data=wjsq1q2.999_with_total,
  col_keys=c("Occ_Code2","Q1_Financial","Q1_PortUser","Q1_GovReg","Q1_Security","Q1_Workforce","Q1_ExistingPort","n"))


ft.wjsq1q2.3.999 <- set_header_labels(ft.wjsq1q2.3.999, 
                                  Employ_Code="Employed in Waterways",
                                  Q1_Financial="Financial Feasibility",
                                  Q1_PortUser="Port User Demand",
                                  Q1_GovReg="Government Regulation",
                                  Q1_Security="Security Risks",
                                  Q1_Workforce="Workforce Issues",
                                  Q1_ExistingPort="Existing Port Status",
                                  n="Total")

ft.wjsq1q2.3.999 <- set_table_properties(ft.wjsq1q2.3.999 , layout = "autofit", width = .75)
ft.wjsq1q2.3.999 <- mk_par(x=ft.wjsq1q2.3.999, j=~. -Employ_Code -n,
                       part="body",
                       value=as_paragraph(fmt_n_percent(wjsq1q2.999_with_total[,2:7],wjsq1q2.999_with_total$pct)))

df.pct1.999 <- rbind(wjsq1q2$pct,wjsq1q2.2.alt$pct,wjsq1q2.999_with_total$pct)
df.cnt1.999 <- rbind(ft.wjsq1q2$body$dataset[,2:8],ft.wjsq1q2.2.alt$body$dataset[,2:8],ft.wjsq1q2.3.999$body$dataset[,2:8])


df.col1 <- data.frame(Variable=c("Age 40 & Above","Age 40 & Above","Employed in Inland Waterway Sector","Employed in Inland Waterway Sector","Management Occupation","Management Occupation","Total"),Category=c("0","1","0","1","0","1",""))


ft.wjsq1q2.3.999.combo1 <-0

ft.wjsq1q2.3.999.combo1 <- flextable(cbind(df.col1,rbind(ft.wjsq1q2$body$dataset[,2:8],ft.wjsq1q2.2.alt$body$dataset[,2:8],ft.wjsq1q2.3.999$body$dataset[,2:8])))



ft.wjsq1q2.3.999.combo1 <- set_header_labels(ft.wjsq1q2.3.999.combo1, 
                                         Variable="Characteristic",
                                         Category="Response Type",
                                         Q1_Financial="Financial Feasibility",
                                         Q1_PortUser="Port User Demand",
                                         Q1_GovReg="Government Regulation",
                                         Q1_Security="Security Risks",
                                         Q1_Workforce="Workforce Issues",
                                         Q1_ExistingPort="Existing Port Status",
                                         n="Total")

ft.wjsq1q2.3.999.combo1 <- set_table_properties(ft.wjsq1q2.3.999.combo1 , layout = "autofit", width = .8)
ft.wjsq1q2.3.999.combo1 <- mk_par(x=ft.wjsq1q2.3.999.combo1, j=~. -Variable -Category -n,
                              part="body",
                              value=as_paragraph(fmt_n_percent(df.cnt1.999[,1:6],df.pct1.999)))

head(ft.wjsq1q2.3.999.combo1$body$dataset)

df.pct1.999$Q1_Financial

ft.wjsq1q2.3.999.combo1

ft.wjsq1q2.3.999.combo1 %>% 
  merge_v(
    j = ~ Variable)

########################## Q2 ############################################
######################### Part 1 #########################################

wjsq1q2.4 <- group_by(wjs,FortyAbove) %>%
  summarise( cnt= across(
    c("Q2_Shore", "Q2_Harbor", "Q2_Cargo", "Q2_Vehicles", "Q2_Rail"),sum), n=n())%>%
  mutate(pct = (cnt /n))  %>% unnest(cnt)

set_flextable_defaults(
  font.family = "Arial",
  font.size = 9,
  padding = 2,
  border.color = "#000000",
  line_spacing = 1.3,
  digits=2,
  theme_fun=theme_vanilla
)


ft.wjsq1q2.4 <- flextable(
  data=wjsq1q2.4,
  col_keys=c("FortyAbove","Q2_Shore", "Q2_Harbor", "Q2_Cargo", "Q2_Vehicles", "Q2_Rail","n"))
ft.wjsq1q2.4 <- set_header_labels(ft.wjsq1q2.4, 
                                FortyAbove="Age 40+",
                                Q2_Shore="Shore Power",
                                Q2_Harbor="Harbor Craft",
                                Q2_Cargo="Cargo Handling Equipment",
                                Q2_Vehicles="Ground Vehicles",
                                Q2_Rail="Rail",
                                n="Total")
ft.wjsq1q2.4  <- set_table_properties(ft.wjsq1q2.4, layout = "autofit", width = .8)
ft.wjsq1q2.4  <- mk_par(x=ft.wjsq1q2.4 , j=~. -FortyAbove -n,
                     part="body",
                     value=as_paragraph(fmt_n_percent(wjsq1q2.4[,2:6],wjsq1q2.4$pct)))
ft.wjsq1q2.4 

######################### Q2, Part 2 #########################################
########################  wjsq1q2.5 ##########################################


wjsq1q2.5 <- group_by(wjs,Employ_Code) %>%
  summarise( cnt= across(
    c("Q2_Shore", "Q2_Harbor", "Q2_Cargo", "Q2_Vehicles", "Q2_Rail"),sum), n=n())%>%
  mutate(pct = (cnt /n))  %>% unnest(cnt)

ft.wjsq1q2.5 <- flextable(
  data=wjsq1q2.5,
  col_keys=c("Employ_Code","Q2_Shore", "Q2_Harbor", "Q2_Cargo", "Q2_Vehicles","Q2_Rail","n"))

ft.wjsq1q2.5 <- set_header_labels(ft.wjsq1q2.5, 
                                      Employ_Code="Employed in Waterways",
                                      Q2_Shore="Shore Power",
                                      Q2_Harbor="Harbor Craft",
                                      Q2_Cargo="Cargo Handling Equipment",
                                      Q2_Vehicles="Ground Vehicles",
                                      Q2_Rail="Rail",
                                      n="Total")

ft.wjsq1q2.5 <- set_table_properties(ft.wjsq1q2.5, layout = "autofit", width = .8)
ft.wjsq1q2.5 <- mk_par(x=ft.wjsq1q2.5, j=~. -Employ_Code -n,
                           part="body",
                           value=as_paragraph(fmt_n_percent(wjsq1q2.5[,2:6], wjsq1q2.5$pct)))

ft.wjsq1q2.5

######################### Q2, Part 3 #########################################
########################  wjsq1q2.6 ##########################################

wjsq1q2.6 <- group_by(wjs,Occ_Code2) %>%
  summarise( cnt= across(
    c("Q2_Shore", "Q2_Harbor", "Q2_Cargo", "Q2_Vehicles", "Q2_Rail"),sum), n=n())%>%
  mutate(pct = (cnt /n)) %>% unnest(cnt)


ft.wjsq1q2.6<- flextable(
  data=wjsq1q2.6,
  col_keys=c("Occ_Code2","Q2_Shore", "Q2_Harbor", "Q2_Cargo", "Q2_Vehicles","Q2_Rail","n"))


ft.wjsq1q2.6 <- set_header_labels(ft.wjsq1q2.6, 
                                  Occ_Code2="Management Occupation",
                                  Q2_Shore="Shore Power",
                                  Q2_Harbor="Harbor Craft",
                                  Q2_Cargo="Cargo Handling Equipment",
                                  Q2_Vehicles="Ground Vehicles",
                                  Q2_Rail="Rail",
                                  n="Total")

ft.wjsq1q2.6 <- set_table_properties(ft.wjsq1q2.6 , layout = "autofit", width = .75)
ft.wjsq1q2.6 <- mk_par(x=ft.wjsq1q2.6, j=~. -Occ_Code2 -n,
                       part="body",
                       value=as_paragraph(fmt_n_percent(wjsq1q2.6[,2:6],wjsq1q2.6$pct)))


ft.wjsq1q2.6


######################### Q2, Part 4 #########################################
########################  wjsq1q2.7 ##########################################

df.pct2 <- rbind(wjsq1q2.4$pct, wjsq1q2.5$pct, wjsq1q2.6$pct)
df.cnt2 <- rbind(ft.wjsq1q2.4$body$dataset[,2:7],ft.wjsq1q2.5$body$dataset[,2:7],ft.wjsq1q2.6$body$dataset[,2:7])


df.col1 <- data.frame(Variable=c("Age 40 & Above","Age 40 & Above","Employed in Inland Waterway Sector","Employed in Inland Waterway Sector","Management Occupation","Management Occupation"),Category=c("0","1","0","1","0","1"))

ft.wjsq1q2.7.combo1 <-0

ft.wjsq1q2.7.combo1 <- flextable(cbind(df.col1,rbind(ft.wjsq1q2.4$body$dataset[,2:7],ft.wjsq1q2.5$body$dataset[,2:7],ft.wjsq1q2.6$body$dataset[,2:7])))

ft.wjsq1q2.7.combo1 <- set_header_labels(ft.wjsq1q2.7.combo1, 
                                         Variable="Characteristic",
                                         Category="Response Type",
                                         Q2_Shore="Shore Power",
                                         Q2_Harbor="Harbor Craft",
                                         Q2_Cargo="Cargo Handling Equipment",
                                         Q2_Vehicles="Ground Vehicles",
                                         Q2_Rail="Rail",
                                         n="Total")

ft.wjsq1q2.7.combo1 <- set_table_properties(ft.wjsq1q2.7.combo1 , layout = "autofit", width = .8)
ft.wjsq1q2.7.combo1 <- mk_par(x=ft.wjsq1q2.7.combo1, j=~. -Variable -Category -n,
                              part="body",
                              value=as_paragraph(fmt_n_percent(df.cnt2[,1:5],df.pct2)))

print(ft.wjsq1q2.7.combo1)

ft.wjsq1q2.7.combo1 %>% 
  merge_v(
    j = ~ Variable)


######################### Q2, Part 5 #########################################
########################  wjsq1q2.8 ##########################################

wjsq1q2.8 <-0

wjsq1q2.8 <- group_by(wjs,Occ_Code2) %>%
  summarise( cnt= across(
    c("Q2_Shore", "Q2_Harbor", "Q2_Cargo", "Q2_Vehicles", "Q2_Rail"),sum), n=n())%>%
  mutate(pct = (cnt /n)) %>%  unnest(cnt)

wjsq1q2.8$Occ_Code2 <- as.character(wjsq1q2.8$Occ_Code2)

wjsq1q2.8_with_total <- wjsq1q2.8 %>%
  bind_rows(summarise(., Occ_Code2="Total", Q2_Shore=sum(Q2_Shore), Q2_Harbor=sum(Q2_Harbor), Q2_Cargo=sum(Q2_Cargo),  Q2_Vehicles=sum(Q2_Vehicles), Q2_Rail=sum(Q2_Rail), n=sum(n)))

wjsq1q2.8_with_total


ft.wjsq1q2.8<- flextable(
  data=wjsq1q2.8_with_total,
  col_keys=c("Occ_Code2","Q2_Shore", "Q2_Harbor", "Q2_Cargo", "Q2_Vehicles", "Q2_Rail","n"))


ft.wjsq1q2.8<- set_header_labels(ft.wjsq1q2.8, 
                                      Occ_Code2="Management Occupation",
                                      Q2_Shore="Shore Power",
                                      Q2_Harbor="Harbor Craft",
                                      Q2_Cargo="Cargo Handling Equipment",
                                      Q2_Vehicles="Ground Vehicles",
                                      Q2_Rail="Rail",
                                      n="Total")

ft.wjsq1q2.8 <- set_table_properties(ft.wjsq1q2.8 , layout = "autofit", width = .75)
ft.wjsq1q2.8 <- mk_par(x=ft.wjsq1q2.8, j=~. -Employ_Code -n,
                           part="body",
                           value=as_paragraph(fmt_n_percent(wjsq1q2.8_with_total[,2:6],wjsq1q2.8_with_total$pct)))

df.pct2.8 <- rbind(wjsq1q2.4$pct,wjsq1q2.5$pct, wjsq1q2.8_with_total$pct)
df.cnt2.8 <- rbind(ft.wjsq1q2.4$body$dataset[,2:7],ft.wjsq1q2.5$body$dataset[,2:7],ft.wjsq1q2.8$body$dataset[,2:7])

#### Create Data Frame with Row Labels
df.col1 <- data.frame(Variable=c("Age 40 & Above","Age 40 & Above","Employed in Inland Waterway Sector","Employed in Inland Waterway Sector","Management Occupation","Management Occupation","Total"),Category=c("0","1","0","1","0","1",""))
#print(df.col1)


ft.wjsq1q2.8.combo1 <-0

ft.wjsq1q2.8.combo1 <- flextable(cbind(df.col1,rbind(ft.wjsq1q2.4$body$dataset[,2:7],ft.wjsq1q2.5$body$dataset[,2:7],ft.wjsq1q2.8$body$dataset[,2:7])))



ft.wjsq1q2.8.combo1 <- set_header_labels(ft.wjsq1q2.8.combo1, 
                                             Variable="Characteristic",
                                             Category="Response Type",
                                             Q2_Shore="Shore Power",
                                             Q2_Harbor="Harbor Craft",
                                             Q2_Cargo="Cargo Handling Equipment",
                                             Q2_Vehicles="Ground Vehicles",
                                             Q2_Rail="Rail",
                                             n="Total")

ft.wjsq1q2.8.combo1 <- set_table_properties(ft.wjsq1q2.8.combo1 , layout = "autofit", width = .8)
ft.wjsq1q2.8.combo1 <- mk_par(x=ft.wjsq1q2.8.combo1, j=~. -Variable -Category -n,
                                  part="body",
                                  value=as_paragraph(fmt_n_percent(df.cnt2.8[,1:5],df.pct2.8)))


ft.wjsq1q2.8.combo1

ft.wjsq1q2.8.combo1 %>% 
  merge_v(
    j = ~ Variable)


######################################################################
######################### Q3 #########################################
######################################################################

wjs.chr <-wjs
wjs.chr$Q3_Score <- as.character(wjs.chr$Q3_Score)
 
wjs.chr %>%
  mutate(FortyAbove_long=set_label(FortyAbove,"Age 40 & Above"),
         Employ_Code_long=set_label(Employ_Code,"Employed in Inland Waterway Sector"),
         Occ_Code2_long=set_label(Occ_Code2,"Management Occupation")) %>%
  crosstable(cols = c(FortyAbove_long, Employ_Code_long, Occ_Code2_long), by = "Q3_Score", total="both", percent_digits=1, showNA="always") %>% 
  as_flextable(fontsizes = list(body=9,subheaders=9,header=10))

######################################################################
######################### Q4 #########################################
######################################################################


wjs.chr <-wjs
wjs.chr$Q4_Score <- as.character(wjs.chr$Q4_Score)

wjs.chr %>%
  mutate(FortyAbove_long=set_label(FortyAbove,"Age 40 & Above"),
         Employ_Code_long=set_label(Employ_Code,"Employed in Inland Waterway Sector"),
         Occ_Code2_long=set_label(Occ_Code2,"Management Occupation")) %>%
  crosstable(cols = c(FortyAbove_long, Employ_Code_long, Occ_Code2_long), by = "Q4_Score", total="both", percent_digits=1, showNA="always") %>% 
  as_flextable(fontsizes = list(body=9,subheaders=9,header=10))


######################################################################
######################### Q5 #########################################
######################################################################


wjs.chr <-wjs
wjs.chr$Q5_Score <- as.character(wjs.chr$Q5_Score)

wjs.chr %>%
  mutate(FortyAbove_long=set_label(FortyAbove,"Age 40 & Above"),
         Employ_Code_long=set_label(Employ_Code,"Employed in Inland Waterway Sector"),
         Occ_Code2_long=set_label(Occ_Code2,"Management Occupation")) %>%
  crosstable(cols = c(FortyAbove_long, Employ_Code_long, Occ_Code2_long), by = "Q5_Score", total="both", percent_digits=1, showNA="always") %>% 
  as_flextable(fontsizes = list(body=9,subheaders=9,header=10))

######################################################################
######################### Q6 #########################################
######################################################################


wjs.chr <-wjs
wjs.chr$Q6_Score <- as.character(wjs.chr$Q6_Score)

wjs.chr %>%
  mutate(FortyAbove_long=set_label(FortyAbove,"Age 40 & Above"),
         Employ_Code_long=set_label(Employ_Code,"Employed in Inland Waterway Sector"),
         Occ_Code2_long=set_label(Occ_Code2,"Management Occupation")) %>%
  crosstable(cols = c(FortyAbove_long, Employ_Code_long, Occ_Code2_long), by = "Q6_Score", total="both", percent_digits=1, showNA="always") %>% 
  as_flextable(fontsizes = list(body=9,subheaders=9,header=10))

######################################################################
######################### Q7 #########################################
######################################################################

wjs.chr <-wjs
wjs.chr$Q7_Score <- as.character(wjs.chr$Q7_Score)

wjs.chr %>%
  mutate(FortyAbove_long=set_label(FortyAbove,"Age 40 & Above"),
         Employ_Code_long=set_label(Employ_Code,"Employed in Inland Waterway Sector"),
         Occ_Code2_long=set_label(Occ_Code2,"Management Occupation")) %>%
  crosstable(cols = c(FortyAbove_long, Employ_Code_long, Occ_Code2_long), by = "Q7_Score", total="both", percent_digits=1, showNA="always") %>% 
  as_flextable(fontsizes = list(body=9,subheaders=9,header=10))

######################################################################
######################### Q8 #########################################
######################################################################

wjs.chr <-wjs
wjs.chr$Q8_Score <- as.character(wjs.chr$Q8_Score)

wjs.chr %>%
  mutate(FortyAbove_long=set_label(FortyAbove,"Age 40 & Above"),
         Employ_Code_long=set_label(Employ_Code,"Employed in Inland Waterway Sector"),
         Occ_Code2_long=set_label(Occ_Code2,"Management Occupation")) %>%
  crosstable(cols = c(FortyAbove_long, Employ_Code_long, Occ_Code2_long), by = "Q8_Score", total="both", percent_digits=1, showNA="always") %>% 
  as_flextable(fontsizes = list(body=9,subheaders=9,header=10))


######################### Q9 #########################################
######################################################################

wjs.chr <-wjs
wjs.chr$Q9_Score <- as.character(wjs.chr$Q9_Score)

wjs.chr %>%
  mutate(FortyAbove_long=set_label(FortyAbove,"Age 40 & Above"),
         Employ_Code_long=set_label(Employ_Code,"Employed in Inland Waterway Sector"),
         Occ_Code2_long=set_label(Occ_Code2,"Management Occupation")) %>%
  crosstable(cols = c(FortyAbove_long, Employ_Code_long, Occ_Code2_long), by = "Q9_Score", total="both", percent_digits=1, showNA="always") %>% 
  as_flextable(fontsizes = list(body=9,subheaders=9,header=10))


####################################################################################
######################### CHI-SQUARE TESTS #########################################
####################################################################################

# Chi-Square Test: FortyAbove & Employ_Code
wjs.chisq1 <- svychisq(~FortyAbove+Employ_Code, wjs.design, statistic="F")

# Chi-Square Test: FortyAbove & Occ_Code2
wjs.chisq2 <- svychisq(~FortyAbove+Occ_Code2, wjs.design, statistic="F")

# Chi-Square Test: FortyAbove & South
wjs.chisq3 <- svychisq(~FortyAbove+south, wjs.design, statistic="F")

# Chi-Square Test: FortyAbove & West
wjs.chisq4 <- svychisq(~FortyAbove+west, wjs.design, statistic="F")

# Chi-Square Test: FortyAbove & Midwest
wjs.chisq5 <- svychisq(~FortyAbove+midwest, wjs.design, statistic="F")

# Chi-Square Test: Employ_Code & Occ_Code2
wjs.chisq6 <- svychisq(~Employ_Code+Occ_Code2, wjs.design, statistic="F")

# Chi-Square Test: Employ_Code & South
wjs.chisq7 <- svychisq(~Employ_Code+south, wjs.design, statistic="F")

# Chi-Square Test: Employ_Code & West
wjs.chisq8 <- svychisq(~Employ_Code+west, wjs.design, statistic="F")

# Chi-Square Test: Employ_Code & Midwest
wjs.chisq9 <- svychisq(~Employ_Code+midwest, wjs.design, statistic="F")

# Chi-Square Test: Occ_Code2 & South
wjs.chisq10 <- svychisq(~Occ_Code2+south, wjs.design, statistic="F")

# Chi-Square Test: Occ_Code2 & West
wjs.chisq11 <- svychisq(~Occ_Code2+west, wjs.design, statistic="F")

# Chi-Square Test: Occ_Code2 & Midwest
wjs.chisq12 <- svychisq(~Occ_Code2+midwest, wjs.design, statistic="F")


### Show output of All Chi-Square Tests
wjs.chisq1
wjs.chisq2
wjs.chisq3
wjs.chisq4
wjs.chisq5
wjs.chisq6
wjs.chisq7
wjs.chisq8
wjs.chisq9
wjs.chisq10
wjs.chisq11
wjs.chisq12
 

#Create Data Frame with all Chi-Square Test Results
wjs.chisq_all<-0
wjs.chisq_all <- data.frame(
  x2 = c(round(wjs.chisq1$statistic[[1]], digits=4),
         round(wjs.chisq2$statistic[[1]], digits=4),
         round(wjs.chisq3$statistic[[1]], digits=4),
         round(wjs.chisq4$statistic[[1]], digits=4),
         round(wjs.chisq5$statistic[[1]], digits=4),
         round(wjs.chisq6$statistic[[1]], digits=4),
         round(wjs.chisq7$statistic[[1]], digits=4),
         round(wjs.chisq8$statistic[[1]], digits=4),
         round(wjs.chisq9$statistic[[1]], digits=4),
         round(wjs.chisq10$statistic[[1]], digits=4),
         round(wjs.chisq11$statistic[[1]], digits=4),
         round(wjs.chisq12$statistic[[1]], digits=4)), 
  
  ndf = c(wjs.chisq1$parameter[[1]], 
          wjs.chisq2$parameter[[1]], 
          wjs.chisq3$parameter[[1]],
          wjs.chisq4$parameter[[1]],
          wjs.chisq5$parameter[[1]],
          wjs.chisq6$parameter[[1]],
          wjs.chisq7$parameter[[1]],
          wjs.chisq8$parameter[[1]],
          wjs.chisq9$parameter[[1]],
          wjs.chisq10$parameter[[1]],
          wjs.chisq11$parameter[[1]],
          wjs.chisq12$parameter[[1]]), 
  
 p_val = c(round(wjs.chisq1$p.value[[1]], digits=4),
           round(wjs.chisq2$p.value[[1]], digits=4),
           round(wjs.chisq3$p.value[[1]], digits=4),
           round(wjs.chisq4$p.value[[1]], digits=4),
           round(wjs.chisq5$p.value[[1]], digits=4),
           round(wjs.chisq6$p.value[[1]], digits=4),
           round(wjs.chisq7$p.value[[1]], digits=4),
           round(wjs.chisq8$p.value[[1]], digits=4),
           round(wjs.chisq9$p.value[[1]], digits=4),
           round(wjs.chisq10$p.value[[1]], digits=4),
           round(wjs.chisq11$p.value[[1]], digits=4),
           round(wjs.chisq12$p.value[[1]], digits=4)),
 
 row.names = c("FortyAbove & Employ_Code",
               "FortyAbove & Occ_Code2",
               "FortyAbove & South",
               "FortyAbove & West",
               "FortyAbove & Midwest",
               "Employ_Code & Occ_Code2",
               "Employ_Code & South",
               "Employ_Code & West",
               "Employ_Code & Midwest",
               "Occ_Code2 & South",
               "Occ_Code2 & West",
               "Occ_Code2 & Midwest"))


for(i in 1:nrow(wjs.chisq_all)){
  if(wjs.chisq_all$p_val[i]<=0.001){wjs.chisq_all$sig[i] = "***"}
  else if(wjs.chisq_all$p_val[i]<=0.01 & wjs.chisq_all$p_val[i]>0.001){wjs.chisq_all$sig[i] = "**"}
  else if(wjs.chisq_all$p_val[i]<=0.05 & wjs.chisq_all$p_val[i]>0.01){wjs.chisq_all$sig[i] = "*"}
  else {wjs.chisq_all$sig[i] = ""}}


wjs.chisq_all

#Generate Flextable of chi-square test results for all categorical variable pairs
wjs.chisq_all.ft <- flextable(wjs.chisq_all %>% rownames_to_column("Chi-Square Test"))
wjs.chisq_all.ft


####################################################################################
######################### Plots ###################################################
####################################################################################

svyhist(~Age, wjs.design, probability=TRUE)
svyhist(~Q3_Score, wjs.design, probability=TRUE)
svyhist(~Q4_Score, wjs.design, probability=TRUE)
svyhist(~Q5_Score, wjs.design, probability=TRUE)
svyhist(~Q6_Score, wjs.design, probability=TRUE)
svyhist(~Q7_Score, wjs.design, probability=TRUE)
svyhist(~Q8_Score, wjs.design, probability=TRUE)
svyhist(~Q9_Score, wjs.design, probability=TRUE)

svyboxplot(~Q3_Score~factor(FortyAbove), wjs.design, all.outliers = TRUE)
svyboxplot(~Q4_Score~factor(FortyAbove), wjs.design, all.outliers = TRUE)
svyboxplot(~Q5_Score~factor(FortyAbove), wjs.design, all.outliers = TRUE)
svyboxplot(~Q6_Score~factor(FortyAbove), wjs.design, all.outliers = TRUE)
svyboxplot(~Q7_Score~factor(FortyAbove), wjs.design, all.outliers = TRUE)
svyboxplot(~Q8_Score~factor(FortyAbove), wjs.design, all.outliers = TRUE)
svyboxplot(~Q9_Score~factor(FortyAbove), wjs.design, all.outliers = TRUE)

svyboxplot(~Q3_Score~factor(Employ_Code), wjs.design, all.outliers = TRUE)
svyboxplot(~Q4_Score~factor(Employ_Code), wjs.design, all.outliers = TRUE)
svyboxplot(~Q5_Score~factor(Employ_Code), wjs.design, all.outliers = TRUE)
svyboxplot(~Q6_Score~factor(Employ_Code), wjs.design, all.outliers = TRUE)
svyboxplot(~Q7_Score~factor(Employ_Code), wjs.design, all.outliers = TRUE)
svyboxplot(~Q8_Score~factor(Employ_Code), wjs.design, all.outliers = TRUE)
svyboxplot(~Q9_Score~factor(Employ_Code), wjs.design, all.outliers = TRUE)

svyboxplot(~Q3_Score~factor(Occ_Code2), wjs.design, all.outliers = TRUE)
svyboxplot(~Q4_Score~factor(Occ_Code2), wjs.design, all.outliers = TRUE)
svyboxplot(~Q5_Score~factor(Occ_Code2), wjs.design, all.outliers = TRUE)
svyboxplot(~Q6_Score~factor(Occ_Code2), wjs.design, all.outliers = TRUE)
svyboxplot(~Q7_Score~factor(Occ_Code2), wjs.design, all.outliers = TRUE)
svyboxplot(~Q8_Score~factor(Occ_Code2), wjs.design, all.outliers = TRUE)
svyboxplot(~Q9_Score~factor(Occ_Code2), wjs.design, all.outliers = TRUE)

wjs.plots_m1m6 <- plot_summs(wjs.m1, 
                             wjs.m2, 
                             wjs.m3, 
                             wjs.m4, 
                             wjs.m5, 
                             wjs.m6, coefs=c("Age40plus" = "FortyAbove",
                                             "Employed in Waterways" = "Employ_Code",
                                             "Management Occupation" = "Occ_Code2",
                                             "Q3 Response" = "Q3_Score",
                                             "Q4 Response" = "Q4_Score",
                                             "Q5 Response" = "Q5_Score",
                                             "Q6 Response" = "Q6_Score",
                                             "Q7 Response" = "Q7_Score",
                                             "Q8 Response" = "Q8_Score",
                                             "Region: South" = "south",
                                             "Region: West" = "west",
                                             "Region: Midwest" = "midwest"),
                             scale = TRUE, robust=TRUE)

wjs.plots_m1m6

wjs.plots_mmodel2 <- plot_summs(wjs.mmodel2, exp=TRUE, coefs=c("Age40plus" = "FortyAbove", 
                                                              "Employed in Waterways" = "Employ_Code",
                                                              "Management Occupation" = "Occ_Code2",
                                                              "Q3 Response" = "Q3_Score",
                                                              "Q4 Response" = "Q4_Score",
                                                              "Q5 Response" = "Q5_Score",
                                                              "Q6 Response" = "Q6_Score",
                                                              "Q7 Response" = "Q7_Score",
                                                              "Q8 Response" = "Q8_Score",
                                                              "Region: South" = "south",
                                                              "Region: West" = "west",
                                                              "Region: Midwest" = "midwest"),
                               point.size=2, point.shape=FALSE, scale = TRUE, robust=TRUE)

wjs.plots_mmodel2


#############################################################################
################### Wald Test & Predicted Probabilities #####################
#############################################################################

# Wald Test - Capturing the z-scores from the Q9 regression coefficients
wjs.mmodel2.z <- summary(wjs.mmodel2)$coefficients/summary(wjs.mmodel2)$standard.errors

# Determining the probabilities of the Q9 coefficients standardized
wjs.mmodel2.p <- (1 - pnorm(abs(wjs.mmodel2.z), 0, 1)) * 2
table(wjs.mmodel2.p)

wjs.mmodel2.p

### Risk Ratio
wjs.mmodel2.rr <-exp(coef(wjs.mmodel2))
wjs.mmodel2.rr

## Generate Predicted Probabilities of Fitted/Predicted Outcome Values
wjs.mmodel2.pp <- fitted(wjs.mmodel2)
head(wjs.mmodel2.pp)

pred_data2<-0

pred_data2 <- data.frame(FortyAbove = rep(c(0:1), each=500, length.out=1000),
                        Employ_Code = rep(c(0:1), each=500, length.out=1000),
                        Occ_Code2 = rep(c(0:1), each=500, length.out=1000),
                        south = rep(c(0:1), each=500, length.out=1000),
                        midwest = rep(c(0:1), each=500, length.out=1000),
                        west = rep(c(0:1), each=500, length.out=1000),
                        Q3_Score = rep(c(1:5), each=200, length.out=1000),
                        Q4_Score = rep(c(1:5), each=200, length.out=1000),
                        Q5_Score = rep(c(1:5), each=200, length.out=1000),
                        Q6_Score = rep(c(1:5), each=200, length.out=1000),
                        Q7_Score = rep(c(1:5), each=200, length.out=1000),
                        Q8_Score = rep(c(1:5), each=200, length.out=1000))


head(pred_data2)

pred_data2.pp<-cbind(pred_data2,predict(wjs.mmodel2,newdata=pred_data2,type="probs", se=TRUE))

head(pred_data2.pp)
by(pred_data2.pp[,13:15], pred_data2.pp$Q9_Score, colMeans)

pred_data2.pp2<-pred_data2.pp[,c(3,7,13:15)]
head(pred_data2.pp2)

### Melt Data
pred_data2.lpp<-melt(pred_data3.pp2, id.vars=c("Occ_Code2","Q9_Score"), value.name="probability")
head(pred_data2.lpp)
nrow(pred_data2.lpp)

#Plot of the average predicted Q9_Scores by Occupation Category
ggplot(pred_data2.lpp, aes(x=Q9_Score, y=probability, colour=factor(Occ_Code2)))+geom_point()+facet_grid(variable~.,scales="free")


#Misc: Creation of interaction terms
#wjs$q3q6_i<-0
#wjs$q3q7_i<-0
#wjs$q3q8_i<-0
#
#wjs$q3q6_i<-wjs$Q3_Score*wjs$Q6_Score
#wjs$q3q7_i<-wjs$Q3_Score*wjs$Q7_Score
# wjs$q3q8_i<-wjs$Q3_Score*wjs$Q8_Score

# Alternative Q9_Score Multinomial regression with interaction terms:
# wjs.Q9.i <- multinom(Q9_Score ~ FortyAbove+Employ_Code+Occ_Code2+south+midwest+west+Q3_Score+Q4_Score+Q5_Score+Q6_Score+Q7_Score+Q8_Score+q3q6_i+q3q7_i+q3q8_i, data=wjs.design)

# Q9 Regression
#wjs.mmodel3 <- svy_vglm(Q9_Score ~ FortyAbove+Employ_Code+Occ_Code2+south+midwest+west+Q3_Score+Q6_Score+Q7_Score+Q8_Score+q3q6_i+q3q7_i+q3q8_i, family=multinomial(refLevel=1), design=wjs.design)
#summary(wjs.mmodel3)

