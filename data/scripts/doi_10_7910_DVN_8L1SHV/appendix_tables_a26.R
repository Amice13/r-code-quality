## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
#############################################
# Appendix Table A26 #
#############################################
rm(list = ls())
options(stringsAsFactors = FALSE)

### Libraries
library(ggplot2)
library(foreign)
library(data.table)
library(stargazer)
library(lme4)
library(arm)
library(memisc)
library(vegan)
library(MASS)
library(haven)
library(lfe)



# Table A26: Effect of female representation

# load district-level data
load(file="~/Dropbox/BKN/Code/Replication/4_Data/district_data.Rdata")


# Model estimation
m1 <- felm(services_provision~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+poverty_pc_l+lgdppc_l
           | kode_neil+year|0|kode_neil,data=districts)
summary(m1)

m2 <- felm(poverty_pc~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+lgdppc_l
           | kode_neil+year|0|kode_neil,data=districts)
summary(m2)

m3 <- felm(birth~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+poverty_pc_l+lgdppc_l
           | kode_neil+year|0|kode_neil,data=districts)
summary(m3)

m4 <- felm(ner_schoolage~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+poverty_pc_l+lgdppc_l
           | kode_neil+year|0|kode_neil,data=districts)
summary(m4)

m5 <- felm(asphalt_road~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+poverty_pc_l+lgdppc_l
           | kode_neil+year|0|kode_neil,data=districts)
summary(m5)

m6 <- felm(safe_water~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+poverty_pc_l+lgdppc_l
           | kode_neil+year|0|kode_neil,data=districts)
summary(m6)

m7 <- felm(safe_sanitation~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+poverty_pc_l+lgdppc_l
           | kode_neil+year|0|kode_neil,data=districts)
summary(m7)

m8 <- felm(edu_exp_sh~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+poverty_pc_l+lgdppc_l
           | kode_neil+year|0|kode_neil,data=districts)
summary(m8)

m9 <- felm(health_exp_sh~women_prop_gov+elected_leader_l+enp_all+golkar_share_all+pdip_share_all+rev_natural_pc_l+gini_l+rev_total_pc2_l+lpop_l+poverty_pc_l+lgdppc_l
            | kode_neil+year|0|kode_neil,data=districts)
summary(m9)


se1 <- m1$cse
se2 <- m2$cse
se3 <- m3$cse
se4 <- m4$cse
se5 <- m5$cse
se6 <- m6$cse
se7 <- m7$cse
se8 <- m8$cse
se9 <- m9$cse


# Table
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,type="latex",style="qje", 
          title            = "Female Civil Servants and Service Delivery",
          covariate.labels = c("Share Female CS",
                               "Direct Elections",
                               "ENP",
                               "Golkar Share",
                               "PDI-P Share",
                               "Natural Resource Rev pc",
                               "Gini Index",
                               "Total Revenue pc",
                               "log(Population)",
                               "Poverty pc",
                               "log(GDP pc)"),
          dep.var.caption  = "",
          dep.var.labels.include = FALSE,
          column.labels   = c("Services Provision","Poverty","Births","Enroll","Roads","Water","Sanitation","Edu Exp","Health Exp"),
          se=list(se1,se2,se3,se4,se5,se6,se7,se8,se9),
          add.lines = list(c("District FE", "Yes","Yes", "Yes","Yes", "Yes","Yes", "Yes","Yes", "Yes"),
                           c("Year FE", "Yes","Yes", "Yes","Yes", "Yes","Yes", "Yes","Yes", "Yes")),
          digits=2,
          out="~/Dropbox/BKN/Code/Replication/appendix_table_A26.tex"
)
