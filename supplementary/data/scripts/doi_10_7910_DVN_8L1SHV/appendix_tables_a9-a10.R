## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
rm(list = ls())
options(stringsAsFactors = FALSE)
seed_to_use <- 216
set.seed(seed_to_use)

### Libraries
library(foreign)
library(data.table)
library(stargazer)
library(lfe)
library(stringr)

#################################################################
####### 1. Load data ----
#################################################################

# load full data
load("~/geog_rotation_full.rdata")

load("~/census_2000_dt.rdata")
# need to get rid of missingness in education
geog_rotation <- geog_rotation[eduname_cd != 999, ]
geog_rotation <- geog_rotation[years < 2016, ]


control_list <- "+gender_cd+umur_skrg+eduname_cd+factor(religion_cd)+yrs_in_bkn"
dv_list <- c("new_vals","jab_change","promotion")


########### Table 9: Education*Female&PD
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+eduname_cd*post_dem+eduname_cd*gender_cd+eduname_cd*gender_cd*post_dem",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+eduname_cd*post_dem+eduname_cd*gender_cd+eduname_cd*gender_cd*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+eduname_cd*post_dem+eduname_cd*gender_cd+eduname_cd*gender_cd*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id+years | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m2 <- felm(fmla2, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m3 <- felm(fmla3, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)

# Get standard errors
se1 <- m1$cse
se2 <- m2$cse
se3 <- m3$cse
se4 <- m4$cse
se5 <- m5$cse
se6 <- m6$cse


# Table:
stargazer(m1,m2,m3,m4,m5,m6,type="latex",notes = "Standard errors are clustered at the individual level.",
          style="qje",
          title            = "Promotion Analysis: Education and Gender",
          covariate.labels = c("Post Democratization",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Female",
                               "Age",
                               "Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Years in Civil Service",
                               "Education: Junior High*Post Democratization",
                               "Education: Senior High*Post Democratization",
                               "Education: Diploma I/II/III*Post Democratization",
                               "Education: Diploma IV/S1*Post Democratization",
                               "Education: Post-Graduate*Post Democratization",
                               "Education: Junior High*Female",
                               "Education: Senior High*Female",
                               "Education: Diploma I/II/III*Female",
                               "Education: Diploma IV/S1*Female",
                               "Education: Post-Graduate*Female",
                               "Post Democratization*Female",
                               "Education: Junior High*Post Democratization*Female",
                               "Education: Senior High*Post Democratization*Female",
                               "Education: Diploma I/II/III*Post Democratization*Female",
                               "Education: Diploma IV/S1*Post Democratization*Female",
                               "Education: Post-Graduate*Post Democratization*Female"),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Promotion","Promotion","Promotion","Promotion","Promotion","Promotion"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_a9_education_gender.tex"
)
# save(m1,m2,m3,m4,m5,m6, file = "~/appendix_table9_full_models.rdata")


###### Table 10: Effect of Election Years and Gender

fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*gender_cd*post_dem",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*gender_cd*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*gender_cd*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id+years | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m2 <- felm(fmla2, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m3 <- felm(fmla3, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)

# # Get standard errors
# load("~/three_way_interaction_full_models.rdata")

se1 <- m1$cse
se2 <- m2$cse
se3 <- m3$cse
se4 <- m4$cse
se5 <- m5$cse
se6 <- m6$cse
summary(m1)
save(m1,m2,m3,m4,m5,m6, file = "~/two_way_interaction_full_models.rdata")

# 
# Table:
stargazer(m1,m2,m3,m4,m5,m6,type="latex",notes = "Standard errors are clustered at the individual level.",
          style="qje",
          title            = "Promotion Analysis: Religion, Education, and Gender",
          covariate.labels = c("Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Female",
                               "Post Democratization",
                               "Age",
                               "Years in Civil Service",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Protestant*Female",
                               "Catholic*Female",
                               "Buddhist*Female",
                               "Hindu*Female",
                               "Confucian*Female",
                               "Other*Female",
                               "Protestant*Post Democratization",
                               "Catholic*Post Democratization",
                               "Buddhist*Post Democratization",
                               "Hindu*Post Democratization",
                               "Confucian*Post Democratization",
                               "Other*Post Democratization",  
                               "Female* Post Democratization",
                               "Protestant*Female*Post Democratization",
                               "Catholic*Female*Post Democratization",
                               "Buddhist*Female*Post Democratization",
                               "Hindu*Female*Post Democratization",
                               "Confucian*Female*Post Democratization",
                               "Other*Female*Post Democratization"
            ),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Promotion","Promotion","Promotion","Promotion","Promotion","Promotion"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_a10_education_gender_religion.tex"
)

