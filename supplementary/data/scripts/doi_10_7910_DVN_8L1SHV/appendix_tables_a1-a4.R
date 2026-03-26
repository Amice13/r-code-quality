## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
# regressions on the cluster
rm(list = ls())
options(stringsAsFactors = FALSE)
seed_to_use <- 216
set.seed(seed_to_use)

# install.packages(pkgs = c("data.table", "stargazer"), repos = "https://cloud.r-project.org/")
### Libraries
library(ggplot2)
library(foreign)
library(data.table)
library(stargazer)
library(lfe)


#################################################################
####### 1. Load data
#################################################################

# load full data
load("~/geog_rotation_full.rdata")

# need to get rid of missingness in education
geog_rotation <- geog_rotation[eduname_cd != 999, ]
geog_rotation <- geog_rotation[years < 2016, ]

structural_sample <- geog_rotation[new_vals > 0, ]

######################################################################
######################################################################
#######  TABLES FOR THE APPENDIX
######################################################################
######################################################################


control_list <- "+gender_cd+umur_skrg+eduname_cd+factor(religion_cd)+yrs_in_bkn"
dv_list <- c("new_vals","jab_change","promotion")

########### Table A1: Gender -----
fmla1 <-  as.formula(paste(paste(dv_list[1],"~"),
                           "post_dem+gender_cd*post_dem",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[1],"~"),
                           "post_dem+gender_cd*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[1],"~"),
                           "post_dem+gender_cd*post_dem",
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
          title            = "Echelon Analysis: Gender",
          covariate.labels = c("Post Democratization",
                               "Female",
                               "Age",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Years in Civil Service",
                               "Female*Post Democratization"),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Echelon","Echelon","Echelon","Echelon","Echelon","Echelon"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_A1_gender_eselon.tex"
)
save(m1,m2,m3,m4,m5,m6, file = "~/appendix_table_A1_gender_eselon.rdata")


########### Table A2: Religion -----
fmla1 <-  as.formula(paste(paste(dv_list[1],"~"),
                           "post_dem+factor(religion_cd)*post_dem",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[1],"~"),
                           "post_dem+factor(religion_cd)*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[1],"~"),
                           "post_dem+factor(religion_cd)*post_dem",
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
          title            = "Promotion Analysis: Echelon",
          covariate.labels = c("Post Democratization",
                               "Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Female",
                               "Age",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Years in Civil Service",
                               "Protestant*Post Democratization",
                               "Catholic*Post Democratization",
                               "Buddhist*Post Democratization",
                               "Hindu*Post Democratization",
                               "Confucian*Post Democratization",
                               "Other*Post Democratization"),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Echelon","Echelon","Echelon","Echelon","Echelon","Echelon"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_A2_religion_eselon.tex"
)
save(m1,m2,m3,m4,m5,m6, file = "~/appendix_table_A2_religion_eselon.rdata")

########### Table A3: Gender -----
fmla1 <-  as.formula(paste(paste("eselon_cat","~"),
                           "post_dem+gender_cd*post_dem",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste("eselon_cat","~"),
                           "post_dem+gender_cd*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste("eselon_cat","~"),
                           "post_dem+gender_cd*post_dem",
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
          title            = "Echelon Analysis: Gender",
          covariate.labels = c("Post Democratization",
                               "Female",
                               "Age",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Years in Civil Service",
                               "Female*Post Democratization"),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Five-Level Echelon","Five-Level Echelon","Five-Level Echelon","Five-Level Echelon","Five-Level Five-Level Echelon","Echelon"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_A3_gender_eselon.tex"
)
save(m1,m2,m3,m4,m5,m6, file = "~/appendix_table_A3_gender_eselon.rdata")


########### Table A4: Religion -----
fmla1 <-  as.formula(paste(paste("eselon_cat","~"),
                           "post_dem+factor(religion_cd)*post_dem",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste("eselon_cat","~"),
                           "post_dem+factor(religion_cd)*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste("eselon_cat","~"),
                           "post_dem+factor(religion_cd)*post_dem",
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
          title            = "Promotion Analysis: Echelon",
          covariate.labels = c("Post Democratization",
                               "Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Female",
                               "Age",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Years in Civil Service",
                               "Protestant*Post Democratization",
                               "Catholic*Post Democratization",
                               "Buddhist*Post Democratization",
                               "Hindu*Post Democratization",
                               "Confucian*Post Democratization",
                               "Other*Post Democratization"),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Five-Level Echelon","Five-Level Echelon","Five-Level Echelon","Five-Level Echelon","Five-Level Five-Level Echelon","Echelon"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_A4_religion_eselon.tex"
)
save(m1,m2,m3,m4,m5,m6, file = "~/appendix_table_A4_religion_eselon.rdata")

