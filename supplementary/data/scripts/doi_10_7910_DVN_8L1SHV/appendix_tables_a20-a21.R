## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
#################################################################################
#################################################################################
####################        PROMOTIONS AND LOCAL ELECTIONS        ###############
#################################################################################
#################################################################################

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
####### 1. Load data
#################################################################

# load full data
load("~/geog_rotation_full.rdata")

load("~/census_2000_dt.rdata")
# need to get rid of missingness in education
geog_rotation <- geog_rotation[eduname_cd != 999, ]
geog_rotation <- geog_rotation[years < 2013, ]
geog_rotation <- geog_rotation[years > 2000, ]


## select district-level employees
instansi_codes <- fread("~/Data Tabel Instansi.csv")
# instansi_codes <- fread("~/desktop/Data Tabel Instansi.csv")

colnames(instansi_codes) <- c("instansi","Department")

geog_rotation <- merge(geog_rotation,instansi_codes,by=c("instansi"),all.x=T)
#table(geog_rotation$Department)

geog_rotation$dept_string <- str_extract(geog_rotation$Department,"Pemerintah K")
#table(geog_rotation$dept_string)

# district_sample <- subset(geog_rotation,geog_rotation$dept_string=="Pemerintah K")
district_sample <- geog_rotation[ dept_string=="Pemerintah K" ]

# merge district-level data
districts <- read.dta("~/budgetdata_for_teachers_2016.dta")
# districts <- read.dta("~/desktop/budgetdata_for_teachers_2016.dta")
districts$size <- districts$exp_total/(districts$gdp * 1000000)

district_sample$kabupaten <- district_sample$Department
district_sample$kabupaten <- gsub("Pemerintah ","",district_sample$kabupaten)
district_sample$kabupaten <- gsub("Kab. ","",district_sample$kabupaten)

test_district_sample <- district_sample[1:10000, ]
save(test_district_sample, file = "~/test_district_sample.rdata")
head(district_sample$kabupaten)


setnames(district_sample, old = "years", new = "year")
district_sample <- merge(district_sample,districts,by=c("kabupaten","year"),all.x=T,allow.cartesian=TRUE)
district_sample[1:5, 1:5]

# muslim
census_dt[, pct := ifelse(is.na(pct), 0, pct)]
pct_muslim <- census_dt[ p07 == 1]
setnames(pct_muslim, old = "pct", new = "pct_muslim")
district_sample[, district_unique := paste0(prov_lokasi_kerja, kab_lokasi_kerja)]

district_sample <- merge(district_sample, pct_muslim, by = "district_unique")

#########################
### Analysis: Local elections and and gender
#########################
control_list <- "+gender_cd+umur_skrg+eduname_cd+factor(religion_cd)+yrs_in_bkn"
district_list <- "+elected_leader_l + incumbency + enp_all + golkar_share_all +
pdip_share_all + services_provision + rev_natural_pc_l + gini_l + 
rev_total_pc2_l + lpop_l + poverty_pc_l + lgdppc_l"

dv_list <- c("new_vals","jab_change","promotion")



########### Table 2: Gender
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "gender_cd*elected_leader_l*pct_muslim",
                           paste(control_list),
                           paste(district_list),
                           paste("| prov_tempat_lahir+instansi+year | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "gender_cd*elected_leader_l*pct_muslim",
                           paste(control_list),
                           paste(district_list),
                           paste("| pns_id+instansi+year | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "gender_cd*elected_leader_l*pct_muslim",
                           paste(control_list),
                           paste(district_list),
                           paste("| pns_id+instansi+golongan_id+year | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = district_sample)
m2 <- felm(fmla2, data = district_sample)
m3 <- felm(fmla3, data = district_sample)
m4 <- felm(fmla1, data = district_sample[post_dem_hire == 0])
m5 <- felm(fmla2, data = district_sample[post_dem_hire == 0])
m6 <- felm(fmla3, data = district_sample[post_dem_hire == 0])

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)

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
          title            = "Promotion Analysis: Gender",
          covariate.labels = c("Female",
                               "Elected Leader",
                               "Percent Muslim",
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
                               "Incumbency",
                               "ENP",
                               "Golkar Share",
                               "PDI-P Share",
                               "Services Provision",
                               "Natural Resource Revenue pc",
                               "Gini Index",
                               "Total Revenue pc",
                               "Log(Population)",
                               "Poverty pc",
                               "Log(GDP pc)",
                               "Female*Elected Leader",
                               "Female*Percent Muslim",
                               "Elected Leader * Percent Muslim",
                               "Female*Elected Leader * Percent Muslim"),
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
          out="~/appendix_tables_a20_pct_muslim_gender.tex"
)
# save(m1, m2, m3, m4, m5, m6, file = "~/paper_table2_districts_full_size.rdata")




########### Table 3: Religion
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*elected_leader_l*pct_muslim",
                           paste(control_list),
                           paste(district_list),
                           paste("| prov_tempat_lahir+instansi+year | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*elected_leader_l*pct_muslim",
                           paste(control_list),
                           paste(district_list),
                           paste("| pns_id+instansi+year | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*elected_leader_l*pct_muslim",
                           paste(control_list),
                           paste(district_list),
                           paste("| pns_id+instansi+golongan_id+year | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = district_sample)
m2 <- felm(fmla2, data = district_sample)
m3 <- felm(fmla3, data = district_sample)
m4 <- felm(fmla1, data = district_sample[post_dem_hire == 0])
m5 <- felm(fmla2, data = district_sample[post_dem_hire == 0])
m6 <- felm(fmla3, data = district_sample[post_dem_hire == 0])

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)

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
          title            = "Promotion Analysis: Education",
          covariate.labels = c("Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Elected Leader",
                               "Percent Muslim",
                               "Female",
                               "Age",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Years in Civil Service",
                               "Incumbency",
                               "ENP",
                               "Golkar Share",
                               "PDI-P Share",
                               "Services Provision",
                               "Natural Resource Revenue pc",
                               "Gini Index",
                               "Total Revenue pc",
                               "Log(Population)",
                               "Poverty pc",
                               "Log(GDP pc)",
                               "Protestant*Elected Leader",
                               "Catholic*Elected Leader",
                               "Buddhist*Elected Leader",
                               "Hindu*Elected Leader",
                               "Confucian*Elected Leader",
                               "Other*Elected Leader",
                               "Protestant*Percent Muslim",
                               "Catholic*Percent Muslim",
                               "Buddhist*Percent Muslim",
                               "Hindu*Percent Muslim",
                               "Confucian*Percent Muslim",
                               "Other*Percent Muslim",
                               "Elected Leader*Percent Muslim",
                               "Protestant*Elected Leader*Percent Muslim",
                               "Catholic*Elected Leader*Percent Muslim",
                               "Buddhist*Elected Leader*Percent Muslim",
                               "Hindu*Elected Leader*Percent Muslim",
                               "Confucian*Elected Leader*Percent Muslim",
                               "Other*Elected Leader*Percent Muslim"),
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
          out="~/appendix_tables_a21_pct_muslim_religion.tex"
)
# save(m1, m2, m3, m4, m5, m6, file = "~/paper_table3_districts_full_size.rdata")

