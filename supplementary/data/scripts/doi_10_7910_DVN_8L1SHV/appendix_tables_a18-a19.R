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
library(data.table)
library(foreign)
library(stringr)
library(lfe)
library(stargazer)

#load data
leg_1999 <- fread("~/leg1999.csv")
leg_2004 <- fread("~/leg2004.csv")
leg_2009 <- fread("~/leg2009.csv")

instansi_codes <- read.csv("~/Data Tabel Instansi.csv")
districts <- read.dta("~/budgetdata_for_teachers_2016.dta")

load("~/geog_rotation_full.rdata")

# need to get rid of missingness in education
geog_rotation <- geog_rotation[eduname_cd != 999, ]
geog_rotation <- geog_rotation[years < 2013, ]
geog_rotation <- geog_rotation[years > 2000, ]

#instansi_codes <- read.csv("~/Dropbox/BKN/Data/Z_Archive/codes_old/Data Tabel Instansi.csv")
colnames(instansi_codes) <- c("instansi","Department")
geog_rotation <- merge(geog_rotation,instansi_codes,by=c("instansi"),all.x=T)
#table(geog_rotation$Department)
geog_rotation$dept_string <- str_extract(geog_rotation$Department,"Pemerintah K")
#table(geog_rotation$dept_string)
district_sample <- subset(geog_rotation,geog_rotation$dept_string=="Pemerintah K")
dim(district_sample)

district_sample$kabupaten <- district_sample$Department
district_sample$kabupaten <- gsub("Pemerintah ","",district_sample$kabupaten)
district_sample$kabupaten <- gsub("Kab. ","",district_sample$kabupaten)

#colnames(district_sample)[2] <- "year"
colnames(district_sample)[4] <- "year"
# district_sample$year <- as.numeric(district_sample$year)
# districts$year <- as.numeric(districts$year)

str(district_sample)

str(districts)

district_sample <- merge(
  district_sample, districts,by = c("kabupaten", "year"), 
  all.x = TRUE, allow.cartesian = TRUE)
# district_sample$id_m
# save(district_sample[1:1000, ], file = "~/test_district_sample2.rdata")

# what to do here
leg_1999_dt <- data.table( # set for 2001-2004
  year = 1999, id_m = leg_1999$id_m, muslim_sh = leg_1999$muslim_sh)

leg_1999_dt <- rbindlist(lapply(2001.0:2004.0, function(i){
  data.table( # set for 2001-2004
  year = as.numeric(i), id_m = leg_1999$id_m, muslim_sh = leg_1999$muslim_sh)
}))


leg_2004_dt <- rbindlist(lapply(2005:2009, function(i){
  data.table( # set for 2001-2004
  year = as.numeric(i), id_m = leg_2004$id_m, muslim_sh = leg_2004$muslim_sh)
}))


# leg_2009

leg_2009$`NAMA Kabupaten/Kota` <- gsub("^ [A-Z]|^ [a-z]","",leg_2009$`NAMA Kabupaten/Kota`, ignore.case = TRUE)
leg_2009$`NAMA Kabupaten/Kota` <- gsub("Pemerintah ","",leg_2009$`NAMA Kabupaten/Kota`, ignore.case = TRUE)
leg_2009$`NAMA Kabupaten/Kota` <- gsub("Kab. ","",leg_2009$`NAMA Kabupaten/Kota`, ignore.case = TRUE)
leg_2009$`NAMA Kabupaten/Kota` <- gsub("Tel. ","teluk ",leg_2009$`NAMA Kabupaten/Kota`, ignore.case = TRUE)
leg_2009$`NAMA Kabupaten/Kota` <- tolower(leg_2009$`NAMA Kabupaten/Kota` )
leg_2009$`NAMA Kabupaten/Kota` <- gsub("ota ","",leg_2009$`NAMA Kabupaten/Kota`, ignore.case = TRUE)
leg_2009$`NAMA Kabupaten/Kota` <- gsub("ab. ","",leg_2009$`NAMA Kabupaten/Kota`, ignore.case = TRUE)
leg_2009$`NAMA Kabupaten/Kota` <- gsub("ab.","",leg_2009$`NAMA Kabupaten/Kota`, ignore.case = TRUE)

merge_key <- unique(data.table(kabupaten = tolower(districts$kabupaten), id_m = districts$id_m))

leg_2009_id_m <- merge(leg_2009, merge_key, by.x = "NAMA Kabupaten/Kota", by.y = "kabupaten", all.x = TRUE)


leg_2009_dt <- rbindlist(lapply(2010:2014, function(i){
  data.table( # set for 2001-2004
  year = as.numeric(i), id_m = leg_2009_id_m$id_m, muslim_sh = leg_2009_id_m$muslim_sh)
}))

leg_results <- rbindlist( list(leg_1999_dt, leg_2004_dt, leg_2009_dt), use.names = TRUE)
leg_results <- na.omit(leg_results)
save(leg_results, file = "leg_results.rdata")
district_sample2 <- merge(
  district_sample, leg_results, by = c("id_m", "year"))



# add muslim share*elec*var_of_interest + elected leader
# muslim share*elec*var_of_interest * elected leader

#########################
### Analysis: Local elections and education and gender
#########################
control_list <- "+gender_cd+umur_skrg+eduname_cd+factor(religion_cd)+yrs_in_bkn"
district_list <- "+elected_leader_l + incumbency + enp_all + golkar_share_all +
pdip_share_all + services_provision + rev_natural_pc_l + gini_l + 
rev_total_pc2_l + lpop_l + poverty_pc_l + lgdppc_l"

dv_list <- c("new_vals","jab_change","promotion")

########### Table 2: Gender

fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "gender_cd*muslim_sh*elected_leader_l",
                           paste(control_list),
                           paste(district_list),
                           paste("| prov_tempat_lahir+instansi+year | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "gender_cd*muslim_sh*elected_leader_l",
                           paste(control_list),
                           paste(district_list),
                           paste("| pns_id+instansi+year | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "gender_cd*muslim_sh*elected_leader_l",
                           paste(control_list),
                           paste(district_list),
                           paste("| pns_id+instansi+golongan_id+year | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = district_sample2, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m2 <- felm(fmla2, data = district_sample2, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m3 <- felm(fmla3, data = district_sample2, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m4 <- felm(fmla1, data = district_sample2[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m5 <- felm(fmla2, data = district_sample2[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m6 <- felm(fmla3, data = district_sample2[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)

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
                                "Muslim Parties Vote Share",
                               "Elected Leader",
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
                               "Female*Muslim Parties Vote Share",
                               "Female*Elected Leader",
                               "Elected Leader*Muslim Parties Vote Share",
                               "Female*Muslim Parties Vote Share*Elected Leader"),
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
          out="~/appendix_table_a18_districts_muslim_share_gender.tex"
)



########### Table 3: Religion
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*muslim_sh*elected_leader_l",
                           paste(control_list),
                           paste(district_list),
                           paste("| prov_tempat_lahir+instansi+year | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*muslim_sh*elected_leader_l",
                           paste(control_list),
                           paste(district_list),
                           paste("| pns_id+instansi+year | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(religion_cd)*muslim_sh*elected_leader_l",
                           paste(control_list),
                           paste(district_list),
                           paste("| pns_id+instansi+golongan_id+year | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = district_sample2, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m2 <- felm(fmla2, data = district_sample2, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m3 <- felm(fmla3, data = district_sample2, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m4 <- felm(fmla1, data = district_sample2[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m5 <- felm(fmla2, data = district_sample2[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m6 <- felm(fmla3, data = district_sample2[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)

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
                               "Muslim Parties Vote Share",
                                "Elected Leader",
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
                               "Protestant*Muslim Parties Vote Share",
                               "Catholic*Muslim Parties Vote Share",
                               "Buddhist*Muslim Parties Vote Share",
                               "Hindu*Muslim Parties Vote Share",
                               "Confucian*Muslim Parties Vote Share",
                               "Other*Muslim Parties Vote Share",                               
                               "Protestant*Elected Leader",
                               "Catholic*Elected Leader",
                               "Buddhist*Elected Leader",
                               "Hindu*Elected Leader",
                               "Confucian*Elected Leader",
                               "Other*Elected Leader",
                               "Muslim Parties Vote Share*Elected Leader",
                               "Protestant*Muslim Parties Vote Share*Elected Leader",
                               "Catholic*Muslim Parties Vote Share*Elected Leader",
                               "Buddhist*Muslim Parties Vote Share*Elected Leader",
                               "Hindu*Muslim Parties Vote Share*Elected Leader",
                               "Confucian*Muslim Parties Vote Share*Elected Leader",
                               "Other*Muslim Parties Vote Share*Elected Leader"),
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
          out="~/appendix_table_a19_districts_muslim_share_religion.tex"
)
