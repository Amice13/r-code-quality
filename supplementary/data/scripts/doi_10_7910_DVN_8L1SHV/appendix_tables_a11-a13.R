## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
#################################################################
####### 1. Load data
#################################################################
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


# load full data
load("~/geog_rotation_full.rdata")

# need to get rid of missingness in education
geog_rotation <- geog_rotation[eduname_cd != 999, ]
geog_rotation <- geog_rotation[years < 2016, ]

agency_ranks_gender <- geog_rotation[
  ,.N, by = .(new_vals, instansi, years, gender_cd)]
agency_ranks_gender <- agency_ranks_gender[ gender_cd == 1]
setnames(agency_ranks_gender, old = "N", new = "count_women")
agency_ranks_gender$gender_cd <- NULL
agency_ranks <- geog_rotation[
  ,.N, by = .(new_vals, instansi, years)]

gender_counts <- merge(agency_ranks, agency_ranks_gender, all.x = TRUE)
gender_counts$count_women[ is.na(gender_counts$count_women) ] <- 0
gender_counts <- gender_counts[ new_vals %in% c(10, 11)] 
gender_pct<- gender_counts[,.(
  total_top_eselon = sum(N), total_top_women = sum(count_women), 
  pct_women_leadership =  100*(sum(count_women)/sum(N))), 
  by =.(instansi, years)]
geog_rotation <- merge(geog_rotation, gender_pct, 
  by = c("instansi", "years"), all.x = TRUE)
geog_rotation$pct_women_leadership[
  is.na(geog_rotation$pct_women_leadership)] <- 0





####### Table A11 ----

control_list <- "+gender_cd+umur_skrg+eduname_cd+factor(religion_cd)+yrs_in_bkn"
dv_list <- c("new_vals","jab_change","promotion")


fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "gender_cd+pct_women_leadership+gender_cd*pct_women_leadership",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "gender_cd+pct_women_leadership+post_dem+gender_cd*pct_women_leadership+gender_cd*post_dem+gender_cd*pct_women_leadership*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[1],"~"),
                           "gender_cd+pct_women_leadership+gender_cd*pct_women_leadership",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla4 <-  as.formula(paste(paste(dv_list[1],"~"),
                           "gender_cd+pct_women_leadership+post_dem+gender_cd*pct_women_leadership+gender_cd*post_dem+gender_cd*pct_women_leadership*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))


# Estimation
m1 <- felm(fmla1, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m2 <- felm(fmla2, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m3 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m4 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)

m5 <- felm(fmla3, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m6 <- felm(fmla4, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m7 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m8 <- felm(fmla4, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)

# Get standard errors
se1 <- m1$cse
se2 <- m2$cse
se3 <- m3$cse
se4 <- m4$cse
se5 <- m5$cse
se6 <- m6$cse
se7 <- m7$cse
se8 <- m8$cse

# Table:
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,type="latex",notes = "Standard errors are clustered at the individual level.",
          style="qje", 
          title = "Promotion Analysis: Gender and Female Superiors",
          covariate.labels = c("Female",
                               "Pct Female Leadership",
                               "Post Democratization",
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
                               "Confucius",
                               "Other",
                               "Years in Civil Service",
                               "Female*Pct Female Leadership",
                               "Female*Post Democratization",
                               "Pct Female Leadership*Post Democratization",
                               "Female*Pct Female Leadership* PD"),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Promotion","Promotion","Promotion","Promotion","Echelon","Echelon","Echelon","Echelon"),
          se=list(se1,se2,se3,se4,se5,se6,se7,se8),
          add.lines = list(c("Sample", "Full", "Full","Pre-1999","Pre-1999","Full", "Full","Pre-1999","Pre-1999" ),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Individual FE", "Yes", "Yes","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_A11_female_superior.tex"
)

# A12, Aceh Effect ----
geog_rotation[, in_aceh := ifelse(prov_lokasi_kerja == 11, 1, 0)]


########### Table 2: Gender
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "in_aceh+post_dem+gender_cd*post_dem*in_aceh",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "in_aceh+post_dem+gender_cd*post_dem*in_aceh",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "in_aceh+post_dem+gender_cd*post_dem*in_aceh",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id+years | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m2 <- felm(fmla2, data = geog_rotation, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m3 <- felm(fmla3, data = geog_rotation, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)

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
          covariate.labels =  c("Working in Aceh",
                              "Post Democratization",
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
                               "Female*Post Democratization",
            "Working in Aceh*Female",
            "Working in Aceh*Post Democratization",
            "Working in Aceh*Post Democratization*Female"),
          dep.var.caption  = "Promotion",
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
          out="~/appendix_table_a12_gender_aceh.tex"
)
# save(m1,m2,m3,m4,m5,m6, file = "~/appendix_table2_full_models_aceh.rdata")

########### Table 3: Religion
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "in_aceh+post_dem+factor(religion_cd)*post_dem*in_aceh",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "in_aceh+post_dem+factor(religion_cd)*post_dem*in_aceh",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "in_aceh+post_dem+factor(religion_cd)*post_dem*in_aceh",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id+years | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m2 <- felm(fmla2, data = geog_rotation, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m3 <- felm(fmla3, data = geog_rotation, keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE, psdef=FALSE)

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
          covariate.labels =  c("Working in Aceh",
                              "Post Democratization",
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
                               "Other*Post Democratization",
                                "Protestant*Working in Aceh",
                               "Catholic*Working in Aceh",
                               "Buddhist*Working in Aceh",
                               "Hindu*Working in Aceh",
                               "Confucian*Working in Aceh",
                               "Other*Working in Aceh",
            "Post Democratization*Working in Aceh",
             "Protestant*Post Democratization*Working in Aceh",
                               "Catholic*Post Democratization*Working in Aceh",
                               "Buddhist*Post Democratization*Working in Aceh",
                               "Hindu*Post Democratization*Working in Aceh",
                               "Confucian*Post Democratization*Working in Aceh",
                               "Other*Post Democratization*Working in Aceh"),
          dep.var.caption  = "Promotion",
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
          out="~/appendix_table_A13_religion_aceh.tex"
)
# save(m1,m2,m3,m4,m5,m6, file = "~/appendix_table3_full_models_aceh.rdata")

