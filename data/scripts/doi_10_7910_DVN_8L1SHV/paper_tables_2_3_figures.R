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
library(stringr)
# clustered fe function

#################################################################
####### 1. Load data
#################################################################

# load full data

load("~/geog_rotation_full.rdata")

# need to get rid of missingness in education
geog_rotation <- geog_rotation[eduname_cd != 999, ]
geog_rotation <- geog_rotation[years < 2016, ]

small_felm <- function(m){
 m$fe <- NULL
 m$residuals <- NULL
 m$r.residuals <- NULL
 m$fitted.values <- NULL
 m$response <- NULL
 m$cfactor <- NULL
 m$inv <- NULL
 m$STATS$promotion <- NULL
 m$clustervar <- NULL
 m
}


#################################################################
####### 2. Produce Tables/Figures
#################################################################

control_list <- "+gender_cd+umur_skrg+eduname_cd+factor(religion_cd)+yrs_in_bkn"
dv_list <- c("new_vals","jab_change","promotion")

########### Table 2: Gender
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+gender_cd*post_dem",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+gender_cd*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+gender_cd*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id+years | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation)
m2 <- felm(fmla2, data = geog_rotation)
m3 <- felm(fmla3, data = geog_rotation)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0])
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0])
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0])

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
          column.labels   = c("Promotion","Promotion","Promotion","Promotion","Promotion","Promotion"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/paper_table_2_gender.tex"
)

########### Table 3: Religion
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+factor(religion_cd)*post_dem",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+factor(religion_cd)*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "post_dem+factor(religion_cd)*post_dem",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id+years | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation)
m2 <- felm(fmla2, data = geog_rotation)
m3 <- felm(fmla3, data = geog_rotation)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0])
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0])
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0])

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
          title            = "Promotion Analysis: Religion",
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
          column.labels   = c("Promotion","Promotion","Promotion","Promotion","Promotion","Promotion"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/paper_table_3_religion.tex"
)

# gender-year plot ----

## Year Interaction
geog_rotation$high_ed <- ifelse(geog_rotation$eduname_cd>4,1,0)

control_list <- "+gender_cd+umur_skrg+high_ed+factor(religion_cd)+yrs_in_bkn"
dv_list <- c("new_vals","jab_change","promotion")
########### Table 2: Gender -----
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(years)+gender_cd*factor(years)",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(years)+gender_cd*factor(years)",
                           paste(control_list),
                           paste("| pns_id+instansi | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(years)+gender_cd*factor(years)",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m2 <- felm(fmla2, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m3 <- felm(fmla3, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)



m1 <- small_felm(m1)
m2 <- small_felm(m2)
m3 <- small_felm(m3)
m4 <- small_felm(m4)
m5 <- small_felm(m5)
m6 <- small_felm(m6)


save(m1,m2,m3,m4,m5,m6, file =  "gender_year_models.rdata")
#load( "gender_year_models.rdata")



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
          covariate.labels = c("1981",
                               "1982",
                               "1983",
                               "1984",
                               "1985",
                               "1986",
                               "1987",
                               "1988",
                               "1989",
                               "1990",
                               "1991",
                               "1992",
                               "1993",
                               "1994",
                               "1995",
                               "1996",
                               "1997",
                               "1998",
                               "1999",
                               "2000",
                               "2001",
                               "2002",
                               "2003",
                               "2004",
                               "2005",
                               "2006",
                               "2007",
                               "2008",
                               "2009",
                               "2010",
                               "2011",
                               "2012",
                               "2013",
                               "2014",
                               "2015",
                               "Female",
                               "Age",
                               "Higher Education",
                               "Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Years in Civil Service",
                               "1981*Female",
                               "1982*Female",
                               "1983*Female",
                               "1984*Female",
                               "1985*Female",
                               "1986*Female",
                               "1987*Female",
                               "1988*Female",
                               "1989*Female",
                               "1990*Female",
                               "1991*Female",
                               "1992*Female",
                               "1993*Female",
                               "1994*Female",
                               "1995*Female",
                               "1996*Female",
                               "1997*Female",
                               "1998*Female",
                               "1999*Female",
                               "2000*Female",
                               "2001*Female",
                               "2002*Female",
                               "2003*Female",
                               "2004*Female",
                               "2005*Female",
                               "2006*Female",
                               "2007*Female",
                               "2008*Female",
                               "2009*Female",
                               "2010*Female",
                               "2011*Female",
                               "2012*Female",
                               "2013*Female",
                               "2014*Female",
                               "2015*Female"),
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
          out="~/paper_table2_years_full.tex"
)


# make plot

# Effect of Gender by Year
zzTransparency <- 0.3
IV <- c("1981",
        "1982",
        "1983",
        "1984",
        "1985",
        "1986",
        "1987",
        "1988",
        "1989",
        "1990",
        "1991",
        "1992",
        "1993",
        "1994",
        "1995",
        "1996",
        "1997",
        "1998",
        "1999",
        "2000",
        "2001",
        "2002",
        "2003",
        "2004",
        "2005",
        "2006",
        "2007",
        "2008",
        "2009",
        "2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015")
Estimate <- m2$coefficients[46:80]
sd <- m2$cse[46:80]
min <- Estimate -1.96*sd
max <- Estimate + 1.96*sd
OutputPlot <- qplot(x=IV,y=Estimate,ymin=min,ymax=max,ylab = "Female Effect", xlab = NULL, geom = "blank" )
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(9/12), colour = "black", alpha = I(10/12))
OutputPlot <- OutputPlot + geom_vline(xintercept=which(IV == '1999'))
OutputPlot <- OutputPlot + annotate("text", x = "1995", y = 0.012, label = "Democratization")
OutputPlot <- OutputPlot + geom_linerange(aes(size = 1),lwd = I(9/12))
OutputPlot <- OutputPlot + scale_size_continuous(guide="none")
OutputPlot <- OutputPlot + geom_point(aes(x = IV, y = Estimate), colour = I(gray(0))) + theme_bw()+theme(axis.text.x = element_text(colour="black",size=15,angle=90,hjust=.5,vjust=.5),
                                                                                                          axis.text.y = element_text(colour="black",size=20,angle=0,hjust=1,vjust=0,face="bold"),axis.title.x = element_text(colour="black",size=22,angle=0,hjust=.5,vjust=0,face="bold"),
                                                                                                          axis.title.y = element_text(colour="black",size=22,angle=90,hjust=.5,vjust=.5,face="bold"))
OutputPlot
pdf(file="~/paper_figure_1_gender_years.pdf",width=10)
OutputPlot
dev.off()


# religion plots ----


control_list <- "+gender_cd+umur_skrg+high_ed+yrs_in_bkn"
dv_list <- c("new_vals","jab_change","promotion")


fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(years)+factor(religion_cd)*factor(years)",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(years)+factor(religion_cd)*factor(years)",
                           paste(control_list),
                           paste("| pns_id+instansi | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "factor(years)+factor(religion_cd)*factor(years)",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m2 <- felm(fmla2, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m3 <- felm(fmla3, data = geog_rotation, keepX = FALSE, keepCX = FALSE)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0], keepX = FALSE, keepCX = FALSE)




m1 <- small_felm(m1)
m2 <- small_felm(m2)
m3 <- small_felm(m3)
m4 <- small_felm(m4)
m5 <- small_felm(m5)
m6 <- small_felm(m6)


save(m1,m2,m3,m4,m5,m6, file =  "religion_year_models.rdata")


rm(m3, m4, m5, m6)


religion_coefs_m1 <- coefficients(m1)
religion_se_m1 <- m1$cse
model_coefs_m1 <- data.table(
  var_names = names(religion_coefs_m1), 
  betas = (religion_coefs_m1), se = religion_se_m1)


religion_coefs_m2 <- coefficients(m2)
religion_se_m2 <- m2$cse
model_coefs_m2 <- data.table(var_names = names(religion_coefs_m2), 
  betas = (religion_coefs_m2), se = religion_se_m2)


## this is ugly and and hacky, but it's how to get religions for tables ----
# str(geog_rotation$religion_cd)
religion_codes <- c("factor\\(religion_cd\\)2", "factor\\(religion_cd\\)3", 
  "factor\\(religion_cd\\)4", "factor\\(religion_cd\\)5", "factor\\(religion_cd\\)6", 
  "factor\\(religion_cd\\)7")
religion_names <- c("Protestant", "Catholic", "Hindu", "Buddhist", "Confucian", "Other")

model_coefs_m2$var_names[grep(model_coefs_m2$var_names, pattern = "relig")]

renaming_relig <- lapply(1:6, function(i){
  tmp <- model_coefs_m2[ grep(var_names, pattern =  religion_codes[i]) ]
  tmp[, var_names := str_replace_all(string = var_names, pattern = religion_codes[i], 
  replacement = religion_names[i])]
  tmp
})

model_coefs_m2 <- rbindlist(renaming_relig)
model_coefs_m2[, var_names:= str_replace_all(string = var_names, 
  pattern = "factor\\(years\\)", replacement = "")]

model_coefs_m2 <- model_coefs_m2[ grep(var_names, pattern = ":") ]
str_split(model_coefs_m2$var_names, pattern = ":")
model_coefs_m2[, c("Year", "Religion") := tstrsplit(var_names, ":") ]
model_coefs_m2[, `:=`(lower_95 = betas - 1.96*se, 
  upper_95 = betas + 1.96*se)]
model_coefs_m2 <- model_coefs_m2[ !(Religion %in% "Other")]
model_coefs_m2[, Religion := factor(Religion, levels = religion_names)]
model_coefs_m2[ Religion == "Confucian" & Year == 2008]
OutputPlot <- ggplot(data = model_coefs_m2, aes(x= Year, y = betas)) + 
  geom_hline(yintercept = 0, lwd = I(9/12), colour = "black", alpha = I(10/12)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.6) + 
  geom_vline(xintercept = which(model_coefs_m2$Year== "1999"))+ 
  facet_wrap(~Religion) + ylim(-.03, .02) + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .18, size = 6), 
  strip.background = element_blank(), strip.text = element_text()) + labs(y = "Interaction Term") + 
  annotate(geom = "text", x = which(model_coefs_m2$Year== "1996")[1], y = 0.02, label = "Democratization", size = 2)


pdf(file="paper_figure_2_disaggregated_religion.pdf",width=10)
OutputPlot
dev.off()

