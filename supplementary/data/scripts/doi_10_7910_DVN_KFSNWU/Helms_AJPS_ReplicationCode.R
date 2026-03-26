#############################################################################
## Replication code for                                                    ##
## Global Economic Integration and Nativist Politics in Emerging Economies ##
## Benjamin Helms                                                          ##
## May 9, 2022                                                             ##
## American Journal of Political Science                                   ##
#############################################################################

## This file contains code to replicate all empirical analyses in the
## main text and Supplemental Information, except for Appendix Table A.4 (see Readme).

## Operating system: MacOS Mojave version 10.14.6 (x86_64-apple-darwin17.0 64-bit)
## Statistical software: R version 4.0.5

## Enter the working directory containing the analysis datasets here:
setwd("")

## The following packages must be installed to run the script:
## install.packages("sf") ## Spatial data manipulation tools
## install.packages("fixest") ## Estimate high-dimensional fixed effects models
## install.packages("tidyverse") ## Data manipulation tools

## Load necessary packages.
library(sf)
library(fixest)
library(tidyverse)

## The following commands load each analysis dataset.

## Main analysis dataset
crime_panel <- read_csv("analysis_dataset_1.csv")

## Internal migration analysis datasets
migration_total <- read_csv("analysis_dataset_2.csv")

migration_male <- read_csv("analysis_dataset_3.csv")

migration_female <- read_csv("analysis_dataset_4.csv")

## Investment data including Rajasthan analysis dataset
investment <- read_csv("analysis_dataset_5.csv")

## Election analysis dataset
elections <- read_csv("analysis_dataset_6.csv")

## World Population Policies analysis dataset
wpp <- read_csv("analysis_dataset_7.csv")

#######################################################
## Figure 1: Capital Investment in Textiles in India ##
#######################################################

## Select necessary variables for graphing.
yearly_data <- select(crime_panel,
                      year,
                      textile_value,
                      riots)

## Group data by year.
yearly_data <- group_by(yearly_data, year)

## Create yearly panel of textile investment.
yearly_data <- summarize(yearly_data,
                         textile_value=sum(textile_value, na.rm = TRUE),
                         riots=sum(riots, na.rm=TRUE))

## Ungroup data.
yearly_data <- ungroup(yearly_data)

## Graph Figure 1.
figure_1 <- ggplot(yearly_data, aes(x=year, y=textile_value)) + 
  geom_line(color="black", size=0.5) + 
  geom_vline(xintercept = 2005, lty=2, color="red", size=0.75) + 
  geom_label(aes(x=2004.6, y=1000, label="Expiration of MFA"), size=4) +
  scale_x_continuous(breaks = seq(1999, 2010, by=1)) + 
  scale_y_continuous(breaks = seq(0, 1000, by=200)) + 
  xlab("Year") + 
  ylab("Textile investment (adjusted Rs)") + 
  theme_classic() + 
  theme(axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=14))

figure_1

## Remove yearly data.
rm(yearly_data)


############################################################################
## Figure 2: 2004 Textile Concentrations and Post-2005 Textile Investment ##
############################################################################

## Limit observations to post-liberalization period.
investments_2005 <- filter(investment, year>=2005)

## Group data by state and district.
investments_2005 <- group_by(investments_2005,
                             state_name,
                             district_name,
                             id)

## Create district cross-section of pre-liberalization textile
## concentration and post-liberalization textile investment.
investments_2005 <- summarize(investments_2005,
                              textile_projects=sum(textile_projects, na.rm=TRUE),
                              text_share=mean(text_share, na.rm=TRUE))

## Ungroup data.
investments_2005 <- ungroup(investments_2005)

## Rename some states for merging purposes.

## Delhi
investments_2005$state_name <- if_else(investments_2005$state_name=="Delhi Ut", "NCT of Delhi",
                                       investments_2005$state_name)

## Jammu and Kashmir
investments_2005$state_name <- if_else(investments_2005$state_name=="Jammu & Kashmir", "Jammu and Kashmir",
                                       investments_2005$state_name)

## Orissa
investments_2005$state_name <- if_else(investments_2005$state_name=="Orissa", "Odisha",
                                       investments_2005$state_name)

## Telangana
investments_2005$state_name <- if_else(investments_2005$state_name=="Telangana", "Andhra Pradesh",
                                       investments_2005$state_name)

## Convert district names to factor.
investments_2005 <- mutate(investments_2005,
                           district_name=factor(district_name))

## Rename some districts for merging purposes.
investments_2005 <- mutate(investments_2005,
                           district_name=fct_recode(district_name,
                                                    "North 24 Parganas"="24 Parganas North",
                                                    "South 24 Parganas"="24 Parganas South",
                                                    "Ahmadnagar"="Ahmednagar",
                                                    "Alappuzha"="Alleppey",
                                                    "Anugul"="Angul",
                                                    "Budaun"="Badaun",
                                                    "Baleshwar"="Balasore",
                                                    "Bangalore"="Bangalore Commr.",
                                                    "Bargarh"="Baragarh",
                                                    "Bid"="Beed",
                                                    "Kaimur"="Bhabhua",
                                                    "Bathinda"="Bhatinda",
                                                    "Balangir"="Bolangir",
                                                    "Bulandshahr"="Bulandshahar",
                                                    "Buldana"="Buldhana",
                                                    "Chamrajnagar"="Chamarajnagar",
                                                    "Chandauli"="Chandoli",
                                                    "Chikmagalur"="Chickmagalur",
                                                    "Chitrakoot"="Chitrakoot Dham",
                                                    "Chittaurgarh"="Chittorgarh",
                                                    "Koch Bihar"="Coochbehar",
                                                    "Amethi"="Csm Nagar",
                                                    "Dakshina Kannada"="Dakshin Kannada",
                                                    "Dantewada"="Dantewara",
                                                    "Darjiling"="Darjeeling",
                                                    "Datia"="Datiya",
                                                    "Dhaulpur"="Dholpur",
                                                    "Firozpur"="Ferozpur",
                                                    "Garhchiroli"="Gadchiroli",
                                                    "East Garo Hills"="Garo Hills East",
                                                    "South Garo Hills"="Garo Hills South",
                                                    "West Garo Hills"="Garo Hills West",
                                                    "Gautam Buddha Nagar"="Gautambudh Nagar",
                                                    "Gondiya"="Gondia",
                                                    "Hardwar"="Haridwar",
                                                    "Sabar Kantha"="Himatnagar",
                                                    "Hisar"="Hissar",
                                                    "Hugli"="Hooghly",
                                                    "Haora"="Howrah",
                                                    "Hyderabad"="Hyderabad City",
                                                    "Amroha"="J.p.nagar",
                                                    "Jagatsinghapur"="Jagatsinghpur",
                                                    "Jajapur"="Jajpur",
                                                    "Jalor"="Jalore",
                                                    "Janjgir-Champa"="Janjgir",
                                                    "Jhunjhunun"="Jhunjhunu",
                                                    "East Kameng"="Kameng East",
                                                    "West Kameng"="Kameng West",
                                                    "Kancheepuram"="Kanchipuram",
                                                    "Uttar Bastar Kanker"="Kanker",
                                                    "Kanniyakumari"="Kanyakumari",
                                                    "Kasaragod"="Kasargod",
                                                    "Kabeerdham"="Kawardha",
                                                    "East Khasi Hills"="Khasi Hills East",
                                                    "West Khasi Hills"="Khasi Hills West",
                                                    "Kheda"="Kheda North",
                                                    "Lakhimpur Kheri"="Khiri",
                                                    "Khordha"="Khurda",
                                                    "Kodarma"="Koderma",
                                                    "Kushinagar"="Kushi Nagar",
                                                    "Kachchh"="Kutch",
                                                    "Lahul & Spiti"="Lahaul-Spiti",
                                                    "Lawangtlai"="Lawngtlai",
                                                    "Leh (Ladakh)"="Leh",
                                                    "Lohardaga"="Lohardagga",
                                                    "Mahbubnagar"="Mahaboobnagar",
                                                    "Maldah"="Malda",
                                                    "Malkangiri"="Malkangir",
                                                    "Madhepura"="Medhepura",
                                                    "Mahesana"="Mehsana",
                                                    "Dima Hasao"="N.c.hills",
                                                    "Nagappattinam"="Nagapattinam",
                                                    "Narsimhapur"="Narsinghpur",
                                                    "Nashik"="Nasik",
                                                    "Nawada"="Nawadah",
                                                    "Shahid Bhagat Singh Nagar"="Nawan Shahr",
                                                    "The Nilgiris"="Nilgiris",
                                                    "Nabarangapur"="Nowrangpur",
                                                    "Panch Mahals"="Panchmahal",
                                                    "Pashchim Champaran"="Paschim Champaran",
                                                    "Garhwal"="Pauri Garhwal",
                                                    "Prakasam"="Prakasham",
                                                    "Pudukkottai"="Pudukottai",
                                                    "Purba Champaran"="Purbi Champaran",
                                                    "Purnia"="Purnea",
                                                    "Puruliya"="Purulia",
                                                    "Rae Bareli"="Raibareilly",
                                                    "Ramanathapuram"="Ramnathapuram",
                                                    "Ri Bhoi"="Ri-Bhoi",
                                                    "Rudraprayag"="Rudra Prayag",
                                                    "Sahibganj"="Sahebganj",
                                                    "Sant Kabir Nagar"="Sant Kabirnagar",
                                                    "Surguja"="Sarguja",
                                                    "Sahibzada Ajit Singh Nagar"="Sas Nagar",
                                                    "Shahid Bhagat Singh Nagar"="Sbs Nagar",
                                                    "Shravasti"="Shrawasti",
                                                    "Upper Siang"="Siang Upper",
                                                    "West Siang"="Siang West",
                                                    "Sivasagar"="Sibsagar",
                                                    "Siddharth Nagar"="Sidharthnagar",
                                                    "Sehore"="Sihore",
                                                    "Sivaganga"="Sivagangai",
                                                    "Sonitpur"="Sonepur",
                                                    "Sant Ravi Das Nagar"="St.ravidasnagar",
                                                    "Lower Subansiri"="Subansiri Lower",
                                                    "Upper Subansiri"="Subansiri Upper",
                                                    "Tiruvannamalai"="Thiruvannamalai",
                                                    "Thoothukkudi"="Thoothugudi",
                                                    "Tiruchirappalli"="Tiruchchirappalli",
                                                    "Thiruvananthapuram"="Trivandrum",
                                                    "Udham Singh Nagar"="Udhamsingh Nagar",
                                                    "Umaria"="Umariya",
                                                    "Uttara Kannada"="Uttar Kannada",
                                                    "Viluppuram"="Villupuram",
                                                    "Virudunagar"="Virudhunagar",
                                                    "Wayanad"="Wayanadu"))

## Convert district names to character.
investments_2005 <- mutate(investments_2005, district_name=as.character(district_name))

## Rename additional district names for merging purposes.

## East Sikkim
investments_2005$district_name <- if_else(investments_2005$district_name=="East" & 
                                            investments_2005$state_name=="Sikkim",
                                          "East Sikkim",
                                          investments_2005$district_name)

## North Sikkim
investments_2005$district_name <- if_else(investments_2005$district_name=="North" & 
                                            investments_2005$state_name=="Sikkim",
                                          "North Sikkim",
                                          investments_2005$district_name)

## South Sikkim
investments_2005$district_name <- if_else(investments_2005$district_name=="South" & 
                                            investments_2005$state_name=="Sikkim",
                                          "South Sikkim",
                                          investments_2005$district_name)

## West Sikkim
investments_2005$district_name <- if_else(investments_2005$district_name=="West" & 
                                            investments_2005$state_name=="Sikkim",
                                          "West Sikkim",
                                          investments_2005$district_name)

## North Tripura
investments_2005$district_name <- if_else(investments_2005$district_name=="North" & 
                                            investments_2005$state_name=="Tripura",
                                          "North Tripura",
                                          investments_2005$district_name)

## South Tripura
investments_2005$district_name <- if_else(investments_2005$district_name=="South" & 
                                            investments_2005$state_name=="Tripura",
                                          "South Tripura",
                                          investments_2005$district_name)

## West Tripura
investments_2005$district_name <- if_else(investments_2005$district_name=="West" & 
                                            investments_2005$state_name=="Tripura",
                                          "West Tripura",
                                          investments_2005$district_name)

## Load India district-level shapefile.
## This shapefile was downloaded at https://gadm.org.
india <- readRDS("india_shapefile.rds")

## Remove unnecessary features.
india <- select(india,
                NAME_0,
                NAME_1,
                NAME_2,
                geometry)

## Rename variables for ease of use.
india <- rename(india,
                country=NAME_0,
                state_name=NAME_1,
                district_name=NAME_2)

## Rename some districts for merging purposes.

## Imphal East
india$district_name <- if_else(india$district_name=="Imphal East", "Imphal", india$district_name)

## Imphal West
india$district_name <- if_else(india$district_name=="Imphal West", "Imphal", india$district_name)

## Pashchim Medinipur
india$district_name <- if_else(india$district_name=="Pashchim Medinipur", "Midnapur", india$district_name)

## Purba Medinipur
india$district_name <- if_else(india$district_name=="Purba Medinipur", "Midnapur", india$district_name)

## Mumbai City
india$district_name <- if_else(india$district_name=="Mumbai City", "Mumbai", india$district_name)

## Mumbai Suburban
india$district_name <- if_else(india$district_name=="Mumbai Suburban", "Mumbai", india$district_name)

## Merge shapefile with post-liberalization textile data.
india <- left_join(india, investments_2005)

## Remove Andaman and Nicobar Islands for ease of mapping.
india <- filter(india, state_name!="Andaman and Nicobar")

## Map upper panel of Figure 2.
figure_2_upper <- ggplot(india) + 
  geom_sf(aes(fill=text_share),  size=.1, color="black") +
  scale_fill_gradient(low = "white", high = "black", name="2004 textile\nconcentration") + 
  theme_void()

figure_2_upper

## Map lower panel of Figure 2.
figure_2_lower <- ggplot(india) + 
  geom_sf(aes(fill=textile_projects),  size=.1, color="black") +
  scale_fill_gradient(low = "white", high = "black", name = "Textile investments,\n2005-2010") + 
  theme_void()

figure_2_lower

rm(investments_2005,
   india)


################################################
## Table 1: Liberalization and Rioting Crimes ##
################################################

## Model (1)
table1_model_1 <- fepois(riots ~ 
                           treatedXpost | 
                           id + year,
                         data=crime_panel)

summary(table1_model_1)

## Model (2)
table1_model_2 <- fepois(riots ~ 
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 | 
                           id + year,
                         data=crime_panel)

summary(table1_model_2)

## Model (3)
table1_model_3 <- fepois(riots ~ 
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(table1_model_3)

## Model (4)
table1_model_4 <- feols(log(riots) ~ 
                          treatedXpost | 
                          id + year,
                        data=crime_panel)

summary(table1_model_4)

## Model (5)
table1_model_5 <- feols(log(riots) ~ 
                          treatedXpost + 
                          popX2000 + popX2001 + popX2002 + popX2003 +
                          popX2004 + popX2005 + popX2006 + popX2007 + 
                          popX2008 + popX2009 + popX2010 | 
                          id + year,
                        data=crime_panel)

summary(table1_model_5)


## Model (6)
table1_model_6 <- feols(log(riots) ~ 
                          treatedXpost + 
                          popX2000 + popX2001 + popX2002 + popX2003 +
                          popX2004 + popX2005 + popX2006 + popX2007 + 
                          popX2008 + popX2009 + popX2010 + 
                          workX2000 + workX2001 + workX2002 + workX2003 + 
                          workX2004 + workX2005 + workX2006 + workX2007 + 
                          workX2008 + workX2009 + workX2010 + 
                          litX2000 + litX2001 + litX2002 + litX2003 + 
                          litX2004 + litX2005 + litX2006 + litX2007 + 
                          litX2008 + litX2009 + litX2010 + 
                          scX2000 + scX2001 + scX2002 + scX2003 + 
                          scX2004 + scX2005 + scX2006 + scX2007 + 
                          scX2008 + scX2009 + scX2010 | 
                          id + year,
                        data=crime_panel)

summary(table1_model_6)


######################################
## Figure 3: Event Study Estimation ##
######################################

## Estimate model.
event_study <- fepois(riots ~ 
                        textX1999 + textX2000 + textX2001 + textX2002 +
                        textX2003 + textX2005 + textX2006 + textX2007 + 
                        textX2008 + textX2009 + textX2010 +
                        popX2000 + popX2001 + popX2002 + popX2003 +
                        popX2004 + popX2005 + popX2006 + popX2007 + 
                        popX2008 + popX2009 + popX2010 + 
                        workX2000 + workX2001 + workX2002 + workX2003 + 
                        workX2004 + workX2005 + workX2006 + workX2007 + 
                        workX2008 + workX2009 + workX2010 + 
                        litX2000 + litX2001 + litX2002 + litX2003 + 
                        litX2004 + litX2005 + litX2006 + litX2007 + 
                        litX2008 + litX2009 + litX2010 + 
                        scX2000 + scX2001 + scX2002 + scX2003 + 
                        scX2004 + scX2005 + scX2006 + scX2007 + 
                        scX2008 + scX2009 + scX2010 | 
                        id + year,
                      data=crime_panel)

## Extract model summary.
sum_event_study <- summary(event_study)

## Extract model coefficients and standard errors.
coefs_event_study <- data.frame(sum_event_study$coefficients,
                                sum_event_study$se)

## Create column for variable name.
coefs_event_study <- rownames_to_column(coefs_event_study, "variable")

## Select year-by-year coefficients only.
coefs_event_study <- filter(coefs_event_study, variable %in% c("textX1999",
                                                               "textX2000",
                                                               "textX2001",
                                                               "textX2002",
                                                               "textX2003",
                                                               "textX2005",
                                                               "textX2006",
                                                               "textX2007", 
                                                               "textX2008", 
                                                               "textX2009", 
                                                               "textX2010"))

## Rename variables.
coefs_event_study <- rename(coefs_event_study,
                            coef=sum_event_study.coefficients,
                            se=sum_event_study.se)

## Calculate 95% confidence intervals.
coefs_event_study <- mutate(coefs_event_study,
                            lb_95=coef-1.96*se,
                            ub_95=coef+1.96*se)

## Create year variable.
coefs_event_study <- separate(coefs_event_study, variable, into=c("junk", "year"), sep="X")

coefs_event_study <- select(coefs_event_study, -junk)

coefs_event_study <- mutate(coefs_event_study, year=as.numeric(year))

## Plot year-by-year coefficients.
figure_3 <- ggplot(coefs_event_study, aes(x=year, y=coef)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=lb_95, ymax=ub_95), width=0, size=0.5) + 
  geom_vline(xintercept = 2004, lty=2, color="red", size=0.5) + 
  geom_hline(yintercept = 0, lty=2, color="gray25", size=0.5) + 
  scale_x_continuous(breaks=seq(1999, 2010, by=1)) + 
  xlab("Year") + 
  ylab("Treatment indicator coeffcient") + 
  theme_classic() + 
  theme(axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=14))

figure_3

rm(event_study,
   sum_event_study,
   coefs_event_study)


####################################################
## Table 2: Liberalization and Internal Migration ##
####################################################

## Panel A

## Model (1)
table2_model_a1 <- feols(within_rate ~ 
                           treatedXpost | 
                           id + post,
                         data=migration_total)

summary(table2_model_a1)

## Model (2)
table2_model_a2 <- feols(beyond_rate ~ 
                           treatedXpost | 
                           id + post,
                         data=migration_total)

summary(table2_model_a2)

## Model (3)
table2_model_a3 <- feols(beyond_rate ~ 
                           treatedXpost | 
                           id + post,
                         data=migration_male)

summary(table2_model_a3)

## Model (4)
table2_model_a4 <- feols(beyond_rate ~ 
                           treatedXpost | 
                           id + post,
                         data=migration_female)

summary(table2_model_a4)

## Panel B

## Model (1)
table2_model_b1 <- feols(within_rate ~ 
                           treatedXpost + 
                           log(tot_p):post | 
                           id + post,
                         data=migration_total)

summary(table2_model_b1)

## Model (2)
table2_model_b2 <- feols(beyond_rate ~ 
                           treatedXpost +
                           log(tot_p):post | 
                           id + post,
                         data=migration_total)

summary(table2_model_b2)

## Model (3)
table2_model_b3 <- feols(beyond_rate ~ 
                           treatedXpost +
                           log(tot_p):post | 
                           id + post,
                         data=migration_male)

summary(table2_model_b3)

## Model (4)
table2_model_b4 <- feols(beyond_rate ~ 
                           treatedXpost + 
                           log(tot_p):post | 
                           id + post,
                         data=migration_female)

summary(table2_model_b4)

## Panel C

## Model (1)
table2_model_c1 <- feols(within_rate ~ 
                           treatedXpost + 
                           log(tot_p):post + 
                           percent_work:post + 
                           percent_lit:post + 
                           percent_sc:post | 
                           id + post,
                         data=migration_total)

summary(table2_model_c1)

## Model (2)
table2_model_c2 <- feols(beyond_rate ~ 
                           treatedXpost +
                           log(tot_p):post + 
                           percent_work:post + 
                           percent_lit:post + 
                           percent_sc:post | 
                           id + post,
                         data=migration_total)

summary(table2_model_c2)

## Model (3)
table2_model_c3 <- feols(beyond_rate ~ 
                           treatedXpost +
                           log(tot_p):post + 
                           percent_work:post + 
                           percent_lit:post + 
                           percent_sc:post | 
                           id + post,
                         data=migration_male)

summary(table2_model_c3)

## Model (4)
table2_model_c4 <- feols(beyond_rate ~ 
                           treatedXpost + 
                           log(tot_p):post + 
                           percent_work:post + 
                           percent_lit:post + 
                           percent_sc:post | 
                           id + post,
                         data=migration_female)

summary(table2_model_c4)


###########################################
## Table 3: Support for Nativist Parties ##
###########################################

## Model (1)
table3_model_1 <- feols(mns_vote ~ 
                          treatedXpost | 
                          id + year,
                        data=elections)

summary(table3_model_1)

## Model (2)
table3_model_2 <- feols(mns_vote ~ 
                          treatedXpost + 
                          log(tot_p):year_2009 + log(tot_p):year_2014 | 
                          id + year,
                        data=elections)

summary(table3_model_2)

## Model (3)
table3_model_3 <- feols(mns_vote ~ 
                          treatedXpost + 
                          log(tot_p):year_2009 + log(tot_p):year_2014 +
                          percent_sc:year_2009 + percent_sc:year_2014 +
                          percent_work:year_2009 + percent_work:year_2014 +
                          percent_lit:year_2009 + percent_lit:year_2014 | 
                          id + year,
                        data=elections)

summary(table3_model_3)


##############################################################################
## Appendix Figure A.1: Internal Migration Policy in Lower-Income Countries ##
##############################################################################

figure_a1 <- ggplot(wpp, aes(x=year, y=percent, color=Policy)) + 
  geom_line(size=0.5) + 
  xlab("Year") +
  ylab("Percent") + 
  scale_x_continuous(limits=c(1975, 2015), breaks=seq(1975, 2015, by=5)) +
  theme_classic() + 
  theme(legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

figure_a1


#########################################################
## Appendix Figure A.2: Density Plot of Rioting Crimes ##
#########################################################

figure_a2 <- ggplot(crime_panel, aes(x=riots)) + 
  geom_density(outline.type = "full") +
  xlab("Riots") + 
  scale_x_continuous(breaks=seq(0, 1400, 200)) + 
  ylab("Density") + 
  theme_classic()

figure_a2


#################################################################################
## Appendix Figure A.3: Riot Incidence Over Time by Textile Concentration Size ##
#################################################################################

crime_panel$textile_concentration <- if_else(crime_panel$text_share>mean(crime_panel$text_share), "Above average", "Below average")

trends <- group_by(crime_panel,
                   textile_concentration,
                   year)

trends <- summarize(trends,
                    riots_pop=mean(riots_pop, na.rm=TRUE),
                    riots=mean(riots, na.rm=TRUE))

trends <- ungroup(trends)

trends <- rename(trends, `Textile concentration size`=textile_concentration)

figure_a3 <- ggplot(trends, aes(x=year, y=riots_pop, linetype=`Textile concentration size`)) + 
  geom_line(size=0.5) + 
  geom_vline(xintercept = 2005, lty=2, size=0.8, color="black") + 
  xlab("Year") + 
  ylab("Rioting crimes per 1000 people") + 
  scale_x_continuous(breaks=seq(1999, 2010, by=1)) + 
  theme_classic() + 
  theme(legend.position = "bottom",
        axis.text=element_text(size=10, color="black"),
        axis.title=element_text(size=12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))

figure_a3

rm(trends)


############################################
## Appendix Table A.1: Summary Statistics ##
############################################

## Calculate summary statistics.
tableA1 <- summarize(crime_panel,
                     riots_mean=mean(riots, na.rm=TRUE),
                     riots_sd=sd(riots, na.rm=TRUE),
                     riots_min=min(riots, na.rm=TRUE),
                     riots_max=max(riots, na.rm=TRUE),
                     murder_mean=mean(murders, na.rm=TRUE),
                     murder_sd=sd(murders, na.rm=TRUE),
                     murder_min=min(murders, na.rm=TRUE),
                     murder_max=max(murders, na.rm=TRUE),
                     text_share_mean=mean(text_share, na.rm=TRUE),
                     text_share_sd=sd(text_share, na.rm=TRUE),
                     text_share_min=min(text_share, na.rm=TRUE),
                     text_share_max=max(text_share, na.rm=TRUE),
                     textile_projects_mean=mean(textile_projects, na.rm=TRUE),
                     textile_projects_sd=sd(textile_projects, na.rm=TRUE),
                     textile_projects_min=min(textile_projects, na.rm=TRUE),
                     textile_projects_max=max(textile_projects, na.rm=TRUE),
                     textile_value_mean=mean(textile_value, na.rm=TRUE),
                     textile_value_sd=sd(textile_value, na.rm=TRUE),
                     textile_value_min=min(textile_value, na.rm=TRUE),
                     textile_value_max=max(textile_value, na.rm=TRUE),
                     tot_p_mean=mean(tot_p, na.rm=TRUE),
                     tot_p_sd=sd(tot_p, na.rm=TRUE),
                     tot_p_min=min(tot_p, na.rm=TRUE),
                     tot_p_max=max(tot_p, na.rm=TRUE),
                     percent_lit_mean=mean(percent_lit, na.rm=TRUE),
                     percent_lit_sd=sd(percent_lit, na.rm=TRUE),
                     percent_lit_min=min(percent_lit, na.rm=TRUE),
                     percent_lit_max=max(percent_lit, na.rm=TRUE),
                     percent_work_mean=mean(percent_work, na.rm=TRUE),
                     percent_work_sd=sd(percent_work, na.rm=TRUE),
                     percent_work_min=min(percent_work, na.rm = TRUE),
                     percent_work_max=max(percent_work, na.rm = TRUE),
                     percent_sc_mean=mean(percent_sc, na.rm=TRUE),
                     percent_sc_sd=sd(percent_sc, na.rm = TRUE),
                     percent_sc_min=min(percent_sc, na.rm = TRUE),
                     percent_sc_max=max(percent_sc, na.rm = TRUE))

tableA1


#####################################################################################
## Appendix Table A.2: Liberalization and Rioting Crimes - Alternative Estimations ##
#####################################################################################

## Model (1)
tableA2_model_1 <- fenegbin(riots ~ 
                              treatedXpost | 
                              id + year,
                            data=crime_panel)

summary(tableA2_model_1)

## Model (2)
tableA2_model_2 <- fenegbin(riots ~ 
                              treatedXpost + 
                              popX2000 + popX2001 + popX2002 + popX2003 +
                              popX2004 + popX2005 + popX2006 + popX2007 + 
                              popX2008 + popX2009 + popX2010 | 
                              id + year,
                            data=crime_panel)

summary(tableA2_model_2)

## Model (3)
tableA2_model_3 <- fenegbin(riots ~ 
                              treatedXpost + 
                              popX2000 + popX2001 + popX2002 + popX2003 +
                              popX2004 + popX2005 + popX2006 + popX2007 + 
                              popX2008 + popX2009 + popX2010 + 
                              workX2000 + workX2001 + workX2002 + workX2003 + 
                              workX2004 + workX2005 + workX2006 + workX2007 + 
                              workX2008 + workX2009 + workX2010 + 
                              litX2000 + litX2001 + litX2002 + litX2003 + 
                              litX2004 + litX2005 + litX2006 + litX2007 + 
                              litX2008 + litX2009 + litX2010 + 
                              scX2000 + scX2001 + scX2002 + scX2003 + 
                              scX2004 + scX2005 + scX2006 + scX2007 + 
                              scX2008 + scX2009 + scX2010 | 
                              id + year,
                            data=crime_panel)

summary(tableA2_model_3)

## Model (4)
tableA2_model_4 <- feols(log(riots_pop) ~ 
                           treatedXpost | 
                           id + year,
                         data=crime_panel)

summary(tableA2_model_4)

## Model (5)
tableA2_model_5 <- feols(log(riots_pop) ~ 
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableA2_model_5)

## Model (6)
tableA2_model_6 <- feols(log(riots_pop) ~ 
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableA2_model_6)

## Model (7)
tableA2_model_7 <- feols(log(riots_pop) ~ 
                           treatedXpost + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableA2_model_7)


###############################################
## Appendix Table A.3: Event Study Estimates ##
###############################################

## Model (1)
tableA3_model_1 <- fepois(riots ~ 
                            textX1999 + textX2000 + textX2001 + textX2002 +
                            textX2003 + textX2005 + textX2006 + textX2007 + 
                            textX2008 + textX2009 + textX2010 +
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 + 
                            workX2000 + workX2001 + workX2002 + workX2003 + 
                            workX2004 + workX2005 + workX2006 + workX2007 + 
                            workX2008 + workX2009 + workX2010 + 
                            litX2000 + litX2001 + litX2002 + litX2003 + 
                            litX2004 + litX2005 + litX2006 + litX2007 + 
                            litX2008 + litX2009 + litX2010 + 
                            scX2000 + scX2001 + scX2002 + scX2003 + 
                            scX2004 + scX2005 + scX2006 + scX2007 + 
                            scX2008 + scX2009 + scX2010 | 
                            id + year,
                          data=crime_panel)

summary(tableA3_model_1)

## Model (2)
tableA3_model_2 <- feols(log(1+textile_projects) ~ 
                           textX1999 + textX2000 + textX2001 + textX2002 +
                           textX2003 + textX2005 + textX2006 + textX2007 + 
                           textX2008 + textX2009 + textX2010 +
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableA3_model_2)


#######################################################################
## Appendix Table A.4: Robustness to Heterogeneous Treatment Effects ##
#######################################################################

## The model in this table was estimated separately using Stata/MP 16.1.
## Please see "Helms_AJPS_ReplicationCode_AppendixTableA4.do" to complete this analysis.

###################################################
## Appendix Table A.5: Liberalization and Murder ##
###################################################

## Model (1)
tableA5_model_1 <- fepois(murders ~ 
                            treatedXpost | 
                            id + year,
                          data=crime_panel)

summary(tableA5_model_1)

## Model (2)
tableA5_model_2 <- fepois(murders ~ 
                            treatedXpost + 
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 | 
                            id + year,
                          data=crime_panel)

summary(tableA5_model_2)

## Model (3)
tableA5_model_3 <- fepois(murders ~ 
                            treatedXpost + 
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 + 
                            workX2000 + workX2001 + workX2002 + workX2003 + 
                            workX2004 + workX2005 + workX2006 + workX2007 + 
                            workX2008 + workX2009 + workX2010 + 
                            litX2000 + litX2001 + litX2002 + litX2003 + 
                            litX2004 + litX2005 + litX2006 + litX2007 + 
                            litX2008 + litX2009 + litX2010 + 
                            scX2000 + scX2001 + scX2002 + scX2003 + 
                            scX2004 + scX2005 + scX2006 + scX2007 + 
                            scX2008 + scX2009 + scX2010 | 
                            id + year,
                          data=crime_panel)

summary(tableA5_model_3)

## Model (4)
tableA5_model_4 <- feols(log(murders) ~ 
                           treatedXpost | 
                           id + year,
                         data=crime_panel)

summary(tableA5_model_4)

## Model (5)
tableA5_model_5 <- feols(log(murders) ~ 
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableA5_model_5)

## Model (6)
tableA5_model_6 <- feols(log(murders) ~ 
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableA5_model_6)


#########################################################################
## Appendix Table A.6: Liberalization, Immigration, and Rioting Crimes ##
#########################################################################

## Model (1)
tableA6_model_1 <- fepois(riots ~ 
                            treatedXpostXmigration + 
                            treatedXpost + 
                            treatedXmigration + 
                            postXmigration + 
                            beyond_rate | 
                            id + year,
                          data=crime_panel)

summary(tableA6_model_1)

## Model (2)
tableA6_model_2 <- fepois(riots ~ 
                            treatedXpostXmigration + 
                            treatedXpost + 
                            treatedXmigration + 
                            postXmigration + 
                            beyond_rate +
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 | 
                            id + year,
                          data=crime_panel)

summary(tableA6_model_2)

## Model (3)
tableA6_model_3 <- fepois(riots ~ 
                            treatedXpostXmigration + 
                            treatedXpost + 
                            treatedXmigration + 
                            postXmigration + 
                            beyond_rate + 
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 + 
                            workX2000 + workX2001 + workX2002 + workX2003 + 
                            workX2004 + workX2005 + workX2006 + workX2007 + 
                            workX2008 + workX2009 + workX2010 + 
                            litX2000 + litX2001 + litX2002 + litX2003 + 
                            litX2004 + litX2005 + litX2006 + litX2007 + 
                            litX2008 + litX2009 + litX2010 + 
                            scX2000 + scX2001 + scX2002 + scX2003 + 
                            scX2004 + scX2005 + scX2006 + scX2007 + 
                            scX2008 + scX2009 + scX2010 | 
                            id + year,
                          data=crime_panel)

summary(tableA6_model_3)

## Model (4)
tableA6_model_4 <- feols(log(riots) ~ 
                           treatedXpostXmigration + 
                           treatedXpost + 
                           treatedXmigration + 
                           postXmigration + 
                           beyond_rate | 
                           id + year,
                         data=crime_panel)

summary(tableA6_model_4)

## Model (5)
tableA6_model_5 <- feols(log(riots) ~ 
                           treatedXpostXmigration + 
                           treatedXpost + 
                           treatedXmigration + 
                           postXmigration + 
                           beyond_rate + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableA6_model_5)

## Model (6)
tableA6_model_6 <- feols(log(riots) ~ 
                           treatedXpostXmigration + 
                           treatedXpost + 
                           treatedXmigration + 
                           postXmigration + 
                           beyond_rate + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableA6_model_6)


###########################################################################
## Appendix Table B.1: Liberalization and Capital Investment in Textiles ##
###########################################################################

## Model (1)
tableB1_model_1 <- feols(log(1+textile_value) ~
                           treatedXpost |
                           id + year,
                         data=crime_panel)

summary(tableB1_model_1)

## Model (2)
tableB1_model_2 <- feols(log(1+textile_value) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 |
                           id + year,
                         data=crime_panel)

summary(tableB1_model_2)

## Model (3)
tableB1_model_3 <- feols(log(1+textile_value) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB1_model_3)

## Model (4)
tableB1_model_4 <- feols(log(1+textile_projects) ~
                           treatedXpost |
                           id + year,
                         data=crime_panel)

summary(tableB1_model_4)

## Model (5)
tableB1_model_5 <- feols(log(1+textile_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 |
                           id + year,
                         data=crime_panel)

summary(tableB1_model_5)

## Model (6)
tableB1_model_6 <- feols(log(1+textile_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB1_model_6)


#########################################################################
## Appendix Table B.2: Placebo Tests of Investment in Other Industries ##
#########################################################################

## Model (1)
tableB2_model_1 <- feols(log(1+machinery_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB2_model_1)

## Model (2)
tableB2_model_2 <- feols(log(1+automobiles_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB2_model_2)

## Model (3)
tableB2_model_3 <- feols(log(1+metals_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB2_model_3)

## Model (4)
tableB2_model_4 <- feols(log(1+chemicals_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB2_model_4)

## Model (5)
tableB2_model_5 <- feols(log(1+pharma_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB2_model_5)

## Model (6)
tableB2_model_6 <- feols(log(1+food_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB2_model_6)

## Model (7)
tableB2_model_7 <- feols(log(1+furnleathrub_projects) ~
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=crime_panel)

summary(tableB2_model_7)


####################################################################################################
## Appendix Table C.1: Liberalization and Rioting Crimes - Excluding Low Riot Incidence Districts ##
####################################################################################################

## Model (1)
tableC1_model_1 <- fepois(riots ~ 
                            treatedXpost | 
                            id + year,
                          data=filter(crime_panel, total_riots>quantile(total_riots, .1)))

summary(tableC1_model_1)

## Model (2)
tableC1_model_2 <- fepois(riots ~ 
                            treatedXpost + 
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 | 
                            id + year,
                          data=filter(crime_panel, total_riots>quantile(total_riots, .1)))

summary(tableC1_model_2)

## Model (3)
tableC1_model_3 <- fepois(riots ~ 
                            treatedXpost + 
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 + 
                            workX2000 + workX2001 + workX2002 + workX2003 + 
                            workX2004 + workX2005 + workX2006 + workX2007 + 
                            workX2008 + workX2009 + workX2010 + 
                            litX2000 + litX2001 + litX2002 + litX2003 + 
                            litX2004 + litX2005 + litX2006 + litX2007 + 
                            litX2008 + litX2009 + litX2010 + 
                            scX2000 + scX2001 + scX2002 + scX2003 + 
                            scX2004 + scX2005 + scX2006 + scX2007 + 
                            scX2008 + scX2009 + scX2010 | 
                            id + year,
                          data=filter(crime_panel, total_riots>quantile(total_riots, .1)))

summary(tableC1_model_3)

## Model (4)
tableC1_model_4 <- feols(log(riots) ~ 
                           treatedXpost | 
                           id + year,
                         data=filter(crime_panel, total_riots>quantile(total_riots, .1) & riots>0))

summary(tableC1_model_4)

## Model (5)
tableC1_model_5 <- feols(log(riots) ~ 
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 | 
                           id + year,
                         data=filter(crime_panel, total_riots>quantile(total_riots, .1) & riots>0))

summary(tableC1_model_5)

## Model (6)
tableC1_model_6 <- feols(log(riots) ~ 
                           treatedXpost + 
                           popX2000 + popX2001 + popX2002 + popX2003 +
                           popX2004 + popX2005 + popX2006 + popX2007 + 
                           popX2008 + popX2009 + popX2010 + 
                           workX2000 + workX2001 + workX2002 + workX2003 + 
                           workX2004 + workX2005 + workX2006 + workX2007 + 
                           workX2008 + workX2009 + workX2010 + 
                           litX2000 + litX2001 + litX2002 + litX2003 + 
                           litX2004 + litX2005 + litX2006 + litX2007 + 
                           litX2008 + litX2009 + litX2010 + 
                           scX2000 + scX2001 + scX2002 + scX2003 + 
                           scX2004 + scX2005 + scX2006 + scX2007 + 
                           scX2008 + scX2009 + scX2010 | 
                           id + year,
                         data=filter(crime_panel, total_riots>quantile(total_riots, .1) & riots>0))

summary(tableC1_model_6)


##################################################################
## Appendix Table D.1: Accounting for Ethnolinguistic Diversity ##
##################################################################

tableD1_model_1 <- fepois(riots ~
                            treatedXpostXdifflang +
                            treatedXpost + 
                            treatedXdifflang + 
                            postXdifflang + 
                            prob_difflang |
                            id + year,
                          data=crime_panel)

summary(tableD1_model_1)

tableD1_model_2 <- fepois(riots ~
                            treatedXpostXdifflang +
                            treatedXpost + 
                            treatedXdifflang + 
                            postXdifflang + 
                            prob_difflang +
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 |
                            id + year,
                          data=crime_panel)

summary(tableD1_model_2)

tableD1_model_3 <- fepois(riots ~
                            treatedXpostXdifflang +
                            treatedXpost + 
                            treatedXdifflang + 
                            postXdifflang + 
                            prob_difflang +
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 + 
                            workX2000 + workX2001 + workX2002 + workX2003 + 
                            workX2004 + workX2005 + workX2006 + workX2007 + 
                            workX2008 + workX2009 + workX2010 + 
                            litX2000 + litX2001 + litX2002 + litX2003 + 
                            litX2004 + litX2005 + litX2006 + litX2007 + 
                            litX2008 + litX2009 + litX2010 + 
                            scX2000 + scX2001 + scX2002 + scX2003 + 
                            scX2004 + scX2005 + scX2006 + scX2007 + 
                            scX2008 + scX2009 + scX2010 |
                            id + year,
                          data=crime_panel)

summary(tableD1_model_3)

tableD1_model_4 <- fepois(riots ~ 
                            treatedXpostXdiff + 
                            treatedXpost + 
                            treatedXdiff + 
                            postXdiff +
                            log(prob_difflang_weighted) + 
                            beyond_rate | 
                            id + year,
                          data=crime_panel)

summary(tableD1_model_4)

tableD1_model_5 <- fepois(riots ~ 
                            treatedXpostXdiff + 
                            treatedXpost + 
                            treatedXdiff + 
                            postXdiff +
                            prob_difflang_weighted + 
                            beyond_rate + 
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 | 
                            id + year,
                          data=crime_panel)

summary(tableD1_model_5)

tableD1_model_6 <- fepois(riots ~ 
                            treatedXpostXdiff + 
                            treatedXpost + 
                            treatedXdiff + 
                            postXdiff +
                            prob_difflang_weighted + 
                            beyond_rate + 
                            popX2000 + popX2001 + popX2002 + popX2003 +
                            popX2004 + popX2005 + popX2006 + popX2007 + 
                            popX2008 + popX2009 + popX2010 + 
                            workX2000 + workX2001 + workX2002 + workX2003 + 
                            workX2004 + workX2005 + workX2006 + workX2007 + 
                            workX2008 + workX2009 + workX2010 + 
                            litX2000 + litX2001 + litX2002 + litX2003 + 
                            litX2004 + litX2005 + litX2006 + litX2007 + 
                            litX2008 + litX2009 + litX2010 + 
                            scX2000 + scX2001 + scX2002 + scX2003 + 
                            scX2004 + scX2005 + scX2006 + scX2007 + 
                            scX2008 + scX2009 + scX2010 | 
                            id + year,
                          data=crime_panel)

summary(tableD1_model_6)



