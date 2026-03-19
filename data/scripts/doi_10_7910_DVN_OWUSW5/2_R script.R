#install necessary packages
install.packages(c("tidyverse", "haven", "sjmisc", "sjPlot", "interplot", "lme4",
                   "influence.ME", "easystats", "jtools", "lmerTest", "ggpubr"))

#data loading and wrangling
library(tidyverse) #data wrangling package
library(haven) #load data from STATA files
library(sjmisc) #more data wrangling options

#visualization
library(sjPlot) #regression tables
library(interplot) #plot interaction effects (CME plots)

#MLM computation and diagnostics
library(lme4) #mixed-effects models
library(influence.ME) #outlier statistics
library(easystats) #performance metrics for mixed-effects models

#set seed (to ensure comparability)
set.seed(313)

#load and clean individual-level data from ESS files
ess_dat_raw <- bind_rows(
  ##load STATA files
  read_dta("Data/ESS_main.dta") %>%
    sjlabelled::remove_all_labels() %>%
    subset(select=-c(name, edition, proddate, idno, iscoco, isco08,
                     rlgatnd, rlgdgr, hhmmb, domicil, imbgeco, imueclt, imwbcnt)),
  read_dta("Data/ESS_2_italy.dta") %>%
    sjlabelled::remove_all_labels() %>%
    subset(select=c(essround, cntry, dweight, pspwght, pweight, gincdif, lrscale,
                    health, gndr, agea, edulvla, eduyrs, hinctnt,
                    marital, mbtru, mnactic)),
  read_dta("Data/ESS_3_latvia.dta") %>%
    sjlabelled::remove_all_labels() %>%
    subset(select=c(essround, cntry, dweight, pspwght, pweight, gincdif, lrscale,
                    health, gndr, agea, edulvla, eduyrs, hinctnt,
                    maritala, mbtru, mnactic)),
  read_dta("Data/ESS_3_romania.dta") %>%
    sjlabelled::remove_all_labels() %>%
    subset(select=c(essround, cntry, dweight, pspwght, pweight, gincdif, lrscale,
                    health, gndr, agea, edulvla, eduyrs, hinctnt,
                    maritala, mbtru, mnactic)),
  read_dta("Data/ESS_4_austria.dta") %>%
    sjlabelled::remove_all_labels() %>%
    subset(select=c(essround, cntry, dweight, pspwght, pweight, gincdif, lrscale,
                    health, gndr, agea, edulvla, eduyrs, hinctnta,
                    maritala, mbtru, mnactic)),
  read_dta("Data/ESS_4_lithuania.dta") %>%
    sjlabelled::remove_all_labels() %>%
    subset(select=c(essround, cntry, dweight, pspwght, pweight, gincdif, lrscale,
                    health, gndr, agea, edulvla, eduyrs, hinctnta,
                    maritala, mbtru, mnactic)),
  read_dta("Data/ESS_5_austria.dta") %>%
    sjlabelled::remove_all_labels() %>%
    subset(select=c(essround, cntry, dweight, pspwght, pweight, gincdif, lrscale,
                    health, gndr, agea, edulvlb, eduyrs, hinctnta,
                    maritalb, mbtru, mnactic)),
  read_dta("Data/ESS_9_romania.dta") %>%
    sjlabelled::remove_all_labels() %>%
    subset(select=c(essround, cntry, dweight, pspwght, pweight, gincdif, lrscale,
                    health, gndr, agea, edulvlb, eduyrs, hinctnta,
                    maritalb, mbtru, mnactic))
) %>%
  ##convert ESS missing values to NAs
  sjlabelled::set_na(na=list(
    gincdif = c(7, 8, 9),
    lrscale = c(77, 88, 99),
    health = c(7, 8, 9),
    gndr = 9,
    edulvla = c(55, 77, 88, 99),
    edulvlb = c(5555, 7777, 8888, 9999),
    eduyrs = c(77, 88, 99),
    hinctnt = c(77, 88, 99),
    hinctnta = c(77, 88, 99),
    marital = c(7, 8, 9),
    maritala = c(77, 88, 99),
    maritalb = c(77, 88, 99),
    mbtru = c(7, 8, 9),
    mnactic = c(66, 77, 88, 99),
    agea = 999
    ))

#re-code individual-level variables
ess_dat_adj <- ess_dat_raw %>%
  ##renaming variables
  rename(country = cntry,
         age = agea) %>%
  ##basic re-coding
  mutate(year = sjmisc::rec(essround, rec="1=2002; 2=2004; 3=2006; 4=2008; 5=2010;
                            6=2012; 7=2014; 8=2016; 9=2018"),
         male =  sjmisc::rec(gndr, rec="1=1; 2=0"),
         redist = (gincdif * -1) + 5) %>%
  ##case-when re-coding
  mutate(marital_sts = 
           case_when((marital == 1 & !is.na(marital)) | 
                       (maritala == 1 | maritala == 2 & !is.na(maritala)) | 
                       (maritalb == 1 | maritalb == 2 & !is.na(maritalb)) ~ 1,
                     TRUE & !is.na(marital) | !is.na(maritala) | !is.na(maritalb) ~ 0),
         edulvl = 
           case_when((edulvla == 1 & !is.na(edulvla)) |
                       (edulvlb <= 129 & !is.na(edulvlb)) ~ 1,
                     (edulvla == 2 & !is.na(edulvla)) |
                       (edulvlb >= 212 & edulvlb <= 229 & !is.na(edulvlb)) ~ 2,
                     (edulvla == 3 & !is.na(edulvla)) |
                       (edulvlb >= 311 & edulvlb <= 323 & !is.na(edulvlb)) ~ 3,
                     (edulvla == 4 & !is.na(edulvla)) |
                       (edulvlb >= 412 & edulvlb <= 423 & !is.na(edulvlb)) ~ 4,
                     (edulvla == 5 & !is.na(edulvla)) |
                       (edulvlb >= 510 & !is.na(edulvlb)) ~ 5),
         health =
           case_when(health >= 3 & !is.na(health) ~ 1,
                     TRUE & !is.na(health) ~ 0),
         union_mem =
           case_when(mbtru == 1 & !is.na(mbtru) ~ 1,
                     TRUE & !is.na(mbtru) ~ 0),
         work =
           case_when(mnactic == 1 | mnactic == 7 & !is.na(mnactic) ~ 1,
                     mnactic == 3 | mnactic == 4 & !is.na(mnactic) ~ 2,
                     TRUE & !is.na(mnactic) ~ 0),
         income_1 = 
           sjmisc::rec(hinctnta, rec="1,2=1; 3,4=2; 5,6=3; 7,8=4; 9,10=5")) %>%
  ##removing original variables
  subset(select=-c(essround, gndr, gincdif, marital, maritala, maritalb, 
                   edulvla, edulvlb, mbtru, mnactic))

##adjusted income variable (quantiles)
ess_dat_adj_income <- ess_dat_adj %>%
  group_by(country, year) %>%
  mutate(income_2 = statar::xtile(hinctnt, n=5),
         income = coalesce(income_1, income_2)) %>%
  subset(select=-c(hinctnt, hinctnta, income_1, income_2)) %>%
  ungroup()

#load macro-level data
macrodata <- inner_join(readxl::read_excel("Data/macrodata.xlsx") %>%
                          #divide GDP per capita by 1000 to avoid scale issues
                          mutate(gdp_pc = (gdp_pc / 1000)),
                        readxl::read_excel("Data/immig_dat.xlsx") %>%
                          #impute missing values for int. migrant stock
                          imputeTS::na_interpolation(option="linear"),
                        by=c("country", "year"))

#merge micro- and macro-level data
data_with_NA <- inner_join(ess_dat_adj_income, macrodata, by=c("country", "year"))

#reduce to final sample size (list-wise deletion using linear regression)

##fitting the simple linear regression model
regmodel_na <- lm(redist ~., 
                  data=data_with_NA[c(5:8, 10:16)])

summary(regmodel_na)

##creating a new variable
data_with_NA$used <- T
data_with_NA$used[na.action(regmodel_na)] <- F

##removing the unused rows
data <- data_with_NA %>%
  filter(used == T) %>%
  subset(select=-used)

rm(regmodel_na) #remove object from environment

#how many missings were deleted due to income and pol. orientation?
regmodel_na1 <- lm(lrscale ~ income,
                   data=data_with_NA[c(5, 16)])

summary(regmodel_na1) #127,305 obs deleted due to income and pol. orientation

rm(regmodel_na1, data_with_NA) #remove objects from environment

#average, minimum, and maximum sample size
avg <- data %>%
  group_by(country, year) %>%
  summarize(amnt = n())

min(avg$amnt)
max(avg$amnt)
mean(avg$amnt)

#create MLM variables

##country-year-mean variables
data <- data %>%
  group_by(country, year) %>%
  mutate(redist_C = mean(redist)) %>%
  ungroup()

##country-mean and -demeaned variables
data <- data %>%
  #main variables
  sjmisc::de_mean(redist, income, lrscale, gini_disp, gdp_pc, soc_exp,
                  grp=country, suffix.dm="_we", suffix.gm="_be") %>%
  #additional variables
  sjmisc::de_mean(unemp, left_gov, global_index, econ_open, immig,
                  grp=country, suffix.dm="_we", suffix.gm="_be")

#descriptive statistics of all main variables (Table 1)
datawizard::describe_distribution(data, iqr=F, quartiles=F,
                                  select=c("redist", "income", "lrscale", "age", 
                                           "male", "marital_sts","health", "union_mem", 
                                           "gini_disp_be", "gini_disp_we","gdp_pc_be", 
                                           "gdp_pc_we", "soc_exp_be", "soc_exp_we",
                                           "welfare_reg_5", "welfare_reg_7"))

#Figure 1 (between-country relationship between redistribution preferences and inequality)
figure1 <- ggplot(data=data, aes(x=gini_disp_be, y=redist_be)) +
  geom_smooth(data=data[data$country!="AT",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="BE",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="BG",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="CH",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="CY",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="CZ",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="DE",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="DK",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="EE",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="ES",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="FI",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="FR",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="GB",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="GR",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="HR",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="HU",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="IE",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="IS",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="IT",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="LT",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="LV",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="NL",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="NO",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="PL",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="PT",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="RO",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="SE",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="SI",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country!="SK",], aes(x=gini_disp_be, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(formula=y~x, method="lm", color="#821313", se=F) +
  geom_point(size=1.75, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_text(data=data, aes(label=country), hjust=1.5, vjust=.8, size=2.45, family="Serif") +
  labs(x="Income Inequality", 
       y="Demand for Redistribution") +
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

#Figure 2 (between- and within-country relationship between redistribution preferences and inequality)
figure2 <- ggplot(data=data, aes(x=gini_disp_be, y=redist_be)) +
  geom_smooth(data=data[data$country=="AT",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="BE",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="BG",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="CH",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="CY",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="CZ",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="DE",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="DK",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="EE",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="ES",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="FI",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="FR",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="GB",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="GR",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="HR",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="HU",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="IE",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="IS",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="IT",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="LT",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="LV",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="NL",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="NO",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="PL",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="PT",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="RO",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="SE",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="SI",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(data=data[data$country=="SK",], aes(x=gini_disp, y=redist_C), 
              method="lm", se=F, color="grey", alpha=.35, size=.9, inherit.aes=F) +
  geom_smooth(formula=y~x, method="lm", color="#821313", se=F) +
  geom_point(size=1.75, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_text(data=data, aes(label=country), hjust=1.5, vjust=.8, size=2.45, family="Serif") +
  labs(x="Income Inequality", 
       y="Demand for Redistribution") +
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

ggsave(filename="figure2.jpeg", plot=figure1, 
       width=6, height=4, units="in", dpi=1000)

ggsave(filename="figure3.jpeg", plot=figure2, 
       width=6, height=4, units="in", dpi=1000)

#correlations

##correlation test (pearson's correlation coefficient)
cor.test(data$gini_disp_be, data$redist_be)

##correlation for-loop (jackknife)
countries <- unique(data$country)
cor_list_jack = list()
counter = ""
min = 1
max = -1
min_cntry = ""
max_cntry = ""
min_pval = ""
max_pval = ""

for (i in countries){
  
  counter = i
  cor_list_jack[[counter]] <- cor.test(data[data$country!=i,]$gini_disp_be,
                                       data[data$country!=i,]$redist_be)
  
  if (cor_list_jack[[counter]]$estimate < min){
    min <- cor_list_jack[[counter]]$estimate
    min_cntry <- i
    min_pval <- cor_list_jack[[counter]]$p.value
  }
  
  if (cor_list_jack[[counter]]$estimate > max){
    max <- cor_list_jack[[counter]]$estimate
    max_cntry <- i
    max_pval <- cor_list_jack[[counter]]$p.value
  }
  
  print(paste("All countries except", i))
  print(cor_list_jack[[counter]])
  
}

print(paste("The weakest correlation can be observed when leaving out", min_cntry, 
            "with a coefficient of", min, "which is significant at the", min_pval, "% level"))

print(paste("The strongest correlation can be observed when leaving out", max_cntry, 
            "with a coefficient of", max, "which is significant at the", max_pval, "% level"))

rm(countries, cor_list_jack, counter, min, max, 
   min_cntry, max_cntry, min_pval, max_pval, i) ##remove objects from environment

################################################################################

#MLMs
options(scipen=999) #no scientific notation in results

##grand mean center continuous variables and turn categorical variables into factors
reg_dat <- data %>%
  mutate(across(c(income, lrscale, age), function(x){x - mean(x)})) %>%
  mutate(across(c(year, male, work, edulvl, marital_sts, health, 
                  union_mem, elec_sys), as.factor))

##create group mean centered variables (income & l-r scale)
reg_dat <- reg_dat %>%
  group_by(country) %>%
  mutate(income_cwc = income - mean(income),
         lrscale_cwc = lrscale - mean(lrscale)) %>%
  ungroup()

##create empty/null model (Model M0)
m0 <- lmer(redist ~
             1+(1|country)+(1|country:year), data=reg_dat, REML=T)

##adding individual-level variables and year dummies (Model M1)
m1 <- lmer(redist ~
             income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
             year+(1|country)+(1|country:year),
           data=reg_dat, REML=T)

##adding BE&WE of Gini index (Model M2)
m2 <- lmer(redist~
             income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
             gini_disp_be+gini_disp_we+
             year+(1|country)+(1|country:year),
           data=reg_dat, REML=T)

##adding BE&WE of GDP/capita (Model M3.1)
m3.1 <- lmer(redist ~
               income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               year+(1|country)+(1|country:year),
             data=reg_dat, REML=T)

##adding welfare regime context (welfare dummies + social spending) (Model M3.2)
m3.2 <- lmer(redist ~
               income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               soc_exp_be+soc_exp_we+
               as.factor(welfare_reg_5)+
               year+(1|country)+(1|country:year),
             data=reg_dat, REML=T)

###check whether 7-category welfare regime makes a difference
m3.2.1 <- lmer(redist ~
                 income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
                 gini_disp_be+gini_disp_we+
                 gdp_pc_be+gdp_pc_we+
                 soc_exp_be+soc_exp_we+
                 as.factor(welfare_reg_7)+
                 year+(1|country)+(1|country:year),
               data=reg_dat, REML=T)

###check whether only social expenditure (and no welfare regime dummies) makes a difference
m3.2.2 <- lmer(redist ~
                 income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
                 gini_disp_be+gini_disp_we+
                 gdp_pc_be+gdp_pc_we+
                 soc_exp_be+soc_exp_we+
                 year+(1|country)+(1|country:year),
               data=reg_dat, REML=T)

###regression table with Model M3.2, M3.2.1, and M3.2.2
tab_model(m3.2, m3.2.1, m3.2.2,
          dv.labels=c("M3.2 (5-category WFS)", "M3.2 (7-category WFS)", "M3.2 (social exp.)"),
          string.est="b/SE", p.val="satterthwaite", show.aic=T, p.style="stars", 
          show.se=T, show.ci=F, show.dev=T, collapse.se=T, digits=4, show.icc=F,
          digits.re=4, digits.rsq=4)

##adding random slopes for income and l-r scale (Model M4)
m4 <- lmer(redist ~
             income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
             gini_disp_be+gini_disp_we+
             gdp_pc_be+gdp_pc_we+
             year+(1+income+lrscale|country)+(1+income+lrscale|country:year),
           data=reg_dat, REML=T,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e6),
                               calc.derivs=F))

##adding cross-level interactions (gini*income & gini*l-r scale) (Model M5)
m5 <- lmer(redist ~
             income_cwc+lrscale_cwc+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
             gini_disp_be+gini_disp_we+
             gdp_pc_be+gdp_pc_we+
             (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
             (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
             year+(1+income_cwc+lrscale_cwc|country)+(1+income_cwc+lrscale_cwc|country:year),
           data=reg_dat, REML=T,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=1e6),
                               calc.derivs=F))

#display results in regression table

##main regression table (Table 2 in paper)
tab_model(m0, m1, m2, m3.1, m3.2, m4, m5,
          dv.labels=c("M0", "M1", "M2", "M3.1", "M3.2", "M4", "M5"),
          string.est="b/SE", p.val="satterthwaite", show.aic=T, show.p=T, digits.p=6,
          show.se=T, show.ci=F, show.dev=T, collapse.se=T, digits=6, show.icc=F,
          digits.re=4, digits.rsq=4)

##display variance and covariance components for each model
data.frame(VarCorr(m0))
data.frame(VarCorr(m1))
data.frame(VarCorr(m2))
data.frame(VarCorr(m3.1))
data.frame(VarCorr(m3.2))
data.frame(VarCorr(m4))
data.frame(VarCorr(m5))

##AIC & BIC values
jtools::summ(m0, digits = getOption("jtools-digits", default=2))
jtools::summ(m1, digits = getOption("jtools-digits", default=2))
jtools::summ(m2, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.1, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.2, digits = getOption("jtools-digits", default=2))
jtools::summ(m4, digits = getOption("jtools-digits", default=2))
jtools::summ(m5, digits = getOption("jtools-digits", default=2))

################################################################################

##re-estimating m3.1 with standardized variables (income, lrscale, gini)

###create standardized variables
reg_dat_std <- reg_dat %>%
  datawizard::standardize(select=c("income", "income_cwc", "lrscale", "lrscale_cwc", 
                                   "gini_disp_be", "gini_disp_we"),
                          robust=F, two_sd=T, append="_std")

###models with standardized variables
m3.1_std <- lmer(redist ~
                   income_std+lrscale_std+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
                   gini_disp_be_std+gini_disp_we_std+
                   gdp_pc_be+gdp_pc_we+
                   year+(1|country)+(1|country:year),
                 data=reg_dat_std, REML=T)

###compare effect sizes
tab_model(m3.1, m3.1_std,
          string.est="b/SE", p.val="satterthwaite", show.aic=T, p.style="stars", 
          show.se=T, show.ci=F, show.dev=T, collapse.se=T, digits=4, show.icc=F,
          digits.re=4, digits.rsq=4)

################################################################################

#marginal effects plots (CME plots)

##interaction plot 1: gini be * income cwc (Figure 4)
me_plot1 <- interplot::interplot(m5, var1="income_cwc", var2="gini_disp_be", 
                                  hist=T, ralpha=.3, rfill="#0878FF", adjCI=T, ci=.95) +
  aes(color="#821313") +
  labs(x="Gini index [BE]", y="CME of Household Income") +
  theme(axis.title.x = element_text(size=7),
        axis.title.y = element_text(size=7),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"),
        legend.position="none") +
  geom_hline(yintercept=0, linetype="dashed", size=.3)

##interaction plot 2: gini we * income cwc (Figure 5)
me_plot2 <- interplot::interplot(m5, var1="income_cwc", var2="gini_disp_we", 
                                  hist=T, ralpha=.3, rfill="#0878FF", adjCI=T, ci=.95) +
  aes(color="#821313") +
  labs(x="Gini index [WE]", y="CME of Household Income") +
  theme(axis.title.x = element_text(size=7),
        axis.title.y = element_text(size=7),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"),
        legend.position="none") +
  geom_hline(yintercept=0, linetype="dashed", size=.3)

##interaction plot 3: gini be * l-r scale cwc (Figure 6)
me_plot3 <- interplot::interplot(m5, var1="lrscale_cwc", var2="gini_disp_be", 
                                  hist=T, ralpha=.3, rfill="#0878FF", adjCI=T, ci=.95) +
  aes(color="#821313") +
  labs(x="Gini index [BE]", y="CME of Political Orientation") +
  theme(axis.title.x = element_text(size=7),
        axis.title.y = element_text(size=7),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"),
        legend.position="none") +
  geom_hline(yintercept=0, linetype="dashed", size=.3)

##interaction plot 4: gini we * l-r scale cwc (Figure 7)
me_plot4 <- interplot::interplot(m5, var1="lrscale_cwc", var2="gini_disp_we", 
                                  hist=T, ralpha=.3, rfill="#0878FF", adjCI=T, ci=.95) +
  aes(color="#821313") +
  labs(x="Gini index [WE]", y="CME of Political Orientation") +
  theme(axis.title.x = element_text(size=7),
        axis.title.y = element_text(size=7),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"),
        legend.position="none") +
  geom_hline(yintercept=0, linetype="dashed", size=.3)

figure3 <- ggpubr::ggarrange(me_plot1, me_plot2, me_plot3, me_plot4,
                             labels = c("a", "", "b", ""),
                             ncol=2, nrow=2)

ggsave("figure4.jpeg", figure3, width=6, height=4, units="in", dpi=1000)

################################################################################

#resampling (jackknifing)
countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE",
               "GR", "HU", "IS", "IE", "IT", "LV", "LT", "NL", "NO", "PL", "PT",
               "RO", "SK", "SI", "ES", "SE", "CH", "GB")
reg_list = list()
counter = ""

coefficients_gini_be = data.frame()
standard_errors_gini_be = data.frame()
p_values_gini_be = data.frame()
coefficients_gini_we = data.frame()
standard_errors_gini_we = data.frame()
p_values_gini_we = data.frame()


for (i in countries){
  
  counter = i
  
  #Model M3.1
  reg_list[[counter]] <- 
    summary(lmerTest::lmer(redist ~
                             income+lrscale+age+male+edulvl+
                             work+marital_sts+health+union_mem+
                             gini_disp_be+gini_disp_we+
                             gdp_pc_be+gdp_pc_we+
                             year+(1|country)+(1|country:year),
                           data=reg_dat[reg_dat$country!=i,], REML=T,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl=list(maxfun=1e6),
                                               calc.derivs=F)))[["coefficients"]]
  
  #Gini BE estimates
  coefficients_gini_be <- 
    rbind(coefficients_gini_be, reg_list[[i]]["gini_disp_be","Estimate"])
  
  standard_errors_gini_be <- 
    rbind(standard_errors_gini_be, reg_list[[i]]["gini_disp_be","Std. Error"])
  
  p_values_gini_be <- 
    rbind(p_values_gini_be, reg_list[[i]]["gini_disp_be","Pr(>|t|)"])
  
  #Gini WE estimates
  coefficients_gini_we <- 
    rbind(coefficients_gini_we, reg_list[[i]]["gini_disp_we","Estimate"])
  
  standard_errors_gini_we <- 
    rbind(standard_errors_gini_we, reg_list[[i]]["gini_disp_we","Std. Error"])
  
  p_values_gini_we <- 
    rbind(p_values_gini_we, reg_list[[i]]["gini_disp_we","Pr(>|t|)"])
  
}

jackknifed_vals <- cbind(coefficients_gini_be, standard_errors_gini_be, p_values_gini_be,
                         coefficients_gini_we, standard_errors_gini_we, p_values_gini_we) %>%
  setNames(c("coefficients_gini_be", "standard_errors_gini_be", "p_values_gini_be",
             "coefficients_gini_we", "standard_errors_gini_we", "p_values_gini_we")) %>%
  add_column(country=c("Austria", "Belgium", "Switzerland", "Croatia", "Cyprus", "Czechia",
                       "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
                       "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania",
                       "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia",
                       "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom"), 
             .before="coefficients_gini_be")

rm(coefficients_gini_be, standard_errors_gini_be, p_values_gini_be,
   coefficients_gini_we, standard_errors_gini_we, p_values_gini_we,
   countries, counter, i, reg_list) #remove unnecessary objects from environment

#average values of estimates

##Gini BE
round(mean(jackknifed_vals$coefficients_gini_be),4)
round(mean(jackknifed_vals$standard_errors_gini_be),4)
round(mean(jackknifed_vals$p_values_gini_be),4)

##Gini WE
round(mean(jackknifed_vals$coefficients_gini_we),4)
round(mean(jackknifed_vals$standard_errors_gini_we),4)
round(mean(jackknifed_vals$p_values_gini_we),4)

##p-value of Gini BE of "outliers"
round(mean(jackknifed_vals[c(8,9,13,14,25,29),"p_values_gini_be"]),4)

################################################################################

#LMMs with additional country-level variables

##Model M3.1 with electoral system and vote share of social democrats
m3.3 <- lmer(redist ~
               income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               elec_sys+
               left_gov_be+left_gov_we+
               year+(1|country)+(1|country:year),
             data=reg_dat, REML=T)

##Model M3.1 with BE&WE of unemployment variable
m3.4 <- lmer(redist ~
               income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               unemp_be+unemp_we+
               year+(1|country)+(1|country:year),
             data=reg_dat, REML=T)

##Model M3.1 with BE&WE of immigration variable
m3.5 <- lmer(redist ~
               income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               immig_be+immig_we+
               year+(1|country)+(1|country:year),
             data=reg_dat, REML=T)

##Model M3.1 with BE&WE of globalization index
m3.6 <- lmer(redist ~
               income+lrscale+age+I(age^2)+male+edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               global_index_be+global_index_we+
               year+(1|country)+(1|country:year),
             data=reg_dat, REML=T)

##Table 3 in paper
tab_model(m3.3, m3.4, m3.5, m3.6,
          dv.labels=c("M3.3", "M3.4", "M3.5", "M3.6"),
          string.est="b/SE", p.val="satterthwaite", show.aic=T, p.style="stars", 
          show.se=T, show.ci=F, show.dev=T, collapse.se=T, digits=4, show.icc=F,
          digits.re=4, digits.rsq=4)

##display all variance and covariance components
data.frame(VarCorr(m3.3))
data.frame(VarCorr(m3.4))
data.frame(VarCorr(m3.5))
data.frame(VarCorr(m3.6))

##AIC & BIC values
jtools::summ(m3.3, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.4, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.5, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.6, digits = getOption("jtools-digits", default=2))

################################################################################

#regression diagnostics

##normality of random effects

###model m3.1

####obtain values for plots
qq_plot_vals_m3.1_c <- plot(performance::check_normality(m3.1, effects="random"),
                            type="qq")[[2]][["data"]] %>%
  subset(select=c(x, y))

qq_plot_vals_m3.1_cy <- plot(performance::check_normality(m3.1, effects="random"),
                             type="qq")[[1]][["data"]] %>%
  subset(select=c(x, y))

####create country plot
qq_plot_m3.1_country <- 
  ggplot(data=qq_plot_vals_m3.1_c, aes(x=x, y=y)) +
  geom_point(size=1, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_smooth(method="lm", se=F, color="#821313", size=.5) +
  labs(x="Theoretical Quantiles", y="Sample Quantiles") +
  theme(axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        axis.text = element_text(size=8),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

####create country-year plot
qq_plot_m3.1_country_year <- 
  ggplot(data=qq_plot_vals_m3.1_cy, aes(x=x, y=y)) +
  geom_point(size=1, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_smooth(method="lm", se=F, color="#821313", size=.5) +
  labs(x="Theoretical Quantiles", y="Sample Quantiles") +
  theme(axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        axis.text = element_text(size=8),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

####append plots
qq_plot1 <- ggpubr::ggarrange(qq_plot_m3.1_country, qq_plot_m3.1_country_year,
                              ncol=2, labels=c("a", "b"))

#save plots
ggsave(filename="figure5.jpeg", plot=qq_plot1, 
       width=6, height=4, units="in", dpi=1000)

###model m5

####obtain values for plots
qq_plot_vals_m5_c <- plot(performance::check_normality(m5, effects="random"),
                          type="qq")[[2]][["data"]] %>%
  subset(select=c(x, y))

qq_plot_vals_m5_cy <- plot(performance::check_normality(m5, effects="random"),
                           type="qq")[[1]][["data"]] %>%
  subset(select=c(x, y))

####create country plots
qq_plot_m5_c_intercept <- ggplot(data=qq_plot_vals_m5_c[c(1:29),], aes(x=x, y=y)) +
  geom_point(size=.5, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_smooth(method="lm", se=F, size=.5, color="#821313") +
  labs(x="Theoretical Quantiles", y="Sample Quantiles") +
  theme(axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

qq_plot_m5_c_income <- ggplot(data=qq_plot_vals_m5_c[c(30:58),], aes(x=x, y=y)) +
  geom_point(size=.5, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_smooth(method="lm", se=F, size=.5, color="#821313") +
  labs(x="Theoretical Quantiles", y="Sample Quantiles") +
  theme(axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

qq_plot_m5_c_lrscale <- ggplot(data=qq_plot_vals_m5_c[c(59:87),], aes(x=x, y=y)) +
  geom_point(size=.5, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_smooth(method="lm", se=F, size=.5, color="#821313") +
  labs(x="Theoretical Quantiles", y="Sample Quantiles") +
  theme(axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

####create country-year plots
qq_plot_m5_cy_intercept <- ggplot(data=qq_plot_vals_m5_cy[c(1:195),], aes(x=x, y=y)) +
  geom_point(size=.5, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_smooth(method="lm", se=F, size=.5, color="#821313") +
  labs(x="Theoretical Quantiles", y="Sample Quantiles") +
  theme(axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

qq_plot_m5_cy_income <- ggplot(data=qq_plot_vals_m5_cy[c(196:390),], aes(x=x, y=y)) +
  geom_point(size=.5, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_smooth(method="lm", se=F, size=.5, color="#821313") +
  labs(x="Theoretical Quantiles", y="Sample Quantiles") +
  theme(axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

qq_plot_m5_cy_lrscale <- ggplot(data=qq_plot_vals_m5_cy[c(391:585),], aes(x=x, y=y)) +
  geom_point(size=.5, fill="#0878FF", color="#08136F", shape=21, stroke=1) +
  geom_smooth(method="lm", se=F, size=.5, color="#821313") +
  labs(x="Theoretical Quantiles", y="Sample Quantiles") +
  theme(axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text = element_text(size=6),
        plot.background = element_blank(),
        panel.background = element_rect(fill="white", colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family="Serif", face="bold"))

####append plots
qq_plot2 <- ggpubr::ggarrange(qq_plot_m5_c_intercept, qq_plot_m5_c_income, qq_plot_m5_c_lrscale,
                              qq_plot_m5_cy_intercept, qq_plot_m5_cy_income, qq_plot_m5_cy_lrscale,
                              labels = c("a", "", "", "b", "", ""),
                              ncol=3, nrow=2)

####save plots
ggsave(filename="figure6.jpeg", plot=qq_plot2, 
       width=6, height=4, units="in", dpi=1000)

################################################################################

##outlier statistics

###compute influence objects
influence_obj_m3.1 <- 
  influence.ME::influence(m3.1, group="country") #note: takes around 30 min to compute

influence_obj_m5 <- 
  influence.ME::influence(m5, group="country") #note: takes 7+ hrs to compute

###Cook's d
influence.ME::cooks.distance.estex(influence_obj_m3.1)

influence.ME::cooks.distance.estex(influence_obj_m5)

###DFBETAs

####model m3.1
dfbetas_giniWE_m3.1 <- as.data.frame(dfbetas(influence_obj_m3.1)) %>%
  subset(select="gini_disp_we")

dfbetas_giniWE_m3.1

####model m5
dfbetas_giniWE_m5 <- as.data.frame(dfbetas(influence_obj_m5)) %>%
  subset(select="gini_disp_we")

dfbetas_giniWE_m5

################################################################################

#Appendix

##country- and country-year-level sample size (Table A1)
addmargins(table(data$country, data$year))

##correlation matrix, pearson (Table A4)
corr_mat <- 
  round(cor(data %>%
              subset(select=c("redist", "income", "lrscale", "work", "edulvl",
                              "age", "male", "marital_sts", "health", "union_mem",
                              "gini_disp_be", "gini_disp_we", "gdp_pc_be", "gdp_pc_we",
                              "soc_exp_be", "soc_exp_we", "welfare_reg_5"))), 6)

corr_mat[upper.tri(corr_mat)] <- ""
corr_mat <- as.data.frame(corr_mat)
corr_mat

##descriptive statistics of all additional country-level variables (Table A5)
datawizard::describe_distribution(data, iqr=F, quartiles=F,
                                  select=c("elec_sys", "left_gov_be", "left_gov_we", 
                                           "unemp_be", "unemp_we", "immig_be", "immig_we", 
                                           "global_index_be", "global_index_we"))

##re-estimating model m3.1 without outliers
m3.1.1 <- lmer(redist ~
                 income_cwc+lrscale_cwc+age+I(age^2)+male+
                 edulvl+work+marital_sts+health+union_mem+
                 gini_disp_be+gini_disp_we+
                 gdp_pc_be+gdp_pc_we+
                 year+(1|country)+(1|country:year),
               data=reg_dat[reg_dat$country!="BG",], REML=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=1e6),
                                   calc.derivs=F))

m3.1.2 <- lmer(redist ~
                 income_cwc+lrscale_cwc+age+I(age^2)+male+
                 edulvl+work+marital_sts+health+union_mem+
                 gini_disp_be+gini_disp_we+
                 gdp_pc_be+gdp_pc_we+
                 year+(1|country)+(1|country:year),
               data=reg_dat[reg_dat$country!="DE",], REML=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=1e6),
                                   calc.derivs=F))

m3.1.3 <- lmer(redist ~
                 income_cwc+lrscale_cwc+age+I(age^2)+male+
                 edulvl+work+marital_sts+health+union_mem+
                 gini_disp_be+gini_disp_we+
                 gdp_pc_be+gdp_pc_we+
                 year+(1|country)+(1|country:year),
               data=reg_dat[reg_dat$country!="GB",], REML=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=1e6),
                                   calc.derivs=F))

m3.1.4 <- lmer(redist ~
                 income_cwc+lrscale_cwc+age+I(age^2)+male+
                 edulvl+work+marital_sts+health+union_mem+
                 gini_disp_be+gini_disp_we+
                 gdp_pc_be+gdp_pc_we+
                 year+(1|country)+(1|country:year),
               data=reg_dat[reg_dat$country!="NO",], REML=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=1e6),
                                   calc.derivs=F))

m3.1.5 <- lmer(redist ~
                 income_cwc+lrscale_cwc+age+I(age^2)+male+
                 edulvl+work+marital_sts+health+union_mem+
                 gini_disp_be+gini_disp_we+
                 gdp_pc_be+gdp_pc_we+
                 year+(1|country)+(1|country:year),
               data=reg_dat[reg_dat$country!="BG" & 
                              reg_dat$country!="DE" & 
                              reg_dat$country!="GB" & 
                              reg_dat$country!="NO",], REML=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=1e6),
                                   calc.derivs=F))

##Table A6 in paper
tab_model(m3.1.1, m3.1.2, m3.1.3, m3.1.4, m3.1.5,
          dv.labels=c("M3.1.1", "M3.1.2", "M3.1.3", "M3.1.4", "M3.1.5"),
          string.est="b/SE", p.val="satterthwaite", show.aic=T, p.style="stars", 
          show.se=T, show.ci=F, show.dev=T, collapse.se=T, digits=4, show.icc=F,
          digits.re=4, digits.rsq=4)

##display all variance and covariance components
data.frame(VarCorr(m3.1.1))
data.frame(VarCorr(m3.1.2))
data.frame(VarCorr(m3.1.3))
data.frame(VarCorr(m3.1.4))
data.frame(VarCorr(m3.1.5))

##AIC & BIC values
jtools::summ(m3.1.1, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.1.2, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.1.3, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.1.4, digits = getOption("jtools-digits", default=2))
jtools::summ(m3.1.5, digits = getOption("jtools-digits", default=2))

#re-estimating model m5 without outliers
m5.1 <- lmer(redist ~
               income_cwc+lrscale_cwc+age+I(age^2)+male+
               edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
               (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
               year+(1+income_cwc+lrscale_cwc|country)+
               (1+income_cwc+lrscale_cwc|country:year),
             data=reg_dat[reg_dat$country!="DE",], REML=T,
             control=lmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=1e6),
                                 calc.derivs=F))

m5.2 <- lmer(redist ~
               income_cwc+lrscale_cwc+age+I(age^2)+male+
               edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
               (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
               year+(1+income_cwc+lrscale_cwc|country)+
               (1+income_cwc+lrscale_cwc|country:year),
             data=reg_dat[reg_dat$country!="GB",], REML=T,
             control=lmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=1e6),
                                 calc.derivs=F))

m5.3 <- lmer(redist ~
               income_cwc+lrscale_cwc+age+I(age^2)+male+
               edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
               (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
               year+(1+income_cwc+lrscale_cwc|country)+
               (1+income_cwc+lrscale_cwc|country:year),
             data=reg_dat[reg_dat$country!="NO",], REML=T,
             control=lmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=1e6),
                                 calc.derivs=F))

m5.4 <- lmer(redist ~
               income_cwc+lrscale_cwc+age+I(age^2)+male+
               edulvl+work+marital_sts+health+union_mem+
               gini_disp_be+gini_disp_we+
               gdp_pc_be+gdp_pc_we+
               (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
               (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
               year+(1+income_cwc+lrscale_cwc|country)+
               (1+income_cwc+lrscale_cwc|country:year),
             data=reg_dat[reg_dat$country!="DE" & 
                            reg_dat$country!="GB" & 
                            reg_dat$country!="NO",], REML=T,
             control=lmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=1e6),
                                 calc.derivs=F))

##Table A7 in paper
tab_model(m5.1, m5.2, m5.3, m5.4,
          dv.labels=c("M5.1", "M5.2", "M5.3", "M5.4"),
          string.est="b/SE", p.val="satterthwaite", show.aic=T, p.style="stars", 
          show.se=T, show.ci=F, show.dev=T, collapse.se=T, digits=4, show.icc=F,
          digits.re=4, digits.rsq=4)

##display all variance and covariance components
data.frame(VarCorr(m5.1))
data.frame(VarCorr(m5.2))
data.frame(VarCorr(m5.3))
data.frame(VarCorr(m5.4))

##AIC & BIC values
jtools::summ(m5.1, digits = getOption("jtools-digits", default=2))
jtools::summ(m5.2, digits = getOption("jtools-digits", default=2))
jtools::summ(m5.3, digits = getOption("jtools-digits", default=2))
jtools::summ(m5.4, digits = getOption("jtools-digits", default=2))

################################################################################

#obtaining estimates from M3.1.1-M3.1.5
m3.1.1_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         year+(1|country)+(1|country:year),
                                       data=reg_dat[reg_dat$country!="BG",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

m3.1.2_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         year+(1|country)+(1|country:year),
                                       data=reg_dat[reg_dat$country!="DE",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

m3.1.3_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         year+(1|country)+(1|country:year),
                                       data=reg_dat[reg_dat$country!="GB",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

m3.1.4_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         year+(1|country)+(1|country:year),
                                       data=reg_dat[reg_dat$country!="NO",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

m3.1.5_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         year+(1|country)+(1|country:year),
                                       data=reg_dat[reg_dat$country!="BG" & 
                                                      reg_dat$country!="DE" & 
                                                      reg_dat$country!="GB" & 
                                                      reg_dat$country!="NO",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

#mean estimates of M3.1.1-M3.1.5

##mean beta coefficient of Gini BE
round(mean(c(m3.1.1_est["gini_disp_be","Estimate"], m3.1.2_est["gini_disp_be","Estimate"],
             m3.1.3_est["gini_disp_be","Estimate"], m3.1.4_est["gini_disp_be","Estimate"],
             m3.1.5_est["gini_disp_be","Estimate"])),4)

##mean p-value of Gini BE
round(mean(c(m3.1.1_est["gini_disp_be","Pr(>|t|)"], m3.1.2_est["gini_disp_be","Pr(>|t|)"],
             m3.1.3_est["gini_disp_be","Pr(>|t|)"], m3.1.4_est["gini_disp_be","Pr(>|t|)"],
             m3.1.5_est["gini_disp_be","Pr(>|t|)"])),4)

##mean beta coefficient of Gini WE
round(mean(c(m3.1.1_est["gini_disp_we","Estimate"], m3.1.2_est["gini_disp_we","Estimate"],
             m3.1.3_est["gini_disp_we","Estimate"], m3.1.4_est["gini_disp_we","Estimate"],
             m3.1.5_est["gini_disp_we","Estimate"])),4)
##mean p-value of Gini WE
round(mean(c(m3.1.1_est["gini_disp_we","Pr(>|t|)"], m3.1.2_est["gini_disp_we","Pr(>|t|)"],
             m3.1.3_est["gini_disp_we","Pr(>|t|)"], m3.1.4_est["gini_disp_we","Pr(>|t|)"],
             m3.1.5_est["gini_disp_we","Pr(>|t|)"])),4)

################################################################################

#obtaining estimates from M5.1-M5.4
m5.1_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
                                         (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
                                         year+(1+income_cwc+lrscale_cwc|country)+
                                         (1+income_cwc+lrscale_cwc|country:year),
                                       data=reg_dat[reg_dat$country!="DE",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

m5.2_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
                                         (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
                                         year+(1+income_cwc+lrscale_cwc|country)+
                                         (1+income_cwc+lrscale_cwc|country:year),
                                       data=reg_dat[reg_dat$country!="GB",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

m5.3_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
                                         (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
                                         year+(1+income_cwc+lrscale_cwc|country)+
                                         (1+income_cwc+lrscale_cwc|country:year),
                                       data=reg_dat[reg_dat$country!="NO",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

m5.4_est <- 
  as.data.frame(summary(lmerTest::lmer(redist ~
                                         income_cwc+lrscale_cwc+age+I(age^2)+male+
                                         edulvl+work+marital_sts+health+union_mem+
                                         gini_disp_be+gini_disp_we+
                                         gdp_pc_be+gdp_pc_we+
                                         (gini_disp_be*income_cwc)+(gini_disp_we*income_cwc)+
                                         (gini_disp_be*lrscale_cwc)+(gini_disp_we*lrscale_cwc)+
                                         year+(1+income_cwc+lrscale_cwc|country)+
                                         (1+income_cwc+lrscale_cwc|country:year),
                                       data=reg_dat[reg_dat$country!="DE" & 
                                                      reg_dat$country!="GB" & 
                                                      reg_dat$country!="NO",], REML=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1e6),
                                                           calc.derivs=F)))[["coefficients"]])

#mean estimates of M5.1-M5.4

##mean beta coefficient of Gini BE
round(mean(c(m5.1_est["gini_disp_be","Estimate"], m5.2_est["gini_disp_be","Estimate"],
             m5.3_est["gini_disp_be","Estimate"], m5.4_est["gini_disp_be","Estimate"])),4)

##mean p-value of Gini BE
round(mean(c(m5.1_est["gini_disp_be","Pr(>|t|)"], m5.2_est["gini_disp_be","Pr(>|t|)"],
             m5.3_est["gini_disp_be","Pr(>|t|)"], m5.4_est["gini_disp_be","Pr(>|t|)"])),4)

##mean beta coefficient of Gini WE
round(mean(c(m5.1_est["gini_disp_we","Estimate"], m5.2_est["gini_disp_we","Estimate"],
             m5.3_est["gini_disp_we","Estimate"], m5.4_est["gini_disp_we","Estimate"])),4)

##mean p-value of Gini WE
round(mean(c(m5.1_est["gini_disp_we","Pr(>|t|)"], m5.2_est["gini_disp_we","Pr(>|t|)"],
             m5.3_est["gini_disp_we","Pr(>|t|)"], m5.4_est["gini_disp_we","Pr(>|t|)"])),4)







