################### KMS STATISTICS - 2013 ####################

ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyr","dplyr","ggplot2","ggpubr")

ipak(packages)

rm(list = ls())
setwd() # set working directory

mig <- read.csv("kms_emigrant_rep.csv")


#### Figure 1b: Migrant Destinations ####
mig$CurrentResidenceCode %>% table(useNA = "always") # All residing outside of India
mig$CountryStateCode %>% table(useNA = "always")
mig = mig %>%
  mutate(
    CountryStateCode = as.numeric(CountryStateCode),
    country_name = case_when(
      CountryStateCode==1 ~ "United Arab Emirates",
      CountryStateCode==2 ~ "Saudi Arabia",
      CountryStateCode==3 ~ "Oman",
      CountryStateCode==4 ~ "Qatar",
      CountryStateCode==5 ~ "Kuwait",
      CountryStateCode==6 ~ "Bahrain",
      CountryStateCode==7 ~ "Australia",
      CountryStateCode==8 ~ "Bangladesh",
      CountryStateCode==9 ~ "Canada",
      CountryStateCode==10 ~ "China",
      CountryStateCode==11 ~ "France",
      CountryStateCode==12 ~ "Germany",
      CountryStateCode==13 ~ "Hong Kong",
      CountryStateCode==14 ~ "Hungary",
      CountryStateCode==15 ~ "Indonesia",
      CountryStateCode==16 ~ "Iran",
      CountryStateCode==17 ~ "Iraq",
      CountryStateCode==18 ~ "Ireland",
      CountryStateCode==19 ~ "Italy",
      CountryStateCode==20 ~ "Japan",
      CountryStateCode==21 ~ "Kenya",
      CountryStateCode==22 ~ "Libya",
      CountryStateCode==23 ~ "Malaysia",
      CountryStateCode==24 ~ "Maldives",
      CountryStateCode==25 ~ "Myanmar",
      CountryStateCode==26 ~ "Nepal",
      CountryStateCode==27 ~ "Netherlands",
      CountryStateCode==28 ~ "New Zealand",
      CountryStateCode==29 ~ "Nigeria",
      CountryStateCode==30 ~ "Pakistan",
      CountryStateCode==31 ~ "Philippines",
      CountryStateCode==32 ~ "Singapore",
      CountryStateCode==33 ~ "South Africa",
      CountryStateCode==34 ~ "Sri Lanka",
      CountryStateCode==35 ~ "Switzerland",
      CountryStateCode==36 ~ "Taiwan",
      CountryStateCode==37 ~ "Thailand",
      CountryStateCode==38 ~ "United Kingdom",
      CountryStateCode==39 ~ "United States of America",
      CountryStateCode==40 ~ "West India",
      CountryStateCode==41 ~ "Yemen", 
      CountryStateCode==42 ~ "Other"))

pdf("Figures/Figure_1B.pdf", width = 7, height = 6)
gcc = c("United Arab Emirates", "Saudi Arabia", "Kuwait", "Qatar", 
        "Bahrain", "Oman")
library(ggpubr)
mig %>%
  group_by(country_name) %>%
  add_tally(name = "mig_per_country") %>%
  summarise_at(vars(mig_per_country), .funs = mean, na.rm = T) %>%
  mutate(percent = mig_per_country/4072) %>%
  mutate(gcc = ifelse(country_name %in% gcc, 1, 0)) %>%
  ggplot(., aes(x = reorder(country_name, -percent), y = percent))+
  geom_bar(stat = "identity", aes(fill = as.factor(gcc)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(name = "GCC Country", values = c("gray50", "black"))+
  theme_pubr()+
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  labs(x = "Country of Migration", y = "% Migrants from Kerala (2013)")
dev.off()
