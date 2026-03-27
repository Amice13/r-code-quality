# Install required packages 
# install.packages("remotes")
# library(remotes)
# install_version("dplyr", version = "1.1.4")
# install_version("stargazer", version = "5.2.3")
# install_version("readstata13", version = "0.10.1");install_version("haven", version = "2.5.4")
# install_version("foreign", version = "0.8-86");install_version("sjmisc", version = "2.8.9")
# install_version("snakecase", version = "0.11.1");install_version("ggplot2", version = "3.4.4");
# install_version("reshape2", version = "3.4.4");install_version("scales", version = "1.3.0")

# Load required packages
library(dplyr)
library(stargazer)
library(haven)
library(foreign)
library(sjmisc)
library(snakecase)
library(ggplot2)
library(reshape2)
library(scales)

# List all objects in the environment
all_objects <- ls()

# Specify the objects to keep
objects_to_keep <- c("lib", 'wd')

# Identify objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)

# Set working directory

setwd(wd)

tab.out = "output/tables/"
fig.out = "output/figures/"


vp = read.csv("vpsurvey.csv")

vp = vp %>% 
  to_dummy(vp_caste, vp_religion, vp_education, suffix = "label") %>% 
  bind_cols(vp) %>% 
  dplyr::select(vp_caste_SC, vp_caste_ST, vp_religion_Christian, vp_religion_Hindu, vp_religion_Muslim,
                vp_religion_Sikh, vp_gender, `vp_education_No formal education (cannot read or write)`) %>% 
  mutate(VP=1) %>%
  rename(caste_st=vp_caste_ST, caste_sc=vp_caste_SC, 
         religion_christian=vp_religion_Christian, religion_hindu=vp_religion_Hindu, 
         religion_muslim=vp_religion_Muslim, religion_sikh=vp_religion_Sikh,
         gender=vp_gender, literate=`vp_education_No formal education (cannot read or write)`) %>%
  filter(gender<2) %>% mutate(literate = ifelse(literate==0, 1, 0)) %>% as_tibble()

mc = read.csv("baseline_observations.csv") %>% as_tibble()
mc=mc %>%
  dplyr::select(gender, caste, religion, education, education_others, literate) %>%
  mutate(literate_fin = ifelse(is.na(education)&is.na(education_others)&is.na(literate), 0, 1),
         literate_fin = ifelse(literate == 0, 0, literate_fin)) %>% 
  dplyr::select(-literate,-education, -education_others) %>% rename(literate=literate_fin)
mc = mc %>% 
  mutate(caste = dplyr::recode(caste, SC = "sc")) %>%
  to_dummy(caste, religion, suffix = "label") %>% 
  bind_cols(mc) %>% 
  mutate(VP=0) %>%
  dplyr::select(caste_sc, caste_st, religion_christian, religion_hindu,
                religion_muslim, gender, VP, literate) %>% 
  mutate(religion_sikh = 0) %>%
  filter(gender<2) %>% as_tibble()

census <- read.csv("census_2011.csv") %>% as_tibble
gender = ((sum(census$M_POP) / sum(census$TOT_POP))) %>% as.numeric %>% unname()
caste_sc=(sum(census$TOT_SC) / sum(census$TOT_POP))
caste_st=(sum(census$TOT_ST) / sum(census$TOT_POP))
literate=(sum(census$TOT_LIT) / sum(census$TOT_POP))
#data from https://www.census2011.co.in/data/religion/state/20-jharkhand.html :
religion_christian = 0.043; religion_hindu = 0.6783
religion_muslim = 0.1453; religion_sikh =0.022
VP=2 #arbitrary coding for population
population = cbind(caste_sc, caste_st, religion_christian, religion_hindu,
                   religion_muslim, religion_sikh, gender, VP, literate) %>% as_tibble()

d=rbind(vp, mc, population) %>% 
  as_tibble %>%
  group_by(VP) %>%
  dplyr::summarise_at(vars(1:8), mean, na.rm=TRUE) %>%
  mutate(population = ifelse(VP==0, "SQ Recruits", ifelse(VP==1, "Mid-level", "Jharkhand"))) %>%
  dplyr::select(-VP) %>%
  rename(Male=gender, SC=caste_sc, ST=caste_st, Christian=religion_christian,
         Hindu=religion_hindu, Muslim=religion_muslim, Sikh=religion_sikh, 
         Literate=literate)

#Standard errors
d1=rbind(vp, mc, population) %>% 
  as_tibble %>%
  group_by(VP) %>%
  mutate(se_sc = sd(caste_sc) / sqrt(n()),
         se_st = sd(caste_st) / sqrt(n()),
         se_christian = sd(religion_christian) / sqrt(n()),
         se_hindu = sd(religion_hindu) / sqrt(n()),
         se_muslim = sd(religion_muslim) / sqrt(n()),
         se_sikh = sd(religion_sikh) / sqrt(n()),
         se_gender = sd(gender) / sqrt(n()), 
         se_literate=sd(literate)/ sqrt(n()))
se = c(names(table(d1$se_sc))[[1]], names(table(d1$se_sc))[[2]], 0,
       names(table(d1$se_st))[[1]], names(table(d1$se_st))[[2]], 0,
       names(table(d1$se_christian))[[1]], names(table(d1$se_christian))[[2]], 0,
       names(table(d1$se_hindu))[[1]], names(table(d1$se_hindu))[[2]], 0,
       names(table(d1$se_muslim))[[1]], names(table(d1$se_muslim))[[2]], 0,
       names(table(d1$se_sikh))[[1]], names(table(d1$se_sikh))[[2]], 0,
       names(table(d1$se_gender))[[1]], names(table(d1$se_gender))[[2]], 0,
       names(table(d1$se_literate))[[1]],  names(table(d1$se_literate))[[2]], 0)
d.m <- melt(d, id.vars='population')  %>%
  mutate_at(vars(population), as.factor) %>%
  cbind(se) %>% mutate(se = as.numeric(as.character(se)), 
                       cilo = value-1.96*as.numeric(se), cihi = value+1.96*as.numeric(se)) %>%
  mutate(cilo = ifelse(population=="Jharkhand", NA,  cilo)) %>% #so that there aren't error bars here
  mutate(cihi = ifelse(population=="Jharkhand", NA,  cihi))
d.m$population <- factor(d.m$population, 
                         levels = c("Jharkhand", "Mid-level", "SQ Recruits"))

descriptiveplot=ggplot(data=d.m, aes(x=variable, y=value, fill=population)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=cilo, ymax=cihi), 
                position=position_dodge(width = 0.9), width=0.2, color="gray10") +
  labs(y="", x="", fill="") + theme_light(base_size=15) + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  scale_fill_manual(values=c("lightsteelblue1","steelblue2","steelblue4"))+ 
  scale_y_continuous(labels=percent)
ggsave(descriptiveplot, file = paste0(fig.out, "figureA3_descriptiveplot_wcontrol.pdf"))



