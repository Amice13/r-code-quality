#The following data preperation was carried out using R version 4.3.0 
require(tidyverse) #tidyverse version 2.0.0
require(MatchIt) #MatchIt version 4.5.4
require(cobalt) #cobalt version 4.5.1
require(did) #did version 2.1.2       
require(rmeta) #rmeta version 3.0
require(xtable) #xtable version 1.8-4

#Du mangler at rename og gøre tilpas store
rm(list = ls())
setwd("C:/Users/au456026/OneDrive - Aarhus Universitet/Part B - Policies with adverse local effects/Version 6 - april 2022 (fourth submission)/Replication material")
source("helpers.R")

#Data prep####
##INC####
incdata_le_schp <- read_csv2("incdata_schp_le.csv") %>% 
  bind_cols(NA) #need to be same width as others for functions
incdata_le_schd1 <- read_csv2("incdata_schd1_le.csv") 
incdata_le_schd2 <- read_csv2("incdata_schd2_le.csv") 
incdata_le_schd3 <- read_csv2("incdata_schd3_le.csv") 
incdata_le_schd4 <- read_csv2("incdata_schd4_le.csv") 
incdata_le_hosd1 <- read_csv2("incdata_hosd1_le.csv") 
incdata_le_hosd2 <- read_csv2("incdata_hosd2_le.csv") 
incdata_le_hosd3 <- read_csv2("incdata_hosd3_le.csv") 
incdata_le_hosd4 <- read_csv2("incdata_hosd4_le.csv") 

incdata_ne_schp <- read_csv2("incdata_schp_ne.csv") %>% 
  bind_cols(NA) #need to be same width as others for functions
incdata_ne_schd1 <- read_csv2("incdata_schd1_ne.csv") 
incdata_ne_schd2 <- read_csv2("incdata_schd2_ne.csv") 
incdata_ne_schd3 <- read_csv2("incdata_schd3_ne.csv") 
incdata_ne_schd4 <- read_csv2("incdata_schd4_ne.csv") 
incdata_ne_hosd1 <- read_csv2("incdata_hosd1_ne.csv") 
incdata_ne_hosd2 <- read_csv2("incdata_hosd2_ne.csv") 
incdata_ne_hosd3 <- read_csv2("incdata_hosd3_ne.csv") 
incdata_ne_hosd4 <- read_csv2("incdata_hosd4_ne.csv") 
##RWP####
rwpdata <- read_csv2("rwpdata.csv") %>% 
  subset(afstemid!=101056)#I remove the area 101056. It is a newly build up area in the port of Copenhagen, and thus does not exist in the early periods.


rwpdata_le <- subset(rwpdata, election=="KV") %>% 
  subset(F== komnr %in% c(741, 563, 492)) #No right-wing populist party fielded candidates in the local elections in 2009 in the three small island municipalities of Fanø, Ærø and Samsø
rwpdata_ne <- subset(rwpdata, election=="FV") 

###Reading datasets

table(subset(rwpdata, election=="KV")$schp_group, subset(rwpdata, election=="KV")$year)

table(subset(rwpdata, election=="FV")$hosd1_group, subset(rwpdata, election=="FV")$year)
#removing observations that should not be included in analysis for various reasons.
rwpdata_le_schp <- subset(rwpdata_le, schp_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_le_schd1 <- subset(rwpdata_le, schd1_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_le_schd2 <- subset(rwpdata_le, schd2_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_le_schd3 <- subset(rwpdata_le, schd3_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_le_schd4 <- subset(rwpdata_le, schd4_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_le_hosd1 <- subset(rwpdata_le, hosd1_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_le_hosd2 <- subset(rwpdata_le, hosd2_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_le_hosd3 <- subset(rwpdata_le, hosd3_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_le_hosd4 <- subset(rwpdata_le, hosd4_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 

rwpdata_ne_schp <- subset(rwpdata_ne, schp_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_ne_schd1 <- subset(rwpdata_ne, schd1_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_ne_schd2 <- subset(rwpdata_ne, schd2_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_ne_schd3 <- subset(rwpdata_ne, schd3_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_ne_schd4 <- subset(rwpdata_ne, schd4_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_ne_hosd1 <- subset(rwpdata_ne, hosd1_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_ne_hosd2 <- subset(rwpdata_ne, hosd2_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_ne_hosd3 <- subset(rwpdata_ne, hosd3_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
rwpdata_ne_hosd4 <- subset(rwpdata_ne, hosd4_group %in% c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 

#Figure 1####
##Panel (a)####

schools <- read_csv("school.csv",  locale=locale(encoding = "UTF-8"))
brks <- seq(as.Date("2005-01-01"), as.Date("2019-06-1"), 365)

schoolscount <- tibble()
for (x in 1:length(brks)) {
  schoolscount[x,1] <- brks[x] 
  schoolscount[x,2] <- length(subset(schools, established<brks[x] & is.na(closed) |established<brks[x] & closed>brks[x] |is.na(established) & is.na(closed) | is.na(established) &  closed>brks[x])$schoolid)
}

schoolscount$...3 <- lag(schoolscount$...2)
schoolscount$...4 <- schoolscount$...2-schoolscount$...3

schfig <- ggplot(schoolscount, aes(y = ...2, x = ...1))+geom_col(width = 200, fill = "#999999")+theme_light()+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", limits = c(min(brks)-100, max(brks)+100))+
  labs(x = "Year", y = "Number of schools", size = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1650), breaks =  seq(0, 1650, 200))+
  theme_bw(base_size = 11)+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank() ,
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

ggsave("Figure_1a.eps" , plot = last_plot(), 
       scale = 1, width = 80, height = 67, units = c("mm"),
       dpi = 1000)

##Panel (b)####
hospitals <- read_csv("hospital.csv",  locale=locale(encoding = "UTF-8"))

hospitalscount <- tibble()
for (x in 1:length(brks)) {
  hospitalscount[x,1] <- brks[x] 
  hospitalscount[x,2] <- length(subset(hospitals, is.na(closed) |  closed>brks[x])$hospid)
}


hosfig <- ggplot(hospitalscount, aes(y = ...2, x = ...1))+
  geom_col(width = 200, fill = "#999999")+theme_light()+
  scale_x_date(date_breaks = "2 year",date_labels = "%Y", limits = c(min(brks)-100, max(brks)+100))+
  labs(x = "Year", y = "Number of hospitals") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 78), breaks =  seq(0, 76, 10))+
  theme_bw(base_size = 11)+
  theme(panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank() ,
      axis.text=element_text(size=10),
      axis.title=element_text(size=10))

ggsave("Figure_1b.eps" , plot = last_plot(), 
       scale = 1, width = 80, height = 67, units = c("mm"),
       dpi = 1000)

#Figure 3#####
m.schp <- clear_and_match_rwp_le(rwpdata_le_schp, "schp")[[2]]

m.hosd1 <- clear_and_match_rwp_ne(rwpdata_ne_hosd1, "hosd1")[[2]]

balance_plot <- bind_rows(bind_cols(bal.tab(m.schp,  binary = "std", thresholds = c(m = .1), abs = T, un = T)[1]$Balance, elec = "Local Election:\n School closure"),
                          bind_cols(bal.tab(m.hosd1,  binary = "std", thresholds = c(m = .1), abs = T, un = T)[1]$Balance, elec = "National Election:\n Hospital closure")) %>%
  mutate(covar=row.names(bind_rows(bind_cols(bal.tab(m.schp,  binary = "std", thresholds = c(m = .1), abs = T, un = T)[1]$Balance, elec = "Local Election:\n School closure"),
                                   bind_cols(bal.tab(m.hosd1,  binary = "std", thresholds = c(m = .1), abs = T, un = T)[1]$Balance, elec = "National Election:\n Hospital closure")))) %>% 
  pivot_longer(cols = c("Diff.Un", "Diff.Adj")) %>% 
  mutate(
    name = factor(name, levels = c("Diff.Adj", "Diff.Un"), labels = c("Matched", "Unmatched")),
    covar = ifelse(covar %in% c("popdens_2009", "popdens_2007"), "Population density",
                   ifelse(covar %in% c("loginc_2009", "loginc_2007"), "Household income",
                          ifelse(covar %in% c("distance...10", "distance...1"), "Propensity Score",
                                 ifelse(covar %in% c("ayoung_2009", "ayoung_2007"), "0-30 year olds",
                                        ifelse(covar %in% c("aunemp_2009", "aunemp_2007"), "Unemployed",
                                               ifelse(covar %in% c("aold_2009", "aold_2007"), "55+ year olds",
                                                      ifelse(covar %in% c("anwimm_2009", "anwimm_2007"), "Immigrants and decendants",
                                                             ifelse(covar %in% c("alowedu_2009", "alowedu_2007"), "Primary education or less",
                                                                    ifelse(covar %in% c("ahighedu_2009", "ahighedu_2007"), "Bachelor's degree or longer",NA))))))))),
    covar = factor(covar, levels = rev(c("Propensity Score",
                                         "55+ year olds",
                                         "0-30 year olds",
                                         "Immigrants and decendants",
                                         "Bachelor's degree or longer",
                                         "Primary education or less",
                                         "Unemployed",
                                         "Household income",
                                         "Population density")))
    )

                   
                                                                           
                                                                    
ggplot(balance_plot, aes(x = value, y = covar, group = name, colour = name)) + 
  geom_vline(xintercept=0.1, linetype="dashed")+
  facet_grid(cols = vars(elec), scales="free_x")+
  geom_point(aes(shape=name)) +
  labs(x = "Absolute Standardized Mean Differences", y = "")+
  scale_colour_manual(values = c('grey40','black')) +
  scale_x_continuous(limits = c(0, 0.9))+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))

ggsave("Figure_3.eps" , plot = last_plot(), 
       scale = 1, width = 160, height = 90, units = c("mm"),
       dpi = 1000)

#Appendix E####
rwpdata_le_schp_tab <- clear_and_match_rwp_le(rwpdata_le_schp, "schp")[[1]]
rwpdata_ne_hosd1_tab <- clear_and_match_rwp_ne(rwpdata_ne_hosd1, "hosd1")[[1]]

tab1 <- tibble()
tab1[1,1:7] <- list("Variable name", "Unmatched control", "Matched control", "Treated",
                                     "Unmatched control", "Matched control", "Treated"
)
tab1[2,1:7] <- list("Area (km2)", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$area), 0), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$area), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$area))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$area), 0), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$area), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$area))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$area), 0), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$area), 0), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$area), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$area))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$area), 0), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$area), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$area))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$area), 0), big.mark = ","))
)

tab1[3,1:7] <- list("Eligible voters", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$eligible.votes), 0), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$eligible.votes), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$eligible.votes))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$eligible.votes), 0), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$pop), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$eligible.votes))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$eligible.votes), 0), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$eligible.votes), 0), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$eligible.votes), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$eligible.votes))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$eligible.votes), 0), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$eligible.votes), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$eligible.votes))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$eligible.votes), 0), big.mark = ","))
                    )

tab1[4,1:7] <- list("55+ year olds", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$aold), 2), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aold), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$aold))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$aold), 2), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aold), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$aold))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aold), 2), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$aold), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aold), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$aold))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$aold), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aold), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$aold))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aold), 2), big.mark = ","))
)

tab1[5,1:7] <- list("0-30 year olds", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$ayoung), 2), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ayoung), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$ayoung))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$ayoung), 2), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ayoung), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$ayoung))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ayoung), 2), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$ayoung), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ayoung), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$ayoung))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$ayoung), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ayoung), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$ayoung))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ayoung), 2), big.mark = ","))
)

tab1[6,1:7] <- list("Immigrants and decendants", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$anwimm), 2), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$anwimm), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$anwimm))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$anwimm), 2), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$anwimm), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$anwimm))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$anwimm), 2), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$anwimm), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$anwimm), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$anwimm))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$anwimm), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$anwimm), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$anwimm))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$anwimm), 2), big.mark = ","))
)

tab1[7,1:7] <- list("Bachelor's degree or longer", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$ahighedu), 2), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ahighedu), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$ahighedu))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$ahighedu), 2), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ahighedu), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$ahighedu))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ahighedu), 2), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$ahighedu), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ahighedu), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$ahighedu))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$ahighedu), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ahighedu), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$ahighedu))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$ahighedu), 2), big.mark = ","))
)

tab1[8,1:7] <- list("Primary education or less", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$alowedu), 2), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$alowedu), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$alowedu))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$alowedu), 2), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$alowedu), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$alowedu))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$alowedu), 2), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$alowedu), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$alowedu), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$alowedu))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$alowedu), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$alowedu), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$alowedu))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$alowedu), 2), big.mark = ","))
)

tab1[9,1:7] <- list("Unemployed", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$aunemp), 2), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aunemp), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$aunemp))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$aunemp), 2), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aunemp), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$aunemp))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aunemp), 2), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$aunemp), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aunemp), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$aunemp))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$aunemp), 2), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aunemp), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$aunemp))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$aunemp), 2), big.mark = ","))
)

tab1[10,1:7] <- list("Median household income", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$minc), -3), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$minc), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$minc))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$minc), -3), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$minc), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$minc))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$minc), -3), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$minc), -3), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$minc), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$minc))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$minc), -3), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$minc), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$minc))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$minc), -3), big.mark = ","))
)

tab1[11,1:7] <- list("Right-wing populist parties", 
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$rwp_sup), 3), big.mark = ","), 
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$rwp_sup), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$rwp_sup))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$rwp_sup), 3), big.mark = ","), ifelse(0.05>t.test(as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$rwp_sup), as.numeric(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$rwp_sup))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$rwp_sup), 3), big.mark = ",")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$rwp_sup), 3), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$rwp_sup), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$rwp_sup))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$rwp_sup), 3), big.mark = ","),
                           ifelse(0.05>t.test(as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$rwp_sup), as.numeric(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$rwp_sup))$p.value, "*", "")),
                    paste0(format(round(mean(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$rwp_sup), 3), big.mark = ","))
)

tab1[12,1:7] <- list("Precinct-year observations", 
                     paste0(format(round(length(subset(rwpdata_le_schp_tab, schp_group %in% c("Control"))$rwp_sup), 3), big.mark = ",")), 
                     paste0(format(round(length(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & match_s==1)$rwp_sup), 3), big.mark = ",")), 
                     paste0(format(round(length(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$rwp_sup), 3), big.mark = ",")),
                     paste0(format(round(length(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control"))$rwp_sup), 3), big.mark = ",")),
                     paste0(format(round(length(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & match_s==1)$rwp_sup), 3), big.mark = ",")),
                     paste0(format(round(length(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period"))$rwp_sup), 3), big.mark = ","))
)

tab1[13,1:7] <- list("Years", 
                     "3", "3", "3", "4", "4", "4")

tab1[14,1:7] <- list("Precinct", 
                     paste0(format(round(length(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & year==2009)$rwp_sup), 3), big.mark = ",")), 
                     paste0(format(round(length(subset(rwpdata_le_schp_tab, schp_group %in% c("Control") & year==2009 & match_s==1)$rwp_sup), 3), big.mark = ",")), 
                     paste0(format(round(length(subset(rwpdata_le_schp_tab, schp_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period") & year==2009)$rwp_sup), 3), big.mark = ",")),
                     paste0(format(round(length(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & year==2007)$rwp_sup), 3), big.mark = ",")),
                     paste0(format(round(length(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Control") & year==2007 & match_s==1)$rwp_sup), 3), big.mark = ",")),
                     paste0(format(round(length(subset(rwpdata_ne_hosd1_tab, hosd1_group %in% c("Treat 1st period", "Treat 2nd period", "Treat 3rd period") & year==2007)$rwp_sup), 3), big.mark = ","))
)

xtable(tab1)
#Figure 4####
incdata_le_schp_1 <- clear_and_match_inc_le(incdata_le_schp, "schp")[[1]]
incdata_le_schp_2 <- clear_and_match_inc_le(incdata_le_schp, "schp")[[2]]

incdata_ne_hosd1_1 <- clear_and_match_inc_ne(incdata_ne_hosd1, "hosd1")[[1]]
incdata_ne_hosd1_2 <- clear_and_match_inc_ne(incdata_ne_hosd1, "hosd1")[[2]]
incdata_ne_hosd1_3 <- clear_and_match_inc_ne(incdata_ne_hosd1, "hosd1")[[3]]


Model1.1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                 gname = "schp_g", xformla = ~ 1,
                 data = subset(incdata_le_schp_1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model1.1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                   gname = "schp_g", xformla = ~ 1,
                   data = subset(incdata_le_schp_1, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model1.2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                   gname = "schp_g", xformla = ~ 1,
                   data = subset(incdata_le_schp_2), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model1.2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                    gname = "schp_g", xformla = ~ 1,
                    data = subset(incdata_le_schp_2, match_s==1), 
                    est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

##ne
Model2.1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                   gname = "hosd1_g",
                   xformla = ~ 1, 
                   data = subset(incdata_ne_hosd1_1),
                   est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model2.1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                    gname = "hosd1_g",
                    xformla = ~ 1,
                    data = subset(incdata_ne_hosd1_1, match_s==1)  ,
                    est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model2.2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                   gname = "hosd1_g",
                   xformla = ~ 1,
                   data = subset(incdata_ne_hosd1_2),
                   est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model2.2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                    gname = "hosd1_g",
                    xformla = ~ 1,
                    data = subset(incdata_ne_hosd1_2, match_s==1),
                    est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model2.3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                   gname = "hosd1_g",
                   xformla = ~ 1,
                   data = subset(incdata_ne_hosd1_3),
                   est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model2.3m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                    gname = "hosd1_g",
                    xformla = ~ 1,
                    data = subset(incdata_ne_hosd1_3, match_s==1),
                    est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )



try1 <- bind_cols(extract.inc.le(Model1.1, Model1.2), Model="Unmatched", elec="Local Election:\n School Closure")
try2 <- bind_cols(extract.inc.le(Model1.1m, Model1.2m), Model="Matched", elec="Local Election:\n School Closure")
try3 <- bind_cols(extract.inc.ne(Model2.1, Model2.2, Model2.3), Model="Unmatched", elec="National Election:\n Hospital Closure")
try4 <- bind_cols(extract.inc.ne(Model2.1m, Model2.2m, Model2.3m), Model="Matched", elec="National Election:\n Hospital Closure")

       
incplot <- bind_rows(try1, try2, try3, try4) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2"))
  )
         
incplot1<- ggplot(data = incplot, aes(x=time, y=att , group = Model, colour = Model))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(aes(shape=Model), position=position_dodge(width=0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(cols = vars(elec), scales="free_x" , space = "free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in support for incumbent \n (percentage points)")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  scale_colour_manual(values = c('grey40','black')) +
  geom_point(data = subset(incplot, !is.na(meta)), aes(x=time, y=att, group = Model, colour = Model), colour = c('black','grey40','black', 'black', 'black', 'grey40','grey40','grey40'), shape = c(24, 21, rep(24,3), rep(21,3)),  fill = "white",  position=position_dodge(width=0.5))+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))


ggsave("Figure_4.eps" , plot = incplot1, 
       scale = 1, width = 160 , height = 90, units = c("mm"),
       dpi = 1000)

#Figure 5####
incdata_le_schd1_l1 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 2.5)[[1]]
incdata_le_schd1_h1 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 2.5)[[2]]
incdata_le_schd1_l2 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 2.5)[[3]]
incdata_le_schd1_h2 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 2.5)[[4]]

incdata_ne_hosd1_l1 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[1]]
incdata_ne_hosd1_h1 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[2]]
incdata_ne_hosd1_l2 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[3]]
incdata_ne_hosd1_h2 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[4]]
incdata_ne_hosd1_l3 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[5]]
incdata_ne_hosd1_h3 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[6]]

summary(subset(incdata_le_schd1, schd1_deltadist!=0)$schd1_deltadist)
names(incdata_ne_hosd1)
#le
Model1.1.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                     gname = "schd1_g", xformla = ~ 1,
                     data = subset(incdata_le_schd1_l1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                     gname = "schd1_g", xformla = ~ 1,
                     data = subset(incdata_le_schd1_h1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model1.2.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                     gname = "schd1_g", xformla = ~ 1,
                     data = subset(incdata_le_schd1_l2), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                     gname = "schd1_g", xformla = ~ 1,
                     data = subset(incdata_le_schd1_h2), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


Model1.1m.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                      gname = "schd1_g", xformla = ~ 1,
                      data = subset(incdata_le_schd1_l1, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                      gname = "schd1_g", xformla = ~ 1,
                      data = subset(incdata_le_schd1_h1, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model1.2m.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                      gname = "schd1_g", xformla = ~ 1,
                      data = subset(incdata_le_schd1_l2, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                      gname = "schd1_g", xformla = ~ 1,
                      data = subset(incdata_le_schd1_h2, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


##ne
Model2.1.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_l1),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_h1),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_l2),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_h2),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_l3),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_h3),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


Model2.1m.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_l1, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1m.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_h1, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_l2, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_h2, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.l <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_l3, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.h <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_h3, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


le.low_um <- bind_cols(extract.inc.le(Model1.1.l, Model1.2.l), Model="Unmatched", deltadist="Low", elec="Local Election:\n School Closure")
le.hig_um <- bind_cols(extract.inc.le(Model1.1.h, Model1.2.h), Model="Unmatched", deltadist="High",  elec="Local Election:\n School Closure")

le.low_ma <- bind_cols(extract.inc.le(Model1.1m.l, Model1.2m.l), Model="Matched", deltadist="Low", elec="Local Election:\n School Closure")
le.hig_ma <- bind_cols(extract.inc.le(Model1.1m.h, Model1.2m.h), Model="Matched", deltadist="High",  elec="Local Election:\n School Closure")

ne.low_um <- bind_cols(extract.inc.ne(Model2.1.l, Model2.2.l, Model2.3.l), Model="Unmatched", deltadist="Low", elec="National Election:\n Hospital Closure")
ne.hig_um <- bind_cols(extract.inc.ne(Model2.1.h, Model2.2.h, Model2.3.h), Model="Unmatched", deltadist="High",  elec="National Election:\n Hospital Closure")

ne.low_ma <- bind_cols(extract.inc.ne(Model2.1m.l, Model2.2m.l, Model2.3m.l), Model="Matched", deltadist="Low", elec="National Election:\n Hospital Closure")
ne.hig_ma <- bind_cols(extract.inc.ne(Model2.1m.h, Model2.2m.h, Model2.3m.h), Model="Matched", deltadist="High",  elec="National Election:\n Hospital Closure")

deltadistplot <- bind_rows(le.low_um, le.hig_um, le.low_ma, le.hig_ma,
                           ne.low_um, ne.hig_um, ne.low_ma, ne.hig_ma) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         deltadist=factor(deltadist, levels = c("Low", "High"), labels = c("Low Severity\nChange of <2.5 km\nChange of <10 km", "High Severity\n>2.5 km to nearest school\n>10 km to nearest hospital")),
         Model  = factor(Model ,levels = c("Unmatched", "Matched"))
  )

ggplot(data = deltadistplot, aes(x=time, y=att , group = deltadist, color = Model))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(aes(shape=deltadist), position=position_dodge(width=0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(cols = vars(elec), rows = vars(Model), scales="free_x", space = "free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in support for incumbent \n (percentage points)")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  geom_point(data = subset(deltadistplot, !is.na(meta)), aes(x=time, y=att, group = deltadist), shape = c(21, 24, 
                                                                                                          21, 21, 21,
                                                                                                          24, 24, 24,
                                                                                                          21, 24,
                                                                                                          21, 21, 21,
                                                                                                          24, 24, 24) , fill = "white",  position=position_dodge(width=0.5))+
  scale_colour_manual(values = c("black", "grey40"), guide = "none") +
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))


ggsave("Figure_5.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)

#Figure 6####
rwpdata_le_schp_m <- clear_and_match_rwp_le(rwpdata_le_schp, "schp")[[1]]
rwpdata_ne_hosd1_m <- clear_and_match_rwp_ne(rwpdata_ne_hosd1, "hosd1")[[1]]


rwp_le_schp_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schp_g", data = subset(rwpdata_le_schp_m)
)
rwp_le_schp_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_m,  match_s==1)
)
rwp_ne_hosd1_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_m)
)
rwp_ne_hosd1_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_m,  match_s==1)
)


rwp.le <- extract.rwp.le(rwp_le_schp_1u, rwp_le_schp_1m)
rwp.ne <- extract.rwp.ne(rwp_ne_hosd1_1u, rwp_ne_hosd1_1m)

rwpplot <- bind_rows(rwp.le, rwp.ne) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2"))
  )


rwpplot1<- ggplot(data = rwpplot, aes(x=time, y=att , group = Model, colour = Model))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(aes(shape=Model), position=position_dodge(width=0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(cols = vars(elec), scales="free_x" , space = "free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in support for right-wing populists \n (percentage points)")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  scale_colour_manual(values = c('grey40','black')) +
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))


ggsave("Figure_6.eps" , plot = rwpplot1, 
       scale = 1, width = 160 , height = 90, units = c("mm"),
       dpi = 1000)
#Figure 7####

rwpdata_le_schd1_l <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 2.5)[[1]]
rwpdata_le_schd1_h <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 2.5)[[2]]

rwpdata_ne_hosd1_l <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 10)[[1]]
rwpdata_ne_hosd1_h <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 10)[[2]]

rwp_le_l <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schd1_g", data = subset(rwpdata_le_schd1_l)
)
rwp_le_h <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schd1_g", data = subset(rwpdata_le_schd1_h)
)

rwp_le_l.m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schd1_g", data = subset(rwpdata_le_schd1_l, match_s==1)
)
rwp_le_h.m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schd1_g", data = subset(rwpdata_le_schd1_h, match_s==1)
)


rwp_ne_l <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l)
)
rwp_ne_l.m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l, match_s==1)
)

rwp_ne_h <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h)
)
rwp_ne_h.m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h, match_s==1)
)


le.low <- bind_cols(extract.rwp.le(rwp_le_l, rwp_le_l.m), deltadist="Low")
le.hig <- bind_cols(extract.rwp.le(rwp_le_h, rwp_le_h.m), deltadist="High")

ne.low <- bind_cols(extract.rwp.ne(rwp_ne_l, rwp_ne_l.m), deltadist="Low")
ne.hig <- bind_cols(extract.rwp.ne(rwp_ne_h, rwp_ne_h.m), deltadist="High")


deltadistplot <- bind_rows(le.low, le.hig, ne.low, ne.hig) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         deltadist=factor(deltadist, levels = c("Low", "High"), labels = c("Low Severity\nChange of <2.5 km\nChange of <10 km", "High Severity\n>2.5 km to nearest school\n>10 km to nearest hospital")),
         Model  = factor(Model ,levels = c("Unmatched", "Matched"))
  )

ggplot(data = deltadistplot, aes(x=time, y=att , group = deltadist, color = Model))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(aes(shape=deltadist), position=position_dodge(width=0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(cols = vars(elec), rows = vars(Model), scales="free_x", space = "free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in support for right-wing populists \n (percentage points)")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  scale_colour_manual(values = c("black", "grey40"), guide = "none") +
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))

ggsave("Figure_7.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)


#Appendix A####
##Figure A1####
incdata_le_schd1_1 <- clear_and_match_inc_le(incdata_le_schd1, "schd1")[[1]]
incdata_le_schd1_2 <- clear_and_match_inc_le(incdata_le_schd1, "schd1")[[2]]

incdata_le_schd2_1 <- clear_and_match_inc_le(incdata_le_schd2, "schd2")[[1]]
incdata_le_schd2_2 <- clear_and_match_inc_le(incdata_le_schd2, "schd2")[[2]]

incdata_le_schd3_1 <- clear_and_match_inc_le(incdata_le_schd3, "schd3")[[1]]
incdata_le_schd3_2 <- clear_and_match_inc_le(incdata_le_schd3, "schd3")[[2]]

incdata_le_schd4_1 <- clear_and_match_inc_le(incdata_le_schd4, "schd4")[[1]]
incdata_le_schd4_2 <- clear_and_match_inc_le(incdata_le_schd4, "schd4")[[2]]

incdata_ne_hosd1_1 <- clear_and_match_inc_ne(incdata_ne_hosd1, "hosd1")[[1]]
incdata_ne_hosd1_2 <- clear_and_match_inc_ne(incdata_ne_hosd1, "hosd1")[[2]]
incdata_ne_hosd1_3 <- clear_and_match_inc_ne(incdata_ne_hosd1, "hosd1")[[3]]

incdata_ne_hosd2_1 <- clear_and_match_inc_ne(incdata_ne_hosd2, "hosd2")[[1]]
incdata_ne_hosd2_2 <- clear_and_match_inc_ne(incdata_ne_hosd2, "hosd2")[[2]]
incdata_ne_hosd2_3 <- clear_and_match_inc_ne(incdata_ne_hosd2, "hosd2")[[3]]

incdata_ne_hosd3_1 <- clear_and_match_inc_ne(incdata_ne_hosd3, "hosd3")[[1]]
incdata_ne_hosd3_2 <- clear_and_match_inc_ne(incdata_ne_hosd3, "hosd3")[[2]]
incdata_ne_hosd3_3 <- clear_and_match_inc_ne(incdata_ne_hosd3, "hosd3")[[3]]

incdata_ne_hosd4_1 <- clear_and_match_inc_ne(incdata_ne_hosd4, "hosd4")[[1]]
incdata_ne_hosd4_2 <- clear_and_match_inc_ne(incdata_ne_hosd4, "hosd4")[[2]]
incdata_ne_hosd4_3 <- clear_and_match_inc_ne(incdata_ne_hosd4, "hosd4")[[3]]

  
le_schd1_1u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "schd1_g", data = subset(incdata_le_schd1_1)# match_s==1)
                   )
le_schd1_2u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd1_g", data = subset(incdata_le_schd1_2)# match_s==1)
)
le_schd1_1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd1_g", data = subset(incdata_le_schd1_1, match_s==1)
)
le_schd1_2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd1_g", data = subset(incdata_le_schd1_2, match_s==1)
)

le_schd2_1u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd2_g", data = subset(incdata_le_schd2_1)# match_s==1)
)
le_schd2_2u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd2_g", data = subset(incdata_le_schd2_2)# match_s==1)
)
le_schd2_1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd2_g", data = subset(incdata_le_schd2_1, match_s==1)
)
le_schd2_2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd2_g", data = subset(incdata_le_schd2_2, match_s==1)
)

le_schd3_1u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd3_g", data = subset(incdata_le_schd3_1)# match_s==1)
)
le_schd3_2u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd3_g", data = subset(incdata_le_schd3_2)# match_s==1)
)
le_schd3_1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd3_g", data = subset(incdata_le_schd3_1, match_s==1)
)
le_schd3_2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd3_g", data = subset(incdata_le_schd3_2, match_s==1)
)

le_schd4_1u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd4_g", data = subset(incdata_le_schd4_1)# match_s==1)
)
le_schd4_2u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd4_g", data = subset(incdata_le_schd4_2)# match_s==1)
)
le_schd4_1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd4_g", data = subset(incdata_le_schd4_1, match_s==1)
)
le_schd4_2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd4_g", data = subset(incdata_le_schd4_2, match_s==1)
)

ne_hosd1_1u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd1_g", data = subset(incdata_ne_hosd1_1)# match_s==1)
)
ne_hosd1_2u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd1_g", data = subset(incdata_ne_hosd1_2)# match_s==1)
)
ne_hosd1_3u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd1_g", data = subset(incdata_ne_hosd1_3)# match_s==1)
)

ne_hosd1_1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd1_g", data = subset(incdata_ne_hosd1_1, match_s==1)
)
ne_hosd1_2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd1_g", data = subset(incdata_ne_hosd1_2, match_s==1)
)
ne_hosd1_3m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd1_g", data = subset(incdata_ne_hosd1_3, match_s==1)
)

ne_hosd2_1u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd2_g", data = subset(incdata_ne_hosd2_1)# match_s==1)
)
ne_hosd2_2u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd2_g", data = subset(incdata_ne_hosd2_2)# match_s==1)
)
ne_hosd2_3u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd2_g", data = subset(incdata_ne_hosd2_3)# match_s==1)
)

ne_hosd2_1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd2_g", data = subset(incdata_ne_hosd2_1, match_s==1)
)
ne_hosd2_2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd2_g", data = subset(incdata_ne_hosd2_2, match_s==1)
)
ne_hosd2_3m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd2_g", data = subset(incdata_ne_hosd2_3, match_s==1)
)

ne_hosd3_1u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd3_g", data = subset(incdata_ne_hosd3_1)# match_s==1)
)
ne_hosd3_2u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd3_g", data = subset(incdata_ne_hosd3_2)# match_s==1)
)
ne_hosd3_3u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd3_g", data = subset(incdata_ne_hosd3_3)# match_s==1)
)

ne_hosd3_1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd3_g", data = subset(incdata_ne_hosd3_1, match_s==1)
)
ne_hosd3_2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd3_g", data = subset(incdata_ne_hosd3_2, match_s==1)
)
ne_hosd3_3m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd3_g", data = subset(incdata_ne_hosd3_3, match_s==1)
)

ne_hosd4_1u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd4_g", data = subset(incdata_ne_hosd4_1)# match_s==1)
)
ne_hosd4_2u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd4_g", data = subset(incdata_ne_hosd4_2)# match_s==1)
)
ne_hosd4_3u <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd4_g", data = subset(incdata_ne_hosd4_3)# match_s==1)
)

ne_hosd4_1m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd4_g", data = subset(incdata_ne_hosd4_1, match_s==1)
)
ne_hosd4_2m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd4_g", data = subset(incdata_ne_hosd4_2, match_s==1)
)
ne_hosd4_3m <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd4_g", data = subset(incdata_ne_hosd4_3, match_s==1)
)

schd1u <- bind_cols(extract.inc.le(le_schd1_1u, le_schd1_2u), Model="Unmatched", elec="Local Election:\n School Closure", d="Nearest")
schd1m <- bind_cols(extract.inc.le(le_schd1_1m, le_schd1_2m), Model="Matched", elec="Local Election:\n School Closure",  d="Nearest")
schd2u <- bind_cols(extract.inc.le(le_schd2_1u, le_schd2_2u), Model="Unmatched", elec="Local Election:\n School Closure", d="Two nearest")
schd2m <- bind_cols(extract.inc.le(le_schd2_1m, le_schd2_2m), Model="Matched", elec="Local Election:\n School Closure",  d="Two nearest")
schd3u <- bind_cols(extract.inc.le(le_schd3_1u, le_schd3_2u), Model="Unmatched", elec="Local Election:\n School Closure", d="Three nearest")
schd3m <- bind_cols(extract.inc.le(le_schd3_1m, le_schd3_2m), Model="Matched", elec="Local Election:\n School Closure",  d="Three nearest")
schd4u <- bind_cols(extract.inc.le(le_schd4_1u, le_schd4_2u), Model="Unmatched", elec="Local Election:\n School Closure", d="Four nearest")
schd4m <- bind_cols(extract.inc.le(le_schd4_1m, le_schd4_2m), Model="Matched", elec="Local Election:\n School Closure",  d="Four nearest")

hosd1u <- bind_cols(extract.inc.ne(ne_hosd1_1u, ne_hosd1_2u, ne_hosd1_3u), Model="Unmatched", elec="National Election:\n Hospital closure", d="Nearest")
hosd1m <- bind_cols(extract.inc.ne(ne_hosd1_1m, ne_hosd1_2m, ne_hosd1_3m), Model="Matched", elec="National Election:\n Hospital closure", d="Nearest")
hosd2u <- bind_cols(extract.inc.ne(ne_hosd2_1u, ne_hosd2_2u, ne_hosd2_3u), Model="Unmatched", elec="National Election:\n Hospital closure", d="Two nearest")
hosd2m <- bind_cols(extract.inc.ne(ne_hosd2_1m, ne_hosd2_2m, ne_hosd2_3m), Model="Matched", elec="National Election:\n Hospital closure", d="Two nearest")
hosd3u <- bind_cols(extract.inc.ne(ne_hosd3_1u, ne_hosd3_2u, ne_hosd3_3u), Model="Unmatched", elec="National Election:\n Hospital closure", d="Three nearest")
hosd3m <- bind_cols(extract.inc.ne(ne_hosd3_1m, ne_hosd3_2m, ne_hosd3_3m), Model="Matched", elec="National Election:\n Hospital closure", d="Three nearest")
hosd4u <- bind_cols(extract.inc.ne(ne_hosd4_1u, ne_hosd4_2u, ne_hosd4_3u), Model="Unmatched", elec="National Election:\n Hospital closure", d="Four nearest")
hosd4m <- bind_cols(extract.inc.ne(ne_hosd4_1m, ne_hosd4_2m, ne_hosd4_3m), Model="Matched", elec="National Election:\n Hospital closure", d="Four nearest")


app.a <- bind_rows(schd1u, schd1m, schd2u, schd2m, schd3u, schd3m, schd4u, schd4m, 
                   hosd1u, hosd1m, hosd2u, hosd2m, hosd3u, hosd3m, hosd4u, hosd4m 
                   ) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         Model=factor(Model, levels = c( "Unmatched", "Matched")),
         d=factor(d, levels = c("Nearest", "Two nearest", "Three nearest", "Four nearest")),
         shapen=ifelse(d=="Nearest", 1,
                       ifelse(d=="Two nearest", 0, 
                              ifelse(d=="Three nearest", 2,
                                     ifelse(d=="Four nearest", 5, NA))))
         
  ) %>% 
  arrange(by_group=d)

ggplot(data = subset(app.a), aes(x=time, y=att, group = d, colour = d, shape = d))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(position=position_dodge(width=0.5))+
  scale_shape_manual(values = c(15, 16, 17, 18))+
  scale_colour_manual(values = c("#cccccc", "#969696", "#525252", "black")) +
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(vars(Model), vars(elec), scales="free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in support for incumbent \n (percentage points)")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))
 
ggsave("Figure_A1.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)

##Figure A2####
rwpdata_le_schd1_1 <- clear_and_match_rwp_le(rwpdata_le_schd1, "schd1")[[1]]
rwpdata_le_schd2_1 <- clear_and_match_rwp_le(rwpdata_le_schd2, "schd2")[[1]]
rwpdata_le_schd3_1 <- clear_and_match_rwp_le(rwpdata_le_schd3, "schd3")[[1]]
rwpdata_le_schd4_1 <- clear_and_match_rwp_le(rwpdata_le_schd4, "schd4")[[1]]
rwpdata_ne_hosd1_1 <- clear_and_match_rwp_ne(rwpdata_ne_hosd1, "hosd1")[[1]]
rwpdata_ne_hosd2_1 <- clear_and_match_rwp_ne(rwpdata_ne_hosd2, "hosd2")[[1]]
rwpdata_ne_hosd3_1 <- clear_and_match_rwp_ne(rwpdata_ne_hosd3, "hosd3")[[1]]
rwpdata_ne_hosd4_1 <- clear_and_match_rwp_ne(rwpdata_ne_hosd4, "hosd4")[[1]]

le_schd1_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd1_g", data = subset(rwpdata_le_schd1_1)
)
le_schd1_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd1_g", data = subset(rwpdata_le_schd1_1, match_s==1)
)
le_schd2_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd2_g", data = subset(rwpdata_le_schd2_1)
)
le_schd2_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd2_g", data = subset(rwpdata_le_schd2_1, match_s==1)
)
le_schd3_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd3_g", data = subset(rwpdata_le_schd3_1)
)
le_schd3_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd3_g", data = subset(rwpdata_le_schd3_1, match_s==1)
)
le_schd4_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd4_g", data = subset(rwpdata_le_schd4_1)
)
le_schd4_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "schd4_g", data = subset(rwpdata_le_schd4_1, match_s==1)
)
ne_hosd1_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_1)
)
ne_hosd1_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_1, match_s==1)
)
ne_hosd2_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd2_g", data = subset(rwpdata_ne_hosd2_1)
)

ne_hosd2_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd2_g", data = subset(rwpdata_ne_hosd2_1, match_s==1)
)
ne_hosd3_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd3_g", data = subset(rwpdata_ne_hosd3_1)
)
ne_hosd3_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd3_g", data = subset(rwpdata_ne_hosd3_1, match_s==1)
)
ne_hosd4_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd4_g", data = subset(rwpdata_ne_hosd4_1)
)
ne_hosd4_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                      gname = "hosd4_g", data = subset(rwpdata_ne_hosd4_1, match_s==1)
)

schd1 <- bind_cols(extract.rwp.le(le_schd1_1u, le_schd1_1m), d="Nearest")
schd2 <- bind_cols(extract.rwp.le(le_schd2_1u, le_schd2_1m), d="Two nearest")
schd3 <- bind_cols(extract.rwp.le(le_schd3_1u, le_schd3_1m), d="Three nearest")
schd4 <- bind_cols(extract.rwp.le(le_schd4_1u, le_schd4_1m), d="Four nearest")

hosd1 <- bind_cols(extract.rwp.ne(ne_hosd1_1u, ne_hosd1_1m), d="Nearest")
hosd2 <- bind_cols(extract.rwp.ne(ne_hosd2_1u, ne_hosd2_1m), d="Two nearest")
hosd3 <- bind_cols(extract.rwp.ne(ne_hosd3_1u, ne_hosd3_1m), d="Three nearest")
hosd4 <- bind_cols(extract.rwp.ne(ne_hosd4_1u, ne_hosd4_1m), d="Four nearest")


app.a <- bind_rows(schd1, schd2, schd3, schd4, 
                   hosd1, hosd2, hosd3, hosd4 
) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         Model=factor(Model, levels = c( "Unmatched", "Matched")),
         d=factor(d, levels = c("Nearest", "Two nearest", "Three nearest", "Four nearest")),
         shapen=ifelse(d=="Nearest", 1,
                       ifelse(d=="Two nearest", 0, 
                              ifelse(d=="Three nearest", 2,
                                     ifelse(d=="Four nearest", 5, NA))))
         
  ) %>% 
  arrange(by_group=d)

ggplot(data = subset(app.a), aes(x=time, y=att, group = d, colour = d, shape = d))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(position=position_dodge(width=0.5))+
  scale_shape_manual(values = c(15, 16, 17, 18))+
  scale_colour_manual(values = c("#cccccc", "#969696", "#525252", "black")) +
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(vars(Model), vars(elec), scales="free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in support for right-wing populists \n (percentage points)")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))

ggsave("Figure_A2.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)

##Figure A3####

incdata_le_schd1_l1_1 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 1.5)[[1]]
incdata_le_schd1_h1_1 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 1.5)[[2]]
incdata_le_schd1_l2_1 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 1.5)[[3]]
incdata_le_schd1_h2_1 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 1.5)[[4]]
incdata_le_schd1_l1_2 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 2.5)[[1]]
incdata_le_schd1_h1_2 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 2.5)[[2]]
incdata_le_schd1_l2_2 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 2.5)[[3]]
incdata_le_schd1_h2_2 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 2.5)[[4]]
incdata_le_schd1_l1_3 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 3.5)[[1]]
incdata_le_schd1_h1_3 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 3.5)[[2]]
incdata_le_schd1_l2_3 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 3.5)[[3]]
incdata_le_schd1_h2_3 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 3.5)[[4]]
incdata_le_schd1_l1_4 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 4.5)[[1]]
incdata_le_schd1_h1_4 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 4.5)[[2]]
incdata_le_schd1_l2_4 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 4.5)[[3]]
incdata_le_schd1_h2_4 <- clear_and_match_inc_le_dist(incdata_le_schd1, "schd1", 4.5)[[4]]

#le
Model1.1.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                     gname = "schd1_g", xformla = ~ 1,
                     data = subset(incdata_le_schd1_l1_1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                     gname = "schd1_g", xformla = ~ 1,
                     data = subset(incdata_le_schd1_h1_1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                     gname = "schd1_g", xformla = ~ 1,
                     data = subset(incdata_le_schd1_l2_1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                     gname = "schd1_g", xformla = ~ 1,
                     data = subset(incdata_le_schd1_h2_1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                      gname = "schd1_g", xformla = ~ 1,
                      data = subset(incdata_le_schd1_l1_1, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                      gname = "schd1_g", xformla = ~ 1,
                      data = subset(incdata_le_schd1_h1_1, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                      gname = "schd1_g", xformla = ~ 1,
                      data = subset(incdata_le_schd1_l2_1, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                      gname = "schd1_g", xformla = ~ 1,
                      data = subset(incdata_le_schd1_h2_1, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


Model1.1.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_l1_2), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_h1_2), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_l2_2), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_h2_2), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_l1_2, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_h1_2, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_l2_2, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_h2_2, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model1.1.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_l1_3), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_h1_3), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_l2_3), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_h2_3), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_l1_3, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_h1_3, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_l2_3, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_h2_3, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

Model1.1.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_l1_4), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_h1_4), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_l2_4), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                       gname = "schd1_g", xformla = ~ 1,
                       data = subset(incdata_le_schd1_h2_4), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_l1_4, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.1m.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_h1_4, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_l2_4, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model1.2m.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid", 
                        gname = "schd1_g", xformla = ~ 1,
                        data = subset(incdata_le_schd1_h2_4, match_s==1), est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

le.low_um_1 <- bind_cols(extract.inc.le(Model1.1.l_1, Model1.2.l_1), Model="Unmatched", deltadist="Low", elec="Local Election:\n School Closure", threshold =1.5)
le.hig_um_1 <- bind_cols(extract.inc.le(Model1.1.h_1, Model1.2.h_1), Model="Unmatched", deltadist="High",  elec="Local Election:\n School Closure", threshold =1.5)
le.low_ma_1 <- bind_cols(extract.inc.le(Model1.1m.l_1, Model1.2m.l_1), Model="Matched", deltadist="Low", elec="Local Election:\n School Closure", threshold =1.5)
le.hig_ma_1 <- bind_cols(extract.inc.le(Model1.1m.h_1, Model1.2m.h_1), Model="Matched", deltadist="High",  elec="Local Election:\n School Closure", threshold =1.5)

le.low_um_2 <- bind_cols(extract.inc.le(Model1.1.l_2, Model1.2.l_2), Model="Unmatched", deltadist="Low", elec="Local Election:\n School Closure", threshold =2.5)
le.hig_um_2 <- bind_cols(extract.inc.le(Model1.1.h_2, Model1.2.h_2), Model="Unmatched", deltadist="High",  elec="Local Election:\n School Closure", threshold =2.5)
le.low_ma_2 <- bind_cols(extract.inc.le(Model1.1m.l_2, Model1.2m.l_2), Model="Matched", deltadist="Low", elec="Local Election:\n School Closure", threshold =2.5)
le.hig_ma_2 <- bind_cols(extract.inc.le(Model1.1m.h_2, Model1.2m.h_2), Model="Matched", deltadist="High",  elec="Local Election:\n School Closure", threshold =2.5)

le.low_um_3 <- bind_cols(extract.inc.le(Model1.1.l_3, Model1.2.l_3), Model="Unmatched", deltadist="Low", elec="Local Election:\n School Closure", threshold =3.5)
le.hig_um_3 <- bind_cols(extract.inc.le(Model1.1.h_3, Model1.2.h_3), Model="Unmatched", deltadist="High",  elec="Local Election:\n School Closure", threshold =3.5)
le.low_ma_3 <- bind_cols(extract.inc.le(Model1.1m.l_3, Model1.2m.l_3), Model="Matched", deltadist="Low", elec="Local Election:\n School Closure", threshold =3.5)
le.hig_ma_3 <- bind_cols(extract.inc.le(Model1.1m.h_3, Model1.2m.h_3), Model="Matched", deltadist="High",  elec="Local Election:\n School Closure", threshold =3.5)

le.low_um_4 <- bind_cols(extract.inc.le(Model1.1.l_4, Model1.2.l_4), Model="Unmatched", deltadist="Low", elec="Local Election:\n School Closure", threshold =4.5)
le.hig_um_4 <- bind_cols(extract.inc.le(Model1.1.h_4, Model1.2.h_4), Model="Unmatched", deltadist="High",  elec="Local Election:\n School Closure", threshold =4.5)
le.low_ma_4 <- bind_cols(extract.inc.le(Model1.1m.l_4, Model1.2m.l_4), Model="Matched", deltadist="Low", elec="Local Election:\n School Closure", threshold =4.5)
le.hig_ma_4 <- bind_cols(extract.inc.le(Model1.1m.h_4, Model1.2m.h_4), Model="Matched", deltadist="High",  elec="Local Election:\n School Closure", threshold =4.5)

thresholdplot <- bind_rows(le.low_um_1, le.hig_um_1, le.low_ma_1, le.hig_ma_1,
                           le.low_um_2, le.hig_um_2, le.low_ma_2, le.hig_ma_2,
                           le.low_um_3, le.hig_um_3, le.low_ma_3, le.hig_ma_3,
                           le.low_um_4, le.hig_um_4, le.low_ma_4, le.hig_ma_4) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         deltadist=factor(deltadist, levels = c("Low", "High"), labels = c("Low Severity", "High Severity")),
         Model  = factor(Model ,levels = c("Unmatched", "Matched")),
         threshold = factor(threshold, levels = c(1.5, 2.5, 3.5, 4.5), labels = c("1.5 km", "2.5 km", "3.5 km", "4.5 km")),
         sig = ifelse(conf.low> 0 & conf.high >0 |conf.low<0 & conf.high<0, "*", "")
  )


ggplot(data = thresholdplot, aes(x=time, y=threshold)) + 
  facet_grid(vars(Model), vars(deltadist), scales="free_x")+
  geom_tile(aes(fill = att)) + 
  geom_text(aes(label = paste(ifelse(is.na(att), "ref", round(att, 1)), ifelse(is.na(att), "", sig),  sep = "")), color = "black", vjust=-0.5, size = 2.5) +
  geom_text(aes(label = ifelse(is.na(att), "", paste("(", round(se, 1), ")", sep = ""))), color = "black", vjust = 1, size = 2.5) +
  scale_fill_gradient2(na.value = "white", mid = "white", low = "#636363",  high = "#636363")+
  labs(x = "Period in relation to closure", y = "Severity threshold")+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))                 

ggsave("Figure_A3.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)
##Figure A4####
incdata_ne_hosd1_l1_1 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 8)[[1]]
incdata_ne_hosd1_h1_1 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 8)[[2]]
incdata_ne_hosd1_l2_1 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 8)[[3]]
incdata_ne_hosd1_h2_1 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 8)[[4]]
incdata_ne_hosd1_l3_1 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 8)[[5]]
incdata_ne_hosd1_h3_1 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 8)[[6]]

incdata_ne_hosd1_l1_2 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[1]]
incdata_ne_hosd1_h1_2 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[2]]
incdata_ne_hosd1_l2_2 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[3]]
incdata_ne_hosd1_h2_2 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[4]]
incdata_ne_hosd1_l3_2 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[5]]
incdata_ne_hosd1_h3_2 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 10)[[6]]

incdata_ne_hosd1_l1_3 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 12)[[1]]
incdata_ne_hosd1_h1_3 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 12)[[2]]
incdata_ne_hosd1_l2_3 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 12)[[3]]
incdata_ne_hosd1_h2_3 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 12)[[4]]
incdata_ne_hosd1_l3_3 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 12)[[5]]
incdata_ne_hosd1_h3_3 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 12)[[6]]

incdata_ne_hosd1_l1_4 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 14)[[1]]
incdata_ne_hosd1_h1_4 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 14)[[2]]
incdata_ne_hosd1_l2_4 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 14)[[3]]
incdata_ne_hosd1_h2_4 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 14)[[4]]
incdata_ne_hosd1_l3_4 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 14)[[5]]
incdata_ne_hosd1_h3_4 <- clear_and_match_inc_ne_dist(incdata_ne_hosd1, "hosd1", 14)[[6]]

summary(subset(incdata_ne_hosd1, hosd1_deltadist!=0)$hosd1_deltadist)

##ne
Model2.1.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_l1_1),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_h1_1),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_l2_1),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_h2_1),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_l3_1),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                     gname = "hosd1_g",
                     xformla = ~ 1, 
                     data = subset(incdata_ne_hosd1_h3_1),
                     est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


Model2.1m.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_l1_1, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1m.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_h1_1, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_l2_1, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_h2_1, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.l_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_l3_1, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.h_1 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                      gname = "hosd1_g",
                      xformla = ~ 1, 
                      data = subset(incdata_ne_hosd1_h3_1, match_s==1),
                      est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )

ne.low_um_1 <- bind_cols(extract.inc.ne(Model2.1.l_1, Model2.2.l_1, Model2.3.l_1), Model="Unmatched", deltadist="Low", elec="National Election:\n Hospital Closure", threshold =8)
ne.hig_um_1 <- bind_cols(extract.inc.ne(Model2.1.h_1, Model2.2.h_1, Model2.3.h_1), Model="Unmatched", deltadist="High",  elec="National Election:\n Hospital Closure", threshold =8)
ne.low_ma_1 <- bind_cols(extract.inc.ne(Model2.1m.l_1, Model2.2m.l_1, Model2.3m.l_1), Model="Matched", deltadist="Low", elec="National Election:\n Hospital Closure", threshold =8)
ne.hig_ma_1 <- bind_cols(extract.inc.ne(Model2.1m.h_1, Model2.2m.h_1, Model2.3m.h_1), Model="Matched", deltadist="High",  elec="National Election:\n Hospital Closure", threshold =8)

Model2.1.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l1_2),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h1_2),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l2_2),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h2_2),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l3_2),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h3_2),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


Model2.1m.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l1_2, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1m.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h1_2, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l2_2, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h2_2, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.l_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l3_2, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.h_2 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h3_2, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )



ne.low_um_2 <- bind_cols(extract.inc.ne(Model2.1.l_2, Model2.2.l_2, Model2.3.l_2), Model="Unmatched", deltadist="Low", elec="National Election:\n Hospital Closure", threshold =10)
ne.hig_um_2 <- bind_cols(extract.inc.ne(Model2.1.h_2, Model2.2.h_2, Model2.3.h_2), Model="Unmatched", deltadist="High",  elec="National Election:\n Hospital Closure", threshold =10)
ne.low_ma_2 <- bind_cols(extract.inc.ne(Model2.1m.l_2, Model2.2m.l_2, Model2.3m.l_2), Model="Matched", deltadist="Low", elec="National Election:\n Hospital Closure", threshold =10)
ne.hig_ma_2 <- bind_cols(extract.inc.ne(Model2.1m.h_2, Model2.2m.h_2, Model2.3m.h_2), Model="Matched", deltadist="High",  elec="National Election:\n Hospital Closure", threshold =10)


Model2.1.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l1_3),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h1_3),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l2_3),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h2_3),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l3_3),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h3_3),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


Model2.1m.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l1_3, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1m.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h1_3, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l2_3, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h2_3, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.l_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l3_3, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.h_3 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h3_3, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )



ne.low_um_3 <- bind_cols(extract.inc.ne(Model2.1.l_3, Model2.2.l_3, Model2.3.l_3), Model="Unmatched", deltadist="Low", elec="National Election:\n Hospital Closure", threshold =12)
ne.hig_um_3 <- bind_cols(extract.inc.ne(Model2.1.h_3, Model2.2.h_3, Model2.3.h_3), Model="Unmatched", deltadist="High",  elec="National Election:\n Hospital Closure", threshold =12)
ne.low_ma_3 <- bind_cols(extract.inc.ne(Model2.1m.l_3, Model2.2m.l_3, Model2.3m.l_3), Model="Matched", deltadist="Low", elec="National Election:\n Hospital Closure", threshold =12)
ne.hig_ma_3 <- bind_cols(extract.inc.ne(Model2.1m.h_3, Model2.2m.h_3, Model2.3m.h_3), Model="Matched", deltadist="High",  elec="National Election:\n Hospital Closure", threshold =12)


Model2.1.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l1_4),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h1_4),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l2_4),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h2_4),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_l3_4),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                       gname = "hosd1_g",
                       xformla = ~ 1, 
                       data = subset(incdata_ne_hosd1_h3_4),
                       est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )


Model2.1m.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l1_4, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.1m.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h1_4, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l2_4, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.2m.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h2_4, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.l_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_l3_4, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )
Model2.3m.h_4 <- att_gt(yname = "inc_sup", tname = "year", idname = "afstemid",
                        gname = "hosd1_g",
                        xformla = ~ 1, 
                        data = subset(incdata_ne_hosd1_h3_4, match_s==1),
                        est_method = "dr", allow_unbalanced_panel = F,control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid", cband = T, bstrap = T )



ne.low_um_4 <- bind_cols(extract.inc.ne(Model2.1.l_4, Model2.2.l_4, Model2.3.l_4), Model="Unmatched", deltadist="Low", elec="National Election:\n Hospital Closure", threshold =14)
ne.hig_um_4 <- bind_cols(extract.inc.ne(Model2.1.h_4, Model2.2.h_4, Model2.3.h_4), Model="Unmatched", deltadist="High",  elec="National Election:\n Hospital Closure", threshold =14)
ne.low_ma_4 <- bind_cols(extract.inc.ne(Model2.1m.l_4, Model2.2m.l_4, Model2.3m.l_4), Model="Matched", deltadist="Low", elec="National Election:\n Hospital Closure", threshold =14)
ne.hig_ma_4 <- bind_cols(extract.inc.ne(Model2.1m.h_4, Model2.2m.h_4, Model2.3m.h_4), Model="Matched", deltadist="High",  elec="National Election:\n Hospital Closure", threshold =14)


thresholdplot <- bind_rows(ne.low_um_1, ne.hig_um_1, ne.low_ma_1, ne.hig_ma_1,
                           ne.low_um_2, ne.hig_um_2, ne.low_ma_2, ne.hig_ma_2,
                           ne.low_um_3, ne.hig_um_3, ne.low_ma_3, ne.hig_ma_3,
                           ne.low_um_4, ne.hig_um_4, ne.low_ma_4, ne.hig_ma_4) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         deltadist=factor(deltadist, levels = c("Low", "High"), labels = c("Low Severity", "High Severity")),
         Model  = factor(Model ,levels = c("Unmatched", "Matched")),
         threshold = factor(threshold, levels = c(8, 10, 12, 14), labels = c("8 km", "10 km", "12 km", "14 km")),
         sig = ifelse(conf.low> 0 & conf.high >0 |conf.low<0 & conf.high<0, "*", "")
  )


ggplot(data = thresholdplot, aes(x=time, y=threshold)) + 
  facet_grid(vars(Model), vars(deltadist), scales="free_x")+
  geom_tile(aes(fill = att)) + 
  geom_text(aes(label = paste(ifelse(is.na(att), "ref", round(att, 1)), ifelse(is.na(att), "", sig),  sep = "")), color = "black", vjust=-0.5, size = 2.5) +
  geom_text(aes(label = ifelse(is.na(att), "", paste("(", round(se, 1), ")", sep = ""))), color = "black", vjust = 1, size = 2.5) +
  scale_fill_gradient2(na.value = "white", mid = "white", low = "#636363",  high = "#636363")+
  labs(x = "Period in relation to closure", y = "Severity threshold")+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))                 

ggsave("Figure_A4.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)

##Figure A5####
rwpdata_le_schd1_l_1 <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 1.5)[[1]]
rwpdata_le_schd1_h_1 <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 1.5)[[2]]
rwpdata_le_schd1_l_2 <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 2.5)[[1]]
rwpdata_le_schd1_h_2 <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 2.5)[[2]]
rwpdata_le_schd1_l_3 <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 3.5)[[1]]
rwpdata_le_schd1_h_3 <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 3.5)[[2]]
rwpdata_le_schd1_l_4 <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 4.5)[[1]]
rwpdata_le_schd1_h_4 <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 4.5)[[2]]

rwp_le_l_1 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "schd1_g", data = subset(rwpdata_le_schd1_l_1)
)
rwp_le_h_1 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "schd1_g", data = subset(rwpdata_le_schd1_h_1)
)
rwp_le_l.m_1 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_l_1, match_s==1)
)
rwp_le_h.m_1 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_h_1, match_s==1)
)
rwp_le_l_2 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_l_2)
)
rwp_le_h_2 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_h_2)
)
rwp_le_l.m_2 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "schd1_g", data = subset(rwpdata_le_schd1_l_2, match_s==1)
)
rwp_le_h.m_2 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "schd1_g", data = subset(rwpdata_le_schd1_h_2, match_s==1)
)

rwp_le_l_3 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_l_3)
)
rwp_le_h_3 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_h_3)
)
rwp_le_l.m_3 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "schd1_g", data = subset(rwpdata_le_schd1_l_3, match_s==1)
)
rwp_le_h.m_3 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "schd1_g", data = subset(rwpdata_le_schd1_h_3, match_s==1)
)
rwp_le_l_4 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_l_4)
)
rwp_le_h_4 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_h_4)
)
rwp_le_l.m_4 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "schd1_g", data = subset(rwpdata_le_schd1_l_4, match_s==1)
)
rwp_le_h.m_4 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "schd1_g", data = subset(rwpdata_le_schd1_h_4, match_s==1)
)

le.low_1 <- bind_cols(extract.rwp.le(rwp_le_l_1, rwp_le_l.m_1), deltadist="Low", threshold =1.5)
le.hig_1 <- bind_cols(extract.rwp.le(rwp_le_h_1, rwp_le_h.m_1), deltadist="High", threshold =1.5)
le.low_2 <- bind_cols(extract.rwp.le(rwp_le_l_2, rwp_le_l.m_2), deltadist="Low", threshold =2.5)
le.hig_2 <- bind_cols(extract.rwp.le(rwp_le_h_2, rwp_le_h.m_2), deltadist="High", threshold =2.5)
le.low_3 <- bind_cols(extract.rwp.le(rwp_le_l_3, rwp_le_l.m_3), deltadist="Low", threshold =3.5)
le.hig_3 <- bind_cols(extract.rwp.le(rwp_le_h_3, rwp_le_h.m_3), deltadist="High", threshold =3.5)
le.low_4 <- bind_cols(extract.rwp.le(rwp_le_l_4, rwp_le_l.m_4), deltadist="Low", threshold =4.5)
le.hig_4 <- bind_cols(extract.rwp.le(rwp_le_h_4, rwp_le_h.m_4), deltadist="High", threshold =4.5)

thresholdplot <- bind_rows(le.low_1, le.hig_1, 
                           le.low_2, le.hig_2, 
                           le.low_3, le.hig_3, 
                           le.low_4, le.hig_4) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         deltadist=factor(deltadist, levels = c("Low", "High"), labels = c("Low Severity", "High Severity")),
         Model  = factor(Model ,levels = c("Unmatched", "Matched")),
         threshold = factor(threshold, levels = c(1.5, 2.5, 3.5, 4.5), labels = c("1.5 km", "2.5 km", "3.5 km", "4.5 km")),
         sig = ifelse(conf.low> 0 & conf.high >0 |conf.low<0 & conf.high<0, "*", "")
  )

ggplot(data = thresholdplot, aes(x=time, y=threshold)) + 
  facet_grid(vars(Model), vars(deltadist), scales="free_x")+
  geom_tile(aes(fill = att)) + 
  geom_text(aes(label = paste(ifelse(is.na(att), "ref", round(att, 1)), ifelse(is.na(att), "", sig),  sep = "")), color = "black", vjust=-0.5, size = 2.5) +
  geom_text(aes(label = ifelse(is.na(att), "", paste("(", round(se, 1), ")", sep = ""))), color = "black", vjust = 1, size = 2.5) +
  scale_fill_gradient2(na.value = "white", mid = "white", low = "#636363",  high = "#636363")+
  labs(x = "Period in relation to closure", y = "Severity threshold")+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))                 

ggsave("Figure_A5.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)
##Figure A6####
rwpdata_ne_hosd1_l_1 <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 8)[[1]]
rwpdata_ne_hosd1_h_1 <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 8)[[2]]
rwpdata_ne_hosd1_l_2 <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 10)[[1]]
rwpdata_ne_hosd1_h_2 <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 10)[[2]]
rwpdata_ne_hosd1_l_3 <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 12)[[1]]
rwpdata_ne_hosd1_h_3 <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 12)[[2]]
rwpdata_ne_hosd1_l_4 <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 14)[[1]]
rwpdata_ne_hosd1_h_4 <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 14)[[2]]

rwp_ne_l_1 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l_1)
)
rwp_ne_l.m_1 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l_1, match_s==1)
)
rwp_ne_h_1 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h_1)
)
rwp_ne_h.m_1 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h_1, match_s==1)
)

rwp_ne_l_2 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l_2)
)
rwp_ne_l.m_2 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l_2, match_s==1)
)
rwp_ne_h_2 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h_2)
)
rwp_ne_h.m_2 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h_2, match_s==1)
)
rwp_ne_l_3 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l_3)
)
rwp_ne_l.m_3 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l_3, match_s==1)
)
rwp_ne_h_3 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h_3)
)
rwp_ne_h.m_3 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h_3, match_s==1)
)
rwp_ne_l_4 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l_4)
)
rwp_ne_l.m_4 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l_4, match_s==1)
)
rwp_ne_h_4 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h_4)
)
rwp_ne_h.m_4 <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                       gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h_4, match_s==1)
)

ne.low_1 <- bind_cols(extract.rwp.ne(rwp_ne_l_1, rwp_ne_l.m_1), deltadist="Low", threshold =8)
ne.hig_1 <- bind_cols(extract.rwp.ne(rwp_ne_h_1, rwp_ne_h.m_1), deltadist="High", threshold =8)
ne.low_2 <- bind_cols(extract.rwp.ne(rwp_ne_l_2, rwp_ne_l.m_2), deltadist="Low", threshold =10)
ne.hig_2 <- bind_cols(extract.rwp.ne(rwp_ne_h_2, rwp_ne_h.m_2), deltadist="High", threshold =10)
ne.low_3 <- bind_cols(extract.rwp.ne(rwp_ne_l_3, rwp_ne_l.m_3), deltadist="Low", threshold =12)
ne.hig_3 <- bind_cols(extract.rwp.ne(rwp_ne_h_3, rwp_ne_h.m_3), deltadist="High", threshold =12)
ne.low_4 <- bind_cols(extract.rwp.ne(rwp_ne_l_4, rwp_ne_l.m_4), deltadist="Low", threshold =14)
ne.hig_4 <- bind_cols(extract.rwp.ne(rwp_ne_h_4, rwp_ne_h.m_4), deltadist="High", threshold =14)

thresholdplot <- bind_rows(ne.low_1, ne.hig_1, 
                           ne.low_2, ne.hig_2, 
                           ne.low_3, ne.hig_3, 
                           ne.low_4, ne.hig_4) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         deltadist=factor(deltadist, levels = c("Low", "High"), labels = c("Low Severity", "High Severity")),
         Model  = factor(Model ,levels = c("Unmatched", "Matched")),
         threshold = factor(threshold, levels = c(8, 10, 12, 14), labels = c("8 km", "10 km", "12 km", "14 km")),
         sig = ifelse(conf.low> 0 & conf.high >0 |conf.low<0 & conf.high<0, "*", "")
  )

ggplot(data = thresholdplot, aes(x=time, y=threshold)) + 
  facet_grid(vars(Model), vars(deltadist), scales="free_x")+
  geom_tile(aes(fill = att)) + 
  geom_text(aes(label = paste(ifelse(is.na(att), "ref", round(att, 1)), ifelse(is.na(att), "", sig),  sep = "")), color = "black", vjust=-0.5, size = 2.5) +
  geom_text(aes(label = ifelse(is.na(att), "", paste("(", round(se, 1), ")", sep = ""))), color = "black", vjust = 1, size = 2.5) +
  scale_fill_gradient2(na.value = "white", mid = "white", low = "#636363",  high = "#636363")+
  labs(x = "Period in relation to closure", y = "Severity threshold")+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))                 

ggsave("Figure_A6.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)

#Appendix B####
##Figure B1####
ul_le_schp_1u <- att_gt(yname = "ul_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                        gname = "schp_g", data = subset(rwpdata_le_schp_tab)# match_s==1)
)
ul_le_schp_1m <- att_gt(yname = "ul_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                        gname = "schp_g", data = subset(rwpdata_le_schp_tab,  match_s==1)
)
ul_ne_hosd1_1u <- att_gt(yname = "ul_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                        gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab)# match_s==1)
)
ul_ne_hosd1_1m <- att_gt(yname = "ul_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                        gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab,  match_s==1)
)
spp_le_schp_1u <- att_gt(yname = "spp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                        gname = "schp_g", data = subset(rwpdata_le_schp_tab)# match_s==1)
)
spp_le_schp_1m <- att_gt(yname = "spp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                        gname = "schp_g", data = subset(rwpdata_le_schp_tab,  match_s==1)
)
spp_ne_hosd1_1u <- att_gt(yname = "spp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab)# match_s==1)
)
spp_ne_hosd1_1m <- att_gt(yname = "spp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab,  match_s==1)
)
socdem_le_schp_1u <- att_gt(yname = "socdem_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_tab)# match_s==1)
)
socdem_le_schp_1m <- att_gt(yname = "socdem_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_tab,  match_s==1)
)
socdem_ne_hosd1_1u <- att_gt(yname = "socdem_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab)# match_s==1)
)
socdem_ne_hosd1_1m <- att_gt(yname = "socdem_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab,  match_s==1)
)
rlib_le_schp_1u <- att_gt(yname = "rlib_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                            gname = "schp_g", data = subset(rwpdata_le_schp_tab)# match_s==1)
)
rlib_le_schp_1m <- att_gt(yname = "rlib_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                            gname = "schp_g", data = subset(rwpdata_le_schp_tab,  match_s==1)
)
rlib_ne_hosd1_1u <- att_gt(yname = "rlib_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                             gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab)# match_s==1)
)
rlib_ne_hosd1_1m <- att_gt(yname = "rlib_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                             gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab,  match_s==1)
)
lib_le_schp_1u <- att_gt(yname = "lib_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "schp_g", data = subset(rwpdata_le_schp_tab)# match_s==1)
)
lib_le_schp_1m <- att_gt(yname = "lib_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "schp_g", data = subset(rwpdata_le_schp_tab,  match_s==1)
)
lib_ne_hosd1_1u <- att_gt(yname = "lib_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                           gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab)# match_s==1)
)
lib_ne_hosd1_1m <- att_gt(yname = "lib_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                           gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab,  match_s==1)
)
cons_le_schp_1u <- att_gt(yname = "cons_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_tab)# match_s==1)
)
cons_le_schp_1m <- att_gt(yname = "cons_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_tab,  match_s==1)
)
cons_ne_hosd1_1u <- att_gt(yname = "cons_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                           gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab)# match_s==1)
)
cons_ne_hosd1_1m <- att_gt(yname = "cons_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                           gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab,  match_s==1)
)
rwp_le_schp_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "schp_g", data = subset(rwpdata_le_schp_tab)# match_s==1)
)
rwp_le_schp_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "schp_g", data = subset(rwpdata_le_schp_tab,  match_s==1)
)
rwp_ne_hosd1_1u <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                           gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab)# match_s==1)
)
rwp_ne_hosd1_1m <- att_gt(yname = "rwp_sup", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                           gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_tab,  match_s==1)
)
partytab1 <- bind_rows(
bind_cols(extract.rwp.le(ul_le_schp_1u, ul_le_schp_1m), party="Unity List"),
bind_cols(extract.rwp.le(spp_le_schp_1u, spp_le_schp_1m), party="Socialist People's Party"),
bind_cols(extract.rwp.le(socdem_le_schp_1u, socdem_le_schp_1m), party="Social Democrats"),
bind_cols(extract.rwp.le(rlib_le_schp_1u, rlib_le_schp_1m), party="Radical Liberals"),
bind_cols(extract.rwp.le(lib_le_schp_1u, lib_le_schp_1m), party="Liberals"),
bind_cols(extract.rwp.le(cons_le_schp_1u, cons_le_schp_1m), party="Conservatives"),
bind_cols(extract.rwp.le(rwp_le_schp_1u, rwp_le_schp_1m), party="Right-Wing Populists"),

bind_cols(extract.rwp.ne(ul_ne_hosd1_1u, ul_ne_hosd1_1m), party="Unity List"),
bind_cols(extract.rwp.ne(spp_ne_hosd1_1u, spp_ne_hosd1_1m), party="Socialist People's Party"),
bind_cols(extract.rwp.ne(socdem_ne_hosd1_1u, socdem_ne_hosd1_1m), party="Social Democrats"),
bind_cols(extract.rwp.ne(rlib_ne_hosd1_1u, rlib_ne_hosd1_1m), party="Radical Liberals"),
bind_cols(extract.rwp.ne(lib_ne_hosd1_1u, lib_ne_hosd1_1m), party="Liberals"),
bind_cols(extract.rwp.ne(cons_ne_hosd1_1u, cons_ne_hosd1_1m), party="Conservatives"),
bind_cols(extract.rwp.ne(rwp_ne_hosd1_1u, rwp_ne_hosd1_1m), party="Right-Wing Populists")

) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         party=factor(party, levels=c("Right-Wing Populists","Conservatives","Liberals","Radical Liberals","Social Democrats","Socialist People's Party", "Unity List")),
         Model=factor(Model, levels = c( "Unmatched", "Matched"))
         
  )
         

appb1 <- ggplot(partytab1, aes(y=party, x=time ))+
  facet_grid(vars(Model), vars(elec), scales="free_x")+
  geom_tile(aes(fill = att)) + 
  geom_text(aes(label = paste(ifelse(is.na(att), "ref", round(att, 1)), ifelse(is.na(att), "", sig),  sep = "")), color = "black", vjust=-0.5, size = 2.5) +
  geom_text(aes(label = ifelse(is.na(att), "", paste("(", round(se, 1), ")", sep = ""))), color = "black", vjust = 1, size = 2.5) +
  scale_fill_gradient2(na.value = "white", mid = "white", low = "#636363",  high = "#636363")+
  labs(x = "Period in relation to closure", y = "")+
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))

ggsave("Figure_B1.eps" , plot = appb1, 
       scale = 1, width = 160 , height = 140 , units = c("mm"),
       dpi = 1000)

#Appendix C####
##Table C1####
tabc <- tibble()
tabc[1:16,1] <- c("Variable name", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "Affected precincts", "Unaffected precincts", "Num. obs.")
tabc[1:16,2] <- c("09-13", "", "","", "", "ref", "", 
                  paste0(round(Model1.1$att[2]*100,2), ifelse(Model1.1$att[2]+Model1.1$se[2]*Model1.1$c>0 & Model1.1$att[2]-Model1.1$se[2]*Model1.1$c>0 | Model1.1$att[2]+Model1.1$se[2]*Model1.1$c<0 & Model1.1$att[2]-Model1.1$se[2]*Model1.1$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model1.1$se[2]*100,2), ")"),
                  paste0(round(Model1.1$att[3]*100,2), ifelse(Model1.1$att[3]+Model1.1$se[3]*Model1.1$c>0 & Model1.1$att[3]-Model1.1$se[3]*Model1.1$c>0 | Model1.1$att[3]+Model1.1$se[3]*Model1.1$c<0 & Model1.1$att[3]-Model1.1$se[3]*Model1.1$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model1.1$se[3]*100,2),")"), "", "",
                  matrix(table(subset(Model1.1$DIDparams$data)$schp_g)/3)[2,1],
                  matrix(table(subset(Model1.1$DIDparams$data)$schp_g)/3)[1,1],
                  Model1.1$n*length(Model1.1$t))
tabc[1:16,3] <- c("13-17", "", "",
                  paste0(round(Model1.2$att[1]*100,2), ifelse(Model1.2$att[1]+Model1.2$se[1]*Model1.2$c>0 & Model1.2$att[1]-Model1.2$se[1]*Model1.2$c>0 | Model1.2$att[1]+Model1.2$se[1]*Model1.2$c<0 & Model1.2$att[1]-Model1.2$se[1]*Model1.2$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model1.2$se[1]*100,2),")"),
                  "ref", "", 
                  paste0(round(Model1.2$att[3]*100,2), ifelse(Model1.2$att[3]+Model1.2$se[3]*Model1.2$c>0 & Model1.2$att[3]-Model1.2$se[3]*Model1.2$c>0 | Model1.2$att[3]+Model1.2$se[3]*Model1.2$c<0 & Model1.2$att[3]-Model1.2$se[3]*Model1.2$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model1.2$se[3]*100,2), ")"),"", "", "", "",
                  matrix(table(subset(Model1.2$DIDparams$data)$schp_g)/3)[2,1],
                  matrix(table(subset(Model1.2$DIDparams$data)$schp_g)/3)[1,1],
                  Model1.2$n*length(Model1.2$t))

tabc[1:16,4] <- c("Meta", "", "",
                  paste0(round(try1$att[1],2), ifelse(try1$conf.low[1]>0 & try1$conf.high[1]>0 | try1$conf.low[1]<0 & try1$conf.high[1]<0 , "$^{*}$", "")),
                  paste0("(",round(try1$se[1],2),")"),
                  "ref", "", 
                  paste0(round(try1$att[3],2), ifelse(try1$conf.low[3]>0 & try1$conf.high[3]>0 | try1$conf.low[3]<0 & try1$conf.high[3]<0 , "$^{*}$", "")),
                  paste0("(",round(try1$se[3],2),")"),
                  paste0(round(try1$att[4],2), ifelse(try1$conf.low[4]>0 & try1$conf.high[4]>0 | try1$conf.low[4]<0 & try1$conf.high[4]<0 , "$^{*}$", "")),
                  paste0("(",round(try1$se[4],2),")"), "", "", "-", "-", "-")

tabc[1:16,5] <- c("09-13", "", "","", "", "ref", "", 
                  paste0(round(Model1.1m$att[2]*100,2), ifelse(Model1.1m$att[2]+Model1.1m$se[2]*Model1.1m$c>0 & Model1.1m$att[2]-Model1.1m$se[2]*Model1.1m$c>0 | Model1.1m$att[2]+Model1.1m$se[2]*Model1.1m$c<0 & Model1.1m$att[2]-Model1.1m$se[2]*Model1.1m$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model1.1m$se[2]*100,2), ")"),
                  paste0(round(Model1.1m$att[3]*100,2), ifelse(Model1.1m$att[3]+Model1.1m$se[3]*Model1.1m$c>0 & Model1.1m$att[3]-Model1.1m$se[3]*Model1.1m$c>0 | Model1.1m$att[3]+Model1.1m$se[3]*Model1.1m$c<0 & Model1.1m$att[3]-Model1.1m$se[3]*Model1.1m$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model1.1m$se[3]*100,2),")"), "", "",
                  matrix(table(subset(Model1.1m$DIDparams$data)$schp_g)/3)[2,1],
                  matrix(table(subset(Model1.1m$DIDparams$data)$schp_g)/3)[1,1],
                  Model1.1m$n*length(Model1.1m$t))
tabc[1:16,6] <- c("13-17", "", "",
                  paste0(round(Model1.2m$att[1]*100,2), ifelse(Model1.2m$att[1]+Model1.2m$se[1]*Model1.2m$c>0 & Model1.2m$att[1]-Model1.2m$se[1]*Model1.2m$c>0 | Model1.2m$att[1]+Model1.2m$se[1]*Model1.2m$c<0 & Model1.2m$att[1]-Model1.2m$se[1]*Model1.2m$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model1.2m$se[1]*100,2),")"),
                  "ref", "", 
                  paste0(round(Model1.2m$att[3]*100,2), ifelse(Model1.2m$att[3]+Model1.2m$se[3]*Model1.2m$c>0 & Model1.2m$att[3]-Model1.2m$se[3]*Model1.2m$c>0 | Model1.2m$att[3]+Model1.2m$se[3]*Model1.2m$c<0 & Model1.2m$att[3]-Model1.2m$se[3]*Model1.2m$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model1.2m$se[3]*100,2), ")"),"", "", "", "",
                  matrix(table(subset(Model1.2m$DIDparams$data)$schp_g)/3)[2,1],
                  matrix(table(subset(Model1.2m$DIDparams$data)$schp_g)/3)[1,1],
                  Model1.2m$n*length(Model1.2m$t))

tabc[1:16,7] <- c("Meta", "", "",
                  paste0(round(try2$att[1],2), ifelse(try2$conf.low[1]>0 & try2$conf.high[1]>0 | try2$conf.low[1]<0 & try2$conf.high[1]<0 , "$^{*}$", "")),
                  paste0("(",round(try2$se[1],2),")"),
                  "ref", "", 
                  paste0(round(try2$att[3],2), ifelse(try2$conf.low[3]>0 & try2$conf.high[3]>0 | try2$conf.low[3]<0 & try2$conf.high[3]<0 , "$^{*}$", "")),
                  paste0("(",round(try2$se[3],2),")"),
                  paste0(round(try2$att[4],2), ifelse(try2$conf.low[4]>0 & try2$conf.high[4]>0 | try2$conf.low[4]<0 & try2$conf.high[4]<0 , "$^{*}$", "")),
                  paste0("(",round(try2$se[4],2),")"), "", "", "-", "-", "-")

xtable(tabc)
##Table C2####
tabc2 <- tibble()
tabc2[1:16,1] <- c("Treated year:", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "Affected precincts", "Unaffected precincts", "Num. obs.")
tabc2[1:16,2] <- c("07-11", "", "","", "", "ref", "", 
                  paste0(round(Model2.1$att[2]*100,2), ifelse(Model2.1$att[2]+Model2.1$se[2]*Model2.1$c>0 & Model2.1$att[2]-Model2.1$se[2]*Model2.1$c>0 | Model2.1$att[2]+Model2.1$se[2]*Model2.1$c<0 & Model2.1$att[2]-Model2.1$se[2]*Model2.1$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model2.1$se[2]*100,2), ")"),
                  paste0(round(Model2.1$att[3]*100,2), ifelse(Model2.1$att[3]+Model2.1$se[3]*Model2.1$c>0 & Model2.1$att[3]-Model2.1$se[3]*Model2.1$c>0 | Model2.1$att[3]+Model2.1$se[3]*Model2.1$c<0 & Model2.1$att[3]-Model2.1$se[3]*Model2.1$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model2.1$se[3]*100,2),")"), 
                  paste0(round(Model2.1$att[4]*100,2), ifelse(Model2.1$att[4]+Model2.1$se[4]*Model2.1$c>0 & Model2.1$att[4]-Model2.1$se[4]*Model2.1$c>0 | Model2.1$att[4]+Model2.1$se[4]*Model2.1$c<0 & Model2.1$att[4]-Model2.1$se[4]*Model2.1$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model2.1$se[4]*100,2),")"),
                  matrix(table(subset(Model2.1$DIDparams$data)$hosd1_g)/4)[2,1],
                  matrix(table(subset(Model2.1$DIDparams$data)$hosd1_g)/4)[1,1],
                  Model2.1$n*length(Model2.1$t))


tabc2[1:16,3] <- c("11-15", "", "",  
                  paste0(round(Model2.2$att[1]*100,2), ifelse(Model2.2$att[1]+Model2.2$se[1]*Model2.2$c>0 & Model2.2$att[1]-Model2.2$se[1]*Model2.2$c>0 | Model2.2$att[1]+Model2.2$se[1]*Model2.2$c<0 & Model2.2$att[1]-Model2.2$se[1]*Model2.2$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model2.2$se[1]*100,2), ")"),
                  "ref", "",
                  paste0(round(Model2.2$att[3]*100,2), ifelse(Model2.2$att[3]+Model2.2$se[3]*Model2.2$c>0 & Model2.2$att[3]-Model2.2$se[3]*Model2.2$c>0 | Model2.2$att[3]+Model2.2$se[3]*Model2.2$c<0 & Model2.2$att[3]-Model2.2$se[3]*Model2.2$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model2.2$se[3]*100,2),")"), 
                  paste0(round(Model2.2$att[4]*100,2), ifelse(Model2.2$att[4]+Model2.2$se[4]*Model2.2$c>0 & Model2.2$att[4]-Model2.2$se[4]*Model2.2$c>0 | Model2.2$att[4]+Model2.2$se[4]*Model2.2$c<0 & Model2.2$att[4]-Model2.2$se[4]*Model2.2$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model2.2$se[4]*100,2),")"),"", "",
                  matrix(table(subset(Model2.2$DIDparams$data)$hosd1_g)/4)[2,1],
                  matrix(table(subset(Model2.2$DIDparams$data)$hosd1_g)/4)[1,1],
                  Model2.2$n*length(Model2.2$t))

tabc2[1:16,4] <- c("15-19",   
                  paste0(round(Model2.3$att[1]*100,2), ifelse(Model2.3$att[1]+Model2.3$se[1]*Model2.3$c>0 & Model2.3$att[1]-Model2.3$se[1]*Model2.3$c>0 | Model2.3$att[1]+Model2.3$se[1]*Model2.3$c<0 & Model2.3$att[1]-Model2.3$se[1]*Model2.3$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model2.3$se[1]*100,2), ")"),
                  paste0(round(Model2.3$att[2]*100,2), ifelse(Model2.3$att[2]+Model2.3$se[2]*Model2.3$c>0 & Model2.3$att[2]-Model2.3$se[2]*Model2.3$c>0 | Model2.3$att[2]+Model2.3$se[2]*Model2.3$c<0 & Model2.3$att[2]-Model2.3$se[2]*Model2.3$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model2.3$se[2]*100,2), ")"),
                  "ref", "",
                  paste0(round(Model2.3$att[4]*100,2), ifelse(Model2.3$att[4]+Model2.3$se[4]*Model2.3$c>0 & Model2.3$att[4]-Model2.3$se[4]*Model2.3$c>0 | Model2.3$att[4]+Model2.3$se[4]*Model2.3$c<0 & Model2.3$att[4]-Model2.3$se[4]*Model2.3$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model2.3$se[4]*100,2),")"),
                  "", "", "", "",
                  matrix(table(subset(Model2.3$DIDparams$data)$hosd1_g)/4)[2,1],
                  matrix(table(subset(Model2.3$DIDparams$data)$hosd1_g)/4)[1,1],
                  Model2.3$n*length(Model2.3$t))

tabc2[1:16,5] <- c("Meta", 
                  paste0(round(try3$att[1],2), ifelse(try3$conf.low[1]>0 & try3$conf.high[1]>0 | try3$conf.low[1]<0 & try3$conf.high[1]<0 , "$^{*}$", "")),
                  paste0("(",round(try3$se[1],2),")"),
                  paste0(round(try3$att[2],2), ifelse(try3$conf.low[2]>0 & try3$conf.high[2]>0 | try3$conf.low[2]<0 & try3$conf.high[2]<0 , "$^{*}$", "")),
                  paste0("(",round(try3$se[2],2),")"),
                  "ref", "", 
                  paste0(round(try3$att[4],2), ifelse(try3$conf.low[4]>0 & try3$conf.high[4]>0 | try3$conf.low[4]<0 & try3$conf.high[4]<0 , "$^{*}$", "")),
                  paste0("(",round(try3$se[4],2),")"),
                  paste0(round(try3$att[5],2), ifelse(try3$conf.low[5]>0 & try3$conf.high[5]>0 | try3$conf.low[5]<0 & try3$conf.high[5]<0 , "$^{*}$", "")),
                  paste0("(",round(try3$se[5],2),")"), 
                  paste0(round(try3$att[6],2), ifelse(try3$conf.low[6]>0 & try3$conf.high[6]>0 | try3$conf.low[6]<0 & try3$conf.high[6]<0 , "$^{*}$", "")),
                  paste0("(",round(try3$se[6],2),")"),  "-", "-", "-")

tabc2[1:16,6] <- c("07-11", "", "","", "", "ref", "", 
                   paste0(round(Model2.1m$att[2]*100,2), ifelse(Model2.1m$att[2]+Model2.1m$se[2]*Model2.1m$c>0 & Model2.1m$att[2]-Model2.1m$se[2]*Model2.1m$c>0 | Model2.1m$att[2]+Model2.1m$se[2]*Model2.1m$c<0 & Model2.1m$att[2]-Model2.1m$se[2]*Model2.1m$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.1m$se[2]*100,2), ")"),
                   paste0(round(Model2.1m$att[3]*100,2), ifelse(Model2.1m$att[3]+Model2.1m$se[3]*Model2.1m$c>0 & Model2.1m$att[3]-Model2.1m$se[3]*Model2.1m$c>0 | Model2.1m$att[3]+Model2.1m$se[3]*Model2.1m$c<0 & Model2.1m$att[3]-Model2.1m$se[3]*Model2.1m$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1m$se[3]*100,2),")"), 
                   paste0(round(Model2.1m$att[4]*100,2), ifelse(Model2.1m$att[4]+Model2.1m$se[4]*Model2.1m$c>0 & Model2.1m$att[4]-Model2.1m$se[4]*Model2.1m$c>0 | Model2.1m$att[4]+Model2.1m$se[4]*Model2.1m$c<0 & Model2.1m$att[4]-Model2.1m$se[4]*Model2.1m$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1m$se[4]*100,2),")"),
                   matrix(table(subset(Model2.1m$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.1m$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.1m$n*length(Model2.1m$t))

tabc2[1:16,7] <- c("11-15", "", "",  
                   paste0(round(Model2.2m$att[1]*100,2), ifelse(Model2.2m$att[1]+Model2.2m$se[1]*Model2.2m$c>0 & Model2.2m$att[1]-Model2.2m$se[1]*Model2.2m$c>0 | Model2.2m$att[1]+Model2.2m$se[1]*Model2.2m$c<0 & Model2.2m$att[1]-Model2.2m$se[1]*Model2.2m$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2m$se[1]*100,2), ")"),
                   "ref", "",
                   paste0(round(Model2.2m$att[3]*100,2), ifelse(Model2.2m$att[3]+Model2.2m$se[3]*Model2.2m$c>0 & Model2.2m$att[3]-Model2.2m$se[3]*Model2.2m$c>0 | Model2.2m$att[3]+Model2.2m$se[3]*Model2.2m$c<0 & Model2.2m$att[3]-Model2.2m$se[3]*Model2.2m$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.2m$se[3]*100,2),")"), 
                   paste0(round(Model2.2m$att[4]*100,2), ifelse(Model2.2m$att[4]+Model2.2m$se[4]*Model2.2m$c>0 & Model2.2m$att[4]-Model2.2m$se[4]*Model2.2m$c>0 | Model2.2m$att[4]+Model2.2m$se[4]*Model2.2m$c<0 & Model2.2m$att[4]-Model2.2m$se[4]*Model2.2m$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.2m$se[4]*100,2),")"),"", "",
                   matrix(table(subset(Model2.2m$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.2m$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.2m$n*length(Model2.2m$t))

tabc2[1:16,8] <- c("15-19",   
                   paste0(round(Model2.3m$att[1]*100,2), ifelse(Model2.3m$att[1]+Model2.3m$se[1]*Model2.3m$c>0 & Model2.3m$att[1]-Model2.3m$se[1]*Model2.3m$c>0 | Model2.3m$att[1]+Model2.3m$se[1]*Model2.3m$c<0 & Model2.3m$att[1]-Model2.3m$se[1]*Model2.3m$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3m$se[1]*100,2), ")"),
                   paste0(round(Model2.3m$att[2]*100,2), ifelse(Model2.3m$att[2]+Model2.3m$se[2]*Model2.3m$c>0 & Model2.3m$att[2]-Model2.3m$se[2]*Model2.3m$c>0 | Model2.3m$att[2]+Model2.3m$se[2]*Model2.3m$c<0 & Model2.3m$att[2]-Model2.3m$se[2]*Model2.3m$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3m$se[2]*100,2), ")"),
                   "ref", "",
                   paste0(round(Model2.3m$att[4]*100,2), ifelse(Model2.3m$att[4]+Model2.3m$se[4]*Model2.3m$c>0 & Model2.3m$att[4]-Model2.3m$se[4]*Model2.3m$c>0 | Model2.3m$att[4]+Model2.3m$se[4]*Model2.3m$c<0 & Model2.3m$att[4]-Model2.3m$se[4]*Model2.3m$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.3m$se[4]*100,2),")"),
                   "", "", "", "",
                   matrix(table(subset(Model2.3m$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.3m$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.3m$n*length(Model2.3m$t))

tabc2[1:16,9] <- c("Meta", 
                   paste0(round(try4$att[1],2), ifelse(try4$conf.low[1]>0 & try4$conf.high[1]>0 | try4$conf.low[1]<0 & try4$conf.high[1]<0 , "$^{*}$", "")),
                   paste0("(",round(try4$se[1],2),")"),
                   paste0(round(try4$att[2],2), ifelse(try4$conf.low[2]>0 & try4$conf.high[2]>0 | try4$conf.low[2]<0 & try4$conf.high[2]<0 , "$^{*}$", "")),
                   paste0("(",round(try4$se[2],2),")"),
                   "ref", "", 
                   paste0(round(try4$att[4],2), ifelse(try4$conf.low[4]>0 & try4$conf.high[4]>0 | try4$conf.low[4]<0 & try4$conf.high[4]<0 , "$^{*}$", "")),
                   paste0("(",round(try4$se[4],2),")"),
                   paste0(round(try4$att[5],2), ifelse(try4$conf.low[5]>0 & try4$conf.high[5]>0 | try4$conf.low[5]<0 & try4$conf.high[5]<0 , "$^{*}$", "")),
                   paste0("(",round(try4$se[5],2),")"), 
                   paste0(round(try4$att[6],2), ifelse(try4$conf.low[6]>0 & try4$conf.high[6]>0 | try4$conf.low[6]<0 & try4$conf.high[6]<0 , "$^{*}$", "")),
                   paste0("(",round(try4$se[6],2),")"),  "-", "-", "-")

xtable(tabc2)
##Table C3####
tabc3 <- tibble()

tabc3[1:16,1] <- c("Treated year:", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "Affected precincts", "Unaffected precincts", "Num. obs.")
tabc3[1:16,2] <- c("09-13", "", "","", "", "ref", "", 
                  paste0(round(Model1.1.l$att[2]*100,2), ifelse(Model1.1.l$att[2]+Model1.1.l$se[2]*Model1.1.l$c>0 & Model1.1.l$att[2]-Model1.1.l$se[2]*Model1.1.l$c>0 | Model1.1.l$att[2]+Model1.1.l$se[2]*Model1.1.l$c<0 & Model1.1.l$att[2]-Model1.1.l$se[2]*Model1.1.l$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model1.1.l$se[2]*100,2), ")"),
                  paste0(round(Model1.1.l$att[3]*100,2), ifelse(Model1.1.l$att[3]+Model1.1.l$se[3]*Model1.1.l$c>0 & Model1.1.l$att[3]-Model1.1.l$se[3]*Model1.1.l$c>0 | Model1.1.l$att[3]+Model1.1.l$se[3]*Model1.1.l$c<0 & Model1.1.l$att[3]-Model1.1.l$se[3]*Model1.1.l$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model1.1.l$se[3]*100,2),")"), "", "",
                  matrix(table(subset(Model1.1.l$DIDparams$data)$schd1_g)/3)[2,1],
                  matrix(table(subset(Model1.1.l$DIDparams$data)$schd1_g)/3)[1,1],
                  Model1.1.l$n*length(Model1.1.l$t))

tabc3[1:16,3] <- c("13-17", "", "",
                  paste0(round(Model1.2.l$att[1]*100,2), ifelse(Model1.2.l$att[1]+Model1.2.l$se[1]*Model1.2.l$c>0 & Model1.2.l$att[1]-Model1.2.l$se[1]*Model1.2.l$c>0 | Model1.2.l$att[1]+Model1.2.l$se[1]*Model1.2.l$c<0 & Model1.2.l$att[1]-Model1.2.l$se[1]*Model1.2.l$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model1.2.l$se[1]*100,2),")"),
                  "ref", "", 
                  paste0(round(Model1.2.l$att[3]*100,2), ifelse(Model1.2.l$att[3]+Model1.2.l$se[3]*Model1.2.l$c>0 & Model1.2.l$att[3]-Model1.2.l$se[3]*Model1.2.l$c>0 | Model1.2.l$att[3]+Model1.2.l$se[3]*Model1.2.l$c<0 & Model1.2.l$att[3]-Model1.2.l$se[3]*Model1.2.l$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model1.2.l$se[3]*100,2), ")"),"", "", "", "",
                  matrix(table(subset(Model1.2.l$DIDparams$data)$schd1_g)/3)[2,1],
                  matrix(table(subset(Model1.2.l$DIDparams$data)$schd1_g)/3)[1,1],
                  Model1.2.l$n*length(Model1.2.l$t))

tabc3[1:16,4] <- c("Meta", "", "",
                  paste0(round(le.low_um$att[1],2), ifelse(le.low_um$conf.low[1]>0 & le.low_um$conf.high[1]>0 | le.low_um$conf.low[1]<0 & le.low_um$conf.high[1]<0 , "$^{*}$", "")),
                  paste0("(",round(le.low_um$se[1],2),")"),
                  "ref", "", 
                  paste0(round(le.low_um$att[3],2), ifelse(le.low_um$conf.low[3]>0 & le.low_um$conf.high[3]>0 | le.low_um$conf.low[3]<0 & le.low_um$conf.high[3]<0 , "$^{*}$", "")),
                  paste0("(",round(le.low_um$se[3],2),")"),
                  paste0(round(le.low_um$att[4],2), ifelse(le.low_um$conf.low[4]>0 & le.low_um$conf.high[4]>0 | le.low_um$conf.low[4]<0 & le.low_um$conf.high[4]<0 , "$^{*}$", "")),
                  paste0("(",round(le.low_um$se[4],2),")"), "", "", "-", "-", "-")
tabc3[1:16,5] <- c("09-13", "", "","", "", "ref", "", 
                  paste0(round(Model1.1.h$att[2]*100,2), ifelse(Model1.1.h$att[2]+Model1.1.h$se[2]*Model1.1.h$c>0 & Model1.1.h$att[2]-Model1.1.h$se[2]*Model1.1.h$c>0 | Model1.1.h$att[2]+Model1.1.h$se[2]*Model1.1.h$c<0 & Model1.1.h$att[2]-Model1.1.h$se[2]*Model1.1.h$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model1.1.h$se[2]*100,2), ")"),
                  paste0(round(Model1.1.h$att[3]*100,2), ifelse(Model1.1.h$att[3]+Model1.1.h$se[3]*Model1.1.h$c>0 & Model1.1.h$att[3]-Model1.1.h$se[3]*Model1.1.h$c>0 | Model1.1.h$att[3]+Model1.1.h$se[3]*Model1.1.h$c<0 & Model1.1.h$att[3]-Model1.1.h$se[3]*Model1.1.h$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model1.1.h$se[3]*100,2),")"), "", "",
                  matrix(table(subset(Model1.1.h$DIDparams$data)$schd1_g)/3)[2,1],
                  matrix(table(subset(Model1.1.h$DIDparams$data)$schd1_g)/3)[1,1],
                  Model1.1.h$n*length(Model1.1.h$t))

tabc3[1:16,6] <- c("13-17", "", "",
                  paste0(round(Model1.2.h$att[1]*100,2), ifelse(Model1.2.h$att[1]+Model1.2.h$se[1]*Model1.2.h$c>0 & Model1.2.h$att[1]-Model1.2.h$se[1]*Model1.2.h$c>0 | Model1.2.h$att[1]+Model1.2.h$se[1]*Model1.2.h$c<0 & Model1.2.h$att[1]-Model1.2.h$se[1]*Model1.2.h$c<0 , "$^{*}$", "")),
                  paste0("(",round(Model1.2.h$se[1]*100,2),")"),
                  "ref", "", 
                  paste0(round(Model1.2.h$att[3]*100,2), ifelse(Model1.2.h$att[3]+Model1.2.h$se[3]*Model1.2.h$c>0 & Model1.2.h$att[3]-Model1.2.h$se[3]*Model1.2.h$c>0 | Model1.2.h$att[3]+Model1.2.h$se[3]*Model1.2.h$c<0 & Model1.2.h$att[3]-Model1.2.h$se[3]*Model1.2.h$c<0 , "$^{*}$", "")),
                  paste0("(", round(Model1.2.h$se[3]*100,2), ")"),"", "", "", "",
                  matrix(table(subset(Model1.2.h$DIDparams$data)$schd1_g)/3)[2,1],
                  matrix(table(subset(Model1.2.h$DIDparams$data)$schd1_g)/3)[1,1],
                  Model1.2.h$n*length(Model1.2.h$t))
tabc3[1:16,7] <- c("Meta", "", "",
                  paste0(round(le.hig_um$att[1],2), ifelse(le.hig_um$conf.low[1]>0 & le.hig_um$conf.high[1]>0 | le.hig_um$conf.low[1]<0 & le.hig_um$conf.high[1]<0 , "$^{*}$", "")),
                  paste0("(",round(le.hig_um$se[1],2),")"),
                  "ref", "", 
                  paste0(round(le.hig_um$att[3],2), ifelse(le.hig_um$conf.low[3]>0 & le.hig_um$conf.high[3]>0 | le.hig_um$conf.low[3]<0 & le.hig_um$conf.high[3]<0 , "$^{*}$", "")),
                  paste0("(",round(le.hig_um$se[3],2),")"),
                  paste0(round(le.hig_um$att[4],2), ifelse(le.hig_um$conf.low[4]>0 & le.hig_um$conf.high[4]>0 | le.hig_um$conf.low[4]<0 & le.hig_um$conf.high[4]<0 , "$^{*}$", "")),
                  paste0("(",round(le.hig_um$se[4],2),")"), "", "", "-", "-", "-")

xtable(tabc3)

##Table C4####
tabc4 <- tibble()

tabc4[1:16,1] <- c("Treated year:", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "Affected precincts", "Unaffected precincts", "Num. obs.")
tabc4[1:16,2] <- c("09-13", "", "","", "", "ref", "", 
                   paste0(round(Model1.1m.l$att[2]*100,2), ifelse(Model1.1m.l$att[2]+Model1.1m.l$se[2]*Model1.1m.l$c>0 & Model1.1m.l$att[2]-Model1.1m.l$se[2]*Model1.1m.l$c>0 | Model1.1m.l$att[2]+Model1.1m.l$se[2]*Model1.1m.l$c<0 & Model1.1m.l$att[2]-Model1.1m.l$se[2]*Model1.1m.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model1.1m.l$se[2]*100,2), ")"),
                   paste0(round(Model1.1m.l$att[3]*100,2), ifelse(Model1.1m.l$att[3]+Model1.1m.l$se[3]*Model1.1m.l$c>0 & Model1.1m.l$att[3]-Model1.1m.l$se[3]*Model1.1m.l$c>0 | Model1.1m.l$att[3]+Model1.1m.l$se[3]*Model1.1m.l$c<0 & Model1.1m.l$att[3]-Model1.1m.l$se[3]*Model1.1m.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model1.1m.l$se[3]*100,2),")"), "", "",
                   matrix(table(subset(Model1.1m.l$DIDparams$data)$schd1_g)/3)[2,1],
                   matrix(table(subset(Model1.1m.l$DIDparams$data)$schd1_g)/3)[1,1],
                   Model1.1m.l$n*length(Model1.1m.l$t))
tabc4[1:16,3] <- c("13-17", "", "",
                   paste0(round(Model1.2m.l$att[1]*100,2), ifelse(Model1.2m.l$att[1]+Model1.2m.l$se[1]*Model1.2m.l$c>0 & Model1.2m.l$att[1]-Model1.2m.l$se[1]*Model1.2m.l$c>0 | Model1.2m.l$att[1]+Model1.2m.l$se[1]*Model1.2m.l$c<0 & Model1.2m.l$att[1]-Model1.2m.l$se[1]*Model1.2m.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model1.2m.l$se[1]*100,2),")"),
                   "ref", "", 
                   paste0(round(Model1.2m.l$att[3]*100,2), ifelse(Model1.2m.l$att[3]+Model1.2m.l$se[3]*Model1.2m.l$c>0 & Model1.2m.l$att[3]-Model1.2m.l$se[3]*Model1.2m.l$c>0 | Model1.2m.l$att[3]+Model1.2m.l$se[3]*Model1.2m.l$c<0 & Model1.2m.l$att[3]-Model1.2m.l$se[3]*Model1.2m.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model1.2m.l$se[3]*100,2), ")"),"", "", "", "",
                   matrix(table(subset(Model1.2m.l$DIDparams$data)$schd1_g)/3)[2,1],
                   matrix(table(subset(Model1.2m.l$DIDparams$data)$schd1_g)/3)[1,1],
                   Model1.2m.l$n*length(Model1.2m.l$t))
tabc4[1:16,4] <- c("Meta", "", "",
                   paste0(round(le.low_ma$att[1],2), ifelse(le.low_ma$conf.low[1]>0 & le.low_ma$conf.high[1]>0 | le.low_ma$conf.low[1]<0 & le.low_ma$conf.high[1]<0 , "$^{*}$", "")),
                   paste0("(",round(le.low_ma$se[1],2),")"),
                   "ref", "", 
                   paste0(round(le.low_ma$att[3],2), ifelse(le.low_ma$conf.low[3]>0 & le.low_ma$conf.high[3]>0 | le.low_ma$conf.low[3]<0 & le.low_ma$conf.high[3]<0 , "$^{*}$", "")),
                   paste0("(",round(le.low_ma$se[3],2),")"),
                   paste0(round(le.low_ma$att[4],2), ifelse(le.low_ma$conf.low[4]>0 & le.low_ma$conf.high[4]>0 | le.low_ma$conf.low[4]<0 & le.low_ma$conf.high[4]<0 , "$^{*}$", "")),
                   paste0("(",round(le.low_ma$se[4],2),")"), "", "", "-", "-", "-")
tabc4[1:16,5] <- c("09-13", "", "", "", "", "ref", "", 
                   paste0(round(Model1.1m.h$att[2]*100,2), ifelse(Model1.1m.h$att[2]+Model1.1m.h$se[2]*Model1.1m.h$c>0 & Model1.1m.h$att[2]-Model1.1m.h$se[2]*Model1.1m.h$c>0 | Model1.1m.h$att[2]+Model1.1m.h$se[2]*Model1.1m.h$c<0 & Model1.1m.h$att[2]-Model1.1m.h$se[2]*Model1.1m.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model1.1m.h$se[2]*100,2), ")"),
                   paste0(round(Model1.1m.h$att[3]*100,2), ifelse(Model1.1m.h$att[3]+Model1.1m.h$se[3]*Model1.1m.h$c>0 & Model1.1m.h$att[3]-Model1.1m.h$se[3]*Model1.1m.h$c>0 | Model1.1m.h$att[3]+Model1.1m.h$se[3]*Model1.1m.h$c<0 & Model1.1m.h$att[3]-Model1.1m.h$se[3]*Model1.1m.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model1.1m.h$se[3]*100,2),")"), "", "",
                   matrix(table(subset(Model1.1m.h$DIDparams$data)$schd1_g)/3)[2,1],
                   matrix(table(subset(Model1.1m.h$DIDparams$data)$schd1_g)/3)[1,1],
                   Model1.1m.h$n*length(Model1.1m.h$t))

tabc4[1:16,6] <- c("13-17", "", "",
                   paste0(round(Model1.2m.h$att[1]*100,2), ifelse(Model1.2m.h$att[1]+Model1.2m.h$se[1]*Model1.2m.h$c>0 & Model1.2m.h$att[1]-Model1.2m.h$se[1]*Model1.2m.h$c>0 | Model1.2m.h$att[1]+Model1.2m.h$se[1]*Model1.2m.h$c<0 & Model1.2m.h$att[1]-Model1.2m.h$se[1]*Model1.2m.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model1.2m.h$se[1]*100,2),")"),
                   "ref", "", 
                   paste0(round(Model1.2m.h$att[3]*100,2), ifelse(Model1.2m.h$att[3]+Model1.2m.h$se[3]*Model1.2m.h$c>0 & Model1.2m.h$att[3]-Model1.2m.h$se[3]*Model1.2m.h$c>0 | Model1.2m.h$att[3]+Model1.2m.h$se[3]*Model1.2m.h$c<0 & Model1.2m.h$att[3]-Model1.2m.h$se[3]*Model1.2m.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model1.2m.h$se[3]*100,2), ")"),"", "", "", "",
                   matrix(table(subset(Model1.2m.h$DIDparams$data)$schd1_g)/3)[2,1],
                   matrix(table(subset(Model1.2m.h$DIDparams$data)$schd1_g)/3)[1,1],
                   Model1.2m.h$n*length(Model1.2m.h$t))

tabc4[1:16,7] <- c("Meta", "", "",
                   paste0(round(le.hig_ma$att[1],2), ifelse(le.hig_ma$conf.low[1]>0 & le.hig_ma$conf.high[1]>0 | le.hig_ma$conf.low[1]<0 & le.hig_ma$conf.high[1]<0 , "$^{*}$", "")),
                   paste0("(",round(le.hig_ma$se[1],2),")"),
                   "ref", "", 
                   paste0(round(le.hig_ma$att[3],2), ifelse(le.hig_ma$conf.low[3]>0 & le.hig_ma$conf.high[3]>0 | le.hig_ma$conf.low[3]<0 & le.hig_ma$conf.high[3]<0 , "$^{*}$", "")),
                   paste0("(",round(le.hig_ma$se[3],2),")"),
                   paste0(round(le.hig_ma$att[4],2), ifelse(le.hig_ma$conf.low[4]>0 & le.hig_ma$conf.high[4]>0 | le.hig_ma$conf.low[4]<0 & le.hig_ma$conf.high[4]<0 , "$^{*}$", "")),
                   paste0("(",round(le.hig_ma$se[4],2),")"), "", "", "-", "-", "-")

xtable(tabc4)
##Table C5####
tabc5 <- tibble()

tabc5[1:16,1] <- c("Treated year:", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "Affected precincts", "Unaffected precincts", "Num. obs.")
tabc5[1:16,2] <- c("07-11", "", "","", "", "ref", "", 
                   paste0(round(Model2.1.l$att[2]*100,2), ifelse(Model2.1.l$att[2]+Model2.1.l$se[2]*Model2.1.l$c>0 & Model2.1.l$att[2]-Model2.1.l$se[2]*Model2.1.l$c>0 | Model2.1.l$att[2]+Model2.1.l$se[2]*Model2.1.l$c<0 & Model2.1.l$att[2]-Model2.1.l$se[2]*Model2.1.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.1.l$se[2]*100,2), ")"),
                   paste0(round(Model2.1.l$att[3]*100,2), ifelse(Model2.1.l$att[3]+Model2.1.l$se[3]*Model2.1.l$c>0 & Model2.1.l$att[3]-Model2.1.l$se[3]*Model2.1.l$c>0 | Model2.1.l$att[3]+Model2.1.l$se[3]*Model2.1.l$c<0 & Model2.1.l$att[3]-Model2.1.l$se[3]*Model2.1.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1.l$se[3]*100,2),")"), 
                   paste0(round(Model2.1.l$att[4]*100,2), ifelse(Model2.1.l$att[4]+Model2.1.l$se[4]*Model2.1.l$c>0 & Model2.1.l$att[4]-Model2.1.l$se[4]*Model2.1.l$c>0 | Model2.1.l$att[4]+Model2.1.l$se[4]*Model2.1.l$c<0 & Model2.1.l$att[4]-Model2.1.l$se[4]*Model2.1.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1.l$se[4]*100,2),")"), 
                   matrix(table(subset(Model2.1.l$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.1.l$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.1.l$n*length(Model2.1.l$t))

tabc5[1:16,3] <- c("11-15", "", "",
                   paste0(round(Model2.2.l$att[1]*100,2), ifelse(Model2.2.l$att[1]+Model2.2.l$se[1]*Model2.2.l$c>0 & Model2.2.l$att[1]-Model2.2.l$se[1]*Model2.2.l$c>0 | Model2.2.l$att[1]+Model2.2.l$se[1]*Model2.2.l$c<0 & Model2.2.l$att[1]-Model2.2.l$se[1]*Model2.2.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.2.l$se[1]*100,2),")"),
                   "ref", "", 
                   paste0(round(Model2.2.l$att[3]*100,2), ifelse(Model2.2.l$att[3]+Model2.2.l$se[3]*Model2.2.l$c>0 & Model2.2.l$att[3]-Model2.2.l$se[3]*Model2.2.l$c>0 | Model2.2.l$att[3]+Model2.2.l$se[3]*Model2.2.l$c<0 & Model2.2.l$att[3]-Model2.2.l$se[3]*Model2.2.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2.l$se[3]*100,2), ")"),
                   paste0(round(Model2.2.l$att[4]*100,2), ifelse(Model2.2.l$att[4]+Model2.2.l$se[4]*Model2.2.l$c>0 & Model2.2.l$att[4]-Model2.2.l$se[4]*Model2.2.l$c>0 | Model2.2.l$att[4]+Model2.2.l$se[4]*Model2.2.l$c<0 & Model2.2.l$att[4]-Model2.2.l$se[4]*Model2.2.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2.l$se[4]*100,2), ")"), "", "", 
                   matrix(table(subset(Model2.2.l$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.2.l$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.2.l$n*length(Model2.2.l$t))

tabc5[1:16,4] <- c("15-19", 
                   paste0(round(Model2.3.l$att[1]*100,2), ifelse(Model2.3.l$att[1]+Model2.3.l$se[1]*Model2.3.l$c>0 & Model2.3.l$att[1]-Model2.3.l$se[1]*Model2.3.l$c>0 | Model2.3.l$att[1]+Model2.3.l$se[1]*Model2.3.l$c<0 & Model2.3.l$att[1]-Model2.3.l$se[1]*Model2.3.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.3.l$se[1]*100,2),")"),
                   paste0(round(Model2.3.l$att[2]*100,2), ifelse(Model2.3.l$att[2]+Model2.3.l$se[2]*Model2.3.l$c>0 & Model2.3.l$att[2]-Model2.3.l$se[2]*Model2.3.l$c>0 | Model2.3.l$att[2]+Model2.3.l$se[2]*Model2.3.l$c<0 & Model2.3.l$att[2]-Model2.3.l$se[2]*Model2.3.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3.l$se[2]*100,2), ")"),
                   "ref", "", 
                   paste0(round(Model2.3.l$att[4]*100,2), ifelse(Model2.3.l$att[4]+Model2.3.l$se[4]*Model2.3.l$c>0 & Model2.3.l$att[4]-Model2.3.l$se[4]*Model2.3.l$c>0 | Model2.3.l$att[4]+Model2.3.l$se[4]*Model2.3.l$c<0 & Model2.3.l$att[4]-Model2.3.l$se[4]*Model2.3.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3.l$se[4]*100,2), ")"),
                   "", "", "", "",
                   matrix(table(subset(Model2.3.l$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.3.l$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.3.l$n*length(Model2.3.l$t))

tabc5[1:16,5] <- c("Meta", 
                   paste0(round(ne.low_um$att[1],2), ifelse(ne.low_um$conf.low[1]>0 & ne.low_um$conf.high[1]>0 | ne.low_um$conf.low[1]<0 & ne.low_um$conf.high[1]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_um$se[1],2),")"),
                   paste0(round(ne.low_um$att[2],2), ifelse(ne.low_um$conf.low[2]>0 & ne.low_um$conf.high[2]>0 | ne.low_um$conf.low[2]<0 & ne.low_um$conf.high[2]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_um$se[2],2),")"),
                   "ref", "", 
                   paste0(round(ne.low_um$att[4],2), ifelse(ne.low_um$conf.low[4]>0 & ne.low_um$conf.high[4]>0 | ne.low_um$conf.low[4]<0 & ne.low_um$conf.high[4]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_um$se[4],2),")"), 
                   paste0(round(ne.low_um$att[5],2), ifelse(ne.low_um$conf.low[5]>0 & ne.low_um$conf.high[5]>0 | ne.low_um$conf.low[5]<0 & ne.low_um$conf.high[5]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_um$se[5],2),")"), 
                   paste0(round(ne.low_um$att[6],2), ifelse(ne.low_um$conf.low[6]>0 & ne.low_um$conf.high[6]>0 | ne.low_um$conf.low[6]<0 & ne.low_um$conf.high[6]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_um$se[6],2),")"),
                   "-", "-", "-")
tabc5[1:16,6] <- c("07-11", "", "","", "", "ref", "", 
                   paste0(round(Model2.1.h$att[2]*100,2), ifelse(Model2.1.h$att[2]+Model2.1.h$se[2]*Model2.1.h$c>0 & Model2.1.h$att[2]-Model2.1.h$se[2]*Model2.1.h$c>0 | Model2.1.h$att[2]+Model2.1.h$se[2]*Model2.1.h$c<0 & Model2.1.h$att[2]-Model2.1.h$se[2]*Model2.1.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.1.h$se[2]*100,2), ")"),
                   paste0(round(Model2.1.h$att[3]*100,2), ifelse(Model2.1.h$att[3]+Model2.1.h$se[3]*Model2.1.h$c>0 & Model2.1.h$att[3]-Model2.1.h$se[3]*Model2.1.h$c>0 | Model2.1.h$att[3]+Model2.1.h$se[3]*Model2.1.h$c<0 & Model2.1.h$att[3]-Model2.1.h$se[3]*Model2.1.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1.h$se[3]*100,2),")"), 
                   paste0(round(Model2.1.h$att[4]*100,2), ifelse(Model2.1.h$att[4]+Model2.1.h$se[4]*Model2.1.h$c>0 & Model2.1.h$att[4]-Model2.1.h$se[4]*Model2.1.h$c>0 | Model2.1.h$att[4]+Model2.1.h$se[4]*Model2.1.h$c<0 & Model2.1.h$att[4]-Model2.1.h$se[4]*Model2.1.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1.h$se[4]*100,2),")"), 
                   matrix(table(subset(Model2.1.h$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.1.h$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.1.h$n*length(Model2.1.h$t))

tabc5[1:16,7] <- c("11-15", "", "",
                   paste0(round(Model2.2.h$att[1]*100,2), ifelse(Model2.2.h$att[1]+Model2.2.h$se[1]*Model2.2.h$c>0 & Model2.2.h$att[1]-Model2.2.h$se[1]*Model2.2.h$c>0 | Model2.2.h$att[1]+Model2.2.h$se[1]*Model2.2.h$c<0 & Model2.2.h$att[1]-Model2.2.h$se[1]*Model2.2.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.2.h$se[1]*100,2),")"),
                   "ref", "", 
                   paste0(round(Model2.2.h$att[3]*100,2), ifelse(Model2.2.h$att[3]+Model2.2.h$se[3]*Model2.2.h$c>0 & Model2.2.h$att[3]-Model2.2.h$se[3]*Model2.2.h$c>0 | Model2.2.h$att[3]+Model2.2.h$se[3]*Model2.2.h$c<0 & Model2.2.h$att[3]-Model2.2.h$se[3]*Model2.2.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2.h$se[3]*100,2), ")"),
                   paste0(round(Model2.2.h$att[4]*100,2), ifelse(Model2.2.h$att[4]+Model2.2.h$se[4]*Model2.2.h$c>0 & Model2.2.h$att[4]-Model2.2.h$se[4]*Model2.2.h$c>0 | Model2.2.h$att[4]+Model2.2.h$se[4]*Model2.2.h$c<0 & Model2.2.h$att[4]-Model2.2.h$se[4]*Model2.2.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2.h$se[4]*100,2), ")"), "", "", 
                   matrix(table(subset(Model2.2.h$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.2.h$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.2.h$n*length(Model2.2.h$t))

tabc5[1:16,8] <- c("15-19", 
                   paste0(round(Model2.3.h$att[1]*100,2), ifelse(Model2.3.h$att[1]+Model2.3.h$se[1]*Model2.3.h$c>0 & Model2.3.h$att[1]-Model2.3.h$se[1]*Model2.3.h$c>0 | Model2.3.h$att[1]+Model2.3.h$se[1]*Model2.3.h$c<0 & Model2.3.h$att[1]-Model2.3.h$se[1]*Model2.3.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.3.h$se[1]*100,2),")"),
                   paste0(round(Model2.3.h$att[2]*100,2), ifelse(Model2.3.h$att[2]+Model2.3.h$se[2]*Model2.3.h$c>0 & Model2.3.h$att[2]-Model2.3.h$se[2]*Model2.3.h$c>0 | Model2.3.h$att[2]+Model2.3.h$se[2]*Model2.3.h$c<0 & Model2.3.h$att[2]-Model2.3.h$se[2]*Model2.3.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3.h$se[2]*100,2), ")"),
                   "ref", "", 
                   paste0(round(Model2.3.h$att[4]*100,2), ifelse(Model2.3.h$att[4]+Model2.3.h$se[4]*Model2.3.h$c>0 & Model2.3.h$att[4]-Model2.3.h$se[4]*Model2.3.h$c>0 | Model2.3.h$att[4]+Model2.3.h$se[4]*Model2.3.h$c<0 & Model2.3.h$att[4]-Model2.3.h$se[4]*Model2.3.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3.h$se[4]*100,2), ")"),
                   "", "", "", "",
                   matrix(table(subset(Model2.3.h$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.3.h$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.3.h$n*length(Model2.3.h$t))

tabc5[1:16,9] <- c("Meta", 
                   paste0(round(ne.hig_um$att[1],2), ifelse(ne.hig_um$conf.high[1]>0 & ne.hig_um$conf.low[1]>0 | ne.hig_um$conf.high[1]<0 & ne.hig_um$conf.low[1]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.hig_um$se[1],2),")"),
                   paste0(round(ne.hig_um$att[2],2), ifelse(ne.hig_um$conf.high[2]>0 & ne.hig_um$conf.low[2]>0 | ne.hig_um$conf.high[2]<0 & ne.hig_um$conf.low[2]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.hig_um$se[2],2),")"),
                   "ref", "", 
                   paste0(round(ne.hig_um$att[4],2), ifelse(ne.hig_um$conf.high[4]>0 & ne.hig_um$conf.low[4]>0 | ne.hig_um$conf.high[4]<0 & ne.hig_um$conf.low[4]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.hig_um$se[4],2),")"), 
                   paste0(round(ne.hig_um$att[5],2), ifelse(ne.hig_um$conf.high[5]>0 & ne.hig_um$conf.low[5]>0 | ne.hig_um$conf.high[5]<0 & ne.hig_um$conf.low[5]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.hig_um$se[5],2),")"), 
                   paste0(round(ne.hig_um$att[6],2), ifelse(ne.hig_um$conf.high[6]>0 & ne.hig_um$conf.low[6]>0 | ne.hig_um$conf.high[6]<0 & ne.hig_um$conf.low[6]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_um$se[6],2),")"),
                   "-", "-", "-")

xtable(tabc5)

##Table C6####
tabc6 <- tibble()

tabc6[1:16,1] <- c("Treated year:", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "Affected precincts", "Unaffected precincts", "Num. obs.")
tabc6[1:16,2] <- c("07-11", "", "","", "", "ref", "", 
                   paste0(round(Model2.1m.l$att[2]*100,2), ifelse(Model2.1m.l$att[2]+Model2.1m.l$se[2]*Model2.1m.l$c>0 & Model2.1m.l$att[2]-Model2.1m.l$se[2]*Model2.1m.l$c>0 | Model2.1m.l$att[2]+Model2.1m.l$se[2]*Model2.1m.l$c<0 & Model2.1m.l$att[2]-Model2.1m.l$se[2]*Model2.1m.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.1m.l$se[2]*100,2), ")"),
                   paste0(round(Model2.1m.l$att[3]*100,2), ifelse(Model2.1m.l$att[3]+Model2.1m.l$se[3]*Model2.1m.l$c>0 & Model2.1m.l$att[3]-Model2.1m.l$se[3]*Model2.1m.l$c>0 | Model2.1m.l$att[3]+Model2.1m.l$se[3]*Model2.1m.l$c<0 & Model2.1m.l$att[3]-Model2.1m.l$se[3]*Model2.1m.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1m.l$se[3]*100,2),")"), 
                   paste0(round(Model2.1m.l$att[4]*100,2), ifelse(Model2.1m.l$att[4]+Model2.1m.l$se[4]*Model2.1m.l$c>0 & Model2.1m.l$att[4]-Model2.1m.l$se[4]*Model2.1m.l$c>0 | Model2.1m.l$att[4]+Model2.1m.l$se[4]*Model2.1m.l$c<0 & Model2.1m.l$att[4]-Model2.1m.l$se[4]*Model2.1m.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1m.l$se[4]*100,2),")"), 
                   matrix(table(subset(Model2.1m.l$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.1m.l$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.1m.l$n*length(Model2.1m.l$t))

tabc6[1:16,3] <- c("11-15", "", "",
                   paste0(round(Model2.2m.l$att[1]*100,2), ifelse(Model2.2m.l$att[1]+Model2.2m.l$se[1]*Model2.2m.l$c>0 & Model2.2m.l$att[1]-Model2.2m.l$se[1]*Model2.2m.l$c>0 | Model2.2m.l$att[1]+Model2.2m.l$se[1]*Model2.2m.l$c<0 & Model2.2m.l$att[1]-Model2.2m.l$se[1]*Model2.2m.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.2m.l$se[1]*100,2),")"),
                   "ref", "", 
                   paste0(round(Model2.2m.l$att[3]*100,2), ifelse(Model2.2m.l$att[3]+Model2.2m.l$se[3]*Model2.2m.l$c>0 & Model2.2m.l$att[3]-Model2.2m.l$se[3]*Model2.2m.l$c>0 | Model2.2m.l$att[3]+Model2.2m.l$se[3]*Model2.2m.l$c<0 & Model2.2m.l$att[3]-Model2.2m.l$se[3]*Model2.2m.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2m.l$se[3]*100,2), ")"),
                   paste0(round(Model2.2m.l$att[4]*100,2), ifelse(Model2.2m.l$att[4]+Model2.2m.l$se[4]*Model2.2m.l$c>0 & Model2.2m.l$att[4]-Model2.2m.l$se[4]*Model2.2m.l$c>0 | Model2.2m.l$att[4]+Model2.2m.l$se[4]*Model2.2m.l$c<0 & Model2.2m.l$att[4]-Model2.2m.l$se[4]*Model2.2m.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2m.l$se[4]*100,2), ")"), "", "", 
                   matrix(table(subset(Model2.2m.l$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.2m.l$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.2m.l$n*length(Model2.2m.l$t))

tabc6[1:16,4] <- c("15-19", 
                   paste0(round(Model2.3m.l$att[1]*100,2), ifelse(Model2.3m.l$att[1]+Model2.3m.l$se[1]*Model2.3m.l$c>0 & Model2.3m.l$att[1]-Model2.3m.l$se[1]*Model2.3m.l$c>0 | Model2.3m.l$att[1]+Model2.3m.l$se[1]*Model2.3m.l$c<0 & Model2.3m.l$att[1]-Model2.3m.l$se[1]*Model2.3m.l$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.3m.l$se[1]*100,2),")"),
                   paste0(round(Model2.3m.l$att[2]*100,2), ifelse(Model2.3m.l$att[2]+Model2.3m.l$se[2]*Model2.3m.l$c>0 & Model2.3m.l$att[2]-Model2.3m.l$se[2]*Model2.3m.l$c>0 | Model2.3m.l$att[2]+Model2.3m.l$se[2]*Model2.3m.l$c<0 & Model2.3m.l$att[2]-Model2.3m.l$se[2]*Model2.3m.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3m.l$se[2]*100,2), ")"),
                   "ref", "", 
                   paste0(round(Model2.3m.l$att[4]*100,2), ifelse(Model2.3m.l$att[4]+Model2.3m.l$se[4]*Model2.3m.l$c>0 & Model2.3m.l$att[4]-Model2.3m.l$se[4]*Model2.3m.l$c>0 | Model2.3m.l$att[4]+Model2.3m.l$se[4]*Model2.3m.l$c<0 & Model2.3m.l$att[4]-Model2.3m.l$se[4]*Model2.3m.l$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3m.l$se[4]*100,2), ")"),
                   "", "", "", "",
                   matrix(table(subset(Model2.3m.l$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.3m.l$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.3m.l$n*length(Model2.3m.l$t))
tabc6[1:16,5] <- c("Meta", 
                   paste0(round(ne.low_ma$att[1],2), ifelse(ne.low_ma$conf.low[1]>0 & ne.low_ma$conf.high[1]>0 | ne.low_ma$conf.low[1]<0 & ne.low_ma$conf.high[1]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_ma$se[1],2),")"),
                   paste0(round(ne.low_ma$att[2],2), ifelse(ne.low_ma$conf.low[2]>0 & ne.low_ma$conf.high[2]>0 | ne.low_ma$conf.low[2]<0 & ne.low_ma$conf.high[2]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_ma$se[2],2),")"),
                   "ref", "", 
                   paste0(round(ne.low_ma$att[4],2), ifelse(ne.low_ma$conf.low[4]>0 & ne.low_ma$conf.high[4]>0 | ne.low_ma$conf.low[4]<0 & ne.low_ma$conf.high[4]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_ma$se[4],2),")"), 
                   paste0(round(ne.low_ma$att[5],2), ifelse(ne.low_ma$conf.low[5]>0 & ne.low_ma$conf.high[5]>0 | ne.low_ma$conf.low[5]<0 & ne.low_ma$conf.high[5]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_ma$se[5],2),")"), 
                   paste0(round(ne.low_ma$att[6],2), ifelse(ne.low_ma$conf.low[6]>0 & ne.low_ma$conf.high[6]>0 | ne.low_ma$conf.low[6]<0 & ne.low_ma$conf.high[6]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_ma$se[6],2),")"),
                   "-", "-", "-")

tabc6[1:16,6] <- c("07-11", "", "","", "", "ref", "", 
                   paste0(round(Model2.1m.h$att[2]*100,2), ifelse(Model2.1m.h$att[2]+Model2.1m.h$se[2]*Model2.1m.h$c>0 & Model2.1m.h$att[2]-Model2.1m.h$se[2]*Model2.1m.h$c>0 | Model2.1m.h$att[2]+Model2.1m.h$se[2]*Model2.1m.h$c<0 & Model2.1m.h$att[2]-Model2.1m.h$se[2]*Model2.1m.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.1m.h$se[2]*100,2), ")"),
                   paste0(round(Model2.1m.h$att[3]*100,2), ifelse(Model2.1m.h$att[3]+Model2.1m.h$se[3]*Model2.1m.h$c>0 & Model2.1m.h$att[3]-Model2.1m.h$se[3]*Model2.1m.h$c>0 | Model2.1m.h$att[3]+Model2.1m.h$se[3]*Model2.1m.h$c<0 & Model2.1m.h$att[3]-Model2.1m.h$se[3]*Model2.1m.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1m.h$se[3]*100,2),")"), 
                   paste0(round(Model2.1m.h$att[4]*100,2), ifelse(Model2.1m.h$att[4]+Model2.1m.h$se[4]*Model2.1m.h$c>0 & Model2.1m.h$att[4]-Model2.1m.h$se[4]*Model2.1m.h$c>0 | Model2.1m.h$att[4]+Model2.1m.h$se[4]*Model2.1m.h$c<0 & Model2.1m.h$att[4]-Model2.1m.h$se[4]*Model2.1m.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.1m.h$se[4]*100,2),")"), 
                   matrix(table(subset(Model2.1m.h$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.1m.h$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.1m.h$n*length(Model2.1m.h$t))

tabc6[1:16,7] <- c("11-15", "", "",
                   paste0(round(Model2.2m.h$att[1]*100,2), ifelse(Model2.2m.h$att[1]+Model2.2m.h$se[1]*Model2.2m.h$c>0 & Model2.2m.h$att[1]-Model2.2m.h$se[1]*Model2.2m.h$c>0 | Model2.2m.h$att[1]+Model2.2m.h$se[1]*Model2.2m.h$c<0 & Model2.2m.h$att[1]-Model2.2m.h$se[1]*Model2.2m.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.2m.h$se[1]*100,2),")"),
                   "ref", "", 
                   paste0(round(Model2.2m.h$att[3]*100,2), ifelse(Model2.2m.h$att[3]+Model2.2m.h$se[3]*Model2.2m.h$c>0 & Model2.2m.h$att[3]-Model2.2m.h$se[3]*Model2.2m.h$c>0 | Model2.2m.h$att[3]+Model2.2m.h$se[3]*Model2.2m.h$c<0 & Model2.2m.h$att[3]-Model2.2m.h$se[3]*Model2.2m.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2m.h$se[3]*100,2), ")"),
                   paste0(round(Model2.2m.h$att[4]*100,2), ifelse(Model2.2m.h$att[4]+Model2.2m.h$se[4]*Model2.2m.h$c>0 & Model2.2m.h$att[4]-Model2.2m.h$se[4]*Model2.2m.h$c>0 | Model2.2m.h$att[4]+Model2.2m.h$se[4]*Model2.2m.h$c<0 & Model2.2m.h$att[4]-Model2.2m.h$se[4]*Model2.2m.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.2m.h$se[4]*100,2), ")"), "", "", 
                   matrix(table(subset(Model2.2m.h$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.2m.h$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.2m.h$n*length(Model2.2m.h$t))
tabc6[1:16,8] <- c("15-19", 
                   paste0(round(Model2.3m.h$att[1]*100,2), ifelse(Model2.3m.h$att[1]+Model2.3m.h$se[1]*Model2.3m.h$c>0 & Model2.3m.h$att[1]-Model2.3m.h$se[1]*Model2.3m.h$c>0 | Model2.3m.h$att[1]+Model2.3m.h$se[1]*Model2.3m.h$c<0 & Model2.3m.h$att[1]-Model2.3m.h$se[1]*Model2.3m.h$c<0 , "$^{*}$", "")),
                   paste0("(",round(Model2.3m.h$se[1]*100,2),")"),
                   paste0(round(Model2.3m.h$att[2]*100,2), ifelse(Model2.3m.h$att[2]+Model2.3m.h$se[2]*Model2.3m.h$c>0 & Model2.3m.h$att[2]-Model2.3m.h$se[2]*Model2.3m.h$c>0 | Model2.3m.h$att[2]+Model2.3m.h$se[2]*Model2.3m.h$c<0 & Model2.3m.h$att[2]-Model2.3m.h$se[2]*Model2.3m.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3m.h$se[2]*100,2), ")"),
                   "ref", "", 
                   paste0(round(Model2.3m.h$att[4]*100,2), ifelse(Model2.3m.h$att[4]+Model2.3m.h$se[4]*Model2.3m.h$c>0 & Model2.3m.h$att[4]-Model2.3m.h$se[4]*Model2.3m.h$c>0 | Model2.3m.h$att[4]+Model2.3m.h$se[4]*Model2.3m.h$c<0 & Model2.3m.h$att[4]-Model2.3m.h$se[4]*Model2.3m.h$c<0 , "$^{*}$", "")),
                   paste0("(", round(Model2.3m.h$se[4]*100,2), ")"),
                   "", "", "", "",
                   matrix(table(subset(Model2.3m.h$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(Model2.3m.h$DIDparams$data)$hosd1_g)/4)[1,1],
                   Model2.3m.h$n*length(Model2.3m.h$t))
tabc6[1:16,9] <- c("Meta", 
                   paste0(round(ne.hig_ma$att[1],2), ifelse(ne.hig_ma$conf.high[1]>0 & ne.hig_ma$conf.low[1]>0 | ne.hig_ma$conf.high[1]<0 & ne.hig_ma$conf.low[1]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.hig_ma$se[1],2),")"),
                   paste0(round(ne.hig_ma$att[2],2), ifelse(ne.hig_ma$conf.high[2]>0 & ne.hig_ma$conf.low[2]>0 | ne.hig_ma$conf.high[2]<0 & ne.hig_ma$conf.low[2]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.hig_ma$se[2],2),")"),
                   "ref", "", 
                   paste0(round(ne.hig_ma$att[4],2), ifelse(ne.hig_ma$conf.high[4]>0 & ne.hig_ma$conf.low[4]>0 | ne.hig_ma$conf.high[4]<0 & ne.hig_ma$conf.low[4]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.hig_ma$se[4],2),")"), 
                   paste0(round(ne.hig_ma$att[5],2), ifelse(ne.hig_ma$conf.high[5]>0 & ne.hig_ma$conf.low[5]>0 | ne.hig_ma$conf.high[5]<0 & ne.hig_ma$conf.low[5]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.hig_ma$se[5],2),")"), 
                   paste0(round(ne.hig_ma$att[6],2), ifelse(ne.hig_ma$conf.high[6]>0 & ne.hig_ma$conf.low[6]>0 | ne.hig_ma$conf.high[6]<0 & ne.hig_ma$conf.low[6]<0 , "$^{*}$", "")),
                   paste0("(",round(ne.low_um$se[6],2),")"),
                   "-", "-", "-")
xtable(tabc6)
##Table C7####
tabc7 <- tibble()
tabc7[1:12,1] <- c("Treated year:",           "p-2", "","p-1", "", "p", "", "p+1", "", "Affected precincts", "Unaffected precincts", "Num. obs.")
tabc7[1:12,2] <- c("09-13","", "", "ref", "", 
                  paste0(round(rwp_le_schp_1u$att[2]*100,2), ifelse(rwp_le_schp_1u$att[2]+rwp_le_schp_1u$se[2]*rwp_le_schp_1u$c>0 & rwp_le_schp_1u$att[2]-rwp_le_schp_1u$se[2]*rwp_le_schp_1u$c>0 | rwp_le_schp_1u$att[2]+rwp_le_schp_1u$se[2]*rwp_le_schp_1u$c<0 & rwp_le_schp_1u$att[2]-rwp_le_schp_1u$se[2]*rwp_le_schp_1u$c<0 , "*", "")),
                  paste0("(", round(rwp_le_schp_1u$se[2]*100,2), ")"),
                  paste0(round(rwp_le_schp_1u$att[3]*100,2), ifelse(rwp_le_schp_1u$att[3]+rwp_le_schp_1u$se[3]*rwp_le_schp_1u$c>0 & rwp_le_schp_1u$att[3]-rwp_le_schp_1u$se[3]*rwp_le_schp_1u$c>0 | rwp_le_schp_1u$att[3]+rwp_le_schp_1u$se[3]*rwp_le_schp_1u$c<0 & rwp_le_schp_1u$att[3]-rwp_le_schp_1u$se[3]*rwp_le_schp_1u$c<0 , "*", "")),
                  paste0("(",round(rwp_le_schp_1u$se[3]*100,2),")"), 
                  matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[2,1],
                  matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[1,1],
                  (matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[2,1]+
                     matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[1,1])*
                    length(unique(rwp_le_schp_1u$t)))

tabc7[1:12,3] <- c("13-17",
                   paste0(round(rwp_le_schp_1u$att[4]*100,2), ifelse(rwp_le_schp_1u$att[4]+rwp_le_schp_1u$se[4]*rwp_le_schp_1u$c>0 & rwp_le_schp_1u$att[4]-rwp_le_schp_1u$se[4]*rwp_le_schp_1u$c>0 | rwp_le_schp_1u$att[4]+rwp_le_schp_1u$se[4]*rwp_le_schp_1u$c<0 & rwp_le_schp_1u$att[4]-rwp_le_schp_1u$se[4]*rwp_le_schp_1u$c<0 , "*", "")),
                   paste0("(", round(rwp_le_schp_1u$se[4]*100,2), ")"),
                   "ref", "", 
                   paste0(round(rwp_le_schp_1u$att[6]*100,2), ifelse(rwp_le_schp_1u$att[6]+rwp_le_schp_1u$se[6]*rwp_le_schp_1u$c>0 & rwp_le_schp_1u$att[6]-rwp_le_schp_1u$se[6]*rwp_le_schp_1u$c>0 | rwp_le_schp_1u$att[6]+rwp_le_schp_1u$se[6]*rwp_le_schp_1u$c<0 & rwp_le_schp_1u$att[6]-rwp_le_schp_1u$se[6]*rwp_le_schp_1u$c<0 , "*", "")),
                   paste0("(",round(rwp_le_schp_1u$se[6]*100,2),")"), 
                   "", "", 
                   matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[3,1],
                   matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[3,1]+
                     matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[1,1])*
                     length(unique(rwp_le_schp_1u$t)))

tabc7[1:12,4] <- c("Event study",
                    paste0(round(rwp.le$att[1],2), rwp.le$sig[1]),
                    paste0("(",round(rwp.le$se[1],2),")"),
                    "ref", "", 
                    paste0(round(rwp.le$att[3],2), rwp.le$sig[3]),
                    paste0("(",round(rwp.le$se[3],2),")"),
                    paste0(round(rwp.le$att[4],2), rwp.le$sig[4]),
                    paste0("(",round(rwp.le$se[4],2),")"),
                   matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[2,1]+
                     matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[3,1],
                   matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_schp_1u$DIDparams$data)$schp_g)/3)[1,1])*
                     length(unique(rwp_le_schp_1u$t)))

tabc7[1:12,5] <- c("09-13","", "", "ref", "", 
                   paste0(round(rwp_le_schp_1m$att[2]*100,2), ifelse(rwp_le_schp_1m$att[2]+rwp_le_schp_1m$se[2]*rwp_le_schp_1m$c>0 & rwp_le_schp_1m$att[2]-rwp_le_schp_1m$se[2]*rwp_le_schp_1m$c>0 | rwp_le_schp_1m$att[2]+rwp_le_schp_1m$se[2]*rwp_le_schp_1m$c<0 & rwp_le_schp_1m$att[2]-rwp_le_schp_1m$se[2]*rwp_le_schp_1m$c<0 , "*", "")),
                   paste0("(", round(rwp_le_schp_1m$se[2]*100,2), ")"),
                   paste0(round(rwp_le_schp_1m$att[3]*100,2), ifelse(rwp_le_schp_1m$att[3]+rwp_le_schp_1m$se[3]*rwp_le_schp_1m$c>0 & rwp_le_schp_1m$att[3]-rwp_le_schp_1m$se[3]*rwp_le_schp_1m$c>0 | rwp_le_schp_1m$att[3]+rwp_le_schp_1m$se[3]*rwp_le_schp_1m$c<0 & rwp_le_schp_1m$att[3]-rwp_le_schp_1m$se[3]*rwp_le_schp_1m$c<0 , "*", "")),
                   paste0("(",round(rwp_le_schp_1m$se[3]*100,2),")"), 
                   matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[2,1],
                   matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[1,1])*
                     length(unique(rwp_le_schp_1m$t)))

tabc7[1:12,6] <- c("13-17",
                   paste0(round(rwp_le_schp_1m$att[4]*100,2), ifelse(rwp_le_schp_1m$att[4]+rwp_le_schp_1m$se[4]*rwp_le_schp_1m$c>0 & rwp_le_schp_1m$att[4]-rwp_le_schp_1m$se[4]*rwp_le_schp_1m$c>0 | rwp_le_schp_1m$att[4]+rwp_le_schp_1m$se[4]*rwp_le_schp_1m$c<0 & rwp_le_schp_1m$att[4]-rwp_le_schp_1m$se[4]*rwp_le_schp_1m$c<0 , "*", "")),
                   paste0("(", round(rwp_le_schp_1m$se[4]*100,2), ")"),
                   "ref", "", 
                   paste0(round(rwp_le_schp_1m$att[6]*100,2), ifelse(rwp_le_schp_1m$att[6]+rwp_le_schp_1m$se[6]*rwp_le_schp_1m$c>0 & rwp_le_schp_1m$att[6]-rwp_le_schp_1m$se[6]*rwp_le_schp_1m$c>0 | rwp_le_schp_1m$att[6]+rwp_le_schp_1m$se[6]*rwp_le_schp_1m$c<0 & rwp_le_schp_1m$att[6]-rwp_le_schp_1m$se[6]*rwp_le_schp_1m$c<0 , "*", "")),
                   paste0("(",round(rwp_le_schp_1m$se[6]*100,2),")"), 
                   "", "", 
                   matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[3,1],
                   matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[1,1])*
                     length(unique(rwp_le_schp_1m$t)))
tabc7[1:12,7] <- c("Event study",
                   paste0(round(rwp.le$att[5],2), rwp.le$sig[5]),
                   paste0("(",round(rwp.le$se[5],2),")"),
                   "ref", "", 
                   paste0(round(rwp.le$att[7],2), rwp.le$sig[7]),
                   paste0("(",round(rwp.le$se[7],2),")"),
                   paste0(round(rwp.le$att[8],2), rwp.le$sig[8]),
                   paste0("(",round(rwp.le$se[8],2),")"),
                   matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[2,1]+
                     matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[3,1],
                   matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_schp_1m$DIDparams$data)$schp_g)/3)[1,1])*
                     length(unique(rwp_le_schp_1m$t)))
xtable(tabc7)

##Table C8####

tabc8 <- tibble()
tabc8[1:16,1] <- c("Treated year:", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "N Treated", "N Control", "Num. obs.")

tabc8[1:16,2] <- c("07-11", "", "","", "", "ref", "", 
                   paste0(round(rwp_ne_hosd1_1u$att[2]*100,2), ifelse(rwp_ne_hosd1_1u$att[2]+rwp_ne_hosd1_1u$se[2]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[2]-rwp_ne_hosd1_1u$se[2]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[2]+rwp_ne_hosd1_1u$se[2]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[2]-rwp_ne_hosd1_1u$se[2]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_hosd1_1u$se[2]*100,2), ")"),
                   paste0(round(rwp_ne_hosd1_1u$att[3]*100,2), ifelse(rwp_ne_hosd1_1u$att[3]+rwp_ne_hosd1_1u$se[3]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[3]-rwp_ne_hosd1_1u$se[3]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[3]+rwp_ne_hosd1_1u$se[3]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[3]-rwp_ne_hosd1_1u$se[3]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1u$se[3]*100,2),")"), 
                   paste0(round(rwp_ne_hosd1_1u$att[4]*100,2), ifelse(rwp_ne_hosd1_1u$att[4]+rwp_ne_hosd1_1u$se[4]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[4]-rwp_ne_hosd1_1u$se[4]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[4]+rwp_ne_hosd1_1u$se[4]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[4]-rwp_ne_hosd1_1u$se[4]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1u$se[4]*100,2),")"),
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_hosd1_1u$t)))

tabc8[1:16,3] <- c("11-15", "", "",  
                   paste0(round(rwp_ne_hosd1_1u$att[5]*100,2), ifelse(rwp_ne_hosd1_1u$att[5]+rwp_ne_hosd1_1u$se[5]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[5]-rwp_ne_hosd1_1u$se[5]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[5]+rwp_ne_hosd1_1u$se[5]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[5]-rwp_ne_hosd1_1u$se[5]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_hosd1_1u$se[5]*100,2), ")"),
                   "ref", "",
                   paste0(round(rwp_ne_hosd1_1u$att[7]*100,2), ifelse(rwp_ne_hosd1_1u$att[7]+rwp_ne_hosd1_1u$se[7]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[7]-rwp_ne_hosd1_1u$se[7]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[7]+rwp_ne_hosd1_1u$se[7]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[7]-rwp_ne_hosd1_1u$se[7]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1u$se[7]*100,2),")"), 
                   paste0(round(rwp_ne_hosd1_1u$att[8]*100,2), ifelse(rwp_ne_hosd1_1u$att[8]+rwp_ne_hosd1_1u$se[8]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[8]-rwp_ne_hosd1_1u$se[8]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[8]+rwp_ne_hosd1_1u$se[8]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[8]-rwp_ne_hosd1_1u$se[8]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1u$se[8]*100,2),")"),"", "",
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[3,1],
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[3,1]+
                      matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_hosd1_1u$t)))
tabc8[1:16,4] <- c("15-19",   
                   paste0(round(rwp_ne_hosd1_1u$att[9]*100,2), ifelse(rwp_ne_hosd1_1u$att[9]+rwp_ne_hosd1_1u$se[9]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[9]-rwp_ne_hosd1_1u$se[9]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[9]+rwp_ne_hosd1_1u$se[9]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[9]-rwp_ne_hosd1_1u$se[9]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_hosd1_1u$se[9]*100,2), ")"),
                   paste0(round(rwp_ne_hosd1_1u$att[10]*100,2), ifelse(rwp_ne_hosd1_1u$att[10]+rwp_ne_hosd1_1u$se[10]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[10]-rwp_ne_hosd1_1u$se[10]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[10]+rwp_ne_hosd1_1u$se[10]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[10]-rwp_ne_hosd1_1u$se[10]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_hosd1_1u$se[10]*100,2), ")"),
                   "ref", "",
                   paste0(round(rwp_ne_hosd1_1u$att[12]*100,2), ifelse(rwp_ne_hosd1_1u$att[12]+rwp_ne_hosd1_1u$se[12]*rwp_ne_hosd1_1u$c>0 & rwp_ne_hosd1_1u$att[12]-rwp_ne_hosd1_1u$se[12]*rwp_ne_hosd1_1u$c>0 | rwp_ne_hosd1_1u$att[12]+rwp_ne_hosd1_1u$se[12]*rwp_ne_hosd1_1u$c<0 & rwp_ne_hosd1_1u$att[12]-rwp_ne_hosd1_1u$se[12]*rwp_ne_hosd1_1u$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1u$se[12]*100,2),")"),
                   "", "", "", "",
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[4,1],
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[4,1]+
                      matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_hosd1_1u$t)))

tabc8[1:16,5] <- c("Event study", 
                   paste0(round(rwp.ne$att[1],2), ifelse(rwp.ne$conf.low[1]>0 & rwp.ne$conf.high[1]>0 | rwp.ne$conf.low[1]<0 & rwp.ne$conf.high[1]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[1],2),")"),
                   paste0(round(rwp.ne$att[2],2), ifelse(rwp.ne$conf.low[2]>0 & rwp.ne$conf.high[2]>0 | rwp.ne$conf.low[2]<0 & rwp.ne$conf.high[2]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[2],2),")"),
                   "ref", "", 
                   paste0(round(rwp.ne$att[4],2), ifelse(rwp.ne$conf.low[4]>0 & rwp.ne$conf.high[4]>0 | rwp.ne$conf.low[4]<0 & rwp.ne$conf.high[4]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[4],2),")"),
                   paste0(round(rwp.ne$att[5],2), ifelse(rwp.ne$conf.low[5]>0 & rwp.ne$conf.high[5]>0 | rwp.ne$conf.low[5]<0 & rwp.ne$conf.high[5]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[5],2),")"), 
                   paste0(round(rwp.ne$att[6],2), ifelse(rwp.ne$conf.low[6]>0 & rwp.ne$conf.high[6]>0 | rwp.ne$conf.low[6]<0 & rwp.ne$conf.high[6]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[6],2),")"), 
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[2,1]+
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[2,1]+
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[3,1],
                   matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[2,1]+
                    matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[3,1]+
                    matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[4,1]+
                    matrix(table(subset(rwp_ne_hosd1_1u$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_hosd1_1u$t)))

tabc8[1:16,6] <- c("07-11", "", "","", "", "ref", "", 
                   paste0(round(rwp_ne_hosd1_1m$att[2]*100,2), ifelse(rwp_ne_hosd1_1m$att[2]+rwp_ne_hosd1_1m$se[2]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[2]-rwp_ne_hosd1_1m$se[2]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[2]+rwp_ne_hosd1_1m$se[2]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[2]-rwp_ne_hosd1_1m$se[2]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_hosd1_1m$se[2]*100,2), ")"),
                   paste0(round(rwp_ne_hosd1_1m$att[3]*100,2), ifelse(rwp_ne_hosd1_1m$att[3]+rwp_ne_hosd1_1m$se[3]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[3]-rwp_ne_hosd1_1m$se[3]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[3]+rwp_ne_hosd1_1m$se[3]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[3]-rwp_ne_hosd1_1m$se[3]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1m$se[3]*100,2),")"), 
                   paste0(round(rwp_ne_hosd1_1m$att[4]*100,2), ifelse(rwp_ne_hosd1_1m$att[4]+rwp_ne_hosd1_1m$se[4]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[4]-rwp_ne_hosd1_1m$se[4]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[4]+rwp_ne_hosd1_1m$se[4]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[4]-rwp_ne_hosd1_1m$se[4]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1m$se[4]*100,2),")"),
                   matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_hosd1_1m$t)))

tabc8[1:16,7] <- c("11-15", "", "",  
                   paste0(round(rwp_ne_hosd1_1m$att[5]*100,2), ifelse(rwp_ne_hosd1_1m$att[5]+rwp_ne_hosd1_1m$se[5]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[5]-rwp_ne_hosd1_1m$se[5]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[5]+rwp_ne_hosd1_1m$se[5]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[5]-rwp_ne_hosd1_1m$se[5]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_hosd1_1m$se[5]*100,2), ")"),
                   "ref", "",
                   paste0(round(rwp_ne_hosd1_1m$att[7]*100,2), ifelse(rwp_ne_hosd1_1m$att[7]+rwp_ne_hosd1_1m$se[7]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[7]-rwp_ne_hosd1_1m$se[7]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[7]+rwp_ne_hosd1_1m$se[7]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[7]-rwp_ne_hosd1_1m$se[7]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1m$se[7]*100,2),")"), 
                   paste0(round(rwp_ne_hosd1_1m$att[8]*100,2), ifelse(rwp_ne_hosd1_1m$att[8]+rwp_ne_hosd1_1m$se[8]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[8]-rwp_ne_hosd1_1m$se[8]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[8]+rwp_ne_hosd1_1m$se[8]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[8]-rwp_ne_hosd1_1m$se[8]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1m$se[8]*100,2),")"),"", "",
                   matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[3,1],
                   matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[3,1]+
                      matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_hosd1_1m$t)))

tabc8[1:16,8] <- c("15-19",   
                   paste0(round(rwp_ne_hosd1_1m$att[9]*100,2), ifelse(rwp_ne_hosd1_1m$att[9]+rwp_ne_hosd1_1m$se[9]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[9]-rwp_ne_hosd1_1m$se[9]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[9]+rwp_ne_hosd1_1m$se[9]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[9]-rwp_ne_hosd1_1m$se[9]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_hosd1_1m$se[9]*100,2), ")"),
                   paste0(round(rwp_ne_hosd1_1m$att[10]*100,2), ifelse(rwp_ne_hosd1_1m$att[10]+rwp_ne_hosd1_1m$se[10]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[10]-rwp_ne_hosd1_1m$se[10]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[10]+rwp_ne_hosd1_1m$se[10]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[10]-rwp_ne_hosd1_1m$se[10]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_hosd1_1m$se[10]*100,2), ")"),
                   "ref", "",
                   paste0(round(rwp_ne_hosd1_1m$att[12]*100,2), ifelse(rwp_ne_hosd1_1m$att[12]+rwp_ne_hosd1_1m$se[12]*rwp_ne_hosd1_1m$c>0 & rwp_ne_hosd1_1m$att[12]-rwp_ne_hosd1_1m$se[12]*rwp_ne_hosd1_1m$c>0 | rwp_ne_hosd1_1m$att[12]+rwp_ne_hosd1_1m$se[12]*rwp_ne_hosd1_1m$c<0 & rwp_ne_hosd1_1m$att[12]-rwp_ne_hosd1_1m$se[12]*rwp_ne_hosd1_1m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_hosd1_1m$se[12]*100,2),")"),
                   "", "", "", "",
                   matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[4,1],
                   matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[4,1]+
                      matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_hosd1_1m$t)))

tabc8[1:16,9] <- c("Event study", 
                   paste0(round(rwp.ne$att[7],2), ifelse(rwp.ne$conf.low[7]>0 & rwp.ne$conf.high[7]>0 | rwp.ne$conf.low[7]<0 & rwp.ne$conf.high[7]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[7],2),")"),
                   paste0(round(rwp.ne$att[8],2), ifelse(rwp.ne$conf.low[8]>0 & rwp.ne$conf.high[8]>0 | rwp.ne$conf.low[8]<0 & rwp.ne$conf.high[8]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[8],2),")"),
                   "ref", "", 
                   paste0(round(rwp.ne$att[10],2), ifelse(rwp.ne$conf.low[10]>0 & rwp.ne$conf.high[10]>0 | rwp.ne$conf.low[10]<0 & rwp.ne$conf.high[10]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[10],2),")"),
                   paste0(round(rwp.ne$att[11],2), ifelse(rwp.ne$conf.low[11]>0 & rwp.ne$conf.high[11]>0 | rwp.ne$conf.low[11]<0 & rwp.ne$conf.high[11]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[11],2),")"), 
                   paste0(round(rwp.ne$att[12],2), ifelse(rwp.ne$conf.low[12]>0 & rwp.ne$conf.high[12]>0 | rwp.ne$conf.low[12]<0 & rwp.ne$conf.high[12]<0 , "$^{*}$", "")),
                   paste0("(",round(rwp.ne$se[12],2),")"), 
                   matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[2,1]+
                     matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[2,1]+
                     matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[3,1],
                   matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[3,1]+
                      matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[4,1]+
                      matrix(table(subset(rwp_ne_hosd1_1m$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_hosd1_1m$t)))


xtable(tabc8)
##Table C9####
tabc9 <- tibble()

tabc9[1:12,1] <- c("Treated year:", "p-2", "","p-1", "", "p", "", "p+1", "",  "N Treated", "N Control", "Num. obs.")
tabc9[1:12,2] <- c("09-13", "", "", "ref", "", 
                   paste0(round(rwp_le_l$att[2]*100,2), ifelse(rwp_le_l$att[2]+rwp_le_l$se[2]*rwp_le_l$c>0 & rwp_le_l$att[2]-rwp_le_l$se[2]*rwp_le_l$c>0 | rwp_le_l$att[2]+rwp_le_l$se[2]*rwp_le_l$c<0 & rwp_le_l$att[2]-rwp_le_l$se[2]*rwp_le_l$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_le_l$se[2]*100,2), ")"),
                   paste0(round(rwp_le_l$att[3]*100,2), ifelse(rwp_le_l$att[3]+rwp_le_l$se[3]*rwp_le_l$c>0 & rwp_le_l$att[3]-rwp_le_l$se[3]*rwp_le_l$c>0 | rwp_le_l$att[3]+rwp_le_l$se[3]*rwp_le_l$c<0 & rwp_le_l$att[3]-rwp_le_l$se[3]*rwp_le_l$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_le_l$se[3]*100,2),")"), 
                   matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[2,1],
                   matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_l$t)))

tabc9[1:12,3] <- c("13-17",
                   paste0(round(rwp_le_l$att[4]*100,2), ifelse(rwp_le_l$att[4]+rwp_le_l$se[4]*rwp_le_l$c>0 & rwp_le_l$att[4]-rwp_le_l$se[4]*rwp_le_l$c>0 | rwp_le_l$att[4]+rwp_le_l$se[4]*rwp_le_l$c<0 & rwp_le_l$att[4]-rwp_le_l$se[4]*rwp_le_l$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_le_l$se[4]*100,2),")"),
                   "ref", "", 
                   paste0(round(rwp_le_l$att[6]*100,2), ifelse(rwp_le_l$att[6]+rwp_le_l$se[6]*rwp_le_l$c>0 & rwp_le_l$att[6]-rwp_le_l$se[6]*rwp_le_l$c>0 | rwp_le_l$att[6]+rwp_le_l$se[6]*rwp_le_l$c<0 & rwp_le_l$att[6]-rwp_le_l$se[6]*rwp_le_l$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_le_l$se[6]*100,2), ")"),"", "",
                   matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[3,1],
                   matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_l$t)))

tabc9[1:12,4] <- c("Event study",
                   paste0(round(le.low$att[1],2), le.low$sig[1]),
                   paste0("(",round(le.low$se[1],2),")"),
                   "ref", "", 
                   paste0(round(le.low$att[3],2), le.low$sig[3]),
                   paste0("(",round(le.low$se[3],2),")"),
                   paste0(round(le.low$att[4],2), le.low$sig[4]),
                   paste0("(",round(le.low$se[4],2),")"),
                   matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[2,1]+
                     matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[3,1],
                   matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_l$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_l$t)))

tabc9[1:12,5] <- c("09-13", "", "", "ref", "", 
                   paste0(round(rwp_le_h$att[2]*100,2), ifelse(rwp_le_h$att[2]+rwp_le_h$se[2]*rwp_le_h$c>0 & rwp_le_h$att[2]-rwp_le_h$se[2]*rwp_le_h$c>0 | rwp_le_h$att[2]+rwp_le_h$se[2]*rwp_le_h$c<0 & rwp_le_h$att[2]-rwp_le_h$se[2]*rwp_le_h$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_le_h$se[2]*100,2), ")"),
                   paste0(round(rwp_le_h$att[3]*100,2), ifelse(rwp_le_h$att[3]+rwp_le_h$se[3]*rwp_le_h$c>0 & rwp_le_h$att[3]-rwp_le_h$se[3]*rwp_le_h$c>0 | rwp_le_h$att[3]+rwp_le_h$se[3]*rwp_le_h$c<0 & rwp_le_h$att[3]-rwp_le_h$se[3]*rwp_le_h$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_le_h$se[3]*100,2),")"), 
                   matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[2,1],
                   matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_h$t)))

tabc9[1:12,6] <- c("13-17",
                   paste0(round(rwp_le_h$att[4]*100,2), ifelse(rwp_le_h$att[4]+rwp_le_h$se[4]*rwp_le_h$c>0 & rwp_le_h$att[4]-rwp_le_h$se[4]*rwp_le_h$c>0 | rwp_le_h$att[4]+rwp_le_h$se[4]*rwp_le_h$c<0 & rwp_le_h$att[4]-rwp_le_h$se[4]*rwp_le_h$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_le_h$se[4]*100,2),")"),
                   "ref", "", 
                   paste0(round(rwp_le_h$att[6]*100,2), ifelse(rwp_le_h$att[6]+rwp_le_h$se[6]*rwp_le_h$c>0 & rwp_le_h$att[6]-rwp_le_h$se[6]*rwp_le_h$c>0 | rwp_le_h$att[6]+rwp_le_h$se[6]*rwp_le_h$c<0 & rwp_le_h$att[6]-rwp_le_h$se[6]*rwp_le_h$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_le_h$se[6]*100,2), ")"),"", "",
                   matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[3,1],
                   matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_h$t)))

tabc9[1:12,7] <- c("Event study",
                   paste0(round(le.hig$att[1],2), le.hig$sig[1]),
                   paste0("(",round(le.hig$se[1],2),")"),
                   "ref", "", 
                   paste0(round(le.hig$att[3],2), le.hig$sig[3]),
                   paste0("(",round(le.hig$se[3],2),")"),
                   paste0(round(le.hig$att[4],2), le.hig$sig[4]),
                   paste0("(",round(le.hig$se[4],2),")"),
                   matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[2,1]+
                     matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[3,1],
                   matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_h$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_h$t)))



xtable(tabc9)

##Table C10####
tabc10 <- tibble()

tabc10[1:12,1] <- c("Treated year:", "p-2", "","p-1", "", "p", "", "p+1", "", "N Treated", "N Control", "Num. obs.")
tabc10[1:12,2] <- c("09-13", "", "", "ref", "", 
                   paste0(round(rwp_le_l.m$att[2]*100,2), ifelse(rwp_le_l.m$att[2]+rwp_le_l.m$se[2]*rwp_le_l.m$c>0 & rwp_le_l.m$att[2]-rwp_le_l.m$se[2]*rwp_le_l.m$c>0 | rwp_le_l.m$att[2]+rwp_le_l.m$se[2]*rwp_le_l.m$c<0 & rwp_le_l.m$att[2]-rwp_le_l.m$se[2]*rwp_le_l.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_le_l.m$se[2]*100,2), ")"),
                   paste0(round(rwp_le_l.m$att[3]*100,2), ifelse(rwp_le_l.m$att[3]+rwp_le_l.m$se[3]*rwp_le_l.m$c>0 & rwp_le_l.m$att[3]-rwp_le_l.m$se[3]*rwp_le_l.m$c>0 | rwp_le_l.m$att[3]+rwp_le_l.m$se[3]*rwp_le_l.m$c<0 & rwp_le_l.m$att[3]-rwp_le_l.m$se[3]*rwp_le_l.m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_le_l.m$se[3]*100,2),")"), 
                   matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[2,1],
                   matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_l.m$t)))

tabc10[1:12,3] <- c("13-17",
                   paste0(round(rwp_le_l.m$att[4]*100,2), ifelse(rwp_le_l.m$att[4]+rwp_le_l.m$se[4]*rwp_le_l.m$c>0 & rwp_le_l.m$att[4]-rwp_le_l.m$se[4]*rwp_le_l.m$c>0 | rwp_le_l.m$att[4]+rwp_le_l.m$se[4]*rwp_le_l.m$c<0 & rwp_le_l.m$att[4]-rwp_le_l.m$se[4]*rwp_le_l.m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_le_l.m$se[4]*100,2),")"),
                   "ref", "", 
                   paste0(round(rwp_le_l.m$att[6]*100,2), ifelse(rwp_le_l.m$att[6]+rwp_le_l.m$se[6]*rwp_le_l.m$c>0 & rwp_le_l.m$att[6]-rwp_le_l.m$se[6]*rwp_le_l.m$c>0 | rwp_le_l.m$att[6]+rwp_le_l.m$se[6]*rwp_le_l.m$c<0 & rwp_le_l.m$att[6]-rwp_le_l.m$se[6]*rwp_le_l.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_le_l.m$se[6]*100,2), ")"),"", "",
                   matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[3,1],
                   matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_l.m$t)))

tabc10[1:12,4] <- c("Event study",
                   paste0(round(le.low$att[5],2), le.low$sig[5]),
                   paste0("(",round(le.low$se[5],2),")"),
                   "ref", "", 
                   paste0(round(le.low$att[7],2), le.low$sig[7]),
                   paste0("(",round(le.low$se[7],2),")"),
                   paste0(round(le.low$att[8],2), le.low$sig[8]),
                   paste0("(",round(le.low$se[8],2),")"),
                   matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[2,1]+
                     matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[3,1],
                   matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_l.m$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_l.m$t)))

tabc10[1:12,5] <- c("09-13", "", "", "ref", "", 
                   paste0(round(rwp_le_h.m$att[2]*100,2), ifelse(rwp_le_h.m$att[2]+rwp_le_h.m$se[2]*rwp_le_h.m$c>0 & rwp_le_h.m$att[2]-rwp_le_h.m$se[2]*rwp_le_h.m$c>0 | rwp_le_h.m$att[2]+rwp_le_h.m$se[2]*rwp_le_h.m$c<0 & rwp_le_h.m$att[2]-rwp_le_h.m$se[2]*rwp_le_h.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_le_h.m$se[2]*100,2), ")"),
                   paste0(round(rwp_le_h.m$att[3]*100,2), ifelse(rwp_le_h.m$att[3]+rwp_le_h.m$se[3]*rwp_le_h.m$c>0 & rwp_le_h.m$att[3]-rwp_le_h.m$se[3]*rwp_le_h.m$c>0 | rwp_le_h.m$att[3]+rwp_le_h.m$se[3]*rwp_le_h.m$c<0 & rwp_le_h.m$att[3]-rwp_le_h.m$se[3]*rwp_le_h.m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_le_h.m$se[3]*100,2),")"), 
                   matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[2,1],
                   matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_h.m$t)))

tabc10[1:12,6] <- c("13-17",
                   paste0(round(rwp_le_h.m$att[4]*100,2), ifelse(rwp_le_h.m$att[4]+rwp_le_h.m$se[4]*rwp_le_h.m$c>0 & rwp_le_h.m$att[4]-rwp_le_h.m$se[4]*rwp_le_h.m$c>0 | rwp_le_h.m$att[4]+rwp_le_h.m$se[4]*rwp_le_h.m$c<0 & rwp_le_h.m$att[4]-rwp_le_h.m$se[4]*rwp_le_h.m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_le_h.m$se[4]*100,2),")"),
                   "ref", "", 
                   paste0(round(rwp_le_h.m$att[6]*100,2), ifelse(rwp_le_h.m$att[6]+rwp_le_h.m$se[6]*rwp_le_h.m$c>0 & rwp_le_h.m$att[6]-rwp_le_h.m$se[6]*rwp_le_h.m$c>0 | rwp_le_h.m$att[6]+rwp_le_h.m$se[6]*rwp_le_h.m$c<0 & rwp_le_h.m$att[6]-rwp_le_h.m$se[6]*rwp_le_h.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_le_h.m$se[6]*100,2), ")"),"", "",
                   matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[3,1],
                   matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_h.m$t)))

tabc10[1:12,7] <- c("Event study",
                   paste0(round(le.hig$att[5],2), le.hig$sig[5]),
                   paste0("(",round(le.hig$se[5],2),")"),
                   "ref", "", 
                   paste0(round(le.hig$att[7],2), le.hig$sig[7]),
                   paste0("(",round(le.hig$se[7],2),")"),
                   paste0(round(le.hig$att[8],2), le.hig$sig[8]),
                   paste0("(",round(le.hig$se[8],2),")"),
                   matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[2,1]+
                     matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[3,1],
                   matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[1,1],
                   (matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[2,1]+
                      matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[3,1]+
                      matrix(table(subset(rwp_le_h.m$DIDparams$data)$schd1_g)/3)[1,1])*
                     length(unique(rwp_le_h.m$t)))

xtable(tabc10)
##Table C11####
tabc11 <- tibble()

tabc11[1:16,1] <- c("Treated year:", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "N Treated", "N Control", "Num. obs.")
tabc11[1:16,2] <- c("07-11", "", "","", "", "ref", "", 
                    paste0(round(rwp_ne_l$att[2]*100,2), ifelse(rwp_ne_l$att[2]+rwp_ne_l$se[2]*rwp_ne_l$c>0 & rwp_ne_l$att[2]-rwp_ne_l$se[2]*rwp_ne_l$c>0 | rwp_ne_l$att[2]+rwp_ne_l$se[2]*rwp_ne_l$c<0 & rwp_ne_l$att[2]-rwp_ne_l$se[2]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_l$se[2]*100,2), ")"),
                    paste0(round(rwp_ne_l$att[3]*100,2), ifelse(rwp_ne_l$att[3]+rwp_ne_l$se[3]*rwp_ne_l$c>0 & rwp_ne_l$att[3]-rwp_ne_l$se[3]*rwp_ne_l$c>0 | rwp_ne_l$att[3]+rwp_ne_l$se[3]*rwp_ne_l$c<0 & rwp_ne_l$att[3]-rwp_ne_l$se[3]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_l$se[3]*100,2),")"), 
                    paste0(round(rwp_ne_l$att[4]*100,2), ifelse(rwp_ne_l$att[4]+rwp_ne_l$se[4]*rwp_ne_l$c>0 & rwp_ne_l$att[4]-rwp_ne_l$se[4]*rwp_ne_l$c>0 | rwp_ne_l$att[4]+rwp_ne_l$se[4]*rwp_ne_l$c<0 & rwp_ne_l$att[4]-rwp_ne_l$se[4]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_l$se[4]*100,2),")"), 
                    matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[2,1],
                    matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[2,1]+
                       matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_l$t)))

tabc11[1:16,3] <- c("11-15", "", "",
                    paste0(round(rwp_ne_l$att[5]*100,2), ifelse(rwp_ne_l$att[5]+rwp_ne_l$se[5]*rwp_ne_l$c>0 & rwp_ne_l$att[5]-rwp_ne_l$se[5]*rwp_ne_l$c>0 | rwp_ne_l$att[5]+rwp_ne_l$se[5]*rwp_ne_l$c<0 & rwp_ne_l$att[5]-rwp_ne_l$se[5]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_l$se[5]*100,2),")"),
                    "ref", "", 
                    paste0(round(rwp_ne_l$att[7]*100,2), ifelse(rwp_ne_l$att[7]+rwp_ne_l$se[7]*rwp_ne_l$c>0 & rwp_ne_l$att[7]-rwp_ne_l$se[7]*rwp_ne_l$c>0 | rwp_ne_l$att[7]+rwp_ne_l$se[7]*rwp_ne_l$c<0 & rwp_ne_l$att[7]-rwp_ne_l$se[7]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_l$se[7]*100,2), ")"),
                    paste0(round(rwp_ne_l$att[8]*100,2), ifelse(rwp_ne_l$att[8]+rwp_ne_l$se[8]*rwp_ne_l$c>0 & rwp_ne_l$att[8]-rwp_ne_l$se[8]*rwp_ne_l$c>0 | rwp_ne_l$att[8]+rwp_ne_l$se[8]*rwp_ne_l$c<0 & rwp_ne_l$att[8]-rwp_ne_l$se[8]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_l$se[8]*100,2), ")"), "", "", 
                    matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[3,1],
                    matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[3,1]+
                       matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_l$t)))

tabc11[1:16,4] <- c("15-19", 
                    paste0(round(rwp_ne_l$att[9]*100,2), ifelse(rwp_ne_l$att[9]+rwp_ne_l$se[9]*rwp_ne_l$c>0 & rwp_ne_l$att[9]-rwp_ne_l$se[9]*rwp_ne_l$c>0 | rwp_ne_l$att[9]+rwp_ne_l$se[9]*rwp_ne_l$c<0 & rwp_ne_l$att[9]-rwp_ne_l$se[9]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_l$se[9]*100,2),")"),
                    paste0(round(rwp_ne_l$att[10]*100,2), ifelse(rwp_ne_l$att[10]+rwp_ne_l$se[10]*rwp_ne_l$c>0 & rwp_ne_l$att[10]-rwp_ne_l$se[10]*rwp_ne_l$c>0 | rwp_ne_l$att[10]+rwp_ne_l$se[10]*rwp_ne_l$c<0 & rwp_ne_l$att[10]-rwp_ne_l$se[10]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_l$se[10]*100,2), ")"),
                    "ref", "", 
                    paste0(round(rwp_ne_l$att[12]*100,2), ifelse(rwp_ne_l$att[12]+rwp_ne_l$se[12]*rwp_ne_l$c>0 & rwp_ne_l$att[12]-rwp_ne_l$se[12]*rwp_ne_l$c>0 | rwp_ne_l$att[12]+rwp_ne_l$se[12]*rwp_ne_l$c<0 & rwp_ne_l$att[12]-rwp_ne_l$se[12]*rwp_ne_l$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_l$se[12]*100,2), ")"),
                    "", "", "", "",
                    matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[4,1],
                    matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[4,1]+
                       matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_l$t)))

tabc11[1:16,5] <- c("Event study",
                    paste0(round(ne.low$att[1],2), ne.low$sig[1]),
                    paste0("(",round(ne.low$se[1],2),")"),
                    paste0(round(ne.low$att[2],2), ne.low$sig[2]),
                    paste0("(",round(ne.low$se[2],2),")"),
                    "ref", "", 
                    paste0(round(ne.low$att[4],2), ne.hig$sig[4]),
                    paste0("(",round(ne.low$se[4],2),")"),
                    paste0(round(ne.low$att[5],2), ne.low$sig[5]),
                    paste0("(",round(ne.low$se[5],2),")"),
                    paste0(round(ne.low$att[6],2), ne.low$sig[6]),
                    paste0("(",round(ne.low$se[6],2),")"),
                    matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[3,1],
                    matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[2,1]+
                       matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[3,1]+
                       matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[4,1]+
                       matrix(table(subset(rwp_ne_l$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_l$t)))

tabc11[1:16,6] <- c("07-11", "", "","", "", "ref", "", 
                    paste0(round(rwp_ne_h$att[2]*100,2), ifelse(rwp_ne_h$att[2]+rwp_ne_h$se[2]*rwp_ne_h$c>0 & rwp_ne_h$att[2]-rwp_ne_h$se[2]*rwp_ne_h$c>0 | rwp_ne_h$att[2]+rwp_ne_h$se[2]*rwp_ne_h$c<0 & rwp_ne_h$att[2]-rwp_ne_h$se[2]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h$se[2]*100,2), ")"),
                    paste0(round(rwp_ne_h$att[3]*100,2), ifelse(rwp_ne_h$att[3]+rwp_ne_h$se[3]*rwp_ne_h$c>0 & rwp_ne_h$att[3]-rwp_ne_h$se[3]*rwp_ne_h$c>0 | rwp_ne_h$att[3]+rwp_ne_h$se[3]*rwp_ne_h$c<0 & rwp_ne_h$att[3]-rwp_ne_h$se[3]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_h$se[3]*100,2),")"), 
                    paste0(round(rwp_ne_h$att[4]*100,2), ifelse(rwp_ne_h$att[4]+rwp_ne_h$se[4]*rwp_ne_h$c>0 & rwp_ne_h$att[4]-rwp_ne_h$se[4]*rwp_ne_h$c>0 | rwp_ne_h$att[4]+rwp_ne_h$se[4]*rwp_ne_h$c<0 & rwp_ne_h$att[4]-rwp_ne_h$se[4]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_h$se[4]*100,2),")"), 
                    matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[2,1],
                    matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[2,1]+
                       matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_h$t)))

tabc11[1:16,7] <- c("11-15", "", "",
                    paste0(round(rwp_ne_h$att[5]*100,2), ifelse(rwp_ne_h$att[5]+rwp_ne_h$se[5]*rwp_ne_h$c>0 & rwp_ne_h$att[5]-rwp_ne_h$se[5]*rwp_ne_h$c>0 | rwp_ne_h$att[5]+rwp_ne_h$se[5]*rwp_ne_h$c<0 & rwp_ne_h$att[5]-rwp_ne_h$se[5]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_h$se[5]*100,2),")"),
                    "ref", "", 
                    paste0(round(rwp_ne_h$att[7]*100,2), ifelse(rwp_ne_h$att[7]+rwp_ne_h$se[7]*rwp_ne_h$c>0 & rwp_ne_h$att[7]-rwp_ne_h$se[7]*rwp_ne_h$c>0 | rwp_ne_h$att[7]+rwp_ne_h$se[7]*rwp_ne_h$c<0 & rwp_ne_h$att[7]-rwp_ne_h$se[7]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h$se[7]*100,2), ")"),
                    paste0(round(rwp_ne_h$att[8]*100,2), ifelse(rwp_ne_h$att[8]+rwp_ne_h$se[8]*rwp_ne_h$c>0 & rwp_ne_h$att[8]-rwp_ne_h$se[8]*rwp_ne_h$c>0 | rwp_ne_h$att[8]+rwp_ne_h$se[8]*rwp_ne_h$c<0 & rwp_ne_h$att[8]-rwp_ne_h$se[8]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h$se[8]*100,2), ")"), "", "", 
                    matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[3,1],
                    matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[3,1]+
                       matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_h$t)))

tabc11[1:16,8] <- c("15-19", 
                    paste0(round(rwp_ne_h$att[9]*100,2), ifelse(rwp_ne_h$att[9]+rwp_ne_h$se[9]*rwp_ne_h$c>0 & rwp_ne_h$att[9]-rwp_ne_h$se[9]*rwp_ne_h$c>0 | rwp_ne_h$att[9]+rwp_ne_h$se[9]*rwp_ne_h$c<0 & rwp_ne_h$att[9]-rwp_ne_h$se[9]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_h$se[9]*100,2),")"),
                    paste0(round(rwp_ne_h$att[10]*100,2), ifelse(rwp_ne_h$att[10]+rwp_ne_h$se[10]*rwp_ne_h$c>0 & rwp_ne_h$att[10]-rwp_ne_h$se[10]*rwp_ne_h$c>0 | rwp_ne_h$att[10]+rwp_ne_h$se[10]*rwp_ne_h$c<0 & rwp_ne_h$att[10]-rwp_ne_h$se[10]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h$se[10]*100,2), ")"),
                    "ref", "", 
                    paste0(round(rwp_ne_h$att[12]*100,2), ifelse(rwp_ne_h$att[12]+rwp_ne_h$se[12]*rwp_ne_h$c>0 & rwp_ne_h$att[12]-rwp_ne_h$se[12]*rwp_ne_h$c>0 | rwp_ne_h$att[12]+rwp_ne_h$se[12]*rwp_ne_h$c<0 & rwp_ne_h$att[12]-rwp_ne_h$se[12]*rwp_ne_h$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h$se[12]*100,2), ")"),
                    "", "", "", "",
                    matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[4,1],
                    matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[4,1]+
                       matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_h$t)))

tabc11[1:16,9] <- c("Event study",
                    paste0(round(ne.hig$att[1],2), ne.hig$sig[1]),
                    paste0("(",round(ne.hig$se[1],2),")"),
                    paste0(round(ne.hig$att[2],2), ne.hig$sig[2]),
                    paste0("(",round(ne.hig$se[2],2),")"),
                    "ref", "", 
                    paste0(round(ne.hig$att[4],2), ne.hig$sig[4]),
                    paste0("(",round(ne.hig$se[4],2),")"),
                    paste0(round(ne.hig$att[5],2), ne.hig$sig[5]),
                    paste0("(",round(ne.hig$se[5],2),")"),
                    paste0(round(ne.hig$att[6],2), ne.hig$sig[6]),
                    paste0("(",round(ne.hig$se[6],2),")"),
                    matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[3,1],
                    matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[2,1]+
                       matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[3,1]+
                       matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[4,1]+
                       matrix(table(subset(rwp_ne_h$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_h$t)))

xtable(tabc11)

##Table C12####
tabc12 <- tibble()

tabc12[1:16,1] <- c("Treated year:", "p-3", "","p-2", "","p-1", "", "p", "", "p+1", "", "p+2", "", "N Treated", "N Control", "Num. obs.")
tabc12[1:16,2] <- c("07-11", "", "","", "", "ref", "", 
                   paste0(round(rwp_ne_l.m$att[2]*100,2), ifelse(rwp_ne_l.m$att[2]+rwp_ne_l.m$se[2]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[2]-rwp_ne_l.m$se[2]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[2]+rwp_ne_l.m$se[2]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[2]-rwp_ne_l.m$se[2]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_l.m$se[2]*100,2), ")"),
                   paste0(round(rwp_ne_l.m$att[3]*100,2), ifelse(rwp_ne_l.m$att[3]+rwp_ne_l.m$se[3]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[3]-rwp_ne_l.m$se[3]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[3]+rwp_ne_l.m$se[3]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[3]-rwp_ne_l.m$se[3]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_l.m$se[3]*100,2),")"), 
                   paste0(round(rwp_ne_l.m$att[4]*100,2), ifelse(rwp_ne_l.m$att[4]+rwp_ne_l.m$se[4]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[4]-rwp_ne_l.m$se[4]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[4]+rwp_ne_l.m$se[4]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[4]-rwp_ne_l.m$se[4]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_l.m$se[4]*100,2),")"), 
                   matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[2,1],
                   matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_l.m$t)))

tabc12[1:16,3] <- c("11-15", "", "",
                   paste0(round(rwp_ne_l.m$att[5]*100,2), ifelse(rwp_ne_l.m$att[5]+rwp_ne_l.m$se[5]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[5]-rwp_ne_l.m$se[5]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[5]+rwp_ne_l.m$se[5]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[5]-rwp_ne_l.m$se[5]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_l.m$se[5]*100,2),")"),
                   "ref", "", 
                   paste0(round(rwp_ne_l.m$att[7]*100,2), ifelse(rwp_ne_l.m$att[7]+rwp_ne_l.m$se[7]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[7]-rwp_ne_l.m$se[7]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[7]+rwp_ne_l.m$se[7]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[7]-rwp_ne_l.m$se[7]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_l.m$se[7]*100,2), ")"),
                   paste0(round(rwp_ne_l.m$att[8]*100,2), ifelse(rwp_ne_l.m$att[8]+rwp_ne_l.m$se[8]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[8]-rwp_ne_l.m$se[8]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[8]+rwp_ne_l.m$se[8]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[8]-rwp_ne_l.m$se[8]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_l.m$se[8]*100,2), ")"), "", "", 
                   matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[3,1],
                   matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[3,1]+
                      matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_l.m$t)))

tabc12[1:16,4] <- c("15-19", 
                   paste0(round(rwp_ne_l.m$att[9]*100,2), ifelse(rwp_ne_l.m$att[9]+rwp_ne_l.m$se[9]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[9]-rwp_ne_l.m$se[9]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[9]+rwp_ne_l.m$se[9]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[9]-rwp_ne_l.m$se[9]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(",round(rwp_ne_l.m$se[9]*100,2),")"),
                   paste0(round(rwp_ne_l.m$att[10]*100,2), ifelse(rwp_ne_l.m$att[10]+rwp_ne_l.m$se[10]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[10]-rwp_ne_l.m$se[10]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[10]+rwp_ne_l.m$se[10]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[10]-rwp_ne_l.m$se[10]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_l.m$se[10]*100,2), ")"),
                   "ref", "", 
                   paste0(round(rwp_ne_l.m$att[12]*100,2), ifelse(rwp_ne_l.m$att[12]+rwp_ne_l.m$se[12]*rwp_ne_l.m$c>0 & rwp_ne_l.m$att[12]-rwp_ne_l.m$se[12]*rwp_ne_l.m$c>0 | rwp_ne_l.m$att[12]+rwp_ne_l.m$se[12]*rwp_ne_l.m$c<0 & rwp_ne_l.m$att[12]-rwp_ne_l.m$se[12]*rwp_ne_l.m$c<0 , "$^{*}$", "")),
                   paste0("(", round(rwp_ne_l.m$se[12]*100,2), ")"),
                   "", "", "", "",
                   matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[4,1],
                   matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[1,1],
                   (matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[4,1]+
                      matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[1,1])*
                     length(unique(rwp_ne_l.m$t)))
tabc12[1:16,5] <- c("Event study",
                    paste0(round(ne.low$att[7],2), ne.low$sig[7]),
                    paste0("(",round(ne.low$se[7],2),")"),
                    paste0(round(ne.low$att[8],2), ne.low$sig[8]),
                    paste0("(",round(ne.low$se[8],2),")"),
                    "ref", "", 
                    paste0(round(ne.low$att[10],2), ne.hig$sig[10]),
                    paste0("(",round(ne.low$se[10],2),")"),
                    paste0(round(ne.low$att[11],2), ne.low$sig[11]),
                    paste0("(",round(ne.low$se[11],2),")"),
                    paste0(round(ne.low$att[12],2), ne.low$sig[12]),
                    paste0("(",round(ne.low$se[12],2),")"),
                    matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[3,1],
                    matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[2,1]+
                       matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[3,1]+
                       matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[4,1]+
                       matrix(table(subset(rwp_ne_l.m$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_l.m$t)))

tabc12[1:16,6] <- c("07-11", "", "","", "", "ref", "", 
                    paste0(round(rwp_ne_h.m$att[2]*100,2), ifelse(rwp_ne_h.m$att[2]+rwp_ne_h.m$se[2]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[2]-rwp_ne_h.m$se[2]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[2]+rwp_ne_h.m$se[2]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[2]-rwp_ne_h.m$se[2]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h.m$se[2]*100,2), ")"),
                    paste0(round(rwp_ne_h.m$att[3]*100,2), ifelse(rwp_ne_h.m$att[3]+rwp_ne_h.m$se[3]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[3]-rwp_ne_h.m$se[3]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[3]+rwp_ne_h.m$se[3]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[3]-rwp_ne_h.m$se[3]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_h.m$se[3]*100,2),")"), 
                    paste0(round(rwp_ne_h.m$att[4]*100,2), ifelse(rwp_ne_h.m$att[4]+rwp_ne_h.m$se[4]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[4]-rwp_ne_h.m$se[4]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[4]+rwp_ne_h.m$se[4]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[4]-rwp_ne_h.m$se[4]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_h.m$se[4]*100,2),")"), 
                    matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[2,1],
                    matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[2,1]+
                       matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_h.m$t)))

tabc12[1:16,7] <- c("11-15", "", "",
                    paste0(round(rwp_ne_h.m$att[5]*100,2), ifelse(rwp_ne_h.m$att[5]+rwp_ne_h.m$se[5]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[5]-rwp_ne_h.m$se[5]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[5]+rwp_ne_h.m$se[5]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[5]-rwp_ne_h.m$se[5]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_h.m$se[5]*100,2),")"),
                    "ref", "", 
                    paste0(round(rwp_ne_h.m$att[7]*100,2), ifelse(rwp_ne_h.m$att[7]+rwp_ne_h.m$se[7]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[7]-rwp_ne_h.m$se[7]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[7]+rwp_ne_h.m$se[7]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[7]-rwp_ne_h.m$se[7]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h.m$se[7]*100,2), ")"),
                    paste0(round(rwp_ne_h.m$att[8]*100,2), ifelse(rwp_ne_h.m$att[8]+rwp_ne_h.m$se[8]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[8]-rwp_ne_h.m$se[8]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[8]+rwp_ne_h.m$se[8]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[8]-rwp_ne_h.m$se[8]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h.m$se[8]*100,2), ")"), "", "", 
                    matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[3,1],
                    matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[3,1]+
                       matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_h.m$t)))

tabc12[1:16,8] <- c("15-19", 
                    paste0(round(rwp_ne_h.m$att[9]*100,2), ifelse(rwp_ne_h.m$att[9]+rwp_ne_h.m$se[9]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[9]-rwp_ne_h.m$se[9]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[9]+rwp_ne_h.m$se[9]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[9]-rwp_ne_h.m$se[9]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(",round(rwp_ne_h.m$se[9]*100,2),")"),
                    paste0(round(rwp_ne_h.m$att[10]*100,2), ifelse(rwp_ne_h.m$att[10]+rwp_ne_h.m$se[10]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[10]-rwp_ne_h.m$se[10]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[10]+rwp_ne_h.m$se[10]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[10]-rwp_ne_h.m$se[10]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h.m$se[10]*100,2), ")"),
                    "ref", "", 
                    paste0(round(rwp_ne_h.m$att[12]*100,2), ifelse(rwp_ne_h.m$att[12]+rwp_ne_h.m$se[12]*rwp_ne_h.m$c>0 & rwp_ne_h.m$att[12]-rwp_ne_h.m$se[12]*rwp_ne_h.m$c>0 | rwp_ne_h.m$att[12]+rwp_ne_h.m$se[12]*rwp_ne_h.m$c<0 & rwp_ne_h.m$att[12]-rwp_ne_h.m$se[12]*rwp_ne_h.m$c<0 , "$^{*}$", "")),
                    paste0("(", round(rwp_ne_h.m$se[12]*100,2), ")"),
                    "", "", "", "",
                    matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[4,1],
                    matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[4,1]+
                       matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_h.m$t)))
tabc12[1:16,9] <- c("Event study",
                    paste0(round(ne.hig$att[7],2), ne.hig$sig[7]),
                    paste0("(",round(ne.hig$se[7],2),")"),
                    paste0(round(ne.hig$att[8],2), ne.hig$sig[8]),
                    paste0("(",round(ne.hig$se[8],2),")"),
                    "ref", "", 
                    paste0(round(ne.hig$att[10],2), ne.hig$sig[10]),
                    paste0("(",round(ne.hig$se[10],2),")"),
                    paste0(round(ne.hig$att[11],2), ne.hig$sig[11]),
                    paste0("(",round(ne.hig$se[11],2),")"),
                    paste0(round(ne.hig$att[12],2), ne.hig$sig[12]),
                    paste0("(",round(ne.hig$se[12],2),")"),
                    matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[2,1]+
                      matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[3,1],
                    matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[1,1],
                    (matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[2,1]+
                       matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[3,1]+
                       matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[4,1]+
                       matrix(table(subset(rwp_ne_h.m$DIDparams$data)$hosd1_g)/4)[1,1])*
                      length(unique(rwp_ne_h.m$t)))

xtable(tabc12)

#Appendix F####
##Figure F1####
rwpdata_le_schp_m <- clear_and_match_rwp_le(rwpdata_le_schp, "schp")[[1]]
rwpdata_ne_hosd1_m <- clear_and_match_rwp_ne(rwpdata_ne_hosd1, "hosd1")[[1]]

rwp_le_schp_1u <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_m)
)
rwp_le_schp_1m <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_m,  match_s==1)
)
rwp_ne_hosd1_1u <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_m)
)
rwp_ne_hosd1_1m <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_m,  match_s==1)
)


rwp.le <- extract.rwp.le(rwp_le_schp_1u, rwp_le_schp_1m)
rwp.ne <- extract.rwp.ne(rwp_ne_hosd1_1u, rwp_ne_hosd1_1m)

rwpplot <- bind_rows(rwp.le, rwp.ne) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         att=att/100,
         conf.low=conf.low/100,
         conf.high=conf.high/100
  )

votersplot1<- ggplot(data = rwpplot, aes(x=time, y=att , group = Model, colour = Model))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(aes(shape=Model), position=position_dodge(width=0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(cols = vars(elec), scales="free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in number of eligible voters")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  scale_colour_manual(values = c('grey40','black')) +
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))

ggsave("Figure_F1.eps" , plot = votersplot1, 
       scale = 1, width = 160 , height = 90, units = c("mm"),
       dpi = 1000)

aggte(rwp_le_schp_1m, "dynamic")


##Figure F2####

rwpdata_le_schd1_l <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 2.5)[[1]]
rwpdata_le_schd1_h <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 2.5)[[2]]

rwpdata_ne_hosd1_l <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 10)[[1]]
rwpdata_ne_hosd1_h <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 10)[[2]]

names(rwpdata)
rwp_le_l <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "schd1_g", data = subset(rwpdata_le_schd1_l)
)
rwp_le_h <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "schd1_g", data = subset(rwpdata_le_schd1_h)
)

rwp_le_l.m <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_l, match_s==1)
)
rwp_le_h.m <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_h, match_s==1)
)


rwp_ne_l <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l)
)
rwp_ne_l.m <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l, match_s==1)
)

rwp_ne_h <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h)
)
rwp_ne_h.m <- att_gt(yname = "eligible.votes", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h, match_s==1)
)


le.low <- bind_cols(extract.rwp.le(rwp_le_l, rwp_le_l.m), deltadist="Low")
le.hig <- bind_cols(extract.rwp.le(rwp_le_h, rwp_le_h.m), deltadist="High")

ne.low <- bind_cols(extract.rwp.ne(rwp_ne_l, rwp_ne_l.m), deltadist="Low")
ne.hig <- bind_cols(extract.rwp.ne(rwp_ne_h, rwp_ne_h.m), deltadist="High")


deltadistplot <- bind_rows(le.low, le.hig, ne.low, ne.hig) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         deltadist=factor(deltadist, levels = c("Low", "High"), labels = c("Low Severity\nChange of <2.5 km\nChange of <10 km", "High Severity\n>2.5 km to nearest school\n>10 km to nearest hospital")),
         Model  = factor(Model ,levels = c("Unmatched", "Matched")),
         att=att/100,
         conf.low=conf.low/100,
         conf.high=conf.high/100
         
  )

ggplot(data = deltadistplot, aes(x=time, y=att , group = deltadist, color = Model))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(aes(shape=deltadist), position=position_dodge(width=0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(cols = vars(elec), rows = vars(Model), scales="free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in number of eligible voters")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  scale_colour_manual(values = c("black", "grey40"), guide = "none") +
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))

ggsave("Figure_F2.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)


##Figure F3####
rwpdata_le_schp_m <- clear_and_match_rwp_le(rwpdata_le_schp, "schp")[[1]]
rwpdata_ne_hosd1_m <- clear_and_match_rwp_ne(rwpdata_ne_hosd1, "hosd1")[[1]]

rwp_le_schp_1u <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_m)
)
rwp_le_schp_1m <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                         gname = "schp_g", data = subset(rwpdata_le_schp_m,  match_s==1)
)
rwp_ne_hosd1_1u <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_m)
)
rwp_ne_hosd1_1m <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                          gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_m,  match_s==1)
)


rwp.le <- extract.rwp.le(rwp_le_schp_1u, rwp_le_schp_1m)
rwp.ne <- extract.rwp.ne(rwp_ne_hosd1_1u, rwp_ne_hosd1_1m)

rwpplot <- bind_rows(rwp.le, rwp.ne) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2"))
  )

turnoutplot1<- ggplot(data = rwpplot, aes(x=time, y=att , group = Model, colour = Model))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(aes(shape=Model), position=position_dodge(width=0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(cols = vars(elec), scales="free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in turnout \n (percentage points)")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  scale_colour_manual(values = c('grey40','black')) +
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size=10))

ggsave("Figure_F3.eps" , plot = turnoutplot1, 
       scale = 1, width = 160 , height = 90, units = c("mm"),
       dpi = 1000)

aggte(rwp_le_schp_1m, "dynamic")


##Figure F4####

rwpdata_le_schd1_l <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 2.5)[[1]]
rwpdata_le_schd1_h <- clear_and_match_rwp_le_dist(rwpdata_le_schd1, "schd1", 2.5)[[2]]

rwpdata_ne_hosd1_l <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 10)[[1]]
rwpdata_ne_hosd1_h <- clear_and_match_rwp_ne_dist(rwpdata_ne_hosd1, "hosd1", 10)[[2]]

names(rwpdata)
rwp_le_l <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "schd1_g", data = subset(rwpdata_le_schd1_l)
)
rwp_le_h <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "schd1_g", data = subset(rwpdata_le_schd1_h)
)

rwp_le_l.m <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_l, match_s==1)
)
rwp_le_h.m <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "schd1_g", data = subset(rwpdata_le_schd1_h, match_s==1)
)


rwp_ne_l <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l)
)
rwp_ne_l.m <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_l, match_s==1)
)

rwp_ne_h <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                   gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h)
)
rwp_ne_h.m <- att_gt(yname = "turnout", tname = "year", idname = "afstemid", xformla = ~ 1, est_method = "dr", allow_unbalanced_panel = F, control_group = "nevertreated", anticipation = 0, alp = 0.05, base_period =  "universal", print_details = T, clustervars = "afstemid",  cband = T,  bstrap = T,
                     gname = "hosd1_g", data = subset(rwpdata_ne_hosd1_h, match_s==1)
)


le.low <- bind_cols(extract.rwp.le(rwp_le_l, rwp_le_l.m), deltadist="Low")
le.hig <- bind_cols(extract.rwp.le(rwp_le_h, rwp_le_h.m), deltadist="High")

ne.low <- bind_cols(extract.rwp.ne(rwp_ne_l, rwp_ne_l.m), deltadist="Low")
ne.hig <- bind_cols(extract.rwp.ne(rwp_ne_h, rwp_ne_h.m), deltadist="High")


deltadistplot <- bind_rows(le.low, le.hig, ne.low, ne.hig) %>% 
  mutate(time=factor(time, levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2")),
         deltadist=factor(deltadist, levels = c("Low", "High"), labels = c("Low Severity\nChange of <2.5 km\nChange of <10 km", "High Severity\n>2.5 km to nearest school\n>10 km to nearest hospital")),
         Model  = factor(Model ,levels = c("Unmatched", "Matched"))
         )

ggplot(data = deltadistplot, aes(x=time, y=att , group = deltadist, color = Model))+ 
  geom_rect(aes(xmin=factor("p-1") ,xmax=factor("p"),ymin=-Inf,ymax=Inf),fill="grey90", linetype = 0)+
  geom_hline(yintercept=0, colour = "grey60")+
  geom_point(aes(shape=deltadist), position=position_dodge(width=0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(width=0.5))+
  facet_grid(cols = vars(elec), rows = vars(Model), scales="free_x")+
  labs(x = "Period in relation to closure", y = "Predicted change in turnout \n (percentage points)")+
  geom_text(aes(x=factor("p-1"), y= 0, label = "ref" ), size = 3, colour= "black")+
  scale_colour_manual(values = c("black", "grey40"), guide = "none") +
  theme_bw(base_size = 11)+
  theme(strip.text = element_text(size=10),
        axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=10),
        strip.text.y = element_text(size = 11))

ggsave("Figure_F4.eps" , plot = last_plot(), 
       scale = 1, width = 160 , height = 140, units = c("mm"),
       dpi = 1000)
