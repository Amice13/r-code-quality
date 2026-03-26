###########################################
#####                                 #####
##### Presidential Investments        #####
##### Nicholas Bednar and David Lewis #####
##### January 9, 2022                 #####
#####                                 #####
###########################################

######################
##### 1.0 Set Up #####
######################
rm(list=ls())

##### 1.1 Setting Working Directory to the Location of the Survey Data
setwd("/home/sfgs_team")

##### 1.2 Loading Packages For Analysis
#install.packages("tidyverse")
library(tidyverse)
library(MASS)
library(sandwich)
library(ggeffects)


##### 1.3 Loading Survey Data
load("data/sfgs_2020_non_pi.RData")

##### 1.4 Loading Grant Data
setwd("/home/nrb67")
grants <- read.csv("distributive_agencies_0619_0620_double_check_122921.csv")

########################
##### 1.0 Cleaning #####
########################

sfgs_npi <- sfgs_npi %>%
            mutate(resp = ifelse(job_it<98 &
                                 job_hr<98 &
                                 job_grants<98 & 
                                 job_enforcement<98 &
                                 job_budget<98 &
                                 job_priorities<98 &
                                 job_rulemaking<98,
                     job_it+job_hr+job_grants+job_enforcement+job_budget+job_priorities+job_rulemaking,NA)) %>%
            mutate(appt = ifelse(app=="appointee", 1,0),
                   com = ifelse(bureau_acr == "AMTRAK" |
                              bureau_acr == "CFTC" |
                              bureau_acr == "CNCS" |
                              bureau_acr == "CPSC" |
                              bureau_acr == "DNFSB" |
                              bureau_acr == "EEOC" |
                              bureau_acr == "EIB" |
                              bureau_acr == "FCA" |
                              bureau_acr == "FCC" |
                              bureau_acr == "FDIC" |
                              bureau_acr == "FLRA" |
                              bureau_acr == "FMC" |
                              bureau_acr == "FTC" |
                              bureau_acr == "Fed" |
                              bureau_acr == "IAF" |
                              bureau_acr == "LSC" |
                              bureau_acr == "MCC" |
                              bureau_acr == "MSPB" |
                              bureau_acr == "NCUA" |
                              bureau_acr == "NLRB" |
                              bureau_acr == "NMB" |
                              bureau_acr == "NRC" |
                              bureau_acr == "NTSB" |
                              bureau_acr == "RRB" |
                              bureau_acr == "SEC" |
                              bureau_acr == "TVA" |
                              bureau_acr == "USITC" |
                              bureau_acr == "USPS" |
                              bureau_acr == "USAGM" |
                              bureau_acr == "CEA" |
                              bureau_acr == "CEQ",
                              1,0)) %>%
            mutate(indcom = ifelse(ind == 1 & com==1, 1, 0),
                   dprio = ifelse(dept_acr == "DOJ" |
                                  dept_acr == "OPM" |
                                  dept_acr == "EOP" |
                                  dept_acr == "OGE" |
                                  dept_acr == "STAT" |
                                  dept_acr == "TREAS" |
                                  dept_acr == "COM" |
                                  dept_acr == "USAID" |
                                  dept_acr == "USITC" |
                                  dept_acr == "USDA" |
                                  dept_acr == "DOI" |
                                  dept_acr == "DOE" |
                                  dept_acr == "EPA" |
                                  dept_acr == "DHS" |
                                  dept_acr == "DOT" |
                                  dept_acr == "VA" |
                                  dept_acr == "ED" |
                                  dept_acr == "HHS" |
                                  dept_acr == "DOD",
                                  1, 0),
                   bprio = ifelse(bureau_acr=="OMB"|
                                  bureau_acr=="ITA"|
                                  bureau_acr=="FAS"|
                                  bureau_acr=="BIS"|
                                  bureau_acr=="FE"|
                                  bureau_acr=="BLM"|
                                  bureau_acr=="NOAA"|
                                  bureau_acr=="OLC"|
                                  bureau_acr=="OJP"|
                                  bureau_acr=="ICE"|
                                  bureau_acr=="CBP"|
                                  bureau_acr=="CIS"|
                                  bureau_acr=="IRS"|
                                  bureau_acr=="FHA"|
                                  bureau_acr=="FTA"|
                                  bureau_acr=="FRA" |
                                  bureau_acr=="FAA"|
                                  bureau_acr=="VHA"|
                                  bureau_acr=="RD"|
                                  bureau_acr=="OESE"|
                                  bureau_acr=="CMMS"|
                                  bureau_acr=="E"|
                                  bureau_acr=="FDA"|
                                  bureau_acr=="ARMY"|
                                  bureau_acr=="NAVY"|
                                  bureau_acr=="USAF"|
                                  bureau_acr=="SecDOD",
                                  1,0),
                   dlevel = ifelse(ind == 1,
                                   "IND",
                            ifelse(dept_acr == "USAID",
                                   "STAT",
                            dept_acr)),
                   offsec2 = ifelse(bureau_acr=="SecDOD"|
                                      bureau_acr=="SecAg"|
                                      bureau_acr=="SecCom"|
                                      bureau_acr=="SecDHS"|
                                      bureau_acr=="SecDOI"|
                                      bureau_acr=="SecDOL"|
                                      bureau_acr=="SecDOT"|
                                      bureau_acr=="SecED"|
                                      bureau_acr=="SecHHS"|
                                      bureau_acr=="SecSTAT"|
                                      bureau_acr=="SecTREAS"|
                                      bureau_acr=="SecVA"|
                                      bureau_acr=="SecAG"|
                                      bureau_acr=="SecCOM",
                                    1,0),
                   other = ifelse(bureau_acr=="OthCom"|
                                  bureau_acr=="OthDOC"|
                                  bureau_acr=="OthDHS"|
                                    bureau_acr=="OthDOD"|
                                    bureau_acr=="OthDOE"|
                                    bureau_acr=="OthDOI"|
                                    bureau_acr=="OthDOJ"|
                                    bureau_acr=="OthDOL"|
                                    bureau_acr=="OthDOT"|
                                    bureau_acr=="OthED"|
                                    bureau_acr=="OthHHS"|
                                    bureau_acr=="OthHUD"|
                                    bureau_acr=="OthSTAT"|
                                    bureau_acr=="OthTREAS"|
                                    bureau_acr=="OthUSDA"|
                                    bureau_acr=="OthVA"|
                                    bureau_acr=="OthCOM"|
                                    bureau_acr=="OthEOP",
                                  1,0)) %>%
        mutate(subcomponent = ifelse(other == 1,
                                     0,
                              ifelse(offsec2 == 1,
                                     0,
                              ifelse(bureau != dept, 
                                     1, 0)))) %>%
        mutate(priority = ifelse(bprio == 1,
                                 1,
                          ifelse(dprio == 1 & offsec2 == 1, 
                                 1,
                          ifelse(dprio == 1 & ind == 1,
                                 1,
                                 0)))) %>%
        mutate(sup_misn_none = ifelse(sup_misn_wh == "None",1,0),
               sup_misn_little = ifelse(sup_misn_wh == "Little",1,0),
               sup_misn_some = ifelse(sup_misn_wh == "Some", 1,0),
               sup_misn_good = ifelse(sup_misn_wh =="A good bit", 1,0),
               sup_misn_great = ifelse(sup_misn_wh == "A great deal", 1, 0)) %>%
        mutate(sup_misn_wh = ifelse(sup_misn_wh == "None",
                                    0,
                             ifelse(sup_misn_wh == "Little",
                                    1,
                             ifelse(sup_misn_wh == "Some",
                                    2,
                             ifelse(sup_misn_wh == "A good bit",
                                    3,
                             ifelse(sup_misn_wh == "A great deal",
                                    4,
                                    NA)))))) %>%
        mutate(pid_3 = ifelse(pid_3 == "Democrat",
                              0,
                       ifelse(pid_3 == "Independent",
                              1,
                       ifelse(pid_3 == "Republican",
                              2,
                              NA)))) %>%
        left_join(grants, by = "bureau_acr") 

sfgs_npi <- sfgs_npi %>%
        mutate(prgr = ifelse(is.na(proj_grants) == TRUE | proj_grants == "",
                             0, proj_grants)) %>%
        mutate(prgr = as.numeric(prgr)) %>%
        mutate(lnprgr = log(1+prgr)) %>%
        mutate(gggiver = ifelse(lnprgr > 0, 1, 0)) %>%
        mutate(indcom = ifelse(bureau_acr == "FERC", 1,indcom)) %>%
        mutate(subcomponent = ifelse(bureau_acr == "FERC" | 
                                    bureau_acr == "NEH" | 
                                    bureau_acr == "NEA" | 
                                    bureau_acr == "IMLS",
                                    1,subcomponent)) 

descs <- sfgs_npi %>%
  filter(as.numeric(as.character(sup_misn_wh))<6) %>%
  mutate(sup_misn_none_wt = sup_misn_none*wt_full,
         sup_misn_little_wt = sup_misn_little*wt_full,
         sup_misn_some_wt = sup_misn_some*wt_full,
         sup_misn_good_wt = sup_misn_good*wt_full,
         sup_misn_great_wt = sup_misn_great*wt_full) %>%
  mutate(sup_misn_none_mean = sum(sup_misn_none_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_little_mean = sum(sup_misn_little_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_some_mean = sum(sup_misn_some_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_good_mean = sum(sup_misn_good_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_great_mean = sum(sup_misn_great_wt,na.rm = T)/sum(wt_full,na.rm=T)) %>%
  mutate(sdi_none_wt = wt_full*(sup_misn_none-sup_misn_none_mean)^(2), 
         sdi_little_wt = wt_full*(sup_misn_little-sup_misn_little_mean)^(2), 
         sdi_some_wt = wt_full*(sup_misn_some-sup_misn_some_mean)^(2), 
         sdi_good_wt = wt_full*(sup_misn_good-sup_misn_good_mean)^(2), 
         sdi_great_wt = wt_full*(sup_misn_great-sup_misn_great_mean)^(2)) %>%
  summarize(sup_misn_none_mean = sum(sup_misn_none_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_none_sd = sqrt(sum(sdi_none_wt,na.rm = T)/sum(wt_full,na.rm=T)),
            sup_misn_little_mean = sum(sup_misn_little_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_little_sd = sqrt(sum(sdi_little_wt,na.rm = T)/sum(wt_full,na.rm=T)),
            sup_misn_some_mean = sum(sup_misn_some_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_some_sd = sqrt(sum(sdi_some_wt,na.rm = T)/sum(wt_full,na.rm=T)),
            sup_misn_good_mean = sum(sup_misn_good_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_good_sd = sqrt(sum(sdi_good_wt,na.rm = T)/sum(wt_full,na.rm=T)),
            sup_misn_great_mean = sum(sup_misn_great_wt,na.rm = T)/sum(wt_full,na.rm=T),
            sup_misn_great_sd = sqrt(sum(sdi_great_wt,na.rm = T)/sum(wt_full,na.rm=T)),
            num_obs = n())

descs <- data.frame(Response = factor(c("None", "Little", "Some", "A good bit", "A great deal")),
                    Proportion = c(descs$sup_misn_none_mean,
                                   descs$sup_misn_little_mean,
                                   descs$sup_misn_some_mean,
                                   descs$sup_misn_good_mean,
                                   descs$sup_misn_great_mean),
                    SD = c(descs$sup_misn_none_sd,
                           descs$sup_misn_little_sd,
                           descs$sup_misn_some_sd,
                           descs$sup_misn_good_sd,
                           descs$sup_misn_great_sd),
                    N = descs$num_obs) %>%
          mutate(LowCI = Proportion-1.96*(SD/sqrt(N)),
                 HighCI = Proportion+1.96*(SD/sqrt(N)))

descs$Response <- reorder(descs$Response, c(1:5))

ggplot(descs, aes(x=Response, y=Proportion))+
  geom_bar(stat = "identity", fill= "gray50", width = 0.5)+
  scale_y_continuous(name = "Proportion of Respondents", breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0,0.5))+
  scale_x_discrete(name = "Reported Level of Presidential Investment")+
  geom_errorbar(aes(ymin=LowCI, ymax=HighCI), width = 0.15)+
  theme_light()

ggsave("resp_investment.pdf", device = "pdf", width = 5, height = 3, units = "in")


sfgs_npi <- sfgs_npi %>%
            filter(as.numeric(as.character(sup_misn_wh))<6) %>%
            filter(is.na(pid_3) == FALSE)


#### Running Ordered Logit
model2 <- polr(as.factor(sup_misn_wh) ~ priority+ideo_rating+(priority*ideo_rating)+skills_mean_14+eop+offsec2+subcomponent+indcom+gggiver+appt+pid_3+resp+usda+doc+dod+doe+ed+hhs+dhs+hud+doj+dol+dos+doi+treas+dot+va,
     data = sfgs_npi,
     weight = wt_full,
     method = "logistic")
covargs = c("sfgs_npi$workplace")
names(covargs) <- c("cluster")

predicted <- data.frame(ggpredict(model2, terms = c("ideo_rating[all]", "priority[all]"), vcov.fun = "vcovCL", vcov.type = "HC1", vcov.args = list(cluster = sfgs_npi$workplace))) 
predicted$response.level[predicted$response.level == 0] <- "(0) None"
predicted$response.level[predicted$response.level == 1] <- "(1) Little"
predicted$response.level[predicted$response.level == 2] <- "(2) Some"
predicted$response.level[predicted$response.level == 3] <- "(3) A Good Bit"
predicted$response.level[predicted$response.level == 4] <- "(4) A Great Deal"
predicted$group <- as.character(predicted$group)
predicted$group[predicted$group == 0] <- "Non-Priority"
predicted$group[predicted$group == 1] <- "Priority"





ggplot(predicted[predicted$response.level == "(4) A Great Deal",], aes(x=x, y=predicted))+
  geom_smooth(aes(lty=group), color = "gray20")+
  scale_y_continuous(name = "Predicted Pr(A Great Deal)")+
  scale_x_continuous(name = "Ideological Distance", breaks = c(-1.75,1.75), labels = c("Liberal", "Conservative"))+
  labs(color = "", lty="")+
  geom_rug(data=sfgs_npi, aes(x=ideo_rating), color = "gray50", inherit.aes = F)+
  theme_light()+
  theme(legend.position = "bottom")

ggsave("m2_predicted_ideo.pdf", device = "pdf", width = 7, height = 3, units = "in")


