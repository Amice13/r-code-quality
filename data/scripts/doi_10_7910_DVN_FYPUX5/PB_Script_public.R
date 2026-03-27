#Replication File for
#Attending Church Encourages Acceptance of Atheists? Suppression Effects in Religion and Politics Research####
#Forthcoming at Political Behavior
#AUthors:
#Paul A. Djupe, Denison University 
#Amanda J. Friesen, Western University 
#Andrew R. Lewis, University of Cincinnati 
#Anand E. Sokhey, University of Colorado, Boulder 
#Jacob R. Neiheisel, University at Buffalo, SUNY 
#Zachary D. Broeren, Vanderbilt University
#Ryan P. Burge, Eastern Illinois University


#Package Installs
install.packages("tidyverse")
install.packages("rio")
install.packages("remotes")
remotes::install_github("ryanburge/socsci")
install.packages("patchwork")
install.packages("showtext")
install.packages("jtools")
install.packages("interactions")
install.packages("ggridges")
install.packages("ggthemes")

library(tidyverse)
library(rio)
library(socsci)
library(patchwork)
library(showtext)
font_add_google("EB Garamond", "G", regular.wt = 400)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
library(jtools)
library(interactions)
library(ggridges)
library(ggthemes)


#Load the Data####

    #Set file location first.
files <- "YOUR_FILES_LOCATION_HERE"

b7 <- import(paste0(files, "baylor_2007.csv"))
b17 <- import(paste0(files, "baylor_2017.csv"))
ces <- import(paste0(files, "ces.csv"))
m21 <- import(paste0(files, "m21.csv"))
s21 <- import(paste0(files, "s21.csv"))

#Recodes are found in the first relevant analysis/display

#Figure A1 – The Distribution of Christian Nationalism in Religious Traditions####

b7 <- b7 %>% mutate(female=gender-1, 
                    white=car::recode(white, "NA=0; 2=0"))

    #higher is more conservative, all condensed to 0-1
b7 <- b7 %>% mutate(marijuan_r=(4-marijuan)/3,
                    physsuic_r=(4-physsuic)/3,
                    stemcell_r=(4-stemcell)/3,
                    war_r=(war-1)/3,
                    deathpen_r=car::recode(deathpen, "3=4; 4=5; 8=3"),
                    deathpen_r=(5-deathpen_r)/4,
                    english_r=car::recode(english, "3=4; 4=5; 8=3"),
                    english_r=(english_r-1)/4,
                    distrib_r=car::recode(distrib, "3=4; 4=5; 8=3"),
                    distrib_r=(5-distrib_r)/4,
                    regulate_r=car::recode(regulate, "3=4; 4=5; 8=3"),
                    regulate_r=(5-regulate_r)/4,
                    punish_r=car::recode(punish, "3=4; 4=5; 8=3"),
                    punish_r=(punish_r-1)/4,
                    gunlaws_r=car::recode(gunlaws, "3=4; 4=5; 8=3"),
                    gunlaws_r=(5-gunlaws_r)/4,
                    improve_r=car::recode(improve, "3=4; 4=5; 8=3"),
                    improve_r=(5-improve_r)/4,
                    gaymarr_r=car::recode(gaymarr, "3=4; 4=5; 8=3"),
                    gaymarr_r=(5-gaymarr_r)/4)


    #Christian nationalism
b7 <- b7 %>% mutate(chrnatn_r=car::recode(chrnatn, "4=5; 3=4; 8=3"))
b7 <- b7 %>% mutate(advochr_r=car::recode(advochr, "4=5; 3=4; 8=3"))
b7 <- b7 %>% mutate(separat_r=car::recode(separat, "4=5; 3=4; 8=3"))
b7 <- b7 %>% mutate(disprel_r=car::recode(disprel, "4=5; 3=4; 8=3"))
b7 <- b7 %>% mutate(prayschl_r=car::recode(prayschl, "4=5; 3=4; 8=3"))
b7 <- b7 %>% mutate(godsplan_r=car::recode(godsplan, "4=5; 3=4; 8=3"))

b7 <- b7 %>% mutate(cn=(chrnatn_r + advochr_r + (6-separat_r) + disprel_r + prayschl_r + godsplan_r)/6)
b7 <- b7 %>% mutate(cn1=(cn-1)/4) 

b7 <- b7 %>% mutate(polviewsr=8-polviews, 
                    pvcon=car::recode(polviewsr, "1:4=0; 5:7=1"))
b7 <- b7 %>% mutate(attend7=car::recode(attend, "8=7"))

b7 <- b7 %>% mutate(attend7f=frcode(attend7==0 ~ "Never",
                                    attend7==1 ~ "<Once\na year",
                                    attend7==2 ~ "1-2x\na year",
                                    attend7==3 ~ "Several x\na year",
                                    attend7==4 ~ "1x a\nmonth",
                                    attend7==5 ~ "2-3x a\nmonth",
                                    attend7==6 ~ "About\nweekly",
                                    attend7==7 ~ "Weekly+"))

b7 <- b7 %>% mutate(reltradf=frcode(reltrad==2 ~ "Black Prot",
                                    reltrad==1 ~ "Evangelical",
                                    reltrad==4 ~ "Catholic",
                                    reltrad==3 ~ "Mainline",
                                    reltrad==6 ~ "Other",
                                    reltrad==7 ~ "None",
                                    reltrad==5 ~ "Jewish"))

b7 %>% filter(reltrad!="NA") %>% 
  ggplot(aes(x=cn1, y=reltradf)) + 
  theme_minimal() +
  geom_density_ridges(rel_min_height=.02, quantiles=2, quantile_lines=T) +
  scale_x_continuous(breaks=seq(0,1,.1)) +
  labs(y="", x="Christian Nationalism", caption="Sources: Baylor 2007 Survey") +
  theme(text=element_text(family="G", size=12))
ggsave(file=paste0(files, "Figure_A1.png"), height=5, width=5)


#Figure 1 - Attenders Tell Us They are Conservative (CES Data Series)####

ces <- ces %>% mutate(attend5=car::recode(relig_church, "1:2=5; 3=4; 4=3; 5=2; 6:7=1; 8=NA"))
ces <- ces %>% mutate(attend6=car::recode(relig_church, "1=6; 2=5; 3=4; 4=3; 5=2; 6:7=1; 8=NA"))

ces %>% ct(ideo5)
ces <- ces %>% mutate(conservative=case_when(ideo5=="Conservative" | ideo5=="Very Conservative" ~ 1,
                                             ideo5=="Moderate" | ideo5=="Liberal" | ideo5=="Very Liberal"  ~ 0,
                                             TRUE ~ NA))

ces %>% filter(year>2007 & year<2022) %>% 
  group_by(year, attend6) %>% mean_ci(conservative, wt=weight) %>% 
  ggplot(aes(x=attend6, y=mean)) + 
  geom_col(color="black", fill="#C1CDCD") +
  geom_text(aes(y=.05, label=paste0(round(mean*100,0), "%")), size=5, family="G") +
  facet_wrap(~year) +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0, .6, .2), labels=percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=seq(1,6,1), labels=c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  theme(text=element_text(family="G", size=12),
        axis.text.x = element_text(size=8),
        strip.text = element_text(size=14)) +
  labs(x="Worship Attendance", y="Percent Conservative")

ggsave(file=paste0(files, "Figure_1.png"), type="cairo-png", height=9, width=12)



#Figure A2 – Attenders Tell us They’re Conservative (Baylor 2007 and 2017 data) ####

pb7 <- b7 %>% filter(attend7f!="NA") %>% group_by(attend7f) %>% mean_ci(pvcon, wt=weight) %>% 
  ggplot(., aes(x=attend7f, y=mean)) + 
  geom_col(fill="#C1CDCD", color="black") +
  geom_text(aes(label=paste0(mean*100, "%"), y=.05), family="G") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12)) +
  labs(x="", y="", subtitle="2007") +
  scale_y_continuous(labels=percent_format(accuracy=1))


b17 <- b17 %>% mutate(attend7f=frcode(attend==0 ~ "Never",
                                      attend==1 ~ "<Once\na year",
                                      attend==2 ~ "1-2x\na year",
                                      attend==3 ~ "Several x\na year",
                                      attend==4 ~ "1x a\nmonth",
                                      attend==5 ~ "2-3x a\nmonth",
                                      attend==6 ~ "About\nweekly",
                                      attend==7 ~ "Weekly+"))

b17 <- b17 %>% mutate(Q31r=8-Q31, pvcon=car::recode(Q31r, "1:4=0; 5:7=1"))

b17 <- b17 %>% mutate(MP12A=car::recode(MP12A, "3=4; 4=5; 8=3"), 
                      MP12B=car::recode(MP12B, "3=4; 4=5; 8=3"),
                      MP12C=car::recode(MP12C, "3=4; 4=5; 8=3"),
                      MP12D=car::recode(MP12D, "3=4; 4=5; 8=3"),
                      MP12E=car::recode(MP12E, "3=4; 4=5; 8=3"),
                      MP12F=car::recode(MP12F, "3=4; 4=5; 8=3"),
                      cn=(MP12A+MP12B+(6-MP12C)+MP12D+MP12E+MP12F)/6,
                      cn1=(cn-1)/4)
b17 <- b17 %>% mutate(female=car::recode(Q77, "1=0;3=0;2=1"))
b17 <- b17 %>% mutate(trumpvote=case_when(MP7==2 | MP6==2 ~ "Trump",
                                          MP7==1 | MP6==1 ~ "Clinton",
                                          TRUE ~ "NA")) 
b17 <- b17 %>% mutate(trv=frcode(trumpvote=="Trump" ~ 1,
                                 trumpvote=="Clinton" ~ 0))
b17 <- b17 %>% mutate(trv=as.numeric(trv)) %>% 
  mutate(trv=car::recode(trv, "1=1; 2=0"))

b17 <- b17 %>% mutate(attend3=car::recode(attend,"0=0; 1:5=1;6:7=2"),
                      attend3=frcode(attend3==0 ~ "Non-attenders",
                                     attend3==1 ~ "Infrequent attenders",
                                     attend3==2 ~ "Frequent attenders"))
b17 <- b17 %>% mutate(white=car::recode(race, "1=1; 2:7=0"))
b17 <- b17 %>% mutate(reltrad=as.factor(reltrad))


pb17 <- b17 %>% filter(attend7f!="NA") %>% group_by(attend7f) %>% mean_ci(pvcon, wt=weight) %>% 
  ggplot(., aes(x=attend7f, y=mean)) + 
  geom_col(fill="#C1CDCD", color="black") +
  geom_text(aes(label=paste0(mean*100, "%"), y=.05), family="G") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12)) +
  labs(x="", y="", subtitle="2017") +
  scale_y_continuous(labels=percent_format(accuracy=1))

pb7 + pb17
ggsave(file=paste0(files, "Figure_A2.png"), type="cairo-png", height=4, width=8)

rm(pb7, pb17)

#Figure 2 - Attenders Take More Conservative Stands on All Issues, Even if Only Slightly####

b7i <- b7 %>% gather(key="issue", value="issue_ans", marijuan_r, physsuic_r, 
                     stemcell_r, war_r, deathpen_r, english_r, distrib_r, 
                     punish_r, regulate_r,gunlaws_r, improve_r, gaymarr_r, na.rm=TRUE)
lmc <- lm(issue_ans ~ issue*attend7 + female+age+white+educ, weight=weight, data=b7i)
summ(lmc)
interact_plot(lmc, pred=attend7, modx=issue, facet.modx = T, interval=T, int.width = .84,
              modx.labels = c("Death Penalty", "Distribute Wealth", "English Only",
                              "Same-Sex Marriage", "Stricter Gun Laws", "Improve SOL for Minorities",
                              "Legalize Marijuana", "Physician-Asst Suicide", "Punish Criminals",
                              "Regulate Business", "Stem Cell Research", "Morality of War")) +
  theme_minimal() +
  theme_hc() +
  theme(text=element_text(family="G", size=12),
        strip.text = element_text(family="G", size=16),
        strip.background = element_rect(fill="gray90"),
        panel.spacing = unit(1.5, "lines")) +
  scale_x_continuous(breaks=c(0, 7), labels=c("Never", "Weekly+")) +
  scale_y_continuous(breaks=seq(.2,.8,.2)) +
  labs(x="Worship Attendance Level", y="Conservative Responses ->")
ggsave(file=paste0(files, "Figure_2.png"), height=9, width=12)

rm(b7i)

    #Appendix Tables for the figure 2 issue models

lmb1 <- lm(deathpen_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb2 <- lm(distrib_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb3 <- lm(english_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb4 <- lm(gaymarr_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb5 <- lm(gunlaws_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb6 <- lm(improve_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb7 <- lm(marijuan_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb8 <- lm(physsuic_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb9 <- lm(punish_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb10 <- lm(regulate_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb11 <- lm(stemcell_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)
lmb12 <- lm(war_r ~ attend7 + female+age+white+educ, weight=weight, data=b7)

export_summs(lmb1, lmb2, lmb3, lmb4, lmb5, lmb6, error_format = "({p.value})", error_pos = "below",
             model.names=c("Death\nPenalty", "Distribute\nWealth",
                           "English\nOnly", "Same-Sex\nMarriage",
                           "Stricter\nGun Laws", "Improve SOL\nfor Minorities"),
             to.file="docx", file.name = paste0(files, "Table_A1A.docx"))

export_summs(lmb7, lmb8, lmb9, lmb10, lmb11, lmb12, error_format = "({p.value})", error_pos = "below",
             model.names=c("Legalize\nMarijuana", "Physician-Assisted\nSuicide",
                           "Punish\nCriminals", "Regulate\nBusiness",
                           "Stem Cell\nResearch", "Morality\nof War"),
             to.file="docx", file.name = paste0(files, "Table_A1B.docx"))


#Figure A3 --  The Marginal Effects of Attendance on Various Policy Attitudes Among Mainline Protestants (higher values are more conservative)
b7mp <- b7 %>% filter(reltrad==3)

lmb1 <- lm(deathpen_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb1)
lmb2 <- lm(distrib_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb2)
lmb3 <- lm(english_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb3)
lmb4 <- lm(gaymarr_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb4)
lmb5 <- lm(gunlaws_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb5)
lmb6 <- lm(improve_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb6)
lmb7 <- lm(marijuan_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb7)
lmb8 <- lm(physsuic_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb8)
lmb9 <- lm(punish_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb9)
lmb10 <- lm(regulate_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb10)
lmb11 <- lm(stemcell_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb11)
lmb12 <- lm(war_r ~ attend7 + female+age+white+educ, weight=weight, data=b7mp)
summ(lmb12)

plot_summs(lmb1, lmb2, lmb3, lmb4, lmb5, lmb6, lmb7, lmb8, lmb9, lmb10, lmb11, lmb12,
           coefs=c(" " = "attend7"), colors = c("black", "black", "black", "black", "black",
                                                "black", "black", "black", "black", "black",
                                                "black", "black"),
           model.names=c("Death Penalty", "Distribute Wealth", "English Only",
                         "Same-Sex Marriage", "Stricter Gun Laws", "Improve SOL for Minorities",
                         "Legalize Marijuana", "Physician-Asst Suicide", "Punish Criminals",
                         "Regulate Business", "Stem Cell Research", "Morality of War")) +
  theme_minimal() + 
  geom_vline(xintercept=0, color="red") +
  labs(x="Marginal Effect of Attendance", y="") +
  theme(text=element_text(family="G", size=12))
ggsave(file=paste0(files, "Figure_A3.png"), type="cairo_png", height=4, width=5)


#Figure 3 – The Link between Christian Nationalism and Worship Attendance, 2007-2021####
attb7 <- b7 %>% group_by(attend7) %>% mean_ci(cn1, wt=weight) %>% 
  ggplot(aes(x=attend7, y=mean)) + 
  geom_col() +
  theme_hc() +
  theme(text=element_text(family="G", size=12)) +
  scale_x_continuous(breaks=c(0,7), labels=c("Never", "Weekly+")) +
  labs(x="Worship Attendance", y="Christian Nationalism",
       subtitle="Baylor 2007") +
  expand_limits(y=c(0,.75))
attb17 <- b17 %>% group_by(attend) %>% mean_ci(cn1, wt=weight) %>% 
  ggplot(aes(x=attend, y=mean)) + 
  geom_col() +
  theme_hc() +
  theme(text=element_text(family="G", size=12)) +
  scale_x_continuous(breaks=c(0,7), labels=c("Never", "Weekly+")) +
  labs(x="Worship Attendance", y="Christian Nationalism",
       subtitle="Baylor 2017") +
  expand_limits(y=c(0,.75))

m21 <- m21 %>% mutate(attend5=car::recode(q45, "1:2=5; 3=4; 4=3; 5=2; 6=1"), 
                      ed=q95, 
                      female=q89-1, 
                      age=2021-q90_1, 
                      white=car::recode(q93_1, "NA=0"))

m21 <- m21 %>% mutate(q31_1r=car::recode(q31_1, "1=5; 2=4; 4=3; 6=2; 7=1"),
                      q31_2r=car::recode(q31_2, "1=5; 2=4; 4=3; 6=2; 7=1"),
                      q31_3r=car::recode(q31_3, "1=5; 2=4; 4=3; 6=2; 7=1"),
                      q31_4r=car::recode(q31_4, "1=5; 2=4; 4=3; 6=2; 7=1"),
                      q31_5r=car::recode(q31_5, "1=5; 2=4; 4=3; 6=2; 7=1"),
                      q31_6r=car::recode(q31_6, "1=5; 2=4; 4=3; 6=2; 7=1"))
m21 <- m21 %>% mutate(cn=(((q31_1r+q31_2r+(6-q31_3r)+q31_4r+q31_5r+q31_6r)-6)/24))


attm21 <- m21 %>% filter(wgt!="NA") %>% group_by(attend5) %>% mean_ci(cn, wt=wgt) %>% 
  ggplot(aes(x=attend5, y=mean)) + 
  geom_col() +
  theme_hc() +
  theme(text=element_text(family="G", size=12)) +
  scale_x_continuous(breaks=c(1,5), labels=c("Never", "Weekly+")) +
  labs(x="Worship Attendance", y="Christian Nationalism",
       subtitle="March 2021") +
  expand_limits(y=c(0,.75))

s21 <- s21 %>% mutate(female=q2-1,
                      female=car::recode(female, "2=NA"),
                      age=2021-q3_1,
                      attend5=car::recode(q6, "1:2=5; 3=4; 4=3; 5=2; 6=1"), 
                      attend51=(attend5-1)/4,
                      ed=q94,
                      white=car::recode(q93, "2:8=0; 1=1"))
#CN
s21 <- s21 %>% mutate(q36_1=car::recode(q36_1, "4=3; 6=4; 7=5"),
                      q36_2=car::recode(q36_2, "4=3; 6=4; 7=5"),
                      q36_3=car::recode(q36_3, "4=3; 6=4; 7=5"),
                      q36_4=car::recode(q36_4, "4=3; 6=4; 7=5"),
                      q36_5=car::recode(q36_5, "4=3; 6=4; 7=5"),
                      q36_6=car::recode(q36_6, "4=3; 6=4; 7=5"))
s21 <- s21 %>% mutate(cn=((6-q36_1) + (6-q36_2) + (q36_3) + (6-q36_4) + (6-q36_5) + (6-q36_6)-6)/24)


atts21 <- s21 %>% filter(wgt!="NA") %>% group_by(attend5) %>% mean_ci(cn, wt=wgt) %>% 
  ggplot(aes(x=attend5, y=mean)) + 
  geom_col() +
  theme_hc() +
  theme(text=element_text(family="G", size=12)) +
  scale_x_continuous(breaks=c(1,5), labels=c("Never", "Weekly+")) +
  labs(x="Worship Attendance", y="Christian Nationalism",
       subtitle="September 2021") +
  expand_limits(y=c(0,.75))

attb7 + attb17 + attm21 + atts21 + plot_layout(axes="collect", ncol=4)
ggsave(file=paste0(files, "Figure_3.png"), height=3, width=9)

#Figure 4 – The Estimated Attendance Effect on Policy is a Function of Model Specification (higher positive values are more conservative stances)####
#Marijuana
lm0 <- lm(marijuan_r ~ attend7, weight=weight, data=b7)
summ(lm0)
lm1 <- lm(marijuan_r ~ attend7+female+age+white+educ, weight=weight, data=b7)
summ(lm1)
lm2 <- lm(marijuan_r ~ attend7+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm2)
lm3 <- lm(marijuan_r ~ attend7+female+age+white+educ+cn1, weight=weight, data=b7)
summ(lm3)
lm4 <- lm(marijuan_r ~ attend7+female+age+white+educ+bible+cn1, weight=weight, data=b7)
summ(lm4)

export_summs(lm0, lm1, lm2, lm3, lm4, error_format = "({p.value})", error_pos = "below",
             model.names=c("Bivariate",
                           "Attendance+demos",
                           "With Literalism",
                           "With Christian Nat.",
                           "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A2A.docx"))

p1 <- plot_summs(lm0, lm1, lm2, lm3, lm4, coefs = c(" "="attend7"),
                 model.names = c("Bivariate", "Attendance+demos",
                                 "With Literalism",
                                 "With Christian Nat.",
                                 "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Legalizing Marijuana", 
       y="Estimated Attendance Effect") +
  theme(text=element_text(family="G", size=12), legend.position="blank")



#Gun Laws
lm0 <- lm(gunlaws_r ~ attend7, weight=weight, data=b7)
summ(lm0)
lm1 <- lm(gunlaws_r ~ attend7+female+age+white+educ, weight=weight, data=b7)
summ(lm1)
lm2 <- lm(gunlaws_r ~ attend7+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm2)
lm3 <- lm(gunlaws_r ~ attend7+female+age+white+educ+cn1, weight=weight, data=b7)
summ(lm3)
lm4 <- lm(gunlaws_r ~ attend7+female+age+white+educ+bible+cn1, weight=weight, data=b7)
summ(lm4)

export_summs(lm0, lm1, lm2, lm3, lm4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate", "Attendance+demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A2B.docx"))

p3 <- plot_summs(lm0, lm1, lm2, lm3, lm4, coefs = c(" "="attend7"),
                 model.names = c("Bivariate", "Attendance+demos",
                                 "With Literalism",
                                 "With Christian Nat.",
                                 "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Gun Control", 
       y="") +
  theme(text=element_text(family="G", size=12), legend.position = "blank")

#Improve standard of living for minorities
lm0 <- lm(improve_r ~ attend7, weight=weight, data=b7)
summ(lm0)
lm1 <- lm(improve_r ~ attend7+female+age+white+educ, weight=weight, data=b7)
summ(lm1)
lm2 <- lm(improve_r ~ attend7+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm2)
lm3 <- lm(improve_r ~ attend7+female+age+white+educ+cn1, weight=weight, data=b7)
summ(lm3)
lm4 <- lm(improve_r ~ attend7+female+age+white+educ+bible+cn1, weight=weight, data=b7)
summ(lm4)
export_summs(lm0, lm1, lm2, lm3, lm4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate", "Attendance+demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A2C.docx"))

p4 <- plot_summs(lm0, lm1, lm2, lm3, lm4, coefs = c(" "="attend7"),
                 model.names = c("Bivariate", "Attendance+demos",
                                 "With Literalism",
                                 "With Christian Nat.",
                                 "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Improve Living for Minorities", 
       y="") +
  theme(text=element_text(family="G", size=12), legend.position="blank")


#Death Penalty
lm0 <- lm(deathpen_r ~ attend7, weight=weight, data=b7)
summ(lm0)
lm1 <- lm(deathpen_r ~ attend7+female+age+white+educ, weight=weight, data=b7)
summ(lm1)
lm2 <- lm(deathpen_r ~ attend7+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm2)
lm3 <- lm(deathpen_r ~ attend7+female+age+white+educ+cn1, weight=weight, data=b7)
summ(lm3)
lm4 <- lm(deathpen_r ~ attend7+female+age+white+educ+bible+cn1, weight=weight, data=b7)
summ(lm4)

export_summs(lm0, lm1, lm2, lm3, lm4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate", 
                             "Attendance+demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A2D.docx"))

p6 <- plot_summs(lm0, lm1, lm2, lm3, lm4, coefs = c(" "="attend7"),
                 model.names = c("Bivariate", "Attendance+demos",
                                 "With Literalism",
                                 "With Christian Nat.",
                                 "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Death Penalty", 
       y="Estimated Attendance Effect") +
  theme(text=element_text(family="G", size=12), legend.position="blank")


#Punish Criminals
lm0 <- lm(punish_r ~ attend7, weight=weight, data=b7)
summ(lm0)
lm1 <- lm(punish_r ~ attend7+female+age+white+educ, weight=weight, data=b7)
summ(lm1)
lm2 <- lm(punish_r ~ attend7+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm2)
lm3 <- lm(punish_r ~ attend7+female+age+white+educ+cn1, weight=weight, data=b7)
summ(lm3)
lm4 <- lm(punish_r ~ attend7+female+age+white+educ+bible+cn1, weight=weight, data=b7)
summ(lm4)

export_summs(lm0, lm1, lm2, lm3, lm4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate", "Attendance+demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A2E.docx"))

p7 <- plot_summs(lm0, lm1, lm2, lm3, lm4, coefs = c(" "="attend7"),
                 model.names = c("Bivariate", "Attendance+demos",
                                 "With Literalism",
                                 "With Christian Nat.",
                                 "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Punish Criminals", 
       y="") +
  theme(text=element_text(family="G", size=12), legend.position="bottom")

#Morality of War
lm0 <- lm(war_r ~ attend7, weight=weight, data=b7)
summ(lm0)
lm1 <- lm(war_r ~ attend7+female+age+white+educ, weight=weight, data=b7)
summ(lm1)
lm2 <- lm(war_r ~ attend7+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm2)
lm3 <- lm(war_r ~ attend7+female+age+white+educ+cn1, weight=weight, data=b7)
summ(lm3)
lm4 <- lm(war_r ~ attend7+female+age+white+educ+bible+cn1, weight=weight, data=b7)
summ(lm4)

export_summs(lm0, lm1, lm2, lm3, lm4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate", "Attendance+demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A2F.docx"))

p8 <- plot_summs(lm0, lm1, lm2, lm3, lm4, coefs = c(" "="attend7"),
                 model.names = c("Bivariate", "Attendance+demos",
                                 "With Literalism",
                                 "With Christian Nat.",
                                 "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Morality of War", 
       y="") +
  theme(text=element_text(family="G", size=12), legend.position="blank")

p1+p3+p4+p6+p7+p8 + plot_layout(ncol=3, axes="collect")
ggsave(file=paste0(files, "Figure_4.png"), type="cairo-png", height=6, width=9)


#Figure 5 – The Estimated Attendance Effect on Group/Figure Feelings is a Function of Model Specification####


#Immigrants
m21i <- m21 %>% gather(key="imvers", value="immigrants", q26_1, q27_1, q28_1, na.rm=TRUE)
m21i <- m21i %>% mutate(attend51=(attend5-1)/4)

lmi0 <- lm(immigrants ~ attend51, data=m21i)
summ(lmi0)
lmi1 <- lm(immigrants ~ attend51+white+age+ed+female, data=m21i)
summ(lmi1)
lmi2 <- lm(immigrants ~ attend51+q49+white+age+ed+female, data=m21i)
summ(lmi2)
lmi3 <- lm(immigrants ~ attend51+cn+white+age+ed+female, data=m21i)
summ(lmi3)
lmi4 <- lm(immigrants ~ attend51+q49+cn+white+age+ed+female, data=m21i)
summ(lmi4)

ps1 <- plot_summs(lmi0, lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Bivariate", "Attendance\n+Demos",
                                  "With Literalism",
                                  "With Christian Nat.",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Immigrants", 
       y="Estimated Attendance Effect") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

export_summs(lmi0, lmi1, lmi2, lmi3,lmi4,  error_format = "({p.value})", error_pos = "below",
             model.names=c("Bivariate","Attendance\n+Demos",
                           "With Literalism",
                           "With Christian Nat.",
                           "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A3A.docx"))

m21i <- m21 %>% gather(key="musvers", value="muslims", q26_2, q27_2, q28_2, na.rm=TRUE)
m21i <- m21i %>% mutate(attend51=(attend5-1)/4)
lmi0 <- lm(muslims ~ attend51, data=m21i)
summ(lmi0)
lmi1 <- lm(muslims ~ attend51+white+age+ed+female, data=m21i)
summ(lmi1)
lmi2 <- lm(muslims ~ attend51+q49+white+age+ed+female, data=m21i)
summ(lmi2)
lmi3 <- lm(muslims ~ attend51+cn+white+age+ed+female, data=m21i)
summ(lmi3)
lmi4 <- lm(muslims ~ attend51+q49+cn+white+age+ed+female, data=m21i)
summ(lmi4)

export_summs(lmi0, lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate", "Attendance\n+Demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A3B.docx"))


ps2 <- plot_summs(lmi0, lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Bivariate","Attendance\n+Demos",
                                  "With Literalism",
                                  "With Christian Nat.",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Muslims", 
       y="") +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom")

m21i <- m21 %>% gather(key="atvers", value="atheists", q26_12, q27_12, q28_12, na.rm=TRUE)
m21i <- m21i %>% mutate(attend51=(attend5-1)/4)
lmi0 <- lm(atheists ~ attend51, data=m21i)
summ(lmi0)
lmi1 <- lm(atheists ~ attend51+white+age+ed+female, data=m21i)
summ(lmi1)
lmi2 <- lm(atheists ~ attend51+q49+white+age+ed+female, data=m21i)
summ(lmi2)
lmi3 <- lm(atheists ~ attend51+cn+white+age+ed+female, data=m21i)
summ(lmi3)
lmi4 <- lm(atheists ~ attend51+q49+cn+white+age+ed+female, data=m21i)
summ(lmi4)

export_summs(lmi0, lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate","Attendance\n+Demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A3C.docx"))

ps3 <- plot_summs(lmi0, lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Bivariate","Attendance\n+Demos",
                                  "With Literalism",
                                  "With Christian Nat.",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Atheists", 
       y="") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

m21i <- m21 %>% gather(key="trvers", value="Trump", q26_9, q27_9, q28_9, na.rm=TRUE)
m21i <- m21i %>% mutate(attend51=(attend5-1)/4)
lmi0 <- lm(Trump ~ attend51, data=m21i)
summ(lmi0)
lmi1 <- lm(Trump ~ attend51+white+age+ed+female, data=m21i)
summ(lmi1)
lmi2 <- lm(Trump ~ attend51+q49+white+age+ed+female, data=m21i)
summ(lmi2)
lmi3 <- lm(Trump ~ attend51+cn+white+age+ed+female, data=m21i)
summ(lmi3)
lmi4 <- lm(Trump ~ attend51+q49+cn+white+age+ed+female, data=m21i)
summ(lmi4)

export_summs(lmi0, lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate","Attendance\n+Demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A3D.docx"))

ps4 <- plot_summs(lmi0, lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Bivariate","Attendance\n+Demos",
                                  "With Literalism",
                                  "With Christian Nat.",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Trump", 
       y="Estimated Attendance Effect") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

m21i <- m21 %>% gather(key="bivers", value="Biden", q26_6, q27_6, q28_6, na.rm=TRUE)
m21i <- m21i %>% mutate(attend51=(attend5-1)/4)
lmi0 <- lm(Biden ~ attend51, data=m21i)
summ(lmi0)
lmi1 <- lm(Biden ~ attend51+white+age+ed+female, data=m21i)
summ(lmi1)
lmi2 <- lm(Biden ~ attend51+q49+white+age+ed+female, data=m21i)
summ(lmi2)
lmi3 <- lm(Biden ~ attend51+cn+white+age+ed+female, data=m21i)
summ(lmi3)
lmi4 <- lm(Biden ~ attend51+q49+cn+white+age+ed+female, data=m21i)
summ(lmi4)

export_summs(lmi0, lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate","Attendance\n+Demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A3E.docx"))

ps5 <- plot_summs(lmi0, lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Bivariate","Attendance\n+Demos",
                                  "With Literalism",
                                  "With Christian Nat.",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Biden", 
       y="") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")


m21i <- m21 %>% gather(key="rivers", value="Rioters", q26_14, q27_14, q28_14, na.rm=TRUE)
m21i <- m21i %>% mutate(attend51=(attend5-1)/4)
lmi0 <- lm(Rioters ~ attend51, data=m21i)
summ(lmi0)
lmi1 <- lm(Rioters ~ attend51+white+age+ed+female, data=m21i)
summ(lmi1)
lmi2 <- lm(Rioters ~ attend51+q49+white+age+ed+female, data=m21i)
summ(lmi2)
lmi3 <- lm(Rioters ~ attend51+cn+white+age+ed+female, data=m21i)
summ(lmi3)
lmi4 <- lm(Rioters ~ attend51+q49+cn+white+age+ed+female, data=m21i)
summ(lmi4)

export_summs(lmi0, lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Bivariate","Attendance\n+Demos",
                             "With Literalism",
                             "With Christian Nat.",
                             "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A3F.docx"))

ps6 <- plot_summs(lmi0, lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Bivariate","Attendance\n+Demos",
                                  "With Literalism",
                                  "With Christian Nat.",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Insurrectionists", 
       y="") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

ps1 + ps2 + ps3 + ps4 + ps5 + ps6 + plot_layout(axes="collect")
ggsave(file=paste0(files, "Figure_5.png"), type="cairo-png", height=6, width=9)



#Figure A4 – The Estimated Attendance Effect on Group/Figure Feelings is a Function of Model Specification (higher values are more conservative)####
#September 2021 data

#Immigrants
lmi1 <- lm(q66_1 ~ attend51+white+age+ed+female, data=s21, weight=wgt)
summ(lmi1)
lmi2 <- lm(q66_1 ~ attend51+q35+white+age+ed+female, data=s21, weight=wgt)
summ(lmi2)
lmi3 <- lm(q66_1 ~ attend51+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi3)
lmi4 <- lm(q66_1 ~ attend51+q35+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi4)

export_summs(lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names=c("Just Attendance",
                           "With Literalism",
                           "With Christian Nationalism",
                           "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A4A.docx"))


ps1 <- plot_summs(lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Just Attendance",
                                  "With Literalism",
                                  "With Christian Nationalism",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Immigrants", 
       y="Estimated Attendance Effect") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

#Muslims
lmi1 <- lm(q66_2 ~ attend51+white+age+ed+female, data=s21, weight=wgt)
summ(lmi1)
lmi2 <- lm(q66_2 ~ attend51+q35+white+age+ed+female, data=s21, weight=wgt)
summ(lmi2)
lmi3 <- lm(q66_2 ~ attend51+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi3)
lmi4 <- lm(q66_2 ~ attend51+q35+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi4)

export_summs(lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names=c("Just Attendance",
                           "With Literalism",
                           "With Christian Nationalism",
                           "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A4B.docx"))


ps2 <- plot_summs(lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Just Attendance",
                                  "With Literalism",
                                  "With Christian Nationalism",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Muslims", 
       y="") +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom")

#Atheists
lmi1 <- lm(q66_12 ~ attend51+white+age+ed+female, data=s21, weight=wgt)
summ(lmi1)
lmi2 <- lm(q66_12 ~ attend51+q35+white+age+ed+female, data=s21, weight=wgt)
summ(lmi2)
lmi3 <- lm(q66_12 ~ attend51+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi3)
lmi4 <- lm(q66_12 ~ attend51+q35+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi4)

export_summs(lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names=c("Just Attendance",
                           "With Literalism",
                           "With Christian Nationalism",
                           "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A4C.docx"))


ps3 <- plot_summs(lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Just Attendance",
                                  "With Literalism",
                                  "With Christian Nationalism",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Atheists", 
       y="") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

#Trump
lmi1 <- lm(q66_9 ~ attend51+white+age+ed+female, data=s21, weight=wgt)
summ(lmi1)
lmi2 <- lm(q66_9 ~ attend51+q35+white+age+ed+female, data=s21, weight=wgt)
summ(lmi2)
lmi3 <- lm(q66_9 ~ attend51+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi3)
lmi4 <- lm(q66_9 ~ attend51+q35+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi4)

export_summs(lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names=c("Just Attendance",
                           "With Literalism",
                           "With Christian Nationalism",
                           "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A4D.docx"))


ps4 <- plot_summs(lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Just Attendance",
                                  "With Literalism",
                                  "With Christian Nationalism",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Trump", 
       y="Estimated Attendance Effect") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

#Evangelicals
lmi1 <- lm(q66_15 ~ attend51+white+age+ed+female, data=s21, weight=wgt)
summ(lmi1)
lmi2 <- lm(q66_15 ~ attend51+q35+white+age+ed+female, data=s21, weight=wgt)
summ(lmi2)
lmi3 <- lm(q66_15 ~ attend51+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi3)
lmi4 <- lm(q66_15 ~ attend51+q35+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi4)

export_summs(lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names=c("Just Attendance",
                           "With Literalism",
                           "With Christian Nationalism",
                           "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A4E.docx"))


ps5 <- plot_summs(lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Just Attendance",
                                  "With Literalism",
                                  "With Christian Nationalism",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Evangelicals", 
       y="") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

#Biden
lmi1 <- lm(q66_6 ~ attend51+white+age+ed+female, data=s21, weight=wgt)
summ(lmi1)
lmi2 <- lm(q66_6 ~ attend51+q35+white+age+ed+female, data=s21, weight=wgt)
summ(lmi2)
lmi3 <- lm(q66_6 ~ attend51+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi3)
lmi4 <- lm(q66_6 ~ attend51+q35+cn+white+age+ed+female, data=s21, weight=wgt)
summ(lmi4)

export_summs(lmi1, lmi2, lmi3, lmi4, error_format = "({p.value})", error_pos = "below",
             model.names=c("Just Attendance",
                           "With Literalism",
                           "With Christian Nationalism",
                           "Literalism +\nChristian Nationalism"),
             to.file="docx", file.name = paste0(files, "Table_A4F.docx"))


ps6 <- plot_summs(lmi1, lmi2, lmi3, lmi4, coefs = c(" "="attend51"),
                  model.names = c("Just Attendance",
                                  "With Literalism",
                                  "With Christian Nationalism",
                                  "Literalism +\nChristian Nationalism")) +
  theme_minimal() +
  labs(x="Effect on Feelings toward Biden", 
       y="") +
  theme(text=element_text(family="G", size=12),
        legend.position = "blank")

ps1 + ps2 + ps3 + ps4 + ps5 + ps6
ggsave(file=paste0(files, "Figure_A4.png"), type="cairo_png", height=6, width=9)


#Figure 6 – The Interactive Effect on Policy of Christian Nationalism Conditional on Attendance Yields a Variety of Patterns (higher positive values are more conservative stances)####
lm1 <- lm(marijuan_r ~ attend7*cn1+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm1)
lm2 <- lm(gunlaws_r ~ attend7*cn1+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm2)
lm3 <- lm(improve_r ~ attend7*cn1+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm3)
lm4 <- lm(deathpen_r ~ attend7*cn1+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm4)
lm5 <- lm(punish_r ~ attend7*cn1+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm5)
lm6 <- lm(war_r ~ attend7*cn1+female+age+white+educ+bible, weight=weight, data=b7)
summ(lm6)

export_summs(lm1, lm2, lm3, lm4, lm5, lm6, error_format = "({p.value})", error_pos = "below",
             model.names=c("Marijuana\nLegalization", "Gun\n Control",
                           "Improve\nSOL for\nMinorities", "Death\nPenalty",
                           "Punish\nCriminals", "Morality\nof War"),
             to.file="docx", file.name = paste0(files, "Table_A5.docx"))


ip_lm1 <- interact_plot(lm1, pred=cn1, modx=attend7, interval=T, int.width = .84,
                        modx.values = c("plus-minus"),
                        modx.labels = c("Low", "High"), legend.main = "Attendance") +
  labs(x="Christian Nationalism Scale", y="Marijuana Legalization")+
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "")

ip_lm2 <- interact_plot(lm2, pred=cn1, modx=attend7, interval=T, int.width = .84,
                        modx.values = c("plus-minus"),
                        modx.labels = c("Low", "High"), legend.main = "Attendance") +
  labs(x="Christian Nationalism Scale", y="Gun Control")+
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "")

ip_lm3 <- interact_plot(lm3, pred=cn1, modx=attend7, interval=T, int.width = .84,
                        modx.values = c("plus-minus"),
                        modx.labels = c("Low", "High"), legend.main = "Attendance") +
  labs(x="Christian Nationalism Scale", y="Improve Living for Minorities")+
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "")

ip_lm4 <- interact_plot(lm4, pred=cn1, modx=attend7, interval=T, int.width = .84,
                        modx.values = c("plus-minus"),
                        modx.labels = c("Low", "High"), legend.main = "Attendance") +
  labs(x="Christian Nationalism Scale", y="Death Penalty")+
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "")

ip_lm5 <- interact_plot(lm5, pred=cn1, modx=attend7, interval=T, int.width = .84,
                        modx.values = c("plus-minus"),
                        modx.labels = c("Low", "High"), legend.main = "Attendance") +
  labs(x="Christian Nationalism Scale", y="Punish Criminals")+
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom")

ip_lm6 <- interact_plot(lm6, pred=cn1, modx=attend7, interval=T, int.width = .84,
                        modx.values = c("plus-minus"),
                        modx.labels = c("Low", "High"), legend.main = "Attendance") +
  labs(x="Christian Nationalism Scale", y="Morality of War")+
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "")

ip_lm1+ip_lm2+ip_lm3+ip_lm4+ip_lm5+ip_lm6 +plot_layout(axes="collect")
ggsave(file=paste0(files, "Figure_6.png"), type="cairo_png", height=6, width=9)



#Figure 7 – Worship Attendance Does Not Condition Christian Nationalism’s Effect on the 2016 Vote for Trump####

lmv <- glm(trv ~ cn1*attend3+female+white+reltrad+I_EDUC+age, data=b17, family="binomial", weight=weight)
summ(lmv)
interact_plot(lmv, pred=cn1, modx=attend3, interval=TRUE, int.width = .84,
              legend.main = "Attendance") +
  theme_minimal() +
  scale_x_continuous(breaks=c(0, 1), labels=c("Lowest", "Highest")) +
  labs(x="Christian Nationalism Index", y="Probability of a Trump Vote") +
  theme(text=element_text(family="G", size=12))
ggsave(file=paste0(files, "Figure_7.png"), type="cairo-png", width=5, height=4)

export_summs(lmv, error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(files, "Table_A6.docx"))

#Removes####
rm(list=ls(pattern="lm"))
rm(list=ls(pattern="ps"))
rm(list=ls(pattern="att"))
rm(list=ls(pattern="b"))
rm(list=ls(pattern="m"))
rm(list=ls(pattern="p"))
rm(list=ls(pattern="s"))
rm(ces)