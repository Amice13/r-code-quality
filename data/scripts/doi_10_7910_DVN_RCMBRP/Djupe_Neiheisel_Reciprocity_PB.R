#Replication Code for 
#The Dimensions and Effects of Reciprocity in Political Tolerance Judgments
#Forthcoming at Political Behavior
#Paul A. Djupe (Denison University) and Jacob R. Neiheisel (SUNY, Buffalo)

#Packages - you may need to install these packages first
library(tidyverse)
library(showtext)
font_add_google("EB Garamond", "G", regular.wt = 400)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
#library(devtools)
#install_github("ryanburge/socsci")
library(socsci)
library(ggthemes)
library(interactions)
library(jtools)
#some users may require library(ggstance)

#Import 2016 Panel and May 2018 data -- Insert your own file locations for "..."
d16 <- read.csv(".../2016_panel_PB.csv")
may18 <- read.csv(".../may2018_recoded_PB.csv")

#Figure 1 - Responsiveness of Expected Reciprocity to Partisan Ingroup/Outgroup Status (June 2017)
d16rec1 <- d16 %>% gather(key="llgselect", value="j_reciprocity", 
                          j_q29_1, j_q29_2, j_q29_3, j_q29_4,j_q29_5, na.rm=TRUE)
d16rec2 <- d16 %>% gather(key="llgselect", value="j_reciprocity", 
                          j_q35_1, j_q35_2, j_q35_3, j_q35_4,j_q35_5, na.rm=TRUE)
d16rec <- bind_rows(d16rec1, d16rec2)

d16rec <- d16rec %>% mutate(llgselect=frcode(llgselect=="j_q29_1" | llgselect=="j_q35_1" ~ "L",
                                             llgselect=="j_q29_2" | llgselect=="j_q35_2" ~ "R", 
                                             llgselect=="j_q29_3" | llgselect=="j_q35_3" ~ "D", 
                                             llgselect=="j_q29_4" | llgselect=="j_q35_4" ~ "C", 
                                             llgselect=="j_q29_5" | llgselect=="j_q35_5" ~ "A"))
d16rec <- d16rec %>% mutate(pid3lab=frcode(pid3==0 ~ "Democrat",
                                           pid3==1 ~ "Independent",
                                           pid3==2 ~ "Republican"))
d16rec %>% filter(pid3lab!="NA") %>% group_by(pid3lab, llgselect) %>% mean_ci((6-j_reciprocity)/5, ci=.84) %>% 
  ggplot(., aes(x=pid3lab, y=mean, group=llgselect)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(width=.3)) +
  geom_point(shape=19, color="white", size=5, position=position_dodge(width=.3)) +
  geom_point(shape=1, size=4, position=position_dodge(width=.3)) +
  geom_text(aes(label=llgselect), size=3, family="G", position=position_dodge(width=.3)) +
  coord_flip() +
  theme_hc() +
  theme(text=element_text(family="G"),
        legend.position = "blank") +
  labs(x="", y="Expected Reciprocity\n0=They Would not, 1=They Would")

  
#Figure 2 - The Tight Links of Threat Types and Expected Reciprocity (September 2016)
d16 %>% ggplot(., aes(y=recip1))+
  geom_smooth(aes(x=threat7r), method="lm", linetype="dashed") + 
  annotate("text", x=.05, y=.48, label="American\nWay of Life", family="G", size=4) +
  geom_segment(x=.2, xend=.18, y=.6, yend=.54) +
  geom_smooth(aes(x=threat8r), method="lm", linetype="solid") +
  annotate("text", x=.2, y=.62, label="People's Freedoms", family="G", size=4) +
  geom_segment(x=.05, xend=.06, y=.505, yend=.53) +
  geom_smooth(aes(x=threat9r), method="lm", linetype="dotted") +
  annotate("text", x=.18, y=.32, label="Personal Threat", family="G", size=4) +
  geom_segment(x=.17, xend=.19, y=.33, yend=.365) +
  theme_minimal() +
  labs(x="Greater Threat ->", y="Expected Reciprocity\n0=They would not, 1=They would") +
  theme(text=element_text(family="G", size=12))  

#Table 1 - OLS Estimates of Differenced Political Tolerance (all wave 2 - wave 1)####
d16 <- d16 %>% mutate(ft_llg1_prejw=case_when(llgw==1 ~ ftw_im,
                                              llgw==2 ~ ftw_mu,
                                              llgw==3 ~ ftw_djt,
                                              llgw==4 ~ ftw_hrc,
                                              llgw==5 ~ ftw_ho,
                                              llgw==6 ~ ftw_cf,
                                              llgw==7 ~ ftw_ath,
                                              llgw==8 ~ ftw_whs))
d16 <- d16 %>% mutate(ft_llg1_prejw=ft_llg1_prejw/100)

d16 <- d16 %>% mutate(ft_llg1_prej=case_when(llg==1 ~ ft_im,
                                             llg==2 ~ ft_mu,
                                             llg==3 ~ ft_djt,
                                             llg==4 ~ ft_hrc,
                                             llg==5 ~ ft_ho,
                                             llg==6 ~ ft_cf,
                                             llg==7 ~ ft_ath,
                                             llg==8 ~ ft_whs))
d16 <- d16 %>% mutate(ft_llg1_prej=ft_llg1_prej/100)

d16 <- d16 %>% mutate(recip_dif=recipw1-recip1,
                      tol_dif=tolw-tol,
                      threat_dif=threatw-threat,
                      ft_llg1_prej_dif=ft_llg1_prejw-ft_llg1_prej)

#This is the reputation of the groups in wave 1
rmeans <- d16 %>% group_by(llg) %>% summarize(recip1=mean(recip1)) %>%
  mutate(llgname=paste0("llg", llg)) %>% dplyr::select(-llg) %>% 
  pivot_wider(names_from="llgname", values_from="recip1") %>% mutate(llgid=1)
d16 <- d16 %>% mutate(llgid=1)
d16 <- left_join(d16, rmeans, by="llgid")

d16 <- d16 %>% mutate(recip_rep=case_when(llg==1 & llgw==1 ~ (llg1.y-llg1.y),
                                          llg==1 & llgw==2 ~ (llg1.y-llg2.y),
                                          llg==1 & llgw==3 ~ (llg1.y-llg3.y),
                                          llg==1 & llgw==4 ~ (llg1.y-llg4.y),
                                          llg==1 & llgw==5 ~ (llg1.y-llg5.y),
                                          llg==1 & llgw==6 ~ (llg1.y-llg6.y),
                                          llg==1 & llgw==7 ~ (llg1.y-llg7.y),
                                          llg==1 & llgw==8 ~ (llg1.y-llg8.y),
                                          llg==2 & llgw==1 ~ (llg2.y-llg1.y),
                                          llg==2 & llgw==2 ~ (llg2.y-llg2.y),
                                          llg==2 & llgw==3 ~ (llg2.y-llg3.y),
                                          llg==2 & llgw==4 ~ (llg2.y-llg4.y),
                                          llg==2 & llgw==5 ~ (llg2.y-llg5.y),
                                          llg==2 & llgw==6 ~ (llg2.y-llg6.y),
                                          llg==2 & llgw==7 ~ (llg2.y-llg7.y),
                                          llg==2 & llgw==8 ~ (llg2.y-llg8.y),
                                          llg==3 & llgw==1 ~ (llg3.y-llg1.y),
                                          llg==3 & llgw==2 ~ (llg3.y-llg2.y),
                                          llg==3 & llgw==3 ~ (llg3.y-llg3.y),
                                          llg==3 & llgw==4 ~ (llg3.y-llg4.y),
                                          llg==3 & llgw==5 ~ (llg3.y-llg5.y),
                                          llg==3 & llgw==6 ~ (llg3.y-llg6.y),
                                          llg==3 & llgw==7 ~ (llg3.y-llg7.y),
                                          llg==3 & llgw==8 ~ (llg3.y-llg8.y),
                                          llg==4 & llgw==1 ~ (llg4.y-llg1.y),
                                          llg==4 & llgw==2 ~ (llg4.y-llg2.y),
                                          llg==4 & llgw==3 ~ (llg4.y-llg3.y),
                                          llg==4 & llgw==4 ~ (llg4.y-llg4.y),
                                          llg==4 & llgw==5 ~ (llg4.y-llg5.y),
                                          llg==4 & llgw==6 ~ (llg4.y-llg6.y),
                                          llg==4 & llgw==7 ~ (llg4.y-llg7.y),
                                          llg==4 & llgw==8 ~ (llg4.y-llg8.y),
                                          llg==5 & llgw==1 ~ (llg5.y-llg1.y),
                                          llg==5 & llgw==2 ~ (llg5.y-llg2.y),
                                          llg==5 & llgw==3 ~ (llg5.y-llg3.y),
                                          llg==5 & llgw==4 ~ (llg5.y-llg4.y),
                                          llg==5 & llgw==5 ~ (llg5.y-llg5.y),
                                          llg==5 & llgw==6 ~ (llg5.y-llg6.y),
                                          llg==5 & llgw==7 ~ (llg5.y-llg7.y),
                                          llg==5 & llgw==8 ~ (llg5.y-llg8.y),
                                          llg==6 & llgw==1 ~ (llg6.y-llg1.y),
                                          llg==6 & llgw==2 ~ (llg6.y-llg2.y),
                                          llg==6 & llgw==3 ~ (llg6.y-llg3.y),
                                          llg==6 & llgw==4 ~ (llg6.y-llg4.y),
                                          llg==6 & llgw==5 ~ (llg6.y-llg5.y),
                                          llg==6 & llgw==6 ~ (llg6.y-llg6.y),
                                          llg==6 & llgw==7 ~ (llg6.y-llg7.y),
                                          llg==6 & llgw==8 ~ (llg6.y-llg8.y),
                                          llg==7 & llgw==1 ~ (llg7.y-llg1.y),
                                          llg==7 & llgw==2 ~ (llg7.y-llg2.y),
                                          llg==7 & llgw==3 ~ (llg7.y-llg3.y),
                                          llg==7 & llgw==4 ~ (llg7.y-llg4.y),
                                          llg==7 & llgw==5 ~ (llg7.y-llg5.y),
                                          llg==7 & llgw==6 ~ (llg7.y-llg6.y),
                                          llg==7 & llgw==7 ~ (llg7.y-llg7.y),
                                          llg==7 & llgw==8 ~ (llg7.y-llg8.y),
                                          llg==8 & llgw==1 ~ (llg8.y-llg1.y),
                                          llg==8 & llgw==2 ~ (llg8.y-llg2.y),
                                          llg==8 & llgw==3 ~ (llg8.y-llg3.y),
                                          llg==8 & llgw==4 ~ (llg8.y-llg4.y),
                                          llg==8 & llgw==5 ~ (llg8.y-llg5.y),
                                          llg==8 & llgw==6 ~ (llg8.y-llg6.y),
                                          llg==8 & llgw==7 ~ (llg8.y-llg7.y),
                                          llg==8 & llgw==8 ~ (llg8.y-llg8.y)))            
d16 <- d16 %>% mutate(recip_repc=frcode(recip_rep==0 ~ "Same",
                                        recip_rep<0 ~ "Higher",
                                        recip_rep>0 ~ "Lower"))

lmtoldiff <- lm(tol_dif ~ threat_dif  + recip_dif + ft_llg1_prej_dif+recip_repc, data=d16)
summ(lmtoldiff)

#Figure 3 - Some Correlates of Expected Reciprocity (September 2016)
d16 <- d16 %>% mutate(pid7f=frcode(pid7==1 ~ "Strong Democrat",
                                   pid7==2 ~ "Democrat",
                                   pid7==3 ~ "Lean Democrat",
                                   pid7==4 ~ "Independent",
                                   pid7==5 ~ "Lean Republican",
                                   pid7==6 ~ "Republican",
                                   pid7==7 ~ "Strong Republican")) 
lmwt <- lm(recip1 ~ ft_llg1_prej+threat+majority1+sdor+risk1+pid7f+age+female+ed+white+
             llg_d2+llg_d3+llg_d4+llg_d5+llg_d6+llg_d7+llg_d8+per_o1+per_c1+per_e1+per_a1+per_n1, 
           data=d16)

lmnot <- lm(recip1 ~ ft_llg1_prej+majority1+sdor+risk1+pid7f+age+female+ed+white+
              llg_d2+llg_d3+llg_d4+llg_d5+llg_d6+llg_d7+llg_d8+per_o1+per_c1+per_e1+per_a1+per_n1, 
            data=d16)
plot_summs(lmnot, lmwt, inner_ci_level = .9, model.names = c("Without threat", "With threat"),
           legend.title = "",
           coefs = c("Prejudice" = "ft_llg1_prej",
                     "Majoritarianism" = "majority1",
                     "Social dominance" = "sdor",
                     "Risk aversion" = "risk1",
                     "Democrat" = "pid7fDemocrat",
                     "Lean Democrat" = "pid7fLean Democrat",
                     "Independent" = "pid7fIndependent",
                     "Lean Republican" = "pid7fLean Republican",
                     "Republican" = "pid7f Republican",
                     "Strong Republican" = "pid7fStrong Republican",
                     "Age" = "age",
                     "Women" = "female",
                     "Education" = "ed",
                     "White" = "white",
                     "LLG:Muslims"="llg_d2",
                     "LLG:Trump supporters" = "llg_d3",
                     "LLG:Clinton supporters" = "llg_d4",
                     "LLG:Homosexuals" = "llg_d5",
                     "LLG:Christian fundamentalists" = "llg_d6",
                     "LLG:Atheists" = "llg_d7",
                     "LLG:White supremacists" = "llg_d8",
                     "Openness" = "per_o1",
                     "Conscientiousness" = "per_c1",
                     "Extraversion" = "per_e1",
                     "Agreeableness" = "per_a1",
                     "Emotional stability" = "per_n1",
                     "Threat" = "threat")) +
  theme(text=element_text(family="G", size=12),
        axis.text.y = element_text(family="G", size=10),
        axis.text.x = element_text(family="G", size=10),
        axis.title.x = element_text(family="G", size=12),
        legend.position = "bottom")
rm(lmnot, lmwt)

#Figure 4 - Reciprocity Links to Tolerance with and without Threat (September 2016)
lmtolwot <- lm(tol ~ recip1+ft_llg1_prej+pid7f+age+female+ed+white+
                 llg_d2+llg_d3+llg_d4+llg_d5+llg_d6+llg_d7+llg_d8+attend*evan, 
               data=d16)

lmtolwt <- lm(tol ~ recip1+threat+ft_llg1_prej+pid7f+age+female+ed+white+
                llg_d2+llg_d3+llg_d4+llg_d5+llg_d6+llg_d7+llg_d8+attend*evan, 
              data=d16)

plot_summs(lmtolwot, lmtolwt, inner_ci_level = .9, model.names = c("Without threat", "With threat"),
           legend.title = "",
           coefs = c("Reciprocity" = "recip1",
                     "Prejudice" = "ft_llg1_prej",
                     "Threat" = "threat",
                     "Democrat" = "pid7fDemocrat",
                     "Lean Democrat" = "pid7fLean Democrat",
                     "Independent" = "pid7fIndependent",
                     "Lean Republican" = "pid7fLean Republican",
                     "Republican" = "pid7f Republican",
                     "Strong Republican" = "pid7fStrong Republican",
                     "Age" = "age",
                     "Women" = "female",
                     "Education" = "ed",
                     "White" = "white",
                     "LLG:Muslims"="llg_d2",
                     "LLG:Trump supporters" = "llg_d3",
                     "LLG:Clinton supporters" = "llg_d4",
                     "LLG:Homosexuals" = "llg_d5",
                     "LLG:Christian fundamentalists" = "llg_d6",
                     "LLG:Atheists" = "llg_d7",
                     "LLG:White supremacists" = "llg_d8",
                     "Attendance" = "attend",
                     "Evangelical" = "evan",
                     "Attendance * Evangelical" = "attend:evan")) +
  theme(text=element_text(family="G", size=12),
        axis.text.y = element_text(family="G", size=10),
        axis.text.x = element_text(family="G", size=10),
        axis.title.x = element_text(family="G", size=12),
        legend.position = "bottom")
rm(lmtolwot, lmtolwt)

#Figure A1 - 
d16 <- d16 %>% mutate(llglab=frcode(llg==1 ~ "Immigrants",
                                    llg==2 ~ "Muslims",
                                    llg==3 ~ "Trump supporters",
                                    llg==4 ~ "Clinton supporters",
                                    llg==5 ~ "Homosexuals",
                                    llg==6 ~ "Christian fundamentalists",
                                    llg==7 ~ "Atheists",
                                    llg==8 ~ "White supremacists"))

d16 <- d16 %>% mutate(llgwlab=frcode(llgw==1 ~ "Immigrants",
                                     llgw==2 ~ "Muslims",
                                     llgw==3 ~ "Trump supporters",
                                     llgw==4 ~ "Clinton supporters",
                                     llgw==5 ~ "Homosexuals",
                                     llgw==6 ~ "Christian fundamentalists",
                                     llgw==7 ~ "Atheists",
                                     llgw==8 ~ "White supremacists"))

d16w1 <- d16 %>% group_by(llglab) %>% mean_ci(recip1, ci=.76) %>% mutate(wave=1)
d16w2 <- d16 %>% group_by(llgwlab) %>% mean_ci(recipw1, ci=.76) %>% mutate(wave=2) %>% rename(llglab=llgwlab)
d16w1w2 <- bind_rows(d16w1, d16w2)

d16w1w2 %>% filter(llglab!="NA") %>% mutate(wave=as.factor(wave)) %>% 
  ggplot(., aes(x=reorder(llglab, -mean), y=mean, shape=wave)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(width=.3)) +
  geom_point(position=position_dodge(width=.3)) +
  coord_flip() +
  scale_shape_manual(values=c(1, 19)) +
  theme_hc() +
  labs(y="Expected Reciprocity\n0=They would not, 1=They would", x="") +
  scale_y_continuous(breaks=c(.2, .4, .6)) +
  theme(text=element_text(family="G", size=12),
        panel.grid.major.y = element_blank(),
        legend.position = "blank") +
  annotate("text", x=1.3, y=.52, label="Wave 1", family="G", size=3) +
  annotate("text", x=1.3, y=.4, label="Wave 2", family="G", size=3)


#Figure 5 - The Estimated Effect on the Difference in Reciprocity Judgments#### 
  #between Parties Given Random Assignment to a Tolerance Task among Independents (May 2018)

#taking the difference in expected reciprocity between Democrats and Republicans on a -1 to +1 scale
#taking the feeling therm difference toward Democrats and Republicans
#Subsetting just to independents
indies <- may18 %>% 
  mutate(rec_diff1=(recip_dic-recip_ric)/5) %>% 
  mutate(ft_diff=ft_5-ft_4) %>% 
  filter(pid3=="Independent")

#Setting the treatment labels
#0=asked to provide tolerance to Democrats
#1=asked to provide tolerance to Republicans
indies <- indies %>% mutate(t_tol=frcode(t_tol=="0" ~ "Democrats",
                                         t_tol=="1" ~ "Republicans"))
#tolc is a tolerance battery described in the appendix

#Figure 5
lmrec1 <- lm(rec_diff1 ~ t_tol+tolc+ft_diff+female+age+ed+white, data=indies)
summ(lmrec1)
cat_plot(lmrec1, pred=t_tol, int.width = .95, errorbar.width = .05) +
  theme_minimal() +
  labs(x="Partisan Assignment", y="Expected Reciprocity Difference\n(higher is more from Democrats)") +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme(panel.grid.major = element_line(linetype="solid"),
        text=element_text(family="G", size=12))


#Removes
rm(list=ls(pattern="lm"))
rm(list=ls(pattern="may"))
rm(list=ls(pattern="d16"))
rm(indies)
rm(rmeans)