#Replication For####
# Religion is Sometimes Raced: Christian Nationalism is Ingroup Protection
# 
# Brooklyn Walker, University of Tennessee, Knoxville
# Paul A. Djupe, Denison University
# Brian R. Calfano, latakoo, Inc.
# Andrew R. Lewis, University of Cincinnati
# Anand E. Sokhey, University of Colorado, Boulder


#Package Installs (if necessary)####
install.packages("tidyverse")
install.packages("rio")
install.packages("remotes")
remotes::install_github("ryanburge/socsci")
install.packages("car")
install.packages("showtext")
install.packages("jtools")
install.packages("interactions")
install.packages("patchwork")
install.packages("MASS")

#Libraries####
library(rio)
library(socsci)
library(tidyverse)
library(MASS)
library(car)
library(showtext)
font_add_google("EB Garamond", "G", regular.wt = 400)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
library(jtools)
library(interactions)
library(patchwork)


#Loading Data -- specify the data directory first#### irst
cnrdir <- "<INSERT_FILE_LOCATION>"

ppr <- import(paste0(cnrdir, "ppr_replicate.csv"))
O20 <- import(paste0(cnrdir, "O20_replicate.csv"))


#Table A1 - Summary Statistics by Race####
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(cn, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(pid7, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(ideology, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(attend5, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(ba, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(ed, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(age, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(female, wt=wgt)

#DVs
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q49_5r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q49_6r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q49_7r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q50_2r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q32, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(govrole, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q50_4r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q49_2r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q50_5r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q49_3r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q50_1r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q50_3r, wt=wgt)
ppr %>% filter(wgt!="NA") %>% group_by(race) %>% mean_ci(q50_6r, wt=wgt)


#Figure 1 -- Predicting CN by race ####
#Split into datasets by race
pprw <- ppr %>% filter(race=="White")
pprb <- ppr %>% filter(race=="Black")
pprh <- ppr %>% filter(race=="Latino/a")

lmcnw <- lm(cn ~ pid7+ideology+attend5+ba+ed+age+income+female, data=pprw, weight=wgt)  
lmcnb <- lm(cn ~ pid7+ideology+attend5+ba+ed+age+income+female, data=pprb, weight=wgt)  
lmcnh <- lm(cn ~ pid7+ideology+attend5+ba+ed+age+income+female, data=pprh, weight=wgt)  

plot_coefs(lmcnw, lmcnb, lmcnh, model.names = c("White", "Black", "Latino"),
           coefs = c("Partisanship"="pid7",
                     "Ideology"="ideology",
                     "Worship attendance"="attend5",
                     "Evangelical ID"="ba",
                     "Education"="ed",
                     "Age"="age",
                     "Income"="income",
                     "Female"="female")) +
  theme_minimal() +
  geom_vline(xintercept=0, color="red", linetype="dashed") +
  theme(text=element_text(family="G", size=12)) +
  labs(x="Estimated Effect on Christian Nationalism", y="")
ggsave(file=paste0(cnrdir, "Fig1_coefplot.png"), width=6, height=4)
ggsave(file=paste0(cnrdir, "Fig1_coefplot.eps"), width=6, height=4)

#Appendix Table A1
export_summs(lmcnw, lmcnb, lmcnh, error_format = "({p.value})", error_pos = "below",
             model.names = c("White", "Black", "Latino/a"), 
             to.file="docx", file.name = paste0(cnrdir, "tableA2.docx"),
             coefs = c("Partisanship"="pid7",
                       "Ideology"="ideology",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Age"="age",
                       "Income"="income",
                       "Female"="female"))
rm(lmcnw, lmcnb, lmcnh)


#Figure 2 -- Racialized Issues####

#Teaching about race
lmr1 <- lm(q49_5r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weigh=wgt)
ipr1 <- interact_plot(lmr1, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Teaching about race in American history in public schools") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Reparations
lmr2 <- lm(q49_6r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weigh=wgt)
ipr2 <- interact_plot(lmr2, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Offering reparations to descendants of slaves") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Reforming Procedures
lmr3 <- lm(q49_7r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weigh=wgt)
ipr3 <- interact_plot(lmr3, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Reforming procedures used by police departments") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Easier to vote
lmr4 <- lm(q50_2r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weigh=wgt)
ipr4 <- interact_plot(lmr4, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="US should make it easier to vote with election-day voter registration") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

ipr1 + ipr2 + ipr3 + ipr4 + plot_layout(ncol=2, guides="collect") & theme(legend.position = "bottom")
ggsave(file=paste0(cnrdir, "Fig2 - racialized_issues.png"), type="cairo-png", height=8, width=10)
ggsave(file=paste0(cnrdir, "Fig2 - racialized_issues.eps"), height=8, width=10)

rm(ipr1, ipr2, ipr3, ipr4)

    #Figure 2 Appendix Tables -- Table A2
export_summs(lmr1, lmr2, lmr3, lmr4, error_format = "({p.value})", error_pos = "below",
             model.names = c("Teaching about race", "Offering Reparations", 
                             "Reforming police", "Easier to vote"), 
             to.file="docx", file.name = paste0(cnrdir, "TableA3_race.docx"),
             coefs = c("Partisanship"="pid7",
                       "Ideology"="ideology",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Age"="age",
                       "Income"="income",
                       "Women"="genderWomen",
                       "Non-binary+"="genderNon-binary+",
                       "Latino/a"="raceLatino/a",
                       "White"="raceWhite",
                       "Christian nationalism"="cn",
                       "Latino/a*CN"="raceLatino/a:cn",
                       "White*CN"="raceWhite:cn"
                       ))
rm(lmr1, lmr2, lmr3, lmr4)



#Figure 3 -- Economic Issues####
lmq32w <- lm(q32 ~ cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=pprw)
lmq32b <- lm(q32 ~ cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=pprb)
lmq32h <- lm(q32 ~ cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=pprh)

plot_coefs(lmq32w, lmq32h, lmq32b, inner_ci_level = .90,
           coefs=c("Christian\nNationalism"="cn"),
           model.names = c("White", "Latino/a", "Black")) +
  labs(title="Government Role in Providing Jobs")

#Get ahead on their own
lme1 <- lm(q32 ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weight=wgt)
ipe1 <- interact_plot(lme1, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Government should let each person get ahead on their own") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Govt responsibility
lme2 <- lm(govrole ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weight=wgt)
ipe2 <- interact_plot(lme2, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Government's Responsibility to Care for Elderly, Sick, Unemployed") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Minimum Wage
lme3 <- lm(q50_4r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weight=wgt)
ipe3 <- interact_plot(lme3, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="The federal minimum wage should be increased to $15/hr") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

ipe1 + ipe2 + ipe3 + plot_layout(ncol=2, guides="collect") & theme(legend.position = "bottom")
ggsave(file=paste0(cnrdir, "Fig3 - economic.png"), type="cairo-png", height=8, width=10)
ggsave(file=paste0(cnrdir, "Fig3 - economic.eps"), height=8, width=10)


export_summs(lme1, lme2, lme3, error_format = "({p.value})", error_pos = "below",
             model.names = c("Get ahead on own", "Government care\nfor elderly, sick", "Minimum Wage"), 
             to.file="docx", file.name = paste0(cnrdir, "TableA4_economic.docx"),
             coefs = c("Partisanship"="pid7",
                       "Ideology"="ideology",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Age"="age",
                       "Income"="income",
                       "Women"="genderWomen",
                       "Non-binary+"="genderNon-binary+",
                       "Latino/a"="raceLatino/a",
                       "White"="raceWhite",
                       "Christian nationalism"="cn",
                       "Latino/a*CN"="raceLatino/a:cn",
                       "White*CN"="raceWhite:cn"
             ))
rm(lme1, lme2, lme3)



#Figure 4 - Social Issues####

#Abortion illegal
lms1 <- lm(q49_2r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips1 <- interact_plot(lms1, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Making abortion illegal in the entire nation\n(except to save the life of the mother)") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#LGBT anti-discrimination
lms2 <- lm(q50_5r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips2 <- interact_plot(lms2, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="I would favor a law that would protect LGBT people against\n discrimination in jobs, public accommodations, and housing") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Small business service refusal
lms3 <- lm(q49_3r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips3 <- interact_plot(lms3, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Allowing a small business owner in your state to refuse\nto provide products or services to gay or lesbian people\nif doing so violates their religious beliefs.") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Ban SSM
lms4 <- lm(q50_1r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips4 <- interact_plot(lms4, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Banning same-sex marriage in my state") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Flag burning
lms5 <- lm(q50_3r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips5 <- interact_plot(lms5, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Burning the American flag is wrong because it is sacred") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

#Background checks
lms6 <- lm(q50_6r ~ race*cn+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips6 <- interact_plot(lms6, pred=cn, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="There should be background checks for all gun sales,\nincluding at gun shows and over the internet") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

ips1 + ips2 + ips3 + ips4 + ips5 + ips6 + plot_layout(ncol=2, guides="collect") & theme(legend.position="bottom")
ggsave(file=paste0(cnrdir, "Fig4 - social_issues.png"), type="cairo-png", height=12, width=10)
ggsave(file=paste0(cnrdir, "Fig4 - social_issues.eps"), height=12, width=10)

rm(ips1, ips2, ips3, ips4, ips5, ips6)

    #Table A4
export_summs(lms1, lms2, lms3, lms4, lms5, lms6, error_format = "({p.value})", error_pos = "below",
             model.names = c("Abortion Ban", "LGBT Anti-\nDiscrimination", 
                             "Small Business\nException", "Ban SSM",
                             "Ban Flag\nBurning", "Gun Background\nChecks"), 
             to.file="docx", file.name = paste0(cnrdir, "TableA5_social.docx"),
             coefs = c("Partisanship"="pid7",
                       "Ideology"="ideology",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Age"="age",
                       "Income"="income",
                       "Women"="genderWomen",
                       "Non-binary+"="genderNon-binary+",
                       "Latino/a"="raceLatino/a",
                       "White"="raceWhite",
                       "Christian nationalism"="cn",
                       "Latino/a*CN"="raceLatino/a:cn",
                       "White*CN"="raceWhite:cn"
             ))
rm(lms1, lms2, lms3, lms4, lms5, lms6)



#Figure 5 -- Persecution Comparison ####

lmcph <- glm.nb(cph4t ~ cn*race+pid7+attend5+ ba + age+ed+female, data=O20, weight=wgt)
summ(lmcph)
ip_O20 <- interact_plot(lmcph, pred=cn, modx=race, interval=T, int.width = .84,
              modx.values = c("Black", "Latino/a", "White"),
              legend.main = "") +
  labs(x="Christian Nationalism Scale", y="Christian Persecution Claims Heard") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom")

lmcphp <- lm(q38_2r ~ cn*race+pid7  + attend5 + ba + age+ed+female, data=ppr, weight=wgt)
summ(lmcphp)
ip_ppr <- interact_plot(lmcphp, pred=cn, modx=race, interval=T, int.width = .84,
              modx.values = c("Black", "Latino/a", "White"),
              legend.main = "") +
  labs(x="Christian Nationalism Scale", y="Discrimination Against Christians") +
  expand_limits(y=c(0,4)) +
  scale_y_continuous(breaks=c(0,1,2,3), labels=c("None", "A little", "Some", "A lot")) +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom")

ip_O20 + ip_ppr + plot_layout(guides="collect") & theme(legend.position="bottom")
ggsave(file=paste0(cnrdir, "Fig5 - discrimination.png"), type="cairo-png", height=4, width=8)
ggsave(file=paste0(cnrdir, "Fig5 - discrimination.eps"), height=4, width=8)

rm(ip_O20, ip_ppr)

    #Table A5
export_summs(lmcph, lmcphp, error_format = "({p.value})", error_pos = "below",
             model.names = c("Christian Persecution\nClaims Heard",
                             "Discrimination Against\nChristians Perceived"), 
             to.file="docx", file.name = paste0(cnrdir, "TableA6_persecution.docx"),
             coefs = c("Partisanship"="pid7",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Age"="age",
                       "Women"="female",
                       "Latino/a"="raceLatino/a",
                       "White"="raceWhite",
                       "Christian nationalism"="cn",
                       "Latino/a*CN"="cn:raceLatino/a",
                       "White*CN"="cn:raceWhite"
             ))
rm(lmcph, lmcphp)



#Figure 6 - Loyalty and Extreme Group Politics####

ppr <- ppr %>% mutate(loyalty=((((q57_4-1)+(q57_8-1))/6)+((5-q59_5)+(5-q59_6))/8)/2)

loyalty <- lm (loyalty ~ cn*race + pid7  + attend5 + ba + ed + age + income + female, data = ppr, weight=wgt)
summary (loyalty)
loy_plot <- interact_plot (loyalty, pred = cn, modx = race, interval = TRUE, int.width=.84, 
                           legend.main="") +
  labs(y = "Loyalty to Group", x = "Christian Nationalism") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12), 
        legend.position="", 
        strip.text=element_text(size=14))

grprts <- lm (grprts ~ cn*race + pid7 + attend5 + ba + ed + age + income + female, data = ppr, weight=wgt)
summary (grprts)
grp_plot <- interact_plot (grprts, pred = cn, modx = race, interval = TRUE, int.width=.84, legend.main="") +
  labs(y = "Extreme Group Support", x = "Christian Nationalism") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12), 
        legend.position="bottom", 
        strip.text=element_text(size=14))

loy_plot + grp_plot  + 
  plot_layout(ncol=2, guides="collect") & theme(legend.position="bottom")
ggsave(file=paste0(cnrdir, "Fig6 - loyalty.png"), type="cairo-png", height=4, width=8)
ggsave(file=paste0(cnrdir, "Fig6 - loyalty.eps"), height=4, width=8)

rm(loy_plot, grp_plot)

    #Table A6
export_summs(loyalty, grprts, error_format = "({p.value})", error_pos = "below",
             model.names = c("Loyalty to group",
                             "Extreme group support"), 
             to.file="docx", file.name = paste0(cnrdir, "TableA7_loyalty.docx"),
             coefs = c("Partisanship"="pid7",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Age"="age",
                       "Women"="female",
                       "Latino/a"="raceLatino/a",
                       "White"="raceWhite",
                       "Christian nationalism"="cn",
                       "Latino/a*CN"="cn:raceLatino/a",
                       "White*CN"="cn:raceWhite"
             ))
rm(loyalty, grprts)



#Doing All that stuff above with the Davis 2 item measure for the Appendix.####
lmcnw <- lm(cnd ~ pid7+ideology+attend5+ba+ed+age+income+female, data=pprw, weight=wgt)  
lmcnb <- lm(cnd ~ pid7+ideology+attend5+ba+ed+age+income+female, data=pprb, weight=wgt)  
lmcnh <- lm(cnd ~ pid7+ideology+attend5+ba+ed+age+income+female, data=pprh, weight=wgt)  

plot_coefs(lmcnw, lmcnb, lmcnh, model.names = c("White", "Black", "Latino"),
           coefs = c("Partisanship"="pid7",
                     "Ideology"="ideology",
                     "Worship attendance"="attend5",
                     "Evangelical ID"="ba",
                     "Education"="ed",
                     "Age"="age",
                     "Income"="income",
                     "Female"="female")) +
  theme_minimal() +
  geom_vline(xintercept=0, color="red", linetype="dashed") +
  theme(text=element_text(family="G", size=12)) +
  labs(x="Estimated Effect on Christian Nationalism", y="")
ggsave(file=paste0(cnrdir, "FigA8 - davis_cn.png"), type="cairo-png", height=4, width=6)

export_summs(lmcnw, lmcnb, lmcnh, model.names = c("White", "Black", "Latino"), 
              error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(cnrdir, "TableA8_daviscn.docx"),
             coefs = c("Partisanship"="pid7",
                       "Ideology"="ideology",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Age"="age",
                       "Income"="income",
                       "Female"="female"))


#Davis Racialized Issues
lmr1 <- lm(q49_5r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weigh=wgt)
ipr1 <- interact_plot(lmr1, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Teaching about race in American history in public schools") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lmr2 <- lm(q49_6r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weigh=wgt)
ipr2 <- interact_plot(lmr2, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Offering reparations to descendants of slaves") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lmr3 <- lm(q49_7r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weigh=wgt)
ipr3 <- interact_plot(lmr3, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Reforming procedures used by police departments") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lmr4 <- lm(q50_2r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weigh=wgt)
ipr4 <- interact_plot(lmr4, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="US should make it easier to vote with election-day voter registration") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

ipr1 + ipr2 + ipr3 + ipr4 + plot_layout(ncol=2, guides="collect") & theme(legend.position = "bottom")
ggsave(file=paste0(cnrdir, "FigA9 - racialized_issues.png"), type="cairo-png", height=8, width=10)

export_summs(lmr1, lmr2, lmr3, lmr4, 
             model.names = c("Teaching", "Reparations", 
                             "Police\nReform", "Voter\nRegistration"), 
             error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(cnrdir, "TableA9_davisrace.docx"),
             coefs = c("Partisanship"="pid7",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Income"="income",
                       "Age"="age",
                       "Women"="genderWomen",
                       "Non-binary+"="genderNon-binary+",
                       "Latino/a"="raceLatino/a",
                       "White"="raceWhite",
                       "Christian nationalism"="cnd",
                       "Latino/a*CN"="raceLatino/a:cnd",
                       "White*CN"="raceWhite:cnd"))


rm(ipr1, ipr2, ipr3, ipr4)
rm(lmr1, lmr2, lmr3, lmr4)

#Davis Economic Issues
lme1 <- lm(q32 ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weight=wgt)
ipe1 <- interact_plot(lme1, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Government should let each person get ahead on their own") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))


lme2 <- lm(govrole ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weight=wgt)
ipe2 <- interact_plot(lme2, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Government's Responsibility to Care for Elderly, Sick, Unemployed") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lme3 <- lm(q50_4r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr, weight=wgt)
ipe3 <- interact_plot(lme3, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="The federal minimum wage should be increased to $15/hr") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

ipe1 + ipe2 + ipe3 + plot_layout(ncol=2, guides="collect") & theme(legend.position = "bottom")
ggsave(file=paste0(cnrdir, "FigA10 - economic.png"), type="cairo-png", height=8, width=10)

export_summs(lme1, lme2, lme3, 
             model.names = c("On their\nown", "Care for\nvulnerable", 
                             "Minimum wage"), 
             error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(cnrdir, "TableA10_davisecon.docx"),
             coefs = c("Partisanship"="pid7",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Income"="income",
                       "Age"="age",
                       "Women"="genderWomen",
                       "Non-binary+"="genderNon-binary+",
                       "Latino/a"="raceLatino/a",
                       "White"="raceWhite",
                       "Christian nationalism"="cnd",
                       "Latino/a*CN"="raceLatino/a:cnd",
                       "White*CN"="raceWhite:cnd"))

rm(ipe1, ipe2, ipe3, lme1, lme2, lme3)


#Davis Social Issues
lms1 <- lm(q49_2r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips1 <- interact_plot(lms1, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Making abortion illegal in the entire nation\n(except to save the life of the mother)") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lms2 <- lm(q50_5r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips2 <- interact_plot(lms2, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="I would favor a law that would protect LGBT people against\n discrimination in jobs, public accommodations, and housing") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lms3 <- lm(q49_3r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips3 <- interact_plot(lms3, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Allowing a small business owner in your state to refuse\nto provide products or services to gay or lesbian people\nif doing so violates their religious beliefs.") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lms4 <- lm(q50_1r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips4 <- interact_plot(lms4, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Banning same-sex marriage in my state") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lms5 <- lm(q50_3r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips5 <- interact_plot(lms5, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="Burning the American flag is wrong because it is sacred") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

lms6 <- lm(q50_6r ~ race*cnd+pid7+ideology+attend5+ba+ed+age+income+gender, data=ppr)
ips6 <- interact_plot(lms6, pred=cnd, modx=race, interval=T, int.width = .84,
                      legend.main = "") +
  labs(x="Christian Nationalism", y="Agreement->",
       subtitle="There should be background checks for all gun sales,\nincluding at gun shows and over the internet") +
  theme_minimal() +
  theme(text=element_text(family="G", size=12),
        legend.position = "bottom",
        strip.text=element_text(size=14))

ips1 + ips2 + ips3 + ips4 + ips5 + ips6 + plot_layout(ncol=2, guides="collect") & theme(legend.position="bottom")
ggsave(file=paste0(cnrdir, "FigA11 - social_issues.png"), type="cairo-png", height=12, width=10)


export_summs(lms1, lms2, lms3, lms4, lms5, lms6, 
             model.names = c("Abortion", "Anti-\ndiscrimination", "Small biz\nexemption",
                             "SSM Ban", "Flag\nburning", "Background\nchecks"), 
             error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(cnrdir, "TableA11_davissocial.docx"),
             coefs = c("Partisanship"="pid7",
                       "Worship attendance"="attend5",
                       "Evangelical ID"="ba",
                       "Education"="ed",
                       "Income"="income",
                       "Age"="age",
                       "Women"="genderWomen",
                       "Non-binary+"="genderNon-binary+",
                       "Latino/a"="raceLatino/a",
                       "White"="raceWhite",
                       "Christian nationalism"="cnd",
                       "Latino/a*CN"="raceLatino/a:cnd",
                       "White*CN"="raceWhite:cnd"))

rm(ips1, ips2, ips3, ips4, ips5, ips6)
rm(lms1, lms2, lms3, lms4, lms5, lms6)


#General Removes####
rm(list=ls(pattern="ips"))
rm(list=ls(pattern="lm"))
rm(list=ls(pattern="ip"))
rm(list=ls(pattern="ppr"))
rm(O20, cnrdir)