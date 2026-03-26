####Sivaram Cheruvu
####University of Texas at Dallas
####Sivaram.Cheruvu@UTDallas.edu
####Replication code for "Are judges on \textit{per curiam} courts ideological? Evidence from the European Court of Justice"

library(tidyverse)
library(broom)
library(stargazer)
library(tidymodels)
library(fixest)
library(marginaleffects)
library(ggExtra)

####Loading Data####
dat <- read.csv("JLC_Final.csv")

####TABLE 1: Descriptive Stats Table####
dat_stats <- dat %>% filter(year < 1995) %>% 
  select("JR Net Observations" = JR_Net_Observations, 
         "Net Observations" = netobs,
         "Turnover" = turnover,
         "Change in EU Ideology" = diff_eu,
         "Chamber Size" =chamber_size, 
         "Commission Observation Plaintiff" = CommObsPl,
         "Commission Observation Defendant" =CommObsDef,
         "Commission is Plaintiff" = CommIsPl,
         "Commission is Defendant" = CommIsDef,
         "AG for Plaintiff" = AGforPl,
         "Government is litigant" = govislit,
         "Infringement Case" = infringement,
         "Annulment Case" = annulment,
         "Failure to Act Case" = failure_to_act,
         "Preliminary Reference Case" = preliminary_reference,
         "Staff Case" = staff)
stargazer(as.data.frame(dat_stats), nobs = F, iqr = F)

####TABLE 2: Hypothesis 1 Main Regression Table####

h1_1 <- feols(CJEU_Agree_Pl~ JR_Net_Observations, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

h1_2 <- feols(CJEU_Agree_Pl~ JR_Net_Observations| judge_rapporteur, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

h1_3 <- feols(CJEU_Agree_Pl~ JR_Net_Observations + AGforPl, 
               cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

h1_4 <- feols(CJEU_Agree_Pl~ JR_Net_Observations + CommObsPl + CommObsDef + CommIsPl + CommIsDef, 
               cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

h1_5 <- feols(CJEU_Agree_Pl~ JR_Net_Observations + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff,
               cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

h1_6 <- feols(CJEU_Agree_Pl~ JR_Net_Observations + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff
               |judge_rapporteur, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

dict = c("CJEU_Agree_Pl" = "CJEU Agrees with Plaintiff", "turnover" = "Turnover",
         "JR_Net_Observations"= "JR Net Observations", "chamber_size" = "Chamber Size",
         "CommObsPl"="Commission Observation Plaintiff", "CommObsDef"="Commission Observation Defendant",
         "CommIsPl" = "Commission is Plaintiff", "CommIsDef" = "Commission is Defendant", "AGforPl" = "AG for Plaintiff",
         "govislit" = "Government is Litigant", "netobs" = "Net Observations", "infringement" = "Infringement Case",
         "annulment" = "Annulment Case", "failure_to_act" = "Failure to Act Case", "preliminary_reference" = "Preliminary Reference Case",
         "staff" = "Staff Case", "year" = "Year", "judge_rapporteur" = "JR")
etable(h1_1,h1_2,h1_3,h1_4,h1_5,h1_6, dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~n,
       tex = T, digits = 3
)

####TABLE 3: Hypothesis 2 Main Regression Table####

h2_1 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*turnover, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

h2_2 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*turnover| judge_rapporteur, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

h2_3 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*turnover + AGforPl, 
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

h2_4 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*turnover + CommObsPl + CommObsDef + CommIsPl + CommIsDef ,
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

h2_5 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*turnover + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff,
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

h2_6 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*turnover + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff
               |judge_rapporteur, cluster =~judge_rapporteur, data = filter(dat, year < 1995))

dict = c("CJEU_Agree_Pl" = "CJEU Agrees with Plaintiff", "turnover" = "Turnover",
         "JR_Net_Observations"= "JR Net Observations", "chamber_size" = "Chamber Size",
         "CommObsPl"="Commission Observation Plaintiff", "CommObsDef"="Commission Observation Defendant",
         "CommIsPl" = "Commission is Plaintiff", "CommIsDef" = "Commission is Defendant", "AGforPl" = "AG for Plaintiff",
         "govislit" = "Government is Litigant", "netobs" = "Net Observations", "infringement" = "Infringement Case",
         "annulment" = "Annulment Case", "failure_to_act" = "Failure to Act Case", "preliminary_reference" = "Preliminary Reference Case",
         "staff" = "Staff Case", "year" = "Year", "judge_rapporteur" = "JR")
etable(h2_1,h2_2,h2_3,h2_4, h2_5, h2_6,
       dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~  n,
       tex = T,
       digits = 3
)

####Figure 1####

p <- predictions(h2_1, newdata = datagrid(JR_Net_Observations =c(-1,0,1), turnover = c(0,1))) %>%
  mutate(model = "Interaction (Model 1)")
p2 <- predictions(h2_5, newdata = datagrid(JR_Net_Observations =c(-1,0,1), turnover = c(0,1))) %>%
  mutate(model = "Interaction + Controls (Model 5)") %>% select(rowid:CJEU_Agree_Pl, JR_Net_Observations, turnover, model)
p <- rbind(p,p2)
p$turnover <- ifelse(p$turnover == 1, "Turnover", "No Turnover")
p$JR_Net_Observations <- factor(p$JR_Net_Observations)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind palette
p <- p %>% rename("JR Net Observations" = JR_Net_Observations)
ggplot(p, aes(x=factor(turnover), y=estimate, color = `JR Net Observations`)) +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), 
                  position = position_dodge2(width = 0.5), size =0.5) +
  theme_bw() + scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values=cbbPalette) +
  facet_wrap(~model) +
  theme(legend.position="bottom",axis.title.x = element_blank(),
        strip.text = element_text(size=13), text = element_text(size=13),
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=15),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(y="Probability of CJEU ruling for Plaintiff")  # Labels


####TABLE A1: Hypothesis 1 Logit####
h1_logit_1 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations, cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h1_logit_2 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations| judge_rapporteur, cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h1_logit_3 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations + AGforPl, 
               cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h1_logit_4 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations + CommObsPl + CommObsDef + CommIsPl + CommIsDef, 
               cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h1_logit_5 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff,
               cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h1_logit_6 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff
               |judge_rapporteur, cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

dict = c("CJEU_Agree_Pl" = "CJEU Agrees with Plaintiff", "turnover" = "Turnover",
         "JR_Net_Observations"= "JR Net Observations", "chamber_size" = "Chamber Size",
         "CommObsPl"="Commission Observation Plaintiff", "CommObsDef"="Commission Observation Defendant",
         "CommIsPl" = "Commission is Plaintiff", "CommIsDef" = "Commission is Defendant", "AGforPl" = "AG for Plaintiff",
         "govislit" = "Government is Litigant", "netobs" = "Net Observations", "infringement" = "Infringement Case",
         "annulment" = "Annulment Case", "failure_to_act" = "Failure to Act Case", "preliminary_reference" = "Preliminary Reference Case",
         "staff" = "Staff Case", "year" = "Year", "judge_rapporteur" = "JR")
etable(h1_logit_1,h1_logit_2,h1_logit_3,h1_logit_4,h1_logit_5,h1_logit_6, dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~n,
       tex = T, digits = 3
)

####TABLE A2: Hypothesis 2 Logit####
h2_logit_1 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations*turnover, cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h2_logit_2 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations*turnover| judge_rapporteur, cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h2_logit_3 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations*turnover + AGforPl, 
               cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h2_logit_4 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations*turnover + CommObsPl + CommObsDef + CommIsPl + CommIsDef, 
               cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h2_logit_5 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations*turnover + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff,
               cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

h2_logit_6 <- feglm(CJEU_Agree_Pl~ JR_Net_Observations*turnover + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff
               |judge_rapporteur, cluster = ~judge_rapporteur, family = "binomial",
               data = filter(dat, year < 1995))

dict = c("CJEU_Agree_Pl" = "CJEU Agrees with Plaintiff", "turnover" = "Turnover",
         "JR_Net_Observations"= "JR Net Observations", "chamber_size" = "Chamber Size",
         "CommObsPl"="Commission Observation Plaintiff", "CommObsDef"="Commission Observation Defendant",
         "CommIsPl" = "Commission is Plaintiff", "CommIsDef" = "Commission is Defendant", "AGforPl" = "AG for Plaintiff",
         "govislit" = "Government is Litigant", "netobs" = "Net Observations", "infringement" = "Infringement Case",
         "annulment" = "Annulment Case", "failure_to_act" = "Failure to Act Case", "preliminary_reference" = "Preliminary Reference Case",
         "staff" = "Staff Case", "year" = "Year", "judge_rapporteur" = "JR")
etable(h2_logit_1,h2_logit_2,h2_logit_3,h2_logit_4,h2_logit_5,h2_logit_6, dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~n,
       tex = T, digits = 3
)

####TABLE A3 PM Turnover####

pm_1 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*pm_turnover, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

pm_2 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*pm_turnover| judge_rapporteur, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

pm_3 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*pm_turnover + AGforPl, 
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

pm_4 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*pm_turnover + CommObsPl + CommObsDef + CommIsPl + CommIsDef ,
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

pm_5 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*pm_turnover + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff,
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

pm_6 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*pm_turnover + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff
               |judge_rapporteur, cluster =~judge_rapporteur, data = filter(dat, year < 1995))

dict = c("CJEU_Agree_Pl" = "CJEU Agrees with Plaintiff", "pm_turnover" = "PM Turnover",
         "JR_Net_Observations"= "JR Net Observations", "chamber_size" = "Chamber Size",
         "CommObsPl"="Commission Observation Plaintiff", "CommObsDef"="Commission Observation Defendant",
         "CommIsPl" = "Commission is Plaintiff", "CommIsDef" = "Commission is Defendant", "AGforPl" = "AG for Plaintiff",
         "govislit" = "Government is Litigant", "netobs" = "Net Observations", "infringement" = "Infringement Case",
         "annulment" = "Annulment Case", "failure_to_act" = "Failure to Act Case", "preliminary_reference" = "Preliminary Reference Case",
         "staff" = "Staff Case", "year" = "Year", "judge_rapporteur" = "JR")
etable(pm_1,pm_2,pm_3,pm_4, pm_5, pm_6,
       dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~  n,
       tex = T,
       digits = 3
)

####TABLE A4: Change in Left-Right Ideology####

left_1 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_left_right, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

left_2 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_left_right| judge_rapporteur, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

left_3 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_left_right + AGforPl, 
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

left_4 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_left_right + CommObsPl + CommObsDef + CommIsPl + CommIsDef ,
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

left_5 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_left_right + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff,
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

left_6 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_left_right + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff
               |judge_rapporteur, cluster =~judge_rapporteur, data = filter(dat, year < 1995))

dict = c("CJEU_Agree_Pl" = "CJEU Agrees with Plaintiff", "diff_left_right" = "Difference Ideology",
         "JR_Net_Observations"= "JR Net Observations", "chamber_size" = "Chamber Size",
         "CommObsPl"="Commission Observation Plaintiff", "CommObsDef"="Commission Observation Defendant",
         "CommIsPl" = "Commission is Plaintiff", "CommIsDef" = "Commission is Defendant", "AGforPl" = "AG for Plaintiff",
         "govislit" = "Government is Litigant", "netobs" = "Net Observations", "infringement" = "Infringement Case",
         "annulment" = "Annulment Case", "failure_to_act" = "Failure to Act Case", "preliminary_reference" = "Preliminary Reference Case",
         "staff" = "Staff Case", "year" = "Year", "judge_rapporteur" = "JR")
etable(left_1,left_2,left_3,left_4, left_5, left_6,
       dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~  n,
       tex = T,
       digits = 3
)

####TABLE A5: Change in EU Ideology####

eu_1 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_eu, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

eu_2 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_eu| judge_rapporteur, cluster = ~judge_rapporteur, data = filter(dat, year < 1995))

eu_3 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_eu + AGforPl, 
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

eu_4 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_eu + CommObsPl + CommObsDef + CommIsPl + CommIsDef ,
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

eu_5 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_eu + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff,
               cluster =~judge_rapporteur, data = filter(dat, year < 1995))

eu_6 <- feols(CJEU_Agree_Pl~ JR_Net_Observations*diff_eu + netobs + chamber_size + CommObsPl + CommObsDef + CommIsPl + CommIsDef + AGforPl + 
                 govislit + infringement + annulment + failure_to_act + preliminary_reference + staff
               |judge_rapporteur, cluster =~judge_rapporteur, data = filter(dat, year < 1995))

dict = c("CJEU_Agree_Pl" = "CJEU Agrees with Plaintiff", "diff_eu" = "Difference EU Ideology",
         "JR_Net_Observations"= "JR Net Observations", "chamber_size" = "Chamber Size",
         "CommObsPl"="Commission Observation Plaintiff", "CommObsDef"="Commission Observation Defendant",
         "CommIsPl" = "Commission is Plaintiff", "CommIsDef" = "Commission is Defendant", "AGforPl" = "AG for Plaintiff",
         "govislit" = "Government is Litigant", "netobs" = "Net Observations", "infringement" = "Infringement Case",
         "annulment" = "Annulment Case", "failure_to_act" = "Failure to Act Case", "preliminary_reference" = "Preliminary Reference Case",
         "staff" = "Staff Case", "year" = "Year", "judge_rapporteur" = "JR")
etable(eu_1,eu_2,eu_3,eu_4, eu_5, eu_6,
       dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~  n,
       tex = T,
       digits = 3
)

####Figure 2####

p <- predictions(eu_5, newdata = datagrid(JR_Net_Observations =c(-1,0,1), diff_eu = seq(0,5,1)))
p$JR_Net_Observations <- factor(p$JR_Net_Observations)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind palette
p <- p %>% rename("JR Net Observations" = JR_Net_Observations)
ggplot(p, aes(x=diff_eu, y=estimate, color = `JR Net Observations`)) +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high), 
                  position = position_dodge2(width = 0.5), size =0.5) +
  theme_classic() + scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0,5,1)) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position="bottom",
        strip.text = element_text(size=13), text = element_text(size=13),
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=15),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black")) +
  xlab("Change in EU Ideology") + ylab("Probability of CJEU ruling for Plaintiff") +
  geom_rug(data = filter(dat, year < 1995), aes(x = diff_eu), inherit.aes = F)