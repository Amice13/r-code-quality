# Can the Fairer Sex Save the Day? Voting for Women After Corruption Scandals in Latin America
# Replication Code, Main Analysis & SI Figures
# Emily Elia 

###################################################
# This replication code includes all main analyses 
# in the manuscript and most figures in the SI. Additional
# SI content (histograms, model outputs) can be found
# in the Stata Code SI .do file.

### DATA SET UP

# import cleaned and fully anonymized data
install.packages('haven')
library(haven)
dat <- read_dta("ScandalsExpData_ANONYMOUS.dta")

# subset by people who completed the survey 
dat1 <- dat[dat$incomplete==0,]

# make treatment condition factor
dat1$treat <- as.factor(dat1$condition)
# make respondent sex factor
dat1$femf <- as.factor(dat1$female)
# make binary stereotype score
dat1$sscorebi <- ifelse(dat1$stereotypescore < 12 , 0, 1)
summary(dat1$sscorebi)
dat1$sscorebi <- as.factor(dat1$sscorebi)

# subset data by countries
dat1mx <- dat1[dat1$ccode==1,]
dat1gt <- dat1[dat1$ccode==2,]
dat1ch <- dat1[dat1$ccode==3,]
dat1uy <- dat1[dat1$ccode==4,]

###################################################

### BALANCE TABLE, POOLED SAMPLE
install.packages("cobalt")
library("cobalt")

covs <- subset(dat1, select = c(agenum, edunum, female))

bal.tab(covs,dat1$treat)

###################################################

### PLOT THEME

theme_eqll <- function() { 
  
  theme_bw() + theme(strip.text.x = element_text(size = 14),
                     strip.text.y = element_text(size = 14),
                     axis.text.x = element_text(size = 12),
                     axis.text.y = element_text(size = 12),
                     axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0)),
                     axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.major = element_blank(),
                     plot.caption = element_text(size = 12),
                     legend.position = "right", legend.text=element_text(size=12),
                     legend.title=element_text(size=12),
                     panel.spacing = unit(1, "lines")) 
}

library(tidyverse) 

install.packages("ggpubr")
library(ggpubr)

###################################################

### MANUSCRIPT FIGURES

### FIG 1: Mean likelihood of vote for candidate by treatment/country

df_cntry <- dat1 %>%
  group_by(treat, Country) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_cntry <- ggplot(df_cntry, aes(x=treat, y =mean, fill = Country)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape =  treat, color = Country), size = 3) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote") +
  guides(color = FALSE, fill = FALSE) +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() +
  facet_wrap(~ Country)

gg.vote_cntry
#ggsave("scandals_votebycntry2.pdf")

###################################################

### FIG 2: Mean likelihood of vote for candidate by treatment/country/stereotype score

## MEX

# create dichotomous score high v low
dat1mx$sscorebi <- ifelse(dat1mx$stereotypescore < 12 , 0, 1)
summary(dat1mx$sscorebi)

dat1mx$sscorebi <- as.factor(dat1mx$sscorebi)

df_st_mex <- dat1mx %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot in black and white 
gg.vote_st_mexGREY <- ggplot(df_st_mex, aes(x=treat, y=mean, color=sscorebi)) +  # Remove fill mapping here
  geom_errorbar(width=.2, aes(ymin=mean - ci, ymax=mean + ci), position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), aes(shape=treat), size=4) +  # Removed fill from aes here
  labs(x=NULL, y="Likelihood of Vote", title="Mexico") +
  scale_shape_manual(values=c(15, 16, 17, 8), name="Condition", labels=c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_manual(values=c("black", "darkgrey"), name="High Stereotype Score", labels=c("No", "Yes")) +  # Use scale_color_manual for custom colors
  scale_y_continuous(breaks=scales::pretty_breaks(n=5)) +
  scale_x_discrete(labels=c("M","W", "M-S", "W-S")) +
  theme_bw() +
  guides(fill=FALSE)  # Only remove the fill legend

gg.vote_st_mexGREY


## GTM

# create dichotomous score high v low
dat1gt$sscorebi <- ifelse(dat1gt$stereotypescore < 12 , 0, 1)
summary(dat1gt$sscorebi)

dat1gt$sscorebi <- as.factor(dat1gt$sscorebi)

df_st_gtm <- dat1gt %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st_gtm <- df_st_gtm[-c(5, 8),]

# plot in black and white 
gg.vote_st_gtmGREY <- ggplot(df_st_gtm, aes(x=treat, y=mean, color=sscorebi)) +  # Remove fill mapping here
  geom_errorbar(width=.2, aes(ymin=mean - ci, ymax=mean + ci), position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), aes(shape=treat), size=4) +  # Removed fill from aes here
  labs(x=NULL, y="Likelihood of Vote", title="Guatemala") +
  scale_shape_manual(values=c(15, 16, 17, 8), name="Condition", labels=c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_manual(values=c("black", "darkgrey"), name="High Stereotype Score", labels=c("No", "Yes")) +  # Use scale_color_manual for custom colors
  scale_y_continuous(breaks=scales::pretty_breaks(n=5)) +
  scale_x_discrete(labels=c("M","W", "M-S", "W-S")) +
  theme_bw() +
  guides(fill=FALSE)  # Only remove the fill legend

gg.vote_st_gtmGREY


## CHL

# create dichotomous score high v low
dat1ch$sscorebi <- ifelse(dat1ch$stereotypescore < 12 , 0, 1)
summary(dat1ch$sscorebi)

dat1ch$sscorebi <- as.factor(dat1ch$sscorebi)

df_st_chl <- dat1ch %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st_chl <- df_st_chl[-c(3, 6, 9, 12),]

# plot in black and white 
gg.vote_st_chlGREY <- ggplot(df_st_chl, aes(x=treat, y=mean, color=sscorebi)) +  # Remove fill mapping here
  geom_errorbar(width=.2, aes(ymin=mean - ci, ymax=mean + ci), position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), aes(shape=treat), size=4) +  # Removed fill from aes here
  labs(x=NULL, y="Likelihood of Vote", title="Chile") +
  scale_shape_manual(values=c(15, 16, 17, 8), name="Condition", labels=c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_manual(values=c("black", "darkgrey"), name="High Stereotype Score", labels=c("No", "Yes")) +  # Use scale_color_manual for custom colors
  scale_y_continuous(breaks=scales::pretty_breaks(n=5)) +
  scale_x_discrete(labels=c("M","W", "M-S", "W-S")) +
  theme_bw() +
  guides(fill=FALSE)  # Only remove the fill legend

gg.vote_st_chlGREY


## URY

# create dichotomous score high v low
dat1uy$sscorebi <- ifelse(dat1uy$stereotypescore < 12 , 0, 1)
summary(dat1uy$sscorebi)

dat1uy$sscorebi <- as.factor(dat1uy$sscorebi)

df_st_ury <- dat1uy %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot in black and white 
gg.vote_st_uryGREY <- ggplot(df_st_ury, aes(x=treat, y=mean, color=sscorebi)) +  # Remove fill mapping here
  geom_errorbar(width=.2, aes(ymin=mean - ci, ymax=mean + ci), position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), aes(shape=treat), size=4) +  # Removed fill from aes here
  labs(x=NULL, y="Likelihood of Vote", title="Uruguay") +
  scale_shape_manual(values=c(15, 16, 17, 8), name="Condition", labels=c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_manual(values=c("black", "darkgrey"), name="High Stereotype Score", labels=c("No", "Yes")) +  # Use scale_color_manual for custom colors
  scale_y_continuous(breaks=scales::pretty_breaks(n=5)) +
  scale_x_discrete(labels=c("M","W", "M-S", "W-S")) +
  theme_light() +
  guides(fill=FALSE)  # Only remove the fill legend

gg.vote_st_uryGREY


## Combine plots 
library(ggpubr)
votebyscore.combinedGREY <- ggarrange(gg.vote_st_mexGREY, NULL, gg.vote_st_gtmGREY, gg.vote_st_chlGREY, NULL, gg.vote_st_uryGREY,
                                      nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                      common.legend = TRUE, legend = "right",
                                      font.label = list(size=4, face="plain"))
votebyscore.combinedGREY

#ggsave("votebyscore.combinedGREY.pdf")

###################################################

### FIG 3: Mean likelihood of vote for candidate by treatment/country/respondent sex

## MEX
df_fem_mex <- dat1mx %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem_mex <- df_fem_mex[-c(3, 6, 9, 12),]

# plot in black and white 
gg.vote_fem_mexGREY <- ggplot(df_fem_mex, aes(x=treat, y=mean, color=femf)) +  # Remove fill mapping here
  geom_errorbar(width=.2, aes(ymin=mean - ci, ymax=mean + ci), position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), aes(shape=treat), size=4) +  # Removed fill from aes here
  labs(x=NULL, y="Likelihood of Vote", title="Mexico") +
  scale_shape_manual(values=c(15, 16, 17, 8), name="Condition", labels=c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_manual(values=c("black", "darkgrey"), name="Respondent Sex", labels=c("Male", "Female")) +  # Use scale_color_manual for custom colors
  scale_y_continuous(breaks=scales::pretty_breaks(n=5)) +
  scale_x_discrete(labels=c("M","W", "M-S", "W-S")) +
  theme_bw() +
  guides(fill=FALSE)  # Only remove the fill legend

gg.vote_fem_mexGREY


## GTM
df_fem_gtm <- dat1gt %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem_gtm <- df_fem_gtm[-c(3, 6, 9, 12),]

# plot in black and white 
gg.vote_fem_gtmGREY <- ggplot(df_fem_gtm, aes(x=treat, y=mean, color=femf)) +  # Remove fill mapping here
  geom_errorbar(width=.2, aes(ymin=mean - ci, ymax=mean + ci), position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), aes(shape=treat), size=4) +  # Removed fill from aes here
  labs(x=NULL, y="Likelihood of Vote", title="Guatemala") +
  scale_shape_manual(values=c(15, 16, 17, 8), name="Condition", labels=c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_manual(values=c("black", "darkgrey"), name="Respondent Sex", labels=c("Male", "Female")) +  # Use scale_color_manual for custom colors
  scale_y_continuous(breaks=scales::pretty_breaks(n=5)) +
  scale_x_discrete(labels=c("M","W", "M-S", "W-S")) +
  theme_bw() +
  guides(fill=FALSE)  # Only remove the fill legend

gg.vote_fem_gtmGREY

## CHL
df_fem_chl <- dat1ch %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem_chl <- df_fem_chl[-c(3, 6, 9),]

# plot in black and white 
gg.vote_fem_chlGREY <- ggplot(df_fem_chl, aes(x=treat, y=mean, color=femf)) +  # Remove fill mapping here
  geom_errorbar(width=.2, aes(ymin=mean - ci, ymax=mean + ci), position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), aes(shape=treat), size=4) +  # Removed fill from aes here
  labs(x=NULL, y="Likelihood of Vote", title="Chile") +
  scale_shape_manual(values=c(15, 16, 17, 8), name="Condition", labels=c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_manual(values=c("black", "darkgrey"), name="Respondent Sex", labels=c("Male", "Female")) +  # Use scale_color_manual for custom colors
  scale_y_continuous(breaks=scales::pretty_breaks(n=5)) +
  scale_x_discrete(labels=c("M","W", "M-S", "W-S")) +
  theme_bw() +
  guides(fill=FALSE)  # Only remove the fill legend

gg.vote_fem_chlGREY

## URY
df_fem_ury <- dat1uy %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem_ury <- df_fem_ury[-c(7, 10),]

# plot in black and white 
gg.vote_fem_uryGREY <- ggplot(df_fem_ury, aes(x=treat, y=mean, color=femf)) +  # Remove fill mapping here
  geom_errorbar(width=.2, aes(ymin=mean - ci, ymax=mean + ci), position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), aes(shape=treat), size=4) +  # Removed fill from aes here
  labs(x=NULL, y="Likelihood of Vote", title="Uruguay") +
  scale_shape_manual(values=c(15, 16, 17, 8), name="Condition", labels=c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_manual(values=c("black", "darkgrey"), name="Respondent Sex", labels=c("Male", "Female")) +  # Use scale_color_manual for custom colors
  scale_y_continuous(breaks=scales::pretty_breaks(n=5)) +
  scale_x_discrete(labels=c("M","W", "M-S", "W-S")) +
  theme_bw() +
  guides(fill=FALSE)  # Only remove the fill legend

gg.vote_fem_uryGREY

## Combine plots
library(ggpubr)
votebysex.combinedGREY <- ggarrange(gg.vote_fem_mexGREY, NULL, gg.vote_fem_gtmGREY, gg.vote_fem_chlGREY, NULL, gg.vote_fem_uryGREY,
                                    nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                    common.legend = TRUE, legend = "right",
                                    font.label = list(size=4, face="plain"))
votebysex.combinedGREY

#ggsave("votebysex.combinedGREY.pdf")

###################################################
###################################################
###################################################

### ONLINE SUPPLEMENTARY INFORMATION FIGURES

### TABLE SI.2: Balance table by country
covsm <- subset(dat1mx, select = c(agenum, edunum, female))
covsg <- subset(dat1gt, select = c(agenum, edunum, female))
covsc <- subset(dat1ch, select = c(agenum, edunum, female))
covsu <- subset(dat1uy, select = c(agenum, edunum, female))

bal.tab(covsm,dat1mx$treat)
bal.tab(covsg,dat1gt$treat)
bal.tab(covsc,dat1ch$treat)
bal.tab(covsu,dat1uy$treat)

### FIG SI.5: Mean trust in candidate by treatment/country
df_cntryt <- dat1 %>%
  group_by(treat, Country) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.trust_cntry <- ggplot(df_cntryt, aes(x=treat, y =mean, fill = Country)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape =  treat, color = Country), size = 3) +
  labs(x = "Treatment Condition", y = "Trust in Candidate") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() +
  facet_wrap(~ Country)

gg.trust_cntry

### FIG SI.6: Mean trust in candidate by treatment/country/respondent sex
## MEX
df_fem_mext <- dat1mx %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem_mext <- df_fem_mext[-c(3, 6, 9, 12),]

# plot means with CIs
gg.trust_fem_mexNOLAB <- ggplot(df_fem_mext, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = NULL, y = "Likelihood of trust", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()
gg.trust_fem_mexNOLAB

## GTM
df_fem_gtmt <- dat1gt %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem_gtmt <- df_fem_gtmt[-c(3, 6, 9, 12),]

# plot means with CIs
gg.trust_fem_gtmNOLAB <- ggplot(df_fem_gtmt, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = NULL, y = "Likelihood of trust", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()
gg.trust_fem_gtmNOLAB

## CHL
df_fem_chlt <- dat1ch %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem_chlt <- df_fem_chlt[-c(3, 6, 9),]

# plot means with CIs
gg.trust_fem_chlNOLAB <- ggplot(df_fem_chlt, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = NULL, y = "Likelihood of trust", title = "Chile") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()
gg.trust_fem_chlNOLAB

## URY
df_fem_uryt <- dat1uy %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem_uryt <- df_fem_uryt[-c(7, 10),]

# plot means with CIs
gg.trust_fem_uryNOLAB <- ggplot(df_fem_uryt, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = NULL, y = "Likelihood of trust", title = "Uruguay") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()
gg.trust_fem_uryNOLAB

## Combining Plots
library(ggpubr)

trustbysex.combined2 <- ggarrange(gg.trust_fem_mexNOLAB, NULL, gg.trust_fem_gtmNOLAB, gg.trust_fem_chlNOLAB, NULL, gg.trust_fem_uryNOLAB,
                                  nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                  common.legend = TRUE, legend = "right",
                                  font.label = list(size=4, face="plain"))
trustbysex.combined2
#ggsave("trustbysex.combined.pdf")

### FIG SI.7: Mean trust in candidate by treatment/country/stereotype score
## MEX
# create dichotomous score high v low
dat1mx$sscorebi <- ifelse(dat1mx$stereotypescore < 12 , 0, 1)
summary(dat1mx$sscorebi)

dat1mx$sscorebi <- as.factor(dat1mx$sscorebi)

df_st_mext <- dat1mx %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.trust_st_mexNOLAB <- ggplot(df_st_mext, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "", y = "Likelihood of trust", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## GTM
# create dichotomous score high v low
dat1gt$sscorebi <- ifelse(dat1gt$stereotypescore < 12 , 0, 1)
summary(dat1gt$sscorebi)

dat1gt$sscorebi <- as.factor(dat1gt$sscorebi)

df_st_gtmt <- dat1gt %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st_gtmt <- df_st_gtmt[-c(5, 8),]

# plot means with CIs
gg.trust_st_gtmNOLAB <- ggplot(df_st_gtmt, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "", y = "Likelihood of trust", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## CHL
# create dichotomous score high v low
dat1ch$sscorebi <- ifelse(dat1ch$stereotypescore < 12 , 0, 1)
summary(dat1ch$sscorebi)

dat1ch$sscorebi <- as.factor(dat1ch$sscorebi)

df_st_chlt <- dat1ch %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st_chlt <- df_st_chlt[-c(3, 6, 9, 12),]

# plot means with CIs
gg.trust_st_chlNOLAB <- ggplot(df_st_chlt, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "", y = "Likelihood of trust", title = "Chile") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## URY
# create dichotomous score high v low
dat1uy$sscorebi <- ifelse(dat1uy$stereotypescore < 12 , 0, 1)
summary(dat1uy$sscorebi)

dat1uy$sscorebi <- as.factor(dat1uy$sscorebi)

df_st_uryt <- dat1uy %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(trust, na.rm = TRUE), sd=sd(trust, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)


# plot means with CIs
gg.trust_st_uryNOLAB <- ggplot(df_st_uryt, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "", y = "Likelihood of trust", title = "Uruguay") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

# Combine plots
trustbyscore.combined <- ggarrange(gg.trust_st_mexNOLAB, NULL, gg.trust_st_gtmNOLAB, gg.trust_st_chlNOLAB, NULL, gg.trust_st_uryNOLAB,
                                   nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                   common.legend = TRUE, legend = "right",
                                   font.label = list(size=4, face="plain"))
trustbyscore.combined
#ggsave("trustbyscore.combined.pdf")

### FIG SI.8: Tukey HSD test for Mexico
## POOLED
anova <- aov(vote ~ treat, data=dat1)
summary(anova)

TukeyHSD(anova, conf.level=.95)
plot(TukeyHSD(anova, conf.level=.95), las=2)

#subset by countries
dat1mx <- dat1[dat1$ccode==1,]
dat1gt <- dat1[dat1$ccode==2,]
dat1ch <- dat1[dat1$ccode==3,]
dat1uy <- dat1[dat1$ccode==4,]

## MEX
avmex <- aov(vote ~ treat, data=dat1mx)
summary(avmex)

TukeyHSD(avmex, conf.level=.95)
plot(TukeyHSD(avmex, conf.level=.95), las=2) 
title(sub="Mexico")

### FIG SI.9: Tukey HSD test for Guatemala
avgtm <- aov(vote ~ treat, data=dat1gt)
summary(avgtm)

TukeyHSD(avgtm, conf.level=.95)
plot(TukeyHSD(avgtm, conf.level=.95), las=2)
title(sub="Guatemala")

### FIG SI.10: Tukey HSD test for Chile
avchl <- aov(vote ~ treat, data=dat1ch)
summary(avchl)

TukeyHSD(avchl, conf.level=.95)
plot(TukeyHSD(avchl, conf.level=.95), las=2)
title(sub="Chile")

### FIG SI.11: Tukey HSD test for Uruguay
avury <- aov(vote ~ treat, data=dat1uy)
summary(avury)

TukeyHSD(avury, conf.level=.95)
plot(TukeyHSD(avury, conf.level=.95), las=2)
title(sub="Uruguay")

### TABLE SI.9: Differences in means of stereotype beliefs across treatment groups, Tukey HSD
# stereotype score
aov_ss_treat <- aov(stereotypescore ~ treat, data=dat1)
summary(aov_ss_treat)

TukeyHSD(aov_ss_treat, conf.level=.95)

# honest
aov_ho_treat <- aov(honestnum ~ treat, data=dat1)
summary(aov_ho_treat)

TukeyHSD(aov_ho_treat, conf.level=.95)

# rules
aov_ru_treat <- aov(rulesnum ~ treat, data=dat1)
summary(aov_ru_treat)

TukeyHSD(aov_ru_treat, conf.level=.95)

# risk
aov_ri_treat <- aov(risknum ~ treat, data=dat1)
summary(aov_ri_treat)

TukeyHSD(aov_ri_treat, conf.level=.95)

### TABLE SI.10: Differences in means of stereotype beliefs across country, Tukey HSD
# stereotype score
aov_ss_cntry <- aov(stereotypescore ~ Country, data=dat1)
summary(aov_ss_cntry)

TukeyHSD(aov_ss_cntry, conf.level=.95)

# honest
aov_ho_cntry <- aov(honestnum ~ Country, data=dat1)
summary(aov_ho_cntry)

TukeyHSD(aov_ho_cntry, conf.level=.95)

# rules
aov_ru_cntry <- aov(rulesnum ~ Country, data=dat1)
summary(aov_ru_cntry)

TukeyHSD(aov_ru_cntry, conf.level=.95)

# risk
aov_ri_cntry <- aov(risknum ~ Country, data=dat1)
summary(aov_ri_cntry)

TukeyHSD(aov_ri_cntry, conf.level=.95)

### FIG SI.12: Mean likelihood of vote by treatment/country/women are more honest
### HONESTY

## MEX
# create dichotomous score high v low
dat1mx$honestbi <- ifelse(dat1mx$honestnum < 4 , 0, 1)
summary(dat1mx$honestbi)

dat1mx$honestbi <- as.factor(dat1mx$honestbi)

df_ho_mex <- dat1mx %>%
  group_by(treat, honestbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_ho_mex <- ggplot(df_ho_mex, aes(x=treat, y =mean, fill = honestbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = honestbi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_ho_mex
ggsave("scandals_votebyhonest_mex.pdf")

gg.vote_ho_mexNOLAB <- ggplot(df_ho_mex, aes(x=treat, y =mean, fill = honestbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = honestbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## GTM
# create dichotomous score high v low
dat1gt$honestbi <- ifelse(dat1gt$honestnum < 4 , 0, 1)
summary(dat1gt$honestbi)

dat1gt$honestbi <- as.factor(dat1gt$honestbi)

df_ho_gtm <- dat1gt %>%
  group_by(treat, honestbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_ho_gtm <- df_ho_gtm[-c(5, 8),]

# plot means with CIs
gg.vote_ho_gtmNOLAB <- ggplot(df_ho_gtm, aes(x=treat, y =mean, fill = honestbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = honestbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## CHL
# create dichotomous score high v low
dat1ch$honestbi <- ifelse(dat1ch$honestnum < 4 , 0, 1)
summary(dat1ch$honestbi)

dat1ch$honestbi <- as.factor(dat1ch$honestbi)

df_ho_chl <- dat1ch %>%
  group_by(treat, honestbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_ho_chl <- df_ho_chl[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_ho_chlNOLAB <- ggplot(df_ho_chl, aes(x=treat, y =mean, fill = honestbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = honestbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Chile") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## URY
# create dichotomous score high v low
dat1uy$honestbi <- ifelse(dat1uy$honestnum < 4 , 0, 1)
summary(dat1uy$honestbi)

dat1uy$honestbi <- as.factor(dat1uy$honestbi)

df_ho_ury <- dat1uy %>%
  group_by(treat, honestbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_ho_uryNOLAB <- ggplot(df_ho_ury, aes(x=treat, y =mean, fill = honestbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = honestbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Uruguay") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Honest", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## Combine plots
library(ggpubr)
votebyhonest.combined <- ggarrange(gg.vote_ho_mexNOLAB, NULL, gg.vote_ho_gtmNOLAB, gg.vote_ho_chlNOLAB, NULL, gg.vote_ho_uryNOLAB,
                                   nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                   common.legend = TRUE, legend = "right",
                                   font.label = list(size=4, face="plain"))
votebyhonest.combined
#ggsave("votebyhonest.combined.pdf")


### FIG SI.13: Mean likelihood of vote by treatment/country/women are more risk averse
## MEX
# create dichotomous score high v low
dat1mx$riskbi <- ifelse(dat1mx$risknum < 4 , 0, 1)
summary(dat1mx$riskbi)

dat1mx$riskbi <- as.factor(dat1mx$riskbi)

df_ri_mex <- dat1mx %>%
  group_by(treat, riskbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_ri_mexNOLAB <- ggplot(df_ri_mex, aes(x=treat, y =mean, fill = riskbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = riskbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Risk Averse", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Risk Averse", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## GTM
# create dichotomous score high v low
dat1gt$riskbi <- ifelse(dat1gt$risknum < 4 , 0, 1)
summary(dat1gt$riskbi)

dat1gt$riskbi <- as.factor(dat1gt$riskbi)

df_ri_gtm <- dat1gt %>%
  group_by(treat, riskbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_ri_gtm <- df_ri_gtm[-c(7),]

# plot means with CIs
gg.vote_ri_gtmNOLAB <- ggplot(df_ri_gtm, aes(x=treat, y =mean, fill = riskbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = riskbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Risk Averse", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Risk Averse", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## CHL
# create dichotomous score high v low
dat1ch$riskbi <- ifelse(dat1ch$risknum < 4 , 0, 1)
summary(dat1ch$riskbi)

dat1ch$riskbi <- as.factor(dat1ch$riskbi)

df_ri_chl <- dat1ch %>%
  group_by(treat, riskbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_ri_chl <- df_ri_chl[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_ri_chlNOLAB <- ggplot(df_ri_chl, aes(x=treat, y =mean, fill = riskbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = riskbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Chile") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Risk Averse", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Risk Averse", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## URY
# create dichotomous score high v low
dat1uy$riskbi <- ifelse(dat1uy$risknum < 4 , 0, 1)
summary(dat1uy$riskbi)

dat1uy$riskbi <- as.factor(dat1uy$riskbi)

df_ri_ury <- dat1uy %>%
  group_by(treat, riskbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_ri_uryNOLAB <- ggplot(df_ri_ury, aes(x=treat, y =mean, fill = riskbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = riskbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Uruguay") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Risk Averse", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Risk Averse", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## Combine plots
library(ggpubr)
votebyrisk.combined <- ggarrange(gg.vote_ri_mexNOLAB, NULL, gg.vote_ri_gtmNOLAB, gg.vote_ri_chlNOLAB, NULL, gg.vote_ri_uryNOLAB,
                                 nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                 common.legend = TRUE, legend = "right",
                                 font.label = list(size=4, face="plain"))
votebyrisk.combined
#ggsave("votebyrisk.combined.pdf")

### FIG SI.14: Mean likelihood of vote by treatment/country/women are more rule abiding
## MEX
# create dichotomous score high v low
dat1mx$rulesbi <- ifelse(dat1mx$rulesnum < 4 , 0, 1)
summary(dat1mx$rulesbi)

dat1mx$rulesbi <- as.factor(dat1mx$rulesbi)

df_ru_mex <- dat1mx %>%
  group_by(treat, rulesbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_ru_mexNOLAB <- ggplot(df_ru_mex, aes(x=treat, y =mean, fill = rulesbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = rulesbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Rule Abiding", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Rule Abiding", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## GTM
# create dichotomous score high v low
dat1gt$rulesbi <- ifelse(dat1gt$rulesnum < 4 , 0, 1)
summary(dat1gt$rulesbi)

dat1gt$rulesbi <- as.factor(dat1gt$rulesbi)

df_ru_gtm <- dat1gt %>%
  group_by(treat, rulesbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_ru_gtm <- df_ru_gtm[-c(5),]

# plot means with CIs
gg.vote_ru_gtmNOLAB <- ggplot(df_ru_gtm, aes(x=treat, y =mean, fill = rulesbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = rulesbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Rule Abiding", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Rule Abiding", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## CHL
# create dichotomous score high v low
dat1ch$rulesbi <- ifelse(dat1ch$rulesnum < 4 , 0, 1)
summary(dat1ch$rulesbi)

dat1ch$rulesbi <- as.factor(dat1ch$rulesbi)

df_ru_chl <- dat1ch %>%
  group_by(treat, rulesbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_ru_chl <- df_ru_chl[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_ru_chlNOLAB <- ggplot(df_ru_chl, aes(x=treat, y =mean, fill = rulesbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = rulesbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Chile") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Rule Abiding", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Rule Abiding", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## URY
# create dichotomous score high v low
dat1uy$rulesbi <- ifelse(dat1uy$rulesnum < 4 , 0, 1)
summary(dat1uy$rulesbi)

dat1uy$rulesbi <- as.factor(dat1uy$rulesbi)

df_ru_ury <- dat1uy %>%
  group_by(treat, rulesbi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_ru_uryNOLAB <- ggplot(df_ru_ury, aes(x=treat, y =mean, fill = rulesbi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = rulesbi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Uruguay") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Women More Rule Abiding", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "Women More Rule Abiding", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

## Combine plots
library(ggpubr)
votebyrules.combined <- ggarrange(gg.vote_ru_mexNOLAB, NULL, gg.vote_ru_gtmNOLAB, gg.vote_ru_chlNOLAB, NULL, gg.vote_ru_uryNOLAB,
                                  nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                  common.legend = TRUE, legend = "right",
                                  font.label = list(size=4, face="plain"))
votebyrules.combined
#ggsave("votebyrules.combined.pdf")

### FIG SI.15: Mean likelihood of vote by treatment and higher stereotype score (13) in Guatemala
# create dichotomous score high v low, cut off at 13 
dat1gt$sscorebi_13 <- ifelse(dat1gt$stereotypescore < 13 , 0, 1)
summary(dat1gt$sscorebi_13)

dat1gt$sscorebi_13 <- as.factor(dat1gt$sscorebi_13)

# GTM
df_st13_gtm <- dat1gt %>%
  group_by(treat, sscorebi_13) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st13_gtm <- df_st13_gtm[-c(5, 8),]

# plot means with CIs
gg.vote_st13_gtm <- ggplot(df_st13_gtm, aes(x=treat, y =mean, fill = sscorebi_13)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi_13), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_st13_gtm
#ggsave("scandals_votebyst13_gtm.pdf")

### FIG SI.16: Mean likelihood of vote by treatment and higher stereotype score (15) in Guatemala
# create dichotomous score high v low, cut off at 15 (highest)
dat1gt$sscorebi_15 <- ifelse(dat1gt$stereotypescore < 15 , 0, 1)
summary(dat1gt$sscorebi_15)

dat1gt$sscorebi_15 <- as.factor(dat1gt$sscorebi_15)

# GTM
df_st15_gtm <- dat1gt %>%
  group_by(treat, sscorebi_15) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st15_gtm <- df_st15_gtm[-c(5, 8),]

# plot means with CIs
gg.vote_st15_gtm <- ggplot(df_st15_gtm, aes(x=treat, y =mean, fill = sscorebi_15)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi_15), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_st15_gtm
#ggsave("scandals_votebyst15_gtm.pdf")

### FIG SI.17: Mean likelihood of vote by treatment/country for people who passed gender manipulation check
# subset by MC
pass1 <- dat1[dat1$passeds==1,]

pass2 <- dat1[dat1$passedc==1,]

pass3 <- dat1[dat1$passedall==1,]

#subset by countries
pass1mx <- pass1[pass1$ccode==1,]
pass1gt <- pass1[pass1$ccode==2,]
pass1ch <- pass1[pass1$ccode==3,]
pass1uy <- pass1[pass1$ccode==4,]

pass2mx <- pass2[pass2$ccode==1,]
pass2gt <- pass2[pass2$ccode==2,]
pass2ch <- pass2[pass2$ccode==3,]
pass2uy <- pass2[pass2$ccode==4,]

pass3mx <- pass3[pass3$ccode==1,]
pass3gt <- pass3[pass3$ccode==2,]
pass3ch <- pass3[pass3$ccode==3,]
pass3uy <- pass3[pass3$ccode==4,]

## Passed Sex Mani Check
## Country Means
df_cntrypass <- pass1 %>%
  group_by(treat, Country) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_cntrypass <- df_cntrypass[-c(17),]

# plot means with CIs
gg.vote_cntrypass <- ggplot(df_cntrypass, aes(x=treat, y =mean, fill = Country)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape =  treat, color = Country), size = 3) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote") +
  guides(color = FALSE, fill = FALSE) +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() +
  facet_wrap(~ Country)

gg.vote_cntrypass
#ggsave("scandals_vote_cntrypass1.pdf")

### FIG SI.18: Mean likelihood of vote by treatment/country for people who passed corruption manipulation check
## Passed Corruption Mani Check
## Country Means
df_cntrypass2 <- pass2 %>%
  group_by(treat, Country) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_cntrypass2 <- df_cntrypass2[-c(17),]

# plot means with CIs
gg.vote_cntrypass2 <- ggplot(df_cntrypass2, aes(x=treat, y =mean, fill = Country)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape =  treat, color = Country), size = 3) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote") +
  guides(color = FALSE, fill = FALSE) +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() +
  facet_wrap(~ Country)

gg.vote_cntrypass2
#ggsave("scandals_vote_cntrypass2.pdf")

### FIG SI.19: Mean likelihood of vote by treatment/country for people who passed both manipulation checks
## Passed Both Mani Checks
## Country Means
df_cntrypass3 <- pass3 %>%
  group_by(treat, Country) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_cntrypass3 <- ggplot(df_cntrypass3, aes(x=treat, y =mean, fill = Country)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape =  treat, color = Country), size = 3) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote") +
  guides(color = FALSE, fill = FALSE) +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() +
  facet_wrap(~ Country)

gg.vote_cntrypass3
#ggsave("scandals_vote_cntrypassall.pdf")

### FIG SI.20: Mean likelihood of vote by treatment/country/respondent sex/stereotype score
## Guatemala
## women only
gtfem <- dat1gt[dat1gt$female==1,]

## men only
gtmale <- dat1gt[dat1gt$female==0,]

gtfemss <- gtfem %>% 
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

gtfemss <- gtfemss[-c(9),]

gtmaless <- gtmale %>% 
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

gtmaless <- gtmaless[-c(5, 8, 11),]

# plot female means with CIs
gg.vote_gtfemss <- ggplot(gtfemss, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Guatemalan Women") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_gtfemss

# plot male means with CIs
gg.vote_gtmaless <- ggplot(gtmaless, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Guatemalan Men") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_gtmaless

# combine plots
votebysexss_GTM <- ggarrange(gg.vote_gtfemss, gg.vote_gtmaless, 
                             nrow = 1, ncol = 2,
                             common.legend = TRUE, legend = "right",
                             font.label = list(size=4, face="plain"))
votebysexss_GTM
#ggsave("votebysexss_GTM.pdf")

## Mexico
## women only
mxfem <- dat1mx[dat1mx$female==1,]

## men only
mxmale <- dat1mx[dat1mx$female==0,]

mxfemss <- mxfem %>% 
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

mxfemss <- mxfemss[-c(9),]

mxmaless <- mxmale %>% 
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

mxmaless <- mxmaless[-c(9),]

# plot female means with CIs
gg.vote_mxfemss <- ggplot(mxfemss, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Mexican Women") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_mxfemss

# plot male means with CIs
gg.vote_mxmaless <- ggplot(mxmaless, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Mexican Men") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_mxmaless

# combine plots
votebysexss_MEX <- ggarrange(gg.vote_mxfemss, gg.vote_mxmaless, 
                             nrow = 1, ncol = 2,
                             common.legend = TRUE, legend = "right",
                             font.label = list(size=4, face="plain"))
votebysexss_MEX
#ggsave("votebysexss_MEX.pdf")

## Chile
## women only
chfem <- dat1ch[dat1ch$female==1,]

## men only
chmale <- dat1ch[dat1ch$female==0,]

chfemss <- chfem %>% 
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

chfemss <- chfemss[-c(9),]

chmaless <- chmale %>% 
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

chmaless <- chmaless[-c(3, 6, 9, 12, 13),]

# plot female means with CIs
gg.vote_chfemss <- ggplot(chfemss, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Chilean Women") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_chfemss

# plot male means with CIs
gg.vote_chmaless <- ggplot(chmaless, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Chilean Men") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_chmaless

# combine plots
votebysexss_CHL <- ggarrange(gg.vote_chfemss, gg.vote_chmaless, 
                             nrow = 1, ncol = 2,
                             common.legend = TRUE, legend = "right",
                             font.label = list(size=4, face="plain"))
votebysexss_CHL
#ggsave("votebysexss_CHL.pdf")

## Uruguay
## women only
uyfem <- dat1uy[dat1uy$female==1,]

## men only
uymale <- dat1uy[dat1uy$female==0,]

uyfemss <- uyfem %>% 
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

uyfemss <- uyfemss[-c(9),]

uymaless <- uymale %>% 
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

uymaless <- uymaless[-c(9),]

# plot female means with CIs
gg.vote_uyfemss <- ggplot(uyfemss, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Uruguayan Women") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_uyfemss

# plot male means with CIs
gg.vote_uymaless <- ggplot(uymaless, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote", title = "Uruguayan Men") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_uymaless

# combine plots
votebysexss_URY <- ggarrange(gg.vote_uyfemss, gg.vote_uymaless, 
                             nrow = 1, ncol = 2,
                             common.legend = TRUE, legend = "right",
                             font.label = list(size=4, face="plain"))
votebysexss_URY
#ggsave("votebysexss_URY.pdf")

# combine all plots for all countries
votebysexss_ALL <- ggarrange(gg.vote_mxfemss, gg.vote_mxmaless,
                             gg.vote_gtfemss, gg.vote_gtmaless,
                             gg.vote_chfemss, gg.vote_chmaless,
                             gg.vote_uyfemss, gg.vote_uymaless,
                             nrow = 4, ncol= 2,
                             common.legend= TRUE,
                             legend = "right",
                             font.label = list(size=4, face="plain"))
votebysexss_ALL
#ggsave("votebysexss_ALL.pdf")

### FIG SI.21: Mean likelihood of vote by treatment and respondent sex, all countries pooled
df_fem <- dat1 %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_fem <- df_fem[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_fem <- ggplot(df_fem, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()

#ggsave("scandals_votebysex.pdf")

### FIG SI.22: Mean likelihood of vote by treatment and stereotype score, all countries pooled
dat1$sscorebi <- ifelse(dat1$stereotypescore < 12 , 0, 1)
summary(dat1$sscorebi)

dat1$sscorebi <- as.factor(dat1$sscorebi)

df_st <- dat1 %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st <- df_st[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_st <- ggplot(df_st, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "Treatment Condition", y = "Likelihood of Vote") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

gg.vote_st
#ggsave("scandals_votebystsc.pdf")

### FIG SI.23: Mean likelihood of vote by treatment/country/stereotype score, Bonferroni adjustment
  # Find new alpha by dividing 0.05 by number of tests
  # For Stereotype Score across Condition and Country, 16 tests
  # 0.05 / 16 = 0.003, Z score for an alpha of 0.003 is 2.75, adjust CIs for this new Z score/alpha

## MEX
# SE * 2.75
df_st_mexBF <- dat1mx %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*2.75)

# plot means with CIs
gg.vote_st_mexNOLABBF <- ggplot(df_st_mexBF, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## GTM
# SE * 2.75
df_st_gtmBF <- dat1gt %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*2.75)

df_st_gtmBF <- df_st_gtmBF[-c(5, 8),]

# plot means with CIs
gg.vote_st_gtmNOLABBF <- ggplot(df_st_gtmBF, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## CHL
# SE * 2.75
df_st_chlBF <- dat1ch %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*2.75)

df_st_chlBF <- df_st_chlBF[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_st_chlNOLABBF <- ggplot(df_st_chlBF, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Chile") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## URY
# SE * 2.75
df_st_uryBF <- dat1uy %>%
  group_by(treat, sscorebi) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*2.75)

# plot means with CIs
gg.vote_st_uryNOLABBF <- ggplot(df_st_uryBF, aes(x=treat, y =mean, fill = sscorebi)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = sscorebi), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Uruguay") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

votebyscore.combined <- ggarrange(gg.vote_st_mexNOLABBF, NULL, gg.vote_st_gtmNOLABBF, gg.vote_st_chlNOLABBF, NULL, gg.vote_st_uryNOLABBF,
                                  nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                  common.legend = TRUE, legend = "right",
                                  font.label = list(size=4, face="plain"))
votebyscore.combined
ggsave("votebyscoreBF.combined.pdf")

### FIG SI.24: Mean likelihood of vote by treatment/country/respondent sex, Bonferroni adjustment
## MEX
# SE * 2.75
df_fem_mexBF <- dat1mx %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*2.75)

df_fem_mexBF <- df_fem_mexBF[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_fem_mexNOLABBF <- ggplot(df_fem_mexBF, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = NULL, y = "Likelihood of Vote", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()
gg.vote_fem_mexNOLABBF

## GTM
# SE * 2.75
df_fem_gtmBF <- dat1gt %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*2.75)

df_fem_gtmBF <- df_fem_gtmBF[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_fem_gtmNOLABBF <- ggplot(df_fem_gtmBF, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = NULL, y = "Likelihood of Vote", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()
gg.vote_fem_gtmNOLABBF

## CHL
# SE * 2.75
df_fem_chlBF <- dat1ch %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*2.75)

df_fem_chlBF <- df_fem_chlBF[-c(3, 6, 9),]

# plot means with CIs
gg.vote_fem_chlNOLABBF <- ggplot(df_fem_chlBF, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = NULL, y = "Likelihood of Vote", title = "Chile") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()
gg.vote_fem_chlNOLABBF

## URY
# SE * 2.75
df_fem_uryBF <- dat1uy %>%
  group_by(treat, femf) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*2.75)

df_fem_uryBF <- df_fem_uryBF[-c(7, 10),]

# plot means with CIs
gg.vote_fem_uryNOLABBF <- ggplot(df_fem_uryBF, aes(x=treat, y =mean, fill = femf)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = femf), size = 4) +
  labs(x = NULL, y = "Likelihood of Vote", title = "Uruguay") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_fill_discrete(name = "Respondent Sex", labels = c("Male", "Female")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll()
gg.vote_fem_uryNOLABBF

## Combining Plots
votebysexBF.combined <- ggarrange(gg.vote_fem_mexNOLABBF, NULL, gg.vote_fem_gtmNOLABBF, gg.vote_fem_chlNOLABBF, NULL, gg.vote_fem_uryNOLABBF,
                                  nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                  common.legend = TRUE, legend = "right",
                                  font.label = list(size=4, face="plain"))
votebysexBF.combined
#ggsave("votebysexBF.combined.pdf")

### FIG SI.25: Mean likelihood of vote by treatment/country/stereotype belief factor score
install.packages("psych")
library(psych)

# run factor analysis for stereotype beliefs
factor_analysis <- fa(dat1[, c("honestnum", "rulesnum", "risknum")], nfactors = 1)

# extract factor scores
factor_scores <- as.data.frame(factor.scores(dat1[, c("honestnum", "rulesnum", "risknum")], factor_analysis)$scores)

# bind factor scores back in with original data
dat1_withfactorscore <- cbind(dat1, factor_scores)

# create a dichotomous variable to represent positive and negative factor scores
dat1_withfactorscore$posfactor <- ifelse(dat1_withfactorscore$MR1 > 0, 1, 0)

## Plot means across high and low factor
# subset by country 
dat1factormx <- dat1_withfactorscore[dat1_withfactorscore$ccode==1,]
dat1factorgt <- dat1_withfactorscore[dat1_withfactorscore$ccode==2,]
dat1factorch <- dat1_withfactorscore[dat1_withfactorscore$ccode==3,]
dat1factoruy <- dat1_withfactorscore[dat1_withfactorscore$ccode==4,]

## MEX
# factor score
dat1factormx$posfactor <- as.factor(dat1factormx$posfactor)

df_st_mexF <- dat1factormx %>%
  group_by(treat, posfactor) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_st_mexNOLABF <- ggplot(df_st_mexF, aes(x=treat, y =mean, fill = posfactor)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = posfactor), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Mexico") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Factor Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Factor Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## GTM
# factor score
dat1factorgt$posfactor <- as.factor(dat1factorgt$posfactor)

df_st_gtmF <- dat1factorgt %>%
  group_by(treat, posfactor) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st_gtmF <- df_st_gtmF[-c(5, 8),]

# plot means with CIs
gg.vote_st_gtmNOLABF <- ggplot(df_st_gtmF, aes(x=treat, y =mean, fill = posfactor)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = posfactor), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Guatemala") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Factor Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Factor Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## CHL
# factor score
dat1factorch$posfactor <- as.factor(dat1factorch$posfactor)

df_st_chlF <- dat1factorch %>%
  group_by(treat, posfactor) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

df_st_chlF <- df_st_chlF[-c(3, 6, 9, 12),]

# plot means with CIs
gg.vote_st_chlNOLABF <- ggplot(df_st_chlF, aes(x=treat, y =mean, fill = posfactor)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = posfactor), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Chile") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Factor Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Factor Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

## URY
# factor score
dat1factoruy$posfactor <- as.factor(dat1factoruy$posfactor)

df_st_uryF <- dat1factoruy %>%
  group_by(treat, posfactor) %>%
  summarise(n=n(), mean=mean(vote, na.rm = TRUE), sd=sd(vote, na.rm = TRUE),
            se=sd/sqrt(n), ci = se*1.96)

# plot means with CIs
gg.vote_st_uryNOLABF <- ggplot(df_st_uryF, aes(x=treat, y =mean, fill = posfactor)) +
  geom_errorbar(width = .2, aes(ymin = mean - ci, ymax = mean + ci), position=position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5), aes(shape = treat, color = posfactor), size = 4) +
  labs(x = "", y = "Likelihood of Vote", title = "Uruguay") +
  scale_shape_manual(values = c(15, 16, 17, 8), name = "Condition", labels = c("Man", "Woman", "M-Scandal", "W-Scandal")) +
  scale_color_discrete(name = "High Stereotype Factor Score", labels = c("No", "Yes")) +
  scale_fill_discrete(name = "High Stereotype Factor Score", labels = c("No", "Yes")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(labels = c("M","W", "M-S", "W-S")) +
  theme_eqll() 

# combine plots
library(ggpubr)
votebyfactor.combined <- ggarrange(gg.vote_st_mexNOLABF, NULL, gg.vote_st_gtmNOLABF, gg.vote_st_chlNOLABF, NULL, gg.vote_st_uryNOLABF,
                                   nrow = 2, ncol = 3,  widths = c(1, 0.05, 1),
                                   common.legend = TRUE, legend = "right",
                                   font.label = list(size=4, face="plain"))
votebyfactor.combined
#ggsave("votebyfactor.combined.pdf")

