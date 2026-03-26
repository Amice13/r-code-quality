#######################################################################
## author:    Michael L. Wicki
## contact:   michael.wicki@istp.ethz.ch, ETH Zurich
## file name: RDD.R
## Context:   ISTP mobility lab - Begleitstudie Linie 12
## started:   2018-08-03
## Summary:   Analysis the Causal Effects of the Uber Accident during Survey 1
#######################################################################

rm(list = ls())
getwd()
setwd("\\\\gess-fs.d.ethz.ch\\home$\\wimi\\Documents\\Autonomous_Driving\\analysis_paper")
set.seed(42)
.libPaths( "\\\\gess-fs.d.ethz.ch\\home$\\wimi\\Documents\\R\\win-library\\3.3" )


library(ggplot2)
library(ggthemes)
library(rdd)
library(texreg)
library(forcats)
library(dotwhisker)
library(cowplot)
library(stargazer)



#################### Import
load("./originalData/20180504_DataPart1_combined_final.Rda")

df <- dataSel
df <- df[df$papi==0,]

rm(dataSel)

date <- read.csv(file = "./originalData/date.csv")

date <- data.frame( date = date$StartDate,
                    finished = date$Finished,
                    id = date$ExternalReference)

df <- merge(x=df, date, by="id")

df <- df[df$papi==0,]

rm(date)

df$date <- as.Date(df$date, "%d.%m.%Y")

df$cutoff <- as.numeric(df$date - as.Date("18-03-2018", format = "%d-%m-%Y"))

df$Acci <- factor(ifelse(df$cutoff <1, "Pre", "Post"), levels = c("Pre", "Post"))

df$educ <- as.factor(ifelse(df$educ=="Obligatorische Schulbildung", "Minimum_education",
                     ifelse(df$educ=="Keine", "Minimum_education",
                            ifelse(df$educ=="Berufslehre, Berufsfachschule, Handels(mittel)schule", "Secondary_education",
                                   ifelse(df$educ=="Gymnasiale MaturitÃ¤t, BerufsmaturitÃ¤t", "Secondary_education",
                                          ifelse(df$educ=="HÃ¶here Fach-/Berufsbildung (z.B. eidg. Fachausweis, Meisterdiplom)", "Tertiary_education",
                                                 ifelse(df$educ=="Fachhochschulabschluss, PÃ¤dagogische Hochschule", "Tertiary_education",
                                                        ifelse(df$educ=="UniversitÃ¤tsabschluss", "Tertiary_education", 
                                                               "Other_education"))))))))

df <- within(df, educ <- relevel(educ, ref = "Other_education"))

df$inc <- ifelse(df$inc1 == "Keine Antwort" | df$inc1 == "Weiss nicht", NA, as.numeric(df$inc1)-2)

controls <- c("+ age + male + educ + inc")

# Concern ####

df$avCncrn_num <- 6 - as.numeric(fct_rev(df$avCncrn))

RDD_genConcern <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avCncrn_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Confidence in self-driving vehicles") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_genConcern.png", RDD_genConcern, width = 8, height = 8)


bw_concern <- IKbandwidth(X=df$cutoff, Y=df$avCncrn_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_concern <- lm(paste0("avCncrn_num ~ Acci", controls),
                data = df[df$cutoff>-bw_concern & df$cutoff<bw_concern,])
summary(m_concern)

m_concern2 <- lm(paste0("avCncrn_num ~ Acci", controls),
                data = df[df$cutoff>-2*bw_concern & df$cutoff<2*bw_concern,])
summary(m_concern2)

# System Reliability ####

df$avcSysRl_num <- as.numeric(df$avcSysRl)

RDD_SysRl <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avcSysRl_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("System reliability concern") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_SysRl.png", RDD_SysRl, width = 8, height = 8)


bw_rel <- IKbandwidth(X=df$cutoff, Y=df$avcSysRl_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_rel <- lm(paste0("avcSysRl_num ~ Acci", controls),
            data = df[df$cutoff>-bw_rel & df$cutoff<bw_rel,])
summary(m_rel)

m_rel2 <- lm(paste0("avcSysRl_num ~ Acci", controls),
             data = df[df$cutoff>-2*bw_rel & df$cutoff<2*bw_rel,])
summary(m_rel2)

# System security ####

df$avcSysSc_num <- as.numeric(df$avcSysSc)

RDD_SysSc <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avcSysSc_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("System safety concern") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_SysSc.png", RDD_SysSc, width = 8, height = 8)


bw_SysSc <- IKbandwidth(X=df$cutoff, Y=df$avcSysSc_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_sysSc <- lm(paste0("avcSysSc_num ~ Acci", controls),
              data = df[df$cutoff>-bw_SysSc & df$cutoff<bw_SysSc,])
summary(m_sysSc)

m_sysSc2 <- lm(paste0("avcSysSc_num ~ Acci", controls),
               data = df[df$cutoff>-2*bw_SysSc & df$cutoff<2*bw_SysSc,])
summary(m_sysSc2)

# Liability ####

df$avcAcci_num <- as.numeric(df$avcAcci)

RDD_liability <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avcAcci_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Liability concern") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_liability.png", RDD_liability, width = 8, height = 8)


bw_liability <- IKbandwidth(X=df$cutoff, Y=df$avcAcci_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_liability <- lm(paste0("avcAcci_num ~ Acci", controls),
                data = df[df$cutoff>-bw_liability & df$cutoff<bw_liability,])
summary(m_liability)

m_liability2 <- lm(paste0("avcAcci_num ~ Acci", controls),
                  data = df[df$cutoff>-2*bw_liability & df$cutoff<2*bw_liability,])
summary(m_liability2)

# Privacy Concern ####

df$avcPriv_num <- as.numeric(df$avcPriv)

RDD_privCncrn <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avcPriv_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Privacy concern") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_privCncrn.png", RDD_privCncrn, width = 8, height = 8)


bw_priv <- IKbandwidth(X=df$cutoff, Y=df$avcPriv_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_priv <- lm(paste0("avcPriv_num ~ Acci", controls),
                data = df[df$cutoff>-bw_priv & df$cutoff<bw_priv,])
summary(m_priv)

m_priv2 <- lm(paste0("avcPriv_num ~ Acci", controls),
                 data = df[df$cutoff>-2*bw_priv & df$cutoff<2*bw_priv,])
summary(m_priv2)

# Software Misuse ####

df$avcHack_num <- as.numeric(df$avcHack)

RDD_systemMisuse <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avcHack_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Software misuse concern") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_systemMisuse.png", RDD_systemMisuse, width = 8, height = 8)


bw_hack <- IKbandwidth(X=df$cutoff, Y=df$avcHack_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_hack <- lm(paste0("avcHack_num ~ Acci", controls),
             data = df[df$cutoff>-bw_hack & df$cutoff<bw_hack,])
summary(m_hack)

m_hack2 <- lm(paste0("avcHack_num ~ Acci", controls),
              data = df[df$cutoff>-2*bw_hack & df$cutoff<2*bw_hack,])
summary(m_hack2)

# Loss of driving enjoyment ####

df$avcDrPl_num <- as.numeric(df$avcDrPl)

RDD_drEnjoy <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avcDrPl_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Loss of driving enjoyment") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_drEnjoy.png", RDD_drEnjoy, width = 8, height = 8)


bw_enj <- IKbandwidth(X=df$cutoff, Y=df$avcDrPl_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_enj <- lm(paste0("avcDrPl_num ~ Acci", controls),
            data = df[df$cutoff>-bw_enj & df$cutoff<bw_enj,])
summary(m_enj)

m_enj2 <- lm(paste0("avcDrPl_num ~ Acci", controls),
             data = df[df$cutoff>-2*bw_enj & df$cutoff<2*bw_enj,])
summary(m_enj2)

# Loss of driving control ####

df$avcDrCtr_num <- as.numeric(df$avcDrCtr)

RDD_drCtr <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avcDrCtr_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Driver control loss concern") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_drCtr.png", RDD_drCtr, width = 8, height = 8)

bw_ctr <- IKbandwidth(X=df$cutoff, Y=df$avcDrCtr_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_ctr <- lm(paste0("avcDrCtr_num ~ Acci", controls),
            data = df[df$cutoff>-bw_ctr & df$cutoff<bw_ctr,])
summary(m_ctr)

m_ctr2 <- lm(paste0("avcDrCtr_num ~ Acci", controls),
             data = df[df$cutoff>-2*bw_ctr & df$cutoff<2*bw_ctr,])
summary(m_ctr2)

# Job Loss ####

df$avcJoblos_num <- as.numeric(df$avcJoblos)

RDD_jobLoss <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avcJoblos_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Job loss concern") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_jobLoss.png", RDD_jobLoss, width = 8, height = 8)

bw_job <- IKbandwidth(X=df$cutoff, Y=df$avcJoblos_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_job <- lm(paste0("avcJoblos_num ~ Acci", controls),
            data = df[df$cutoff>-bw_job & df$cutoff<bw_job,])
summary(m_job)

m_job2 <- lm(paste0("avcJoblos_num ~ Acci", controls),
             data = df[df$cutoff>-2*bw_job & df$cutoff<2*bw_job,])
summary(m_job2)

# Policy Support for Test Drives ####

df$avTests_num <- abs(as.numeric(df$avTests)-6)

RDD_testsupport <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = avTests_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Self-driving vehicles test support") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_testsupport.png", RDD_testsupport, width = 8, height = 8)

bw_test <- IKbandwidth(X=df$cutoff, Y=df$avTests_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_test <- lm(paste0("avTests_num ~ Acci", controls),
            data = df[df$cutoff>-bw_test & df$cutoff<bw_test,])
summary(m_test)

m_test2 <- lm(paste0("avTests_num ~ Acci", controls),
             data = df[df$cutoff>-2*bw_test & df$cutoff<2*bw_test,])
summary(m_test2)

# Change StVo ####

df$chgStVO_num <- abs(as.numeric(df$chgStVO)-6)

RDD_StVOChng <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d"), y = chgStVO_num, colour = Acci), size=.25) + 
  geom_jitter(height = .25, width = 0.1, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_bw() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2018-03-18", format = "%Y-%m-%d"), lty="dashed") +
  theme(legend.position="none", text = element_text(size=25), axis.text.x = element_text(size=20))+
  ggtitle("Legal adaptation support") +
  scale_colour_manual(values = c("#7fcdbb", "#2c7fb8")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2018-03-10", "2018-03-25"), format = "%Y-%m-%d"))

ggsave(filename = "./plots/RDD_StVOChng.png", RDD_StVOChng, width = 8, height = 8)

bw_StVO <- IKbandwidth(X=df$cutoff, Y=df$chgStVO_num, cutpoint = 0, verbose = T, kernel = "triangular")

m_StVO <- lm(paste0("chgStVO_num ~ Acci", controls),
             data = df[df$cutoff>-bw_StVO & df$cutoff<bw_StVO,])
summary(m_StVO)

m_StVO2 <- lm(paste0("chgStVO_num ~ Acci", controls),
              data = df[df$cutoff>-2*bw_StVO & df$cutoff<2*bw_StVO,])
summary(m_StVO2)

# full sample regressions ###
m_concern3 <- lm(paste0("avCncrn_num ~ Acci", controls),
                 data = df)
summary(m_concern3)

m_StVO3 <- lm(paste0("chgStVO_num ~ Acci", controls),
                 data = df)
summary(m_StVO3)

m_test3 <- lm(paste0("avTests_num ~ Acci", controls),
              data = df)
summary(m_test3)

# treatment and running variable interaction ###
df$interaction <- df$cutoff * (as.numeric(df$Acci) - 1)
m_concern4 <- lm("avCncrn_num ~ Acci + cutoff + interaction",
                 data = df)
summary(m_concern4)

m_StVO4 <- lm("chgStVO_num ~ Acci + cutoff + interaction",
                     data = df)
summary(m_StVO4)

m_test4 <- lm("avTests_num ~ Acci + cutoff + interaction",
                     data = df)
summary(m_test4)

m_concern4_8 <- lm("avCncrn_num ~ Acci + cutoff + interaction",
                   data = df[df$cutoff>-2*bw_enj & df$cutoff<2*bw_enj,])

m_concern4_4 <- lm("avCncrn_num ~ Acci + cutoff + interaction",
                   data = df[df$cutoff>-bw_enj & df$cutoff<bw_enj,])

m_StVO4_8 <- lm("chgStVO_num ~ Acci + cutoff + interaction",
                data = df[df$cutoff>-2*bw_enj & df$cutoff<2*bw_enj,])

m_StVO4_4 <- lm("chgStVO_num ~ Acci + cutoff + interaction",
                data = df[df$cutoff>-bw_enj & df$cutoff<bw_enj,])

m_test4_8 <- lm("avTests_num ~ Acci + cutoff + interaction",
                data = df[df$cutoff>-2*bw_enj & df$cutoff<2*bw_enj,])

m_test4_4 <- lm("avTests_num ~ Acci + cutoff + interaction",
                data = df[df$cutoff>-bw_enj & df$cutoff<bw_enj,])

# Baseline ###
m_concern5 <- lm("avCncrn_num ~ Acci",
                 data = df)
summary(m_concern5)

m_StVO5 <- lm("chgStVO_num ~ Acci",
              data = df)
summary(m_StVO5)

m_test5 <- lm("avTests_num ~ Acci",
              data = df)
summary(m_test5)

m_concern5_8 <- lm("avCncrn_num ~ Acci",
             data = df[df$cutoff>-2*bw_enj & df$cutoff<2*bw_enj,])

m_concern5_4 <- lm("avCncrn_num ~ Acci",
                   data = df[df$cutoff>-bw_enj & df$cutoff<bw_enj,])

m_StVO5_8 <- lm("chgStVO_num ~ Acci",
                   data = df[df$cutoff>-2*bw_enj & df$cutoff<2*bw_enj,])

m_StVO5_4 <- lm("chgStVO_num ~ Acci",
                   data = df[df$cutoff>-bw_enj & df$cutoff<bw_enj,])

m_test5_8 <- lm("avTests_num ~ Acci",
                   data = df[df$cutoff>-2*bw_enj & df$cutoff<2*bw_enj,])

m_test5_4 <- lm("avTests_num ~ Acci",
                   data = df[df$cutoff>-bw_enj & df$cutoff<bw_enj,])

# Export ####
############# interaction and baseline DVs ##################
#graph Baseline
plotfull5 <- dwplot(list(m_StVO5, m_concern5, m_test5), show_intercept = TRUE, dot_args = list(aes(shape = model)),
                    whisker_args = list(aes(linetype = model))) +
  scale_y_discrete(labels = c(AcciPost = 'Treatment group')) +
  theme_bw() + xlab(" ") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("A) Full sample, N=1094") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1, 4)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans")) +
  guides(col=guide_legend(nrow=1,byrow=TRUE))

legend <- get_legend(plotfull5)
plotfull5 <- plotfull5 + theme(legend.position = 0)

plotfull5_8 <- dwplot(list(m_StVO5_8, m_concern5_8, m_test5_8), show_intercept = TRUE, dot_args = list(aes(shape = model)),
                      whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("B) ± 8.8 days, N=569") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1, 4)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans"), legend.position = "none")

plotfull5_4 <- dwplot(list(m_StVO5_4, m_concern5_4, m_test5_4), show_intercept = TRUE, dot_args = list(aes(shape = model)),
                      whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab(" ") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("C) ± 4.4 days, N=151") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1, 4)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans"), legend.position = "none")


jointBaseline <- ggdraw() +
  draw_plot(plotfull5, x = 0, y = 0.1, width = 0.42, height = 0.9) +
  draw_plot(plotfull5_8, x = 0.42, y = 0.1, width = 0.29, height = 0.9) +
  draw_plot(plotfull5_4, x = 0.71, y = 0.1, width = 0.29, height = 0.9) +
  draw_plot(legend, x = 0.1, y = 0.02, width = 1, height = 0.1)
jointBaseline

ggsave(filename = "./plots/jointBaseline.png", jointBaseline, width = 13, height = 8)

#graph interaction
plotfull4 <- dwplot(list(m_StVO4, m_concern4, m_test4), show_intercept = TRUE, dot_args = list(aes(shape = model)),
                    whisker_args = list(aes(linetype = model))) +
  scale_y_discrete(labels = c(AcciPost = 'Treatment group', cutoff = 'Days', interaction = 'Treatment*Days')) +
  theme_bw() + xlab(" ") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("A) Full sample, N=1094") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1.5, 3.5)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans")) +
  guides(col=guide_legend(nrow=1,byrow=TRUE))

legend <- get_legend(plotfull4)
plotfull4 <- plotfull4 + theme(legend.position = 0)

plotfull4_8 <- dwplot(list(m_StVO4_8, m_concern4_8, m_test4_8), show_intercept = TRUE, dot_args = list(aes(shape = model)),
                      whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("B) ± 8.8 days, N=569") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1.5, 3.5)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans"), legend.position = "none")

plotfull4_4 <- dwplot(list(m_StVO4_4, m_concern4_4, m_test4_4), show_intercept = TRUE, dot_args = list(aes(shape = model)),
                      whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab(" ") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("C) ± 4.4 days, N=151") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1.5, 3.5)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans"), legend.position = "none")


jointInteract <- ggdraw() +
  draw_plot(plotfull4, x = 0, y = 0.1, width = 0.42, height = 0.9) +
  draw_plot(plotfull4_8, x = 0.42, y = 0.1, width = 0.29, height = 0.9) +
  draw_plot(plotfull4_4, x = 0.71, y = 0.1, width = 0.29, height = 0.9) +
  draw_plot(legend, x = 0.1, y = 0.02, width = 1, height = 0.1)
jointInteract

ggsave(filename = "./plots/jointInteract.png", jointInteract, width = 13, height = 8)

#Interact and Baseline
plotfull5 <- plotfull5 + coord_cartesian(xlim = c(-1.5, 3.5)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) + theme(plot.title = element_text(size=16, family="Sans", face="plain")) +
  ggtitle("A) Pure treatment full sample")
plotfull5_8 <- plotfull5_8 + coord_cartesian(xlim = c(-1.5, 3.5)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) + xlab(" ") + theme(plot.title = element_text(size=16, family="Sans", face="plain")) +
  ggtitle("B) Pure treatment ± 8.8 days")
plotfull5_4 <- plotfull5_4 + coord_cartesian(xlim = c(-1.5, 3.5)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) + theme(plot.title = element_text(size=16, family="Sans", face="plain")) +
  ggtitle("C) Pure treatment ± 4.4 days")
plotfull4 <- plotfull4 + theme(plot.title = element_text(size=16, family="Sans", face="plain")) +
  ggtitle("D) Response day interaction full sample")
plotfull4_8 <- plotfull4_8 + theme(plot.title = element_text(size=16, family="Sans", face="plain")) +
  ggtitle("E) Response day interaction ± 8.8 days")
plotfull4_4 <- plotfull4_4 + theme(plot.title = element_text(size=16, family="Sans", face="plain")) +
  ggtitle("F) Response day interaction ± 4.4 days")

jointFull <- ggdraw() +
  draw_plot(plotfull5, x = 0, y = 0.65, width = 0.4, height = 0.35) +
  draw_plot(plotfull5_8, x = 0.4, y = 0.65, width = 0.3, height = 0.35) +
  draw_plot(plotfull5_4, x = 0.7, y = 0.65, width = 0.3, height = 0.35) +
  draw_plot(plotfull4, x = 0, y = 0.1, width = 0.4, height = 0.55) +
  draw_plot(plotfull4_8, x = 0.4, y = 0.1, width = 0.3, height = 0.55) +
  draw_plot(plotfull4_4, x = 0.7, y = 0.1, width = 0.3, height = 0.55) +
  draw_plot(legend, x = 0.15, y = 0.02, width = 1, height = 0.1)
jointFull

ggsave(filename = "./plots/jointFull.png", jointFull, width = 14.5, height = 8)


#graph
plotfull3 <- dwplot(list(m_StVO3, m_concern3, m_test3), show_intercept = FALSE, dot_args = list(aes(shape = model)),
                    whisker_args = list(aes(linetype = model))) +
  scale_y_discrete(labels = c(AcciPost = 'Treatment group', age = 'Age', male = 'Male dummy', educMinimum_education = 'Minimum education',
                              educSecondary_education = 'Secondary education', educTertiary_education = 'Tertiary education', inc = 'Income')) +
  theme_bw() + xlab(" ") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("A) Full sample, N=1094") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1, 3)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans")) +
  guides(col=guide_legend(nrow=1,byrow=TRUE))

legend <- get_legend(plotfull3)
plotfull3 <- plotfull3 + theme(legend.position = 0)

plotfull2 <- dwplot(list(m_StVO2, m_concern2, m_test2), show_intercept = FALSE, dot_args = list(aes(shape = model)),
                    whisker_args = list(aes(linetype = model))) +
  scale_y_discrete(labels = c(AcciPost = 'Treatment group', age = 'Age', male = 'Male dummy', educMinimum_education = 'Minimum education',
                              educSecondary_education = 'Secondary education', educTertiary_education = 'Tertiary education', inc = 'Income')) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("B) ± 8.8 days, N=569") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1, 3)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans"), legend.position = "none")

plotfull <- dwplot(list(m_StVO, m_concern, m_test), show_intercept = FALSE, dot_args = list(aes(shape = model)),
                   whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab(" ") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("C) ± 4.4 days, N=151") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_colour_manual(name="Dependent Variable",
                      breaks=c("Model 1", "Model 2", "Model 3"),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  coord_cartesian(xlim = c(-1, 3)) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(), axis.text.y = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans"), legend.position = "none")

jointUESD <- ggdraw() +
  draw_plot(plotfull3, x = 0, y = 0.1, width = 0.45, height = 0.9) +
  draw_plot(plotfull2, x = 0.45, y = 0.1, width = 0.275, height = 0.9) +
  draw_plot(plotfull, x = 0.725, y = 0.1, width = 0.275, height = 0.9) +
  draw_plot(legend, x = 0.1, y = 0.02, width = 1, height = 0.1)
jointUESD

ggsave(filename = "./plots/jointUESD.png", jointUESD, width = 13, height = 8)

#interaction
inter <- list(m_StVO4, m_concern4, m_test4)
inter$dv <- factor()

plotfull4 <- dwplot(list(m_StVO4, m_concern4, m_test4), show_intercept = TRUE, dot_args = list(aes(shape = model)),
                    whisker_args = list(aes(linetype = model))) +
  scale_y_discrete(labels = c(AcciPost = 'Treatment group', cutoff = 'Days', interaction = 'Treatment*Days')) +
  theme_bw() + xlab(" ") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Full sample with interaction, N=1094") +
  theme(plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.position = "bottom") +
  scale_color_manual(name="Dependent Variable",
                     breaks = c('Model 1', 'Model 2', 'Model 3'),
                      labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support"),
                      values=c("#cc79a7","#0072b2","#009e73")) +
  scale_shape_discrete(name="Dependent Variable",
                       breaks=c("Model 1", "Model 2", "Model 3"),
                       labels=c("Legal adaptation support", "Confidence in self-driving vehicles", "Self-driving vehicle test support")) +
  scale_x_discrete(limits=c(-1, 0, 1, 2, 3, 4)) +
  theme(legend.title=element_text(size=14, family="Sans"), axis.text=element_text(size=14, family="Sans"), axis.title=element_text(size=14, family="Sans"), 
        axis.text.x = element_text(family="Sans"), panel.grid.minor = element_blank(),
        legend.text = element_text(size=14, family="Sans"), plot.title = element_text(size=20, family="Sans"))

ggsave(filename = "./plots/interactionplot.png", plotfull4, width = 11.5, height = 11.5)



#tables

table_difference3 <- stargazer(m_concern3, m_StVO3, m_test3,
                              type="html", 
                              single.row = TRUE,
                              title="Table A: Effect of Uber accident on attitudes towards self-driving vehicles, full sample", 
                              align=TRUE,
                              star.char = c("+", "*", "**", "***"),
                              star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                              digits = 3,
                              dep.var.labels = c("Confidence in self-driving vehicles", "Legal adaptation support", "Self-driving vehicle test support"),
                              order=c(1,2,3,4,5,6,7,8,9),
                              covariate.labels = c("Treatment group", "Age", "Male dummy",
                                                   "Minimum education", "Secondary education",
                                                   "Tertiary education", "Income", "Intercept"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs/DVs3.html')

table_difference2 <- stargazer(m_concern2, m_StVO2, m_test2,
                              type="html", 
                              single.row = TRUE,
                              title="Table A: Effect of Uber accident on attitudes towards self-driving vehicles, ± 8.8 days", 
                              align=TRUE,
                              star.char = c("+", "*", "**", "***"),
                              star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                              digits = 3,
                              dep.var.labels = c("Confidence in self-driving vehicles", "Legal adaptation support", "Self-driving vehicle test support"),
                              order=c(1,2,3,4,5,6,7,8,9),
                              covariate.labels = c("Treatment group", "Age", "Male dummy",
                                                   "Minimum education", "Secondary education",
                                                   "Tertiary education", "Income", "Intercept"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs/DVs2.html')

table_difference <- stargazer(m_concern, m_StVO, m_test,
                               type="html", 
                               single.row = TRUE,
                               title="Table A: Effect of Uber accident on attitudes towards self-driving vehicles, ± 4.4 days", 
                               align=TRUE,
                               star.char = c("+", "*", "**", "***"),
                               star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                               digits = 3,
                               dep.var.labels = c("Confidence in self-driving vehicles", "Legal adaptation support", "Self-driving vehicle test support"),
                               order=c(1,2,3,4,5,6,7,8,9),
                              covariate.labels = c("Treatment group", "Age", "Male dummy",
                                                   "Minimum education", "Secondary education",
                                                   "Tertiary education", "Income", "Intercept"),
                               object.names = FALSE,
                               notes.append = FALSE, notes.align = "l",
                               notes = "***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                               out='tabs/DVs.html')


table_difference <- stargazer(m_sysSc, m_rel, m_hack, m_liability,
                              type="html", 
                              single.row = TRUE,
                              title="Table A: Effect of Uber accident on self-driving vehicle concerns 1, ± 4.4 days", 
                              align=TRUE,
                              star.char = c("+", "*", "**", "***"),
                              star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                              digits = 3,
                              dep.var.labels = c("System safety concern", "System reliability concern", "Software misuse concern", "Liability concern"),
                              order=c(1,2,3,4,5,6,7,8,9),
                              covariate.labels = c("Treatment group", "Age", "Male dummy",
                                                   "Minimum education", "Secondary education",
                                                   "Tertiary education", "Income", "Intercept"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs/Concern_sig2.html')

table_difference <- stargazer(m_enj, m_ctr, m_job, m_priv,
                              type="html", 
                              single.row = TRUE,
                              title="Table A: Effect of Uber accident on self-driving vehicle concerns 2, ± 4.4 days", 
                              align=TRUE,
                              star.char = c("+", "*", "**", "***"),
                              star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                              digits = 3,
                              dep.var.labels = c("Driver control loss concern", "Loss of driving enjoyment", "Job loss concern", "Privacy concern"),
                              order=c(1,2,3,4,5,6,7,8,9),
                              covariate.labels = c("Treatment group", "Age", "Male dummy",
                                                   "Minimum education", "Secondary education",
                                                   "Tertiary education", "Income", "Intercept"),
                              object.names = FALSE,
                              notes.append = FALSE, notes.align = "l",
                              notes = "***p < 0.001, **p < 0.01, *p < 0.05, +p < 0.1",
                              out='tabs/Concern_nonsig2.html')


# Balance tests -----------------------------------------------------------

df$educ_numeric <- ifelse(df$educ == "NA" | df$educ == "Other_education", NA, as.numeric(df$educ)-1)

bw <- rep(c(2:10), 4)
dv <- rep(c("age", "male", "educ_numeric", "inc"), each=9)
c <- NA
lwr <- NA
upr <- NA

for (i in 1:length(bw)) {
  
  m <- lm(formula(paste0(dv[i], "~ Acci")),  data = df[(df$cutoff>-bw[i] & df$cutoff<bw[i]),], na.action=na.omit)
  
  c[i] <- coef(m)["AcciPost"]
  lwr[i] <- confint(m)["AcciPost", 1]
  upr[i] <- confint(m)["AcciPost", 2]
}

df_balance <- data.frame(dv,bw,c,lwr,upr)

df_balance$dv <- factor(df_balance$dv, levels = c("age", "male", "educ_numeric", "inc"), labels = c("Age", "Gender", "Education", "Income"))

p_balance <- ggplot(df_balance, aes(x=bw, y=c)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  theme_bw() +
  facet_wrap(~dv, ncol=2, scales = "free_y") +
  geom_hline(yintercept = 0, lty = "dashed") +
  labs(y="Effect of the accident", x= "Bandwidth in days")+
  theme(text = element_text(size=20), axis.text.x = element_text(size=16)) 
p_balance

ggsave(filename = "./plots/p_balance.png", p_balance, width = 8, height = 8)


######### joint output #########
joint <- ggdraw() +
  draw_plot(RDD_genConcern, x = 0, y = 0.75, width = 0.333, height = 0.25) +
  draw_plot(RDD_StVOChng, x = 0.333, y = 0.75, width = 0.333, height = 0.25) +
  draw_plot(RDD_testsupport, x = 0.666, y = 0.75, width = 0.333, height = 0.25) +
  draw_plot(RDD_SysSc, x = 0, y = 0.5, width = 0.333, height = 0.25) +
  draw_plot(RDD_SysRl, x = 0.333, y = 0.5, width = 0.333, height = 0.25) +
  draw_plot(RDD_jobLoss, x = 0.666, y = 0.5, width = 0.333, height = 0.25) +
  draw_plot(RDD_privCncrn, x = 0, y = 0.25, width = 0.333, height = 0.25) +
  draw_plot(RDD_drCtr, x = 0.333, y = 0.25, width = 0.333, height = 0.25) +
  draw_plot(RDD_drEnjoy, x = 0.666, y = 0.25, width = 0.333, height = 0.25) +
  draw_plot(RDD_liability, x = 0.167, y = 0, width = 0.333, height = 0.25) +
  draw_plot(RDD_systemMisuse, x = 0.5, y = 0, width = 0.333, height = 0.25)
joint

ggsave(filename = "./plots/RDD_joint.png", joint, width = 21, height = 28)

df$Date2 <- as.Date(as.character(df$date), "%Y-%m-%d")
eightdaysubset <- subset(df, Date2 > as.Date("2018-03-10") )