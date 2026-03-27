
packages <- c("DeclareDesign","tidyverse","kableExtra","sf","magrittr","rio","stargazer","lfe",
              "car","scales","ggthemes","lubridate","survminer","survival","splitstackshape",
              "gridExtra","knitr","modelsummary","wesanderson","janitor","ggprism","sandwich",
              "lmtest","ri2","ggplot2","RColorBrewer","xtable","texreg","dplyr","tidyr","reshape2",
              "Hmisc","estimatr","patchwork","haven","scales","countrycode","schoolmath","Rmisc")

ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}
ipak(packages)

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Import Dataset
df.final <- readRDS("wvs.rds")

regime <- c("Closed Autocracy", "Electoral Autocracy", "Electoral Democracy", "Liberal Democracy")
migrant <- c("Native", "Migrant")

#* destination by regime type, Indian ----
host.india <- ggplot(df.final[df.final$HomeCountry=="IND" & df.final$Q263==2,], aes(v2x_regime_des)) +
  geom_bar(stat = "count") +
  xlab("") +
  ylab("Num. of Immigrants")+
  scale_x_continuous(labels= regime, breaks=c(0,1,2,3))

pdf("host_india.pdf", width=6, height=4)
print(host.india)
dev.off()

#* destination by regime type, Asian ----
df.final$region <- countrycode(df.final$HomeCountry, "iso3c", "region23")

df.final.asia <- df.final %>% filter(grepl('Asia', region))

host.Asia <- ggplot(df.final.asia[df.final.asia$Q263==2,], aes(v2x_regime_des)) +
  geom_bar(stat = "count") +
  xlab("Regime Type") +
  ylab("Num. of Immigrants")+
  scale_x_continuous(labels= regime, breaks=c(0,1,2,3))

pdf("host_Asia.pdf", width=6, height=4)
print(host.Asia)
dev.off()

#* inter-group tolerance (native vs. migrant) ----
# Q19 (race), Q21 (foreign), Q23 (religion), Q26 (language)
# Could you please mention any that you would not like to have as neighbors?

df.final$Q19 <- replace(df.final$Q19, df.final$Q19 < 0, NA) 
df.final$Q21 <- replace(df.final$Q21, df.final$Q21 < 0, NA) 
df.final$Q23 <- replace(df.final$Q23, df.final$Q23 < 0, NA) 
df.final$Q26 <- replace(df.final$Q26, df.final$Q26 < 0, NA) 

df.final$tolerance <- (df.final$Q19+df.final$Q21+df.final$Q23+df.final$Q26)/4

df.final$Q263 <- replace(df.final$Q263, df.final$Q263 < 0, NA)

tolerance <- summarySE(df.final, measurevar="tolerance", groupvars="Q263", na.rm = TRUE)

tolerance$Q263 <- factor(tolerance$Q263)
tolerance <- tolerance %>% na.omit()

tolerance.migrant <- ggplot(tolerance, aes(x=Q263, y=tolerance)) +
                    geom_errorbar(aes(ymin=tolerance-ci, ymax=tolerance+ci), width=.1) +
                    geom_point()+
                    ylab("Interpersonal Tolerance") +
                    xlab("")+
                    scale_x_discrete(labels= migrant, breaks=c(1,2))

pdf("tolerance_migrant.pdf", width=6, height=4)
print(tolerance.migrant)
dev.off()

#* inter-group trust (native vs. migrant) ----
# Q61 (meet first-time), Q62 (religion), Q63 (nationality)
# 1 - Trust completely, 4 - Do not trust at all

df.final$Q61 <- replace(df.final$Q61, df.final$Q61 < 0, NA)
df.final$Q61 <- 5-df.final$Q61
df.final$Q62 <- replace(df.final$Q62, df.final$Q62 < 0, NA)
df.final$Q62 <- 5-df.final$Q62
df.final$Q63 <- replace(df.final$Q63, df.final$Q63 < 0, NA)
df.final$Q63 <- 5-df.final$Q63

df.final$trust <- (df.final$Q61+df.final$Q62+df.final$Q63)/3

df.final$Q263 <- replace(df.final$Q263, df.final$Q263 < 0, NA)

trust <- summarySE(df.final, measurevar="trust", groupvars="Q263", na.rm = TRUE)

trust$Q263 <- factor(trust$Q263)
trust <- trust %>% na.omit()

trust.migrant <- ggplot(trust, aes(x=Q263, y=trust)) +
  geom_errorbar(aes(ymin=trust-ci, ymax=trust+ci), width=.1) +
  geom_point()+
  ylab("Interpersonal Trust") +
  xlab("")+
  scale_x_discrete(labels= migrant, breaks=c(1,2))

pdf("trust_migrant.pdf", width=6, height=4)
print(trust.migrant)
dev.off()

#* inter-group tolerance (migrant in dem vs. aut) ----
tolerance_2 <- summarySE(df.final[df.final$Q263==2,], measurevar="tolerance", groupvars="v2x_regime_des", na.rm = TRUE)

tolerance_2$v2x_regime_des <- factor(tolerance_2$v2x_regime_des)
tolerance_2 <- tolerance_2 %>% na.omit()

tolerance.regime <- ggplot(tolerance_2, aes(x=v2x_regime_des, y=tolerance)) +
  geom_errorbar(aes(ymin=tolerance-ci, ymax=tolerance+ci), width=.1) +
  geom_point()+
  ylab("Interpersonal Tolerance") +
  xlab("")+
  scale_x_discrete(labels= regime, breaks=c(0,1,2,3))

pdf("tolerance_regime.pdf", width=6, height=4)
print(tolerance.regime)
dev.off()

#* inter-group trust (migrant in dem vs. aut) ----
trust_2 <- summarySE(df.final[df.final$Q263==2,], measurevar="trust", groupvars="v2x_regime_des", na.rm = TRUE)

trust_2$v2x_regime_des <- factor(trust_2$v2x_regime_des)
trust_2 <- trust_2 %>% na.omit()

trust.regime <- ggplot(trust_2, aes(x=v2x_regime_des, y=trust)) +
  geom_errorbar(aes(ymin=trust-ci, ymax=trust+ci), width=.1) +
  geom_point()+
  ylab("Interpersonal Trust") +
  xlab("")+
  scale_x_discrete(labels= regime, breaks=c(0,1,2,3))

pdf("trust_regime.pdf", width=6, height=4)
print(trust.regime)
dev.off()
