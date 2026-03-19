#Embryo quality

##import dataset and use df####

df <- data_P4andembryo

##libraries####
library(Hmisc)
library(tidyverse)
library(ggplot2)
library(ordinal)
library(lme4)
library(lmerTest)
library(emmeans)

##1. descriptive analyses####

###1a. cows####
d.cows <- df %>%
  distinct(uid,.keep_all = TRUE)

count(d.cows)
d.cows %>%
  group_by(study)%>%
  count()

describe(d.cows$lact)
d.cows %>%
  group_by(study)%>%
  summarise(range = range(lact))
  
describe(d.cows$cyclic)
d.cows %>%
  filter(!is.na(cyclic))%>%
  group_by(study, cyclic)%>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

describe(d.cows$mast)
d.cows %>%
  filter(!is.na(mast))%>%
  group_by(study, mast)%>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

describe(d.cows$heatai)
d.cows %>%
  filter(!is.na(heatai))%>%
  group_by(study, heatai)%>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

describe(d.cows$dbovai)
d.cows %>%
  filter(!is.na(dbovai))%>%
  group_by(study, dbovai)%>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

describe(d.cows$bcsfinal)
d.cows %>%
  filter(!is.na(bcsfinal))%>%
  group_by(study)%>%
  summarise(range = range(bcsfinal))

describe(d.cows$folai)
d.cows %>%
  filter(!is.na(folai))%>%
  group_by(study)%>%
  summarise(range = range(folai))

describe(d.cows$p4pgovs)
d.cows %>%
  filter(!is.na(p4pgovs))%>%
  group_by(study)%>%
  summarise(range = range(p4pgovs))

describe(d.cows$e2ai)
d.cows %>%
  filter(!is.na(e2ai))%>%
  group_by(study)%>%
  summarise(range = range(e2ai))

describe(d.cows$p4ai)
d.cows %>%
  filter(!is.na(p4ai))%>%
  group_by(study)%>%
  summarise(range = range(p4ai))

describe(d.cows$p4flush)
d.cows %>%
  filter(!is.na(p4flush))%>%
  group_by(study)%>%
  summarise(range = range(p4flush))

###1b. structures####
count(df)
df %>%
  group_by(study)%>%
  count()

describe(df$grade)
df %>%
  group_by(study, grade)%>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

describe(df$asc)
df %>%
  filter(!is.na(asc))%>%
  group_by(study)%>%
  summarise(range = range(asc))

##2. unconditional models####

#parity (P = 0.02*)
m = clm(grade ~ parity, data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#cyclicity (P = 0.82)
m = clm(grade ~ cyclic, data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#mastitis (P = 0.32)
m = clm(grade ~ mast, data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#estrus at ai (P = 0.60)
m = clm(grade ~ heatai, 
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#double ovulation (P = 0.24)
m = clm(grade ~ dbovai, 
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#bcs (P = 0.88)
m = clm(grade ~ bcsfinal_cat, scale = ~bcsfinal_cat,
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#ovulatory follicle (P = 0.95)
m = clm(grade ~ folai, 
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#P4 at pgf2a (P = 0.56)
m = clm(grade ~ p4pgovs_cat, 
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#E2 at ai (P = 0.67)
m = clm(grade ~ e2ai_cat, scale = ~e2ai_cat,
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#P4 at ai (P < 0.01**)
m = clm(grade ~ p4ai_cat, 
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#P4 at flush (P = 0.28)
m = clm(grade ~ p4flush_cat, 
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

#accessory spermatozoa (P < 0.01***)
m = clm(grade ~ asc_cat, scale = ~asc_cat,
        data=df, Hess=T, nAGQ=10)
nominal_test(m)
summary(m)

##3. multivariable models####

#parity
m0 = clmm(grade ~ 1  + (1|trt), data=df, Hess=T, nAGQ=10)
m = clmm(grade ~ parity + (1|trt), data=df, Hess=T, nAGQ=10)
summary(m)

emmeans_Fig1 <- emmeans(m, ~ grade | parity, mode = "prob")

d1 <- data.frame(emmeans_Fig1)
d1$parity <- as.factor(d1$parity)
b <- ggplot(d1, aes(x = grade, y = prob, ymin = prob-SE, ymax = prob+SE, color = parity, shape = parity)) + 
  scale_y_continuous(n.breaks = 6, limits = c(0, 0.5)) +
  theme_classic() +
  geom_point( position = position_dodge(width = 0.3), size = 3 )  +
  geom_errorbar(position = position_dodge(width = 0.3), size = 1.1, width = 0.3 )  +
  #geom_pointrange( position = position_dodge(width = 0.3) )  +
  scale_color_manual(values = c("navyblue", "darkorange"), name = "Parity", 
                     labels = c("Primiparous", "Multiparous")) +
  scale_shape_manual(values = c(16, 17), name = "Parity", 
                     labels = c("Primiparous", "Multiparous")) +
  xlab("Embryo grade") +
  ylab("Probability distribution") +
  theme(legend.position = "bottom", text=element_text(family="Times",size=10)) 

jpeg("Figure1.jpeg", width = 8.9, height = 8.9, units = 'cm', res = 300)
b
dev.off()

#P4 at ai
m0 = clmm(grade ~ parity + (1|trt), data=df[!is.na(df$p4ai_cat),], Hess=T, nAGQ=10)
m = clmm(grade ~ parity  + p4ai_cat + (1|trt), data=df, Hess=T, nAGQ=10)
summary(m)

emmeans_Fig2 <- emmeans(m, ~ grade | p4ai_cat, mode = "prob")
d2 <- data.frame(emmeans_Fig2)
d2$p4ai_cat <- factor(d2$p4ai_cat,
                      levels = c("low","high"))
a <- ggplot(d2, aes(grade, prob, ymin = prob-SE, ymax = prob+SE, color = p4ai_cat, shape = p4ai_cat)) + 
  scale_y_continuous(n.breaks = 6, limits = c(0, 0.5)) +
  theme_classic() +
  geom_point( position = position_dodge(width = 0.3), size = 3 )  +
  geom_errorbar(position = position_dodge(width = 0.3), size = 1.1, width = 0.3 )  +
  #geom_pointrange( position = position_dodge(width = 0.3) )  +
  scale_color_manual(values = c("navyblue", "darkorange"), name = "Progesterone", 
                     labels = c("< 0.5 ng/mL", "\u2265 0.5 ng/mL")) +
  scale_shape_manual(values = c(16, 17), name = "Progesterone", 
                     labels = c("< 0.5 ng/mL", "\u2265 0.5 ng/mL")) +
  xlab("Embryo grade") +
  ylab("Probability distribution") +
  theme(legend.position = "bottom", text=element_text(family="Times",size=10)) 

jpeg("Figure2.jpeg", width = 8.9, height = 8.9, units = 'cm', res = 300)
a
dev.off()

#accessory spermatozoids
m0 = clm(grade ~ parity  + p4ai_cat + trt, data=df)
m = clm(grade ~ parity  + p4ai_cat + asc_cat + trt,scale = ~asc_cat, data=df)
summary(m)

emmeans_Fig3 <- emmeans(m, ~ grade | asc_cat, mode = "prob")

d3 <- data.frame(emmeans_Fig3)
d3$asc_cat <- factor(d3$asc_cat,
                     levels = c("low","high"))

c <- ggplot(d3, aes(grade, prob, ymin = prob-SE, ymax = prob+SE, color = asc_cat, shape = asc_cat)) + 
  scale_y_continuous(n.breaks = 6, limits = c(0, 0.5)) +
  theme_classic() +
  geom_point( position = position_dodge(width = 0.3), size = 3 )  +
  geom_errorbar(position = position_dodge(width = 0.3), size = 1.1, width = 0.3 )  +
  #geom_pointrange( position = position_dodge(width = 0.3) )  +
  scale_color_manual(values = c("navyblue", "darkorange"), name = "Accessory \n spermatozoa", 
                     labels = c("7 and less", "More than 7")) +
  scale_shape_manual(values = c(16, 17), name = "Accessory \n spermatozoa", 
                     labels = c("7 and less", "More than 7")) +
  xlab("Embryo grade") +
  ylab("Probability distribution") +
  theme(legend.position = "bottom", text=element_text(family="Times",size=10)) 


jpeg("Figure3.jpeg", width = 8.9, height = 8.9, units = 'cm', res = 300)
c
dev.off()