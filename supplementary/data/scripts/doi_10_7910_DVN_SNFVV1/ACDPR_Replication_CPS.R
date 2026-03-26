### Replication code for Arriola, Choi, Davis, Phillips, and Rakner:
### Policymakers' Abortion Preferences: Understanding the Intersection of Gender and Wealth
### Accepted for Publication at Comparative Political Studies

### The following code reproduces all Figures and Tables in the main text and 
### Appendix except for Table 1, Table 3, and Figure 1
### To reproduce Figure 1, run Figure1.R

rm(list=ls())

options(scipen=999)

library(readstata13)
library(stargazer)
library(estimatr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(patchwork)
library(interplot)
library(ri2)
library(DeclareDesign)
library(estimatr)
library(randomizr)
library(tidyverse)
library(ggpubr)
library(patchwork)
library(purrr)
library(xtable)


### Read Data File ####
data <- readRDS(file = "data_final.rds")

### Main Text Figures/Tables ####

#### Table 1: Not generated from main data set ####
#### Table 2: Descriptive Statistics ####
vars <- c("woman", "married", "catholic","mainline","evangelical","bemba",
          "lala","lozi","ngoni","nyanja","tonga",
          "ruling","assetindex","mp","college",
          "womenorg","agenum", "electexp")
vars2 <- c("Woman","Married", "Catholic","Christian","Evangelical",
           "Bemba","Lala", "Lozi", "Ngoni","Nyanja","Tonga",
           "Ruling party", "Asset index", "Parliamentary candidate","College",
           "Women's organization","Age", "Prior electoral experience")
require(pastecs)
sum_stats <- t(stat.desc(data[vars]))

sum_stats <- as.data.frame(sum_stats[,c("mean", "min", "max", "std.dev", "nbr.val")])
sum_stats$des <- vars2 # labels
sum_stats <- sum_stats[c(6,1,2,3,4,5)]
colnames(sum_stats) <- c("", "mean", "min", "max", "std.dev", "n")

require(xtable)
print(xtable(sum_stats, digits=c(0,0,2,0,0,2,0), caption="Descriptive Statistics", label="sum_stats", align="llccccc"),
      include.rownames = F, table.placement="H", caption.placement = "top", file="sum_stats.tex")

#### Table 3: Treatment Script ####
# Not generated from main data set


#### Figure 2: Preference on Abortion Policy Change ####
data$abortion_leg_rescale <- NA
#changing mroe restrictive to 0
data$abortion_leg_rescale[data$abortion_leg==1] <- 0
#changing stay the same to 1
data$abortion_leg_rescale[data$abortion_leg==0] <- 1
#changing less restirctive to 2
data$abortion_leg_rescale[data$abortion_leg==-1] <- 2

data$abortomni2 <- NA
data$abortomni2[data$abortion_leg_rescale==2] <- "Less Restrictive"
data$abortomni2[data$abortion_leg_rescale==1] <- "Stay the Same"
data$abortomni2[data$abortion_leg_rescale==0] <- "More Restrictive"

data2 <- subset(data, !is.na(abortomni2))

# subset data for Figure 2
# summarizing data for analysis
abortomni2 <- data2 %>%
  group_by(offgen, abortomni2) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  mutate(woman = if_else(offgen==2 | offgen==4, 1, 0),
         office = if_else(offgen==1 | offgen==2, "Parliamentary Candidates", "Ward Councilor Candidates"),
         label = paste0(sprintf("%.0f", freq*100), "%"),
         offgen = as.numeric(offgen))

abortomni2a <- data2 %>%
  group_by(woman, abortomni2) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  ungroup() %>%
  mutate(office = "All Candidates",
         label = paste0(sprintf("%.0f", freq*100), "%"),
         offgen = if_else(woman==0, 5, 6)) %>%
  dplyr::select(offgen, abortomni2, n, freq, woman, office, label)

abort_pct <- rbind(abortomni2, abortomni2a)

abort_pct$abortxg <- paste0(abort_pct$abortomni2, abort_pct$woman)
abort_pct$abortxg <- factor(abort_pct$abortxg, levels = c("Less Restrictive0", "Stay the Same0", "More Restrictive0",
                                                             "Less Restrictive1", "Stay the Same1", "More Restrictive1"))
abort_pct$abortomni2 <- factor(abort_pct$abortomni2, levels = c("Less Restrictive", "Stay the Same", "More Restrictive"))
abort_pct$woman_rev <- 1 - abort_pct$woman

# To place value labels later
abort_pct$alpha <- ifelse(abort_pct$abortxg=="Less Restrictive0", 1.8, NA)
abort_pct$alpha <- ifelse(abort_pct$abortxg=="Stay the Same0", 2.0, abort_pct$alpha)
abort_pct$alpha <- ifelse(abort_pct$abortxg=="More Restrictive0", 2.2, abort_pct$alpha)
abort_pct$alpha <- ifelse(abort_pct$abortxg=="Less Restrictive1", 0.8, abort_pct$alpha)
abort_pct$alpha <- ifelse(abort_pct$abortxg=="Stay the Same1", 1.0, abort_pct$alpha)
abort_pct$alpha <- ifelse(abort_pct$abortxg=="More Restrictive1", 1.2, abort_pct$alpha)

ggplot(abort_pct) +
    geom_bar(aes(x = factor(woman_rev), y = freq*100, fill = factor(woman_rev), alpha= factor(abortomni2)), position="dodge", stat = "identity", width = .5) +
    facet_grid(.~office) +
    scale_alpha_manual(name = "Preference for Abortion Policy",
                       values = c(0.3, 0.6, 1.0)) +
    ylab("Percentage (%)") +
    xlab("Candidate Gender") +
    theme_bw() +
    scale_fill_manual(name   = "Candidate Gender",
                    breaks = c("0","1"),
                    labels = c("Women", "Men"),
                    values = c("#ef5350", "#187bcd"),
                    guide = "none") +
    scale_x_discrete(labels=c("0" = "Women", 
                              "1" = "Men")) +
    theme(legend.text = element_text(size=10),
          legend.title = element_text(size=10),
          legend.position = "bottom") +
    geom_text(aes(x=alpha, y=freq*100, label = label), vjust=-0.5, size = 3)


ggsave("abortion_leg_final.pdf", width=8, height=5)

#### Table 4: Existing Abortion Policy Change Preference: Ordered Logit ####
# recode so that higher values are less restrictive
data$abortion_leg_inv <- -1*data$abortion_leg


data$abortion_leg_fact_inv  <- as.factor(data$abortion_leg_inv)

m1 <- polr(abortion_leg_fact_inv ~  woman + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp +  assetindex + college + womenorg, data=data, Hess = T)
m2 <- polr(abortion_leg_fact_inv ~  married + mainline + evangelical + catholic + agenum + ruling + mp + electexp +  assetindex + college + womenorg, data=subset(data, woman==1), Hess = T)
m3 <- polr(abortion_leg_fact_inv ~  married + mainline + evangelical + catholic + agenum + ruling + mp + electexp +  assetindex + college + womenorg, data=subset(data, woman==0), Hess = T)

stargazer(m1, m2, m3,
          type="text",
          column.labels = c("Full Sample", "Women","Men"),
          dep.var.labels = "",
          no.space=T,
          covariate.labels=c("Woman", "Married", "Mainline Christian", "Evangelical", "Catholic",
                             "Age", "Ruling Party", "MP Candidate", "Prior Election Experience",
                             "Asset Index", "College", "Women's Organization"),
          out="ologit.tex")

# For robustness: OLS
m1 <- lm(abortion_leg_inv ~  woman + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp +  assetindex + college + womenorg, data=data)
m2 <- lm(abortion_leg_inv ~  married + mainline + evangelical + catholic + agenum + ruling + mp + electexp +  assetindex + college + womenorg, data=subset(data, woman==1))
m3 <- lm(abortion_leg_inv ~  married + mainline + evangelical + catholic + agenum + ruling + mp + electexp +  assetindex + college + womenorg, data=subset(data, woman==0))


stargazer(m1, m2, m3,
          type="text",
          column.labels = c("Full Sample", "Women","Men"),
          dep.var.labels = "",
          no.space=T,
          covariate.labels=c("Woman", "Married", "Mainline Christian", "Evangelical", "Catholic",
                             "Age", "Ruling Party", "MP Candidate", "Prior Election Experience",
                             "Asset Index", "College", "Women's Organization"),
          out="ols.tex")

#### Figure 3: Treatment FX by Electoral Tier & Gender  ####
full  <- lm(about ~ treat, data=data)
men   <- lm(about ~ treat, data=subset(data, woman==0))
women <- lm(about ~ treat, data=subset(data, woman==1))

fullmp     <- lm(about~treat, data=subset(data, mp==1))
mpmen      <- lm(about~treat, data=subset(data, mp==1 & woman==0))
mpwomen    <- lm(about~treat, data=subset(data, mp==1 & woman==1))

fullward     <- lm(about~treat, data=subset(data, mp==0))
wardmen    <- lm(about~treat, data=subset(data, mp==0 & woman==0))
wardwomen  <- lm(about~treat, data=subset(data, mp==0 & woman==1))


f1 <- tidy(full)
f3 <- tidy(men)
f2 <- tidy(women)
m1 <- tidy(fullmp)
m3 <- tidy(mpmen)
m2 <- tidy(mpwomen)
w1 <- tidy(fullward)
w3 <- tidy(wardmen)
w2 <- tidy(wardwomen)


beta <-         f1$estimate[2]
beta <- c(beta, f2$estimate[2])
beta <- c(beta, f3$estimate[2])
beta <- c(beta, m1$estimate[2])
beta <- c(beta, m2$estimate[2])
beta <- c(beta, m3$estimate[2])
beta <- c(beta, w1$estimate[2])
beta <- c(beta, w2$estimate[2])
beta <- c(beta, w3$estimate[2])


se <-         f1$std.error[2]
se <- c(se, f2$std.error[2])
se <- c(se, f3$std.error[2])
se <- c(se, m1$std.error[2])
se <- c(se, m2$std.error[2])
se <- c(se, m3$std.error[2])
se <- c(se, w1$std.error[2])
se <- c(se, w2$std.error[2])
se <- c(se, w3$std.error[2])

fxplot <- cbind(beta, se)
fxplot <- as.data.frame(fxplot)

tier <- c("All Candidates", "All Candidates", "All Candidates",
               "Parliamentary Candidates", "Parliamentary Candidates", "Parliamentary Candidates",
               "Ward Councilor Candidates", "Ward Councilor Candidates", "Ward Councilor Candidates")

gender <- c("Full", "Women", "Men",
              "Full", "Women", "Men",
              "Full", "Women", "Men")


fxplot <- cbind(fxplot, tier, gender)
fxplot$gender <- factor(fxplot$gender, levels = c("Full", "Women", "Men"))

fxplot$rb <- sprintf("%0.3f", round(fxplot$beta, digits = 4))
interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)


treatplot <- ggplot(data=fxplot, aes(x=fct_rev(gender), y=beta, shape=factor(gender))) +
  geom_pointrange(aes(y = beta, ymin = beta - se*interval1, ymax = beta + se*interval1),
                   lwd = 1, position = position_dodge(width = 1/2), size=1) +
  geom_pointrange(aes(y = beta, ymin = beta - se*interval2,
                      ymax = beta + se*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2), size=1) +
  geom_text(aes(label = rb), vjust=-0.7, hjust=-0.5) +
  xlab("Electoral Tier") +
  ylab("Treatment Effect Estimate") +
  theme_bw() +
  facet_wrap(~tier, ncol=1) +
  ylim(-2.5, 2.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  scale_shape_manual(name   = "Candidate Gender",
                    labels = c("Full", "Women", "Men"),
                    values = c(15, 16, 17)) +
  theme(axis.text.y= element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x= element_text(size=10)) +
  coord_flip()

print(treatplot)

ggsave("treatfx.pdf", width=8, height=5)


#### Figure 4: Average Responses on the Experimental Outcome ####

tebytype <- data %>%
  group_by(offgen, treat) %>%
  summarise(mean = mean(about, na.rm=T),
            sd   = sd(about, na.rm=T),
            se_min   = mean_se(about)$ymin,
            se_max   = mean_se(about)$ymax) %>%
  filter(!is.na(treat))

tebytype$label <- paste0(sprintf("%.3f", tebytype$mean))
tebytype$ctype <- NA
tebytype$ctype <- ifelse(tebytype$offgen==1, "Men Parliamentary Candidates", tebytype$ctype)
tebytype$ctype <- ifelse(tebytype$offgen==2, "Women Parliamentary Candidates", tebytype$ctype)
tebytype$ctype <- ifelse(tebytype$offgen==3, "Men Ward Councilor Candidates", tebytype$ctype)
tebytype$ctype <- ifelse(tebytype$offgen==4, "Women Ward Councilor Candidates", tebytype$ctype)

tebytype$ctype <- factor(tebytype$ctype, levels = c("Women Parliamentary Candidates",
                                                    "Men Parliamentary Candidates",
                                                    "Women Ward Councilor Candidates",
                                                    "Men Ward Councilor Candidates"))



tebytype$treat <- as.factor(tebytype$treat)

pd <- position_dodge(0.7)
plot  <- ggplot(tebytype, aes(x=treat, y=mean, fill=treat)) +
  geom_bar(position=pd, stat="identity", width=0.5, alpha=0.9) +
  theme_bw() +
  geom_errorbar(aes(y=mean, ymin = se_min, ymax = se_max, width=0.1), position=position_dodge(0.7)) +
  geom_text(aes(label = label), size = 4.0, hjust = -0.3, vjust=-0.7, position=position_dodge(0.7)) +
  geom_segment(aes(x=1, y=5.8, xend=2, yend=5.8), colour="gray") +
  scale_fill_manual(values=c("#e5f5e0", "#35a356"),
                    breaks=c("0", "1"),
                    labels=c("Control", "Treatment"),
                    name="Treatment Group")  +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Control", "Treated")) +
  ylim(0, 6.3) +
  ylab("Mean") +
  xlab("Treatment Group") +
  facet_wrap(.~ ctype, ncol=2)

f_labels <- data.frame(ctype = c("Women Parliamentary Candidates",
                                 "Men Parliamentary Candidates",
                                 "Women Ward Councilor Candidates",
                                 "Men Ward Councilor Candidates"), label_grid = c("", "", "**", ""), x=c(1.5, 1.5, 1.5, 1.5), y=c(6, 6, 6, 6))
f_labels$ctype <- factor(f_labels$ctype, levels = c("Women Parliamentary Candidates",
                                                    "Men Parliamentary Candidates",
                                                    "Women Ward Councilor Candidates",
                                                    "Men Ward Councilor Candidates"))


plot <- plot +  geom_text(data=f_labels, aes(x = x,  y = y, label = label_grid), inherit.aes = FALSE) + theme(legend.position="none")

print(plot)

ggsave("tebytype.pdf", width=8, height=6)


#### Figure 5: Heterogeneity in Treatment FX among Women Ward Candidates ####
tebydv1 <- data %>%
  filter(woman==1 & mp==0) %>%
  group_by(abortion_leg_inv, treat) %>%
  summarise(mean = mean(about),
            sd   = sd(about),
            se_min   = mean_se(about)$ymin,
            se_max   = mean_se(about)$ymax)

tebydv1$label <- paste0(sprintf("%.3f", tebydv1$mean))

pd <- position_dodge(0.7)
ggplot(tebydv1, aes(x=factor(abortion_leg_inv), y=mean, fill=factor(treat))) +
  geom_bar(position=pd, stat="identity", width=0.5, alpha=0.9) +
  theme_minimal() +
  geom_errorbar(aes(y=mean, ymin = se_min, ymax = se_max, width=0.1), position=position_dodge(0.7)) +
  geom_text(aes(label = label), size = 4.0, hjust = -0.3, vjust=-0.7, position=position_dodge(0.7)) +
  scale_fill_manual(values=c("#e5f5e0", "#35a356"),
                    breaks=c("0", "1"),
                    labels=c("Control", "Treatment"),
                    name="Treatment Group") +
  scale_x_discrete(breaks=c("-1","0","1"),
                   labels=c("More Restrictive", "Stay the Same", "Less Restrictive")) +
  geom_segment(aes(x=0.80, y=7.1, xend=1.20, yend=7.1), colour="gray") +
  geom_segment(aes(x=1.80, y=7.1, xend=2.20, yend=7.1), colour="gray") +
  geom_segment(aes(x=2.80, y=7.1, xend=3.20, yend=7.1), colour="gray") +
  annotate("text", x=1, y=7.2, label="***") +
  annotate("text", x=2, y=7.2, label="**") +
  annotate("text", x=3, y=7.2, label="") +
  ylim(0, 7.5) +
  ylab("") +
  xlab("Pre-treatment Preference for Abortion Policy Change")

ggsave("treatfx_het_bydv1.pdf", width=8, height=4)


#### Figure 6: Asset Index ####

data$womenpol[data$mp==1 & data$woman==1] <- 2
data$womenpol[data$mp==0 & data$woman==1] <- 1

asseti<- data %>%
  group_by(womenpol) %>%
  dplyr::summarise(mean=mean(assetindex, na.rm=TRUE),
                   sd=sd(assetindex, na.rm=TRUE),
                   n=n(),
                   se=sd/sqrt(n))

asseti <- asseti[which(!is.na(asseti$womenpol)),]

asseti$label <- round(asseti$mean, 3)

interval1 <- -qnorm((1-0.95)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.99)/2)  # 95% multiplier


pd <- position_dodge(0.1)
assetplot <- ggplot(asseti, aes(x=factor(womenpol), y=mean, fill=factor(womenpol))) +
  geom_bar(position=pd, stat="identity", width=0.45) +
  geom_errorbar(aes(ymin = mean - se*interval1, ymax = mean+ se*interval1),
                width = 0.1, position = position_dodge(width = 1/2)) +
  theme_minimal()+
  theme(plot.title = element_text(size = (15)),
        legend.position ="none",
        panel.border = element_blank(),
        legend.text = element_text(),
        axis.title = element_text(size = (12)),
        axis.text = element_text(color = "Black", size = (12)))+
  scale_x_discrete(breaks=c("1", "2"),
                   labels=c("Women Ward Councilor\nCandidates", "Women Parliamentary\nCandidates")) +
  geom_segment(aes(x=1, y=.98, xend=2, yend=.98), colour="darkgray") +
  geom_text(aes(label = label), size = 4.0, hjust = -0.3, vjust=-0.7, position=position_dodge(0.1)) +
  annotate("text", x=1.5, y=1, label="***", size=4) +
  labs(y = "Average Asset Index Holdings", x="") +
  scale_fill_manual(values=c("lightgray", "darkgray"))
print(assetplot)

ggsave("assetplot.pdf", width=5, height=5)


#### Figure 7: Individual Assets ####

assethouse <- data %>%
  group_by(womenpol) %>%
  summarise(mean=mean(house, na.rm=TRUE),
            sd=sd(house, na.rm=TRUE),
            n=n(),
            se=sd/sqrt(n))
assethouse <- assethouse[which(!is.na(assethouse$womenpol)),]
assethouse$assettype <- "House"

assetland <- data %>%
  group_by(womenpol) %>%
  summarise(mean=mean(land, na.rm=TRUE),
            sd=sd(land, na.rm=TRUE),
            n=n(),
            se=sd/sqrt(n))
assetland <- assetland[which(!is.na(assetland$womenpol)),]
assetland$assettype <- "Land"

assetvehicle <- data %>%
  group_by(womenpol) %>%
  summarise(mean=mean(vehicle, na.rm=TRUE),
            sd=sd(vehicle, na.rm=TRUE),
            n=n(),
            se=sd/sqrt(n))
assetvehicle <- assetvehicle[which(!is.na(assetvehicle$womenpol)),]
assetvehicle$assettype <- "Vehicle"

assetcommprop <- data %>%
  group_by(womenpol) %>%
  summarise(mean=mean(commprop, na.rm=TRUE),
            sd=sd(commprop, na.rm=TRUE),
            n=n(),
            se=sd/sqrt(n))
assetcommprop <- assetcommprop[which(!is.na(assetcommprop$womenpol)),]
assetcommprop$assettype <- "Commercial Property"

assetbuesiness <- data %>%
  group_by(womenpol) %>%
  summarise(mean=mean(business, na.rm=TRUE),
            sd=sd(business, na.rm=TRUE),
            n=n(),
            se=sd/sqrt(n))
assetbuesiness <- assetbuesiness[which(!is.na(assetbuesiness$womenpol)),]
assetbuesiness$assettype <- "Business"

assetfarm <- data %>%
  group_by(womenpol) %>%
  summarise(mean=mean(farm, na.rm=TRUE),
            sd=sd(farm, na.rm=TRUE),
            n=n(),
            se=sd/sqrt(n))
assetfarm <- assetfarm[which(!is.na(assetfarm$womenpol)),]
assetfarm$assettype <- "Farm"

assets <- rbind(assetfarm,assetbuesiness,assetcommprop, assethouse, assetland, assetvehicle)

assetsdtf1 <- rbind(assetbuesiness, assetcommprop, assetland)

assetsdtf2 <- rbind(assetfarm, assethouse, assetvehicle)

assetsdtf1$label <- round(assetsdtf1$mean, 3)
assetsdtf2$label <- round(assetsdtf2$mean, 3)


# Subplot 1

assetssplit1 <-ggplot(assetsdtf1, aes(x = interaction(womenpol, assettype), y = mean, fill = factor(womenpol))) +
  geom_col(width=.75,
           position=position_dodge(0.5))+
  geom_errorbar(aes(ymin = mean - se*interval1, ymax = mean+ se*interval1),
                width = 0.1, position = position_dodge(width = 1/2)) +
  geom_text(aes(label = label), size = 3.0, hjust = -0.3, vjust=-0.4, position=position_dodge(0.1)) +
  geom_segment(aes(x=1, y=1.07, xend=2, yend=1.07), colour="gray") +
  annotate("text", x=1.5, y=1.09, label="**", size=4) +
  geom_segment(aes(x=3, y=1.07, xend=4, yend=1.07), colour="gray") +
  annotate("text", x=3.5, y=1.09, label="**", size=4) +
  geom_segment(aes(x=5, y=1.07, xend=6, yend=1.07), colour="gray") +
  annotate("text", x=5.5, y=1.09, label="***", size=4) +
  scale_fill_manual(values=c("lightgray", "darkgray"),
                    name=" ",
                    labels=c("Women Ward Councilor\nCandidates", "Women Parliamentary\nCandidates")) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))+
  theme(axis.text.x=element_text(colour = "black", size=12),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(0, 1, 0.2)) +
  xlab("") +
  ylab("Proportion Owning")

assetssplit1<- assetssplit1 + scale_x_discrete(labels=c("1.Business" = "Business",
                                                        "2.Business" = " ",
                                                        "1.Commercial Property"= "Commercial",
                                                        "2.Commercial Property"= " ",
                                                        "1.Land" = "Land",
                                                        "2.Land" = " " ))

assetssplit1 <- assetssplit1 + theme(axis.text.x = element_text(hjust=-.25))

# Subplot 2

assetssplit2 <-ggplot(assetsdtf2, aes(x = interaction(womenpol, assettype), y = mean, fill = factor(womenpol))) +
  geom_col(width=.75,
           position=position_dodge(0.5))+
  geom_errorbar(aes(ymin = mean - se*interval1, ymax = mean+ se*interval1),
                width = 0.1, position = position_dodge(width = 1/2)) +
  geom_text(aes(label = label), size = 3.0, hjust = -0.3, vjust=-0.4, position=position_dodge(0.1)) +
  geom_segment(aes(x=1, y=1.07, xend=2, yend=1.07), colour="gray") +
  annotate("text", x=1.5, y=1.09, label="**", size=4) +
  geom_segment(aes(x=3, y=1.07, xend=4, yend=1.07), colour="gray") +
  annotate("text", x=3.5, y=1.09, label="", size=4) +
  geom_segment(aes(x=5, y=1.07, xend=6, yend=1.07), colour="gray") +
  annotate("text", x=5.5, y=1.09, label="**", size=4) +
  scale_fill_manual(values=c("lightgray", "darkgray"),
                    name=" ",
                    labels=c("Women Ward Councilor\nCandidates", "Women Parliamentary\nCandidates")) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))+
  theme(axis.text.x=element_text(colour = "black", size=12),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(0, 1, 0.2)) +
  xlab("") +
  ylab("Proportion Owning")

assetssplit2<- assetssplit2 + scale_x_discrete(labels=c( "1.Farm" = "Farm",
                                                         "2.Farm" = " ",
                                                         "1.House" = "House",
                                                         "2.House" = " ",
                                                         "1.Vehicle" = "Vehicle",
                                                         "2.Vehicle" = " "))

assetssplit2 <- assetssplit2 + theme(axis.text.x = element_text(hjust=-.25))

figure <- assetssplit1 + assetssplit2 + plot_layout(nrow=2, guides = "collect") & theme(legend.position = 'bottom')
print(figure)

ggsave("assetplot_bytype.pdf", width=8, height=6)

#### Figure 8: Treatment x Wealth Interaction: Men vs Women ####
fm2a <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + college + womenorg, data=subset(data, woman==1))
fm3a <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + college + womenorg, data=subset(data, woman==0))

set.seed(303)
aintdat <- interplot(m=fm3a, var1="treat", var2="assetindex", point=T, plot=F)
aintdat$lev <- c("0/6", "1/6", "2/6", "3/6", "4/6", "5/6", "6/6")
set.seed(303)
aintdat1 <- interplot(m=fm2a, var1="treat", var2="assetindex", point=T, plot=F)
aintdat1$lev <- c("0/6", "1/6", "2/6", "3/6", "4/6", "5/6", "6/6")


aintm <- ggplot(data=aintdat, aes(x=lev, y=coef)) +
  geom_point(size=3, alpha=0.8) +
  geom_linerange(aes(y=coef, ymin = lb, ymax = ub), linewidth=0.3) +
  xlab("Asset Index") +
  ylab("Estimated Treatment Effects") +
  geom_hline(yintercept = 0, linetype = "dashed", color="red") +
  ylim(-2, 6) +
  theme_minimal() +
  ggtitle("(a) Men Candidates")

aintf <- ggplot(data=aintdat1, aes(x=lev, y=coef)) +
  geom_point(size=3, alpha=0.8) +
  geom_linerange(aes(y=coef, ymin = lb, ymax = ub), linewidth=0.3) +
  xlab("Asset Index") +
  ylab("Estimated Treatment Effects") +
  geom_hline(yintercept = 0, linetype = "dashed", color="red") +
  ylim(-2, 6) +
  theme_minimal() +
  ggtitle("(b) Women Candidates")

aintm + aintf
ggsave("assetintmf.pdf", width=8, height=4)

#### Figure 9: Treatment x Wealth Interactions, Women Candidates Only ####
fm6a <- lm(about ~  treat*assetindex, data=subset(data, woman==1 & mp==1))
fm6b <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + electexp + college + womenorg, data=subset(data, woman==1 & mp==1))

fm7a <- lm(about ~  treat*assetindex, data=subset(data, woman==1 & mp==0))
fm7b <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + electexp + college + womenorg, data=subset(data, woman==1 & mp==0))

stargazer(fm6a, fm6b, fm7a, fm7b,
          type="text",
          column.separate = c(2, 2),
          column.labels = c("Women MP Candidates","Women Ward Candidates"),
          keep=c("treat", "treat:", "Constant"),
          dep.var.labels = "",
          no.space=T,
          # covariate.labels=c("Treatment", "Prior Abortion Preference", "Married", "Mainline Christian", "Evangelical", "Catholic",
          #                    "Age", "Ruling Party", "MP Candidate", "Prior Election Experience",
          #                    "Asset Index", "College", "Women's Organization"),
          out="tables/te_asset_women.tex")

set.seed(303)
aintdat <- interplot(m=fm6b, var1="treat", var2="assetindex", point=T, plot=F)
aintdat$lev <- c("2/6", "3/6", "4/6", "5/6", "6/6")
set.seed(303)
aintdat1 <- interplot(m=fm7b, var1="treat", var2="assetindex", point=T, plot=F)
aintdat1$lev <- c("0/6", "1/6", "2/6", "3/6", "4/6", "5/6", "6/6")


aintm <- ggplot(data=aintdat, aes(x=lev, y=coef)) +
  geom_point(size=3, alpha=0.8) +
  geom_linerange(aes(y=coef, ymin = lb, ymax = ub), linewidth=0.3) +
  xlab("Asset Index") +
  ylab("Estimated Treatment Effects") +
  geom_hline(yintercept = 0, linetype = "dashed", color="red") +
  ylim(-6, 8) +
  theme_minimal() +
  ggtitle("(a) Women MP Candidates")

aintf <- ggplot(data=aintdat1, aes(x=lev, y=coef)) +
  geom_point(size=3, alpha=0.8) +
  geom_linerange(aes(y=coef, ymin = lb, ymax = ub), linewidth=0.3) +
  xlab("Asset Index") +
  ylab("Estimated Treatment Effects") +
  geom_hline(yintercept = 0, linetype = "dashed", color="red") +
  ylim(-6, 8) +
  theme_minimal() +
  ggtitle("(b) Women Ward Candidates")

aintm + aintf
ggsave("assetint_womenonly.pdf", width=8, height=4)


#### Table 5: Treatment x Identity Interactions ####
marw1 <- lm(about ~  treat*married, data=subset(data, woman==1))
marw2 <- lm(about ~  treat*married + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==1))

marm1 <- lm(about ~  treat*married, data=subset(data, woman==0))
marm2 <- lm(about ~  treat*married + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==0))


relw1 <- lm(about ~  treat*catholic    , data=subset(data, woman==1))
relm2 <- lm(about ~  treat*catholic    , data=subset(data, woman==0))
relw3 <- lm(about ~  treat*mainline   , data=subset(data, woman==1))
relm4 <- lm(about ~  treat*mainline   , data=subset(data, woman==0))
relw5 <- lm(about ~  treat*evangelical , data=subset(data, woman==1))
relm6 <- lm(about ~  treat*evangelical , data=subset(data, woman==0))


stargazer(marw1,
          marm1,
          relw1,
          relm2,
          relw3,
          relm4,
          relw5,
          relm6,
          type="text",
          column.labels = c("Women", "Men","Women", "Men", "Women", "Men", "Women", "Men"),
          dep.var.labels = "",
          keep=c("treat", "married", "catholic", "christian", "evangelical", "treat:", "Constant"),
          no.space=T,
          # covariate.labels=c("Treatment", "Married", "Catholic", "Mainline Christian", "Evangelical",
          #                    "Treatment x Married",
          #                    "Treatment x Catholic", "Treatment x Mainline", "Treatment x Evangelical"),
          #add.lines = list(c("Pretreatment Controls", "Yes","Yes","Yes", "Yes", "Yes", "Yes")),
          out="tables/het_mar_religion.tex")


#### Table 6: Treatment x Knowledge Interactions ####
collegew1 <- lm(about ~ treat*college, data=subset(data, woman==1))
collegem1 <- lm(about ~ treat*college, data=subset(data, woman==0))
worgw1 <- lm(about ~ treat*womenorg, data=subset(data, woman==1))
worgm1 <- lm(about ~ treat*womenorg, data=subset(data, woman==0))


stargazer(collegew1, collegem1,
          type="text",
          column.labels = c("Women", "Men"),
          dep.var.labels = "",
          keep=c("treat", "college", "treat:", "Constant"),
          no.space=T, out="tables/het_college.tex")

stargazer(worgw1, worgm1,
          type="text",
          column.labels = c("Women", "Men"),
          dep.var.labels = "",
          keep=c("treat", "womenorg", "treat:", "Constant"),
          no.space=T, out="tables/het_worg.tex")

### Appendix Tables/Figures ####

#### Table A1: Balance ####
balance <- data.frame(vars2)

for (i in 1:length(vars)){

  balance[i,2]<-lapply(data[vars[i]], function(x) {t.test(x[data$treat==1], x[data$treat==0])$estimate[1]})
  balance[i,3]<-lapply(data[vars[i]], function(x) {t.test(x[data$treat==1], x[data$treat==0])$estimate[2]})
  balance[i,4]<-lapply(data[vars[i]], function(x) {t.test(x[data$treat==1], x[data$treat==0])$p.value})

}

colnames(balance)<-c("Variable", "Treated", "Control", "P-value")

balance <- balance %>% mutate_if(is.numeric, round, 2)

print(xtable(balance, digits=c(0,0,2,2,2), caption="Descriptive Statistics", label="balance", align="lcccc"), include.rownames = F, table.placement="H", caption.placement = "top", file="balance.tex")


#### Table A2: Descriptive Stats by Gender and Position ####

vars_by_tier <- c("married", "catholic","mainline","evangelical",
                  "bemba", "lala","lozi","ngoni","nyanja","tonga",
                   "ruling","assetindex","college",
                   "womenorg","agenum", "electexp", "offgen")
vars_lab <- c("Married", "Catholic","Christian","Evangelical",
           "Bemba","Lala", "Lozi", "Ngoni","Nyanja","Tonga",
           "Ruling party", "Asset index", "College",
           "Women's organization","Age", "Prior electoral experience")


dat_bytier <- data[vars_by_tier]
sum_tier <- dat_bytier %>%
  dplyr::filter(!is.na(offgen)) %>%
  dplyr::group_by(offgen) %>%
  dplyr::summarise_all(mean, na.rm=T) %>%
  tidyr::gather(key = key, value = value, -offgen, factor_key = T) %>%
  tidyr::spread(key = offgen, value = value)

colnames(sum_tier) <- c("Variable", "Men Parliamentary", "Women Parliamentary", "Men Ward", "Women Ward")
sum_tier$variable_lab <- vars_lab
sum_tier <- sum_tier %>%
  dplyr::select(-Variable) %>%
  dplyr::select(5, 1, 2, 3, 4)

print(xtable(sum_tier, align="lccccc", digits=c(0,0,2,2,2,2),
             caption="Descriptive Statistics by Electoral Tier and Gender",
             label="sum_tier"), caption.placement = "top", include.rownames=FALSE, file="sum_tier.tex")

#### Table A3: Treatment FX ####

full  <- lm(about ~ treat, data=data)
men   <- lm(about ~ treat, data=subset(data, woman==0))
women <- lm(about ~ treat, data=subset(data, woman==1))

fullmp     <- lm(about~treat, data=subset(data, mp==1))
mpmen      <- lm(about~treat, data=subset(data, mp==1 & woman==0))
mpwomen    <- lm(about~treat, data=subset(data, mp==1 & woman==1))

fullward     <- lm(about~treat, data=subset(data, mp==0))
wardmen    <- lm(about~treat, data=subset(data, mp==0 & woman==0))
wardwomen  <- lm(about~treat, data=subset(data, mp==0 & woman==1))


stargazer(full, women, men,
          type="text",
          covariate.labels=c("Treatment Effect", "Control Mean"),
          dep.var.labels=c("Agree with Making Abortion More Accessible (7-point scale)"),
          keep = c("treat", "Constant"),
          keep.stat=c("n", "rsq"), digits=3, title="", no.space=T, model.numbers=TRUE, out="offgentab0.tex")

stargazer(fullmp, mpwomen, mpmen,
          type="text",
          covariate.labels=c("Treatment", "Control Mean"),
          dep.var.labels="Agree with Making Abortion More Accessible (7-point scale)",
          keep = c("treat", "Constant"),
          keep.stat=c("n", "rsq"), digits=3, title="", no.space=T, model.numbers=TRUE, out="offgentab.tex")

stargazer(fullward, wardwomen, wardmen,
          type="text",
          covariate.labels=c("Treatment", "Control Mean"),
          dep.var.labels="Agree with Making Abortion More Accessible (7-point scale)",
          keep = c("treat", "Constant"),
          keep.stat=c("n", "rsq"), digits=3, title="", no.space=T, model.numbers=TRUE, out="offgentab1.tex")


#### Table A4: Treatment FX + Prior Abortion Belief ####

full_p  <- lm(about ~ treat + abortion_leg_inv, data=data)
men_p   <- lm(about ~ treat + abortion_leg_inv, data=subset(data, woman==0))
women_p <- lm(about ~ treat + abortion_leg_inv, data=subset(data, woman==1))

fullmp_p     <- lm(about~treat + abortion_leg_inv, data=subset(data, mp==1))
mpmen_p      <- lm(about~treat + abortion_leg_inv, data=subset(data, mp==1 & woman==0))
mpwomen_p    <- lm(about~treat + abortion_leg_inv, data=subset(data, mp==1 & woman==1))

fullward_p     <- lm(about~treat + abortion_leg_inv, data=subset(data, mp==0))
wardmen_p      <- lm(about~treat + abortion_leg_inv, data=subset(data, mp==0 & woman==0))
wardwomen_p    <- lm(about~treat + abortion_leg_inv, data=subset(data, mp==0 & woman==1))


stargazer(full_p, women_p, men_p,
          type="text",
          covariate.labels=c("Treatment Effect", "Prior Abortion Preference", "Constant"),
          dep.var.labels=c("Agree with Making Abortion More Accessible (7-point scale)"),
          keep.stat=c("n", "rsq"), digits=3, title="", no.space=T, model.numbers=TRUE, out="treatfx_all_pa.tex")

stargazer(fullmp_p, mpwomen_p, mpmen_p,
          type="text",
          covariate.labels=c("Treatment Effect", "Prior Abortion Preference", "Constant"),
          dep.var.labels="Agree with Making Abortion More Accessible (7-point scale)",
          keep.stat=c("n", "rsq"), digits=3, title="", no.space=T, model.numbers=TRUE, out="treatfx_mp_pa.tex")

stargazer(fullward_p, wardwomen_p, wardmen_p,
          type="text",
          covariate.labels=c("Treatment Effect", "Prior Abortion Preference", "Constant"),
          dep.var.labels="Agree with Making Abortion More Accessible (7-point scale)",
          keep.stat=c("n", "rsq"), digits=3, title="", no.space=T, model.numbers=TRUE, out="treatfx_wc_pa.tex")

#### Table A5: Treatment FX + Full Controls ####
fm1 <- lm(about ~  treat + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + assetindex + college + womenorg, data=data)
fm2 <- lm(about ~  treat + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + assetindex + college + womenorg, data=subset(data, woman==1))
fm3 <- lm(about ~  treat + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + assetindex + college + womenorg, data=subset(data, woman==0))

stargazer(fm1, fm2, fm3,
          type="text",
          column.labels = c("Full Sample", "Women","Men"),
          dep.var.labels = "",
          no.space=T,
          covariate.labels=c("Treatment", "Prior Abortion Preference", "Married", "Mainline Christian", "Evangelical", "Catholic",
                             "Age", "Ruling Party", "MP Candidate", "Prior Election Experience",
                             "Asset Index", "College", "Women's Organization"),
          out="te_fullcontrols.tex")

#### Table A6: Treatment FX + Full Controls + Interactions ####
fm4 <- lm(about ~  treat*woman + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + assetindex + college + womenorg, data=data)
fm5 <- lm(about ~  treat*woman + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling  + electexp + assetindex + college + womenorg, data=subset(data, mp==1))
fm6 <- lm(about ~  treat*woman + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling  + electexp + assetindex + college + womenorg, data=subset(data, mp==0))

stargazer(fm4, fm5, fm6,
          type="text",
          column.labels = c("Full Sample", "MP Candidate", "Ward Candidate"),
          dep.var.labels = "",
          no.space=T,
          out="te_fullcontrols_interact.tex")

#### Table A7: Treatment FX x Asset Interactions ####
fm1a <- lm(about ~  treat*assetindex, data=data)
fm1a2 <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=data)

fm2a <- lm(about ~  treat*assetindex, data=subset(data, woman==1))
fm2a2 <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + college + womenorg, data=subset(data, woman==1))

fm3a <- lm(about ~  treat*assetindex, data=subset(data, woman==0))
fm3a2 <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + college + womenorg, data=subset(data, woman==0))


stargazer(fm1a, fm1a2, fm2a, fm2a2, fm3a, fm3a2,
          type="text",
          column.labels = c("All", "All", "Women", "Women", "Men", "Men"),
          dep.var.labels = "",
          keep=c("treat", "assetindex", "treat:", "Constant"),
          no.space=T,
          # covariate.labels=c("Treatment", "Prior Abortion Preference", "Married", "Mainline mainline", "Evangelical", "Catholic",
          #                    "Age", "Ruling Party", "MP Candidate", "Prior Election Experience",
          #                    "Asset Index", "College", "Women's Organization"),
          add.lines = list(c("Pretreatment Controls", "No", "Yes", "No", "Yes", "No", "Yes")),
          out="te_asset.tex")

#### Table A8: Treatment FX x Asset Interactions Full Controls ####

fm1a <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=data)
fm2a <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + college + womenorg, data=subset(data, woman==1))
fm3a <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + mp + electexp + college + womenorg, data=subset(data, woman==0))

stargazer(fm1a, fm2a, fm3a,
          type="text",
          column.labels = c("Full Sample", "Women","Men"),
          dep.var.labels = "",
          no.space=T,
          # covariate.labels=c("Treatment", "Prior Abortion Preference", "Married", "Mainline Christian", "Evangelical", "Catholic",
          #                    "Age", "Ruling Party", "MP Candidate", "Prior Election Experience",
          #                    "Asset Index", "College", "Women's Organization"),
          out="te_fullcontrols_asset.tex")

#### Table A9: Treatment FX x Asset, Women candidates only ####
fm6a <- lm(about ~  treat*assetindex, data=subset(data, woman==1 & mp==1))
fm6b <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + electexp + college + womenorg, data=subset(data, woman==1 & mp==1))

fm7a <- lm(about ~  treat*assetindex, data=subset(data, woman==1 & mp==0))
fm7b <- lm(about ~  treat*assetindex + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling + electexp + college + womenorg, data=subset(data, woman==1 & mp==0))

stargazer(fm6a, fm6b, fm7a, fm7b,
          type="text",
          column.separate = c(2, 2),
          column.labels = c("Women MP Candidates","Women Ward Candidates"),
          keep=c("treat", "assetindex", "treat:", "Constant"),
          dep.var.labels = "",
          no.space=T,
          # covariate.labels=c("Treatment", "Prior Abortion Preference", "Married", "Mainline Christian", "Evangelical", "Catholic",
          #                    "Age", "Ruling Party", "MP Candidate", "Prior Election Experience",
          #                    "Asset Index", "College", "Women's Organization"),
          out="te_asset_women.tex")

#### Figure A1 & Table A10 Randomization Inference 1 ####
women <- data %>%
  filter(woman==1 &!is.na(treat) & !is.na(about))


N <- 74
declaration <- declare_ra(N = N, m = 43)
X <- women$assetindex
set.seed(090283)
Z <- women$treat
Y <- women$about

dat <- data.frame(X, Y, Z)

ate_hat <- coef(lm(Y ~ Z, data = dat))[2]

ri_out0 <-
  conduct_ri(
    model_1 = Y ~ Z + X, # restricted model
    model_2 = Y ~ Z + X + Z*X, # unrestricted model
    declaration = declaration,
    sharp_hypothesis = ate_hat,
    data = dat
  )

plot(ri_out0)
ggsave("ri_out0.pdf", width=6, height=4)


#### Figure A2 & Table A11 Randomization Inference 1 ####
womenward <- data %>%
  filter(woman==1 & mp==0 &!is.na(treat) & !is.na(about))

N <- 49
declaration <- declare_ra(N = N, m = 28)
X <- womenward$assetindex
set.seed(090283)
Z <- womenward$treat
Y <- womenward$about

dat <- data.frame(X, Y, Z)

ate_hat <- coef(lm(Y ~ Z, data = dat))[2]


ri_out1 <-
  conduct_ri(
    model_1 = Y ~ Z + X, # restricted model
    model_2 = Y ~ Z + X + Z*X, # unrestricted model
    declaration = declaration,
    sharp_hypothesis = ate_hat,
    data = dat
  )

plot(ri_out1)
ggsave("ri_out1.pdf", width=6, height=4)


#### Table A12: Treatment FX x Marriage Interaction ####
marw1 <- lm(about ~  treat*married, data=subset(data, woman==1))
marw2 <- lm(about ~  treat*married + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==1))

marm1 <- lm(about ~  treat*married, data=subset(data, woman==0))
marm2 <- lm(about ~  treat*married + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==0))

stargazer(marw1, marw2, marm1, marm2,
          type="text",
          column.labels = c("Women", "Women", "Men", "Men"),
          dep.var.labels = "",
          keep=c("treat", "married", "treat:", "Constant"),
          no.space=T,
          add.lines = list(c("Pretreatment Controls", "No", "Yes", "No", "Yes")),
          out="het_married.tex")


#### Table A13: Treatment FX + Religion  ####
rel1 <- lm(about ~  treat + catholic + mainline + evangelical, data=subset(data, woman==1))
rel2 <- lm(about ~  treat + catholic + mainline + evangelical, data=subset(data, woman==0))

stargazer(rel1, rel2,
          no.space=T,
          type="text",
          covariate.labels=c("Treatment", "Catholic", "Mainline Christian", "Evangelical"),
          column.labels = c("Women", "Men"),
          dep.var.labels = "", keep.stat = c("n"), out="te_relcontrols.tex")


#### Table A14: Treatment FX x Religion  ####
relw1 <- lm(about ~  treat*catholic + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==1))
relm2 <- lm(about ~  treat*catholic + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==0))
relw3 <- lm(about ~  treat*mainline + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==1))
relm4 <- lm(about ~  treat*mainline + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==0))
relw5 <- lm(about ~  treat*evangelical + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==1))
relm6 <- lm(about ~  treat*evangelical + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==0))

stargazer(relw1,
          relm2,
          relw3,
          relm4,
          relw5,
          relm6,
          type="text",
          column.labels = c("Women", "Men", "Women", "Men", "Women", "Men"),
          dep.var.labels = "",
          keep=c("treat", "married", "catholic", "mainline", "evangelical", "treat:", "Constant"),
          no.space=T,
          covariate.labels=c("Treatment", "Catholic", "Mainline mainline", "Evangelical",
                             "Treatment x Catholic", "Treatment x Mainline", "Treatment x Evangelical"),
          add.lines = list(c("Pretreatment Controls", "Yes","Yes","Yes", "Yes", "Yes", "Yes")),
          out="het_religion.tex")

#### Table A15: Treatment FX + Ethnicity Controls ####
eth1 <- lm(about ~  treat + bemba + lozi + lala + ngoni + nyanja + tonga, data=subset(data, woman==1))
eth2 <- lm(about ~  treat + bemba + lozi + lala + ngoni + nyanja + tonga, data=subset(data, woman==0))

stargazer(eth1, eth2,
          no.space=T,
          type="text",
          covariate.labels=c("Treatment", "Bemba", "Lozi", "Lala", "Ngoni", "Nyanja", "Tonga"),
          column.labels = c("Women", "Men"),
          dep.var.labels = "", keep.stat = c("n"), out="te_ethcontrols.tex")

#### Table A16: Treatment FX x Ethnicity Controls ####
fm8   <- lm(about ~ treat*factor(ethniccat), data=subset(data, woman==1))
fm8.1 <- lm(about ~ treat*factor(ethniccat) + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling  + electexp + assetindex + college + womenorg, data=subset(data, woman==1))
fm9   <- lm(about ~ treat*factor(ethniccat), data=subset(data, woman==0))
fm9.1 <- lm(about ~  treat*factor(ethniccat) + abortion_leg_inv + married + mainline + evangelical + catholic + agenum + ruling  + electexp + assetindex + college + womenorg, data=subset(data, woman==0))


stargazer(fm8, fm8.1, fm9, fm9.1,
          type="text",
          column.labels = c("Women", "Women", "Men", "Men"),
          dep.var.labels = "",
          no.space=T,
          add.lines = list(c("Pretreatment Controls", "No", "Yes", "No", "Yes")),
          out="het_ethnicity.tex")

#### Table A17: Treatment FX x College ####
collegew1 <- lm(about ~ treat*college, data=subset(data, woman==1))
collegew2 <- lm(about ~ treat*college + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==1) )
collegem1 <- lm(about ~ treat*college, data=subset(data, woman==0))
collegem2 <- lm(about ~ treat*college + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==0) )

stargazer(collegew1, collegew2, collegem1, collegem2,
          type="text",
          column.labels = c("Women", "Women", "Men", "Men"),
          dep.var.labels = "",
          keep=c("treat", "college", "treat:", "Constant"),
          no.space=T,
          add.lines = list(c("Pretreatment Controls", "No", "Yes", "No", "Yes")),
          out="het_college.tex")

#### Table A18: Treatment FX x Knoweldge ####
worgw1 <- lm(about ~ treat*womenorg, data=subset(data, woman==1))
worgw2 <- lm(about ~ treat*womenorg + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==1) )
worgm1 <- lm(about ~ treat*womenorg, data=subset(data, woman==0))
worgm2 <- lm(about ~ treat*womenorg + abortion_leg_inv + assetindex + mainline + evangelical + catholic + agenum + ruling + mp + electexp  + college + womenorg, data=subset(data, woman==0) )

stargazer(worgw1, worgw2, worgm1, worgm2,
          type="text",
          column.labels = c("Women", "Women", "Men", "Men"),
          dep.var.labels = "",
          keep=c("treat", "womenorg", "treat:", "Constant"),
          no.space=T,
          add.lines = list(c("Pretreatment Controls", "No", "Yes", "No", "Yes")),
          out="het_worg.tex")

#### Figure A3: Abortion preference by party ####
sumparty <- data %>%
  group_by(ruling) %>%
  summarise(abort_leg = mean(abortion_leg_inv, na.rm=T),
            sd   = sd(abortion_leg_inv, na.rm=T),
            se_min   = mean_se(abortion_leg_inv)$ymin,
            se_max   = mean_se(abortion_leg_inv)$ymax) %>%
  filter(!is.na(ruling)) %>%
  mutate(label = round(abort_leg, 2))


pd <- position_dodge(0.7)
plot  <- ggplot(sumparty, aes(x=ruling, y=abort_leg, fill=factor(ruling))) +
  geom_bar(position=pd, stat="identity", width=0.5, alpha=0.9) +
  theme_minimal() +
  geom_errorbar(aes(y=abort_leg, ymin = se_min, ymax = se_max, width=0.1), position=position_dodge(0.7)) +
  geom_text(aes(label = label), size = 4.0, hjust = -0.3, vjust=-0.7, position=position_dodge(0.7)) +
  geom_segment(aes(x=0, y=-0.3, xend=1, yend=-0.3), colour="gray") +
  scale_fill_manual(values=c("red", "blue"),
                    breaks=c("0", "1"),
                    labels=c("Opposition: UPND", "Ruling Party: PF"),
                    name="Political Party Affiliation")  +
  scale_x_discrete(breaks=c(0, 1),
                   labels=c("Opposition: UPND", "Ruling Party: PF")) +
  #ylim(0.1, -0.4) +
  ylab("Mean Abortion Policy Preference") +
  xlab("Political Party") +
  coord_flip()
print(plot)

ggsave("abort_leg_byparty.pdf", width=6, height=4)

