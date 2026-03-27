
# First, clean out any objects 
rm(list=ls())

library(ggplot2)

library(haven)

library(dplyr)
library(reshape2)
library(tidyverse)
library(ggthemes)
library(scales)
library(stringr)

library(sandwich)
library(lmtest)
library(broom)
library(margins)

library(stargazer)
library(cowplot)
library(xtable)


# library(zeligverse) # stopped working; install directly; see below


setwd("C:/Users/msouva/Documents/Air_Power/Air superiority and war outcomes")

# Load the data
#airpower <- read_dta("AirVictory_Masterdata_Clean.dta")
# airpower <- read_dta("ap_data_5-17-20.dta")

airpower <- read_dta("SS_airvictory.dta")

# create some variables and add them to the data
# create binary regime variables
airpower <-mutate(airpower,d6=ifelse(polity>16,1,0))
airpower <-mutate(airpower,a6=ifelse(polity <6,1,0))
airpower <-mutate(airpower,anoc=ifelse(polity>=6 & polity<=16,1,0))

# create a binary air superiority variable
# air superiority binary = 1 if air superiority, 0 otherwise
airpower <-mutate(airpower, airsupbin=ifelse(airsup>1,1,0))
airpower <-mutate(airpower, airparity=ifelse(airsup==1,1,0))

# create binary modern system variables, one for each category
airpower <-mutate(airpower, ms3=ifelse(msadopt==3,1,0))
airpower <-mutate(airpower, ms2=ifelse(msadopt==2,1,0))
airpower <-mutate(airpower, ms1=ifelse(msadopt==1,1,0))
airpower <-mutate(airpower, ms0=ifelse(msadopt==0,1,0))
# create a single binary modern system variable
# msbinary = 1 if modern system = 3 or 4, 0 otherwise 
airpower <-mutate(airpower, msbinary=ifelse(msadopt>=2,1,0))

# create a binary IWD_Win_Loss DV
airpower <- mutate(airpower, iwd_wl=ifelse(IWD_WLT==2,1,0))
table(airpower$IWD_WLT)
table(airpower$iwd_wl)

# create a binary IWD Win Loss outcome for subwars
airpower <- mutate(airpower, subwariwd_wl=ifelse(Subwar_WLT==2,1,0))
table(airpower$Subwar_WLT)
table(airpower$subwariwd_wl)

# combine IWD DV wars and subwars into one outcome variable
airpower <- mutate(airpower, iwd_wl_all=iwd_wl)

airpower <- mutate(airpower, iwd_wl=replace_na(iwd_wl,-9)) 
airpower <- mutate(airpower, iwd_wl_all=ifelse(iwd_wl == -9, subwariwd_wl,iwd_wl_all))
table(airpower$iwd_wl_all)

airpower <- mutate(airpower, iwd_wl=na_if(iwd_wl,-9)) 
summary(airpower$iwd_wl)



# create a data frame with complete cases, i.e. drops cases that have missing values on a covariate
df <- airpower %>% 
  filter(!is.na(win)) %>%
  filter(!is.na(msadopt)) %>%
  filter(!is.na(polity)) %>%
  filter(!is.na(airsup)) %>%
  filter(!is.na(cinc)) %>%
  filter(!is.na(troopsengaged)) %>%
  filter(!is.na(opptroopsengaged))

summary(df)


# Table air superiority-binary
table(df$airsupbin)

df <- mutate(df, outcome=ifelse(win==1,"Win","Loss"))

# cross-tabulation: air superiority and decisive battle outcome
tb <- table(df$airsupbin, df$outcome)
tb

tb.prop <- prop.table(tb, 1)
tb.prop

tb.df <- as.data.frame(tb.prop)
glimpse(tb.df)

names(tb.df) <- c("Air Superiority", "Outcome", "Frequency")
glimpse(tb.df)

cht <- chisq.test(df$airsupbin, df$outcome)
cht
sbtitle <- 
  sprintf("Chi-Square Test Statistic = %.1f, P-value = %.3f", 
          cht$statistic,
          cht$p.value)
sbtitle

# Figure: cross-tabulation: air superiority and decisive battle outcome

as_outcome <- ggplot(tb.df, aes(x=`Air Superiority`, y=Frequency, fill=`Outcome`)) +
  geom_col(position="dodge") +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  labs(title="Air Superiority and Decisive Battle Outcome, 1932-2003")+
  labs(subtitle = sbtitle) +
  labs(caption="Notes: Loss defined as loss or tie, Grauer & Horowitz (2012) data. \n
       No Air Superiority defined as parity or inferiority.")
as_outcome


# Figure: Regime Type and Air Superiority

df <-mutate(df,dem3=ifelse(polity>16,"Democracy",ifelse(polity>=6 & polity <=16,"Anocracy","Autocracy")))

table(df$dem3)

df <-mutate(df,airsupbin=ifelse(airsup>1,"Yes","No"))


tb3 <- table(df$dem3, df$airsupbin)
tb3

tb.prop3 <- prop.table(tb3, 1)
tb.prop3

tb.df3 <- as.data.frame(tb.prop3)
glimpse(tb.df3)

names(tb.df3) <- c("Regime Type", "Air Superiority", "Frequency")
glimpse(tb.df3)


cht <- chisq.test(df$dem3, df$airsupbin)
cht
sbtitle <- 
  sprintf("Chi-Square Test Statistic = %.1f, P-value = %.3f", 
          cht$statistic,
          cht$p.value)
sbtitle

# The palette with black:
cbbPalette <- c( "#D55E00", "#0072B2")

ggplot(tb.df3, aes(x=`Regime Type`, y=Frequency, fill=`Air Superiority`)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=cbbPalette) +
  labs(title="Regime Type and Air Superiority, 1932-2003")+
  labs(subtitle=sbtitle)+
  labs(caption="Notes: Regime types based on polity2 variable. \n
       No Air Superiority defined as parity or inferiority.")



# Models 

df <-mutate(df,airsupbin=ifelse(airsup>1,1,0))

# GH with binary regime vars, no air sup
m1 <- glm(win ~ msadopt + d6 +  anoc + cinc + troopsengaged + opptroopsengaged, 
          family=binomial(link="logit"), data=df)

clusterSEm1 <- vcovCL(m1, cluster = ~ ccode)

coeftest(m1,clusterSEm1)
AIC(m1)

# airsup with controls 
m2 <- glm(win ~ airsup + msadopt + d6 + anoc + cinc + troopsengaged + opptroopsengaged, 
          family=binomial(link="logit"), data=df)

clusterSEm2 <- vcovCL(m2, cluster = ~ ccode)

coeftest(m2,clusterSEm2)
AIC(m2)

# airsup, change dem comparison 
m3 <- glm(win ~ airsup + msadopt + d6 + cinc + troopsengaged + opptroopsengaged, 
          family=binomial(link="logit"), data=df)

clusterSEm3 <- vcovCL(m3, cluster = ~ ccode)

coeftest(m3,clusterSEm3)
AIC(m3)


# all ordinal variables as binary
m4 <- glm(win ~ airsupbin + airparity + ms3 + ms2 + ms1 + d6 + anoc + cinc + troopsengaged + opptroopsengaged, 
          family=binomial(link="logit"), data=df)

clusterSEm4 <- vcovCL(m4, cluster = ~ ccode)

coeftest(m4,clusterSEm4)
AIC(m4)


# In footnote 7, we compare AIC for a model with CINC versus one with Air Superiority
# Model 3b: air superiority and no CINC
# Model 3c: CINC and no air superiority

m3b <- glm(win ~ airsup + msadopt + d6 + troopsengaged + opptroopsengaged, 
          family=binomial(link="logit"), data=df)
AIC(m3b)

m3c <- glm(win ~ msadopt + d6 + cinc + troopsengaged + opptroopsengaged, 
           family=binomial(link="logit"), data=df)
AIC(m3c)

stargazer(m1, m2, m3, m4, type="text", title="Effect of Air Superiority on Decisive Battle Outcome, 1932-2003",
          column.labels=c("Model 1","Model 2","Model 3","Model 4"),
          dep.var.labels = "Decisive Battle Outcome",
          covariate.labels = c( "Air Superiority","Modern System", "Air Superiority (binary)", "Air Parity",
          "Modern System 3", "Modern System 2", "Modern System 1","Democracy", "Anocracy","CINC",
                               "Troops Engaged", "Opponent Troops Engaged"),
          style="io",
          digits=2,
          digits.extra = 4,
          notes=("Two-tail significance levels; Standard Errors Clustered on Country"),
          out="models1-4.txt")


# latex code: 
stargazer(m1, m2, m3, m4, title="Effect of Air Superiority on Decisive Battle Outcome, 1932-2003",
          column.labels=c("Model 1","Model 2","Model 3","Model 4"),
          dep.var.labels = "Decisive Battle Outcome",
          covariate.labels = c( "Air Superiority","Modern System", "Air Superiority (binary)", "Air Parity",
                                "Modern System 3", "Modern System 2", "Modern System 1","Democracy", "Anocracy","CINC",
                                "Troops Engaged", "Opponent Troops Engaged"),
          style="io",
          digits=2,
          digits.extra = 4,
          notes=("Two-tail significance levels; Standard Errors Clustered on Country"))



# Produce Coefficient Plot 
model1Frame <- data.frame(Variable = rownames(summary(m1)$coef),
                          Coefficient = summary(m1)$coef[, 1],
                          SE = diag(clusterSEm1),
                          Model = "M1: No Air Superiority")


model2Frame <- data.frame(Variable = rownames(summary(m2)$coef),
                          Coefficient = summary(m2)$coef[, 1],
                          SE = diag(clusterSEm2),
                          Model = "M2: Air Superiority With Controls")


model3Frame <- data.frame(Variable = rownames(summary(m3)$coef),
                          Coefficient = summary(m3)$coef[, 1],
                          SE = diag(clusterSEm3),
                          Model = "M3: Change Regime Reference Group")


model4Frame <- data.frame(Variable = rownames(summary(m4)$coef),
                          Coefficient = summary(m4)$coef[, 1],
                          SE = diag(clusterSEm4),
                          Model = "M4: Transform Ordinal Variables")


# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame, model4Frame))

# Order the variables
allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels = c("(Intercept)","opptroopsengaged","troopsengaged", 
                                            "cinc" , "anoc" , "d6", "msadopt",
                                            "ms1", "ms2", "ms3", "airparity", "airsupbin", "airsup"))



# Specify the width of your confidence intervals
#interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
logit_plot <- ggplot(allModelFrame, aes(colour = Model)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - sqrt(SE)*interval2,
                                ymax = Coefficient + sqrt(SE)*interval2),
                            lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - sqrt(SE)*interval2,
                                 ymax = Coefficient + sqrt(SE)*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE") + 
  scale_x_discrete(labels=c("airsup" = "Air Superiority", "airsupbin" = "Air Superiority (Binary)",
                            "airparity"="Air Parity", "ms3"="Modern System 3","ms2"="Modern System 2",
                            "ms1"="Modern System 1","msadopt"="Modern System",
                            "d6"="Democracy","anoc"="Anocracy","cinc"="National Capabilities",
                            "troopsengaged"="Troops Engaged","opptroopsengaged"="Opponent Troops",
                            "(Intercept)"="Intercept")) +
coord_flip() + theme_bw() + 
  labs(title = "Logistic Regression Estimates, 1932-2003", subtitle = "Outcome: Decisive Battle Victory/Loss") + 
  labs(caption = "95% CIs, standard errors clustered on country")

print(logit_plot)



# Appendix and Robustness Checks 


# Summary Statistics 

airpower <-mutate(airpower,airsupbin=ifelse(airsup>1,1,0))

df3 <- airpower %>% 
  filter(!is.na(win)) %>%
  filter(!is.na(msadopt)) %>%
  filter(!is.na(d6)) %>%
  filter(!is.na(airsup)) %>%
  filter(!is.na(cinc)) %>%
  filter(!is.na(troopsengaged)) %>%
  filter(!is.na(opptroopsengaged)) %>%
  select(win, airsup, msadopt, d6, anoc, cinc, troopsengaged, opptroopsengaged) %>%
  data.frame()


stargazer(df3, type="text", summary.stat = c("n", "mean", "sd", "min", "max"), digits=2,
          covariate.labels = c("Outcome", "Air Superiority","Modern System", "Democracy", "Anocracy","CINC",
                                "Troops Engaged", "Opponent Troops Engaged"))



# now for latex: just drop type="text" in the first line 
stargazer(df3, summary.stat = c("n", "mean", "sd", "min", "max"), digits=2,
          covariate.labels = c("Outcome", "Air Superiority","Modern System", "Democracy", "Anocracy","CINC",
                               "Troops Engaged", "Opponent Troops Engaged"))


# Correlation Table using stargazer

correlation.matrix <- cor(df3)
# latex correlation table 
stargazer(correlation.matrix, title="Correlation Matrix")


# Correlation table with Xtable; this is easier to manipulate and only get the bottom half

mcor<-round(cor(df3),2)
mcor
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper


print(xtable(upper), type="latex")

print(xtable(upper), type="latex", file="corr_table.latex")


# substantive effects: first difference of air superiority and modern system; uses Zelig

#install.packages("https://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_5.1.6.1.tar.gz", 
#                 repos=NULL, 
#                 type="source")

library(Zelig)

# zelig


zm2 <- zelig(win ~ airsup + msadopt + d6 + anoc + cinc + troopsengaged + opptroopsengaged, 
             model="logit", data=df)

summary(zm2)

# set values of air superiority
zm2 <- setx(zm2, airsup=0)
zm2 <- setx1(zm2, airsup=2)

# run simulations
zm2 <- sim(zm2)
# summarize simulations
summary(zm2)
# obtain zelig plots
plot(zm2)

# calculate first difference for air superiority
fd <- zm2$get_qi(xvalue="x1", qi="fd")
summary(fd)

# set values for modern system
zm2 <- setx(zm2, msadopt=0)
zm2 <- setx1(zm2, msadopt=3)
# run simulations
zm2 <- sim(zm2)
# calculate first difference for modern system
fd2 <- zm2$get_qi(xvalue="x1", qi="fd")
summary(fd2)

# combine the air superiority and modern system first differences into a data frame
dfd <- data.frame(cbind(fd, fd2))
summary(dfd)

# tidy and organize the data frame to get what you want out of it
tidd <- dfd %>% 
  gather(class, simv)

tidd %>% 
  group_by(class) %>% 
  summarise(mean = mean(simv), sd = sd(simv))


# plot histograms of the simulations of the two first differences
ggplot(tidd, aes(simv)) + geom_histogram() + facet_grid(~class) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  labs(title="Effect of Air Superiority and Modern System on Battle Outcome, 1932-2003")+
  labs(caption="Notes: First differences based on Model 2.")


library(cowplot)

dfd <- data.frame(cbind(fd, fd2))
summary(dfd)

# instead of side-by-side plots based on facet, make two plots and place them side-by-side using library(cowplot)
# the advantage of this is that it is easier to adjust the plots 

p <- ggplot(dfd, aes(x=X1)) + geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept=median(X1)), color="blue", linetype="dashed", size=1) +
  #labs(title="Effect of Air Superiority on Battle Outcome",x="Air Superiority, First Difference", y = "Count")+
  labs(caption="Notes: First difference change from minimum to maximum.") +
  xlab("Air Superiority") +
  theme_classic()
p

p2 <- ggplot(dfd, aes(x=X2)) +  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=median(X2)), color="blue", linetype="dashed", size=1) +
  #labs(title="Effect of Modern System on Battle Outcome",x="Modern System, First Difference", y = "Count")+
  labs(caption="Effects based on Model 2. Calculated using Zelig 5.1.6.1.") +
  xlab("Modern System") +
  theme_classic()

p2

plot_grid(p, p2, labels = "AUTO")

packageVersion("zelig")

# the following code places a title above the two plots and adjusts the margins to make it look good 
plot_row <- plot_grid(p, p2)

title <- ggdraw() + 
  draw_label(
    #"Substantive Effect of Air Superiority and Modern System on Battle Outcome",
    "",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )


plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)



# Robustness Analysis

# Change the DV from Decisive Battle to War, IWD data  

# Model 3 where unit of analysis is IWD war or subwar country major participant
# DV is IWD war or subwar outcome
# war outcome = 1 for victory, 0 otherwise
m5 <- glm(iwd_wl_all ~ airsup + msadopt + d6 + cinc + troopsengaged + opptroopsengaged, 
          family=binomial(link="logit"), data=df)

clusterSEm5 <- vcovCL(m5, cluster = ~ ccode)

coeftest(m5,clusterSEm5)
AIC(m5)

# Model 3 where unit of analysis is IWD war country major participant
# DV is IWD war outcome
# war outcome = 1 for victory, 0 otherwise
m6 <- glm(iwd_wl ~ airsup + msadopt + d6 + cinc + troopsengaged + opptroopsengaged, 
          family=binomial(link="logit"), data=df)

clusterSEm6 <- vcovCL(m6, cluster = ~ ccode)

coeftest(m6,clusterSEm6)
AIC(m6)


stargazer(m5, m6, type="text", title="Effect of Air Superiority on IWD War Outcome, 1932-2003",
          column.labels=c("Model 5","Model 6"),
          dep.var.labels = "IWD War Outcome",
          covariate.labels = c( "Air Superiority","Modern System", "Democracy", "CINC",
                                "Troops Engaged", "Opponent Troops Engaged"),
          style="io",
          digits=2,
          digits.extra = 4,
          notes=("Two-tail significance levels; Clustered standard errors"),
          out="models5-6.txt")


# latex code: 
stargazer(m5, m6, title="Effect of Air Superiority on IWD War Outcome, 1932-2003",
          column.labels=c("Model 5","Model 6"),
          dep.var.labels = "IWD War Outcome",
          covariate.labels = c( "Air Superiority","Modern System", "Democracy", "CINC",
                                "Troops Engaged", "Opponent Troops Engaged"),
          style="io",
          digits=2,
          digits.extra = 4,
          notes=("Two-tail significance levels; Standard errors clustered on country"),
          out="models5-6.txt")


# Produce Coefficient Plot 
model5Frame <- data.frame(Variable = rownames(summary(m5)$coef),
                          Coefficient = summary(m5)$coef[, 1],
                          SE = diag(clusterSEm5),
                          modelName = "M5: IWD Wars and SubWars")


model6Frame <- data.frame(Variable = rownames(summary(m6)$coef),
                          Coefficient = summary(m6)$coef[, 1],
                          SE = diag(clusterSEm6),
                          modelName = "M6: IWD Wars")


# Combine these data.frames
allModelFrame2 <- data.frame(rbind(model5Frame, model6Frame))

# Order the variables
allModelFrame2$Variable <- factor(allModelFrame2$Variable, 
                                  levels = c("(Intercept)","opptroopsengaged","troopsengaged", 
                                             "cinc", "d6", "msadopt", "airsup"))



# Specify the width of your confidence intervals
#interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
logit_plot2 <- ggplot(allModelFrame2, aes(colour = modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - sqrt(SE)*interval2,
                     ymax = Coefficient + sqrt(SE)*interval2),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - sqrt(SE)*interval2,
                      ymax = Coefficient + sqrt(SE)*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  scale_x_discrete(labels=c("airsup" = "Air Superiority", "msadopt"="Modern System",
                            "d6"="Democracy","cinc"="National Capabilities",
                            "troopsengaged"="Troops Engaged","opptroopsengaged"="Opponent Troops",
                            "(Intercept)"="Intercept")) +
  coord_flip() + theme_bw() + 
  labs(title = "Logistic Regression Estimates, 1932-2003", subtitle = "Outcome: IWD War Win/Loss") + 
  labs(caption = "95% CIs, standard errors clustered on country")

print(logit_plot2)  # The trick to these is position_dodge().



# Model 3 estimated on non-democracies only 

aut <- subset(df, subset=df$d6==0)

m3b <- glm(win ~ airsup + msadopt + cinc + troopsengaged + opptroopsengaged, 
          family=binomial(link="logit"), data=aut)

clusterSEm3b <- vcovCL(m3b, cluster = ~ ccode)

coeftest(m3b,clusterSEm3b)



# cross-tabulation: air superiority and decisive battle outcome, Autocracies Only
tb <- table(aut$airsupbin, aut$outcome)
tb

tb.prop <- prop.table(tb, 1)
tb.prop

tb.df <- as.data.frame(tb.prop)

names(tb.df) <- c("Air Superiority", "Outcome", "Frequency")

cht <- chisq.test(aut$airsupbin, aut$outcome)
cht
sbtitle <- 
  sprintf("Chi-Square Test Statistic = %.1f, P-value = %.3f", 
          cht$statistic,
          cht$p.value)
sbtitle

# Figure: cross-tabulation: air superiority and decisive battle outcome

aut_as_outcome <- ggplot(tb.df, aes(x=`Air Superiority`, y=Frequency, fill=`Outcome`)) +
  geom_col(position="dodge") +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  labs(title="Air Superiority and Decisive Battle Outcome, 1932-2003")+
  labs(subtitle = sbtitle) +
  labs(caption="Notes: Loss defined as loss or tie, Grauer & Horowitz (2012) data. \n
       No Air Superiority defined as parity or inferiority.")
aut_as_outcome


# Democracies with Air superiority and Battle Outcome

dem <- subset(df, subset=df$d6==1)


# cross-tabulation: air superiority and decisive battle outcome, Autocracies Only
tb <- table(dem$airsupbin, dem$outcome)
tb

tb.prop <- prop.table(tb, 1)
tb.prop

tb.df <- as.data.frame(tb.prop)

names(tb.df) <- c("Air Superiority", "Outcome", "Frequency")

cht <- chisq.test(dem$airsupbin, dem$outcome)
cht
sbtitle <- 
  sprintf("Chi-Square Test Statistic = %.1f, P-value = %.3f", 
          cht$statistic,
          cht$p.value)
sbtitle

# Figure: cross-tabulation: air superiority and decisive battle outcome

dem_as_outcome <- ggplot(tb.df, aes(x=`Air Superiority`, y=Frequency, fill=`Outcome`)) +
  geom_col(position="dodge") +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  labs(title="Air Superiority and Decisive Battle Outcome, 1932-2003")+
  labs(subtitle = sbtitle) +
  labs(caption="Notes: Loss defined as loss or tie, Grauer & Horowitz (2012) data. \n
       No Air Superiority defined as parity or inferiority.")
dem_as_outcome
