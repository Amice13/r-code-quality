library(MASS)   #Used for mvrnorm.  Important to load this first.
library(dplyr)
library(reshape2)
library(ggplot2)
library(foreign)
library(broom)
library(xtable)
library(haven)
library(tidyr)
library(stargazer)
library(readr)
library(stringr)
library(interplot)


#################################
# Make tables and figures
#################################

remove(list = ls())
load(file = "jopdata.RData")


# Table 1 -----------------------------------------------------------------

a <- lm(change_toward_president ~ conditions_met, data = scdata)
b <- lm(change_toward_president ~ conditions_met + distal_vacancy + president_senate_agree, data = scdata)

covlabs <- c("Conditions met for change", "Distal vacancy", "President-Senate agree", "Constant")
stargazer(a, b,
          title = "Table 1: Predicting Change in Median Toward the President",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Change toward president",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          notes = "",
          covariate.labels = covlabs,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_1.txt")

# Table 1 -----------------------------------------------------------------

lev.df <- c("predicted_change",
            "peer_effects",
            "dmood",
            "predicted_change_no_senate2",
            "(Intercept)")      

lab.df <- c("Predicted Median Change",
            "Peer Effects",
            "Change in Public Mood",
            "Predicted Median Change (Non-constraining Senate)",         
            "Constant")             


a <- lm(actual_change ~ predicted_change, data = scdata)
b <- lm(actual_change ~ predicted_change + peer_effects + dmood, data = scdata)
c <- lm(actual_change ~ predicted_change + predicted_change_no_senate2, data = scdata)
d <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood, data = scdata)
stargazer(a, b, c, d,
          title = "Table 2: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median change",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          notes = "",
          covariate.labels = lab.df,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_2.txt")


# Figure 2 ----------------------------------------------------------------


labels <- c("Predicted Median Change",
            "Predicted Median Change\n(Non-constraining Senate)",
            "Peer Effects",
            "Change in Public Mood")

coefs <- data.frame(confint(d,level = .95 ))[-1,]
names(coefs) <- c("low", "high")
coefs$mean <- d$coefficients[-1]
coefs$term <- labels


coefs$term <- factor(coefs$term, labels[4:1])
f2 <- ggplot(coefs, aes(y = term, x = mean)) + 
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = low, xmax = high), height = .125, size = 1) +
  ggtitle("Point Predictions\n(Estimate and 95% Confidence Intervals)") +
  ylab(NULL) + 
  xlab("Beta") +
  scale_x_continuous(breaks = seq(-.5, 2.5, by = .5), limits = c(-.5, 2.5) ) +
  theme(axis.text = element_text(colour="black"))
ggsave(f2 ,filename = "Figure_2.pdf", height=4, width = 8)

# Figure 3 ----------------------------------------------------------------

y<-c(.85,.85,1,.1,2)
y2<-c(4,4,4,4,4) - 3
y94 <- draws %>%
  filter(incomingname =="Ginsburg")
yy94 <- y94 %>% select(variable, lag_median, J4, outgoing) %>% gather(Justice, value, -variable)

ideals<-summarize(y94, 
                  presidentideal = mean(presidentideal,na.rm=T),
                  senateideal = mean(senateideal, na.rm=T),
                  lag_median = mean(lag_median, na.rm=T), 
                  J4 = mean(J4, na.rm=T),
                  outgoing = mean(outgoing, na.rm=T))
names(ideals)<- c("Clinton", "Senate", "Median","J[4]", "Departing\nJustice")

mideals<-gather(ideals,Justice, value)
yy94$Justice <- factor(yy94$Justice, levels = c("lag_median" , "J4",   "outgoing"), labels = c("Median","J[4]","Departing\nJustice"))

ltypes <- c("solid", "solid", "solid", "dotted", "dashed")
cols <- c("black", "black", "black", "purple", "blue")

f3 <- ggplot(yy94, aes(x=value, fill=Justice)) + 
  geom_density(alpha=0, size=0, linetype = 0) + 
  geom_hline(yintercept=0) +
  ylim(0,6) +
  xlab("\nIdeal Point") +
  ylab(NULL) +
  #ggtitle("Ginsburg Replaces White, August 1993") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-1,1, by = .2), limits = c(-1,1)) +
  geom_segment(data=mideals, aes(x = value, y = 0, xend = value*y, yend = y2-.1), linetype = ltypes, colour = cols,  size = 1 ) + 
  geom_text(data=mideals, aes(x=value*y, y = y2, label = Justice), size = 9, vjust=0, colour = cols, parse = TRUE) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 22),
        legend.position = "none")
ggsave(f3 ,filename = "Figure_3.pdf", width=11, height=7)


# Figure 4 ----------------------------------------------------------------

f4 <- ggplot(yy94, aes()) + 
  geom_density(aes(x=value,group=Justice, linetype = Justice, colour = Justice),  size = 1) + 
  geom_hline(yintercept=0) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  scale_colour_manual(values = cols[3:5]) +
  ylim(0,6) +
  xlab("\nIdeal Point") +
  ylab(NULL) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-1,1, by = .2), limits = c(-1,1)) +
  geom_segment(data=mideals, aes(x = value, y = 0, xend = value*y, yend = y2-.1), linetype = ltypes, colour = cols,  size = 1) + 
  geom_text(data=mideals, aes(x=value*y, y = y2, label = Justice), size = 9, vjust=0, colour = cols, parse = TRUE) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 22),
        legend.position = "none")

ggsave(f4 ,filename = "Figure_4.pdf", width=11, height=7)



# Footnote 28 -------------------------------------------------------------

draws %>%
  filter(abs(predicted_change) > 0) %>%
  distinct(incomingname) %>%
  arrange(incomingname)  

scdata %>%
  filter(abs(predicted_change) > 0) %>%
  distinct(incomingname) %>%
  arrange(incomingname)


# Figure 5 ----------------------------------------------------------------


models <- draws %>%
  group_by(variable) %>%
  do(mod = lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood, data = .))


hyp <- .95
alpha <- 1-hyp
set.seed(2341554)
coefs <- models %>%
  do(data.frame(
    variable = .$variable,
    term = names(coef(.$mod)), 
    old = coef(.$mod), 
    estimate = mvrnorm(mu = coef(.$mod), Sigma = vcov(.$mod)), stringsAsFactors = F)) %>%
  ungroup() %>%
  mutate(term = factor(term, levels = lev.df, labels = lab.df)) %>% 
  group_by(term) %>% 
  summarise(mean = mean(estimate), low = quantile(estimate, alpha/2 ), high = quantile(estimate, 1 - alpha/2)) %>%
  data.frame

coefs <- coefs[coefs$term != "Constant", ]

levels <- c("Predicted Median Change",
            "Predicted Median Change (Non-constraining Senate)",
            "Peer Effects",
            "Change in Public Mood")[4:1]
labels <- c("Predicted Median Change",
            "Predicted Median Change\n(Non-constraining Senate)",
            "Peer Effects",
            "Change in Public Mood")[4:1]


coefs$term <- factor(coefs$term, levels, labels)
f5 <- ggplot(coefs, aes(y = term, x = mean)) + 
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = low, xmax = high), height = .125, size = 1) +
  ggtitle("Point Predictions\n(Mean and Middle 95% of Beta Coefficients)") +
  ylab(NULL) + 
  xlab("Beta") +
  scale_x_continuous(breaks = seq(-.5, 2.5, by = .5), limits = c(-.5, 2.5) ) +
  theme(axis.text = element_text(colour="black"))
ggsave(f5 ,filename = "Figure_5.pdf", height=4, width = 8)



#################################
# Appendix
#################################



# Table A1 ----------------------------------------------------------------


forkeep1 <- c("\\bconditions_met\\b","\\bdistal_vacancy\\b", "\\bpresident_senate_agree\\b", "\\bConstant\\b")
covlabs1 <- c("Conditions met for change", "Distal vacancy", "President-Senate agree", "Constant")


forkeep2 <- c("\\bpredicted_change\\b","\\bpeer_effects\\b", "\\bdmood\\b", "\\bpredicted_change_no_senate2\\b", "\\bConstant\\b")
covlabs2 <- c("Predicted Median Change (PMC)", "Peer Effects",  "Change in Public Mood", "PMC (NCS)", "Constant")        


a <- lm(change_toward_president ~ conditions_met + chiefjustice, data = scdata)
b <- lm(change_toward_president ~ conditions_met + distal_vacancy + president_senate_agree + chiefjustice, data = scdata)
stargazer(a, b,
          title = "Table A1: Predicting Change in Median Toward the President",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          keep = forkeep1,
          notes = "(fixed effects not displayed)",
          covariate.labels = covlabs1,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A1.txt")


# Table A2 ----------------------------------------------------------------

a <- lm(actual_change ~ predicted_change  + chiefjustice, data = scdata)
b <- lm(actual_change ~ predicted_change + peer_effects + dmood + chiefjustice, data = scdata)
c <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + chiefjustice, data = scdata)
d <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + chiefjustice, data = scdata)
stargazer(a, b, c, d,
          title = "Table A2: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          keep = forkeep2,
          notes = "(fixed effects not displayed)",
          covariate.labels = covlabs2,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A2.txt")

# Table A3 ----------------------------------------------------------------

a <- lm(change_toward_president ~ conditions_met + presidentname, data = scdata)
b <- lm(change_toward_president ~ conditions_met + distal_vacancy + president_senate_agree + presidentname, data = scdata)

stargazer(a, b,
          title = "Table A3: Predicting Change in Median Toward the President",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          keep = forkeep1,
          notes = "(fixed effects not displayed)",
          covariate.labels = covlabs1,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A3.txt")


# Table A4 ----------------------------------------------------------------

a <- lm(actual_change ~ predicted_change  + presidentname, data = scdata)
b <- lm(actual_change ~ predicted_change + peer_effects + dmood + presidentname, data = scdata)
c <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + presidentname, data = scdata)
d <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + presidentname, data = scdata)
stargazer(a, b, c, d,
          title = "Table A4: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          keep = forkeep2,
          notes = "(fixed effects not displayed)",
          covariate.labels = covlabs2,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A4.txt")


# Table A5 ----------------------------------------------------------------

a <- lm(change_toward_president ~ conditions_met, data = scdata[!is.na(scdata$incomingname),])
b <- lm(change_toward_president ~ conditions_met + distal_vacancy + president_senate_agree, data = scdata[!is.na(scdata$incomingname),])

stargazer(a, b,
          title = "Table A5: Predicting Change in Median Toward the President",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          keep = forkeep1,
          notes = "",
          covariate.labels = covlabs1,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A5.txt")



# Table A6 ----------------------------------------------------------------

a <- lm(actual_change ~ predicted_change, data = scdata[!is.na(scdata$incomingname),])
b <- lm(actual_change ~ predicted_change + peer_effects + dmood, data = scdata[!is.na(scdata$incomingname),])
c <- lm(actual_change ~ predicted_change + predicted_change_no_senate2, data = scdata[!is.na(scdata$incomingname),])
d <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood, data = scdata[!is.na(scdata$incomingname),])
stargazer(a, b, c, d,
          title = "Table A6: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          keep = forkeep2,
          notes = "",
          covariate.labels = covlabs2,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A6.txt")


# Table A7 ----------------------------------------------------------------



i <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + qual, data = scdata)
j <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + fedca, data = scdata)
k <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + fedcayrs, data = scdata)
l <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + agenom, data = scdata)


covlabs5 <- c("Predicted Median Change (PMC)", 
              "Peer Effects", 
              "Change in Public Mood", 
              "PMC (NCS)",
              "Segal-Cover Qualifications Scores",  
              "Federal Appellate Experience",  
              "Years of Federal Appellate Experience",  
              "Nominee Age",  
              "Constant")         


stargazer(i,j,k,l, order = c(1, 3, 4, 2, 5, 6:13), 
          title = "Table A7: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f",  "adj.rsq"),
          type = "text", 
          notes = "",
          covariate.labels = covlabs5,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A7.txt")



# Table A8 ----------------------------------------------------------------

i <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + qual + predicted_change_no_senate2:qual, data = scdata)
j <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + fedca + predicted_change_no_senate2:fedca, data = scdata)
k <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + fedcayrs + predicted_change_no_senate2:fedcayrs, data = scdata)
l <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + agenom + predicted_change_no_senate2:agenom, data = scdata)

forkeep5 <- c("\\bpredicted_change\\b",
              "\\bpeer_effects\\b", 
              "\\bdmood\\b", 
              "\\bpredicted_change_no_senate2\\b",
              "\\bqual\\b",  
              "\\bpredicted_change_no_senate2:qual\\b",
              "\\bfedca\\b",  
              "\\bpredicted_change_no_senate2:fedca\\b",
              "\\bfedcayrs\\b",  
              "\\bpredicted_change_no_senate2:fedcayrs\\b", 
              "\\bagenom\\b",  
              "\\bpredicted_change_no_senate2:agenom\\b",
              "\\bConstant\\b")

covlabs5 <- c("Predicted Median Change (PMC)", 
              "Peer Effects", 
              "Change in Public Mood", 
              "PMC (NCS)",
              "Segal-Cover Qualifications Scores",  
              "Segal-Cover Qualifications Scores X PMC (NCS)",
              "Federal Appellate Experience",  
              "Federal Appellate Experience X PMC (NCS)",
              "Years of Federal Appellate Experience",  
              "Years of Federal Appellate Experience X PMC (NCS)",
              "Nominee Age",  
              "Nominee Age X PMC (NCS)", 
              "Constant")         



stargazer(i,j,k,l, order = c(1, 3, 4, 2, 5, 6:13), 
          title = "Table A8: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f",  "adj.rsq"),
          type = "text", 
          keep = forkeep5,
          notes = "",
          covariate.labels = covlabs5,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A8.txt")


# Table A9 ----------------------------------------------------------------

scdata_nomonly <- scdata[!is.na(scdata$incomingname),]

e <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + firstyear, data = scdata_nomonly)
f <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + war, data = scdata_nomonly)
g <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + approval, data = scdata_nomonly)
h <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + firstnom, data = scdata_nomonly)


forkeep4 <- c("\\bpredicted_change\\b",
              "\\bpeer_effects\\b", 
              "\\bdmood\\b", 
              "\\bpredicted_change_no_senate2\\b",
              "\\bfirstyear\\b",  
              "\\bwar\\b",  
              "\\bapproval\\b",  
              "\\bfirstnom\\b", 
              "\\bConstant\\b")

covlabs4 <- c("Predicted Median Change (PMC)", 
              "Peer Effects", 
              "Change in Public Mood", 
              "PMC (NCS)",
              "First Year",  
              "War",  
              "Presidential Approval",
              "First Appointment", 
              "Constant")         



stargazer(e,f,g,h, order = c(1, 3, 4, 2, 5, 6:9), 
          title = "Table A9: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f",  "adj.rsq"),
          type = "text", 
          keep = forkeep4,
          notes = "",
          covariate.labels = covlabs4,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A9.txt")


# Table A10 ---------------------------------------------------------------

scdata_nomonly <- scdata[!is.na(scdata$incomingname),]

e <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + firstyear + predicted_change_no_senate2:firstyear, data = scdata_nomonly)
f <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + war + predicted_change_no_senate2:war, data = scdata_nomonly)
g <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + approval + predicted_change_no_senate2:approval, data = scdata_nomonly)
h <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + firstnom + predicted_change_no_senate2:firstnom, data = scdata_nomonly)


forkeep4 <- c("\\bpredicted_change\\b",
              "\\bpeer_effects\\b", 
              "\\bdmood\\b", 
              "\\bpredicted_change_no_senate2\\b",
              "\\bfirstyear\\b",  
              "\\bpredicted_change_no_senate2:firstyear\\b",
              "\\bwar\\b",  
              "\\bpredicted_change_no_senate2:war\\b", 
              "\\bapproval\\b",  
              "\\bpredicted_change_no_senate2:approval\\b",
              "\\bfirstnom\\b",  
              "\\bpredicted_change_no_senate2:firstnom\\b",
              "\\bConstant\\b")

covlabs4 <- c("Predicted Median Change (PMC)", 
              "Peer Effects", 
              "Change in Public Mood", 
              "PMC (NCS)",
              "First Year",  
              "First Year X PMC (NCS)",
              "War",  
              "War X PMC (NCS)", 
              "Presidential Approval",  
              "Presidential Approval X PMC (NCS)",
              "First Appointment",  
              "First Appointment X PMC (NCS)",
              "Constant")         



stargazer(e,f,g,h, order = c(1, 3, 4, 2, 5, 6:13), 
          title = "Table A10: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f",  "adj.rsq"),
          type = "text", 
          keep = forkeep4,
          notes = "",
          covariate.labels = covlabs4,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A10.txt")



# Table A11 ---------------------------------------------------------------

scdata_nomonly <- scdata[!is.na(scdata$incomingname),]


a <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + postbork, data = scdata_nomonly)
b <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + postfortas, data = scdata_nomonly)
c <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + unity, data = scdata_nomonly)
d <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + divided, data = scdata_nomonly)

forkeep3 <- c("\\bpredicted_change\\b",
              "\\bpeer_effects\\b", 
              "\\bdmood\\b", 
              "\\bpredicted_change_no_senate2\\b",
              "\\bpostbork\\b",  
              "\\bpostfortas\\b",
              "\\bunity\\b",  
              "\\bdivided\\b",  
              "\\bConstant\\b")

covlabs3 <- c("Predicted Median Change (PMC)", 
              "Peer Effects", 
              "Change in Public Mood", 
              "PMC (NCS)",
              "Post-Bork",  
              "Post-Fortas", 
              "Party Unity",
              "Divided Party Control",  
              "Constant")             

stargazer(a, b, c, d, order = c(1, 3, 4, 2, 5, 6:9), 
          title = "Table A11: Predicting the Amount of Median Change",
          dep.var.caption  = "Dependent Variable:", 
          dep.var.labels =  "Median Change",               
          omit.stat=c("LL","ser","f", "adj.rsq"),
          type = "text", 
          keep = forkeep3,
          notes = "",
          covariate.labels = covlabs3,
          font.size = "scriptsize",
          header=FALSE,
          out="Table_A11.txt")



# Table A12 ---------------------------------------------------------------

scdata_nomonly <- scdata[!is.na(scdata$incomingname),]
a <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + postbork + predicted_change_no_senate2:postbork, data = scdata_nomonly)
b <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + postfortas + predicted_change_no_senate2:postfortas, data = scdata_nomonly)
c <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + unity + predicted_change_no_senate2:unity, data = scdata_nomonly)
d <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + divided + predicted_change_no_senate2:divided, data = scdata_nomonly)

forkeep3 <- c("\\bpredicted_change\\b",
              "\\bpeer_effects\\b", 
              "\\bdmood\\b", 
              "\\bpredicted_change_no_senate2\\b",
              "\\bpostbork\\b",  
              "\\bpredicted_change_no_senate2:postbork\\b",
              "\\bpostfortas\\b",  
              "\\bpredicted_change_no_senate2:postfortas\\b",
              "\\bunity\\b",  
              "\\bpredicted_change_no_senate2:unity\\b",  
              "\\bdivided\\b",  
              "\\bpredicted_change_no_senate2:divided\\b",  
              "\\bConstant\\b")

covlabs3 <- c("Predicted Median Change (PMC)", 
              "Peer Effects", 
              "Change in Public Mood", 
              "PMC (NCS)",
              "Post-Bork",  
              "Post-Bork X PMC (NCS)",
              "Post-Fortas",  
              "Post-Fortas X PMC (NCS)",
              "Party Unity",  
              "Party Unity X PMC (NCS)", 
              "Divided Party Control",  
              "Divided Party Control X PMC (NCS)",
              "Constant")             

stargazer( a, b, c, d, order = c(1, 3, 4, 2, 5, 6:15), 
           title = "Table A12: Predicting the Amount of Median Change",
           dep.var.caption  = "Dependent Variable:", 
           dep.var.labels =  "Median Change",               
           omit.stat=c("LL","ser","f", "adj.rsq"),
           type = "text", 
           keep = forkeep3,
           notes = "",
           covariate.labels = covlabs3,
           font.size = "scriptsize",
           header=FALSE,
           out="Table_A12.txt")


# Figure A1 ---------------------------------------------------------------

# Segal-Cover Qualifications
a <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + qual + predicted_change_no_senate2:qual, data = scdata_nomonly)
fa1 <- interplot(m = a, var1 = "predicted_change_no_senate2", var2 = "qual", rfill = "grey80", ralpha = 1) + ylab("Estimated Effect of\nPredicted Median Change\n(Non-Constraining Senate)") + xlab("Segal-Cover Qualifications Scores") + geom_line() +  theme_bw() 
ggsave(fa1, filename = "Figure_A1.pdf", width = 6.5, height = 4.5)

# Figure A2 ---------------------------------------------------------------

## Nominee Age
b <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + agenom + predicted_change_no_senate2:agenom, data = scdata_nomonly)
fa2 <- interplot(m = b, var1 = "predicted_change_no_senate2", var2 = "agenom", rfill = "grey80", ralpha = 1) + ylab("Estimated Effect of\nPredicted Median Change\n(Non-Constraining Senate)") + xlab("Nominee Age") + geom_line() + theme_bw()
ggsave(fa2, filename = "Figure_A2.pdf", width = 6.5, height = 4.5)

# Figure A3 ---------------------------------------------------------------

# First Appointment
## Party Unity
c <- lm(actual_change ~ predicted_change + predicted_change_no_senate2 + peer_effects + dmood + unity + predicted_change_no_senate2:unity, data = scdata_nomonly)
fa3 <- interplot(m = c, var1 = "predicted_change_no_senate2", var2 = "unity", rfill = "grey80", ralpha = 1) + ylab("Estimated Effect of\nPredicted Median Change\n(Non-Constraining Senate)") + xlab("Party Unity") + geom_line() + theme_bw()
ggsave(fa3, filename = "Figure_A3.pdf", width = 6.5, height = 4.5)


# Figure A4 ---------------------------------------------------------------

fa4 <- ggplot(y94, aes(x=predicted_change)) + 
  geom_histogram(colour = "black", fill = "white", binwidth=.025, boundary = 0) + 
  ylab(NULL) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text", x = -.4, 75, label = "(MORE LIBERAL)", colour = "black", size =6) +
  annotate("text", x = .4, 75, label = "(MORE CONSERVATIVE)", colour = "black", size =6) +
  xlab("\nPredicted Change in Median Ideal Point") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-6,6, by = 1)/10, limits = c(-.6,.6)) +
  scale_y_continuous(breaks = seq(0,150, 25),  limits = c(0, 150)) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22))
ggsave(fa4, filename = "Figure_A4.pdf", width = 6.5, height = 4.5)



