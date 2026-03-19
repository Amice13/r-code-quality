####################################################################
## author:    Steven M. Van Hauwaert & Robert A. Huber
## file name: terror_rdd.R
## Context:   Causal Effects of Terror Attacks
## started:   2018-03-13
## Summary:   runs R-Scripts
######################################################################

## empty memory (!)
rm(list=ls())

df <- read.csv("./final data/data_clean_2019-07-12.csv")

df$postterror<- relevel(df$postterror, ref = "Pre-Terror")

df$date <- as.Date(df$date, format = "%Y-%m-%d", origin = "1960-10-01")

df$date <- format(df$date, "%Y-%m-%d %H:%M")

# Descriptive Analyses ----------------------------------------------------

p_socCoh <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d %H:%M"), y = socCoh_num, colour = postterror),size=.25) + 
  geom_jitter(height = .25, width = 0.2, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_tufte() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2015-11-14 00:00", format = "%Y-%m-%d %H:%M"), lty="dashed") +
  theme(legend.position="none")+
  ggtitle("2.A) Social Cohesion") +
  scale_y_continuous(breaks = 1:4,
                     labels = c("Very\nlow", "Low", "High", "Very\nhigh"),
                     limits = c(1,4.5)) +
  scale_colour_manual(values = c("black", "black")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2015-10-30 00:00", "2015-11-27 00:00"), format = "%Y-%m-%d %H:%M")) +
  annotate("text", x = as.Date("2015-11-15 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Post-Terror'), hjust = 0)+
  annotate("text", x = as.Date("2015-11-13 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Pre-Terror'), hjust = 1)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=16), axis.text.x = element_text(size=12)) 

p_socInt <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d %H:%M"), y = socInt_num, colour = postterror),size=.25) + 
  geom_jitter(height = .25, width = 0.2, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_tufte() + 
  ylab("") + 
  xlab("Time") +
  geom_vline(xintercept = as.Date("2015-11-14 00:00", format = "%Y-%m-%d %H:%M"), lty="dashed") +
  theme(legend.position="none")+
  ggtitle("2.B) Societal Integration") +
  scale_y_continuous(breaks = 1:4,
                     labels = c("", "", "", ""),
                     limits = c(1,4.5)) +
  scale_colour_manual(values = c("black", "black")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2015-10-30 00:00", "2015-11-27 00:00"), format = "%Y-%m-%d %H:%M")) +
  annotate("text", x = as.Date("2015-11-15 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Post-Terror'), hjust = 0)+
  annotate("text", x = as.Date("2015-11-13 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Pre-Terror'), hjust = 1)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=16), axis.text.x = element_text(size=12)) 

p_salImm <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d %H:%M"), y = salImm_num, colour = postterror),size=.25) + 
  geom_jitter(height = .25, width = 0.2, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_tufte() + 
  ylab("") + 
  xlab("Time") +
  geom_vline(xintercept = as.Date("2015-11-14 00:00", format = "%Y-%m-%d %H:%M"), lty="dashed") +
  theme(legend.position="none")+
  ggtitle("1.B) Immigration Salience") +
  scale_y_continuous(breaks = 1:4,
                     labels = c("", "", "", ""),
                     limits = c(1,4.5)) +
  scale_colour_manual(values = c("black", "black")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2015-10-30 00:00", "2015-11-27 00:00"), format = "%Y-%m-%d %H:%M")) +
  annotate("text", x = as.Date("2015-11-15 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Post-Terror'), hjust = 0)+
  annotate("text", x = as.Date("2015-11-13 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Pre-Terror'), hjust = 1)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=16), axis.text.x = element_text(size=12)) 

p_posImm <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d %H:%M"), y = posImm_num, colour = postterror),size=.25) + 
  geom_jitter(height = .25, width = 0.2, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_tufte() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2015-11-14 00:00", format = "%Y-%m-%d %H:%M"), lty="dashed") +
  theme(legend.position="none")+
  ggtitle("1.A) Anti-immigrant Opinions") +
  scale_y_continuous(breaks = 1:4,
                     labels = c("Very\nlow", "Low", "High", "Very\nhigh"),
                     limits = c(1,4.5)) +
  scale_colour_manual(values = c("black", "black")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2015-10-30 00:00", "2015-11-27 00:00"), format = "%Y-%m-%d %H:%M")) +
  annotate("text", x = as.Date("2015-11-15 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Post-Terror'), hjust = 0)+
  annotate("text", x = as.Date("2015-11-13 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Pre-Terror'), hjust = 1)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=16), axis.text.x = element_text(size=12)) 

p_polTrust <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d %H:%M"), y = polTrust_num, colour = postterror),size=.25) + 
  geom_jitter(height = .25, width = 0.2, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_tufte() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2015-11-14 00:00", format = "%Y-%m-%d %H:%M"), lty="dashed") +
  theme(legend.position="none")+
  ggtitle("2.C) Political Trust") +
  scale_y_continuous(breaks = 1:4,
                     labels = c("", "", "", ""),
                     limits = c(1,4.5)) +
  scale_colour_manual(values = c("black", "black")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2015-10-30 00:00", "2015-11-27 00:00"), format = "%Y-%m-%d %H:%M")) +
  annotate("text", x = as.Date("2015-11-15 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Post-Terror'), hjust = 0)+
  annotate("text", x = as.Date("2015-11-13 00:00", format = "%Y-%m-%d %H:%M"), y = 4.5, label = c('Pre-Terror'), hjust = 1)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=16), axis.text.x = element_text(size=12)) 

p_extreme11 <- ggplot(df, aes(x=as.Date(date, format = "%Y-%m-%d %H:%M"), y = extreme11, colour = postterror), size=.25) + 
  geom_jitter(height = .25, width = 0.2, alpha = I(.2)) + 
  stat_smooth(method = loess, size = 1) +
  theme_tufte() + 
  ylab("") + 
  xlab("") +
  geom_vline(xintercept = as.Date("2015-11-14 00:00", format = "%Y-%m-%d %H:%M"), lty="dashed") +
  theme(legend.position="none")+
  ggtitle("1.C) Political Polarisation") +
  scale_y_continuous(breaks = c(0:5), labels = c("Low", "", "", "", "", "High")) +
  scale_colour_manual(values = c("black", "black")) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d.\n%b.",
               limits = as.Date(c("2015-10-30 00:00", "2015-11-27 00:00"), format = "%Y-%m-%d %H:%M")) +
  annotate("text", x = as.Date("2015-11-15 00:00", format = "%Y-%m-%d %H:%M"), y = 6, label = c('Post-Terror'), hjust = 0)+
  annotate("text", x = as.Date("2015-11-13 00:00", format = "%Y-%m-%d %H:%M"), y = 6, label = c('Pre-Terror'), hjust = 1)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=16), axis.text.x = element_text(size=12)) 

p_completeIN <- grid.arrange(p_socCoh, p_socInt, p_polTrust, nrow=1)

ggsave(filename = "./figures/Figure_2.pdf", p_completeIN, height = 10, width = 28, units = "cm", dpi = 100)
ggsave(filename = "./figures/Figure_2.png", p_completeIN, height = 10, width = 28, units = "cm", dpi = 100)
ggsave(filename = "./figures/Figure_2.tiff", p_completeIN, height = 10, width = 28, units = "cm", dpi = 300)

p_completeOUT<- grid.arrange(p_posImm, p_salImm, p_extreme11, nrow = 1)

ggsave(filename = "./figures/Figure_1.pdf", p_completeOUT, height = 25, width = 20, units = "cm", dpi = 100)
ggsave(filename = "./figures/Figure_1.png", p_completeOUT, height = 10, width = 28, units = "cm", dpi = 100)
ggsave(filename = "./figures/Figure_1.tiff", p_completeOUT, height = 10, width = 28, units = "cm", dpi = 300)

rm(list=setdiff(ls(), "df"))

# RDD ---------------------------------------------------------------------

bw_socCoh <- IKbandwidth(df$cutoff, df$socCoh_num, cutpoint = 0, verbose = FALSE, kernel = "triangular")
bw_posImm <- IKbandwidth(df$cutoff, df$posImm_num, cutpoint = 0, verbose = FALSE, kernel = "triangular")
bw_salImm <- IKbandwidth(df$cutoff, df$salImm_num, cutpoint = 0, verbose = FALSE, kernel = "triangular")
bw_polTrust <- IKbandwidth(df$cutoff, df$polTrust_num, cutpoint = 0, verbose = FALSE, kernel = "triangular")
bw_extreme11 <- IKbandwidth(df$cutoff, df$extreme11, cutpoint = 0, verbose = T, kernel = "triangular")
bw_socInt <- IKbandwidth(df$cutoff, df$socInt_num, cutpoint = 0, verbose = FALSE, kernel = "triangular")

# Optimal Bandwidth w/o Controls

m_socCoh_ref <- lm(socCoh_num ~ postterror,
               data = df[df$cutoff>-bw_socCoh & df$cutoff<bw_socCoh,], 
               weights = weight)

m_posImm_ref <- lm(posImm_num ~ postterror,
                   data = df[df$cutoff>-bw_posImm & df$cutoff<bw_posImm,], 
                   weights = weight)

m_salImm_ref <- lm(salImm_num ~ postterror,
               data = df[df$cutoff>-bw_salImm & df$cutoff<bw_salImm,], 
               weights = weight)

m_polTrust_ref <- lm(polTrust_num ~ postterror,
                 data = df[df$cutoff>-bw_polTrust & df$cutoff<bw_polTrust,], 
                 weights = weight)

m_extreme11_ref <- lm(extreme11 ~ postterror,
                    data = df[df$cutoff>-bw_extreme11 & df$cutoff<bw_extreme11,], 
                    weights = weight)

m_socInt_ref <- lm(socInt_num ~ postterror,
               data = df[df$cutoff>-bw_socInt & df$cutoff<bw_socInt,], 
               weights = weight)

# Optimal Bandwidth ####

m_socCoh <- lm(socCoh_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_socCoh & df$cutoff<bw_socCoh,], 
               weights = weight)

m_posImm <- lm(posImm_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_posImm & df$cutoff<bw_posImm,], 
               weights = weight)

m_salImm <- lm(salImm_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_salImm & df$cutoff<bw_salImm,], 
               weights = weight)

m_polTrust <- lm(polTrust_num ~ postterror + age + genderF + edu + employment + inc,
                 data = df[df$cutoff>-bw_polTrust & df$cutoff<bw_polTrust,], 
                 weights = weight)

m_extreme11 <- lm(extreme11 ~ postterror + age + genderF + edu + employment + inc,
                data = df[df$cutoff>-bw_extreme11 & df$cutoff<bw_extreme11,], 
                weights = weight)

m_socInt <- lm(socInt_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_socInt & df$cutoff<bw_socInt,], 
               weights = weight)

# 1.5x Bandwidth ####

m_socCoh15 <- lm(socCoh_num ~ postterror + age + genderF + edu + employment + inc,
                 data = df[df$cutoff>-1.5*bw_socCoh & df$cutoff<1.5*bw_socCoh,], 
                 weights = weight)

m_posImm15 <- lm(posImm_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-1.5*bw_posImm & df$cutoff<1.5*bw_posImm,], 
               weights = weight)

m_salImm15 <- lm(salImm_num ~ postterror + age + genderF + edu + employment + inc,
                 data = df[df$cutoff>-1.5*bw_salImm & df$cutoff<1.5*bw_salImm,], 
                 weights = weight)

m_polTrust15 <- lm(polTrust_num ~ postterror + age + genderF + edu + employment + inc,
                   data = df[df$cutoff>-1.5*bw_polTrust & df$cutoff<1.5*bw_polTrust,], 
                   weights = weight)

m_extreme1115 <- lm(extreme11 ~ postterror + age + genderF + edu + employment + inc,
                  data = df[df$cutoff>-1.5*bw_extreme11 & df$cutoff<1.5*bw_extreme11,], 
                  weights = weight)

m_socInt15 <- lm(socInt_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-1.5*bw_socInt & df$cutoff<1.5*bw_socInt,], 
               weights = weight)

# Double Bandwidth ####

m_socCoh2 <- lm(socCoh_num ~ postterror + age + genderF + edu + employment + inc,
                data = df[df$cutoff>-2*bw_socCoh & df$cutoff<2*bw_socCoh,], 
                weights = weight)

m_posImm2 <- lm(posImm_num ~ postterror + age + genderF + edu + employment + inc,
                 data = df[df$cutoff>-2*bw_posImm & df$cutoff<2*bw_posImm,], 
                weights = weight)

m_salImm2 <- lm(salImm_num ~ postterror + age + genderF + edu + employment + inc,
                data = df[df$cutoff>-2*bw_salImm & df$cutoff<2*bw_salImm,], 
                weights = weight)

m_polTrust2 <- lm(polTrust_num ~ postterror + age + genderF + edu + employment + inc,
                  data = df[df$cutoff>-2*bw_polTrust & df$cutoff<2*bw_polTrust,], 
                  weights = weight)

m_extreme112 <- lm(extreme11 ~ postterror + age + genderF + edu + employment + inc,
                 data = df[df$cutoff>-2*bw_extreme11 & df$cutoff<2*bw_extreme11,], 
                 weights = weight)

m_socInt2 <- lm(socInt_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-2*bw_socInt & df$cutoff<2*bw_socInt,], 
               weights = weight)

# Output: Regression Tables #

texreg(list(m_socCoh_ref, m_socCoh, m_socCoh15, m_socCoh2), 
        file = "./tables/Table_E1.tex",
       float.pos = "htb",
       label = "t_socCoh",
        leading.zero = T,
        stars = c(0.05, 0.01, 0.001),
        custom.model.names = c("Optimal Bandwidth w/o Controls", "Optimal Bandwidth",  "1.5 Bandwidth", "Double Bandwidth"),
        custom.coef.names = c(NA, "Post Terror Dummy", "Age", "Gender", "Education", "Employment", "Income"),
        caption = "Full regression models for Social Cohesion",
        caption.above = T,
        custom.note = "%stars. Note: The optimal bandwidth for social cohesion is 3.33.")

texreg(list(m_socInt_ref, m_socInt, m_socInt15, m_socInt2),
        file = "./tables/Table_E2.tex",
        float.pos = "htb",
        label = "t_socInt",
        leading.zero = T,
        digits = 3,
        stars = c(0.05, 0.01, 0.001),
        custom.model.names = c("Optimal Bandwidth w/o Controls", "Optimal Bandwidth",  "1.5 Bandwidth", "Double Bandwidth"),
        custom.coef.names = c(NA, "Post Terror Dummy", "Age", "Gender", "Education", "Employment", "Income"),
        caption = "Full regression models for Societal Integration",
        caption.above = T,
        custom.note = "%stars. Note: The optimal bandwidth for social cohesion is 2.15.")

texreg(list(m_polTrust_ref, m_polTrust, m_polTrust15, m_polTrust2),
        file = "./tables/Table_E3.tex",
        float.pos = "htb",
        label = "t_polTrust",
        leading.zero = T,
        stars = c(0.05, 0.01, 0.001),
        custom.model.names = c("Optimal Bandwidth w/o Controls", "Optimal Bandwidth",  "1.5 Bandwidth", "Double Bandwidth"),
        custom.coef.names = c(NA, "Post Terror Dummy", "Age", "Gender", "Education", "Employment", "Income"),
        caption = "Full regression models for Political Trust",
        caption.above = T,
        custom.note = "%stars. Note: The optimal bandwidth for political trust is 2.15.")

texreg(list(m_posImm_ref, m_posImm, m_posImm15, m_posImm2),
       file = "./tables/Table_E4.tex",
       float.pos = "htb",
       label = "t_posImm",
        leading.zero = T,
        stars = c(0.05, 0.01, 0.001),
        custom.model.names = c("Optimal Bandwidth w/o Controls", "Optimal Bandwidth",  "1.5 Bandwidth", "Double Bandwidth"),
        custom.coef.names = c(NA, "Post Terror Dummy", "Age", "Gender", "Education", "Employment", "Income"),
        caption = "Full regression models for Immigration Opinions",
        caption.above = T,
        custom.note = "%stars. Note: The optimal bandwidth for attitudes on immigration is 3.92.")

texreg(list(m_salImm_ref, m_salImm, m_salImm15, m_salImm2),
       file = "./tables/Table_E5.tex",
       float.pos = "htb",
       label = "t_salImm",
        leading.zero = T,
        stars = c(0.05, 0.01, 0.001),
        custom.model.names = c("Optimal Bandwidth w/o Controls", "Optimal Bandwidth",  "1.5 Bandwidth", "Double Bandwidth"),
        custom.coef.names = c(NA, "Post Terror Dummy", "Age", "Gender", "Education", "Employment", "Income"),
        caption = "Full regression models for Immigration Salience",
        caption.above = T,
        custom.note = "%stars. Note: The optimal bandwidth for salience of immigration is 3.44.")

texreg(list(m_extreme11_ref, m_extreme11, m_extreme1115, m_extreme112),
        file = "./tables/Table_E6.tex",
        float.pos = "htb",
        label = "t_extreme",
        leading.zero = T,
        stars = c(0.05, 0.01, 0.001),
        custom.model.names = c("Optimal Bandwidth w/o Controls", "Optimal Bandwidth",  "1.5 Bandwidth", "Double Bandwidth"),
        custom.coef.names = c(NA, "Post Terror Dummy", "Age", "Gender", "Education", "Employment", "Income"),
        caption = "Full regression models for Political Polarisation",
        caption.above = T,
        custom.note = "%stars. Note: The optimal bandwidth for political extremism is 3.29.")

## Alternative Bandwidth specificaitons

bw <- rep(c(2:10), 6)
dv <- rep(c("socCoh_num", "socInt_num", "polTrust_num", "posImm_num", "salImm_num", "extreme11"), each=9)
c <- NA
lwr <- NA
upr <- NA

for (i in 1:length(bw)) {

  m <- lm(formula(paste0(dv[i], "~ postterror + age + genderF + inc + edu + employment")),  data = df[(df$cutoff>-bw[i] & df$cutoff<bw[i]),], na.action=na.omit, weights = weight)

  c[i] <- coef(m)["postterrorPost-Terror"]
  lwr[i] <- confint(m)["postterrorPost-Terror", 1]
  upr[i] <- confint(m)["postterrorPost-Terror", 2]
}

df_sensi <- data.frame(dv,bw,c,lwr,upr)

df_sensi$dv <- factor(df_sensi$dv, levels = c("socCoh_num", "socInt_num", "polTrust_num", "posImm_num", "salImm_num", "lrscale11", "extreme11", "left", "right"), labels = c("Social Cohesion", "Societal Integration", "Political Trust", "Immigration Opinions",  "Immigration Salience", "Left-Right Identification", "Political Polarisation", "Political Left", "Political Right"))

p_sensi <- ggplot(subset(df_sensi, dv != "Political Left" & dv != "Political Right"), aes(x=bw, y=c)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  theme_tufte() +
  facet_wrap(~dv, ncol=2, scales = "free_y") +
  geom_hline(yintercept = 0, lty = "dashed") +
  labs(y="The effect of terrorist events", x= "Bandwidth in days")+
  theme(text = element_text(size=20), axis.text.x = element_text(size=14)) +
  scale_y_continuous(limits = c(-1.1,1.1))
p_sensi

ggsave(filename = "./figures/Figure_D1.pdf", width = 12.2, height = 10, unit = "in")
ggsave(filename = "./figures/Figure_D1.jpg", width = 12.2, height = 10, unit = "in")

# Sensibility to Polynomials ####

poly <- rep(c("cutoff*postterror + I(cutoff^2)*postterror +","cutoff*postterror + I(cutoff^2)*postterror + I(cutoff^3)*postterror +", "cutoff*postterror + I(cutoff^2)*postterror + I(cutoff^3)*postterror + I(cutoff^4)*postterror +", "cutoff*postterror + I(cutoff^2)*postterror + I(cutoff^3)*postterror + I(cutoff^4)*postterror + I(cutoff^5)*postterror +"), 6)
dv <- rep(c("socCoh_num", "socInt_num", "polTrust_num", "posImm_num", "salImm_num", "extreme11"), each=4)
c <- NA
lwr <- NA
upr <- NA
pValue <- NA

for (i in 1:length(poly)) {
  
  m <- lm(formula(paste0(dv[i], "~", poly[i], " + age + genderF + inc + edu")),  data = df, na.action=na.omit, weights = weight)
  
  c[i] <- coef(m)["postterrorPost-Terror"]
  lwr[i] <- confint(m)["postterrorPost-Terror", 1]
  upr[i] <- confint(m)["postterrorPost-Terror", 2]
  pValue[i] <- as.numeric(summary(m)$coefficients["postterrorPost-Terror", 4])
}

df_poly <- data.frame(dv,poly,c,lwr,upr, pValue,
                      n_poly = rep(c(2:5), 6))



df_poly$dv <- factor(df_poly$dv, levels = c("socCoh_num", "socInt_num", "polTrust_num", "posImm_num", "salImm_num", "lrscale11", "extreme11", "left", "right"), labels = c("Social Cohesion", "Societal Integration", "Political Trust", "Immigration Opinions", "Immigration Salience", "Left-Right Identification", "Political Polarisation", "Political Left", "Political Right"))

p_poly <- ggplot(df_poly, aes(x=n_poly, y=c)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  theme_tufte() +
  facet_wrap(~dv, ncol = 2, scales = "free_y") +
  geom_hline(yintercept = 0, lty = "dashed") +
  labs(y="The effect of terrorist events", x= "Higher Order Polynomials")+
  theme(text = element_text(size=20), axis.text.x = element_text(size=14)) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  NULL
p_poly

ggsave(filename = "./figures/Figure_D2.pdf", p_poly, width = 12.2, height = 10, unit = "in")
ggsave(filename = "./figures/Figure_D2.jpg", p_poly, width = 12.2, height = 10, unit = "in")

df_poly$ci <- paste0("[", round(df_poly$lwr,3), " ", round(df_poly$upr,3), "]")

df_poly$n_trt <- "1013"

df_poly$n_con <- "2010"

df_poly_plot <- data.frame(df_poly$dv, df_poly$n_poly, round(df_poly$c,3), df_poly$ci, round(df_poly$pValue, 3), df_poly$n_trt, df_poly$n_con)

print(xtable(df_poly_plot, digits = 2), type="latex", file="tables/Table_D1.tex", digits = 2)

#### Descriptive Statistics ####

#Contains all information for Table B.1

describe(df$socCoh == "Very weak")
describe(df$socCoh == "Weak")
describe(df$socCoh == "Strong")
describe(df$socCoh == "Very strong")
describe(df$socCoh == "Don't know")

describe(df$socInt == "Very weak")
describe(df$socInt == "Weak")
describe(df$socInt == "Strong")
describe(df$socInt == "Very strong")
describe(df$socInt == "Don't know")

describe(df$polTrust == "Very weak")
describe(df$polTrust == "Weak")
describe(df$polTrust == "Strong")
describe(df$polTrust == "Very strong")
describe(df$polTrust == "Don't know")

describe(df$posImm == "Very positive")
describe(df$posImm == "Positive")
describe(df$posImm == "Negative")
describe(df$posImm == "Very negative")
describe(df$posImm == "Don't know")

describe(df$salImm == "Very weak")
describe(df$salImm == "Weak")
describe(df$salImm == "Strong")
describe(df$salImm == "Very strong")
describe(df$salImm == "Don't know")

describe(df$extreme11)

describe(df$postterror == "Post-Terror")

describe(df$cutoff)

describe(df$age)

describe(df$genderF == "Female")

describe(df$inc)
describe(is.na(df$inc)==T)

describe(df$edu)

# Balance Tests -----------------------------------------------------------

#Sociodemographics ####

bw_age <- IKbandwidth(df$cutoff, df$age, cutpoint = 0, verbose = T, kernel = "triangular")
bw_genderF <- IKbandwidth(df$cutoff, df$genderF == "Male", cutpoint = 0, verbose = T, kernel = "triangular")
bw_inc <- IKbandwidth(df$cutoff, as.numeric(df$inc), cutpoint = 0, verbose = T, kernel = "triangular")
bw_edu <- IKbandwidth(df$cutoff, df$edu, cutpoint = 0, verbose = T, kernel = "triangular")
bw_empl <- IKbandwidth(df$cutoff, df$employment == "Employed", cutpoint = 0, verbose = T, kernel = "triangular")

m.age <- lm(age~ postterror, data = df[df$cutoff>-bw_age & df$cutoff<bw_age,], weights = weight)
m.gen <- lm(genderF == "Female" ~ postterror, data = df[df$cutoff>-bw_genderF & df$cutoff<bw_genderF,], weights = weight)
m.inc <- lm(as.numeric(inc)~postterror, data = df[df$cutoff>-bw_inc & df$cutoff<bw_inc,], weights = weight)
m.edu <- lm(edu ~ postterror, data = df[df$cutoff>-bw_edu & df$cutoff<bw_edu,], weights = weight)
m.empl <- lm(employment == "Employed" ~ postterror, data = df[df$cutoff>-bw_empl & df$cutoff<bw_empl,], weights = weight)

texreg(list(m.age, m.gen, m.edu, m.empl, m.inc),
       file = "./tables/Table_C1.tex",
       leading.zero = T,
       stars = c(0.05, 0.01, 0.001),
       custom.model.names = c("Age", "Gender",  "Education", "Employment", "Income"),
       custom.coef.names = c(NA, "Post Terror Dummy"),
       caption = "Balance Tests of DREES data",
       label = "t_balance_rdd",
       caption.above = T, float.pos = "htb",
       custom.note = "%stars. Note: We use the following bandwidths (in days): Age (7.52), Gender (3.00), Education (4.24), and Income (6.92). See Table D.1 in the Appendix for full regression tables and goodness of fit indicators.")

# ## Balance tests Alternative ####

df$gender_numeric <- as.numeric(df$genderF)-1
df$edu_numeric <- as.numeric(df$edu)
df$inc_numeric <- as.numeric(df$inc)
df$employment_numeric <- (as.numeric(df$employment) * -1)+2

bw <- rep(c(2:10), 5)
dv <- rep(c("age", "gender_numeric", "edu_numeric", "employment_numeric", "inc_numeric"), each=9)
c <- NA
lwr <- NA
upr <- NA

for (i in 1:length(bw)) {
  
  m <- lm(formula(paste0(dv[i], "~ postterror")),  data = df[(df$cutoff>-bw[i] & df$cutoff<bw[i]),], na.action=na.omit, weights = weight)
  
  c[i] <- coef(m)["postterrorPost-Terror"]
  lwr[i] <- confint(m)["postterrorPost-Terror", 1]
  upr[i] <- confint(m)["postterrorPost-Terror", 2]
}

df_balance <- data.frame(dv,bw,c,lwr,upr)

df_balance$dv <- factor(df_balance$dv, levels = c("age", "gender_numeric", "edu_numeric", "employment_numeric", "inc_numeric"), labels = c("Age", "Gender", "Education", "Employment", "Income"))

p_balance <- ggplot(df_balance, aes(x=bw, y=c)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  theme_tufte() +
  facet_wrap(~dv, ncol=3, scales = "free_y") +
  geom_hline(yintercept = 0, lty = "dashed") +
  labs(y="The effect of terrorist events", x= "Bandwidth in days")+
  theme(text = element_text(size=20), axis.text.x = element_text(size=14)) 

p_balance <- egg::symmetrise_scale(p_balance, axis = "y")

ggsave(filename = "./figures/Figure_C1.pdf", p_balance , width = 12.2, height = 6.16, unit = "in")
ggsave(filename = "./figures/Figure_C1.png", p_balance , width = 12.2, height = 6.16, unit = "in")

# Placebo at other time ####

df_loop <- expand.grid(newCutoff = -3:-20,
                       dv = c("socCoh_num", "posImm_num", "salImm_num", "polTrust_num", "extreme11", "socInt_num"))

c <- NA
s <- NA
p <- NA
bandw <- NA
for (i in 1:nrow(df_loop)) {
  
  df$postterror2 <- factor(ifelse(df$cutoff < df_loop$newCutoff[i], "Pre-Terror", "Post-Terror"),
                           levels = c("Pre-Terror", "Post-Terror"))
  
  df$cutoff2 <- df$cutoff - df_loop$newCutoff[i]
  
  if(testit::has_error(IKbandwidth(df$cutoff2, df[,as.character(df_loop$dv[i])], cutpoint = newCutoff, verbose = FALSE, kernel = "triangular"), silent = T)){
    
    bw <- 3
    
  }  else{
    IKbandwidth(df$cutoff2, df[,as.character(df_loop$dv[i])], cutpoint = newCutoff, verbose = FALSE, kernel = "triangular")
  }
  
  m <- lm(formula = paste0(df_loop$dv[i], "~ postterror2 + age + genderF + edu + employment + inc"),
          data = df[df$cutoff2>-bw & df$cutoff2<bw,], 
          weights = weight)
  
  bandw[i] <- bw
  c[i] <- coef(m)["postterror2Post-Terror"]
  s[i] <- sqrt(vcov(m)["postterror2Post-Terror", "postterror2Post-Terror"])
  p[i] <- summary(m)$coefficients["postterror2Post-Terror",4]  
}

prop.table(table(p < 0.05))

df_plot <- data.frame(cutoff = df_loop$newCutoff,
                      dv = df_loop$dv,
                      c = c,
                      s = s,
                      p = p)

df_plot$dv <- factor(df_plot$dv, levels = c("socCoh_num", "socInt_num", "polTrust_num", "posImm_num", "salImm_num", "lrscale11", "extreme11", "left", "right"), labels = c("Social Cohesion", "Societal Integration", "Political Trust", "Immigration Opinions", "Immigration Salience", "Left-Right Identification", "Political Polarisation", "Political Left", "Political Right"))

p_placebo <- ggplot(df_plot, aes(x=cutoff, y= c, shape = (p < .05), colour = (p < .05))) +
  geom_pointrange(aes(ymin = c - 1.96*s, ymax = c + 1.96*s)) +
  geom_hline(yintercept = 0, lty= "dashed") +
  facet_wrap(~dv,ncol = 2, scales = "free_y") +
  theme_tufte() +
  scale_colour_colorblind("Significance", labels = c("p > 0.05", "p < 0.05")) +
  scale_shape_manual("Significance", values = c(16:17), labels = c("p > 0.05", "p < 0.05")) +
  labs(y="Coefficient size", x= "Number of days prior to the terrorist event") +
  theme(text = element_text(size=20), axis.text.x = element_text(size=14)) +
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(-1.1,1.1))

ggsave(filename = "./figures/Figure_D3.pdf", p_placebo, width = 12.2, height = 6.16, units = "in")
ggsave(filename = "./figures/Figure_D3.png", p_placebo, width = 12.2, height = 6.16, units = "in")

# Placebo with other attitudes ####

bw_salAIDS <- IKbandwidth(df$cutoff, df$salAIDS_num, cutpoint = 0, verbose = T, kernel = "triangular")
bw_posHom <- IKbandwidth(df$cutoff, df$posHom_num, cutpoint = 0, verbose = T, kernel = "triangular")
bw_socPro <- IKbandwidth(df$cutoff, df$socPro_num, cutpoint = 0, verbose = T, kernel = "triangular")
bw_healthcare <- IKbandwidth(df$cutoff, df$healthcare_num, cutpoint = 0, verbose = T, kernel = "triangular")
bw_socSec1 <- IKbandwidth(df$cutoff, df$socSec1_num, cutpoint = 0, verbose = T, kernel = "triangular")
bw_socSec2 <- IKbandwidth(df$cutoff, df$socSec2_num, cutpoint = 0, verbose = T, kernel = "triangular")
bw_perceived_health <- IKbandwidth(df$cutoff, df$perceived_health_num, cutpoint = 0, verbose = T, kernel = "triangular")


m_salAIDS <- lm(salAIDS_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_salAIDS & df$cutoff<bw_salAIDS,], 
               weights = weight)

m_posHom <- lm(posHom_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_posHom & df$cutoff<bw_posHom,], 
               weights = weight)

m_socPro <- lm(socPro_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_socPro & df$cutoff<bw_socPro,], 
               weights = weight)

m_healthcare <- lm(healthcare_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_healthcare & df$cutoff<bw_healthcare,], 
               weights = weight)

m_socSec1 <- lm(socSec1_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_socSec1 & df$cutoff<bw_socSec1,], 
               weights = weight)

m_socSec2 <- lm(socSec2_num ~ postterror + age + genderF + edu + employment + inc,
               data = df[df$cutoff>-bw_socSec2 & df$cutoff<bw_socSec2,], 
               weights = weight)

m_perceived_health <- lm(perceived_health_num ~ postterror + age + genderF + edu + employment + inc,
                data = df[df$cutoff>-bw_perceived_health & df$cutoff<bw_perceived_health,], 
                weights = weight)

texreg::texreg(list(m_salAIDS, m_posHom, m_socPro, m_healthcare, m_socSec1, m_socSec2, m_perceived_health),
                file = "./tables/Table_D2.tex",
                custom.model.names = c("Salience AIDS", "Position Same-Sex Marriage", "Social Security works well?", "Should healthcare remain public?", "Social security is sufficient", "Social security is to expensive", "Perceived Health"),
                custom.coef.names = c(NA, "Post Terror Dummy", "Age", "Gender", "Education", "Employment", "Income"),
                leading.zero = T,
               caption = c("Placebo tests with other variables"), caption.above = T,
               label = "t_placebo_altVar",
                digits = 2, float.pos = "htb", sideways = T,
                stars = c(0.05, 0.01, 0.001), custom.note = "%stars. Note: We use the following bandwidths (in days): Salience AIDS (2.50), Position Same-Sex Marriage (2.94), Social Security works well? (3.78), Should healthcare remain public? (3.21), Social security is sufficient (3.23), Social security is to expensive (2.94), and Perceived Health (2.84).")

# Duration of effect ####

df_loop <- expand.grid(startWindow = 1:12, 
                       dv = c("socCoh_num", "posImm_num", "salImm_num", "polTrust_num", "extreme11", "socInt_num"))

df_loop$endWindow <- df_loop$startWindow + 5

c <- NA
s <- NA
p <- NA


for (i in 1:nrow(df_loop)) {
  
df_temp <- subset(df, cutoff %in% c(-5:-1, df_loop$startWindow[i]: df_loop$endWindow[i]))
  
  m <- lm(formula = paste0(df_loop$dv[i], "~ postterror + age + genderF + edu + employment + inc"),
          data = df_temp, 
          weights = weight)
  
  c[i] <- coef(m)["postterrorPost-Terror"]
  s[i] <- sqrt(vcov(m)["postterrorPost-Terror", "postterrorPost-Terror"])
  p[i] <- summary(m)$coefficients["postterrorPost-Terror",4]  
}

df_loop$c <- c
df_loop$s <- s
df_loop$p <- p

df_loop$dv <- factor(df_loop$dv, levels = c("socCoh_num", "socInt_num", "polTrust_num", "posImm_num", "salImm_num", "lrscale11", "extreme11", "left", "right"), labels = c("Social Cohesion", "Societal Integration", "Political Trust", "Immigration Opinions", "Immigration Salience", "Left-Right Identification", "Political Polarisation", "Political Left", "Political Right"))

p_duration <- ggplot(df_loop, aes(x= startWindow, y= c))+
  geom_pointrange(aes(ymin = c - 1.96*s, ymax = c + 1.96*s, shape = (p < .05), colour = (p < .05))) +
  geom_hline(yintercept = 0, lty= "dashed") +
  facet_wrap(~dv,ncol = 2, scales = "free_y") +
  theme_tufte() +
  scale_colour_colorblind("Significance", labels = c("p > 0.05", "p < 0.05")) +
  scale_shape_manual("Significance", values = c(16:17), labels = c("p > 0.05", "p < 0.05")) +
  scale_x_continuous(breaks = c(1:12), labels = paste0("t+", 1:12)) +
  labs(y="Coefficient size", x= "Start day of five-day window") +
  theme(text = element_text(size=20), axis.text.x = element_text(size=14)) +
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(-.45, .45))

ggsave(filename = "./figures/Figure_G1.pdf", p_duration, width = 12.2, height = 6.16, units = "in")
ggsave(filename = "./figures/Figure_G1.jpg", p_duration, width = 12.2, height = 6.16, units = "in")

#END OF SCRIPT