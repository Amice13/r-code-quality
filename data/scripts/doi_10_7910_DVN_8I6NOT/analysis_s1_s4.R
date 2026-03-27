############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code analyses the data from Studies 1-4  #
#       for things reported in main text        #
#                                               #
# 2020.06.24.                                   #
# code by Alexander Bor                         #
############################################### #

# import and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load("stargazer", "here", "gridExtra", "weights", "tidyverse",
               "jtools", "clubSandwich", "lm.beta")


# source imports 
source("C.code/import_s1_US_2018.R")
source("C.code/import_s2_DK_2019.R")
source("C.code/import_s3_lucid.R")
source("C.code/import_s4_s5_US_2021.R")

# source my own custom functions
source("C.code/functions.R")

# load data
df.us <- readRDS(here("B.analysis.data", "s1_us_2018.rds"))      
df.dk <- readRDS(here("B.analysis.data", "s2_dk_2019.rds"))    
df.us2 <- readRDS(here("B.analysis.data", "s3_us_2019.rds"))
df.us3.part <- readRDS(here("B.analysis.data", "s4_us_2021.rds"))  %>% 
        filter(part_online > 0 & part_offline > 0)  


########## ########## ########## ########## ########## ########## ######### #
# 1. Hostility gap -- difference in tone on vs off                     ###########
########## ########## ########## ########## ########## ########## ######### #

# t tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tone.t.sum <- rbind(
        t.sum(df.us$tone_offline_neg, df.us$tone_online_neg, 
              "Offline", "Online", "Tone - US", paired = T),
        t.sum(df.dk$tone_offline_neg, df.dk$tone_online_neg,
              "Offline", "Online", "Tone - Denmark", paired = T)
)
row.names(tone.t.sum) <- NULL


stargazer(tone.t.sum, summary = F, digits = 2,
          title = "T-test between positive online and offline tone ratings",
          out = "D.documents/tables/backup_tc1_tonetest.html", 
          type = "text")

# calculate Cohen's d
tone.t.sum$diff[1] / sd(df.us$tone_offline_neg - df.us$tone_online_neg, na.rm = T)
tone.t.sum$diff[2] / sd(df.dk$tone_offline_neg - df.dk$tone_online_neg, na.rm = T)

# ~ Fig 1 code ------------------------------------------------------------

# US 

# make neat long df with only tone ratings
p1.us <- df.us %>%  
        select(tone_offline_neg, tone_online_neg) %>% 
        gather(environment, value, 1:2) %>% 
        mutate(environment = fct_recode(environment, 
                                        Offline = "tone_offline_neg",
                                        Online = "tone_online_neg"), 
               environment = fct_rev(environment)) %>% 
# create actual plot 
ggplot(., aes(x = value, fill = environment)) + 
        geom_density(alpha = 0.6) + 
        xlab("") + 
        ylab("Density") + 
        scale_fill_grey(guide = FALSE) + 
        ggtitle("A. USA") +
        theme_bw()
# p1.us


# DK

# make neat long df with only tone ratings
p1.dk <- df.dk %>%  
        select(tone_offline_neg, tone_online_neg) %>% 
        gather(environment, value, 1:2) %>% 
        mutate(environment = fct_recode(environment, 
                                        Offline = "tone_offline_neg",
                                        Online = "tone_online_neg"), 
               environment = fct_rev(environment)) %>% 
        # create actual plot 
        ggplot(., aes(x = value, fill = environment)) + 
        geom_density(alpha = 0.6) + 
        xlab("") + 
        ylab("") + 
        scale_fill_grey(name = NULL) + 
        ggtitle("B. Denmark") +
        theme_bw()

# p1.dk

# combine the two panels 
p1 <- arrangeGrob(p1.us, p1.dk, ncol = 2, widths = c(0.45, 0.55))

ggsave(p1, file = "D.documents/figures/fig1_tone.jpg", width = 7, height = 4)


########## ########## ########## ########## ########## ########## ######### #
# 2. Change hypothesis -- correlation of online and offline hostility  #########
########## ########## ########## ########## ########## ########## ######### #


# prevalence of hostility 

mean.sd(df.us$hostile_online)
mean.sd(df.dk$hostile_online)
mean.sd(df.us2$hostile_online)
mean.sd(df.us3.part$hostile_online)

mean.sd(df.us$hostile_offline)
mean.sd(df.dk$hostile_offline)
mean.sd(df.us2$hostile_offline)
mean.sd(df.us3.part$hostile_offline)

# prop of 0s 
mean(df.us$hostile_online == 0)
mean(df.dk$hostile_online == 0, na.rm = T)
mean(df.us2$hostile_online == 0)
mean(df.us3.part$hostile_online == 0, na.rm = T )

mean(df.us$hostile_offline == 0)
mean(df.dk$hostile_offline == 0, na.rm = T)
mean(df.us2$hostile_offline == 0)
mean(df.us3.part$hostile_offline == 0, na.rm = T )


# regressions 

fit2.us <- lm(hostile_online ~ hostile_offline, df.us)

fit2.dk <- lm(hostile_online ~ hostile_offline, df.dk)

fit2.us2 <- lm(hostile_online ~ hostile_offline, df.us2)

fit2.us3 <- lm(hostile_online ~ hostile_offline, df.us3.part)


stargazer(fit2.us, fit2.dk, fit2.us2, fit2.us3,
          covariate.labels = c("Offline hostility", "Intercept"), 
          dep.var.labels = "Online hostility", 
          out = here("D.documents/tables/tc1_hostile_symm.html"),
          column.labels = c("USA", "Denmark", "USA", "USA"),
          # column.separate = c(2,2),
          keep.stat = c("n", "rsq"),
          single.row = T,
          title = "Table C1. Regressing online hostility on offline hostility in our three studies",
          type = "text")



# ~ Fig 2 code ------------------------------------------------------------

p2.us <- effect_plot(fit2.us, pred = "hostile_offline", interval = T, 
                     plot.points = T, jitter = 0.02, point.size = 0.6,
                     point.alpha = 0.3,
                     x.label = "Offline hostility", y.label = "Online hostility", 
                     main.title = "S1. USA") + 
        geom_abline(aes(intercept = 0, slope = 1))

p2.dk <- effect_plot(fit2.dk, pred = "hostile_offline", interval = T, 
                     plot.points = T, jitter = 0.02, point.size = 0.6,
                     point.alpha = 0.3,
                     x.label = "Offline hostility", y.label = "Online hostility", 
                     main.title = "S2. Denmark") + 
        geom_abline(aes(intercept = 0, slope = 1))

p2.us2 <- effect_plot(fit2.us2, pred = "hostile_offline", interval = T, 
                     plot.points = T, jitter = 0.02, point.size = 0.6,
                     point.alpha = 0.3,
                     x.label = "Offline hostility", y.label = "Online hostility", 
                     main.title = "S3. USA") + 
        geom_abline(aes(intercept = 0, slope = 1))

# grid.arrange(p2.us, p2.dk, p2.us2, ncol = 3)
p2 <- arrangeGrob(p2.us, p2.dk, p2.us2, ncol = 3)

ggsave(p2, file = "D.documents/figures/fig2_hostile_symm_new.jpg", 
       width = 7, height = 3)


# ~ Fig 5 code ------------------------------------------------------------


# study 4 is in a separate section. but plot looks identical
p2.us3 <- effect_plot(fit2.us3, pred = "hostile_offline", interval = T, 
                      plot.points = T, jitter = 0.02, point.size = 0.6,
                      point.alpha = 0.3,
                      x.label = "Offline hostility", y.label = "Online hostility", 
                      main.title = "S4. USA") + 
        geom_abline(aes(intercept = 0, slope = 1))

ggsave(p2.us3, file = "D.documents/figures/fig5_hostile_s4.jpg", 
       width = 7, height = 4)

########## ########## ########## ########## ########## ########## ######### #
# 3. Change hypothesis -- hostility and personality measures                       ##########
########## ########## ########## ########## ########## ########## ######### #


# ~ 3.1 Models ----------------------------------------------------------------

# Study 1 USA ~~~~~~~~~~~~~
# Online  

fit3.1.us <- lm(hostile_online ~ sdrt + 
                         female + age + white + 
                         highered + income + pid, df.us)

fit3.2.us <- lm(hostile_online ~ ders + 
                 female + age + white + 
                 highered + income + pid, df.us)

fit3.3.us <- lm(hostile_online ~ sdrt + ders + 
                 female + age + white + 
                 highered + income + pid, df.us)


# Offline  

fit3.4.us <- lm(hostile_offline ~ sdrt + 
                        female + age + white + 
                        highered + income + pid, df.us)

fit3.5.us <- lm(hostile_offline ~ ders + 
                        female + age + white + 
                        highered + income + pid, df.us)

fit3.6.us <- lm(hostile_offline ~ sdrt + ders + 
                        female + age + white + 
                        highered + income + pid, df.us)
 
# Study 2 Denmark ~~~~~~~~~~~~~
# Online  

fit3.1.dk <- lm(hostile_online ~ sdrt + 
                     female + age + highered + 
                        income + pid, df.dk)

fit3.2.dk <- lm(hostile_online ~ ders + 
                     female + age + highered + 
                        income + pid, df.dk)

fit3.3.dk <- lm(hostile_online ~ sdrt + ders + 
                     female + age + highered + 
                        income + pid, df.dk)
# Offline  

fit3.4.dk <- lm(hostile_offline ~ sdrt + 
                        female + age + highered + 
                        income + pid, df.dk)

fit3.5.dk <- lm(hostile_offline ~ ders + 
                        female + age + highered + 
                        income + pid, df.dk)

fit3.6.dk <- lm(hostile_offline ~ sdrt + ders + 
                                female + age + highered +
                                income + pid, df.dk)


# Study 4 US ~~~~~~~~~~~~~~~~~


# Online

fit3.9.us <- lm(hostile_online ~ sdrt + 
                        female + age + white + 
                        highered + income + pid, df.us3.part)

fit3.10.us <- lm(hostile_online ~ ders + 
                         female + age + white + 
                         highered + income + pid, df.us3.part)

fit3.11.us <- lm(hostile_online ~ sdrt + ders + 
                         female + age + white + 
                         highered + income + pid, df.us3.part)



# Offline

fit3.12.us <- lm(hostile_offline ~ sdrt + 
                         female + age + white + 
                         highered + income + pid, df.us3.part)

fit3.13.us <- lm(hostile_offline ~ ders + 
                         female + age + white + 
                         highered + income + pid, df.us3.part)

fit3.14.us <- lm(hostile_offline ~ sdrt + ders + 
                         female + age + white + 
                         highered + income + pid, df.us3.part)


# Export main findings for appendix 
stargazer(fit3.1.us, fit3.4.us,fit3.1.dk, fit3.4.dk, fit3.9.us, fit3.12.us,
          dep.var.labels = rep(c("Online", "Offline"), 3),
          covariate.labels = c("SDRT", "Female", "Age",
                               "White", "Higher educated", "Income", "US - Party ID",
                               "DK - Party ID:Other", "DK - Party ID:Red block"),
          column.labels = c("S1. United States", "S2. Denmark", "S4. United States"),
          column.separate = c(2,2,2 ),
          keep.stat = c("n", "adj.rsq"),
          digits = 2, 
          single.row = T,
          # notes.align = "l",
          report = "vcs*",
          title = "Table C2. Regressing self-reported hostility on SDRT and demographic covariates",
          out = "D.documents/tables/tc2_hostile_psych.html",
          type = "text")

# Export all models for OSF repo
stargazer(fit3.1.us, fit3.4.us, fit3.2.us, fit3.5.us, fit3.3.us, fit3.6.us,
          fit3.1.dk, fit3.4.dk, fit3.2.dk, fit3.5.dk, fit3.3.dk, fit3.6.dk,
          fit3.9.us, fit3.12.us,   fit3.10.us, fit3.13.us,fit3.11.us,  fit3.14.us,
          dep.var.labels = rep(c("Online", "Offline"), 9),
          covariate.labels = c("SDRT", "DERS", "Female", "Age",
                               "White", "Higher educated", "Income", "US - Party ID",
                               "DK - Party ID:Other", "DK - Party ID:Red block"),
          column.labels = c("S1. United States", "S2. Denmark", "S4. United States"),
          column.separate = c(6,6,6),
          keep.stat = c("n", "adj.rsq"),
          out = "D.documents/tables/backup_t1_all_hostile_psych.html",
          type = "text")



# ~ Fig 3 code ------------------------------------------------------------


pred3.us.sdrt <- bind_rows(
        predicted.plot(fit3.1.us, country = "us", key = "sdrt"), 
        predicted.plot(fit3.4.us, country = "us", key = "sdrt")) 
# note, warnings about unknown levels and coerced characters and vectors are fine 



pred3.dk.sdrt <- bind_rows(
        predicted.plot(fit3.1.dk, country = "dk", key = "sdrt"), 
        predicted.plot(fit3.4.dk, country = "dk", key = "sdrt")) 


pred3.sdrt <- pred3.dk.sdrt %>% select(-pid) %>% 
        bind_rows(pred3.us.sdrt)

plot3.sdrt<- ggplot(pred3.sdrt, aes(x = sdrt, y = fit, fill = env)) + 
        # geom_line(aes(color = env)) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) + 
        xlab("Status-Driven Risk Taking") + 
        scale_fill_grey(name = NULL, start = 0, end = 0.5) + 
        ylab("Hostility") + 
        # ylim(0.08, 0.52) +
        # ggtitle("A. USA") + 
        facet_grid(~ fct_rev(country)) +
        theme_minimal()

ggsave(plot3.sdrt, file = "D.documents/figures/fig3_hostile_sdrt.jpg", 
       width = 7.5, height = 3.75)

# Note: the reason for the offline line to have a lower max value is that there is 
#       a respondent with extreme SDRT but missing value on offline hostility.


# Again, because Study 4 is a separate section, we don't combine the plots with the above
#       But we report that we fail to replicate the selection effect from above. 

pred3.us3.sdrt <- bind_rows(
        predicted.plot(fit3.11.us, country = "us3", key = "sdrt"), 
        predicted.plot(fit3.14.us, country = "us3", key = "sdrt")) %>% 
        mutate(country = "USA 2021")

ggplot(pred3.us3.sdrt, aes(x = sdrt, y = fit, fill = env)) + 
        # geom_line(aes(color = env)) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) + 
        xlab("Status-Driven Risk Taking") + 
        scale_fill_grey(name = NULL, start = 0, end = 0.5) + 
        ylab("Hostility") + 
        # ylim(0.08, 0.52) +
        # ggtitle("A. USA") + 
        facet_grid(~ fct_rev(country)) +
        theme_minimal()


########## ########## ########## ########## ########## ########## ######### #
# 4. Selection hypothesis -- talking politics and personality.    ###########
########## ########## ########## ########## ########## ########## ######### #


# ~ 4.1 Models ------------------------------------------------------------

# Study 1 - USA 
fit4.1.us <- lm(talk_online ~ sdrt + 
                        female + age + white + 
                        highered + income + pid, df.us)

fit4.2.us <- lm(talk_offline ~ sdrt + 
                      female + age + white + 
                      highered + income + pid, df.us)

# Study 2 - Denmark
fit4.1.dk <- lm(talk_online ~ sdrt + 
                        female + age + highered + 
                        income + pid, df.dk)

fit4.2.dk <- lm(talk_offline ~ sdrt + 
                        female + age + highered + 
                        income + pid, df.dk)

# Study 4 - USA 

fit4.3.us <- lm(talk_online ~ sdrt + 
                        female + age + white + 
                        highered + income + pid, df.us3.part)

fit4.4.us <- lm(talk_offline ~ sdrt + 
                        female + age + white + 
                        highered + income + pid, df.us3.part)




stargazer(fit4.1.us, fit4.2.us, fit4.1.dk, fit4.2.dk, fit4.3.us, fit4.4.us,
          title = "Table C3. Talking about politics regressed on status drive and covariates",
          dep.var.labels = rep(c("Online", 
                                 "Offline"), 3),
          covariate.labels = c("SDRT", "Female", "Age",
                               "White", "Higher educated", "Income", "US - Party ID",
                               "DK - Party ID:Other", "DK - Party ID:Red block"),
          column.labels = c("S1.United States", "S2.Denmark", "S4.United States"),
          column.separate = c(2,2,2),
          keep.stat = c("n", "adj.rsq"),
          digits = 2, 
          # single.row = TRUE,
          notes.align = "l",
          # report = "vcs*",
          out = "D.documents/tables/tc3_talk_sdrt.html",
          type = "text")


# ~ Fig 4 code ------------------------------------------------------------


# new data for plot 4
nd4.us <- data.frame(
        # sequence along the range of SDRT
        sdrt = seq(round(min(df.us$sdrt), 2), 
                   round(max(df.us$sdrt), 2), 
                   0.05), 
        female = mean(df.us$female),  # covariates are set to their mean
        age = mean(df.us$age), 
        highered = mean(df.us$highered), 
        income = mean(df.us$income, na.rm = T), 
        pid = mean(df.us$pid), 
        white = mean(df.us$white)
)


nd4.dk <- data.frame(
        # sequence along the range of SDRT
        sdrt = seq(round(min(df.dk$sdrt), 2), 
                   round(max(df.dk$sdrt), 2), 
                   0.05), 
        female = mean(df.dk$female),  # covariates are set to their mean
        age = mean(df.dk$age), 
        highered = mean(df.dk$highered), 
        income = mean(df.dk$income, na.rm = T), 
        pid = "Red_block"
)


# predict 
pred4.us <- rbind(
        cbind(nd4.us, 
              predict(fit4.1.us, newdata = nd4.us, interval = "confidence"),
              env = "Online", country = "USA"), 
        cbind(nd4.us, 
              predict(fit4.2.us, newdata = nd4.us, interval = "confidence"),
              env = "Offline", country = "USA"))

pred4.dk <- rbind(
        cbind(nd4.dk, 
              predict(fit4.1.dk, newdata = nd4.dk, interval = "confidence"),
              env = "Online", country = "Denmark"), 
        cbind(nd4.dk, 
              predict(fit4.2.dk, newdata = nd4.dk, interval = "confidence"),
              env = "Offline", country = "Denmark")) 


pred4 <- pred4.dk %>% select(-pid) %>% 
        bind_rows(pred4.us)




ggplot(pred4, aes(x = sdrt, y = fit, fill = env)) + 
        # geom_line(aes(color = env)) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) + 
        xlab("Status Driven Risk Taking") + 
        scale_fill_grey(name = NULL, start = 0, end = 0.5) + 
        ylab("Talking about politics") + 
        # ylim(0.08, 0.52) +
        # ggtitle("A. USA") + 
        facet_grid(~ fct_rev(country)) +
        theme_minimal()
ggsave(file = "D.documents/figures/fig4_talk_sdrt.jpg", 
       width = 7, height = 4)

wtd.t.test(x = df.us$sdrt > median(df.us$sdrt), 
           y = df.us$sdrt > median(df.us$sdrt),
           weight = df.us$talk_online, 
           weighty = df.us$talk_offline, 
           bootse = T)

0.068/sd(df.us$sdrt > median(df.us$sdrt))

with(filter(df.dk, !is.na(talk_online) & !is.na(talk_offline)), 
     wtd.t.test(x = sdrt > median(sdrt), y = sdrt > median(sdrt), 
                weight = talk_online, weighty = talk_offline, 
                bootse = T))
0.0564/sd(df.dk$sdrt > median(df.dk$sdrt))

########## ########## ########## ########## ########## ########## ######### #
# 5. Perception hypothesis ############################################################
########## ########## ########## ########## ########## ########## ######### #


conflict.t.sum <- rbind(
        tost.sum(df.dk$conflict_all_online,  df.dk$conflict_all_offline, 
              xname = "online", yname = "offline", name = "S2 - DK"),
        tost.sum(df.us2$perc_on, df.us2$perc_off,
              xname = "online", yname = "offline", name = "S3 - US"))

row.names(conflict.t.sum) <- NULL

stargazer(conflict.t.sum, summary = F, digits = 2,
          title = "Table C6. Perception scale and subscales online and offline",
          out = "D.documents/tables/tc6_conflict_perception.html",
          type = "text")



# ~ frequency of disagreeable political discussions


frequent.t.sum <- rbind(
        t.sum(df.us$talk_online5, df.us$talk_offline5, paired = T,
                 xname = "online", yname = "offline", name = "S1 - US"),
        t.sum(df.dk$talk_online5,  df.dk$talk_offline5, paired = T,
                 xname = "online", yname = "offline", name = "S2 - DK"), 
        t.sum(df.us3.part$talk_online_5,  df.us3.part$talk_offline_5, paired = T,
              xname = "online", yname = "offline", name = "S4a - US")
        
        )

row.names(frequent.t.sum) <- NULL

stargazer(frequent.t.sum, summary = F, digits = 2,
          title = "Table C7. Frequency of disagreeable political discussions",
          out = "D.documents/tables/tc6_conflict_perception.html",
          type = "text")
########## ########## ########## ########## ########## ########## ######### #
# 6. Connectivity hypothesis - exposure to hostility ##############################
# ########## ########## ########## ########## ########## ########## ######### #

df.off.dk <- df.dk %>% 
        mutate(id = 1:nrow(df.dk)) %>% 
        dplyr::select(contains("offended")[1:6], id) %>% 
        gather(variable, value, -id) %>% 
        separate(variable, c("environment", "target"), -1) %>% 
        mutate(target = fct_recode(target, Self = "1", Friends = "2", 
                                   Strangers = "3"),
               target = fct_relevel(target, "Strangers"),
               # environment = fct_rev(environment), 
               environment = fct_recode(environment, 
                                        Online = "offended_online", 
                                        Offline = "offended_offline")) 
        # glimpse


# run model 
fit6.dk <- lm(value ~ environment * target, df.off.dk)

# cluster standard errors by respondents
fit6.dk.cluster <- coef_test(fit6.dk, vcov = "CR1", 
          cluster = df.off.dk$id)
# https://cran.r-project.org/web/packages/clubSandwich/vignettes/panel-data-CRVE.html 

# summary(fit6.dk)


witness_on <- paste0("witness_on_", 1:3)
witness_off <- paste0("witness_off_", 1:3)
df.off.us <- df.us2 %>% 
        dplyr::select(witness_on, witness_off, id) %>% 
        gather(target, value, witness_on_1:witness_off_3) %>%
        separate(target, c("witness", "environment", "target"), sep = "_") %>% 
        mutate(target = case_when(target == 1 ~ "Self", 
                                  target == 2 ~ "Friends", 
                                  target == 3 ~ "Strangers"),
               target = fct_relevel(target, "Strangers"),
               environment = fct_recode(environment, 
                                        Online = "on", 
                                        Offline = "off")) 


fit6.us <- lm(value ~ environment * target, df.off.us)
summary(fit6.us)
fit6.us.cluster <- coef_test(fit6.us, vcov = "CR1", 
                             cluster = df.off.us$id)

stargazer(fit6.us, fit6.dk, 
          covariate.labels = c("Online environment", 
                               "Target: Friends", "Target: Self", 
                               "Online × Friends", "Online × Self"), 
          column.labels = c("USA", "Denmark"),
          dep.var.labels = "Witnessing hostility", 
          se = list(fit6.us.cluster$SE, fit6.dk.cluster$SE),
          digits = 2,
          single.row = T,
          report = "vcs*",
          title = "Table C4. Likelihood of witnessing hostility online and offline against various parties", 
          keep.stat = c("adj.rsq", "n"), 
          out = "D.documents/tables/tc4_witness.html",
          type = "text")


# witnessing offenses in Denmark ~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~


df.offplot.dk <- df.off.dk %>% 
        group_by(target, environment) %>% 
        summarise(mean = mean(value, na.rm = T), 
                  lwr = mean + 1.97 * se(value), 
                  upr = mean - 1.97 * se(value))



# witnessing offenses in US / Lucid ~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~ ~~~~~~~~~~~

df.offplot.us <- df.off.us %>% 
        group_by(environment, target) %>% 
        summarise(mean = mean(value, na.rm = T), 
                  lwr = mean + 1.97 * se(value), 
                  upr = mean - 1.97 * se(value))

# merge two countries 
df.offplot <- bind_rows(USA = df.offplot.us, Denmark = df.offplot.dk, 
                        .id = "country") %>% 
        mutate(country = fct_relevel(country, "USA"))


# ~ Fig 6 code -----------------------------------------------------------------

ggplot(df.offplot, aes(x = fct_relevel(target, "Self", "Friends"), 
                       y = mean, color = fct_rev(environment),
                       group = fct_rev(environment))) + 
        geom_point(position = position_dodge(.1)) + 
        geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
                      position = position_dodge(.1)) + 
        geom_line()+
        ylim(0, 0.5) + 
        ylab("Mean values") + 
        xlab("") + 
        facet_grid(~ country) + 
        scale_color_grey(name = NULL, 
                         start = 0, end = 0.6,
                         guide = guide_legend(reverse = TRUE)) + 
        geom_hline(yintercept = 0) +
        theme_minimal() +
        theme(legend.position="bottom")
ggsave("D.documents/figures/fig6_offended.jpg", width = 6.5, height = 3.5)
