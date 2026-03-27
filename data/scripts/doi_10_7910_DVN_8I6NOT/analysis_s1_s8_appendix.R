############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code analyses data from all studies      #
#       for supplementary stuff                 #
#       reported in the Online Appendix         #
#                                               #
# 2021.03.24.                                   #
# code by Alexander Bor                         #
############################################### #

if (!require("pacman")) install.packages("pacman")
pacman::p_load("stargazer", "here", "gridExtra", "rio", "tidyverse",
               "psych", "lavaan", "TOSTER", "interactions", "patchwork")

# import data for s6 and s7 (if it hasn't been done yet)
source("C.code/import_s6_s7_MTurk.R")

source("C.code/functions.R")


# load data
df.us <- readRDS(here("B.analysis.data/s1_us_2018.rds"))      
df.dk <- readRDS(here("B.analysis.data/s2_dk_2019.rds"))    
df.us2 <- readRDS(here("B.analysis.data/s3_us_2019.rds"))
df.us3 <- readRDS(here("B.analysis.data/s4_us_2021.rds"))
df.us3.part <- df.us3 %>% filter(part_online > 0 & part_offline > 0) 
df4 <- readRDS(file = "B.analysis.data/s6_exp.rds")
df5 <- readRDS("B.analysis.data/s7_crowd.rds")


# Sample characteristics #######################################################

mean.sd <- function(x){
        avg <-round(mean(x, na.rm = T), 2)
        sd <- sprintf("%.2f", round(sd(x, na.rm = T), 2))
        paste(avg, " (", sd, ")", sep = '')
}

sample.us <- df.us %>% 
        summarise(age = mean.sd(age_year), 
                  female = mean.sd(female), 
                  white = mean.sd(white), 
                  highered = mean.sd(highered),
                  democrat = mean.sd(pid < 0.34), 
                  republican = mean.sd(pid >= 0.66),
                  N = n())
       
sample.dk <- df.dk %>% 
        summarise(age = mean.sd(age_year), 
                  female = mean.sd(female), 
                  highered = mean.sd(highered),
                  blue_block = mean.sd(pid == "Blue_block"),
                  red_block = mean.sd(Party_red), 
                  N = n())
                  
sample.us2 <- df.us2 %>% 
        summarise(age = mean.sd(age), 
                  female = mean.sd(female), 
                  white = mean.sd(white), 
                  highered = mean.sd(highered),
                  democrat = mean.sd(pid == "Democrat"), 
                  republican = mean.sd(pid == "Republican"),
                  N = n())

sample.us3a <- df.us3.part %>% 
        summarise(age = mean.sd(age), 
                  female = mean.sd(female), 
                  white = mean.sd(white), 
                  highered = mean.sd(highered),
                  democrat = mean.sd(pid < 0.34), 
                  republican = mean.sd(pid >= 0.66),
                  N = n())

sample.us3b <- df.us3 %>% 
        summarise(age = mean.sd(age), 
                  female = mean.sd(female), 
                  white = mean.sd(white), 
                  highered = mean.sd(highered),
                  democrat = mean.sd(pid < 0.34), 
                  republican = mean.sd(pid >= 0.66),
                  N = n())

                  
sample.s4 <- df4 %>% 
		filter(attentive != "not") %>%
        summarise(age = mean.sd(age_year), 
                  female = mean.sd(female), 
                  highered = mean.sd(highered),
                  democrat = mean.sd(pid == 1), 
                  republican = mean.sd(pid == 2),
                  N = n())

sample.s5 <- df5 %>% 
        summarise(age = mean.sd(age_year), 
                  female = mean.sd(female), 
                  highered = mean.sd(highered),
                  democrat = mean.sd(pid == 1), 
                  republican = mean.sd(pid == 2),
                  N = n())

             
samples <- bind_rows(sample.us, sample.dk, sample.us2, 
                     sample.us3a, sample.us3b,
                     sample.s4, sample.s5) %>%
        # order the variables nicely 
		dplyr::select(age, female, white, highered, democrat, republican,
		blue_block, red_block, N)%>%
        # transpose so cols refer to samples.
		t() %>%
        # convert to data frame.
		data.frame()
		
names(samples) <- c("S1.USA.YouGov", "S2.Denmark.YouGov", "S3.USA.Lucid", 
                    "S4.USA.YouGov", "S5.USA.YouGov",
                    "S6.USA.MTurk", "S7.USA.MTurk")
rownames(samples) <- c("Age", "Female", "White", "Higher educated", 
						"Party ID: Democrat", "Party ID: Republican", 
						"Party ID: Blue block", "Party ID: Red block", 
						"N")
stargazer(samples, summary = F, 
          title = "Table B1. Sample demographics",
          out = "D.documents/tables/tb1_samples.html",
          type = "text")


# Descriptives on main variables ------------------------------------------

cor.matrix <- function(x){
        require(Hmisc)
        
        # calculate correlations and p values
        cor_table <- rcorr(as.matrix(x))
        
        # calculate descriptives
        out <- data.frame(
                num = 1:ncol(x),
                          # names = names(x),
                          mean = round(apply(x, 2, mean, na.rm = T),2),
                          sd =   round(apply(x, 2, sd, na.rm = T ), 2))
        
        # merge r estimates with significance signs
        # cor_matrix <- matrix(paste0(round(cor_table$r, 2), signs), nrow = nrow(signs))
        cor_matrix <- round(cor_table$r, 2)
        cor_matrix[!lower.tri(cor_matrix)] <- " "
        diag(cor_matrix) <- "-"
        
        # replace col heads with numbers
        attributes(cor_matrix)$dimnames[[2]] <- out$num
        
        # combine matrix to output
        out <- cbind(out, cor_matrix)
        out
}


df.us %>% 
        transmute(
                `Hostile Offline` = hostile_offline, 
                `Hostile Online` = hostile_online, 
                `Talk Offline` = talk_offline, 
                `Talk Online` = talk_online, 
                `SDRT` = sdrt, 
                `Trait Aggression` = aggression
        ) %>% 
        cor.matrix %>% 

        stargazer(., summary = F, 
          title = "Table B3a. Descriptives S1 USA",
          digits = 2,
          out = "D.documents/tables/tb3_descript_s1.html",
          type = "text")

df.dk %>% 
        transmute(
                `Hostile Offline` = hostile_offline, 
                `Hostile Online` = hostile_online, 
                `Talk Offline` = talk_offline, 
                `Talk Online` = talk_online, 
                `Perception Offline` = conflict_all_offline,
                `Perception Online` = conflict_all_online,
                `SDRT` = sdrt, 
                `Trait Aggression` = aggression
        ) %>% 
        cor.matrix %>% 
        
        stargazer(., summary = F, 
                  title = "Table B3b. Descriptives -- S2 Denmark",
                  digits = 2,
                  out = "D.documents/tables/tb3_descript_s2.html",
                  type = "text")

df.us2 %>% 
        transmute(
                `Hostile Offline` = hostile_offline, 
                `Hostile Online` = hostile_online, 
                `Perception Offline` = perc_off,
                `Perception Online` = perc_on,
        ) %>% 
        cor.matrix %>% 
        
        stargazer(., summary = F, 
                  title = "Table B3c. Descriptives -- S3 USA",
                  digits = 2,
                  out = "D.documents/tables/tb3_descript_s3.html",
                  type = "text")


df.us3.part %>% 
        transmute(
                `Hostile Offline` = hostile_offline, 
                `Hostile Online` = hostile_online, 
                `Talk Offline` = talk_offline,
                `Talk Online` = talk_online, 
                `SDRT` = sdrt, 
                `Trait Aggression` = aggression
        ) %>% 
        cor.matrix %>% 
        
        stargazer(., summary = F, 
                  title = "Table B3d. Descriptives -- S4 USA",
                  digits = 2,
                  out = "D.documents/tables/tb3_descript_s4.html",
                  type = "text")

# Calculate alpha levels #######################################################

get.alpha <- function(df, x){
        psych::alpha(df[, c(x)], warnings = F)$total$raw_alpha
}


talk_offline <- paste0("talk_offline", 1:5)
talk_online <- paste0("talk_online", 1:5)
tone_offline_neg <- paste0("tone_offline", 4:6)
tone_online_neg <- paste0("tone_online", 4:6)
conflict_online <- paste0("conflict_online", 1:8)
conflict_offline <- paste0("conflict_offline", 1:8)
hostile_online <- paste0("hostile_online", 1:6)
hostile_offline <- paste0("hostile_offline", 1:4)
sdrt <- paste0("sdrt", 1:14)
ders <- paste0("ders", 1:12)
aggression <- paste0("aggression", 1:12)

# estimate alpha reliability of scales 
alpha <- NULL

alpha.dk <- data.frame(
        talk_offline = get.alpha(df.dk, talk_offline),
        tone_offline_neg = get.alpha(df.dk, tone_offline_neg),
        hostile_offline = get.alpha(df.dk, hostile_offline),
        conflict_offline = get.alpha(df.dk, conflict_offline),

        talk_online = get.alpha(df.dk, talk_online),
        tone_online_neg = get.alpha(df.dk, tone_online_neg),
        hostile_online = get.alpha(df.dk, hostile_online),
        conflict_online = get.alpha(df.dk, conflict_online),
        
        sdrt = get.alpha(df.dk, sdrt),
        ders = get.alpha(df.dk, ders)) 


alpha.us <- data.frame(
        
        talk_offline = get.alpha(df.us, talk_offline),
        tone_offline_neg = get.alpha(df.us, tone_offline_neg),
        hostile_offline = get.alpha(df.us, hostile_offline[-4]),
        talk_online = get.alpha(df.us, talk_online),
        tone_online_neg = get.alpha(df.us, tone_online_neg),
        hostile_online = get.alpha(df.us, hostile_online[-6]),
        
        sdrt = get.alpha(df.us, sdrt),
        ders = get.alpha(df.us, ders)) 

alpha.us2 <- data.frame(
        hostile_offline = get.alpha(df.us2, hostile_offline),
        hostile_online = get.alpha(df.us2, hostile_online),
        
        conflict_offline = get.alpha(df.us2, paste0("perc_off_", 1:8)),
        conflict_online = get.alpha(df.us2, paste0("perc_on_", 1:8))
)

alpha.us3a <- data.frame(
        hostile_offline = get.alpha(df.us3.part, paste0("hostile_offline_", 1:8)),
        hostile_online = get.alpha(df.us3.part, paste0("hostile_online_", 1:8)),
        
        talk_offline = get.alpha(df.us3.part, paste0("talk_offline_", 1:5)),
        talk_online = get.alpha(df.us3.part, paste0("talk_online_", 1:5)), 
        
        
        aggression = get.alpha(df.us3.part, aggression),
        sdrt = get.alpha(df.us3.part, sdrt)
)

alpha.mturk1 <- data.frame(
        
        hostile_online = get.alpha(df4[df4$attentive != "not",], paste0("hostile_", 1:5)),
        
        sdrt = get.alpha(df4[df4$attentive != "not",], paste0("riskseek_", 1:14)),
        ders = get.alpha(df4[df4$attentive != "not",], paste0("emotionreg_", 1:16))) 

alpha.mturk2 <- data.frame(
        
        aggression = get.alpha(df5, paste0("aggression_", 1:12)),
        sdrt = get.alpha(df5, paste0("riskseek_", 1:14)),
        ders = get.alpha(df5, paste0("emotionreg_", 1:16))) 

alpha <- bind_rows(alpha.us, alpha.dk, alpha.us2, alpha.us3a, alpha.mturk1, alpha.mturk2) %>% 
        t() %>% 
        as.data.frame() %>% 
        # mutate(names = rownames(.)) %>% 
        arrange(rownames(.))

names(alpha) <- c("Study1_USA", "Study2_Denmark", "Study3_Lucid", "Study4a_USA",
                  "Study5_MTurk", "Study6_MTurk")
        
rownames(alpha) <- c("Aggression", 
                     "Perceptions of conflicts (all) - offline",
                     "Perceptions of conflicts (all) - online", 
                     "Difficulties in emotion regulation",
                     
                     "Hostility - offline",
                     "Hostility - online",
                     
                     "Status-driven risk taking",
                     
                     "Talking about politics - offline",
                     "Talking about politics - online",
                     
                     "Negative tone - offline", 
                     "Negative tone - online")

stargazer(alpha, summary = F, 
          title = "Table B2. Raw alpha reliability statistics for the indices",
          out = "D.documents/tables/tb2_alphas.html",
          type = "text")

#  ~ additional analysis about S3 perception indices 

psych::fa(df.us2[, paste0("perc_on_", 1:8)], 2)
psych::fa(df.us2[, paste0("perc_off_", 1:8)], 2)


# ~ validate hostility scales with Trait aggression

df.us %>% 
        dplyr::select("aggression", "hostile_offline", "hostile_online") %>% 
        as.matrix() %>% 
        Hmisc::rcorr()

df.dk %>% 
        dplyr::select("aggression", "hostile_offline", "hostile_online") %>% 
        as.matrix() %>% 
        Hmisc::rcorr()


# Additional analyses ##########################################################

# ~ differences in talking about politics online and offline #####


talksum.us <- df.us %>% 
        dplyr::select(talk_offline1:talk_offline5, 
                      talk_online1:talk_online5) %>% 
        gather(var, value) %>% 
        group_by(var) %>% 
        summarise(mean = mean(value, na.rm = T),
                  se = se(value)) %>% 
        separate(var, c("environment", "partner"), -1) %>% 
        mutate(environment = fct_recode(environment, Offline = "talk_offline", 
                                        Online = "talk_online"), 
               group = ifelse(partner == "4" | partner == "5", "Agreement", "Group"),
               partner = fct_recode(partner, 
                                    Friends = "1", 
                                    Acquaintances = "2", 
                                    Strangers = "3", 
                                    Agree = "4", 
                                    Disagree = "5")) 

talksum.dk <- df.dk %>% 
        dplyr::select(talk_offline1:talk_offline5, 
                      talk_online1:talk_online5) %>% 
        gather(var, value) %>% 
        group_by(var) %>% 
        summarise(mean = mean(value, na.rm = T),
                  se = se(value)) %>% 
        separate(var, c("environment", "partner"), -1) %>% 
        mutate(environment = fct_recode(environment, Offline = "talk_offline", 
                                        Online = "talk_online"), 
               group = ifelse(partner == "4" | partner == "5", "Agreement", "Group"),
               partner = fct_recode(partner, 
                                    Friends = "1", 
                                    Acquaintances = "2", 
                                    Strangers = "3", 
                                    Agree = "4", 
                                    Disagree = "5")) 

talksum.us3 <- df.us3.part %>% 
        dplyr::select(talk_offline_1:talk_offline_5, 
                      talk_online_1:talk_online_5) %>% 
        gather(var, value) %>% 
        group_by(var) %>% 
        summarise(mean = mean(value, na.rm = T),
                  se = se(value)) %>% 
        separate(var, c("environment", "partner"), -1) %>% 
        mutate(environment = fct_recode(environment, Offline = "talk_offline_", 
                                        Online = "talk_online_"), 
               group = ifelse(partner == "4" | partner == "5", "Agreement", "Group"),
               partner = fct_recode(partner, 
                                    Friends = "1", 
                                    Acquaintances = "2", 
                                    Strangers = "3", 
                                    Agree = "4", 
                                    Disagree = "5")) 


talksum <- bind_rows(`S1. USA` = talksum.us, 
                     `S2. Denmark` = talksum.dk,
                     `S4. USA` = talksum.us3,
                     .id = "country") %>% 
        mutate(country = fct_relevel(country, "USA"), 
               group = fct_relevel(group, "Group"))

ggplot(talksum, aes(x = partner, y = mean, fill = fct_rev(environment))) + 
        geom_col(position = position_dodge(0.9)) + 
        geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), 
                      position = position_dodge(0.9), width = 0.1) + 
        facet_grid(country~group, scales = "free_x") +
        xlab("") + 
        ylab("Means and 95% CIs") + 
        scale_fill_grey(name = NULL) + 
        theme_bw()
ggsave(here("D.documents/figures/fig_b1_talking_patterns.jpg"), 
       width = 7, height = 5)



disagree.t.sum <- rbind(
        t.sum(df.us$talk_online5, df.us$talk_offline5, 
              "Online",  "Offline","Disagreers - S1.US", paired = TRUE),
        t.sum( df.dk$talk_online5,df.dk$talk_offline5,
              "Online","Offline",  "Disagreers - S2.Denmark", paired = TRUE),
        t.sum(df.us3.part$talk_online_5, df.us3.part$talk_offline_5, 
              "Online",  "Offline","Disagreers - S4.US", paired = TRUE)
)
row.names(disagree.t.sum) <- NULL

stargazer(disagree.t.sum, summary = F, digits = 2,
          type = "text")

# ~ talking ~ sdrt disaggregated by groups ####################################

# usa online
fit4.3.us.1 <- lm(talk_online1 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)

fit4.3.us.2 <- lm(talk_online2 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)
fit4.3.us.3 <- lm(talk_online3 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)
fit4.3.us.4 <- lm(talk_online4 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)
fit4.3.us.5 <- lm(talk_online5 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)

# usa offline
fit4.4.us.1 <- lm(talk_offline1 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)

fit4.4.us.2 <- lm(talk_offline2 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)
fit4.4.us.3 <- lm(talk_offline3 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)
fit4.4.us.4 <- lm(talk_offline4 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)
fit4.4.us.5 <- lm(talk_offline5 ~ sdrt + 
                    female + age + white + 
                    highered + income + pid, df.us)

# Denmark online
fit4.5.dk.1 <- lm(talk_online1 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

fit4.5.dk.2 <- lm(talk_online2 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

fit4.5.dk.3 <- lm(talk_online3 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

fit4.5.dk.4 <- lm(talk_online4 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

fit4.5.dk.5 <- lm(talk_online5 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

# Denmark offline  
fit4.6.dk.1 <- lm(talk_offline1 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

fit4.6.dk.2 <- lm(talk_offline2 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

fit4.6.dk.3 <- lm(talk_offline3 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

fit4.6.dk.4 <- lm(talk_offline4 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)

fit4.6.dk.5 <- lm(talk_offline5 ~ sdrt + 
                    female + age + highered + 
                    income + pid, df.dk)


# new data for plot 5
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

# new data for plot 5
nd4.us3 <- data.frame(
        # sequence along the range of SDRT
        sdrt = seq(round(min(df.us3.part$sdrt), 2), 
                   round(max(df.us3.part$sdrt), 2), 
                   0.05), 
        female = mean(df.us3.part$female),  # covariates are set to their mean
        age = mean(df.us3.part$age), 
        highered = mean(df.us3.part$highered), 
        income = mean(df.us3.part$income, na.rm = T), 
        pid = mean(df.us3.part$pid), 
        white = mean(df.us3.part$white)
)


# predict 
pred5.us <- rbind(
  cbind(nd4.us, 
        predict(fit4.3.us.1, newdata = nd4.us, interval = "confidence"),
        env = "Online", target = "Friends", country = "USA"), 
  cbind(nd4.us, 
        predict(fit4.3.us.2, newdata = nd4.us, interval = "confidence"),
        env = "Online", target = "Acquaintances", country = "USA"), 
  cbind(nd4.us, 
        predict(fit4.3.us.3, newdata = nd4.us, interval = "confidence"),
        env = "Online", target = "Strangers", country = "USA"), 
  cbind(nd4.us, 
        predict(fit4.3.us.4, newdata = nd4.us, interval = "confidence"),
        env = "Online", target = "Agreers", country = "USA"), 
  cbind(nd4.us, 
        predict(fit4.3.us.5, newdata = nd4.us, interval = "confidence"),
        env = "Online",  target = "Disagreers", country = "USA"),
  
  cbind(nd4.us, 
        predict(fit4.4.us.1, newdata = nd4.us, interval = "confidence"),
        env = "Offline", target = "Friends", country = "USA"), 
  cbind(nd4.us, 
        predict(fit4.4.us.2, newdata = nd4.us, interval = "confidence"),
        env = "Offline", target = "Acquaintances", country = "USA"), 
  cbind(nd4.us, 
        predict(fit4.4.us.3, newdata = nd4.us, interval = "confidence"),
        env = "Offline", target = "Strangers", country = "USA"), 
  cbind(nd4.us, 
        predict(fit4.4.us.4, newdata = nd4.us, interval = "confidence"),
        env = "Offline", target = "Agreers", country = "USA"), 
  cbind(nd4.us, 
        predict(fit4.4.us.5, newdata = nd4.us, interval = "confidence"),
        env = "Offline",  target = "Disagreers", country = "USA")
)

# predict 
pred5.dk <- rbind(
  cbind(nd4.dk, 
        predict(fit4.5.dk.1, newdata = nd4.dk, interval = "confidence"),
        env = "Online", target = "Friends", country = "Denmark"), 
  cbind(nd4.dk, 
        predict(fit4.5.dk.2, newdata = nd4.dk, interval = "confidence"),
        env = "Online", target = "Acquaintances", country = "Denmark"), 
  cbind(nd4.dk, 
        predict(fit4.5.dk.3, newdata = nd4.dk, interval = "confidence"),
        env = "Online", target = "Strangers", country = "Denmark"), 
  cbind(nd4.dk, 
        predict(fit4.5.dk.4, newdata = nd4.dk, interval = "confidence"),
        env = "Online", target = "Agreers", country = "Denmark"), 
  cbind(nd4.dk, 
        predict(fit4.5.dk.5, newdata = nd4.dk, interval = "confidence"),
        env = "Online",  target = "Disagreers", country = "Denmark"),
  
  cbind(nd4.dk, 
        predict(fit4.6.dk.1, newdata = nd4.dk, interval = "confidence"),
        env = "Offline", target = "Friends", country = "Denmark"), 
  cbind(nd4.dk, 
        predict(fit4.6.dk.2, newdata = nd4.dk, interval = "confidence"),
        env = "Offline", target = "Acquaintances", country = "Denmark"), 
  cbind(nd4.dk, 
        predict(fit4.6.dk.3, newdata = nd4.dk, interval = "confidence"),
        env = "Offline", target = "Strangers", country = "Denmark"), 
  cbind(nd4.dk, 
        predict(fit4.6.dk.4, newdata = nd4.dk, interval = "confidence"),
        env = "Offline", target = "Agreers", country = "Denmark"), 
  cbind(nd4.dk, 
        predict(fit4.6.dk.5, newdata = nd4.dk, interval = "confidence"),
        env = "Offline",  target = "Disagreers", country = "Denmark")
)


# combine predictions from USA and DK
pred5 <- pred5.dk %>% select(-pid) %>% 
  bind_rows(pred5.us)

ggplot(pred5, aes(x = sdrt, y = fit, fill = env)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) + 
  xlab("Status Driven Risk Taking") + 
  scale_fill_grey(name = NULL, start = 0, end = 0.5) + 
  ylab("Talking about politics") + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.25), 
                     labels = c('0', '.25', '.5', ".75", '1')) + 
  facet_grid(fct_rev(country) ~ target) +
  theme_minimal()

ggsave(here("D.documents/figures/fig_d1_talkbygroup_sdrt.jpg"))



# Replicate the same analysis for Study 4. Here, we fail to replicate the selection
#       hypothesis, therefore there is little interest in the disaggregation.


# usa online
fit4.3.us3.1 <- lm(talk_online_1 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)

fit4.3.us3.2 <- lm(talk_online_2 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)
fit4.3.us3.3 <- lm(talk_online_3 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)
fit4.3.us3.4 <- lm(talk_online_4 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)
fit4.3.us3.5 <- lm(talk_online_5 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)

# usa offline
fit4.4.us3.1 <- lm(talk_offline_1 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)

fit4.4.us3.2 <- lm(talk_offline_2 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)
fit4.4.us3.3 <- lm(talk_offline_3 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)
fit4.4.us3.4 <- lm(talk_offline_4 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)
fit4.4.us3.5 <- lm(talk_offline_5 ~ sdrt + 
                          female + age + white + 
                          highered + income + pid, df.us3.part)


pred4.us3 <- rbind(
        cbind(nd4.us3, 
              predict(fit4.3.us3.1, newdata = nd4.us3, interval = "confidence"),
              env = "Online", target = "Friends", country = "USA"), 
        cbind(nd4.us3, 
              predict(fit4.3.us3.2, newdata = nd4.us3, interval = "confidence"),
              env = "Online", target = "Acquaintances", country = "USA"), 
        cbind(nd4.us3, 
              predict(fit4.3.us3.3, newdata = nd4.us3, interval = "confidence"),
              env = "Online", target = "Strangers", country = "USA"), 
        cbind(nd4.us3, 
              predict(fit4.3.us3.4, newdata = nd4.us3, interval = "confidence"),
              env = "Online", target = "Agreers", country = "USA"), 
        cbind(nd4.us3, 
              predict(fit4.3.us3.5, newdata = nd4.us3, interval = "confidence"),
              env = "Online",  target = "Disagreers", country = "USA"),
        
        cbind(nd4.us3, 
              predict(fit4.4.us3.1, newdata = nd4.us3, interval = "confidence"),
              env = "Offline", target = "Friends", country = "USA"), 
        cbind(nd4.us3, 
              predict(fit4.4.us3.2, newdata = nd4.us3, interval = "confidence"),
              env = "Offline", target = "Acquaintances", country = "USA"), 
        cbind(nd4.us3, 
              predict(fit4.4.us3.3, newdata = nd4.us3, interval = "confidence"),
              env = "Offline", target = "Strangers", country = "USA"), 
        cbind(nd4.us3, 
              predict(fit4.4.us3.4, newdata = nd4.us3, interval = "confidence"),
              env = "Offline", target = "Agreers", country = "USA"), 
        cbind(nd4.us3, 
              predict(fit4.4.us3.5, newdata = nd4.us3, interval = "confidence"),
              env = "Offline",  target = "Disagreers", country = "USA")
)


ggplot(pred4.us3, aes(x = sdrt, y = fit, fill = env)) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) + 
        xlab("Status Driven Risk Taking") + 
        scale_fill_grey(name = NULL, start = 0, end = 0.5) + 
        ylab("Talking about politics") + 
        scale_x_continuous(breaks = seq(0, 1, by = 0.25), 
                           labels = c('0', '.25', '.5', ".75", '1')) + 
        facet_grid(. ~ target) +
        theme_minimal()

# ~ positive tone perceptions ##################################################


postone.t.sum <- rbind(
        t.sum(df.us$tone_offline_pos, df.us$tone_online_pos, 
              "Offline", "Online", "Tone - US", paired = TRUE),
        t.sum(df.dk$tone_offline_pos, df.dk$tone_online_pos,
              "Offline", "Online", "Tone - Denmark", paired = TRUE)
)
row.names(postone.t.sum) <- NULL


stargazer(postone.t.sum, summary = F, digits = 2,
          title = "Table D1. T-test between positive online and offline tone ratings", 
          out = "D.documents/tables/td1_postonetest.html",
          type = "text")


# ~ online vs offline hostility with equivalence tests ##########################


change.t.sum <- rbind(
        tost.sum(df.us$hostile_online, df.us$hostile_offline, 
                 xname = "Online", yname = "Offline", name = "S1. USA"),
        tost.sum(df.dk$hostile_online, df.dk$hostile_offline,
                 xname = "Online", yname = "Offline", name = "S2. Denmark"),
        tost.sum(df.us2$hostile_online, df.us2$hostile_offline, 
                 xname = "Online", yname = "Offline", name = "S3. USA"), 
        tost.sum(df.us3.part$hostile_online, df.us3.part$hostile_offline, 
                 xname = "Online", yname = "Offline", name = "S4. USA"))

row.names(change.t.sum) <- NULL


stargazer(change.t.sum, summary = F, digits = 2,
          title = "Table D2. Paired t-tests on self-reported hostility offline and online", 
          out = "D.documents/tables/td2_changettest.html",
          rownames = F,
          type = "text")

# ~ online vs offline hostility experiment #####################################

# for study 3, we preregistered the bounds .21

TOSTtwo(m1 = mean(df.us2$hostile_online[df.us2$online_first == 1]), 
        m2 = mean(df.us2$hostile_offline[df.us2$online_first == 0]), 
        sd1 = sd(df.us2$hostile_online[df.us2$online_first == 1]), 
        sd2 = sd(df.us2$hostile_offline[df.us2$online_first == 0]),
        n1 = length(df.us2$hostile_online[df.us2$online_first == 1]), 
        n2 = length(df.us2$hostile_offline[df.us2$online_first == 0]), 
        low_eqbound_d = -.21, 
        high_eqbound_d = .21)


# for study 4, we preregistered the SESOI as a bound
#       coincidentally, it's agaon a d= 0.21
powerTOSTtwo(alpha = 0.05, statistical_power = 0.8, N = nrow(df.us3.part)/2)

TOSTtwo(m1 = mean(df.us3.part$hostile_online[df.us3.part$online_first == 1], na.rm = T), 
        m2 = mean(df.us3.part$hostile_offline[df.us3.part$online_first == 0]), 
        sd1 = sd(df.us3.part$hostile_online[df.us3.part$online_first == 1]), 
        sd2 = sd(df.us3.part$hostile_offline[df.us3.part$online_first == 0]),
        n1 = length(df.us3.part$hostile_online[df.us3.part$online_first == 1]), 
        n2 = length(df.us3.part$hostile_offline[df.us3.part$online_first == 0]), 
        low_eqbound_d = -.21, 
        high_eqbound_d = .21)

# ~ online vs offline with control for talk -------------------------------

with(filter(df.us, talk_online != 0), cor(hostile_online, hostile_offline))

talkcontrol <- lm(hostile_online ~ hostile_offline, df.us, subset = talk_online != 0)

plot.talkcontrol <- effect_plot(talkcontrol, pred = "hostile_offline", interval = T, 
                     plot.points = T, jitter = 0.02, point.size = 0.6,
                     point.alpha = 0.3,
                     x.label = "Offline hostility", y.label = "Online hostility", 
                     main.title = "S1. USA -- Filtering out those who don't talk pol online") + 
        geom_abline(aes(intercept = 0, slope = 1))

# ~ Trait aggression #############################################################

# Study 1 - USA
agg.1.us <- lm(hostile_online ~ aggression + 
                       female + age + white + 
                       highered + income + pid, df.us)

agg.2.us <- lm(hostile_offline ~ aggression + 
                       female + age + white + 
                       highered + income + pid, df.us)

agg.3.us <- lm(talk_online ~ aggression + 
                       female + age + white + 
                       highered + income + pid, df.us)

agg.4.us <- lm(talk_offline ~ aggression + 
                       female + age + white + 
                       highered + income + pid, df.us)

# Study 2 - DK 
agg.1.dk <- lm(hostile_online ~ aggression + 
                       female + age + highered + 
                       income + pid, df.dk)

agg.2.dk <- lm(hostile_offline ~ aggression + 
                       female + age + highered + 
                       income + pid, df.dk)

agg.3.dk <- lm(talk_online ~ aggression + 
                       female + age + highered + 
                       income + pid, df.dk)

agg.4.dk <- lm(talk_offline ~ aggression + 
                       female + age + highered + 
                       income + pid, df.dk)

# Study 4 - USA

agg.1.us3 <- lm(hostile_online ~ aggression + 
                       female + age + white + 
                       highered + income + pid, df.us3.part)

agg.2.us3 <- lm(hostile_offline ~ aggression + 
                       female + age + white + 
                       highered + income + pid, df.us3.part)

agg.3.us3 <- lm(talk_online ~ aggression + 
                       female + age + white + 
                       highered + income + pid, df.us3.part)

agg.4.us3 <- lm(talk_offline ~ aggression + 
                       female + age + white + 
                       highered + income + pid, df.us3.part)


stargazer(agg.1.us, agg.2.us, agg.3.us, agg.4.us, 
          agg.1.dk, agg.2.dk, agg.3.dk, agg.4.dk, 
          agg.1.us3, agg.2.us3, agg.3.us3, agg.4.us3, 
          dep.var.labels = paste(rep(c("Online", "Offline"), 6),
                                 rep(rep(c("Hostility", "Talking"), each = 2), 3),
                                 sep = " - "),
          covariate.labels = c("Trait aggression", "Female", "Age",
                               "White", "Higher educated", "Income", "US - Party ID",
                               "DK - Party ID:Other", "DK - Party ID:Red block"),
          column.labels = c("United States", "Denmark", "United States"),
          column.separate = c(4,4, 4),
          out = "D.documents/tables/backup_t2_trait_aggression.html",
          type = "text")

# custom function to draw predicted value plots 
# predicted.plot <- function(fit, key, country){
#         
#         # define a range of values for IV of interest for which to predict 
#         keystring <- seq(round(min(fit$model[key]), 2), 
#                          round(max(fit$model[key]), 2), 
#                          0.1)
#         
#         # remind the model which IV is not in the focus at the moment
#         other <- ifelse(key == "sdrt", "ders", "sdrt")
#         
#         # create data frames with covariates set to 0
#         # the two countries have slightly different covariates 
#         if(country == "us"){
#                 newdat <- data.frame(
#                         female = rep(0, length(keystring)),
#                         age = rep(0, length(keystring)), 
#                         highered = rep(0, length(keystring)), 
#                         income = rep(0, length(keystring)), 
#                         pid = rep(0, length(keystring)), 
#                         white = rep(0, length(keystring))
#                 ) 
#                 
#         } else if(country == "dk"){
#                 newdat <- data.frame(
#                         female = rep(0, length(keystring)),
#                         age = rep(0, length(keystring)), 
#                         highered = rep(0, length(keystring)), 
#                         income = rep(0, length(keystring)), 
#                         pid = rep("Red_block", length(keystring))
#                 ) 
#         } else {stop("Unrecognized country")}
#         
#         newdat[, key] <- keystring # add key IV to DF
#         newdat[, other] <- 0 # add other IV to DF and set to 0
#         
#         # check which environment is used in the model
#         # split the first variable (dv) in fitted model object and check
#         #       what is after the dash
#         env <- str_split(names(fit$model[1]), fixed("_"))[[1]][2]
#         
#         # predict values and 95% confidence interval
#         pred <- cbind(newdat,
#                       predict(fit, newdata = newdat, interval = "confidence"),
#                       env = env, country = country) %>% 
#                 select(-other)%>% 
#                 mutate(country = fct_recode(country, Denmark = "dk", USA = "us"), 
#                        env = fct_recode(env, Offline = "offline", Online = "online"))
#         
#         pred
#         
# }

# export predicted values for each model
#       predicted.plot throws a bunch of warnings about unknown levels, which is fine
agg1.us <- bind_rows(predicted.plot(agg.1.us, country = "us", key = "aggression"),
        predicted.plot(agg.2.us, country = "us", key = "aggression")) %>% 
        dplyr::select(-(female:white))

agg1.dk <- bind_rows(predicted.plot(agg.1.dk, country = "dk", key = "aggression"),
                     predicted.plot(agg.2.dk, country = "dk", key = "aggression")) %>% 
        dplyr::select(-(female:pid))

agg1.us3 <- bind_rows(predicted.plot(agg.1.us3, country = "us", key = "aggression"),
        predicted.plot(agg.2.us3, country = "us", key = "aggression")) %>% 
        dplyr::select(-(female:white))

agg3.us <- bind_rows(predicted.plot(agg.3.us, country = "us", key = "aggression"),
        predicted.plot(agg.4.us, country = "us", key = "aggression")) %>% 
        dplyr::select(-(female:white))

agg3.dk <- bind_rows(predicted.plot(agg.3.dk, country = "dk", key = "aggression"),
                     predicted.plot(agg.4.dk, country = "dk", key = "aggression")) %>% 
        dplyr::select(-(female:pid))

agg3.us4 <- bind_rows(predicted.plot(agg.3.us3, country = "us", key = "aggression"),
        predicted.plot(agg.4.us3, country = "us", key = "aggression")) %>% 
        dplyr::select(-(female:white))

# draw each plot. 
plot.agg1.us <- ggplot(agg1.us, aes(x = aggression, y = fit, fill = fct_rev(env))) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
        scale_fill_grey(name = NULL, start = 0, end = 0.5, guide = F) + 
        # facet_wrap(~ variable, nrow = 1, switch = "x") +
        ylab("Hostility") + 
        xlab("Trait aggression") + 
        ggtitle("Study 1. USA - Hostility")+
        ylim(c(-0.1,.6)) + 
        theme_minimal() 

plot.agg1.dk <- ggplot(agg1.dk, aes(x = aggression, y = fit, fill = fct_rev(env))) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
        scale_fill_grey(name = NULL, start = 0, end = 0.5, guide = F) + 
        # facet_wrap(~ variable, nrow = 1, switch = "x") +
        ylab("Hostility") + 
        xlab("Trait aggression") + 
        ggtitle("Study 2. Denmark")+
        ylim(c(-0.1,.6)) +
        theme_minimal() 

plot.agg1.us3 <- ggplot(agg1.us3, aes(x = aggression, y = fit, fill = fct_rev(env))) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
        scale_fill_grey(name = NULL, start = 0, end = 0.5, guide = F) + 
        # facet_wrap(~ variable, nrow = 1, switch = "x") +
        ylab("Hostility") + 
        xlab("Trait aggression") + 
        ggtitle("Study 4. USA")+
        ylim(c(-0.1,.6)) + 
        theme_minimal() 

plot.agg3.us <- ggplot(agg3.us, aes(x = aggression, y = fit, fill = fct_rev(env))) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
        scale_fill_grey(name = NULL, start = 0, end = 0.5, guide = F) + 
        # facet_wrap(~ variable, nrow = 1, switch = "x") +
        ylab("Talking about politics") + 
        xlab("Trait aggression") + 
        ggtitle("Study 1. USA - Talking")+
        ylim(c(-0.1,.6)) +
        theme_minimal() 

plot.agg3.dk <- ggplot(agg3.dk, aes(x = aggression, y = fit, fill = fct_rev(env))) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
        scale_fill_grey(name = NULL, start = 0, end = 0.5) + 
        # facet_wrap(~ variable, nrow = 1, switch = "x") +
        ylab("Talking about politics") + 
        xlab("Trait aggression") + 
        ggtitle("Study 2. Denmark")+
        ylim(c(-0.1,.6)) +
        theme_minimal() + theme(legend.justification=c(1,0), 
                                legend.position=c(1,0))

plot.agg3.us3 <- ggplot(agg3.us4, aes(x = aggression, y = fit, fill = fct_rev(env))) + 
        geom_line() + 
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
        scale_fill_grey(name = NULL, start = 0, end = 0.5, guide = F) + 
        # facet_wrap(~ variable, nrow = 1, switch = "x") +
        ylab("Talking about politics") + 
        xlab("Trait aggression") + 
        ggtitle("Study 4. USA")+
        ylim(c(-0.1,.6)) +
        theme_minimal() 


plot.agg1.us + plot.agg1.dk + plot.agg1.us3 +
        plot.agg3.us +plot.agg3.dk + plot.agg3.us3

ggsave(file = "D.documents/figures/fig_d2_aggression.jpg",
       width = 8, height = 6)

# Seemingly Unrelated Regressions################################################

# ~ Hostility predicted by SDRT --------------------------------------

# ~ S1 usa 
model1.us <- ' 
        # regressions 
        hostile_offline ~ a1*1+b1*sdrt + c1*female + c2*age +c3*highered+
        c4*income  + c5*pid + c6*white
        
        hostile_online ~ a2*1 + b2*sdrt + d1*female + d2*age +d3*highered  +
        d4*income + d5*pid + d6*white
        
        # residual covariance
        hostile_online ~~ hostile_offline
        hostile_online ~~ hostile_online
        hostile_offline ~~ hostile_offline
        
        # test
        intercept_contrast := a2 - a1
        sdrt_contrast := b2 - b1
'

sem1.us <- sem(model1.us, data = df.us)

# ~ S2. DK
model1.dk <- ' 
        # regressions 
        hostile_offline ~ a1*1+b1*sdrt +  c1*female + c2*age +c3*highered+
        c4*income  + c5 * Party_red + c6*Party_other
        
        hostile_online ~ a2*1 + b2*sdrt  + d1*female + d2*age +d3*highered  +
        d4*income + d5 * Party_red + d6*Party_other
        
        # residual covariance
        hostile_online ~~ hostile_offline
        hostile_online ~~ hostile_online
        hostile_offline ~~ hostile_offline
        
        # test
        intercept_contrast := a2 - a1
        sdrt_contrast := b2 - b1
'

sem1.dk <- sem(model1.dk, data = df.dk)


# ~ S4 usa 
model1.us3 <- ' 
        # regressions 
        hostile_offline ~ a1*1+b1*sdrt + c1*female + c2*age +c3*highered+
        c4*income  + c5*pid + c6*white
        
        hostile_online ~ a2*1 + b2*sdrt + d1*female + d2*age +d3*highered  +
        d4*income + d5*pid + d6*white
        
        # residual covariance
        hostile_online ~~ hostile_offline
        hostile_online ~~ hostile_online
        hostile_offline ~~ hostile_offline
        
        # test
        intercept_contrast := a2 - a1
        sdrt_contrast := b2 - b1
'

sem1.us3 <- sem(model1.us3, data = df.us3.part)


sem1 <- bind_rows(list(S1.USA = parameterestimates(sem1.us), 
                  S2.Denmark = parameterestimates(sem1.dk), 
                  S4.USA = parameterestimates(sem1.us3)), .id = "Country") %>% 
        filter(label == "a1" | label == "b1" | label == "a2"| label == "b2"|
                       label == "intercept_contrast"| label == "sdrt_contrast") %>% 
        mutate(label = fct_recode(label,
                                  Intercept_offline = 'a1', 
                                  Intercept_online = 'a2', 
                                  
                                  sdrt_offline = 'b1', 
                                  sdrt_online = 'b2')) %>% 
        separate(label, c("Parameter", "Environment"), sep = "_") %>% 
        mutate(Parameter = fct_recode(Parameter, `Status drive` = "sdrt")) %>% 
        dplyr::select(Country, Parameter, Environment, B = est, SE = se, p= pvalue, ci.lower, ci.upper)  


stargazer(sem1, summary = F, 
          digits = 2,
          title = "Table D3. Seemingly unrelated regressions for online and offline hostility",
          out = "D.documents/tables/td3_hostility_sur.html",
          rownames = F,
          type = "text")

# # additional test for SDRT vs DERS 
# 
# parameterestimates(sem1.us)[69, ]
# parameterestimates(sem1.dk)[69, ]


# ~ Talking about politics predicted by SDRT ---------------------------------- 


model2.us <- ' 
        # regressions 
        talk_offline ~ a1*1+b1*sdrt + c1*female + c2*age +c3*highered+
        c4*income  + c5*pid + c6*white
        
        talk_online ~ a2*1 + b2*sdrt + d1*female + d2*age +d3*highered  +
        d4*income + d5*pid + d6*white
        
        # residual covariance
        talk_online ~~ talk_offline
        talk_online ~~ talk_online
        talk_offline ~~ talk_offline
        
        # test
        intercept_contrast := a2 - a1
        sdrt_contrast := b2 - b1
'

sem2.us <- sem(model2.us, data = df.us)


model2.dk <- ' 
        # regressions 
        talk_offline ~ a1*1+b1*sdrt + c1*female + c2*age +c3*highered+
        c4*income  + c5 * Party_red + c6*Party_other
        
        talk_online ~ a2*1 + b2*sdrt + d1*female + d2*age +d3*highered  +
        d4*income + d5 * Party_red + d6*Party_other
        
        # residual covariance
        talk_online ~~ talk_offline
        talk_online ~~ talk_online
        talk_offline ~~ talk_offline
        
        # test
        intercept_contrast := a2 - a1
        sdrt_contrast := b2 - b1
'

sem2.dk <- sem(model2.dk, data = df.dk)



model2.us3 <- ' 
        # regressions 
        talk_offline ~ a1*1+b1*sdrt + c1*female + c2*age +c3*highered+
        c4*income  + c5*pid + c6*white
        
        talk_online ~ a2*1 + b2*sdrt + d1*female + d2*age +d3*highered  +
        d4*income + d5*pid + d6*white
        
        # residual covariance
        talk_online ~~ talk_offline
        talk_online ~~ talk_online
        talk_offline ~~ talk_offline
        
        # test
        intercept_contrast := a2 - a1
        sdrt_contrast := b2 - b1
'

sem2.us3 <- sem(model2.us, data = df.us3.part)


sem2 <- bind_rows(list(S1.USA = parameterestimates(sem2.us), 
                       S2.Denmark = parameterestimates(sem2.dk),
                       S4.USA = parameterestimates(sem2.us3)), .id = "Country") %>% 
        filter(label == "a1" | label == "b1" | label == "a2"| label == "b2"|
                       label == "intercept_contrast"| label == "sdrt_contrast" ) %>% 
        mutate(label = fct_recode(label,
                                  Intercept_offline = 'a1', 
                                  Intercept_online = 'a2', 
                                  
                                  sdrt_offline = 'b1', 
                                  sdrt_online = 'b2')) %>% 
        separate(label, c("Parameter", "Environment"), sep = "_") %>% 
        mutate(Parameter = fct_recode(Parameter, `Status drive` = "sdrt")) %>% 
        dplyr::select(Country, Parameter, Environment, B = est, SE = se, p= pvalue, ci.lower, ci.upper)  



stargazer(sem2, summary = F, 
          digits = 2,
          title = "Table D4. Seemingly unrelated regressions for online and offline talk about politics",
          out = "D.documents/tables/td4_talk_sur.html",
          rownames = F,
          type = "text")



# Hostility by talking and SDRT ---------------------------------------------------------
library(jtools)


hvt2.us <- lm(hostile_online ~ talk_online * sdrt, df.us)

hvt2.dk <- lm(hostile_online ~ talk_online * sdrt, df.dk)

hvt2.us3 <- lm(hostile_online ~ talk_online * sdrt, df.us3.part)

stargazer(hvt2.us, hvt2.dk, hvt2.us3, 
          type = "text")





interactions::interact_plot(hvt2.us, pred = talk_online, modx = sdrt, linearity.check = T,
                            plot.points = T, facet.modx = T, point.alpha = .2, 
                            x.label = "", y.label = "") /
interactions::interact_plot(hvt2.dk, pred = talk_online, modx = sdrt,  linearity.check = T,
                            plot.points = T, facet.modx = T, point.alpha = .2, 
                            x.label = "", y.label = "Hostile online") / 
interactions::interact_plot(hvt2.us3, pred = talk_online, modx = sdrt,  linearity.check = T,
                                    plot.points = T, facet.modx = T, point.alpha = .2, 
                            x.label = "Talking online", y.label = "") 

ggsave("D.documents/figures/fig_d3_hostilebytalk.jpg", height = 7, width = 7)

# An alternative way of plotting:
# interflex kernel smoother marginal effects 
# this shows that "talking online"-s effect kicks in at medium levels of SDRT
# kern1 <- interflex::inter.kernel(data = df.us, Y = "hostile_online", D = "talk_online", 
#                                  X = "sdrt", bw = 0.02 ) 
# 
# kern2 <- interflex::inter.kernel(data = df.dk, Y = "hostile_online", D = "talk_online", 
#                                  X = "sdrt", bw = 0.02,  na.rm = T) 
# 
# kern3 <- interflex::inter.kernel(data = df.us3.part, Y = "hostile_online", D = "talk_online", 
#                                  X = "sdrt", na.rm = T)
# 
# kern1$graph + kern2$graph + kern3$graph