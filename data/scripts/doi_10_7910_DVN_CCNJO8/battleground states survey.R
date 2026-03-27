##### Installing Packages #####
# Uncomment the following codes if you need to install the following packages
# install.packages(c("ggplot2", "dplyr", "Rmisc", "ggpubr", "cowplot", "haven",
#                    "stargazer", "car", "mctest", "sm", "scales", "plotrix"))

##### Loading Packages #####
library(haven)
library(ggplot2)
library(dplyr)
library(Rmisc)
library(ggpubr)
library(cowplot)
library(stargazer)
library(car)
library(mctest)
library(dotwhisker)
library(sm)
library(scales)
library(plotrix)
library(tidyverse)


##### Import Data #####
df <- read_dta("battleground_survey.dta")

##### Cleaning Data and Recoding Variables #####
df$age <- 2023 - (df$birthyr + 1899)
table(df$age)

df$race2 <- car::recode(df$race, "1=1; 6=0; else=NA") ## 0-hispanics, 1-white
table(df$race2)
table(df$race2, df$state)


table(df$pid)
table(df$pid_leaner)
table(df$pid, df$pid_leaner)
table(df$pid, df$pid_strength_Dem)
table(df$pid, df$pid_strength_Rep)

df$pid7 <- ifelse(df$pid==1 & df$pid_strength_Dem==1, 1,
                  ifelse(df$pid==1 & df$pid_strength_Dem==2, 2,
                         ifelse(df$pid==3 & df$pid_leaner==2, 3,
                                ifelse(df$pid==3 & df$pid_leaner==3, 4,
                                       ifelse(df$pid==3 & df$pid_leaner==1, 5,
                                              ifelse(df$pid==2 & df$pid_strength_Rep==2, 6,
                                                     ifelse(df$pid==2 & df$pid_strength_Rep==1, 7,
                                                            NA)))))))
table(df$pid7)
df$pid3r <- car::recode(df$pid7, "1:3=1; 4=2; 5:7=3")
table(df$pid3r)

table(df$race2, df$pid3r)

df$pid_str <- car::recode(df$pid7, "1=4; 2=3; 3=2; 4=1; 5=2; 6=3; 7=4")
table(df$pid_str)
table(df$pid7)

table(df$Age_18)
table(df$birthyr)
mean(df$birthyr, na.rm=T)

table(df$gender)
df$gender_r <- car::recode(df$gender, "1=0; 2=1; else=NA") #female=1; male=0
table(df$gender_r)
df$gender2 <- 1 - df$gender_r # 1=male, 0=female
table(df$gender2)

table(df$education)
df$edu <- car::recode(df$education, "8=7")
table(df$edu)
df$college <- car::recode(df$edu, "1:4=1; 5:7=0") # 1=noncollege; 0=college
table(df$college)

table(df$income)
df$income_r <- car::recode(df$income, "-99=NA")
table(df$income_r)
df$poor <- car::recode(df$income_r, "1:3=1; 4:7=0") # 1=poor, 0=rich
table(df$poor)


table(df$ideology)
df$ideo <- car::recode(df$ideology, "-99=NA; 4.94=NA; 8=NA; 11.225=NA")
table(df$ideo)
df$ideo2 <- car::recode(df$ideo, "1:3=0; 5:7=1; 4=NA")
table(df$ideo2)

table(df$religion)
df$religion2 <- car::recode(df$religion, "-99=NA; 1:4=1; 13=0") # attend=1; never=0
table(df$religion2)

df$id_gop <- rowSums(cbind(df$ideo2, df$gender2, df$religion2, df$college), na.rm=T)
table(df$id_gop)

df$ideo2.r <- 1 - df$ideo2
df$gender2.r <- 1 - df$gender2
df$religion2.r <- 1 - df$religion2
df$college.r <- 1 - df$college

df$id_dem <- rowSums(cbind(df$ideo2.r, df$gender2.r, df$religion2.r, df$college.r), na.rm=T)
table(df$id_dem)

df$id_in <- ifelse(df$pid3r==3, df$id_gop,
                   ifelse(df$pid3r==1, df$id_dem,
                          NA))
table(df$id_in)

df$id_out <- ifelse(df$pid3r==1, df$id_gop,
                    ifelse(df$pid3r==3, df$id_dem,
                           NA))
table(df$id_out)

table(df$id_in, df$id_out)

df$id_dif <- df$id_in - df$id_out
table(df$id_dif)

table(df$therm_democratic_1)
table(df$therm_republican_1)

df$ft_dem <- car::recode(df$therm_democratic_1, "-99=NA")
table(df$ft_dem)
df$ft_rep <- car::recode(df$therm_republican_1, "-99=NA")
table(df$ft_rep)

df$ft_in <- ifelse(df$pid3r == 1, df$ft_dem, 
                   ifelse(df$pid3r == 3, df$ft_rep,
                          NA))
table(df$ft_in)

df$ft_out <- ifelse(df$pid3r == 3, df$ft_dem, 
                    ifelse(df$pid3r == 1, df$ft_rep,
                           NA))
table(df$ft_out)

df$ft_rep.his <- df$therm_redux_9
df$ft_dem.his <- df$therm_redux_8


df$ft_out.his <- ifelse(df$pid3r == 3, df$ft_dem.his, 
                        ifelse(df$pid3r == 1, df$ft_rep.his,
                               NA))
table(df$ft_out.his)



##### Making plot to show the distribution of age by race #####
df7 <- df %>% dplyr::select(age, race2) %>% na.omit()

df7$race2.f <- factor(df7$race2, levels= c(0,1),
                      labels = c("Latino", "Non-Latino White"))

df7 %>%  
  dplyr::count(race2.f, age) %>% 
  group_by(race2.f) %>% 
  dplyr::mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = age, y = freq, fill = race2.f)) + 
  geom_bar(stat="identity", position = 'dodge') +
  labs(x = "Age", y = "Percentage") +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Latino", "Non-Latino White")) +
  scale_y_continuous(labels=scales::percent) +
  #scale_linetype_manual("Party ID", values=c("solid", "dashed"), labels=c("Republicans", "Democrats")) +
  theme_light() +
  theme(text = element_text(size=16)) +
  theme(legend.position="bottom")


##### Making plot to show the distribution of education by race #####
df7 <- df %>% dplyr::select(edu, race2) %>% na.omit()

df7$race2.f <- factor(df7$race2, levels= c(0,1),
                      labels = c("Latino", "Non-Latino White"))
df7$educ_c <- car::recode(as.numeric(df7$edu), "1='Below High School'; 2='High School';
                          3='Some College'; 4='2-year College'; 5='4-year College';
                          6:8='Post-graduate' ")
table(df7$educ_c)

df7$educ_c <- ordered(df7$educ_c, 
                      levels = c("Below High School", "High School", "Some College",
                                 "2-year College", "4-year College", "Post-graduate"))


df7 %>%  
  dplyr::count(race2.f, educ_c) %>% 
  group_by(race2.f) %>% 
  dplyr::mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = educ_c, y = freq, fill = race2.f)) + 
  geom_bar(stat="identity", position = 'dodge') +
  labs(x = "Education", y = "Percentage") +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Latino", "Non-Latino White")) +
  scale_y_continuous(labels=scales::percent) +
  #scale_linetype_manual("Party ID", values=c("solid", "dashed"), labels=c("Republicans", "Democrats")) +
  theme_light() +
  theme(text = element_text(size=16)) +
  theme(legend.position="bottom")


##### Making plot to show the distribution of social sorting score by race #####
df1 <- df %>% dplyr::select(id_dif, race2) 
df1$id_dif <- as.factor(df$id_dif)
df1$race2 <- as.factor(df$race2)

df1 %>%  
  na.omit() %>% 
  dplyr::count(race2, id_dif) %>% 
  group_by(race2) %>% 
  dplyr::mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = as.factor(id_dif), y = freq, fill = race2)) + 
  geom_bar(stat="identity", position = 'dodge') +
  labs(x = "Social Sorting", y = "Percentage") +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  scale_y_continuous(labels=scales::percent) +
  theme(text = element_text(size=16)) +
  theme(legend.position="bottom")

##### Making plot to show the distribution of partisan strength by race  #####
df$race2.f <- factor(df$race2, levels= c(0,1),
                     labels = c("Hispanic", "Non-Hispanic White"))

dfx <- df %>% dplyr::select(pid_str, race2.f) %>% na.omit()

sm.density.compare(dfx$pid_str, dfx$race2.f, xlab="Partisan Strength")
title(main="MPG Distribution by Car Cylinders")


dfx %>%  
  dplyr::count(race2.f, pid_str) %>% 
  group_by(race2.f) %>% 
  dplyr::mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = as.factor(pid_str), y = freq, fill = race2.f)) + 
  geom_bar(stat="identity", position = 'dodge') +
  labs(x = "Partisan Strength", y = "Percentage") +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  scale_y_continuous(labels=scales::percent) +
  theme(text = element_text(size=16)) +
  theme(legend.position="bottom")


##### Making a plot to compare out-party feeling among White and Latinos #####
##### with 0.95 confidence intervals #####
table(df$state)
df$state_r <- car::recode(df$state, " 'AZ'='Arizona'; 'FL'='Florida'; 'NC'='North Carolina'; 
                          'PA'='Pennsylvania'; 'WI'='Wisconsin'; else=NA")
table(df$state_r)


df1 <- df %>% select(race2, ft_out) %>% na.omit
df1.1 <- summarySE(df1, measurevar="ft_out", groupvars="race2", na.rm=T) 

p1 <- ggplot(df1.1, aes(x=as.factor(race2), y=ft_out, fill=as.factor(race2))) +
  geom_bar(aes(x=as.factor(race2), y=ft_out),  stat="identity") +
  geom_errorbar(aes(ymin=ft_out-ci, ymax=ft_out+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 40) +
  # xlab("Targets") + 
  ylab("Feeling Thermometer Towards Outparty") +
  ggtitle("Overall") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(aspect.ratio=3)


df2 <- df %>% select(race2, ft_out, state_r) %>% na.omit
df2.1 <- summarySE(df2, measurevar="ft_out", groupvars=c("race2", "state_r"), na.rm=T) 

pd <- position_dodge(0.9)
p2 <- ggplot(df2.1, aes(x=state_r, y=ft_out, fill=as.factor(race2))) +
  geom_bar(aes(x=state_r, y=ft_out),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft_out-ci, ymax=ft_out+ci), 
                width=0.4, alpha=0.9, size=1.5, position=pd) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 40) +
  # xlab("In Each State") + 
  ylab("") +
  ggtitle("In Each State") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank())


plot_grid(
  p1, p2, 
  align = 'hv',
  rel_widths = c(1,2)
)


##### Making a plot to compare out-party feeling among White and Latinos #####
##### with 0.83 confidence intervals #####
df1 <- df %>% select(race2, ft_out) %>% na.omit
df1.1 <- summarySE(df1, measurevar="ft_out", conf.interval = 0.83, groupvars="race2", na.rm=T) 

p1 <- ggplot(df1.1, aes(x=as.factor(race2), y=ft_out, fill=as.factor(race2))) +
  geom_bar(aes(x=as.factor(race2), y=ft_out),  stat="identity") +
  geom_errorbar(aes(ymin=ft_out-ci, ymax=ft_out+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 40) +
  # xlab("Targets") + 
  ylab("Feeling Thermometer Towards Outparty") +
  ggtitle("Overall") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(aspect.ratio=3)


df2 <- df %>% select(race2, ft_out, state_r) %>% na.omit
df2.1 <- summarySE(df2, measurevar="ft_out", conf.interval = 0.83, groupvars=c("race2", "state_r"), na.rm=T) 

pd <- position_dodge(0.9)
p2 <- ggplot(df2.1, aes(x=state_r, y=ft_out, fill=as.factor(race2))) +
  geom_bar(aes(x=state_r, y=ft_out),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft_out-ci, ymax=ft_out+ci), 
                width=0.4, alpha=0.9, size=1.5, position=pd) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 40) +
  # xlab("In Each State") + 
  ylab("") +
  ggtitle("In Each State") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank())


plot_grid(
  p1, p2, 
  align = 'hv',
  rel_widths = c(1,2)
)


##### Making a plot to compare in-party feeling among White and Latinos #####
##### with 0.95 confidence intervals #####
df3 <- df %>% dplyr::select(race2, ft_in) %>% na.omit
df3.1 <- summarySE(df3, measurevar="ft_in", groupvars="race2", na.rm=T) 

p3 <- ggplot(df3.1, aes(x=as.factor(race2), y=ft_in, fill=as.factor(race2))) +
  geom_bar(aes(x=as.factor(race2), y=ft_in),  stat="identity") +
  geom_errorbar(aes(ymin=ft_in-ci, ymax=ft_in+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 100) +
  # xlab("Targets") + 
  ylab("Feeling Thermometer Towards Inparty") +
  ggtitle("Overall") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(aspect.ratio=3)


df4 <- df %>% dplyr::select(race2, ft_in, state_r) %>% na.omit
df4.1 <- summarySE(df4, measurevar="ft_in", groupvars=c("race2", "state_r"), na.rm=T) 

pd <- position_dodge(0.9)
p4 <- ggplot(df4.1, aes(x=state_r, y=ft_in, fill=as.factor(race2))) +
  geom_bar(aes(x=state_r, y=ft_in),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft_in-ci, ymax=ft_in+ci), 
                width=0.4, alpha=0.9, size=1.5, position=pd) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 100) +
  # xlab("In Each State") + 
  ylab("") +
  ggtitle("In Each State") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank())


plot_grid(
  p3, p4, 
  align = 'hv',
  rel_widths = c(1,2)
)


##### Making a plot to compare in-party feeling among White and Latinos #####
##### with 0.83 confidence intervals #####
df3 <- df %>% dplyr::select(race2, ft_in) %>% na.omit
df3.1 <- summarySE(df3, measurevar="ft_in", conf.interval = 0.83, groupvars="race2", na.rm=T) 

p3 <- ggplot(df3.1, aes(x=as.factor(race2), y=ft_in, fill=as.factor(race2))) +
  geom_bar(aes(x=as.factor(race2), y=ft_in),  stat="identity") +
  geom_errorbar(aes(ymin=ft_in-ci, ymax=ft_in+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 100) +
  # xlab("Targets") + 
  ylab("Feeling Thermometer Towards Inparty") +
  ggtitle("Overall") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(aspect.ratio=3)


df4 <- df %>% dplyr::select(race2, ft_in, state_r) %>% na.omit
df4.1 <- summarySE(df4, measurevar="ft_in", conf.interval = 0.83, groupvars=c("race2", "state_r"), na.rm=T) 

pd <- position_dodge(0.9)
p4 <- ggplot(df4.1, aes(x=state_r, y=ft_in, fill=as.factor(race2))) +
  geom_bar(aes(x=state_r, y=ft_in),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft_in-ci, ymax=ft_in+ci), 
                width=0.4, alpha=0.9, size=1.5, position=pd) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 100) +
  # xlab("In Each State") + 
  ylab("") +
  ggtitle("In Each State") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank())


plot_grid(
  p3, p4, 
  align = 'hv',
  rel_widths = c(1,2)
)


##### Rescale main independent variable to 0-1 #####
df$age.r <- scales::rescale(df$age)
table(df$age.r)
df$income_r.r <- scales::rescale(df$income_r)
table(df$income_r.r)
df$pid_str.r <- scales::rescale(df$pid_str)
table(df$pid_str.r)
df$ideo.r <- scales::rescale(df$ideo)
table(df$ideo.r)
df$id_in.r <- scales::rescale(df$id_in)
table(df$id_in.r)
df$id_out.r <- scales::rescale(df$id_out)
table(df$id_out.r)
table(df$id_dif)
df$id_dif.r <- scales::rescale(df$id_dif)
table(df$id_dif.r)
df$race2.his <- 1 - df$race2



##### Regression models predicting out-party feeling by social sorting #####
lmb1.1 <- lm(ft_out ~ age.r + race2.his + gender2 + college + income_r.r + pid_str.r + ideo.r + id_in.r, df)
summary(lmb1.1)

lmb1.2 <- lm(ft_out ~ age.r + race2.his + gender2 + college + income_r.r + pid_str.r + ideo.r + id_out.r, df)
summary(lmb1.2)

lmb1.3 <- lm(ft_out ~ age.r +  race2.his + gender2 + college + income_r.r + pid_str.r + ideo.r + id_dif.r, df)
summary(lmb1.3)

stargazer(lmb1.3, lmb1.1, lmb1.2, 
          dep.var.labels = c("Feeling Towards Out-party"),
          covariate.labels=c("Age", "Hispanic", "Male", "Non-College", "Income", "Partisan Strength", "Ideology",  "Consistent - Inconsistent", "Consistent", "Inconsistent"),
          type="html",
          out="b1.html")

##### Calculating VIF scores for regression models #####
vif(lmb1.1)
vif(lmb1.2)
vif(lmb1.3)

##### Comparing out-party feeling among whites ana Latinos by strong/weak partisan #####
t.test(df$ft_out[df$race2==0][df$pid_str==4], df$ft_out[df$race2==0][df$pid_str==2], na.rm=T)
t.test(df$ft_out[df$race2==1][df$pid_str==4], df$ft_out[df$race2==1][df$pid_str==2], na.rm=T)
t.test(df$ft_out[df$race2==0][df$pid_str==2], df$ft_out[df$race2==1][df$pid_str==2], na.rm=T)
t.test(df$ft_out[df$race2==1][df$pid_str==4], df$ft_out[df$race2==0][df$pid_str==4], na.rm=T)

##### Calculating standard error for out-party feeling by race and partisan strength #####
std.error(df$ft_out[df$race2==0][df$pid_str==4])
std.error(df$ft_out[df$race2==0][df$pid_str==2])
std.error(df$ft_out[df$race2==0][df$pid_str==2])
std.error(df$ft_out[df$race2==1][df$pid_str==2])
std.error(df$ft_out[df$race2==1][df$pid_str==4])






##### Import Data #####
df <- read_dta("battleground_survey2.dta")



##### Cleaning Data and Recoding Variables #####
df$race2 <- car::recode(df$race, "1=1; 6=0; else=NA") ## 0-hispanics, 1-white
table(df$race2)

table(df$pid)
table(df$pid_leaner)
table(df$pid, df$pid_leaner)
table(df$pid, df$pid_strength_Dem)
table(df$pid, df$pid_strength_Rep)

df$pid7 <- ifelse(df$pid==1 & df$pid_strength_Dem==1, 1,
                  ifelse(df$pid==1 & df$pid_strength_Dem==2, 2,
                         ifelse(df$pid==3 & df$pid_leaner==2, 3,
                                ifelse(df$pid==3 & df$pid_leaner==3, 4,
                                       ifelse(df$pid==3 & df$pid_leaner==1, 5,
                                              ifelse(df$pid==2 & df$pid_strength_Rep==2, 6,
                                                     ifelse(df$pid==2 & df$pid_strength_Rep==1, 7,
                                                            NA)))))))
table(df$pid7)
df$pid3r <- car::recode(df$pid7, "1:3=1; 4=2; 5:7=3")
table(df$pid3r)

df$ft_dem <- car::recode(df$therm_democratic_1, "-99=NA")
table(df$ft_dem)
df$ft_rep <- car::recode(df$therm_republican_1, "-99=NA")
table(df$ft_rep)

df$ft_in <- ifelse(df$pid3r == 1, df$ft_dem, 
                   ifelse(df$pid3r == 3, df$ft_rep,
                          NA))
table(df$ft_in)

df$ft_out <- ifelse(df$pid3r == 3, df$ft_dem, 
                    ifelse(df$pid3r == 1, df$ft_rep,
                           NA))
table(df$ft_out)


table(df$therm_redux_9)
df$ft_rep.his <- df$therm_redux_9
df$ft_dem.his <- df$therm_redux_8


df$ft_out.his <- ifelse(df$pid3r == 3, df$ft_dem.his, 
                        ifelse(df$pid3r == 1, df$ft_rep.his,
                               NA))
table(df$ft_out.his)

##### Compare feeling towards the out-party and Hispanic out-partisans among Latinos #####
df1 <- df[df$race2==0, ] %>% dplyr::select(ft_out, ft_out.his) %>% na.omit()
t.test(df1$ft_out, df1$ft_out.his)
std.error(df1$ft_out)
std.error(df1$ft_out.his)

df1_long <- df1 %>% gather(cond, ft)

df2 <- summarySE(df1_long, measurevar="ft", groupvars="cond", na.rm=T) 

p1 <- ggplot(df2, aes(x=cond, y=ft, fill=cond)) +
  geom_bar(aes(x=cond, y=ft),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft-ci, ymax=ft+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Target", values = c("grey25", "grey75"),
                    labels= c("Out-party", "Hispanic Out-partisans")) +
  ylim(0, 60) +
  # xlab("In Each State") + 
  ylab("Feeling Thermometer (0-100)") +
  ggtitle("Battleground States Survey 2022 
  (Targets Asked Sequentially)") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())





