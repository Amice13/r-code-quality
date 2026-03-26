##### Installing Packages #####
# Uncomment the following codes if you need to install the following packages
# install.packages(c("ggplot2", "dplyr", "Rmisc", "ggpubr", "cowplot", "haven",
#                    "stargazer", "sjPlot", "sjmisc", "scales", "plotrix"))

##### Loading Packages #####
library(haven)
library(stargazer)
library(margins)
library(pscl)
library(sjPlot)
library(sjmisc)
library(magrittr) 
library(ggplot2)
library(scales)
library(dplyr)
library(Rmisc)
library(ggpubr)
library(cowplot)
library(plotrix)
library(tidyr)


##### Import Data #####
df <- read_dta("western_states_survey.dta")



##### Cleaning Data and Recoding Variables #####

df$age <- 2020 - df$birthyr
table(df$age)
table(df$pid7)
df$pid7 <- car::recode(df$pid7, "8=NA")
table(df$pid7)
df$pid3 <- car::recode(df$pid7, "1:3=1; 4=2; 5:7=3;")
table(df$pid3)

df$pid_str <- car::recode(df$pid7, "1=4; 2=3; 3=2; 4=1; 5=2; 6=3; 7=4")
table(df$pid_str)

table(df$ideo5)
df$ideo5.r <- car::recode(df$ideo5, "6=3")
table(df$ideo5.r)
df$ideo_str <- car::recode(df$ideo5.r, "1=3; 2=2; 3=1; 4=4")

table(df$race)
df$race3 <- car::recode(df$race, "1=1; 2=2; 3=3; 4:8=NA")
table(df$race3)
df$race2 <- car::recode(df$race3, "1=1; 3=0; 2=NA") # 1-white, 0-hispanics
table(df$race2)

df$inputstate <- as.numeric(df$inputstate)
table(df$inputstate)
df$state_residence <- car::recode(df$inputstate, "4='Arizona'; 8='Colorado'; 32='Nevada'; 35='New Mexico'; 49='Utah'")
table(df$state_residence)

table(df$state_residence)
df$state_r <- df$state_residence

table(df$educ)
df$college <- car::recode(df$educ, "1:3=0; 4:6=1")
table(df$college)

# feeling thermometer in all the sample
table(as.numeric(df$WSS29_1))
df$ft_gop1 <- as.numeric(df$WSS29_1) 
df$ft_dem1 <- as.numeric(df$WSS29_2) 
table(df$ft_gop1)
table(df$ft_dem1)

df$ft_out1 <- ifelse(df$pid3 == 1, df$ft_gop1,
                     ifelse(df$pid3 == 3, df$ft_dem1,
                            NA))
table(df$ft_out1)

df$ft_in1 <- ifelse(df$pid3 == 1, df$ft_dem1,
                    ifelse(df$pid3 == 3, df$ft_gop1,
                           NA))
table(df$ft_in1)

# feeling thermometer in AZ

table(as.numeric(df$AZ301))
summary(df$AZ301)
table(as.numeric(df$AZ302))
summary(df$AZ302)
df$ft_dem <- as.numeric(df$AZ301) - 1
df$ft_gop <- as.numeric(df$AZ302) - 1
df$ft_dif <- ifelse(df$pid3 == 1, df$ft_dem - df$ft_gop,
                    ifelse(df$pid3 == 3, df$ft_gop - df$ft_dem,
                           NA))
table(df$ft_dif)


df$ft_out <- ifelse(df$pid3 == 1, df$ft_gop,
                    ifelse(df$pid3 == 3, df$ft_dem,
                           NA))
table(df$ft_out)

# feeling thermometer in AZ

table(as.numeric(df$AZ301))
summary(df$AZ301)
table(as.numeric(df$AZ302))
summary(df$AZ302)
df$ft_dem <- as.numeric(df$AZ301) - 1
df$ft_gop <- as.numeric(df$AZ302) - 1
df$ft_dif <- ifelse(df$pid3 == 1, df$ft_dem - df$ft_gop,
                    ifelse(df$pid3 == 3, df$ft_gop - df$ft_dem,
                           NA))
table(df$ft_dif)


df$ft_out <- ifelse(df$pid3 == 1, df$ft_gop,
                    ifelse(df$pid3 == 3, df$ft_dem,
                           NA))
table(df$ft_out)

# feeling towards hispanic community 

table(as.numeric(df$AZ304))
summary(df$AZ304)
table(as.numeric(df$AZ305))
summary(df$AZ305)

df$ft_dem.his <- as.numeric(df$AZ304) - 1
df$ft_gop.his <- as.numeric(df$AZ305) - 1
df$ft_dif.his <- ifelse(df$pid3 == 1, df$ft_dem.his - df$ft_gop.his,
                        ifelse(df$pid3 == 3, df$ft_gop.his - df$ft_dem.his,
                               NA))
table(df$ft_dif.his)

df$ft_out.his <- ifelse(df$pid3 == 1, df$ft_gop.his,
                        ifelse(df$pid3 == 3, df$ft_gop.his,
                               NA))
table(df$ft_out.his)


##### Making plot to show the distribution of partisan strength by race #####
df6 <- df %>% dplyr::select(pid_str, race2) %>% na.omit()

df6$race2.f <- factor(df6$race2, levels= c(0,1),
                     labels = c("Latino", "Non-Latino White"))

df6 %>%  
  dplyr::count(race2.f, pid_str) %>% 
  group_by(race2.f) %>% 
  dplyr::mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = as.factor(pid_str), y = freq, fill = race2.f)) + 
  geom_bar(stat="identity", position = 'dodge') +
  labs(x = "Partisan Strength", y = "Percentage") +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Latino", "Non-Latino White")) +
  scale_y_continuous(labels=scales::percent) +
  #scale_linetype_manual("Party ID", values=c("solid", "dashed"), labels=c("Republicans", "Democrats")) +
  theme(text = element_text(size=16)) +
  theme(legend.position="bottom")




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
  theme(text = element_text(size=16)) +
  theme(legend.position="bottom")



##### Making plot to show the distribution of education by race #####
df7 <- df %>% dplyr::select(educ, race2) %>% na.omit()

df7$race2.f <- factor(df7$race2, levels= c(0,1),
                      labels = c("Latino", "Non-Latino White"))

df7$educ_c <- car::recode(as.numeric(df7$educ), "1='Below High School'; 2='High School';
                          3='Some College'; 4='2-year College'; 5='4-year College';
                          6='Post-graduate' ")
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
  theme(text = element_text(size=16)) +
  theme(legend.position="bottom")


##### Making a plot to compare out-party feeling among White and Latinos #####
##### with 0.83 confidence intervals #####
df1 <- df %>% dplyr::select(race2, ft_out1) %>% na.omit
df1.1 <- summarySE(df1, measurevar="ft_out1", conf.interval = 0.83, groupvars="race2", na.rm=T) 

p1 <- ggplot(df1.1, aes(x=as.factor(race2), y=ft_out1, fill=as.factor(race2))) +
  geom_bar(aes(x=as.factor(race2), y=ft_out1),  stat="identity") +
  geom_errorbar(aes(ymin=ft_out1-ci, ymax=ft_out1+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Latino", "Non-Latino White")) +
  ylim(0, 50) +
  # xlab("Targets") + 
  ylab("Feeling Thermometer Towards Outparty") +
  ggtitle("Overall") +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(aspect.ratio=3) 


df2 <- df %>% dplyr::select(race2, ft_out1, state_r) %>% na.omit()
df2.1 <- summarySE(df2, measurevar="ft_out1", conf.interval = 0.83,
                   groupvars=c("race2", "state_r"), na.rm=T) 

pd <- position_dodge(0.9)
p2 <- ggplot(df2.1, aes(x=state_r, y=ft_out1, fill=as.factor(race2))) +
  geom_bar(aes(x=state_r, y=ft_out1),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft_out1-se, ymax=ft_out1+se), 
                width=0.4, alpha=0.9, size=1.5, position=pd) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Latino", "Non-Latino White")) +
  ylim(0, 50) +
  # xlab("In Each State") + 
  ylab("") +
  ggtitle("In Each State") +
  theme_grey() +
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
##### with 0.95 confidence intervals #####

df1 <- df %>% dplyr::select(race2, ft_out1) %>% na.omit
df1.1 <- summarySE(df1, measurevar="ft_out1", groupvars="race2", na.rm=T) 

alpha <- 0.5 
df1.1$df <- df1.1$N - 1
df1.1$t <- qt(p=alpha/2, df=df1.1$df, lower.tail=F)
df1.1$ci <- df1.1$t*df1.1$se

p1 <- ggplot(df1.1, aes(x=as.factor(race2), y=ft_out1, fill=as.factor(race2))) +
  geom_bar(aes(x=as.factor(race2), y=ft_out1),  stat="identity") +
  geom_errorbar(aes(ymin=ft_out1-ci, ymax=ft_out1+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 50) +
  # xlab("Targets") + 
  ylab("Feeling Thermometer Towards Outparty") +
  ggtitle("Overall") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(aspect.ratio=3) 


df2 <- df %>% dplyr::select(race2, ft_out1, state_r) %>% na.omit()
df2.1 <- summarySE(df2, measurevar="ft_out1", 
                   groupvars=c("race2", "state_r"), na.rm=T) 

alpha <- 0.5 
df2.1$df <- df2.1$N - 1
df2.1$t <- qt(p=alpha/2, df=df2.1$df, lower.tail=F)
df2.1$ci <- df2.1$t*df2.1$se



pd <- position_dodge(0.9)
p2 <- ggplot(df2.1, aes(x=state_r, y=ft_out1, fill=as.factor(race2))) +
  geom_bar(aes(x=state_r, y=ft_out1),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft_out1-ci, ymax=ft_out1+ci), 
                width=0.4, alpha=0.9, size=1.5, position=pd) +
  scale_fill_manual(name = "Ethnicity", 
                    breaks = c(0, 1),
                    values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 50) +
  # xlab("In Each State") + 
  ylab("") +
  ggtitle("In Each State") +
  theme_light() +
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
##### with 0.83 confidence intervals #####
df3 <- df %>% dplyr::select(race2, ft_in1) %>% na.omit
df3.1 <- summarySE(df3, measurevar="ft_in1", conf.interval = 0.83, groupvars="race2", na.rm=T)


p3 <- ggplot(df3.1, aes(x=as.factor(race2), y=ft_in1, fill=as.factor(race2))) +
  geom_bar(aes(x=as.factor(race2), y=ft_in1),  stat="identity") +
  geom_errorbar(aes(ymin=ft_in1-ci, ymax=ft_in1+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 100) +
  # xlab("Targets") + 
  ylab("Feeling Thermometer Towards In-party") +
  ggtitle("Overall") +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(aspect.ratio=3) 

t.test(df$ft_in1[df$race2==1], df$ft_in1[df$race2==0], na.rm=T)

df4 <- df %>% dplyr::select(race2, ft_in1, state_r) %>% na.omit()
df4.1 <- summarySE(df4, measurevar="ft_in1", conf.interval = 0.83,
                   groupvars=c("race2", "state_r"), na.rm=T) 


pd <- position_dodge(0.9)
p4 <- ggplot(df4.1, aes(x=state_r, y=ft_in1, fill=as.factor(race2))) +
  geom_bar(aes(x=state_r, y=ft_in1),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft_in1-ci, ymax=ft_in1+ci), 
                width=0.4, alpha=0.9, size=1.5, position=pd) +
  scale_fill_manual("Ethnicity", values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 100) +
  # xlab("In Each State") + 
  ylab("") +
  ggtitle("In Each State") +
  theme_grey() +
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
##### with 0.95 confidence intervals #####

df3 <- df %>% dplyr::select(race2, ft_in1) %>% na.omit
df3.1 <- summarySE(df3, measurevar="ft_in1", groupvars="race2", na.rm=T)


alpha <- 0.5 
df3.1$df <- df3.1$N - 1
df3.1$t <- qt(p=alpha/2, df=df3.1$df, lower.tail=F)
df3.1$ci <- df3.1$t*df3.1$se


p3 <- ggplot(df3.1, aes(x=as.factor(race2), y=ft_in1, fill=as.factor(race2))) +
  geom_bar(aes(x=as.factor(race2), y=ft_in1),  stat="identity") +
  geom_errorbar(aes(ymin=ft_in1-ci, ymax=ft_in1+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual(name = "Ethnicity", 
                    breaks = c(0, 1),
                    values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 100) +
  # xlab("Targets") + 
  ylab("Feeling Thermometer Towards In-party") +
  ggtitle("Overall") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(aspect.ratio=3) 

t.test(df$ft_in1[df$race2==1], df$ft_in1[df$race2==0], na.rm=T)



df4 <- df %>% dplyr::select(race2, ft_in1, state_r) %>% na.omit()
df4.1 <- summarySE(df4, measurevar="ft_in1",
                   groupvars=c("race2", "state_r"), na.rm=T) 

alpha <- 0.5 
df4.1$df <- df4.1$N - 1
df4.1$t <- qt(p=alpha/2, df=df4.1$df, lower.tail=F)
df4.1$ci <- df4.1$t*df4.1$se


pd <- position_dodge(0.9)
p4 <- ggplot(df4.1, aes(x=state_r, y=ft_in1, fill=as.factor(race2))) +
  geom_bar(aes(x=state_r, y=ft_in1),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft_in1-ci, ymax=ft_in1+ci), 
                width=0.4, alpha=0.9, size=1.5, position=pd) +
  scale_fill_manual(name = "Ethnicity", 
                    breaks = c(0, 1),
                    values = c("grey25", "grey75"),
                    labels= c("Hispanic", "Non-Hispanic White")) +
  ylim(0, 100) +
  # xlab("In Each State") + 
  ylab("") +
  ggtitle("In Each State") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank())


plot_grid(
  p3, p4, 
  align = 'hv',
  rel_widths = c(1,2)
)



##### Making a plot to compare people's feeling towards out-partisans in Arizona #####
##### versus out-partisans in Latino community among Latinos #####
dfx <- df[df$state=="Arizona", ]

dfx$ft <- ifelse(dfx$AZ_split == 1, dfx$ft_out,
                 ifelse(dfx$AZ_split == 2, dfx$ft_out.his, 
                        NA))
table(dfx$ft)

dfx$cond <- ifelse(dfx$AZ_split == 1, "az",
                 ifelse(dfx$AZ_split == 2, "his", 
                        NA))
table(dfx$cond)
dfx1 <- dfx[dfx$race2==0, ]

az <- dfx1 %>% dplyr::select(ft, cond) %>% na.omit()
az1 <- summarySE(az, measurevar="ft", groupvars="cond", na.rm=T) 


p2 <- ggplot(az1, aes(x=cond, y=ft, fill=cond)) +
  geom_bar(aes(x=cond, y=ft),  stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=ft-ci, ymax=ft+ci), 
                width=0.4, alpha=0.9, size=1.5) +
  scale_fill_manual("Target", values = c("grey25", "grey75"),
                    labels= c("Out-partisans in Arizona", "Out-partisans in Hispanic Community")) +
  ylim(0, 6) +
  # xlab("In Each State") + 
  ylab("Feeling Thermometer (0-10)") +
  ggtitle("Western States Survey 2020
  (Targets Randomly Assigned)") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=16)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())





##### Comparing out-party feeling among whites ana Latinos by strong/weak partisan #####
t.test(df$ft_out1[df$race2==0][df$pid_str==4], df$ft_out1[df$race2==0][df$pid_str==2], na.rm=T)
t.test(df$ft_out1[df$race2==0][df$pid_str==2], df$ft_out1[df$race2==1][df$pid_str==2], na.rm=T)
t.test(df$ft_out1[df$race2==1][df$pid_str==4], df$ft_out1[df$race2==1][df$pid_str==2], na.rm=T)
t.test(df$ft_out1[df$race2==1][df$pid_str==4], df$ft_out1[df$race2==0][df$pid_str==4], na.rm=T)

##### Calculating standard error for out-party feeling by race and partisan strength #####
std.error(df$ft_out1[df$race2==0][df$pid_str==4])
std.error(df$ft_out1[df$race2==0][df$pid_str==2])
std.error(df$ft_out1[df$race2==1][df$pid_str==2])
std.error(df$ft_out1[df$race2==1][df$pid_str==4])













