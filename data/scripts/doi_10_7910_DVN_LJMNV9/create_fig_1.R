# This R-file is generating the Figure 1

rm(list = ls())

library(textreg);library(pder);library(ggrepel);library(broom);library(margins);library(stargazer);library(modelr)
library(ggthemes);library(plm);library(readstata13);library(gridExtra);library(visreg);library(memisc);library(splines)
library(ggeffects);library(modelr);library(pglm);library(lmtest);library(lfe);library(clubSandwich);library(jtools)
library(ggstance);library(interflex);library(ggthemes);library(clusterSEs);library(AER); library(tidyverse); library(gtsummary)
library(tidyr)
library(dplyr)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission/")
getwd()

mydata <- read.dta13("Data/arealong_final.dta")
names(mydata)

table(mydata$area_num)
table(mydata$year)

mydata %>% filter(!is.na(year), area_num < 27) %>%
  group_by(year) %>%
  dplyr::summarize(Mean1 = mean(israelhayom_percent, na.rm=TRUE),
                   Mean2 = weighted.mean(israelhayom_percent, na.rm=TRUE, potential),
                   EY = mean(israelhayom_percent_electionyear, na.rm=TRUE),
                   potential = mean(potential, na.rm=TRUE))



# subset data; assign israelhayom_percent =0 pre-launch; and drop Eilat and "Other"
IH <-
  mydata %>% dplyr::select(
    area_num,
    year,
    voters,
    potential,
    israelhayom_percent,
    israelhayom_percent_cumulative,
    israelhayom_percent_electionyear,
    Plikud,
    Plabor,
    Pisrael_beitenu,
    Pkadima,
    Pbaityehudi,
    Pshas,
    Pmerez,
    ALL_rightbloc,
    RIGHTBLOC,
    BIG_rightbloc,
    age_median_08,
    africa_pcnt_08,
    europe_pcnt_08,
    matriccert_pcnt_08,
    national_pop,
    travel_dist,
    secular_max
  ) %>%
  replace_na(list(israelhayom_percent = 0)) %>% group_by(area_num) %>%
  dplyr::mutate(yr = dplyr::row_number()) %>% dplyr::filter(area_num < 27)

colnames(IH)[colnames(IH)=="BIG_rightbloc"] <- "RightBloc"

IH$IH08 <- NA
for (i in unique(IH$area_num)){
  IH$IH08[IH$area_num==i] <- IH$israelhayom_percent_electionyear[IH$year==2009 & IH$area_num==i]
}

IH$IH13 <- NA
for (i in unique(IH$area_num)){
  IH$IH13[IH$area_num==i] <- IH$israelhayom_percent_electionyear[IH$year==2013 & IH$area_num==i]
}

IH$IH15 <- NA
for (i in unique(IH$area_num)){
  IH$IH15[IH$area_num==i] <- IH$israelhayom_percent_electionyear[IH$year==2015 & IH$area_num==i]
}

IH$population <- NA
for (i in unique(IH$area_num)){
  IH$population[IH$area_num==i] <- IH$national_pop[IH$year==2009 & IH$area_num==i]
}

for (i in unique(IH$area_num)){
  IH$secular_max[IH$area_num==i] <- IH$secular_max[IH$year==2006 & IH$area_num==i]
}

# craete binary measure of high and low exposure to IH (by median)
IH <- IH %>% group_by(year) %>% mutate(highexp= ifelse(israelhayom_percent_electionyear>median(israelhayom_percent_electionyear, na.rm=T), 1, 0))

for (i in unique(IH$area_num)){
  IH$highexp[IH$area_num==i] <- IH$highexp[IH$yr==7 & IH$area_num==i]
}

table(IH$highexp)

IH %>% group_by(year) %>%
  dplyr::summarize(Mean1 = mean(israelhayom_percent, na.rm=TRUE),
                   Mean2 = mean(israelhayom_percent_cumulative, na.rm=TRUE),
                   Mean3 = mean(israelhayom_percent_electionyear, na.rm=TRUE),
                   Mean4 = mean(potential, na.rm=TRUE))

ihp<-ggplot(data=subset(IH,year>2006), aes(x=year, y=israelhayom_percent, size=population, weight=population)) +
         geom_point() + theme_light() + geom_smooth(method="loess", se=T, color = "red") +
  labs(x="", y="Israel Hayom Exposure") + geom_jitter() +
  ggtitle("") +
  scale_x_continuous(breaks = (seq(min(2007), max(2015), by = 1)), limits = c(2007, 2015)) +
  scale_y_continuous(breaks = (seq(min(0), max(60), by = 10)), limits = c(0, 60)) +
  theme(
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))

ggsave(filename = "Figures/Fig_1.png",
       plot = ihp,
       height = 3, width = 7, units = "in")
