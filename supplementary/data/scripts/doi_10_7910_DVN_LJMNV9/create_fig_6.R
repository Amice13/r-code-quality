# This R-file is generating the Figure 6

rm(list = ls())

library(textreg);library(pder);library(ggrepel);library(broom);library(margins);library(stargazer);library(modelr)
library(ggthemes);library(plm);library(readstata13);library(gridExtra);library(visreg);library(memisc);library(splines)
library(ggeffects);library(modelr);library(pglm);library(lmtest);library(lfe);library(clubSandwich);library(jtools)
library(ggstance);library(interflex);library(ggthemes);library(clusterSEs);library(AER); library(tidyverse); library(gtsummary)
library(tidyr)
library(dplyr)

#get data
setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")
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


#-----------------------------------------------------
# create cross-sectional data
# Average the four elections prior to 2007
#-----------------------------------------------------

test.elections <- c("IH2009", "IH2012", "IH2015")
election_years <- c(1996, 1999, 2003, 2006, 2009, 2013, 2015)

datasets = list()
for (i in 5:7) {
  y <- election_years[i]
  temp <- IH %>% group_by(area_num) %>%
    dplyr::summarize(
      treat = israelhayom_percent_electionyear[year == y],
      treat2 = israelhayom_percent_cumulative[year == y],
      RIGHTBLOC0 = mean(RIGHTBLOC[year %in% election_years[1:4]], na.rm=T),
      RIGHTBLOC1 = mean(RIGHTBLOC[year == y], na.rm=T),
      RightBloc2 = mean(RightBloc[year == 2006], na.rm=T),
      RightBloc0 = mean(RightBloc[year %in% election_years[1:4]], na.rm=T),
      RightBloc1 = mean(RightBloc[year == y], na.rm=T),
      likud2 = mean(Plikud[year == 2006], na.rm=T),
      likud0 = mean(Plikud[year %in% election_years[1:4]], na.rm=T),
      likud1 = mean(Plikud[year == y], na.rm=T),
      labor0 = mean(Plabor[year %in% election_years[1:4]], na.rm=T),
      labor1 = mean(Plabor[year == y], na.rm=T),
      shas0 = mean(Pshas[year %in% election_years[1:4]], na.rm=T),
      shas1 = mean(Pshas[year == y], na.rm=T),
      kadima0 = mean(Pkadima[year %in% election_years[1:4]], na.rm = T),
      kadima1 = mean(Pkadima[year == y], na.rm=T),
      IB0 = mean(Pisrael_beitenu[year %in% election_years[1:4]], na.rm = T),
      IB1 = mean(Pisrael_beitenu[year == y], na.rm=T),
      merez0 = mean(Pmerez[year %in% election_years[1:4]], na.rm=T),
      merez1 = mean(Pmerez[year == y], na.rm=T),
      baityehudi0 = mean(Pbaityehudi[year %in% election_years[1:4]], na.rm=T),
      baityehudi1 = mean(Pbaityehudi[year == y], na.rm=T),
      population = population[year == y],
      adultpop = potential[year == y],
      voters = voters[year == y],
      age = age_median_08[year == 2009],
      africa = africa_pcnt_08[year == 2009],
      europe = europe_pcnt_08[year == 2009],
      matric = matriccert_pcnt_08[year == 2009],
      secular_max = secular_max[year == 2006]
    ) %>% dplyr::mutate(
      dRight = RIGHTBLOC1 - RIGHTBLOC0,
      dRightBloc = RightBloc1 - RightBloc0,
      dlikud = likud1 - likud0,
      dlabor = labor1 - labor0,
      dshas = shas1 - shas0,
      dkadima = kadima0,
      dIB = IB1 - IB0,
      dbaityehudi = baityehudi1- baityehudi0,
      turnout = voters/adultpop
    )
  temp$right <- ifelse(temp$RightBloc0>median(temp$RightBloc0), 1, 0)
  datasets[[i-4]] = temp
}

names(datasets) = test.elections

# Correlations ------------------------------------------------------------

# raw data correlation between change in vote share and IH exposure in 2013

B1 <- ggplot(data=subset(datasets$IH2012,treat>20), mapping = aes(x = treat, y = dRightBloc)) +
  geom_point(aes(size=adultpop/1000)) + theme_light() + geom_smooth(method="lm", se=T, color = "firebrick") +
  labs(x="Israel Hayom 2012", y="2013-Pre", size = "Population") +
  ggtitle("Right Bloc (2013)") + theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))

B2 <- ggplot(data=subset(datasets$IH2012,treat>20), mapping = aes(x = treat, y = dlikud)) +
  geom_point(aes(size=adultpop/1000)) + theme_light() + geom_smooth(method="lm", se=T, color = "firebrick") +
  labs(x="Israel Hayom 2012", y="2013-Pre", size = "Population") +
  ggtitle("Likud (2013)") + theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))

B3<-ggplot(data=subset(datasets$IH2012,treat>20), mapping = aes(x = treat, y = dRightBloc)) + geom_point(aes(size=adultpop/1000)) +
  theme_light() + geom_smooth(method="lm", se=T, color = "firebrick") +
  labs(x="Israel Hayom 2012", y="Diff in Vote Share (pre-IH and 2013)", size = "Population") +
  ggtitle("Right Bloc (2013)") + theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))

B4<-ggplot(data=subset(datasets$IH2012,treat>20), mapping = aes(x = treat, y = dlikud)) + geom_point(aes(size=adultpop/1000)) +
  theme_light() + geom_smooth(method="lm", se=T, color = "firebrick") +
  labs(x="Israel Hayom 2012", y="2013-Pre", size = "Population") +
  ggtitle("Likud (2013)") + theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))

# raw data correlation between change in vote share and IH exposure in 2015

C1 <- ggplot(data=subset(datasets$IH2015,treat>20), mapping = aes(x = treat, y = dRightBloc)) +
  geom_point(aes(size=adultpop/1000)) + theme_light() + geom_smooth(method="lm", se=T, color = "firebrick") +
  labs(x="Israel Hayom 2014", y="2015-Pre", size = "Population") +
  ggtitle("Right Bloc (2015)") + theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))


C2 <- ggplot(data=subset(datasets$IH2015,treat>20), mapping = aes(x = treat, y = dlikud)) +
  geom_point(aes(size=adultpop/1000)) + theme_light() + geom_smooth(method="lm", se=T, color = "firebrick") +
  labs(x="Israel Hayom 2014", y="2015-Pre", size = "Population") +
  ggtitle("Likud (2015)") + theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))


C3<-ggplot(data=subset(datasets$IH2015,treat>20), mapping = aes(x = treat, y = dRightBloc)) + geom_point(aes(size=adultpop/1000)) +
  theme_light() + geom_smooth(method="lm", se=T, color = "firebrick") +
  labs(x="Israel Hayom 2014", y="Diff in Vote Share (pre-IH and 2015)", size = "Population", color = "firebrick") +
  ggtitle("Right Bloc (2015)") + theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))

C4<-ggplot(data=subset(datasets$IH2015,treat>20), mapping = aes(x = treat, y = dlikud)) + geom_point(aes(size=adultpop/1000)) +
  theme_light() + geom_smooth(method="lm", se=T, color = "firebrick") +
  labs(x="Israel Hayom 2014", y="2015-Pre", size = "Population") +
  ggtitle("Likud (2015)") + theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    plot.title=element_text(face="bold"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"))

###############################################################################

inprogress12<-gather(data=subset(datasets$IH2012,treat>20),period,value, RightBloc0:RightBloc1,RightBloc2) %>%
  dplyr::select(area_num,period, value, treat, dRightBloc, adultpop) %>%
  arrange(area_num, period)

inprogress12$period<-as.factor(inprogress12$period)
levels(inprogress12$period)[levels(inprogress12$period)=="RightBloc0"] <- "Pre (all)"
levels(inprogress12$period)[levels(inprogress12$period)=="RightBloc1"] <- "Post"
levels(inprogress12$period)[levels(inprogress12$period)=="RightBloc2"] <- "Pre (last)"

inprogress15<-gather(data=subset(datasets$IH2015,treat>20),period,value,RightBloc0:RightBloc1, RightBloc2) %>%
  dplyr::select(area_num,period,value, treat, dRightBloc, adultpop) %>%
  arrange(area_num, period)

inprogress15$period<-as.factor(inprogress15$period)
levels(inprogress15$period)[levels(inprogress15$period)=="RightBloc0"] <- "Pre (all)"
levels(inprogress15$period)[levels(inprogress15$period)=="RightBloc1"] <- "Post"
levels(inprogress15$period)[levels(inprogress15$period)=="RightBloc2"] <- "Pre (last)"

minX1 <- min(inprogress15$treat)
maxX1 <- max(inprogress12$treat)

mycolours <- c("Pre (all)" = "gray80", "Pre (last)" = "gray55", "Post" = "black")

colnames(inprogress12)


E1 <- ggplot(data=inprogress12, mapping = aes(x = treat, y = value, size = adultpop, group=period)) +
  ggtitle("Right Bloc (2013)") +
  theme_bw() +
  geom_point(aes(color=factor(period))) +
  labs(x="Israel Hayom 2012", y="Vote Share in 2013",  color="Period") +
  geom_smooth(method="lm", se=F, aes(color=factor(period), weight = adultpop)) +
  theme(legend.position = c(0.75, 0.15), axis.text=element_text(size=12),
        plot.title=element_text(face="bold"),
        axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"),
        legend.background = element_rect(color = "black", size = .25, linetype = "solid")) +
  scale_color_manual("Period", values = mycolours, breaks = c("Pre (all)", "Pre (last)", "Post")) +
  scale_x_continuous(limits = c(minX1, maxX1)) +
  guides(size = FALSE)


E2 <-ggplot(inprogress15, mapping = aes(x = treat, y = value, size = adultpop, group=period)) +
  ggtitle("Right Bloc (2015)") +
  geom_point(aes(color=factor(period))) +
  theme_bw() +
  labs(x="Israel Hayom 2014", y="Vote Share in 2015",  color="Period") +
  geom_smooth(method="lm", se=F, aes(color=factor(period), weight = adultpop)) +
  theme(legend.position = c(0.75, 0.15), axis.text=element_text(size=12),
        plot.title=element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.background = element_rect(color = "black", size = .25, linetype = "solid")) +
  scale_color_manual("Period", values = mycolours, breaks = c("Pre (all)", "Pre (last)", "Post")) +
  scale_x_continuous(limits = c(minX1, maxX1)) +
  guides(size = FALSE)

pdf("Figures/Fig_6.pdf", width=10, height=10)
grid.arrange(E1, B3 + scale_x_continuous(limits = c(minX1, maxX1)), E2,
             C3 + scale_x_continuous(limits = c(minX1, maxX1)), widths = 4:5, ncol=2)
dev.off()
