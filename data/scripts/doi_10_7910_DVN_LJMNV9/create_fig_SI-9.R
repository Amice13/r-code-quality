# This R-file is generating the Figure SI-9

rm(list = ls())

library(textreg);library(pder);library(ggrepel);library(broom);library(margins);library(stargazer);library(modelr)
library(ggthemes);library(plm);library(readstata13);library(gridExtra);library(visreg);library(memisc);library(splines)
library(ggeffects);library(modelr);library(pglm);library(lmtest);library(lfe);library(clubSandwich);library(jtools)
library(ggstance);library(interflex);library(ggthemes);library(clusterSEs);library(AER); library(tidyverse); library(gtsummary)
library(tidyr)
library(dplyr)

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

# plot IH 2008 against party vote share in previous years#
#-----------------------------------------------------

electiony <- IH %>% filter(year<2007 | year==2009 | year==2013 | year==2015)
names(electiony)

table(electiony$IH15)

electiony %>% group_by(year) %>% dplyr::summarize(Mean1 = mean(IH13, na.rm=TRUE),
                                                  Mean2 = mean(yr, na.rm=TRUE))

# Vote share by party
parties <- c("RightBloc", "Plikud", "Plabor", "Pmerez", "Pbaityehudi","Pshas")
parties2 <- c("Right Bloc", "Likud", "Labor", "Merez", "Bait Yehudi","Shas")

parties3 <- c("RightBloc", "Plikud", "Plabor", "Pmerez", "Pbaityehudi","Pshas", "Pisrael_beitenu", "Pkadima")
parties4 <- c("Right Bloc", "Likud", "Labor", "Merez", "Bait Yehudi","Shas", "Israel Beitenu", "Kadima")


p0 <- list()
p1 <- list()
p2 <- list()
p3 <- list()
p4 <- list()
p5 <- list()
p6 <- list()
p7 <- list()

for (i in 1:length(parties)) {

  t <-
    paste0("ggplot(data=subset(electiony, IH15>20) , mapping = aes(x = IH13, y = ",
           parties[i],
           ", group=year, size = potential)) + guides(size = FALSE)")

  p0[[parties[i]]] <- eval(parse(text = t))

  t1 <-
    paste0(
      "p0[[\"", parties[i], "\"]] + geom_point(alpha = 0.3, aes(color = factor(year))) + theme_bw() +
      labs(x = \"Israel Hayom 2012\", y =\"",
      parties2[i],
      " Vote Share\",  color = \"Election\nYear\") +
      geom_smooth(method = \"lm\", se = F, aes(color = factor(year), weight = potential))"
    )

  p1[[parties[i]]] <- eval(parse(text = t1))

  t2 <-
    paste0(
      "p0[[\"", parties[i], "\"]] + geom_point(alpha = 0.4, aes(color = factor(year))) +  theme_bw() +
            labs(x = \"Israel Hayom 2014\", y = \"",
      parties2[i],
      " Vote Share\") +
            geom_smooth(method = \"lm\", se = T, aes(color = factor(year), weight = potential)) +
            theme(legend.position = \"none\") + facet_wrap( ~ factor(year))"
    )

  p2[[parties[i]]] <- eval(parse(text = t2))

  t3 <-
    paste0(
      "p0[[\"", parties[i], "\"]] + geom_point(alpha = 0.4, aes(color = factor(year))) + theme_bw() +
            labs(x = \"Israel Hayom 2014\", y = \"",
      parties2[i],
      " Vote Share\") +
            geom_smooth(method = \"loess\", se = T, aes(color = factor(year))) +
            theme(legend.position = \"none\") + facet_wrap( ~ factor(year))"
    )

  p3[[parties[i]]] <- eval(parse(text = t3))

  t4 <-
    paste0("ggplot(data=subset(electiony, IH15>20) , mapping = aes(x = IH15, y = ",
           parties[i],
           ", group=year, size = potential)) + guides(size = FALSE)")

  p4[[parties[i]]] <- eval(parse(text = t4))

  t5 <-
    paste0(
      "p4[[\"", parties[i], "\"]] + geom_point(alpha = 0.3, aes(color = factor(year))) + theme_bw() +
      labs(x = \"Israel Hayom 2014\", y =\"",
      parties2[i],
      " Vote Share\",  color = \"Election\nYear\") +
      geom_smooth(method = \"lm\", se = F, aes(color = factor(year)))"
    )

  p5[[parties[i]]] <- eval(parse(text = t5))

  t6 <-
    paste0(
      "p4[[\"", parties[i], "\"]] + geom_point(alpha = 0.4, aes(color = factor(year))) +  theme_bw() +
      labs(x = \"Israel Hayom 2014\", y = \"",
      parties2[i],
      " Vote Share\") +
      geom_smooth(method = \"lm\", se = T, aes(color = factor(year))) +
      theme(legend.position = \"none\") + facet_wrap( ~ factor(year))"
    )

  p6[[parties[i]]] <- eval(parse(text = t6))

  t7 <-
    paste0(
      "p4[[\"", parties[i], "\"]] + geom_point(alpha = 0.4, aes(color = factor(year))) + theme_bw() +
      labs(x = \"Israel Hayom 2014\", y = \"",
      parties2[i],
      " Vote Share\") +
      geom_smooth(method = \"loess\", se = T, aes(color = factor(year))) +
      theme(legend.position = \"none\") + facet_wrap( ~ factor(year))"
    )

  p7[[parties[i]]] <- eval(parse(text = t7))
}

pdf("Figures/Fig_SI-9.pdf",width=6,height=4)
p2[["RightBloc"]]
dev.off()
