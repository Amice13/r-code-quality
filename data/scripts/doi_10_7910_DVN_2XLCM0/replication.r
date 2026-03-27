# Load libraries

library(lmtest)
library(sandwich)
library(stargazer)
library(ggplot2)
library(ggeffects)
library(tidyverse)

# Load data

police <- read.csv("police_trust.csv", header=TRUE)

# Create table of levels of trust by region, 
# plus gender and ethnicity

regions <- c("North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West") 

trustLevel <- c(
	round(mean(police$trustPolice[police$profile_GOR == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 2], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 3], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 4], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 5], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 6], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 7], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 8], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 9], na.rm=TRUE), 2) 
)

trustRegion <- data.frame(regions, trustLevel) 
trustRegion[order(trustRegion$trustLevel), ]


trustLevelMen <- c(
	round(mean(police$trustPolice[police$profile_GOR == 1 & police$women == 0], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 2 & police$women == 0], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 3 & police$women == 0], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 4 & police$women == 0], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 5 & police$women == 0], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 6 & police$women == 0], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 7 & police$women == 0], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 8 & police$women == 0], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 9 & police$women == 0], na.rm=TRUE), 2) 
)

trustRegionMen <- data.frame(regions, trustLevelMen) 
trustRegionMen[order(trustRegionMen$trustLevelMen), ]

trustLevelWomen <- c(
	round(mean(police$trustPolice[police$profile_GOR == 1 & police$women == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 2 & police$women == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 3 & police$women == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 4 & police$women == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 5 & police$women == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 6 & police$women == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 7 & police$women == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 8 & police$women == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 9 & police$women == 1], na.rm=TRUE), 2) 
)

trustRegionWomen <- data.frame(regions, trustLevelWomen) 
trustRegionWomen[order(trustRegionWomen$trustLevelWomen), ]

trustLevelEthnic <- c(
	round(mean(police$trustPolice[police$profile_GOR == 1 & police$ethnic == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 2 & police$ethnic == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 3 & police$ethnic == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 4 & police$ethnic == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 5 & police$ethnic == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 6 & police$ethnic == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 7 & police$ethnic == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 8 & police$ethnic == 1], na.rm=TRUE), 2), 
	round(mean(police$trustPolice[police$profile_GOR == 9 & police$ethnic == 1], na.rm=TRUE), 2) 
)

trustRegionEthnic <- data.frame(regions, trustLevelEthnic) 
trustRegionEthnic[order(trustRegionEthnic$trustLevelEthnic), ]

trustAll <- data.frame(regions, trustLevel, trustLevelMen, trustLevelWomen, trustLevelEthnic)
trustAll[order(trustAll$trustLevelWomen, decreasing=TRUE), ]

# Create a time series plot

londonWomen <- c(
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==1  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==1 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==2  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==2 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==3  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==3 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==4  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==4 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==5  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==5 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==6  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==6 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==7  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==7 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==8  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==8 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==9  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==9 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==10 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==10 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==11 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==11 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==12 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==12 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==13 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==13 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==14 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==14 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 1 & police$wave==15 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 1 & police$wave==15 & police$women==1])
	) 
	
londonWomen <- londonWomen * 100

restOfEnglandWomen <- c(
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==1  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==1 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==2  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==2 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==3  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==3 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==4  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==4 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==5  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==5 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==6  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==6 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==7  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==7 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==8  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==8 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==9  & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==9 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==10 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==10 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==11 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==11 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==12 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==12 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==13 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==13 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==14 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==14 & police$women==1]),
	length(police$trustPolice[police$trustPolice > 4 & !is.na(police$trustPolice) & police$london == 0 & police$wave==15 & police$women==1]) / length(police$trustPolice[police$trustPolice & !is.na(police$trustPolice) & police$london == 0 & police$wave==15 & police$women==1])
	) 
	
restOfEnglandWomen <- restOfEnglandWomen * 100

	
month <- c("Jul 2022", "Aug 2022", "Sep 2022", "Oct 2022", "Nov 2022", "Dec 2022", "Jan 2023", "Feb 2023", "Mar 2023", "Apr 2023", "May 2023", "Jun 2023", "Jul 2023", "Aug 2023", "Sep 2023")

trust <- data.frame(month, londonWomen, restOfEnglandWomen) 


# Convert month to date type

trust$month <- as.Date(paste("01 ", trust$month), format="%d %b %Y")

# Define a colourblind-friendly palette
# We'll use the first two colours from Okabe-Ito

okabe_ito_palette <- c("#E69F00", "#56B4E9")

# Create the plot with LOESS smoothing

ggplot(trust, aes(x=month)) +

  geom_smooth(aes(y=londonWomen, color="londonWomen"), method='loess') +

  geom_smooth(aes(y=restOfEnglandWomen, color="restOfEnglandWomen"), method='loess') +
  scale_color_manual(values=okabe_ito_palette,
                     breaks=c("restOfEnglandWomen", "londonWomen"),
                     labels=c("Women not living in London", "Women living in London")) +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  labs(title="Trust in Police (LOESS smoothed)",
       x="Month",
       y="Trust Level (%)",
       color="Population Group") +
  theme_minimal()


# Create models 

m1 <- lm(trustPolice ~ women + profile_gross_personal + age + ethnic + london, data=police)
m2 <- lm(trustPolice ~ women + profile_gross_personal + age + ethnic + london + burglaryDivPop + violentCrimeDivPop, data=police)
m3 <- lm(trustPolice ~ women + profile_gross_personal + age + ethnic + london + burglaryDivPop + violentCrimeDivPop + convote19 + conShare, data=police)
m4 <- lm(trustPolice ~ women + profile_gross_personal + age + ethnic + london + burglaryDivPop + violentCrimeDivPop + convote19 + conShare + womenxlondon, data=police)

stargazer(m1, m2, m3, m4, type="text") 

# Create interaction plot

fit <- lm(trustPolice ~ women * london + profile_gross_personal + age + ethnic + burglaryDivPop + violentCrimeDivPop + convote19 + conShare, data = police)

ggpredict(fit, c("women", "london")) %>% 
  plot() +
  scale_x_continuous(name = "Gender", 
                     breaks = c(0, 1), 
                     labels = c("Male", "Female")) +
  labs(title = "Predicted values of trust in the police", 
       y = "Trust in the police") +
  guides(fill = guide_legend(title = NULL),
         colour = "none") +  # remove old legend for color
  scale_fill_manual(name = "", 
                    values = c("blue", "red"), 
                    labels = c("Lives outside London", "Lives inside London"))

