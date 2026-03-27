##### The following script replicates all the tables and figures in the main paper 

################################################################################
########################## Part I: Introduction  ###############################
################################################################################

rm(list=ls())
library(dplyr)
library(tidyr)
library(foreign)
library(haven)
library(readr)
library(readxl)
library(haven)
library(ggplot2)
library(ggrepel)
library(WDI)
library(countrycode)
library(data.table)
library(foreign)
library(stringr)
library(grid)
library(gridExtra)
library(ggpubr)
library(timeDate)
library(openxlsx)
library(tidyverse)
library(ordinal)
library(MASS)
library(ggeffects)
library(effects)
library(marginaleffects)
library(haven)
#library(fwildclusterboot)
library(boot)
library(margins)
library(lmtest)
library(stargazer)
library(DescTools)
library(brant)
library(nnet)
library(lmtest)
library(sandwich)
library(patchwork)

# Adjust this path variable to your local directory as needed
# setwd("")

# Load dataset
df <- read.csv("D02_currencyCompetition-collapsed-v1.csv")

# Select variables of interest
df <- dplyr::select(df, iso, country, monthly_date, monthlyDate, year, month, 
                    fxi_spot_pub_gdp_m, fxi_spot_proxy_gdp_m, fxi_broad_proxy_gdp_m,
                    interventionAmount, lnInterventionAmount, gdp_usd, lnGDP, gdp_pc,lnGDP_pc )

# Period of interest 
df <- dplyr::filter(df, year > 1999)

df <- df %>%
  mutate(intervention_gdp_m = coalesce(fxi_spot_pub_gdp_m, fxi_spot_proxy_gdp_m, fxi_broad_proxy_gdp_m))

df <- dplyr::select(df, -fxi_spot_pub_gdp_m, -fxi_spot_proxy_gdp_m, -fxi_broad_proxy_gdp_m)

#### Prepare for descriptive graphs

df2 <- mutate(df, intervention_bin = ifelse(interventionAmount != 0, 1, 0))
df2 <- mutate(df2, intervention_bin_devaluat = ifelse(intervention_bin == 1 & interventionAmount > 0, 1, 0))

# Calculate the the three quantitites to be plotted (percentage of months with depreciation inducing intervention / 
# average monthly amount of depreciation inducing intervention / average log GDP)
df2 <- df2 %>% filter(intervention_bin == 1) %>%
  group_by(country) %>% 
  summarise(
    intervention_bin_devaluat = mean(intervention_bin_devaluat, na.rm = T),
    ln_gdp = mean(lnGDP, na.rm = T),
    intervention_amount_devaluat = mean(lnInterventionAmount, na.rm = T))

# Cleaning in preparation to graph the results
df2[df2 == "NaN"] <- NA
df2 <- filter(df2, !is.na(ln_gdp))

# Highlightinh Asian countries
interesting <- c("Japan", "South Korea", "China", "Taiwan",
                 "Thailand", "Philippines", "Malaysia", "Singapore", "Indonesia")


################################################################################
######################################### Figure 1 #############################
################################################################################


g1 <- df2 %>%
  mutate(country = replace(country, !country %in% interesting, "")) %>%
  ggplot(., aes(ln_gdp, intervention_bin_devaluat)) +
  geom_point(data = filter(df2, !(country %in% interesting)), 
             aes(color = (country %in% interesting)), size = 3) +
  geom_point(data = filter(df2, country %in% interesting), 
             aes(color = (country %in% interesting), shape = (country %in% interesting)), size = 3) +
  scale_color_manual(guide = FALSE, values = c("FALSE" = "grey", "TRUE" = "black")) +
  scale_shape_manual(guide = FALSE, values = c("FALSE" = 16, "TRUE" = 17)) + # 16 for circle, 17 for triangle
  ggrepel::geom_text_repel(
    data = filter(df2, country %in% interesting), 
    aes(label = country), color = "black",
    nudge_x = 0.5, direction = "y", hjust = 2.8
  ) +
  labs(x = "GDP (Log)", y = "% Months Foreign Intervention (buy foreign, sell local)") +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 13) # Increase the Y axis label size
  )  

g1
ggsave("Fig1_frequency_MOTIVATION.pdf", plot = g1, width = 7, height = 5, units = "in")




################################################################################
########################## Part II: Japan  #####################################
################################################################################

rm(list=ls())
getwd()

# Load intervention data for Japan (official data from BoJ)
forex_int_jpn <- read.csv("JPNForexInterventions1989-2016.csv")
# Loading, cleaning, and merging the data for Figure 2
jap_er <- read.csv("DEXJPUS.csv")

# Clean
jap_er[jap_er == "."] <- NA
sum(sapply(jap_er$DEXJPUS, function(x) sum(length(which(is.na(x))))) ) 
jap_er$DATE <- as.Date(jap_er$DATE, format = '%Y-%m-%d')
jap_er <- dplyr::rename(jap_er,date =  DATE, er_j = DEXJPUS)
class(jap_er$er_j)
jap_er$er_j <- as.numeric(jap_er$er_j)

# Adding official closure date at NY Exchange for later
closing_days_ny <- holidayNYSE(1971:2023)
closing_days_ny <- closing_days_ny@Data
closing_days_ny <- as.data.frame(closing_days_ny)
closing_days_ny$date <- as.Date(closing_days_ny$closing_days_ny)
class(closing_days_ny$date)
closing_days_ny$date <- as.Date(closing_days_ny$date, format="%Y-%m-%d")
jap_er <- dplyr::left_join(jap_er, closing_days_ny)
jap_er2 <- dplyr::filter(jap_er, !(is.na(er_j) & !is.na(closing_days_ny)))
sum(sapply(jap_er2$er_j, function(x) sum(length(which(is.na(x))))) ) 

# Goal: fill in the NAs with the ER in the Tokyo Market.
# For the period since 1999, we use data from BoJ 
tokyo_boj <- read.csv("yen_dollarER_tokyo_BOJ.csv", skip = 2)
tokyo_boj <- dplyr::rename(tokyo_boj, date = "Name.of.time.series", er_j_tok_9am = "US.Dollar.Yen.Spot.Rate.at.9.00.in.JST..Tokyo.Market", er_j_tok_5pm ="US.Dollar.Yen.Spot.Rate.at.17.00.in.JST..Tokyo.Market")
class(tokyo_boj$date)
tokyo_boj$date <- as.Date(tokyo_boj$date, format="%Y/%m/%d")

# For the period pre 1999, we rely on data from Ito abd Yabu (2007) 
# Notice that the below code will work only up to the last day of 2002 because the original xls file
# format changes after that (see date column). This is NOT a problem since we use the official BoJ data after 1999 anyway
# Something to keep in mind, though, if one re-uses the function for different time frames.

tokyo_boj2 <- read_excel("JapanIntervention1991-2004.xls", 
                         sheet = "YenDollar", skip = 2)
tokyo_boj2 <- dplyr::rename(tokyo_boj2, date = "...1", er_j_tok_5pm = "５PM...3", er_j_tok_9am ="Central")
tokyo_boj2 <- dplyr::select(tokyo_boj2, -"５PM...2", -"Buy YEN, Sell DOL (100mil yen)", -"Buy YEN, Sell DOL (1mil $)")
tokyo_boj2$date <- as.Date(tokyo_boj2$date, format="%m/%d/%Y")
tokyo_boj2 <- dplyr::filter(tokyo_boj2, !is.na(date))

# Keep the right time frame for each of the sources
tokyo_boj <- tokyo_boj[9502:17167, ]
tokyo_boj2 <- tokyo_boj2[1:1751, ]

# Merge the two Tokyo ER datasets
tokyo_boj3 <- rbind(tokyo_boj, tokyo_boj2)
rm(tokyo_boj)
rm(tokyo_boj2)
rm(closing_days_ny)
rm(jap_er2)
jap_er <- left_join(jap_er, tokyo_boj3)
rm(tokyo_boj3)

# Fill in the Tokyo ER if it is present while the NY ER is absent 
jap_er <- mutate(jap_er, er_j = ifelse(is.na(er_j), er_j_tok_5pm, er_j))
jap_er <- dplyr::select(jap_er, -er_j_tok_9am, -er_j_tok_5pm)

# This chunck will break down the original format of closing_days_ny (it coerces the variable into numeric), but it does not matter.
# Whenever it is not missing (regardless of whether it is filled with charcaters or digits), it means that the NYE was closed.
jap_er <- mutate(jap_er, closing_days_ny = ifelse(is.na(er_j) & !is.na(closing_days_ny), closing_days_ny, NA))

# Exclude the days where nye was closed and where there is no tokyo ER
jap_er <- dplyr::filter(jap_er, is.na(closing_days_ny))

jap_er <- dplyr::select(jap_er, -closing_days_ny)
str(jap_er)

# restrict sample to 1990-2020
jap_er <- dplyr::filter(jap_er, date > "1990-01-01" & date < "2021-01-04")


# Keep only raw intervention amount to construct the binary and ordinal DV later
forex_int_jpn <- dplyr::select(forex_int_jpn, date, intervAmount)

# Clean the date
forex_int_jpn$day <- substr(forex_int_jpn$date, start = 1, stop = 2)
forex_int_jpn$month <- substr(forex_int_jpn$date, start = 3, stop = 5)
forex_int_jpn$year <- substr(forex_int_jpn$date, start = 6, stop = 9)
forex_int_jpn$day <- as.numeric(forex_int_jpn$day)
forex_int_jpn$year <- as.numeric(forex_int_jpn$year)
forex_int_jpn$month <- str_to_title(forex_int_jpn$month)
forex_int_jpn$month <- match(forex_int_jpn$month,month.abb)
forex_int_jpn$date<-as.Date(with(forex_int_jpn,paste(year,month,day,sep="-")),"%Y-%m-%d")
forex_int_jpn <- dplyr::select(forex_int_jpn, date, intervAmount)
head(forex_int_jpn)

# Merge (I left join so that it does up to 2020. There is no boj intervention in 2015-2020)
final <- left_join(jap_er, forex_int_jpn)
final <- mutate(final, intervAmount = ifelse(is.na(intervAmount), 0, intervAmount))
final <- na.omit(final)


theme_set(theme_bw())

# Set the scaling factor
scale <- 900

################################################################################
################################# Figure 3 #####################################
################################################################################


graph <- ggplot(final, aes(x = date, y = er_j)) +
  geom_line() +
  geom_line(aes(y = intervAmount / scale), color = "gray10", size = 0.2) +
  scale_y_continuous(sec.axis = sec_axis(~ . * scale, name = "Intervention Amount")) +
  labs(x = "Year", y = "Yen/USD", color = "") +
  scale_color_manual(values = c("black", "gray10")) +
  theme(
    axis.title.y = element_text(size = 14), # Make Y-axis label bigger
    axis.title.y.right = element_text(size = 14) # Make right Y-axis label bigger as well
  )

print(graph)
ggsave("Fig2_yen_USD.pdf", plot = graph, width = 8, height = 5, units = "in")


