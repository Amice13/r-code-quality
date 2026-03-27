```
### Installing & Loading Packages

install.packages("lme4")
install.packages("plm")
install.packages("caret")
install.packages("ggrepel")
install.packages("Rcpp", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages("stargazer")
install.packages("interplot")
install.packages("interactions")
install.packages("pals")
install.packages("sjPlot")
install.packages("dotwhisker")
install.packages("glmmTMB")
install.packages("effects")
install.packages("multiwayvcov")
install.packages("xtable")

library(tidyverse)
library(car)
library(haven)
library(dbplyr)
library(ggplot2)
library(interplot)
library(plm)
library(lme4)
library(nlme)
library(stargazer)
library(viridis)
library(caret)
library(ggrepel)
library(purrr)
library(dplyr)
library(interactions)
library(sjPlot)
library(lubridate)
library(dotwhisker)
library(sjlabelled)
library(multiwayvcov)
library(lmtest)
library(xtable)



### Cleaning Data

rawdata <- read.csv("MPDataset (7.5.25).csv", header = TRUE)
attach(rawdata)
r_date <- as.Date(edate_num, origin = "1899-12-30")
dater <-as.data.frame(r_date)
rawdata <- cbind(rawdata, dater)
rawdata$log_gdp_2015constant <- as.numeric(as.character(log_gdp_2015constant))
rawdata$gdp_percapita_2015constant <- as.numeric(as.character(gdp_percapita_2015constant))
rawdata$gdp_growth <- as.numeric(as.character(gdp_growth))
rawdata$lag_party_defscore <- as.numeric(as.character(lag_party_defscore))
rawdata$firstdiff_loggdp_2015constant <- as.numeric(as.character(firstdiff_loggdp_2015constant))
clean_parties <- filter(rawdata, majparty == "YES")
clean_countries <- filter(clean_parties, Region == "Western Europe" & year > 1948)



### - Nationalism & GDP Interactions (H1, H2)
nat_interact <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + midcount + lag_party_defscore + rileadjusted2*net_nat_sent + (1 | countryname), data = clean_countries)
nat_interact2 <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + midcount + lag_party_defscore + rileadjusted2*net_nat_sent + (1 | countryname) + (1 | party), data = clean_countries)

gdp_interact <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + midcount + lag_party_defscore + rileadjusted2*log_gdp_2015constant + (1 | countryname), data = clean_countries)
gdp_interact2 <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + midcount + lag_party_defscore + rileadjusted2*log_gdp_2015constant + (1 | countryname) + (1 | party), data = clean_countries)

i <- interplot(nat_interact,var1 = "rileadjusted2",var2 = "net_nat_sent") + xlab("Party Nationalism") + ylab("Party L-R Position") 
i + ggtitle("Coefficients for Party L-R Position, Conditional on Nationalism") + theme(plot.title = element_text(hjust = 0.5))
i2 <- interplot(nat_interact,var1 = "net_nat_sent",var2 = "rileadjusted2") + xlab("Party L-R Position") + ylab("Party Nationalism")
i2 + ggtitle("Coefficients for Nationalism, Conditional on Party L-R Position") + theme(plot.title = element_text(hjust = 0.5))

rug <- ggplot(data = clean_countries, aes(x = net_nat_sent, rileadjusted2)) + 
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, size=1.5)
rug + xlab("Party Nationalism") + ylab("Party L-R Position") + ggtitle("Distribution of Values for Nationalism and L-R Position") +
  theme(plot.title = element_text(hjust = 0.5))

rug2 <- ggplot(data = clean_countries, aes(x = rileadjusted2, net_nat_sent)) + 
  geom_point() +
  geom_rug(col="darksalmon",alpha=0.1, size=1.5)
rug2 + xlab("Party L-R Position") + ylab("Party Nationalism") + ggtitle("Distribution of Values for Nationalism and L-R Position") + 
  theme(plot.title = element_text(hjust = 0.5))



### - Robustness Checks & Results, H1 & H2
# straight versions drop the DV lag; all_interact includes both interaction terms

nat_interact_straight <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + midcount + rileadjusted2*net_nat_sent + (1 | countryname), data = clean_countries)
gdp_interact_straight <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + midcount + rileadjusted2*log_gdp_2015constant + (1 | countryname), data = clean_countries)

all_interact <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + midcount + lag_party_defscore + rileadjusted2*net_nat_sent + rileadjusted2*log_gdp_2015constant + (1 | countryname), data = clean_countries)
stargazer(nat_interact, nat_interact2, gdp_interact, gdp_interact2, type = "latex")
stargazer(all_interact, type = "text")
stargazer(nat_interact_straight, type = "text")
stargazer(gdp_interact_straight, type = "text")

# GDP Alternative Specifications (H2)
gdp_percap <- lmer(net_party_defscore ~ cold_war + gdp_percapita_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + rileadjusted2*gdp_percapita_2015constant + (1 | countryname) + (1 | party), data = clean_countries)
gdp_grow <- lmer(net_party_defscore ~ cold_war + gdp_growth + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + rileadjusted2*gdp_growth + (1 | countryname) + (1 | party), data = clean_countries)
gdp_debt <- lmer(net_party_defscore ~ cold_war + debt_gdp + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + rileadjusted2*debt_gdp + (1 | countryname) + (1 | party), data = clean_countries)

# Welfare Score Alternative Specifications (H2)
clean_countries$welfare <- clean_countries$per504 - clean_countries$per505

welfare_score <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + welfare + net_nat_sent + lag_party_defscore + midcount + welfare*log_gdp_2015constant + (1 | countryname) + (1 | party), data = clean_countries)

# Alternate Specifications - Results Table
stargazer(gdp_interact2, gdp_percap, gdp_grow, gdp_debt, welfare_score, style = "ajps", type = "latex")



### - Clustered Standard Errors, H1 & H2
nat_interact_clst <- lm(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + midcount + lag_party_defscore + rileadjusted2*net_nat_sent, data = clean_countries)

# Cluster by Country
vcov_country <- cluster.vcov(nat_interact_clst, clean_countries$countryname)
a <- coeftest(nat_interact_clst, vcov_country)

# Cluster by Year
vcov_year <- cluster.vcov(nat_interact_clst, clean_countries$year)
b <- coeftest(nat_interact_clst, vcov_year)

# Cluster by Party
vcov_party <- cluster.vcov(nat_interact_clst, clean_countries$name.id_concat)
c <- coeftest(nat_interact_clst, vcov_party)

# Double Cluster by Country and Year
vcov_both <- cluster.vcov(nat_interact_clst, cbind(clean_countries$countryname, clean_countries$year))
d <- coeftest(nat_interact_clst, vcov_both)

# Double Cluster by Party and Year
vcov_both2 <- cluster.vcov(nat_interact_clst, cbind(clean_countries$name.id_concat, clean_countries$year))
e <- coeftest(nat_interact_clst, vcov_both2)

ses <- list(a[,2], b[,2], c[,2], d[,2], e[,2])
ps <- list(a[,4], b[,4], c[,4], d[,4], e[,4])

stargazer(nat_interact, a, b, c, d, e, style = "ajps", type = "latex")
stargazer(nat_interact_clst, style = "ajps", type = "text")



### - Cold War Hypothesis Test (H3)

party_interact_soc <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + SOC + cold_war*SOC + (1 | countryname) + (1 | party), data = clean_countries)
party_interact_lef <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + LEF + cold_war*LEF + (1 | countryname) + (1 | party), data = clean_countries)
party_interact_allfam1 <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + party_categorical + cold_war*party_categorical + (1 | countryname) + (1 | party), data = clean_countries)

stargazer(party_interact_soc, party_interact_lef, party_interact_allfam1, style = "ajps", type = "latex", 
          omit = c("party_categorical3", "party_categorical4", "party_categorical5", "party_categorical6", "party_categorical7", "party_categorical8", "party_categorical9")
)



### - H3 Without L-R Measure
party_interact_soc2 <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + net_nat_sent + lag_party_defscore + midcount + SOC + cold_war*SOC + (1 | countryname) + (1 | party), data = clean_countries)
party_interact_lef2 <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + net_nat_sent + lag_party_defscore + midcount + LEF + cold_war*LEF + (1 | countryname) + (1 | party), data = clean_countries)
party_interact_allfam2 <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + net_nat_sent + lag_party_defscore + midcount + party_categorical + cold_war*party_categorical + (1 | countryname) + (1 | party), data = clean_countries)

stargazer(party_interact_soc2, party_interact_lef2, party_interact_allfam2, style = "ajps", type = "latex", 
          omit = c("party_categorical3", "party_categorical4", "party_categorical5", "party_categorical6", "party_categorical7", "party_categorical8", "party_categorical9")
)



### - Clustered Standard Errors (H3)
party_interact_soc_clst <- lm(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + SOC + cold_war*SOC, data = clean_countries)
party_interact_lef_clst <- lm(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + LEF + cold_war*LEF, data = clean_countries)
party_interact_allfam1_clst <- lm(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + party_categorical + cold_war*party_categorical, data = clean_countries)

# Cluster by Country
vcov_soc <- cluster.vcov(party_interact_soc_clst, clean_countries$countryname)
cluster_soc <- coeftest(party_interact_soc_clst, vcov_soc)

vcov_lef <- cluster.vcov(party_interact_lef_clst, clean_countries$countryname)
cluster_lef <- coeftest(party_interact_lef_clst, vcov_lef)

vcov_allparty <- cluster.vcov(party_interact_allfam1_clst, clean_countries$countryname)
cluster_all <- coeftest(party_interact_allfam1_clst, vcov_allparty)

# Cluster by Year
vcov_soc2 <- cluster.vcov(party_interact_soc_clst, clean_countries$year)
coeftest(party_interact_soc_clst, vcov_soc2)

vcov_lef2 <- cluster.vcov(party_interact_lef_clst, clean_countries$year)
coeftest(party_interact_lef_clst, vcov_lef2)

vcov_allparty2 <- cluster.vcov(party_interact_allfam1_clst, clean_countries$year)
coeftest(party_interact_allfam1_clst, vcov_allparty2)

# Cluster by Party
vcov_soc3 <- cluster.vcov(party_interact_soc_clst, clean_countries$name.id_concat)
coeftest(party_interact_soc_clst, vcov_soc3)

vcov_lef3 <- cluster.vcov(party_interact_lef_clst, clean_countries$name.id_concat)
coeftest(party_interact_lef_clst, vcov_lef3)

vcov_allparty3 <- cluster.vcov(party_interact_allfam1_clst, clean_countries$name.id_concat)
coeftest(party_interact_allfam1_clst, vcov_allparty3)

# Double Cluster by Country and Year
vcov_socboth <- cluster.vcov(party_interact_soc_clst, cbind(clean_countries$countryname, clean_countries$year))
coeftest(party_interact_soc_clst, vcov_socboth)

vcov_lefboth <- cluster.vcov(party_interact_lef_clst, cbind(clean_countries$countryname, clean_countries$year))
coeftest(party_interact_lef_clst, vcov_lefboth)

vcov_allpartyboth <- cluster.vcov(party_interact_allfam1_clst, cbind(clean_countries$countryname, clean_countries$year))
coeftest(party_interact_allfam1_clst, vcov_allpartyboth)

# Double Cluster by Party and Year
vcov_socboth2 <- cluster.vcov(party_interact_soc_clst, cbind(clean_countries$name.id_concat, clean_countries$year))
coeftest(party_interact_soc_clst, vcov_socboth2)

vcov_lefboth2 <- cluster.vcov(party_interact_lef_clst, cbind(clean_countries$name.id_concat, clean_countries$year))
coeftest(party_interact_lef_clst, vcov_lefboth2)

vcov_allpartyboth2 <- cluster.vcov(party_interact_allfam1_clst, cbind(clean_countries$name.id_concat, clean_countries$year))
coeftest(party_interact_allfam1_clst, vcov_allpartyboth2)



### - Appendix C, Marginal Effects Plot for Before/After Cold War (H3)

test_all <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + party_categorical + cold_war*party_categorical + rileadjusted2*net_nat_sent + rileadjusted2*log_gdp_2015constant + (1 | countryname) + (1 | party), data = clean_countries)

stargazer(party_interact_soc, cluster_soc, party_interact_lef, cluster_lef, party_interact_allfam1, cluster_all, test_all, style = "ajps", type = "latex")

dvpredict <- plot_model(party_interact_allfam1, type = "pred", terms = c("party_categorical", "cold_war"))
dvpredict + 
  ggtitle("Predicted Positioning on Military & Defense Spending \n During & After the Cold War") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Party Family") + ylab("Predicted Position on Military & Defense Spending") + 
  labs(color = "Cold War Status") + 
  scale_color_discrete(labels = c("During", "After")) + labs(caption = "0-LIB 1-SOC 2-LEF 3-CON 4-CHR 5-AGR 6-GRN 7-SIP 8-NAT 9-ETH")



### - Right-Left & Cold War Interact (H3)
rile_cw <- lmer(net_party_defscore ~ cold_war + log_gdp_2015constant + rileadjusted2 + net_nat_sent + lag_party_defscore + midcount + cold_war*rileadjusted2 + (1 | countryname) + (1 | party), data = clean_countries)

stargazer(rile_cw, style = "ajps", type = "latex")

lrinteract <- interplot(rile_cw, var1 = "cold_war", var2 = "rileadjusted2") + 
  xlab("Left-Right Positioning") + ylab("Cold War Coefficient")
lrinteract + ggtitle("Cold War Dummy Coefficient Conditional on Party L-R Position") + 
  theme(plot.title = element_text(hjust = 0.5))

interplot(rile_cw, var1 = "rileadjusted2", var2 = "cold_war") + 
  xlab("Cold War") + ylab("Right-Left Positioning")

rug3 <- ggplot(data = clean_countries, aes(x = rileadjusted2, cold_war)) + 
  geom_point() +
  geom_rug(col="aquamarine3",alpha=0.1, size=1.5)
rug3 + xlab("Left-Right Positioning") + ylab("Cold War Dummy") + ggtitle("Distribution of Values for L-R Position, During & After the Cold War") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=seq(-80,80,20))



### - GDP Unit Root Check
df_test <- data.frame(read.csv("GDP_testdata.csv", header = TRUE))
df_test <- filter(df_test, Country == "Sweden" | Country == "Norway" | Country == "Denmark" | Country == "Finland" | 
                    Country == "Iceland" | Country == "Belgium" | Country == "Netherlands" | Country == "Luxembourg" | 
                    Country == "France" | Country == "Italy" | Country == "Spain" | Country == "Greece" | Country == "Portugal" | Country == "Germany" | 
                    Country == "Austria" | Country == "Switzerland" | Country == "United Kingdom" | Country == "Ireland" | Country == "Malta" | Country == "Cyprus")
view(df_test)
df_test$GDP_Log <- as.numeric(df_test$GDP_Log)
df_test <- subset(df_test, select = -c(GDP_2015constantUSD))
pdata <- pdata.frame(df_test, index = c("Country", "Year"))
ips_test <- purtest(pdata$GDP_Log, test = "ips", exo = "intercept", lags = "AIC", pmax = 5)
summary(ips_test)



### - Backup/Supporting Charts | Descriptive
p <- ggplot(data = clean_countries, aes(rileadjusted, net_party_defscore)) + geom_point(na.rm = TRUE) + geom_smooth(method = "loess")
p + labs(x="Party L+R Ideological Positioning", y="Support for Military & Defense Spending", title = "European Party Support for Military & Defense Spending, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))

n <- ggplot(data = clean_countries, aes(rileadjusted2, net_nat_sent)) + geom_point(na.rm = TRUE) + geom_smooth(method = "loess")
n + labs(x="Party L+R Ideological Positioning", y="Nationalism", title = "European Party Nationalism & L-R Positioning, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))


selectcountries <- c("Sweden", "Norway", "Denmark", "Finland", "Netherlands", "France", "Italy", "Germany", "United Kingdom", "Switzerland", "Portugal", "Spain")
filtered_countries <- clean_countries %>%
  filter(countryname %in% selectcountries)

parfamselect <- c("LEF", "SOC")
filtered_parties <- filtered_countries %>%
  filter(parfam_colors %in% parfamselect)

leftparties <- c("LEF")
parties_left <- clean_countries %>%
  filter(parfam_colors %in% leftparties)

socialists <- c("SOC")
parties_soc <- clean_countries %>%
  filter(parfam_colors %in% socialists)

ggplot(filtered_countries, aes(x = r_date, y = net_party_defscore, color = partyname)) + 
  geom_line() + 
  theme(legend.position="none") + 
  geom_vline(xintercept = as.numeric(as.Date("1991-12-26")), linetype = 1, color = "red") + 
  facet_wrap(~countryname)

ggplot(filtered_parties, aes(x = r_date, y = net_party_defscore, color = partyname)) + 
  geom_line() + 
  geom_vline(xintercept = as.numeric(as.Date("1991-12-26")), linetype = 1, color = "red") +
  theme(legend.position="none") + 
  facet_wrap(~countryname) + 
  labs(x="Year", y="Support for Military & Defense Spending", title = "Left & Socialist Party Support for Military & Defense Spending, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))

ggplot(parties_left, aes(x = r_date, y = net_party_defscore, color = partyname)) + 
  geom_line() + 
  theme(legend.position="none") + 
  geom_vline(xintercept = as.numeric(as.Date("1991-12-26")), linetype = 1, color = "red") +
  labs(x="Year", y="Support for Military & Defense Spending", title = "Left Party Support for Military & Defense Spending, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))

ggplot(parties_soc, aes(x = r_date, y = net_party_defscore, color = partyname)) + 
  geom_line() + 
  theme(legend.position="none") + 
  geom_vline(xintercept = as.numeric(as.Date("1991-12-26")), linetype = 1, color = "red") + 
  labs(x="Year", y="Support for Military & Defense Spending", title = "Socialist Party Support for Military & Defense Spending, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))


Total Defense Salience (sums per104 & per105)
ggplot(parties_left, aes(x = r_date, y = total_def_salience, color = partyname)) + 
  geom_line() + 
  theme(legend.position="none") + 
  geom_vline(xintercept = as.numeric(as.Date("1991-12-26")), linetype = 1, color = "red") +
  labs(x="Year", y="Total Mentions of Military & Defense Spending", title = "Left Party Mentions of Military & Defense Spending, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))

ggplot(parties_soc, aes(x = r_date, y = total_def_salience, color = partyname)) + 
  geom_line() + 
  theme(legend.position="none") + 
  geom_vline(xintercept = as.numeric(as.Date("1991-12-26")), linetype = 1, color = "red") + 
  labs(x="Year", y="Total Mentions of Military & Defense Spending", title = "Socialist Party Mentions of Military & Defense Spending, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))


Total Defense Salience by Party Family (sums per104 & per105)
ggplot(clean_countries, aes(x = parfam_colors, y = total_def_salience)) + 
  geom_bar(stat = "identity") + 
  theme(legend.position="none") + 
  labs(x="Year", y="Total Mentions of Military & Defense Spending", title = "Mentions of Military & Defense Spending by Party Family, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))

Total Defense Salience by Party Family (sums per104 & per105)
ggplot(clean_countries, aes(x = r_date, y = total_def_salience, color = parfam_colors)) + 
  geom_point() + 
  labs(color = "Party Family", x="Year", y="Total Mentions of Military & Defense Spending", title = "Mentions of Military & Defense Spending, 1949-2019") + theme(plot.title = element_text(hjust = 0.5))



### - 10+ Observations
timeseries_include variable isolates leftist and socialist parties with 10 or more observations during the time series

leftselector <- c("YES")
left_series <- clean_countries %>%
  filter(timeseries_include %in% leftselector)

View(left_series)

ggplot(left_series, aes(x = r_date, y = net_party_defscore, color = partyname)) + 
  geom_line() + 
  theme(legend.position="none") + 
  geom_vline(xintercept = as.numeric(as.Date("1991-12-26")), linetype = 1, color = "red") + 
  labs(x="Year", y="Support for Military & Defense Spending", title = "Left & Socialist Support - Major Parties Only") + theme(plot.title = element_text(hjust = 0.5))

ggplot(left_series, aes(x = r_date, y = total_def_salience, color = partyname)) + 
  geom_line() + 
  theme(legend.position="none") + 
  geom_vline(xintercept = as.numeric(as.Date("1991-12-26")), linetype = 1, color = "red") + 
  labs(x="Year", y="Total Mentions of Military & Defense Spending", title = "Left & Socialist Mentions - Major Parties Only") + theme(plot.title = element_text(hjust = 0.5))

