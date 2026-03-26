# List of required packages
required_packages <- c(
  "foreign", "readxl", "tidyr", "dplyr", "zoo", "countrycode", "ggplot2",
  "ggeffects", "haven", "gridExtra", "estimatr", "texreg", "mgcv",
  "stringr", "rlang", "cowplot"
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

# Install missing packages
install_if_missing(required_packages)

# Load all packages
lapply(required_packages, library, character.only = TRUE)

# Print message indicating that all packages are loaded
print("All required packages are installed and loaded.")

rm(list=ls())

# Set working directory to the "Replication Files" folder -- enter file path below
setwd("[SET WORKING DIRECTORY HERE]")

# DATA PREP --------------

# Set working directory to data
setwd(file.path(".","Data"))

## Preparing V-Dem ---------------

# Loading V-Dem data
vdem <- readRDS("V-Dem-CY-Full+Others-v13.rds")

# Lagging electoral democracy variable
vdem <- vdem %>%
  group_by(country_id) %>%
  mutate(v2x_polyarchy_lag = lag(v2x_polyarchy))

# Leading electoral democracy variable
vdem <- vdem %>%
  group_by(country_id) %>%
  mutate(v2x_polyarchy_next = lead(v2x_polyarchy)) %>%
  mutate(v2x_polyarchy_next2 = lead(v2x_polyarchy_next)) %>%
  mutate(v2x_polyarchy_next3 = lead(v2x_polyarchy_next2)) %>%
  mutate(v2x_polyarchy_next4 = lead(v2x_polyarchy_next3)) %>%
  mutate(v2x_polyarchy_next5 = lead(v2x_polyarchy_next4)) %>%
  mutate(v2x_polyarchy_next6 = lead(v2x_polyarchy_next5)) %>%
  mutate(v2x_polyarchy_next7 = lead(v2x_polyarchy_next6))

vdem$vdem_change <- vdem$v2x_polyarchy_next - vdem$v2x_polyarchy_lag
vdem$vdem_change_2yr <- vdem$v2x_polyarchy_next2 - vdem$v2x_polyarchy_lag
vdem$vdem_change_3yr <- vdem$v2x_polyarchy_next3 - vdem$v2x_polyarchy_lag
vdem$vdem_change_4yr <- vdem$v2x_polyarchy_next4 - vdem$v2x_polyarchy_lag
vdem$vdem_change_5yr <- vdem$v2x_polyarchy_next5 - vdem$v2x_polyarchy_lag
vdem$vdem_change_6yr <- vdem$v2x_polyarchy_next6 - vdem$v2x_polyarchy_lag
vdem$vdem_change_7yr <- vdem$v2x_polyarchy_next7 - vdem$v2x_polyarchy_lag

# Lagging pro-democracy mobilization
vdem <- vdem %>%
  group_by(country_id) %>%
  mutate(v2cademmob_lag = lag(v2cademmob))

# Opposition Harassment + Free & Fair Elections + Irregularities

# Remove variables where there is no election year - in order to properly lag/lead variables in election years
# Limiting to election years
vdem$v2elintim <- ifelse(!is.na(vdem$v2eltype_0) & (vdem$v2eltype_0==1 | vdem$v2eltype_1==1 | vdem$v2eltype_2==1 | vdem$v2eltype_3==1 | vdem$v2eltype_6==1 | vdem$v2eltype_7==1), vdem$v2elintim, NA)

vdem$v2elirreg <- ifelse(!is.na(vdem$v2eltype_0) & (vdem$v2eltype_0==1 | vdem$v2eltype_1==1 | vdem$v2eltype_2==1 | vdem$v2eltype_3==1 | vdem$v2eltype_6==1 | vdem$v2eltype_7==1), vdem$v2elirreg, NA)

vdem$v2elfrfair <- ifelse(!is.na(vdem$v2eltype_0) & (vdem$v2eltype_0==1 | vdem$v2eltype_1==1 | vdem$v2eltype_2==1 | vdem$v2eltype_3==1 | vdem$v2eltype_6==1 | vdem$v2eltype_7==1), vdem$v2elfrfair, NA)

# Carry forward all variables to fill non-election years
vdem$v2elintim2 <- na.locf(vdem$v2elintim, na.rm = F, fromLast = F)
vdem$v2elfrfair2 <- na.locf(vdem$v2elfrfair, na.rm = F, fromLast = F)
vdem$v2elirreg2 <- na.locf(vdem$v2elirreg, na.rm = F, fromLast = F)

# Carry backward all variables to fill non-election years
vdem$v2elintim <- na.locf(vdem$v2elintim, na.rm = F, fromLast = T)
vdem$v2elfrfair <- na.locf(vdem$v2elfrfair, na.rm = F, fromLast = T)
vdem$v2elirreg <- na.locf(vdem$v2elirreg, na.rm = F, fromLast = T)

vdem <- vdem %>%
  group_by(country_id) %>%
  mutate(v2elintim_lag = lag(v2elintim2)) %>%
  mutate(v2elfrfair_lag = lag(v2elfrfair2)) %>%
  mutate(v2elintim_lead = lead(v2elintim)) %>%
  mutate(v2elfrfair_lead = lead(v2elfrfair)) %>%
  mutate(v2elirreg_lag = lag(v2elirreg2)) %>%
  mutate(v2elirreg_lead = lead(v2elirreg))

# Change variables (negative equals change for the worse)
vdem$intim_change <- vdem$v2elintim_lead - vdem$v2elintim 
vdem$freefair_change <- vdem$v2elfrfair_lead - vdem$v2elfrfair
vdem$irreg_change <- vdem$v2elirreg_lead - vdem$v2elirreg

# Create ID variable (country-year)
vdem$country_year <- paste(vdem$country_name, vdem$year, sep = "_")

# Renaming country code variable for merging
vdem$ccode <- vdem$COWcode

# Adjusting Kazakhstan 2022 election (missing election information in V-Dem)
vdem$v2eltype_0 <- ifelse(vdem$country_year=="Kazakhstan_2022", 0, vdem$v2eltype_0)
vdem$v2eltype_6 <- ifelse(vdem$country_year=="Kazakhstan_2022", 1, vdem$v2eltype_6)

# Extending GDP per capita data forward (generates rough estimates between 2020 and 2022 so that data do not drop out during those years)
vdem$e_gdppc <- na.locf(vdem$e_gdppc, na.rm = F, fromLast = F)

## Merging with Fariss Data ---------
# Loading Fariss data
fariss <- readRDS("HumanRightsProtectionScores_v4.01.rds")

# Renaming variables
fariss <- rename(fariss, COWcode = COW)
fariss <- rename(fariss, year = YEAR)
fariss <- rename(fariss, latentmean = theta_mean)
# Deleting country_name variable to facilitate merge
fariss <- subset(fariss, select = -c(country_name))
# Merging with V-Dem
vdem <- merge(vdem, fariss, by=c("COWcode", "year"), all.x = TRUE)

# Getting future measures of repression
vdem <- vdem %>%
  group_by(country_id) %>%
  mutate(latentmean_lag = lag(latentmean)) %>%
  mutate(latentmean_next = lead(latentmean)) %>%
  mutate(latentmean_next2 = lead(latentmean_next)) %>%
  mutate(latentmean_next3 = lead(latentmean_next2)) %>%
  mutate(latentmean_next4 = lead(latentmean_next3)) %>%
  mutate(latentmean_next5 = lead(latentmean_next4)) %>%
  mutate(latentmean_next6 = lead(latentmean_next5)) %>%
  mutate(latentmean_next7 = lead(latentmean_next6))

vdem$latentmean_change_ey <- vdem$latentmean - vdem$latentmean_lag
vdem$latentmean_change <- vdem$latentmean_next - vdem$latentmean_lag
vdem$latentmean_change_2yr <- vdem$latentmean_next2 - vdem$latentmean_lag
vdem$latentmean_change_3yr <- vdem$latentmean_next3 - vdem$latentmean_lag
vdem$latentmean_change_4yr <- vdem$latentmean_next4 - vdem$latentmean_lag
vdem$latentmean_change_5yr <- vdem$latentmean_next5 - vdem$latentmean_lag
vdem$latentmean_change_6yr <- vdem$latentmean_next6 - vdem$latentmean_lag
vdem$latentmean_change_7yr <- vdem$latentmean_next7 - vdem$latentmean_lag

## Merging with Economic Growth Data ------------

# Loading economic growth data (World Bank + IMF)
econ <- read.csv("econgrowth.csv")

# Reshape the data from wide to long format
econ_long <- econ %>%
  pivot_longer(cols = -c(Country.Name,Country.Code,Indicator.Name,Indicator.Code),
               names_to = "year",
               values_to = "econ_growth")

# Remove the "X" at the beginning of each year value
econ_long <- econ_long %>%
  mutate(year = substring(year, 2))

# Updating country codes for merge
econ_long$COWcode <- countrycode(econ_long$Country.Code, "wb", "cown")

# Updating country code for Serbia (country code missing from economic data)
econ_long$COWcode <- ifelse(econ_long$Country.Name=="Serbia", 345, econ_long$COWcode)

# Merging with V-Dem
vdem <- merge(vdem, econ_long, by=c("COWcode", "year"))

## Limiting Sample -------------
### Basic Criteria from V-Dem -------------

# The specifications below limit to the widest possible sample, which is a 5-year threshold for continued individual/party rule and a V-Dem cut-off of 0.55. I further limit the sample further down with manual checks and updates, and to other specifications, including the main testing specifications (10-year threshold, 0.5 V-Dem cutoff).

# 1) Ruled by the same individual or party for at least 5 years
# Addressed through manual checks -- see section below

# 2) Autocratic: falling below V-Dem electoral democracy threshold of 0.55 (lagged one year)
vdem <- vdem[vdem$v2x_polyarchy_lag<=0.55,]

# 3) Independent: not under occupation or some other kind of de facto foreign control
vdem <- vdem[vdem$country_name!="Hong Kong" & vdem$country_name!="Somaliland" & vdem$country_name!="Palestine/Gaza" & vdem$country_name!="Palestine/West Bank" & vdem$country_name!="Zanzibar" & vdem$country_name!="German Democratic Republic",]

# 4) Technically elected top leadership: no absolute monarchies or similar arrangements (where no election represents an actual referendum on the regime or ability to change the system) -- Brunei and Tonga are not in V-Dem because they're micro-states
vdem <- vdem[vdem$country_name!="Kuwait" & vdem$country_name!="Bahrain" & vdem$country_name!="Jordan" & vdem$country_name!="Morocco" & vdem$country_name!="Iran" & vdem$country_name!="Bhutan" & vdem$country_name!="United Arab Emirates" & vdem$country_name!="Saudi Arabia" & vdem$country_name!="Eswatini" & vdem$country_name!="Qatar",]

# 5) Post-Cold War: 1990-2019
vdem <- vdem[vdem$year>1989 & vdem$year<2023,]

# 6) Multiparty elections: multiple parties permitted to run, including opposition parties
vdem$v2elmulpar_ord <- na.locf(vdem$v2elmulpar_ord, na.rm = F)
vdem <- vdem[vdem$v2elmulpar_ord>=2 & !is.na(vdem$v2elmulpar_ord),]

# 7) Exclude micro-states (<500k pop)
# Addressed through manual checks -- see section below
# Many micro-states not included in V-Dem and Fariss data; manual checks exclude the few remaining cases on a country-year basis

# Limiting to election years
vdem <- vdem[!is.na(vdem$v2eltype_0),]
vdem <- vdem[vdem$v2eltype_0==1 | vdem$v2eltype_1==1 | vdem$v2eltype_2==1 | vdem$v2eltype_3==1 | vdem$v2eltype_6==1 | vdem$v2eltype_7==1,]

### Manual Checks -----------

# Importing manual checks/research
checkwork <- read_excel("OPAE_electionyears_fulllist.xlsx")

# Combining with V-Dem
vdem <- merge(vdem, checkwork, by=c("country_name", "year"), all.x = TRUE, all.y = TRUE)

# Updating variables to be numeric
vdem$oppvote <- as.numeric(vdem$oppvote)
vdem$incvote <- as.numeric(vdem$incvote)
vdem$margin <- as.numeric(vdem$margin)
vdem$oppcoal_full <- as.numeric(vdem$oppcoal_full)
vdem$turnover_full <- as.numeric(vdem$turnover_full)
vdem$exclude <- as.numeric(vdem$exclude)
vdem$include_10 <- as.numeric(vdem$include_10)
vdem$exec <- as.numeric(vdem$exec)
vdem$pres <- as.numeric(vdem$pres)
vdem$leg <- as.numeric(vdem$leg)

# Include cases that meet 5-year incumbency & 0.55 V-Dem thresholds (widest possible sample)
vdem <- vdem[vdem$exclude==0 & !is.na(vdem$exclude),]

### Additional Variables --------------

# Calculating margins (replaces manual margin variable with variable calculated based on incumbent and opposition vote shares)
vdem$margin <- vdem$oppvote - vdem$incvote
# Restricting margin (based on turnover); limiting margins to <=0 if no turnover, >=0 if turnover
vdem$margin_restricted <- ifelse(vdem$margin>0 & vdem$turnover_full==0, 0, vdem$margin)
vdem$margin_restricted <- ifelse(vdem$margin<0 & vdem$turnover_full==1, 0, vdem$margin_restricted)
# Restricting oppvote (based on turnover); <=50 if no turnover, >=50 if turnover
vdem$oppvote_restricted <- ifelse(vdem$oppvote>50 & vdem$turnover_full==0, 50, vdem$oppvote)
vdem$oppvote_restricted <- ifelse(vdem$oppvote<50 & vdem$turnover_full==1, 50, vdem$oppvote_restricted)

# Creating an indicator for "nearly stunning" election -- above 40% opposition vote share, but no turnover
vdem$nearstun <- ifelse(vdem$oppvote>40 & vdem$turnover_full==0, 1, 0)

# Creating region factor variable
vdem$region <- as.factor(vdem$e_regionpol_6C)

## Final Datasets ----------------

# Limiting to main results data
# 10-year incumbent & 0.5 V-Dem threshold
vdem_main <- vdem[vdem$include_10==1 & !is.na(vdem$include_10) & vdem$v2x_polyarchy_lag<=0.5,]

# Limiting to other samples for robustness
# 0.42 V-Dem threshold
vdem_alt10.4 <- vdem[vdem$include_10==1 & !is.na(vdem$include_10) & vdem$v2x_polyarchy_lag<=0.42,]
# 0.55 V-Dem threshold
vdem_alt10.55 <- vdem[vdem$include_10==1 & !is.na(vdem$include_10),]
# 5-year incumbent threshold
vdem_alt5inc <- vdem[vdem$exclude==0 & !is.na(vdem$exclude) & vdem$v2x_polyarchy_lag<=0.5,]

# Limiting to only executive elections
vdem_exec <- vdem_main[vdem_main$exec==1,]

# Widest sample (least restrictive V-Dem and incumbency thresholds)
vdem_preserve <- vdem

### Toggle Dataset --------
vdem <- vdem_main
vdem <- vdem_alt10.4
vdem <- vdem_alt10.55
vdem <- vdem_alt5inc
vdem <- vdem_exec
vdem <- vdem_preserve

# MAIN ANALYSIS ---------------

# Set working directory to save plots to sub-folder -- enter file path below
setwd(file.path(".."))
setwd(file.path(".","Final Plots"))

# Set to main dataset (based on restrictions specified in the main manuscript)
vdem <- vdem_main

### Basic Summary -----------

# This code is used to calcluate basic summary statistics discussed in the "Data" section of the manuscript

# Total elections
length(vdem$country_year)

# Elections featuring coalitions
sum(vdem$oppcoal_full)
sum(vdem$oppcoal_full)/length(vdem$country_year)

# Elections featuring turnovers
sum(vdem$turnover_full)
sum(vdem$turnover_full)/length(vdem$country_year)

### Vote Share ---------

#### Plots -------------

# These plots correspond to Figure 2 in the main manuscript

# Opposition Vote Share
plot1 <- ggplot(vdem[!is.na(vdem$oppcoal_full),], aes(x=oppvote, fill=as.factor(oppcoal_full))) +
  geom_density(alpha=0.6) +
  scale_fill_grey(name="Coalition", labels=c("No", "Yes")) +
  xlab("Opposition vote share") + ylab("Density") +
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"),
        legend.position = "none") # Remove legend

# Save plot
ggsave("ManuscriptFigure2_CATK_densityplots_a.png", plot = plot1, width = 4.6875, height = 3.9583, dpi = 300, units = "in")

# Vote Share Margin
plot2 <- ggplot(vdem[!is.na(vdem$oppcoal_full),], aes(x=margin, fill=as.factor(oppcoal_full))) +
  geom_density(alpha=0.6) +
  scale_fill_grey(name="Coalition", labels=c("No", "Yes")) +
  xlab("Vote margin (opp. vote share - incumbent vote share)") + ylab("Density") +
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"),
        legend.position = "none") # Remove legend

# Save plot
ggsave("ManuscriptFigure2_CATK_densityplots_b.png", plot = plot2, width = 4.6875, height = 3.9583, dpi = 300, units = "in")

# Extract the legend
legend <- get_legend(
  ggplot(vdem[!is.na(vdem$oppcoal_full),], aes(x=oppvote, fill=as.factor(oppcoal_full))) +
    geom_density(alpha=0.6) +
    scale_fill_grey(name="Coalition", labels=c("No", "Yes")) +
    theme(
      legend.position = "bottom", 
      legend.title = element_text(size=10, family = "Arial", color = "#000000"), 
      legend.text = element_text(size=8, family = "Arial", color = "#000000"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA)) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
)

# Save the legend separately
legend_plot <- plot_grid(legend, ncol = 1)
ggsave("ManuscriptFigure2_CATK_densityplots_legend.png", plot = legend_plot, width = 4.6875, height = 1, dpi = 300, units = "in")

#### Averages ------------

# This code is used to calculate the median opposition vote shares mentioned in the manuscript under "Results: Opposition Performance"

# Median
# Opp Vote Share
median(vdem$oppvote[vdem$oppcoal_full==1])
median(vdem$oppvote[vdem$oppcoal_full==0])
# Margins
median(vdem$margin[vdem$oppcoal_full==1])
median(vdem$margin[vdem$oppcoal_full==0])

#### Coalition Percentages -------------

# This code is used to calculate statistics included in the text of the manuscript under "Results: Post-Election Trajectories"

# Overall proportion of coalitions across all losing elections
sum(vdem$oppcoal_full[vdem$turnover_full==0])/length(vdem$oppcoal_full[vdem$turnover_full==0])

# Proportion of coalitions across losing elections where opposition received more than 40% of the vote
sum(vdem$oppcoal_full[vdem$turnover_full==0 & vdem$oppvote>40])/length(vdem$oppcoal_full[vdem$turnover_full==0 & vdem$oppvote>40])

# Proportion of coalitions across turnover elections
sum(vdem$oppcoal_full[vdem$turnover_full==1])/length(vdem$oppcoal_full[vdem$turnover_full==1])

### LOESS Plots --------------

#### Regime Trajectories ------------

# These plots correspond to Figure 3 in the main manuscript

# 1-year ED change, all losing elections
plot1 <- ggplot(vdem[vdem$turnover_full==0,], aes(x=oppvote_restricted, y=vdem_change)) + 
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==0,], color="black", shape=4) +
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==1,], color="black", shape=16) +
  geom_smooth(method='loess', linewidth = 1, se = T, level = 0.95, color = "grey40") +
  coord_cartesian(ylim=c((-.16), .16)) +
  xlab("Opposition vote share") + ylab("Electoral democracy change") +
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"))
# Save plot
ggsave("ManuscriptFigure3_CATK_LOESSmain_a.png", plot = plot1, width = 4.6875, height = 5.5208, dpi = 300, units = "in")

# 5-year ED change, all losing elections
plot2 <- ggplot(vdem[vdem$turnover_full==0,], aes(x=oppvote_restricted, y=vdem_change_5yr)) + 
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==0,], color="black", shape=4) +
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==1,], color="black", shape=16) +
  geom_smooth(method='loess', linewidth = 1, se = T, level = 0.95, color = "grey40") +
  coord_cartesian(ylim=c((-.16), .16)) +
  xlab("Opposition vote share") + ylab("Electoral democracy change") +
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"))
# Save plot
ggsave("ManuscriptFigure3_CATK_LOESSmain_b.png", plot = plot2, width = 4.6875, height = 5.5208, dpi = 300, units = "in")

#### Other Outcomes ------------

# These plots correspond to Figure 16 in the Appendix

# Elec irregularities change, all losing elections
plot1 <- ggplot(vdem[vdem$turnover_full==0,], aes(x=oppvote_restricted, y=irreg_change)) + 
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==0,], color="black", shape=4) +
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==1,], color="black", shape=16) +
  geom_smooth(method='loess', linewidth = 1, se = T, level = 0.95, color = "grey40") +
  coord_cartesian(ylim=c((-1.5), 1.5)) +
  ggtitle("Electoral Irregularities") +
  xlab("Opposition Vote Share") + ylab("Change in Electoral Irregularities") +
  theme_classic()

# Opp harassment change, all losing elections
plot2 <- ggplot(vdem[vdem$turnover_full==0,], aes(x=oppvote_restricted, y=intim_change)) + 
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==0,], color="black", shape=4) +
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==1,], color="black", shape=16) +
  geom_smooth(method='loess', linewidth = 1, se = T, level = 0.95, color = "grey40") +
  coord_cartesian(ylim=c((-1), 1)) +
  ggtitle("Opposition Harassment") +
  xlab("Opposition Vote Share") + ylab("Change in Opposition Harassment") +
  theme_classic()

# Repression change, all losing elections
plot3 <- ggplot(vdem[vdem$turnover_full==0,], aes(x=oppvote_restricted, y=latentmean_change_3yr)) + 
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==0,], color="black", shape=4) +
  geom_point(data = vdem[vdem$turnover_full==0 & vdem$oppcoal_full==1,], color="black", shape=16) +
  geom_smooth(method='loess', linewidth = 1, se = T, level = 0.95, color = "grey40") +
  coord_cartesian(ylim=c((-1.2), 1.2)) +
  ggtitle("Repression") +
  xlab("Opposition Vote Share") + ylab("Change in Physical Integrity Rights") +
  theme_classic()

loessh3 <- grid.arrange(plot1, plot2, plot3, ncol=3)

ggsave("CATK_LOESS_H3.png", plot = loessh3, width = 9.375, height = 4.6875, dpi = 300, units = "in")

### OLS Models -------------

#### Regime Trajectories -----------

# These plots correspond to Figure 4 in the manuscript

vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0,])

m <- lm(vdem_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot1 <- plot(p, ci.style = "dot") +
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition vote share", y="Change in democracy score", title = NULL) + 
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

ggsave("ManuscriptFigure4_CATK_OLSplotsmain_a.png", plot = plot1, width = 4.6875, height = 5.46875, dpi = 300, units = "in")

m <- lm(vdem_change_5yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot2 <- plot(p, ci.style = "dot") +
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition vote share", y="Change in democracy score", title = NULL) + 
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

ggsave("ManuscriptFigure4_CATK_OLSplotsmain_b.png", plot = plot2, width = 4.6875, height = 5.46875, dpi = 300, units = "in")


#### Other Outcomes ---------------

# These plots correspond to Figure 6 in the manuscript

vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0,])

m<-lm(irreg_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + latentmean_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot1 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-1.5), 1)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition vote share", y="Change in electoral irregularities", title=NULL) + 
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"))

ggsave("ManuscriptFigure6_CATK_H3_OLSplots_a.png", plot = plot1, width = 3.125, height = 4.4792, dpi = 300, units = "in")

m<-lm(intim_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + latentmean_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot2 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-1.35), .9)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition vote share", y="Change in opposition harassment", title=NULL) + 
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"))

ggsave("ManuscriptFigure6_CATK_H3_OLSplots_b.png", plot = plot2, width = 3.125, height = 4.4792, dpi = 300, units = "in")

m<-lm(latentmean_change_3yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + econ_growth + e_gdppc + v2xel_frefair + exec + v2x_polyarchy_lag + latentmean_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot3 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.75), .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition vote share", y="Change in physical integrity rights", title=NULL) + 
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "#000000", size = 11.5),
        axis.title.x = element_text(margin = margin(t = 10), color = "#000000"),
        axis.title.y = element_text(margin = margin(r = 10), color = "#000000"),
        axis.text = element_text(color = "#000000"))

ggsave("ManuscriptFigure6_CATK_H3_OLSplots_c.png", plot = plot3, width = 3.125, height = 4.4792, dpi = 300, units = "in")


### GAM Models --------------

#### Regime Trajectories --------------

# These plots correspond to Figure 5 in the manuscript

# Set data
vdem_use <- vdem[vdem$turnover_full == 0,]

# 1-year ED change, all losing elections
gam.1yr <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# 5-year ED change, all losing elections
gam.5yr <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# Open PNG device
png("ManuscriptFigure5_CATK_GAM_main_a.png", width = 4.6875, height = 5, units = "in", res = 300)

# Adjust margins: bottom, left, top, right
par(mar = c(3, 3.75, 1, 2) + 0.1, tcl = -0.2, mgp = c(1, 0, 0), family = "Arial")

# Plotting the 1-year ED change GAM model without axis labels and y-axis ticks
plot(gam.1yr, rug = FALSE, se = TRUE, select = 1, ylab = "", xlab = "", yaxt = "n", xaxt = "n", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), bty = "l", las = 1)
abline(h = 0, col = "black", lty = 2)

# Add custom y-axis title
mtext("Partial effect of opposition performance", side = 2, line = 2, col = "#000000", cex = 0.88, family = "Arial")

# Add custom x-axis title
mtext("Opposition vote share", side = 1, line = 1.25, col = "#000000", cex = 0.88, family = "Arial")

# Add custom y-axis ticks
par(mgp = c(1, 0.5, 0), family = "Arial")
axis(2, at = c(-0.1, 0, 0.1), las = 1, col.axis = "#000000", cex.axis = 0.68)

# Add custom x-axis ticks
par(mgp = c(1, 0, 0), family = "Arial")
axis(1, at = seq(0, 50, by = 10), col.axis = "#000000", cex.axis = 0.68)


dev.off()

# Open PNG device
png("ManuscriptFigure5_CATK_GAM_main_b.png", width = 4.6875, height = 5, units = "in", res = 300)

# Adjust margins: bottom, left, top, right
par(mar = c(3, 3.75, 1, 2) + 0.1, tcl = -0.2, mgp = c(1, 0, 0), family = "Arial")

# Plotting the 5-year ED change GAM model without axis labels and y-axis ticks
plot(gam.5yr, rug = FALSE, se = TRUE, select = 1, ylab = "", xlab = "", yaxt = "n", xaxt = "n", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), bty = "l", las = 1)
abline(h = 0, col = "black", lty = 2)

# Add custom y-axis title
mtext("Partial effect of opposition performance", side = 2, line = 2, col = "#000000", cex = 0.88, family = "Arial")

# Add custom x-axis title
mtext("Opposition vote share", side = 1, line = 1.25, col = "#000000", cex = 0.88, family = "Arial")

# Add custom y-axis ticks
par(mgp = c(1, 0.5, 0), family = "Arial")
axis(2, at = c(-0.1, 0, 0.1), las = 1, col.axis = "#000000", cex.axis = 0.68)

# Add custom x-axis ticks
par(mgp = c(1, 0, 0), family = "Arial")
axis(1, at = seq(0, 50, by = 10), col.axis = "#000000", cex.axis = 0.68)


dev.off()

#### Other Outcomes -------------

# These plots correspond to Figure 17 in the Appendix

# Open PNG device
png("CATK_GAM_H3.png", width = 9.375, height = 4.375, units = "in", res = 300)

# Set data
vdem_use <- vdem[vdem$turnover_full == 0,]

# Elec irregularities
gam.irreg <- gam(irreg_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# Opp harassment
gam.intim <- gam(intim_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# Repression
gam.repress <- gam(latentmean_change_3yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + latentmean_lag + region, data = vdem_use)

# Setting up the plot area to arrange plots side by side
par(mfrow = c(1, 3))

# Plotting the election irregularities GAM model
plot(gam.irreg, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.9, 0.25), main = "Electoral Irregularities", bty = "l")
abline(h = 0, col = "black")

# Plotting the opp harassment GAM model
plot(gam.intim, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.9, 0.25), main = "Opposition Harassment", bty = "l")
abline(h = 0, col = "black")

# Plotting the repression GAM model
plot(gam.repress, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.9, 0.25), main = "Repression", bty = "l")
abline(h = 0, col = "black")

dev.off()


# APPENDIX ANALYSIS --------------

# Set working directory to save plots to sub-folder -- enter file path below
setwd(file.path(".."))
setwd(file.path(".","Final Plots"))

## Alternate Coalitions --------------------

# Set to main dataset (based on restrictions specified in the main manuscript)
vdem <- vdem_main

# Create alternate coalitions variable
vdem$oppcoal_alt <- ifelse(vdem$oppcoal_borderline==0, vdem$oppcoal_full, ifelse(vdem$oppcoal_full==1, 0, 1))

### Plots -------------

# These plots correspond to Figure 1 in the Appendix

# Opposition Vote Share
vdem$oppcoal_inv <- ifelse(vdem$oppcoal_alt==1, 0, 1)
plot1 <- ggplot(vdem[!is.na(vdem$oppcoal_alt),], aes(x=oppvote, fill=as.factor(oppcoal_alt))) +
  geom_density(alpha=0.6) +
  scale_fill_grey(name="Coalition",labels=c("No", "Yes")) +
  ggtitle("Vote Share") +
  xlab("Opposition Vote Share") + ylab("Density") +
  theme_classic()
# Vote Share Margin
plot2 <- ggplot(vdem[!is.na(vdem$oppcoal_alt),], aes(x=margin, fill=as.factor(oppcoal_alt))) +
  geom_density(alpha=0.6) +
  scale_fill_grey(name="Coalition",labels=c("No", "Yes")) +
  ggtitle("Vote Margin") +
  xlab("Vote Margin (Opp Vote Share - Incumbent Vote Share)") + ylab("Density") +
  labs(fill = "Coalition") +
  theme_classic()

alt_coals <- grid.arrange(plot1, plot2, ncol=2)

ggsave("CATK_densityplots_ALT.png", plot = alt_coals, width = 900, height = 400, dpi = 96, units = "px")

### Averages ------------

# This code is used to calculate the median opposition vote shares using alternative coalition measures mentioned on pg. 4 in the Appendix

# Median
# Opp Vote Share
median(vdem$oppvote[vdem$oppcoal_alt==1])
median(vdem$oppvote[vdem$oppcoal_alt==0])
# Margins
median(vdem$margin[vdem$oppcoal_alt==1])
median(vdem$margin[vdem$oppcoal_alt==0])

## Alternate Samples -------------------

### 0.42 V-Dem Threshold -----------------

vdem <- vdem_alt10.4

#### OLS -------------

# These plots correspond to Figure 2 in the Appendix

vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0,])

m<-lm(vdem_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot1 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 1-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

m<-lm(vdem_change_5yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot2 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 5-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

comb42 <- grid.arrange(plot1, plot2, ncol=2)

ggsave("CATK_OLS_vdem42.png", plot = comb42, width = 900, height = 550, dpi = 96, units = "px")

#### GAM ---------------

# These plots correspond to Figure 3 in the Appendix

# Open PNG device
png("CATK_GAM_vdem42.png", width = 900, height = 500, res = 96)

# Set data
vdem_use <- vdem[vdem$turnover_full == 0,]

# 1-year ED change, all losing elections
gam.1yr <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# 5-year ED change, all losing elections
gam.5yr <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# Setting up the plot area to arrange plots side by side
par(mfrow = c(1, 2))

# Plotting the 1-year ED change GAM model
plot(gam.1yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "1-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

# Plotting the 5-year ED change GAM model
plot(gam.5yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "5-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

dev.off()

### 0.55 V-Dem Threshold --------------------

vdem <- vdem_alt10.55

#### OLS ---------------

# These plots correspond to Figure 4 in the Appendix

vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0,])

m<-lm(vdem_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot1 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 1-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

m<-lm(vdem_change_5yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot2 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 5-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

comb55 <- grid.arrange(plot1, plot2, ncol=2)

ggsave("CATK_OLS_vdem55.png", plot = comb55, width = 900, height = 550, dpi = 96, units = "px")


#### GAM ---------------

# These plots correspond to Figure 5 in the Appendix

# Open PNG device
png("CATK_GAM_vdem55.png", width = 900, height = 500, res = 96)

# Set data
vdem_use <- vdem[vdem$turnover_full == 0,]

# 1-year ED change, all losing elections
gam.1yr <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# 5-year ED change, all losing elections
gam.5yr <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# Setting up the plot area to arrange plots side by side
par(mfrow = c(1, 2))

# Plotting the 1-year ED change GAM model
plot(gam.1yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "1-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

# Plotting the 5-year ED change GAM model
plot(gam.5yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "5-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

dev.off()


### 5-year Incumbency Threshold --------------------

vdem <- vdem_alt5inc


#### OLS ---------------

# These plots correspond to Figure 6 in the Appendix

vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0,])

m<-lm(vdem_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot1 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 1-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

m<-lm(vdem_change_5yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot2 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 5-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

comb5yr <- grid.arrange(plot1, plot2, ncol=2)

ggsave("CATK_OLS_5yrinc.png", plot = comb5yr, width = 900, height = 550, dpi = 96, units = "px")


#### GAM ---------------

# These plots correspond to Figure 7 in the Appendix

# Open PNG device
png("CATK_GAM_5yrinc.png", width = 900, height = 500, res = 96)

# Set data
vdem_use <- vdem[vdem$turnover_full == 0,]

# 1-year ED change, all losing elections
gam.1yr <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# 5-year ED change, all losing elections
gam.5yr <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# Setting up the plot area to arrange plots side by side
par(mfrow = c(1, 2))

# Plotting the 1-year ED change GAM model
plot(gam.1yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "1-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

# Plotting the 5-year ED change GAM model
plot(gam.5yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "5-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

dev.off()


### Executive Elections Only -------------------

vdem <- vdem_exec


#### OLS -------------

# These plots correspond to Figure 8 in the Appendix

vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0,])

m<-lm(vdem_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot1 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 1-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

m<-lm(vdem_change_5yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot2 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 5-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

combexec <- grid.arrange(plot1, plot2, ncol=2)

ggsave("CATK_OLS_exec.png", plot = combexec, width = 900, height = 550, dpi = 96, units = "px")


#### GAM ---------------

# These plots correspond to Figure 9 in the Appendix

# Open PNG device
png("CATK_GAM_exec.png", width = 900, height = 500, res = 96)

# Set data
vdem_use <- vdem[vdem$turnover_full == 0,]

# 1-year ED change, all losing elections
gam.1yr <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# 5-year ED change, all losing elections
gam.5yr <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# Setting up the plot area to arrange plots side by side
par(mfrow = c(1, 2))

# Plotting the 1-year ED change GAM model
plot(gam.1yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "1-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

# Plotting the 5-year ED change GAM model
plot(gam.5yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "5-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

dev.off()


### Widest Sample ------------------

vdem <- vdem_preserve


#### OLS ----------------

# These plots correspond to Figure 10 in the Appendix

vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0,])

m<-lm(vdem_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot1 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 1-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

m<-lm(vdem_change_5yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = vdem_use)
p <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
plot2 <- plot(p, ci.style = "dot") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 5-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  theme_classic()

combwidest <- grid.arrange(plot1, plot2, ncol=2)

ggsave("CATK_OLS_widest.png", plot = combwidest, width = 900, height = 550, dpi = 96, units = "px")


#### GAM ---------------

# These plots correspond to Figure 11 in the Appendix

# Open PNG device
png("CATK_GAM_widest.png", width = 900, height = 500, res = 96)

# Set data
vdem_use <- vdem[vdem$turnover_full == 0,]

# 1-year ED change, all losing elections
gam.1yr <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# 5-year ED change, all losing elections
gam.5yr <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

# Setting up the plot area to arrange plots side by side
par(mfrow = c(1, 2))

# Plotting the 1-year ED change GAM model
plot(gam.1yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "1-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

# Plotting the 5-year ED change GAM model
plot(gam.5yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = "5-Year Change in Democracy", bty = "l")
abline(h = 0, col = "black")

dev.off()


## Regression Tables ------------

### Main Sample ------------
vdem_main$region <- as.factor(vdem_main$e_regionpol_6C)

vdem_main$oppvote_adjusted <- (vdem_main$oppvote_restricted)/100

m1<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m5<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

### Alternate Samples ----------------

# Moving democracy threshold -- from 0.5 to 0.42
vdem_alt10.4$region <- as.factor(vdem_alt10.4$e_regionpol_6C)

vdem_alt10.4$oppvote_adjusted <- (vdem_alt10.4$oppvote_restricted)/100
m1b<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_alt10.4[vdem_alt10.4$turnover_full==0,])

m5b<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_alt10.4[vdem_alt10.4$turnover_full==0,])

# Moving democracy threshold -- from 0.5 to 0.55
vdem_alt10.55$region <- as.factor(vdem_alt10.55$e_regionpol_6C)

vdem_alt10.55$oppvote_adjusted <- (vdem_alt10.55$oppvote_restricted)/100
m1c<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_alt10.55[vdem_alt10.55$turnover_full==0,])

m5c<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_alt10.55[vdem_alt10.55$turnover_full==0,])

# Moving continued rule threshold -- from 10 years to 5 years
vdem_alt5inc$region <- as.factor(vdem_alt5inc$e_regionpol_6C)

vdem_alt5inc$oppvote_adjusted <- (vdem_alt5inc$oppvote_restricted)/100
m1d<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_alt5inc[vdem_alt5inc$turnover_full==0,])

m5d<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_alt5inc[vdem_alt5inc$turnover_full==0,])

# Limiting to executive elections only
vdem_exec$region <- as.factor(vdem_exec$e_regionpol_6C)

vdem_exec$oppvote_adjusted <- (vdem_exec$oppvote_restricted)/100
m1e<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_exec[vdem_exec$turnover_full==0,])

m5e<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_exec[vdem_exec$turnover_full==0,])

# Widest sample
vdem_preserve$region <- as.factor(vdem_preserve$e_regionpol_6C)

vdem_preserve$oppvote_adjusted <- (vdem_preserve$oppvote_restricted)/100
m1f<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_preserve[vdem_preserve$turnover_full==0,])

m5f<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_preserve[vdem_preserve$turnover_full==0,])

# One-year table
# Corresponds to Appendix Table 3
texreg(l = list(m1, m1b, m1c, m1d, m1e, m1f), include.ci = FALSE, 
       #digits = 3, 
       custom.coef.names = c("Constant", "Opp Vote Share", "$\text{Opp Vote Share}^2$", "$\text{Opp Vote Share}^3$", "Electoral Irregularities", "Opp Harassment", "Election Quality", "Pro-Dem Mobilization", "Executive", "Democracy (lagged)", "GDP growth", "GDP per capita"),
       omit.coef = "region",
       stars = c(0.1, 0.05, 0.01)
)

# Five-year table
# Corresponds to Appendix Table 4
texreg(l = list(m5, m5b, m5c, m5d, m5e, m5f), include.ci = FALSE, 
       #digits = 3, 
       custom.coef.names = c("Constant", "Opp Vote Share", "$\text{Opp Vote Share}^2$", "$\text{Opp Vote Share}^3$", "Electoral Irregularities", "Opp Harassment", "Election Quality", "Pro-Dem Mobilization", "Executive", "Democracy (lagged)", "GDP growth", "GDP per capita"),
       omit.coef = "region",
       stars = c(0.1, 0.05, 0.01)
)


### 1-7 Year Outcomes --------------

m1<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m2<-lm(vdem_change_2yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m3<-lm(vdem_change_3yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m4<-lm(vdem_change_4yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m5<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m6<-lm(vdem_change_6yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m7<-lm(vdem_change_7yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])


# Combined table
# Corresponds to Appendix Table 8
texreg(l = list(m1, m2, m3, m4, m5, m6, m7), include.ci = FALSE, 
       #digits = 3, 
       custom.coef.names = c("Constant", "Opp Vote Share", "$\text{Opp Vote Share}^2$", "$\text{Opp Vote Share}^3$", "Electoral Irregularities", "Opp Harassment", "Election Quality", "Pro-Dem Mobilization", "Executive", "Democracy (lagged)", "GDP growth", "GDP per capita"),
       omit.coef = "region",
       stars = c(0.1, 0.05, 0.01)
)


### Alternate Controls -----------------

vdem_main$region <- as.factor(vdem_main$e_regionpol_6C)

vdem_main$oppvote_adjusted <- (vdem_main$oppvote_restricted)/100

m1<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m1a<-lm(vdem_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m5<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

m5a<-lm(vdem_change_5yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

# Output regressions
# Corresponds to Appendix Table 9
texreg(l = list(m1, m1a, m5, m5a), include.ci = FALSE, 
       #digits = 3, 
       custom.coef.names = c("Constant", "Opp Vote Share", "$ \text{Opp Vote Share}^2$", "$ \text{Opp Vote Share}^3$", "Electoral Irregularities", "Opp Harassment", "Election Quality", "Pro-Dem Mobilization", "Executive", "Democracy (lagged)", "GDP growth", "GDP per capita"),
       omit.coef = "region",
       stars = c(0.1, 0.05, 0.01),
       custom.model.names = c("Main Model", "Alternate Controls", "Main Model", "Alternate Controls"))

### Other Outcomes -------------

vdem_main$oppvote_adjusted <- (vdem_main$oppvote_restricted)/100

mirreg<-lm(irreg_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim  + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

mintim<-lm(intim_change ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_main[vdem_main$turnover_full==0,])

mrepress<-lm(latentmean_change_3yr ~ poly(oppvote_adjusted, 3, raw = TRUE) + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + latentmean_lag + region, data = vdem_main[vdem_main$turnover_full==0,])

# Output regressions
# Corresponds to Appendix Table 10
texreg(l = list(mirreg, mintim, mrepress), include.ci = FALSE, 
       #digits = 3, 
       custom.coef.names = c("Constant", "Opp Vote Share", "$\text{Opp Vote Share}^2$", "$\text{Opp Vote Share}^3$", "Electoral Irregularities", "Opp Harassment", "Election Quality", "Pro-Dem Mobilization", "Executive", "Democracy (lagged)", "GDP growth", "GDP per capita", "Repression (lagged)"),
       omit.coef = "region",
       stars = c(0.1, 0.05, 0.01),
       custom.model.names = c("Elec Irregularities", "Opp Harassment", "Repression"))

### GAM -------------

#### 1-Year Change -------------

# Results below correspond to columns in Appendix Table 5 (table compiled based on these results; this code does not automatically output Latex-ready tables)

# Set data
vdem <- vdem_main
vdem_use <- vdem[vdem$turnover_full == 0,]

# 1-year ED change, main sample
gam.1yr <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr)

# V-Dem 0.42 threshold
vdem_use <- vdem_alt10.4[vdem_alt10.4$turnover_full == 0,]
gam.1yr.alt10.4 <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr.alt10.4)

# V-Dem 0.55 threshold
vdem_use <- vdem_alt10.55[vdem_alt10.55$turnover_full == 0,]
gam.1yr.alt10.55 <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr.alt10.55)

# 5-year incumbency threshold
vdem_use <- vdem_alt5inc[vdem_alt5inc$turnover_full == 0,]
gam.1yr.alt5inc <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr.alt5inc)

# Executive elections only
vdem_use <- vdem_exec[vdem_exec$turnover_full == 0,]
gam.1yr.exec <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr.exec)

# Widest sample
vdem_use <- vdem_preserve[vdem_preserve$turnover_full == 0,]
gam.1yr.widest <- gam(vdem_change ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr.widest)


#### 5-Year Change -------------

# Results below correspond to columns in Appendix Table 6 (table compiled based on these results; this code does not automatically output Latex-ready tables)

# Set data
vdem <- vdem_main
vdem_use <- vdem[vdem$turnover_full == 0,]

# 5-year ED change, main sample
gam.5yr <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr)

# V-Dem 0.42 threshold
vdem_use <- vdem_alt10.4[vdem_alt10.4$turnover_full == 0,]
gam.5yr.alt10.4 <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr.alt10.4)

# V-Dem 0.55 threshold
vdem_use <- vdem_alt10.55[vdem_alt10.55$turnover_full == 0,]
gam.5yr.alt10.55 <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr.alt10.55)

# 5-year incumbency threshold
vdem_use <- vdem_alt5inc[vdem_alt5inc$turnover_full == 0,]
gam.5yr.alt5inc <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr.alt5inc)

# Executive elections only
vdem_use <- vdem_exec[vdem_exec$turnover_full == 0,]
gam.5yr.exec <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr.exec)

# Widest sample
vdem_use <- vdem_preserve[vdem_preserve$turnover_full == 0,]
gam.5yr.widest <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=4, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr.widest)

#### Alt DFs -------------

# Results below correspond to columns in Appendix Table 7 (table compiled based on these results; this code does not automatically output Latex-ready tables)

# Set data
vdem <- vdem_main
vdem_use <- vdem[vdem$turnover_full == 0,]

# 3df
gam.1yr.3df <- gam(vdem_change ~ s(oppvote_restricted, k=3, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr.3df)

# 5df
gam.1yr.5df <- gam(vdem_change ~ s(oppvote_restricted, k=5, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr.5df)

# 6df
gam.1yr.6df <- gam(vdem_change ~ s(oppvote_restricted, k=6, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.1yr.6df)

# Set data
vdem <- vdem_main
vdem_use <- vdem[vdem$turnover_full == 0,]

# 3df
gam.5yr.3df <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=3, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr.3df)

# 5df
gam.5yr.5df <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=5, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr.5df)

# 6df
gam.5yr.6df <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=6, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)

summary(gam.5yr.6df)


## Jackknifing ---------------

### Country Jackknife ---------------------

# Code below produces Figure 14 in the Appendix

vdem <- vdem_main
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0,])

# 1-year democracy change

# Get unique country names
unique_countries <- unique(vdem_use$country_name)

# Add region variable
vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0 & !is.na(vdem$turnover_full),])

# Create an empty plot
plot1 <- ggplot() +
  geom_blank() +  # Create an empty plot to initialize the ggplot object
  
  # Additional aesthetics and theme
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 1-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic()

# Iterate over each unique country
for (i in unique(vdem_use$country_name)) {
  # Subset the data by removing the current country
  subset_data <- vdem_use %>%
    filter(country_name != i)
  
  # Fit the linear model
  m <- lm(vdem_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = subset_data)
  
  # Get marginal effects for the subset data
  p_subset <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
  
  # Add layer to the combined plot
  plot1 <- plot1 +
    geom_line(data = p_subset, aes(x = x, y = predicted), color = "grey40", size = 1, alpha = 0.5) +
    geom_line(data = p_subset, aes(x = x, y = conf.low), color = "grey40", size = 0.5, alpha = 0.3, linetype = "dotted") +
    geom_line(data = p_subset, aes(x = x, y = conf.high), color = "grey40", size = 0.5, alpha = 0.3, linetype = "dotted")
}


# 5-year democracy change

unique_countries <- unique(vdem_use$country_name)

# Add region variable
vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem_use <- as.data.frame(vdem[vdem$turnover_full==0 & !is.na(vdem$turnover_full),])

# Create an empty plot
plot2 <- ggplot() +
  geom_blank() +  # Create an empty plot to initialize the ggplot object
  
  # Additional aesthetics and theme
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 5-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic()

# Iterate over each unique country
for (i in unique(vdem_use$country_name)) {
  # Subset the data by removing the current country
  subset_data <- vdem_use %>%
    filter(country_name != i)
  
  # Fit the linear model
  m <- lm(vdem_change_5yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = subset_data)
  
  # Get marginal effects for the subset data
  p_subset <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
  
  # Add layer to the combined plot
  plot2 <- plot2 +
    geom_line(data = p_subset, aes(x = x, y = predicted), color = "grey40", size = 1, alpha = 0.5) +
    geom_line(data = p_subset, aes(x = x, y = conf.low), color = "grey40", size = 0.5, alpha = 0.3, linetype = "dotted") +
    geom_line(data = p_subset, aes(x = x, y = conf.high), color = "grey40", size = 0.5, alpha = 0.3, linetype = "dotted")
}


# Combine plots
country_jk <- grid.arrange(plot1, plot2, ncol = 2)

ggsave("CATK_country_jackknife.png", plot = country_jk, width = 900, height = 550, dpi = 96, units = "px")


### Year Jackknife --------------

# Code below produces Figure 15 in the Appendix

# 1-year democracy change

# Get unique country names
unique_years <- unique(vdem_use$year)

# Add region variable
vdem_use$region <- as.factor(vdem_use$e_regionpol_6C)

# Create an empty plot
plot1 <- ggplot() +
  geom_blank() +  # Create an empty plot to initialize the ggplot object
  
  # Additional aesthetics and theme
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 1-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic()

# Iterate over each unique country
for (i in unique_years) {
  # Subset the data by removing the current country
  subset_data <- vdem_use %>%
    filter(year != i)
  
  # Fit the linear model
  m <- lm(vdem_change ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = subset_data)
  
  # Get marginal effects for the subset data
  p_subset <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
  
  # Add layer to the combined plot
  plot1 <- plot1 +
    geom_line(data = p_subset, aes(x = x, y = predicted), color = "grey40", size = 1, alpha = 0.5) +
    geom_line(data = p_subset, aes(x = x, y = conf.low), color = "grey40", size = 0.5, alpha = 0.3, linetype = "dotted") +
    geom_line(data = p_subset, aes(x = x, y = conf.high), color = "grey40", size = 0.5, alpha = 0.3, linetype = "dotted")
}


# 5-year democracy change

unique_years <- unique(vdem_use$year)

# Add region variable
vdem_use$region <- as.factor(vdem_use$e_regionpol_6C)

# Create an empty plot
plot2 <- ggplot() +
  geom_blank() +  # Create an empty plot to initialize the ggplot object
  
  # Additional aesthetics and theme
  labs(x="Opposition Vote Share", y="Change in Democracy Score", title="Predicted 5-Year Change in Democracy", subtitle="", colour = "Opposition\nCoalition") + 
  coord_cartesian(ylim=c((-.16), .12)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic()

# Iterate over each unique country
for (i in unique_years) {
  # Subset the data by removing the current country
  subset_data <- vdem_use %>%
    filter(year != i)
  
  # Fit the linear model
  m <- lm(vdem_change_5yr ~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region, data = subset_data)
  
  # Get marginal effects for the subset data
  p_subset <- ggpredict(m, c("oppvote_restricted [all]"), ci.lvl = 0.95)
  
  # Add layer to the combined plot
  plot2 <- plot2 +
    geom_line(data = p_subset, aes(x = x, y = predicted), color = "grey40", size = 1, alpha = 0.5) +
    geom_line(data = p_subset, aes(x = x, y = conf.low), color = "grey40", size = 0.5, alpha = 0.3, linetype = "dotted") +
    geom_line(data = p_subset, aes(x = x, y = conf.high), color = "grey40", size = 0.5, alpha = 0.3, linetype = "dotted")
}


# Combine plots
year_jk <- grid.arrange(plot1, plot2, ncol = 2)

ggsave("CATK_year_jackknife.png", plot = year_jk, width = 900, height = 550, dpi = 96, units = "px")


### 1-7 Year Outcomes -----------------

# Code below produces Figure 13 in the Appendix

# Setup the dataset and factor for region
vdem$region <- as.factor(vdem$e_regionpol_6C)
vdem$vdem_change_1yr <- vdem$vdem_change
vdem_use <- as.data.frame(vdem[vdem$turnover_full == 0,])

# Create an initial plot object with the base aesthetics
plot_combined <- ggplot() + 
  coord_cartesian(ylim = c(-0.16, 0.12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Opposition Vote Share", y = "Change in Democracy Score", 
       title = "Predicted Yearly Changes in Democracy") +
  theme_classic()

# Grayscale colors from light gray to black
grayscale <- gray.colors(7, start = 0.8, end = 0.2, gamma = 2.2)

# Initialize an empty data frame for storing predictions
predictions <- data.frame()

# Loop over each year from 1 to 7 and add predictions to the plot
for (i in 1:7) {
  y_var_name <- paste0("vdem_change_", i, "yr")  # Dynamic y-variable name
  
  # Fit the model with the dynamically specified dependent variable
  formula <- as.formula(paste(y_var_name, "~ poly(oppvote_restricted, 3, raw = TRUE) + v2elintim + v2elirreg + v2cademmob_lag + e_gdppc + econ_growth + v2xel_frefair + exec + v2x_polyarchy_lag + region"))
  m <- lm(formula, data = vdem_use)
  
  # Predict values
  p <- ggpredict(m, terms = "oppvote_restricted [all]", ci.lvl = 0.95)
  p$outcome_year <- factor(rep(i, nrow(p)), labels = paste(i, "Year Change", sep = "-"))
  
  # Merge predicted data back to a single dataframe
  predictions <- rbind(predictions, p)
}

# Add lines and confidence intervals to the plot
plot_combined <- plot_combined +
  geom_line(data = predictions, aes(x = x, y = predicted, group = outcome_year, color = outcome_year), size = 1) +
  geom_line(data = predictions, aes(x = x, y = conf.low, group = outcome_year), linetype = "dotted", color = "grey40", size = 0.5) +
  geom_line(data = predictions, aes(x = x, y = conf.high, group = outcome_year), linetype = "dotted", color = "grey40", size = 0.5) +
  scale_color_manual(values = grayscale) +
  facet_wrap(~ outcome_year, ncol = 4) +
  theme(legend.position = "none")

# Save the combined plot
ggsave("CATK_OLS_altoutcomeyears.png", plot = plot_combined, width = 1100, height = 1100, dpi = 96, units = "px")


## GAM Degrees of Freedom ---------

# Code below produces Figure 12 in the Appendix

# Open PNG device
png("CATK_GAM_dfs.png", width = 600, height = 1400, res = 96)

# Setting up the plot area to arrange plots in 4 rows and 2 columns
par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))

# Iterate over k values from 3 to 6
for (k in 3:6) {
  # Set data
  vdem_use <- vdem[vdem$turnover_full == 0,]
  
  # 1-year ED change, all losing elections
  gam.1yr <- gam(vdem_change ~ s(oppvote_restricted, k=k, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)
  
  # 5-year ED change, all losing elections
  gam.5yr <- gam(vdem_change_5yr ~ s(oppvote_restricted, k=k, fx=TRUE, bs="tp") + v2elirreg + v2elintim + v2xel_frefair + v2cademmob_lag + exec + v2x_polyarchy_lag + econ_growth + e_gdppc + region, data = vdem_use)
  
  # Plotting the 1-year ED change GAM model
  plot(gam.1yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = sprintf("1-Year, %d EDF", k-1), bty = "l")
  abline(h = 0, col = "black")
  
  # Plotting the 5-year ED change GAM model
  plot(gam.5yr, rug = FALSE, se = TRUE, select = 1, ylab = "Partial effect of opposition performance", xlab = "Opposition Vote Share", shade = TRUE, xlim = c(0, 50), ylim = c(-0.18, 0.14), main = sprintf("5-Year, %d EDF", k-1), bty = "l")
  abline(h = 0, col = "black")
}

dev.off()
