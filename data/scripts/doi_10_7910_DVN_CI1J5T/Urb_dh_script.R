# This file is written for R.  It assumes that the working directory contains the accompanying data files 
# (seatpop_fig1.csv and normherfs_fig2.csv).

# Load necessary packages

library(ggplot2)    # A graphics package, used for the "qplot()" commands below
library(reshape2)   # A data-management package, used for the "melt()" commands below

# Figure 1: Seat-population data (population figures given in country-level statistical yearbooks before 1950, 
#and the U.S. Census Bureau's International Data Base from 1950 on)

seatpops <- read.csv("C:\\Users\\Daniel\\Dropbox\\Journal (II)\\Replication Files\\Urbastch\\seatpop_fig1.csv")     # Bring the data about country population and parliament sizes into R
seatpops$US <- 100*(seatpops$US_pop/seatpops$US_seat)/(seatpops$US_pop[1]/seatpops$US_seat[1])      # Generate seat/population ratio for United States, normalize to index
seatpops$UK <- 100*(seatpops$UK_pop/seatpops$UK_seat)/(seatpops$UK_pop[1]/seatpops$UK_seat[1])      # Generate seat/population ratio for United Kingdom, normalize to index
seatpops$CA <- 100*(seatpops$CA_pop/seatpops$CA_seat)/(seatpops$CA_pop[1]/seatpops$CA_seat[1])      # Generate seat/population ratio for Canada, normalize to index
seatpops$AUS <- 100*(seatpops$AUS_pop/seatpops$AUS_seat)/(seatpops$AUS_pop[1]/seatpops$AUS_seat[1])      # Generate seat/population ratio for Australia, normalize to index
seatpops <- seatpops[, c(1, 10:13)]            # Drop raw data; retain index values
seatpoplong <- melt(seatpops, id = 1)             # Reshape data from wide to long format

qplot(Year, value, data = seatpoplong, linetype = variable, geom="line", ylab="Residents-per-Legislator Index") + theme_minimal() + theme(legend.position="none") + geom_text(label=seatpoplong$variable, alpha = as.numeric(seatpoplong$Year==2015), size = 3, hjust = -0.2)  # Graph, with various aesthetic requests


# Figure 2 (employment figures derived from the U.S. Census Bureau's County Business Patterns database)

normherfs <- read.csv("C:\\Users\\Daniel\\Dropbox\\Journal (II)\\Replication Files\\Urbastch\\normherfs_fig2.csv")     # Bring the data about state-level economic heterogeneity into R
normherflong <- melt(normherfs, id = "State")       # Reshape data from wide to long format
normherflong$year <- 1946+10*as.numeric(normherflong$variable)   # Convert variable-name factor into associated numeric year

qplot(data = normherflong, year, value, color=State, geom="line", ylab="Normalized Industrial Concentration (log scale)", xlab="Year") + theme_minimal() + theme(legend.position = "none") + scale_colour_grey(start=0.2, end=0.2) + scale_y_log10(breaks=1:4) + scale_x_continuous(breaks=seq(1956, 2006, by=10)) + geom_text(label=normherflong$State,alpha=as.numeric(normherflong$year==1956),colour="dimgrey",size=3,hjust=1) + geom_text(label=normherflong$State,alpha=as.numeric(normherflong$year==2006),colour="dimgrey",size=3,hjust=0) + expand_limits(x = c(1952,2010))   # Graph, with various aesthetic requests


# Table 1

tab1data <- subset(normherflong, value > 2) # Extract entries where phi > 2
tab1data$State <- factor(tab1data$State)  # Remove states not seen in subset
tab1.firstyear <- t(do.call(rbind, list(by(tab1data$year, tab1data$State, min)))) # Get each state's first phi > 2 year
tab1.lastyear <- t(do.call(rbind, list(by(tab1data$year, tab1data$State, max)))) # Get each state's last phi > 2 year
tab1.maxphi <- t(do.call(rbind, list(by(tab1data$value, tab1data$State, max)))) # Get each state's maximum of phi
tab1.maxyear <- NULL  # Initialize variable

for (i in 1:length(tab1.maxphi)) {  # Run a loop over each value of the maximum-phi vector
  tab1.maxyear[i] <- tab1data[tab1data[,3]==tab1.maxphi[i],4]  # Find year with maximum phi for each state
}      # Close the loop

tab1 <- data.frame(tab1.firstyear, tab1.lastyear, round(tab1.maxphi,2), tab1.maxyear)  # Combine variables into single data frame
names(tab1) <- c("firstyear","lastyear","maxphi","maxyear")     # Name variables more meaningfully
tab1[order(-tab1$maxphi), ]  # Display in descending order of maxphi; note that Nevada only appears once when not specifying largest industry


# Other data-based claims in the text

# "If the total number of economic interests in the US had increased proportionally with the population between 1930 and 2000, the relative concentration of the typical legislative district to that of the country as a whole should have declined by 56.3%"

100*(1 - subset(seatpoplong, variable == "US" & Year==1930)$value/subset(seatpoplong, variable == "US" & Year==2000)$value) # Proportional change in population between 1930 and 2000

# "A simple statistical test bears this out: the average state concentration level in 1956 (1.66) is larger than that in 2006 (1.11) with a t-statistic of 6.24."

t.test(x = subset(normherflong, year==1956)$value, y = subset(normherflong, year==2006)$value, paired = TRUE)  # A paired t test comparing the sector-of-employment concentrations of states in 1956 with the analogous figure in 2006

# "Taking the logarithm of the concentration ratios to alleviate skew increases the t-statistic on the difference of means to 9.12."

t.test(x = log(subset(normherflong, year==1956)$value), y = log(subset(normherflong, year==2006)$value), paired = TRUE)  # A paired t test comparing the natural logarithm of sector-of-employment concentrations of states in 1956 with the analogous figure in 2006






