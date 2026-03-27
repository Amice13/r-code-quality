# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

###########################################
# getting data - change path as necessary #
###########################################

data <- read.csv('~/Dropbox/pa_replication/2016_election_application/2016_election_dataset.csv')
out_path <- '~/Dropbox/pa_replication/Appendix_C/Xsmall.RData'

# data preparation
data$dem_2016_percent <- 100*data$dem_2016/(data$dem_2016 + data$gop_2016)
data$gop_2016_percent <- 100*data$gop_2016/(data$dem_2016 + data$gop_2016)
data$dem_2012_percent <- 100*data$dem_2012/(data$dem_2012 + data$gop_2012)
data$gop_2012_percent <- 100*data$gop_2012/(data$dem_2012 + data$gop_2012)

data$white_population[is.na(data$white_population)] <- 0
data$latino_population[is.na(data$latino_population)] <- 0
data$black_population[is.na(data$black_population)] <- 0
data$asian_population[is.na(data$asian_population)] <- 0

data$percent_white <- 100*data$white_population/data$total_population
data$percent_latino <- 100*data$latino_population/data$total_population
data$percent_black <- 100*data$black_population/data$total_population
data$percent_asian <- 100*data$asian_population/data$total_population

data$all_mortality_2009.2011 <- data$all_mortality_2009.2011/100
data$all_mortality_2013.2015 <- data$all_mortality_2013.2015/100

data$all_2013.2015_despair_mortality <- data$all_2013.2015_despair_mortality/100

data$mortality_delta <- data$all_mortality_2013.2015 - data$all_mortality_2009.2011

data$gop_2016_delta <- data$gop_2016_percent - data$gop_2012_percent

data$percent_poverty <- 100*data$POVALL_2015/data$total_population

data$Median_Household_Income_2015 <- data$Median_Household_Income_2015/10000
data$AGE050210D <- data$AGE050210D/10

# creating the model preliminaries
X <- data.frame('all_mortality' = data$all_mortality_2013.2015)
X$mortality_delta <- data$mortality_delta

#X <- data.frame('despair_mortality' = data$all_2013.2015_despair_mortality)

X$unemployment <- data$Unemployment_rate_2015
X$rural <- data$Rural.urban_Continuum_Code_2013

X$age <- data$AGE050210D
X$income <- data$Median_Household_Income_2015
X$poverty <- data$percent_poverty

X$high_school_dropout <- data$Percent.of.adults.with.less.than.a.high.school.diploma..2011.2015
X$high_school_grad <- data$Percent.of.adults.with.a.high.school.diploma.only..2011.2015
X$some_college <- data$Percent.of.adults.completing.some.college.or.associate.s.degree..2011.2015
X$college_grad <- data$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2011.2015

X$percent_white <- data$percent_white
X$percent_latino <- data$percent_latino
X$percent_black <- data$percent_black
X$percent_asian <- data$percent_asian

X$lat <- data$lat
X$lon <- data$lon

# alaska is the excluded category
states <- model.matrix(~data$state)[,2:51]
colnames(states) <- sort(unique(data$state))[2:51]

X <- cbind(X, states)

# fitting the model
gop_2016_delta <- data$gop_2016_delta
complete <- complete.cases(X) & !is.na(gop_2016_delta)
gop_2016_delta <- gop_2016_delta[complete]
X <- X[complete,]

# mapping states to census divisions
census <- read.csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv", 
                   stringsAsFactors = FALSE)

state <- colnames(X)[18:67][apply(X[,18:67], 1, function(x) which(x == 1))]

division <- census$Division[match(state, census$State.Code)]
r <- matrix(nrow=nrow(X), ncol=length(unique(census$Division)), 0)
r[cbind(1:nrow(r), match(division, unique(census$Division)))] <- 1
X <- cbind(X[,1:17], r)
colnames(X)[18:ncol(X)] <- unique(census$Division)

# subset to the variables used in simulations
Xsmall <- X[,c('all_mortality', 'unemployment', 'rural', 'age', 'income', 'poverty', 'college_grad', 'percent_white',
             'Pacific', 'East South Central', 'West South Central', 'Mountain', 'New England', 'South Atlantic',
             'West North Central', 'East North Central', 'Middle Atlantic')]

Xsmall <- as.matrix(Xsmall)

save(Xsmall, file=out_path)
