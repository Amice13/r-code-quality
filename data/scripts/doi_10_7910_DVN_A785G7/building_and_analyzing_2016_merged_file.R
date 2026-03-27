# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

# all scripts in the replication materials assume bigKRLS 3.0.0 or higher
require(pacman)                   
p_load(bigKRLS, update = TRUE)
p_load(ggplot2, stringr, maps, dplyr)

###############################
# update paths as appropriate #
###############################

data <- read.csv('2016_election_dataset.csv')
fit_bigKRLS_folder <- '2016_election_ests'


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
X <- as.matrix(X)

# full model - uncomment to estimate
#out <- bigKRLS(gop_2016_delta, X, eigtrunc=0)
load.bigKRLS(fit_bigKRLS_folder, newname = 'out')

# AME table
library(knitr)
labels <- c(c('Age-Adjusted Mortality',
              'Change in 3-Year Mortality',
              'Unemployment',
              'Urban-Rural Continuum',
              'Age',
              'Household Income',
              'Poverty',
              'High School Dropout',
              'High School Graduate',
              'Some College',
              'College Graduate',
              'Proportion White',
              'Proportion Latino',
              'Proportion Black',
              'Proportion Asian'),
            colnames(X)[16:67])
kable(summary(out, labs=labels)[[1]][1:15,], digit=5)

# figure size: 5.5x4
# plotting non-linear effects - k=1 gives the in-text mortality-mortality interaction plot
k <- 1
print(colnames(X)[k])
ggplot(NULL) + geom_point(aes(as.numeric(X[,k]), y=out$derivatives[,k]), alpha = 1, size=.1, color='grey') + 
  geom_line(aes(as.numeric(x=X[X[,k] < 12 & X[,k] > 4.5,k]), y=out$derivatives[X[,k] < 12 & X[,k] > 4.5, k]), 
            method='loess', alpha=0.5, color='grey10', stat='smooth', size=1) + 
  xlab('Mortality') + 
  ylab('Marginal Derivative of Mortality') + 
  geom_rug(aes(as.numeric(X[,k])), sides='b', alpha=0.05) + 
  geom_rug(aes(y=out$derivatives[,k]), sides='l', alpha=0.05) +
  geom_hline(aes(yintercept=0), linetype='dashed', alpha=0.5) +
  theme_minimal() +
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  coord_cartesian(ylim=c(-0.4, 0.4))

# interacting effects with %white - k=1 gives the in-text mortality-white interaction plot
k <- 1
ggplot(NULL) + geom_point(aes(as.numeric(X[,12]) ,y=out$derivatives[,k]), alpha = 1, size=.1, color='grey') + 
  geom_line(aes(as.numeric(x=X[X[,12] > 25,12]), y=out$derivatives[X[,12] > 25,k]), 
            method='loess', se=FALSE, alpha=0.5, size=1, stat='smooth', color='grey10') + 
  xlab('Percent White') + 
  ylab('Marginal Derivative of Mortality') + 
  geom_rug(aes(as.numeric(X[,12])), sides='b', alpha=0.05) + 
  geom_rug(aes(y=out$derivatives[,k]), sides='l', alpha=0.05) +
  geom_hline(aes(yintercept=0), linetype='dashed', alpha=0.5) +
  theme_minimal() + 
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  coord_cartesian(ylim=c(-0.4, 0.4))

# creating a counterfactual dataset to predict from - here, we increase mortality by the 0.95th percentile to 0.05th percentile gap (~3sd)
j <- 1

to_pred <- X
to_pred[,j] <- to_pred[,j] + quantile(X[,j], 0.95) - quantile(X[,j], 0.05)
p <- predict(out, to_pred, se.pred=TRUE)

# assessing significance
p_delta <- p$predicted - out$yfitted
se_p_delta <- sqrt((p$se.pred^2 + diag(out$vcov.est.fitted[])))

tstats <- p_delta/se_p_delta
p_vals <- 2*pt(abs(tstats), out$Neffective, lower.tail=FALSE)

signif <- p_vals < 0.05
sum(signif)
sum(p.adjust(p_vals, method='BH') < 0.05)
adj_p_vals <- p.adjust(p_vals, method='BH')
adj_signif <- adj_p_vals < 0.05

adj_high_low <- ifelse(adj_signif,
                       ifelse(p_delta < 0, 'Negative, Significant', 'Positive, Significant'),
                       'Insignificant')

# significance map
geo_df_signif <- data.frame('region'=character(), 'subregion'=character(), 'signif'=character(), stringsAsFactors = F)
for(m in 1:nrow(X)){
  geo_df_signif <- rbind(geo_df_signif,
                         data.frame('region'= data[complete,]$state[m],
                                    'fips'=data[complete,]$fips[m],
                                    'signif'=adj_high_low[m]))
}

geo_df_signif$region <- state.name[match(geo_df_signif$region,state.abb)]
geo_df_signif$region <- tolower(geo_df_signif$region)

fips <- cbind(county.fips, matrix(unlist(strsplit(as.character(county.fips$polyname), ',')), ncol=2, byrow=T))
names(fips) <- c('fips', 'polyname', 'region', 'subregion')
counties_geo <- map_data("county")
states_geo <- map_data("state")
counties_geo <- merge(counties_geo, fips, by=c('region', 'subregion'))

map_df_signif <- merge(counties_geo,geo_df_signif, by=c("region", "fips"), all.x=T)
map_df_signif <- map_df_signif[order(map_df_signif$order),]
ggplot(map_df_signif, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=signif))+
  scale_fill_manual(values=c('grey90', 'red', 'blue'), guide = guide_legend(title='Mortality Coefficient')) +
  geom_path(size=.05) + 
  coord_map() + theme_minimal() + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin=unit(c(-2, 0, -2, 0),units = 'line')) + geom_path(colour = "white", data = states_geo) 



# colored coefficient map
geo_df <- data.frame('region'=character(), 'subregion'=character(), 'change'=numeric(), stringsAsFactors = F)
for(m in 1:nrow(X)){
  geo_df <- rbind(geo_df,
                  data.frame('region'= data[complete,]$state[m],
                             'fips'=data[complete,]$fips[m],
                             'change'=p_delta[m]))
}

geo_df$region <- state.name[match(geo_df$region,state.abb)]
geo_df$region <- tolower(geo_df$region)

fips <- cbind(county.fips, matrix(unlist(strsplit(as.character(county.fips$polyname), ',')), ncol=2, byrow=T))
names(fips) <- c('fips', 'polyname', 'region', 'subregion')
counties_geo <- map_data("county")
states_geo <- map_data("state")
counties_geo <- merge(counties_geo, fips, by=c('region', 'subregion'))


map.df <- merge(counties_geo,geo_df, by=c("region", "fips"), all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=change))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", guide = guide_legend(title='Mortality'))+
  coord_map() + theme_minimal() + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin=unit(c(-2, 0, -2, 0),units = 'line')) + geom_path(colour = "white", data = states_geo) 

# symbolic coefficient map
symbol_df <- data.frame('lat'=character(), 'long'=character(), 'change'=numeric(), stringsAsFactors = F)
states_geo <- map_data("state")

for(m in 1:nrow(X)){
  symbol_df <- rbind(symbol_df,
                    data.frame('lat'= data[complete,]$lat[m],
                               'long'=data[complete,]$lon[m],
                               'change'=p_delta[m]))
}

ggplot(symbol_df, aes(x=long,y=lat)) +
  geom_path(aes(group=group), colour = "grey90", data = states_geo) + 
  geom_point(aes(size=abs(change)^2, shape=as.factor(sign(change)), color=as.factor(sign(change)), fill=as.factor(sign(change)))) + 
  scale_size_continuous(range=c(0.01, 2), guide=FALSE) +
  scale_shape_manual(values=c(25, 24),
                     breaks=c('-1', '1'),
                     labels=c('Negative', 'Positive')) + 
  scale_color_manual(values=c('black', 'grey60'),
                     breaks=c('-1', '1'),
                     labels=c('Negative', 'Positive')) + 
  scale_fill_manual(values=c('black', 'grey60'),
                    breaks=c('-1', '1'),
                    labels=c('Negative', 'Positive')) +
  labs(shape='Estimate', fill='Estimate', color='Estimate') + 
  coord_map() + theme_bw() + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin=unit(c(-17.5, 0, -20, -15),units = 'line'))

