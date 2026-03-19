# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

# all scripts in the replication materials assume bigKRLS 3.0.0 or higher
require(pacman) 
p_load(bigKRLS, update = TRUE)
p_load(ggplot2, stringr, maps, dplyr, knitr)

if("y_gop_2016_delta.csv" %in% dir()){
  
  gop_2016_delta <- as.matrix(read.csv("y_gop_2016_delta.csv"))
  X <- as.matrix(read.csv("X_2016.csv"))
  
}else{
  message("Please set your working directory to 2016_election_application.")
}

# full model
out <- bigKRLS(gop_2016_delta, X, eigtrunc=0)

# AME table
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



# generating the coefficient map
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
  geom_path(size=.05) + 
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
