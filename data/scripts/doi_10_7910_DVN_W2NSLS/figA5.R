############################################################################################
#### Fig A5: Excess Followers, absolute (top) and ratio normalized by category 
#### growth rate (bottom).  Growth rate is calculated based on the December 2019 
#### average number of new followers by category.
#### data: twitter/data/followers_daily_change.csv
#### outputs: twitter/figs/FigA5_top.png
####          twitter/figs/FigA5_bottom.png
############################################################################################

# The purpose of this script is to analyze by day how many surplus followers are caused by the lockdown.

##########################
##
##	GLOBALS
##
##########################
rm(list=ls())
library(dplyr)
library(ggplot2)
library(here)

start <- '2020-01-23'
end <- "2020-03-13"

##########################
##
##	WORK
##
##########################
df <- read.csv(here('twitter/data/followers_daily_change.csv'), stringsAsFactors=FALSE)

df$type_complete <- gsub(' Accounts', '', df$type_complete)

df2 <- df[grep(paste('China', sep='|'), df$location_classified),]

df2 <- data.frame(df2 %>% group_by(type_complete, location_classified) %>% mutate(day=1:n(), followers_predicted=day*dec_mean, followers_new=cumsum(followers), followers_surplus=followers_new-followers_predicted))

# Add normalized measures
df2 <- data.frame(df2 %>% group_by(type_complete, location_classified) %>% mutate(norm_dec_mean = dec_mean/dec_mean, norm_followers = followers/dec_mean, norm_followers_new = cumsum(norm_followers), norm_followers_predicted=day*norm_dec_mean, norm_followers_surplus =norm_followers_new-norm_followers_predicted))

df2$followers_ratio <- df2$followers_new/df2$followers_predicted



ymax <- 55000
ggplot(df2[df2$location_classified=='China',], aes(x=as.Date(follow_date), y=followers_surplus, color=type_complete)) + geom_line() + theme_classic() + geom_vline(xintercept=as.Date(start), lty='dotted') + annotate('text', x=as.Date(start)+1, y=ymax-2000, label = 'Wuhan Lockdown Through End', hjust=0) + xlab('') + ylab('Excess Followers from China') + annotate('rect', xmin=as.Date(start), xmax=as.Date(end), ymin=0, ymax=ymax, alpha=.25) + theme(legend.title=element_blank()) + scale_y_continuous(breaks=c(10000,20000,30000,40000,50000,55000)) + theme(legend.position='bottom', legend.box='vertical', legend.margin=margin()) + guides(colour = guide_legend(nrow = 3))
ggsave(here('twitter/figs/FigA5_top.png'), plot=last_plot(), width=7, height=5)


ymax <- 4
ggplot(df2[df2$location_classified=='China',], aes(x=as.Date(follow_date), y=followers_ratio, color=type_complete)) + geom_line() + theme_classic() + geom_vline(xintercept=as.Date(start), lty='dotted') + annotate('text', x=as.Date(start)+1, y=ymax-.1, label = 'Wuhan Lockdown Through End', hjust=0) + xlab('') + ylab('Followers/Expected Followers from China') + annotate('rect', xmin=as.Date(start), xmax=as.Date(end), ymin=0, ymax=ymax, alpha=.25) + theme(legend.title=element_blank()) + scale_y_continuous(breaks=c(1, 2, 3, 4)) + theme(legend.position='bottom', legend.box='vertical', legend.margin=margin()) + guides(colour = guide_legend(nrow = 3))
ggsave(here('twitter/figs/FigA5_bottom.png'), plot=last_plot(), width=7, height=5)


## EXPLORE, DESCRIPTIVE STATISTICS
max(df2$followers_surplus[df2$type_complete=='Citizen Journalists / Political Bloggers'])
max(df2$followers_surplus[df2$type_complete=='International News Agencies'])

max(df2$followers_ratio[df2$type_complete=='Citizen Journalists / Political Bloggers'])
max(df2$followers_ratio[df2$type_complete=='Activists or US / Taiwan / Hong Kong Politics'])
max(df2$followers_ratio[df2$type_complete=='International News Agencies'])


max(df2$norm_followers_surplus[df2$type_complete=='Citizen Journalists / Political Bloggers'])
max(df2$norm_followers_surplus[df2$type_complete=='Activists or US / Taiwan / Hong Kong Politics'])
max(df2$norm_followers_surplus[df2$type_complete=='International News Agencies'])

subset(df2, follow_date==end)
