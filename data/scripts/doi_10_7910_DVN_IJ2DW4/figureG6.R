rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(ggplot2)
library(zoo)
library(lubridate)

## load data
load('./data/crime/hate_crime.RDS')

### Figure G6: Is there a decay in hate crime in minority barely lost constituencies?
# use optimal bandwidth from main analysis
estimates <- readRDS('./output/estimates/crime_estimates.RDS')
bw <- estimates[month==9 & cov=='yes', bw]

ids <- hate_crimes_w_at_9m[,.(election,month,PCON17CD)]
values15 <- (interval(as.yearmon('2015-05'), as.yearmon(hate_crimes_w_at_9m$month, format='%m-%Y')) %/% months(1))+1
values17 <- (interval(as.yearmon('2017-06'), as.yearmon(hate_crimes_w_at_9m$month, format='%m-%Y')) %/% months(1))+1
values19 <- (interval(as.yearmon('2019-12'), as.yearmon(hate_crimes_w_at_9m$month, format='%m-%Y')) %/% months(1))+1
values <- cbind(values15,values17,values19,ids)
values[election==2015, month_post_election:=values15]
values[election==2017, month_post_election:=values17]
values[election==2019, month_post_election:=values19]
hate_crimes_w_at_9m <- merge(hate_crimes_w_at_9m, values[,.(election, month, PCON17CD, month_post_election)], by=c('election', 'month', 'PCON17CD'))
hate_crimes_post <- hate_crimes_w_at_9m
crimes_post <- hate_crimes_post[(victory_margin<0 & victory_margin>-bw), .(crime_rate=mean(crime_rate)), by=.(month_post_election)]

# crime before election
load('./data/crime/hate_crime_before_election.RDS')
ids <- hate_crimes_w_at_9m[,.(election,month,PCON17CD)]
values15 <- (interval(as.yearmon(hate_crimes_w_at_9m$month, format='%m-%Y'), as.yearmon('2015-05')) %/% months(1))
values17 <- (interval(as.yearmon(hate_crimes_w_at_9m$month, format='%m-%Y'), as.yearmon('2017-06')) %/% months(1))
values19 <- (interval(as.yearmon(hate_crimes_w_at_9m$month, format='%m-%Y'), as.yearmon('2019-12')) %/% months(1))
values <- cbind(values15,values17,values19,ids)
values[election==2015, month_post_election:=values15]
values[election==2017, month_post_election:=values17]
values[election==2019, month_post_election:=values19]
hate_crimes_w_at_9m <- merge(hate_crimes_w_at_9m, values[,.(election, month, PCON17CD, month_post_election)], by=c('election', 'month', 'PCON17CD'))
hate_crimes_pre <- hate_crimes_w_at_9m
hate_crimes_pre[,month_post_election:=(-1)*month_post_election]
crimes_pre <- hate_crimes_pre[(victory_margin<0 & victory_margin>-bw), .(crime_rate=mean(crime_rate)), by=.(month_post_election)]

ggplot(rbind(crimes_pre,crimes_post), aes(x=month_post_election, y=crime_rate)) +
  geom_point() +
  geom_vline(xintercept=0, linetype='longdash', size=0.2) +
  geom_segment(aes(x=-9,xend=0,y=crimes_pre[,mean(crime_rate)],yend=crimes_pre[,mean(crime_rate)])) +
  geom_segment(aes(x=0,xend=10,y=crimes_post[,mean(crime_rate)],yend=crimes_post[,mean(crime_rate)])) +
  scale_x_continuous(breaks = c(-9:10)) +
  xlab('Number of months around election') +
  ylab(paste("Average hate crimes per 1000 residents",
             "in constituencies with minority defeats",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureG6.pdf', width=6, height=4.85)
