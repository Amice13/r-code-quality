############################################################################################
#### Fig 2: (Top) Number of Unique Geo-Locating Users in China Posting in Chinese. 
#### (Bottom) The Fraction of Active Users Who Joined Twitter in the Last 30 Days.
#### data: twitter/data/a_CN_2019-12-01_2020-07-02_agg_fig2.csv
#### output: twitter/figs/Fig2_twitter_activity_geolocating_overall_counts_and_fraction.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("here")
agg = read_csv(here('twitter/data/a_CN_2019-12-01_2020-07-02_agg_fig2.csv'))

## Below is vestigial from when aggregation happened here.
# 
# source("Scripts/functions.R")
# tweets = read_csv("Data/tweets_raw/a_CN_2019-12-01_2020-07-02_cleaned.csv",
#                   col_types = cols(.default = "c"))## ,
# agg <- subset(tweets, as.Date(day) < "2020-07-01") %>%
#   filter(lang == "zh") %>%
#   mutate(
#     day = as.Date(
#       ## format(
#       as.POSIXct(as.numeric((timestamp_ms))/1000, origin="1970-01-01", tz="GMT") + 60 * 60 * 6
#       ## tz="Etc/GMT+6",
#       ## usetz=T
#     )## )## ,
#     ## day = as.Date(local_time_dt)
#   ) %>%
#   group_by(day, user.id) %>%
#   summarise(n = n(), daysSinceJoinedTwitter = mean(as.numeric(as.character(daysSinceJoinedTwitter)))) %>%
#   group_by(day) %>%
#   summarise(
#     days_on_twitter_year = mean(as.numeric(as.character(daysSinceJoinedTwitter))/365 < 1),
#     days_on_twitter_month = mean(as.numeric(as.character(daysSinceJoinedTwitter))/30 < 1),
#     days_on_twitter_15days = mean(as.numeric(as.character(daysSinceJoinedTwitter))/15 < 1),
#     n_users = n()
#   )

zh_dates <- as.Date(c("2020-01-24", "2020-03-13"))

pdf(here("twitter/figs/Fig2_twitter_activity_geolocating_overall_counts_and_fraction.pdf"), width = 6.5, height = 4)
par(mar=c(4, 6.5, 4, 3))
with(
  agg,
  plot(
    as.Date(day), n_users, type="l",
    bty="n", lwd=6, col = rgb(29, 161, 242, maxColorValue=255),
    xlab = "Date",
    ylab = "Number of geo-locating users\nin China posting in Chinese",
    cex.lab=1.75, cex.axis=1.5,
    ylim = c(min(n_users) - 150, max(n_users) + 170),
    yaxt="n"
  )
)
## abline(v = as.Date("2020-01-24"), col="red", lwd=4, lty=3)
abline(v = zh_dates, col=gray(0.5), lwd=4, lty=1)
pre_mean <- mean(subset(agg, as.Date(day) < "2020-01-01")$n_users)
contemp_mean <- mean(subset(agg, as.Date(day) == "2020-01-30")$n_users)
post_mean <- mean(subset(agg, as.Date(day) >= "2020-03-01")$n_users)
abline(h = pre_mean, col="black", lwd=4, lty=3)
points(
  x=as.Date("2020-01-30"),
  y = contemp_mean,
  col="black", cex = 2.5, pch=16
)
text(
  labels=paste0(round(contemp_mean / pre_mean, 1), "x"),
  x=as.Date("2020-01-30"),
  y = contemp_mean + 150,
  col="black", lwd=4, lty=3,
  las=2, adj=0.5, cex = 2
)
mtext(
  paste0(round(post_mean / pre_mean, 1), "x"),
  side=4,
  at = post_mean,
  col="black", lwd=4, lty=3,
  las=2, adj=0.25, cex=2
)
segments(
  y0=pre_mean,
  y1=pre_mean,
  x0=as.Date("2019-11-01"),
  x1=zh_dates[1]-1,
  col="black",
  lwd=4## , lty=3
)
segments(
  y0=post_mean,
  y1=post_mean,
  x0=zh_dates[2],
  x1=as.Date("2020-06-30"),
  col="black",
  lwd=4## , lty=3
)
mtext(
  text="Lockdown", side=3, at=mean(zh_dates), cex=1.25, col=gray(0.3)
)
theseq <- seq(500, 3000, 250)
axis(2, at=theseq, labels=theseq, las=1, cex.axis=1.3, hadj=0.5, tick=F)
##
with(
  agg,
  plot(
    as.Date(day), days_on_twitter_month, type="l",
    bty="n", lwd=6, col = rgb(29, 161, 242, maxColorValue=255),
    xlab = "Date",
    ylab = "Fraction of geo-locating users\nwho joined in last 30 days", ylim = c(0, 0.15),
    cex.lab=1.75, lty=1, cex.axis=1.5, yaxt="n"## ,
    ## xlim=c(as.Date("2019-12-01"), as.Date("2020-04-01"))
  )
)
## abline(v = as.Date("2020-01-28"), col="red", lwd=4, lty=3)
abline(v = zh_dates+30, col=gray(0.5), lwd=4, lty=1)
## abline(v = zh_dates[2]+30, col=gray(0.5), lwd=4, lty=3)
theseq <- seq(0, 0.15, 0.05)
axis(2, at=theseq, labels=format(theseq, nsmall = 2), las=1, cex.axis=1.3, hadj=0.5, tick=F)
##
pre_mean <- mean(subset(agg, as.Date(day) < "2020-01-01")$days_on_twitter_month)
contemp_mean <- mean(subset(agg, as.Date(day) == "2020-01-30")$days_on_twitter_month)
post_mean <- mean(subset(agg, as.Date(day) >= zh_dates[1] & as.Date(day) < zh_dates[2] + 30)$days_on_twitter_month)
abline(h = pre_mean, col="black", lwd=4, lty=3)
mtext(
  paste0(round(post_mean / pre_mean, 1), "x"),
  side=4,
  at = post_mean,
  col="black", lwd=4, lty=3,
  las=2, adj=0.25, cex=2
)
segments(
  y0=pre_mean,
  y1=pre_mean,
  x0=as.Date("2019-11-01"),
  x1=as.Date("2020-01-23"),
  col="black",
  lwd=4## , lty=3
)
segments(
  y0=post_mean,
  y1=post_mean,
  x0=zh_dates[1] + 30,
  x1=zh_dates[2] + 30,
  col="black",
  lwd=4## , lty=3
)
mtext(
  text="Lockdown\n+30 days", side=3, at=mean(zh_dates)+30, cex=1.25, col=gray(0.3)
)
dev.off()
