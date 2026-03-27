# This R-file is generating the table SI-3

library(xtable)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

load("Data/newspapers.rdata")

newspapers$front_pages$year = as.numeric(substr(newspapers$front_pages$date, 1,4))

years = 2008:2015

comparing_slants = data.frame("year" = rep(NA, length(years)),
                              "mean_yediot" = rep(NA, length(years)),
                              "mean_ih" = rep(NA, length(years)),
                              "pct_change" = rep(NA, length(years)))

for(i in 1:length(years)){
  comparing_slants$year[i] = years[i]
  comparing_slants$mean_yediot[i] = mean(newspapers$front_pages$score_normalized[newspapers$front_pages$ih==0 & newspapers$front_pages$year==years[i]])
  comparing_slants$mean_ih[i] = mean(newspapers$front_pages$score_normalized[newspapers$front_pages$ih==1 & newspapers$front_pages$year==years[i]])
  comparing_slants$pct_change[i] = 100*((comparing_slants$mean_ih[i]-comparing_slants$mean_yediot[i])/comparing_slants$mean_yediot[i])
}


print(xtable(comparing_slants, type = "latex", digits=2), file = "Tables/Table_SI-3.tex")
