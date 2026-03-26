## Eszter Hargittai & Aaron Shaw
## 2019
## 03-gen_descriptive_stats.R

## Description of this file:
## Generates descriptive statistics reported in Table 1 of Hargittai &
## Shaw, 2019

setwd("~/research/web_use_US_survey/mturk_norc_comparison/analysis/")

if(!exists("d")){
    d <- readRDS("Hargittai-Shaw-AMT-NORC-2019.rds")
}

### Group descriptive stats
group.sums <- function(var.name, data=d, dig=1){
    print(var.name)
    norc <- d[d$amtsample == 0,]
    amt <- d[d$amtsample == 1,]
    norc.mean <- round(mean(norc[,var.name], na.rm=T), dig)
    amt.mean <- round(mean(amt[, var.name], na.rm=T), dig)
    norc.sd <- round(sd(norc[,var.name], na.rm=T), 1)
    amt.sd <- round(sd(amt[, var.name], na.rm=T), 1)
    norc.len <- sum(!is.na(norc[,var.name]))
    amt.len <- sum(!is.na(amt[,var.name]))
    norc.out <- data.frame("source" = "norc",
                           "mean" = norc.mean,
                           "sd" = norc.sd,
                           "len" = norc.len)
    amt.out <- data.frame("source" = "amt",
                          "mean" = amt.mean,
                          "sd" = amt.sd,
                          "len" = amt.len)
    return(rbind(norc.out, amt.out))
}

### Background attributes
## continuous measures:
cont.vars <- c("age","incomediv")
lapply(cont.vars, group.sums)

for(v in cont.vars){
    print(v)
    print(t.test(d[,v] ~ d$amtsample))
}

### categorical measures:
cat.vars <- c("female", "employed", "rural", "eduhsorless", "edusc",
              "edubaormore", "white", "hispanic", "black", "asian",
              "nativeam", "raceother")

lapply(cat.vars, group.sums, dig=2) ## note 2 sig. digits for proportions

for(v in cat.vars){
    print(v)
    print(t.test(d[,v] ~ d$amtsample))
}

### internet use
dvs.internet <- c("accesssum", "webweekhrs", "skillsmean",
                  "snssumcompare", "do_sum")

lapply(dvs.internet, group.sums)

for(v in dvs.internet){
    print(v)
    print(t.test(d[,v] ~ d$amtsample))
}

### prosociality
dvs.prosocial <- c("trust_scale", "altru_scale", "pts_give")

lapply(dvs.prosocial, group.sums)

for(v in dvs.prosocial){
    print(v)
    print(t.test(d[,v] ~ d$amtsample))
    }

