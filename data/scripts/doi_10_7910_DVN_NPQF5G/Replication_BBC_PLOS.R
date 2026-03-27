#################################################
# Project:
#   Event Dependence in Death Penalty Executions
#
# File: 
#   EDDPE_Replication.R
#
# Date Last Edited:
#   12.14.2017
#
# Author:
#   Benjamin Campbell
#
# Purpose:
#   Replicate analysis
#
##################################################

#############################
### Distributional Form Rep
#############################
# Clean everything up
rm(list=ls())
options(stringsAsFactors = FALSE)

library(readstata13)
library(rio)
exec <- read.dta13('execs-by-date-by-county.dta')

exec <- na.omit(exec)

counties <- unique(exec$fips)

exec_by_county_df <- data.frame()
for(i in 1:length(counties)){
  temp <- exec[exec$fips == counties[i],]
  exec_by_county <- data.frame(county_fips = counties[i],
                               n_exec = nrow(temp))
  exec_by_county_df <- rbind(exec_by_county_df, exec_by_county)
}

exec_n <- sort(unique(exec_by_county_df$n_exec))
n_counties_by_exec_n <- data.frame()
for(i in 1:length(exec_n)){
  n_execs <- exec_n[i]
  n_counties <- nrow(exec_by_county_df[which(exec_by_county_df$n_exec == n_execs),])
  temp <- data.frame(number_executions = n_execs,
                     number_counties = n_counties)
  n_counties_by_exec_n <- rbind(n_counties_by_exec_n, temp)
}

cens <- read.dta13('Census_Judicial_DP_Master_782015.dta')
all_counties <- unique(cens$geo_fips)
no_execs <- setdiff(all_counties, counties)

n_counties_by_exec_n <- rbind(c(0, length(no_execs)), n_counties_by_exec_n)

for(i in 2:nrow(n_counties_by_exec_n)){
  n_counties_by_exec_n[i,2] <- sum(n_counties_by_exec_n$number_counties[i:nrow(n_counties_by_exec_n)])
}

county_dat <- import("Gaz_counties_national.txt")
county_dat <- county_dat[county_dat$USPS != "AK",]
county_dat <- county_dat[county_dat$USPS != "HI",]
colnames(exec_by_county_df) <- c("GEOID", "execs")
county_dat_merged <- merge(county_dat, exec_by_county_df, by = "GEOID", all.x = TRUE)

states <- map_data("state")

# let's bring in murders by counties
murders_by_county_df <- data.frame()
for(i in 1:length(all_counties)){
  temp <- cens[cens$geo_fips == all_counties[i],]
  murders_by_county <- data.frame(GEOID = all_counties[i],
                                  n_murders = sum(temp$homicides, na.rm = TRUE))
  # Now standardize this by year
  murders_by_county$n_murders <- murders_by_county$n_murders/38
  
  # standardize by 100000 residents
  murders_by_county$n_murders <- (murders_by_county$n_murders)/mean(temp$totalpop, na.rm = TRUE)*100000
  
  murders_by_county_df <- rbind(murders_by_county_df, murders_by_county)
}

murder_dat_merged <- merge(county_dat, murders_by_county_df, by = "GEOID", all.x = TRUE)

murdmap <- ggplot() + 
  geom_polygon(aes(x = states$long, y = states$lat, group = states$group), color = "white", lwd = 0.25) + 
  geom_point(aes(x = murder_dat_merged$INTPTLONG, y = murder_dat_merged$INTPTLAT, size = murder_dat_merged$n_murders, colour = murder_dat_merged$n_murders), alpha = 0.75) +
  #  geom_point(aes(x = county_dat_merged$INTPTLONG, y = county_dat_merged$INTPTLAT, size = county_dat_merged$execs), colour = "orange", alpha = 0.75, show.legend = TRUE) +
  coord_fixed(1.3) +
  #guides(fill=FALSE) +
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.87, 0.45),
        plot.margin = unit(c(-3, 0, -7, 0), "cm"),
        legend.text = element_text(size=8),
        legend.key.size =  unit(0.20, "in"),
        legend.title = element_text(size=8)) +
  scale_colour_gradient("Murders", low = "dodgerblue", high = "firebrick1") +
  guides(size=FALSE) 

pdf("Figure1A.pdf")
murdmap
dev.off()

# Now I need to go through and standardize execs
county_dat_merged_new <- data.frame()
for(i in 1:nrow(county_dat_merged)){
  row <- county_dat_merged[i,]
  cens_temp <- cens[cens$geo_fips == row$GEOID,]
  avg_pop <- mean(cens_temp$totalpop, na.rm = TRUE)
  row$execs <- (row$execs/avg_pop)*100000
  county_dat_merged_new<-rbind(county_dat_merged_new, row)
}
county_dat_merged<-county_dat_merged_new

execmap <- ggplot() + 
  geom_polygon(aes(x = states$long, y = states$lat, group = states$group), color = "white", lwd = 0.25) + 
  #  geom_point(aes(x = murder_dat_merged$INTPTLONG, y = murder_dat_merged$INTPTLAT, size = murder_dat_merged$rescaled, colour = murder_dat_merged$rescaled), alpha = 0.75) +
  geom_point(aes(x = county_dat_merged$INTPTLONG, y = county_dat_merged$INTPTLAT, size = county_dat_merged$execs, colour = county_dat_merged$execs), alpha = 0.75, show.legend = TRUE) +
  coord_fixed(1.3) +
  #guides(fill=FALSE) +
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.87, 0.45),
        plot.margin = unit(c(-3, 0, -7, 0), "cm"),
        legend.text = element_text(size=8),
        legend.key.size =  unit(0.20, "in"),
        legend.title = element_text(size=8)) +
  scale_colour_gradient("Executions", low = "dodgerblue", high = "firebrick1") +
  guides(size=FALSE) 

pdf("Figure1B.pdf")
execmap
dev.off()

# Panel A and B for Murder and Exec Maps

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



murdmapA <- ggplot() + 
  geom_polygon(aes(x = states$long, y = states$lat, group = states$group), color = "white", lwd = 0.25) + 
  geom_point(aes(x = murder_dat_merged$INTPTLONG, y = murder_dat_merged$INTPTLAT, size = murder_dat_merged$n_murders, colour = murder_dat_merged$n_murders), alpha = 0.75) +
  #  geom_point(aes(x = county_dat_merged$INTPTLONG, y = county_dat_merged$INTPTLAT, size = county_dat_merged$execs), colour = "orange", alpha = 0.75, show.legend = TRUE) +
  coord_fixed(1.3) +
  #guides(fill=FALSE) +
  theme_bw() +
  theme(axis.title=element_blank(),
        plot.title = element_text(size=16, face="bold"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.87, 0.45),
        plot.margin = unit(c(-3, 0, -7, 0), "cm"),
        legend.text = element_text(size=8),
        legend.key.size =  unit(0.20, "in"),
        legend.title = element_text(size=8)) +
  scale_colour_gradient("Murders", low = "dodgerblue", high = "firebrick1") + 
  ggtitle("A. Geographic Distribution of Murders")+
  guides(size=FALSE) 


execmapB <- ggplot() + 
  geom_polygon(aes(x = states$long, y = states$lat, group = states$group), color = "white", lwd = 0.25) + 
  #  geom_point(aes(x = murder_dat_merged$INTPTLONG, y = murder_dat_merged$INTPTLAT, size = murder_dat_merged$rescaled, colour = murder_dat_merged$rescaled), alpha = 0.75) +
  geom_point(aes(x = county_dat_merged$INTPTLONG, y = county_dat_merged$INTPTLAT, size = county_dat_merged$execs, colour = county_dat_merged$execs), alpha = 0.75, show.legend = TRUE) +
  coord_fixed(1.3) +
  #guides(fill=FALSE) +
  theme_bw() +
  theme(axis.title=element_blank(),
        plot.title = element_text(size=16, face="bold"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.87, 0.45),
        plot.margin = unit(c(-3, 0, -7, 0), "cm"),
        legend.text = element_text(size=8),
        legend.key.size =  unit(0.20, "in"),
        legend.title = element_text(size=8)) +
  scale_colour_gradient("Executions", low = "dodgerblue", high = "firebrick1") +
  ggtitle("B. Geographic Distribution of Executions")+
  guides(size=FALSE) 

pdf("Figure1Full.pdf")
multiplot(murdmapA, execmapB)
dev.off()

#####################
# Clauset et al test
#####################
library(poweRlaw)
set.seed(1234)
dat_pl <- exec_by_county_df$execs

#### fit power law
plaw_m_m <- displ$new(dat_pl)
(est = estimate_pars(plaw_m_m))

(est = estimate_xmin(plaw_m_m))

plaw_m_m$setXmin(est)

#plot(plaw_m_m)
#lines(plaw_m_m, col=2)

bs_plaw = bootstrap_p(plaw_m_m, no_of_sims=100000, threads=2)
bs_plaw$p
# 0.78898

bs_plaw$gof

#### Poisson

pois_m_m <- dispois$new(dat_pl)
(est = estimate_pars(pois_m_m))

(est = estimate_xmin(pois_m_m))

pois_m_m$setXmin(est)

#plot(pois_m_m)
#lines(pois_m_m, col=2)

bs_pois = bootstrap_p(pois_m_m, no_of_sims=100000, threads=2)
bs_pois$p

bs_pois$gof


#### log normal
lognorm_m_m <- dislnorm$new(dat_pl)
(est = estimate_pars(lognorm_m_m))

(est = estimate_xmin(lognorm_m_m))

lognorm_m_m$setXmin(est)

#plot(lognorm_m_m)
#lines(lognorm_m_m, col=2)

bs_lognorm = bootstrap_p(lognorm_m_m, no_of_sims=100000, threads=2)
bs_lognorm$p
# 0.82559

bs_lognorm$gof


#### exponential
expon_m_m <- disexp$new(dat_pl)
(est = estimate_pars(expon_m_m))

(est = estimate_xmin(expon_m_m))

expon_m_m$setXmin(est)

#plot(expon_m_m)
#lines(expon_m_m, col=2)

bs_expon = bootstrap_p(expon_m_m, no_of_sims=100000, threads=2)
bs_expon$p

bs_expon$gof


plot(plaw_m_m, ylab="CDF")
lines(lognorm_m_m)
lines(plaw_m_m, col=2, lty=2)

save.image("PowerLaw_Output.RData")

# put into table

Distribution <- c("Power Law", "Log Normal", "Poisson", "Exponential")
P <- c(bs_plaw$p, bs_lognorm$p, bs_pois$p, bs_expon$p)

table_df <- data.frame(Distribution, P)

sink("Table1.txt")
table_df
sink()

library(xtable)
xtable(table_df)

#### compare models
#load("PowerLaw_Output.RData")


### Create plot containing all candidate distributions

xcoords <- log(plot(plaw_m_m)$x)
ycoords <- log(plot(plaw_m_m)$y)
xlabs <- plot(plaw_m_m)$x
ylabs <- plot(plaw_m_m)$y

plaw_x_coords <- log(lines(plaw_m_m)$x)
plaw_y_coords <- log(lines(plaw_m_m)$y)

pois_x_coords <- log(lines(pois_m_m)$x)
pois_y_coords <- log(lines(pois_m_m)$y)

lognormal_x_coords <- log(lines(lognorm_m_m)$x)
lognormal_y_coords <- log(lines(lognorm_m_m)$y)

expon_x_coords <- log(lines(expon_m_m)$x)
expon_y_coords <- log(lines(expon_m_m)$y)

frame_df <- data.frame(
  x = xcoords,
  y = ycoords,
  xlabs = xlabs,
  ylabs = ylabs
)

lines_df <- data.frame(
  x = c(plaw_x_coords, lognormal_x_coords, expon_x_coords),
  y = c(plaw_y_coords, lognormal_y_coords, expon_y_coords),
  type = c(rep("Power Law", times = length(plaw_x_coords)), rep("Log-Normal", times = length(lognormal_x_coords)), rep("Exponential", times = length(expon_x_coords)))
)

library(RColorBrewer)
# make these prettier
pal <- brewer.pal(3, "Dark2")
pal_rev <- rev(pal)



pdf("Figure2.pdf")
ggplot()+
  geom_point(data=frame_df, aes(x = frame_df$xlabs, y = frame_df$ylabs), shape = 16, size=2) +
  geom_line(data = lines_df, aes(x = exp(lines_df$x), y = exp(lines_df$y), colour = lines_df$type), size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  xlab("Cumulative Number of Executions") +
  ylab("Fraction of Counties") + 
  scale_color_manual(name = "Distribution", 
                     values = pal_rev) +
  theme(legend.position = c(0.8, 0.8), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
dev.off()


#############################
### Top Matter for CFM
#############################

rm(list=ls())

options(stringsAsFactors = FALSE)

library(data.table)
library(foreign)
library(readstata13) # probably need to install this package


#############################
### Data Preparation for CFM
#############################

cens <- read.dta13('Census_Judicial_DP_Master_782015.dta')
exec <- read.dta13('execs-by-date-by-county.dta')

setDT(cens)
setDT(exec)

cens <- cens[geo_fips > 0 & !is.na(year) & deathstate == 1,]

new <- rbindlist(lapply(1:12, function(i) {
  cens$month <- i
  cens
}))

exec <- exec[fips > 0, .(fips, date)]
setnames(exec, 'fips', 'geo_fips')

exec[, `:=`(year = year(date), month = month(date), execInMonth = 1)]

exec <- unique(exec[, date := NULL])

exec2 <- merge(new, exec, by = c('geo_fips', 'year', 'month'), all.x = TRUE)
exec2$execInMonth[is.na(exec2$execInMonth)] <- 0

# There are 1321 execution-months in exec but only 1307 in the exec2 file...

setkeyv(exec2, c('geo_fips', 'year', 'month'))

exec2[, past_exec := shift(cumsum(execInMonth)), .(geo_fips)]
exec2$past_exec[is.na(exec2$past_exec)] <- 0


new_ex_master <- lapply(3:8, function(i) {
  # drops counties in highest strata after they execute someone (is this okay?)
  d <- exec2[past_exec < i, ] 
  d[, max_strat := i]
  d[, `:=`(stratum = ifelse(past_exec == i - 1, i, past_exec + 1),
           newtime = year - 1977 + month / 12 - 1)]
  d <- d[, .(
    # can add more variables as you wish
    state_fips = unique(state_fips),
    state = unique(state),
    totalpop = mean(totalpop),
    pctwhitepop = mean(pctwhitepop),
    pctpovyes = mean(pctpovyes, na.rm = TRUE),
    judelec = mean(judelec), # this is a mean - how to aggregate judselect?
    circuit = unique(circuit),
    dp_opinion = mean(dp_opinion, na.rm = TRUE),
    homicides = mean(homicides, na.rm = TRUE),
    exec = max(execInMonth),
    time1 = min(newtime),
    time2 = max(newtime), 
    max_strat = unique(max_strat)),
    .(geo_fips, stratum)]
  d[, racialthreat := abs(70 - pctwhitepop)]
  d
})

save(new_ex_master, file = 'new_ex_master.Rdata')

# 8 strata
dat <- new_ex_master[[6]]
table(dat$stratum)

library(car)
dat$stratum_new <- recode(dat$stratum,
                          "1=1; 2=2; 3=3; 4=3; 5=3; 6=3; 7=3; 8=3")
table(dat$stratum_new)

# New recial threat
dat$racialthreat_new <- 100 - dat$racialthreat

dat$racialthreat <- dat$racialthreat_new

# Exclude observations where time1 == time 2 
dat <- subset(dat, dat$time1 != dat$time2)

# Load Bootstrapped CFM function
source("bootCFM_3s.R")

################
# Primary Model
################

# takes a minute or so
# Don't worry about setting the seed, it's done within the function
m1 <- bootCFM(form = Surv(time1, time2, exec) ~
                strata(stratum_new) + 
                homicides +
                pctpovyes +
                racialthreat +
                log(totalpop) +
                frailty(state, distribution="gamma", method="em"),
              dat = dat, strata_var = dat$stratum_new)
m1$CI_plot$data$strata <- ifelse(m1$CI_plot$data$strata == 1, "0 Executions", 
                                 ifelse(m1$CI_plot$data$strata == 2, "1 Execution", "2 or More Executions"))

pdf("Figure3.pdf")
m1$CI_plot
dev.off()

baseline <- coxph(Surv(time1, time2, exec) ~
                    strata(stratum_new) + 
                    homicides +
                    pctpovyes +
                    racialthreat +
                    log(totalpop) +
                    frailty(state, distribution="gamma", method="em"),
                  method = c("efron"),
                  x = TRUE, 
                  y = TRUE,
                  data = dat)

library(texreg)

sink("Table2.txt")
screenreg(baseline)
sink()

### This completes the primary analysis, moving to SI
# Cox Snell Figure 1S

dat <- na.omit(dat)
# Cox Snell Residuals
cs <- dat$exec - residuals(baseline, type = "martingale")

fitres <- survfit(coxph(Surv(cs, dat$exec) ~ 1 +
                          strata(dat$stratum_new)), type = "aalen")

cd_df <- data.frame(res = fitres$time, 
                    ch = -log(fitres$surv),
                    Strata = factor(c(rep(1, fitres$strata[1]), 
                                      rep(2, fitres$strata[2]), 
                                      rep(3, fitres$strata[3]))))

pal <- brewer.pal(3, "Set2")
pal_rev <- rev(pal)

pdf("FigureS1.pdf")
ggplot(cd_df, aes(res, ch, color = Strata, group = Strata)) +
  geom_line(size = 1) + geom_abline(intercept = 0, slope = 1, size = 1) +
  ylab('Est. Cumulative Hazard') + 
  scale_color_manual(name = "Strata", 
                     #values = c("black", "dodgerblue", "firebrick", "gold2")) +
                     values = pal_rev) + 
  theme_bw() + 
  theme(legend.position = c(0.2, 0.8), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  xlab('Cox-Snell Residuals') 
dev.off()

# Figure S2 Martingale
library(Rmisc)

covariates <- c("homicides", "pctpovyes", "racialthreat", "totalpop", "strata(stratum_new)", 
                "frailty(state, distribution = 'gamma',  method = 'em')")

better_names <- c("Homicides", "Poverty", "Racial Threat", "Population")

martin_dt <- rbindlist(lapply(1:4, function(i) {
  f <- as.formula(paste("Surv(time1, time2, exec) ~",  
                        paste0(covariates[-i], collapse = "+")))
  res_without <- residuals(coxph(f, data = dat), type = "martingale")
  res_with <- residuals(baseline, type = "martingale")
  d <- data.table(cov_name = better_names[i],
                  cov_value = as.numeric(dat[[covariates[i]]]), 
                  res_with = res_with,
                  res_without = res_without)
  setnames(d, c("res_with", "res_without"), 
           c("With Covariate", "Without Covariate"))
  melt(d, id.vars = c("cov_name", "cov_value"), 
       variable.name = "res", value.name = "res_value")
}))


martin_dt$cov_value[martin_dt$cov_name == "homicides"] <- 
  martin_dt$cov_value[martin_dt$cov_name == "homicides"] / 1000
levs <- expand.grid(res = sort(unique(martin_dt$res)), 
                    cov_name = sort(unique(martin_dt$cov_name)),
                    stringsAsFactors = FALSE)
martin_plot_cf <- lapply(seq_len(nrow(levs)), function(i) {
  d <- martin_dt[cov_name == levs$cov_name[i] &
                   res == levs$res[i]]
  p <- ggplot(d, aes(cov_value, res_value)) +
    geom_point(alpha = 0.1) +
    theme_bw() +
    xlab(levs$cov_name[i]) +
    theme(axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          axis.title.y = element_text(size = 9),
          axis.title.x = element_text(size = 9))
  if (levs$cov_name[i] == 'Homicides') {
    p <- p + geom_smooth(se = FALSE, size = 0.5)
  } else if (levs$cov_name[i] == 'Judicial Election') {
    p <- p + geom_smooth(method = 'loess', se = FALSE, size = 0.5)
  } else {
    p <- p + geom_smooth(method = 'lm', se = FALSE, size = 0.5)
  }
  if (levs$res[i] == "With Covariate") {
    p <- p + ylab('Residual, With Cov.')
  } else {
    p <- p + ylab('Residual, Without Cov.')
  }
  p
})

pdf("FigureS2.pdf")
multiplot(plotlist = martin_plot_cf, cols = 4)
dev.off()

# Figure S3 DFBetas
dfb <- data.table(residuals(baseline, type = "dfbeta"))
setnames(dfb, c("Homicides", "Poverty", "Racial Threat", "Population"))
dfb[, Observation := seq_len(nrow(dfb))]
dfb <- melt(dfb, id.vars = "Observation", 
            variable.name = "Covariate", 
            variable.factor = FALSE,
            value.name = "dfbeta")

pdf("FigureS3.pdf")
ggplot(dfb, aes(Observation, dfbeta)) + 
  # geom_point(size = 0.1) + 
  # geom_point(aes(size = abs(dfbeta)/10000, color = dfbeta), alpha = 0.5) + 
  geom_point(aes(size = abs(dfbeta)/10000), alpha = 0.5) + 
  # geom_point(aes(color = dfbeta), size =0.1, alpha = 0.5) + 
  facet_wrap(~ Covariate, scales = "free") + 
  theme_bw() + 
  geom_hline(yintercept = 0) +
  xlab('Observation Number') +
  ylab('DFBETAs') +
  theme(strip.background = element_rect(fill = "white", color = "white"),
        legend.position = "none", 
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 9),
        panel.margin.x = unit(0.7, "lines"), 
        panel.margin.y = unit(0.7, "lines"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) 
dev.off()

# Figure S7
dev_data <- data.table(obs = seq_len(nrow(dat)),
                       time1 = dat$time1,
                       time2 = dat$time2, 
                       exec = dat$exec,
                       dev_res = residuals(baseline, type = 'deviance'))

dev_data[, Time  :=time2 - time1]
p1 <- ggplot(dev_data, aes(obs, dev_res)) + geom_point(aes(color = dev_res), alpha = 0.2) +
  geom_smooth(se = FALSE, size = 0.5) +
  geom_hline(yintercept = 0) + 
  xlab('Observation Number') + ylab('Deviance Residuals') +
  theme_bw() + 
  theme(legend.position = c(0.2, 0.8), 
        axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) +
  guides(color=FALSE)
p2 <- ggplot(dev_data, aes(Time, dev_res)) + geom_point(aes(color = dev_res), alpha = 0.2) +
  geom_smooth(se = FALSE, size = 0.5) + 
  geom_hline(yintercept = 0) + 
  xlab('Time Until Next Execution') + ylab('Deviance Residuals') +
  theme_bw() + 
  theme(legend.position = c(0.2, 0.8), 
        axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"),
        axis.title.y =element_blank()) +
  guides(color=FALSE) 

pdf("FigureS7.pdf")
multiplot(p1, p2, cols = 2)
dev.off()

# Robustness models 
# Exclude Haris County
library(data.table)

noHarrDat <- dat[!(geo_fips %in% 48201)]

m.NoHarr <- bootCFM(form = Surv(time1, time2, exec) ~
                      strata(stratum_new) + 
                      homicides +
                      pctpovyes +
                      racialthreat +
                      log(totalpop) +
                      frailty(state, distribution="gamma", method="em"),
                    dat = noHarrDat, strata_var = noHarrDat$stratum_new)

m.NoHarr$CI_plot$data$strata <- ifelse(m.NoHarr$CI_plot$data$strata == 1, "0 Executions", 
                                       ifelse(m.NoHarr$CI_plot$data$strata == 2, "1 Execution", "2 or More Executions"))

pdf("FigureS5.pdf")
m.NoHarr$CI_plot
dev.off()

# Remove Dallas County
noDallDat <- dat[!(geo_fips %in% 48113)]

m.NoDall <- bootCFM(form = Surv(time1, time2, exec) ~
                      strata(stratum_new) + 
                      homicides +
                      pctpovyes +
                      racialthreat +
                      log(totalpop) +
                      frailty(state, distribution="gamma", method="em"),
                    dat = noDallDat, strata_var = noDallDat$stratum_new)

m.NoDall$CI_plot$data$strata <- ifelse(m.NoDall$CI_plot$data$strata == 1, "0 Executions", 
                                       ifelse(m.NoDall$CI_plot$data$strata == 2, "1 Execution", "2 or More Executions"))

pdf("FigureS4.pdf")
m.NoDall$CI_plot
dev.off()


# Remove Abolitionist States
# From DPIC
abolishionist_states <- c("AK", "CT", "HI", "IL", "IA", "ME", "MD", "MA", 
                          "MI", "MN", "NJ", "NM", "NY", "ND", "RI", "VT", "WV",
                          "WI")

noAbolDat <- dat[!(dat$state %in% abolishionist_states),]



m.noAbol <- bootCFM(form = Surv(time1, time2, exec) ~
                      strata(stratum_new) + 
                      homicides +
                      pctpovyes +
                      racialthreat +
                      log(totalpop) +
                      frailty(state, distribution="gamma", method="em"),
                    dat = noAbolDat, strata_var = noAbolDat$stratum_new)

m.noAbol$CI_plot$data$strata <- ifelse(m.noAbol$CI_plot$data$strata == 1, "0 Executions", 
                                       ifelse(m.noAbol$CI_plot$data$strata == 2, "1 Execution", "2 or More Executions"))

pdf("FigureS6.pdf")
m.noAbol$CI_plot
dev.off()

