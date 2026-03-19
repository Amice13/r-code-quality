rm(list = ls())
library(data.table)
library(ggplot2)
library(here)
library(stringi)
library(ggthemes)
library(gridExtra)

load(here('data','ver_sim.RData'))

# Create/modify variables
  ver[,leftwing:=ifelse(party %in% c('PT','PDT','PSB','PSTU','PCO','PCdoB','PV','PSOL'),1,0)]
  ver[,log_donations := log(1+Donations)]

tt <- ver[year %in% c(2004,2008,2012) & abs(marginal)!=0]

tt <- unique(tt[abs(marginal_muni) < 0.0025, .(muni_code, elected_muni, sc_before, pop, marginal_muni, law_enforcement, dv_homicides, p_elected_muni, dv_nonwhite_men_lagged, dv_finbra_lagged, gini, gdp_pc)])

dos <- subset(tt, marginal_muni > 0) 
uno <- subset(tt, marginal_muni < 0)

tres <- rbind(uno,dos) 
#tres <- tres[muni_code != 90581]

setnames(tres, old = c("gdp_pc",
                       "gini", 
                       "p_elected_muni", 
                       "dv_nonwhite_men_lagged",
                       "dv_finbra_lagged",
                       "sc_before",
                       "dv_homicides"), 
         new = c("GDP_per_capita",
                "Inequality_Gini",
                "Incumbent_Law_and_Order",
                "Past_Homicides_non_white_men",
                "Past_local_spending_in_public_security",
                "Preexisting_security_committee",
                "BASELINE_Homicides"))
         
vars <- c("GDP_per_capita",
          "Inequality_Gini",
          "Incumbent_Law_and_Order",
          "Past_Homicides_non_white_men",
          "Past_local_spending_in_public_security",
          "Preexisting_security_committee",
          "BASELINE_Homicides")
tres <- data.frame(tres)

plot_municipal <- list() # this list will collect plots
for(v in vars){
  tres[paste0("dv")] <- tres[paste0(v)]
  tres$zdv <- I(tres$dv - mean(tres$dv,na.rm=TRUE)) / sd(tres$dv,na.rm=TRUE) #standardize so it can be all in the same plot
  y = tres$zdv
  
  cut <- cut(tres$marginal_muni, 100, include.lowest = TRUE) 
  tmp <- aggregate(y, by=list(cut = cut), FUN=mean, na.rm=T)
  tmp1 <- aggregate((tres$marginal_muni), by=list(cut = cut), FUN=mean, na.rm=T)
  data <- data.frame(marginal_muni = tmp1$x, y = tmp$x)
  
  plot_municipal[[v]] <- ggplot() +
    geom_point(data = data[data$marginal_muni < 0,], aes(marginal_muni, y), na.rm=T,size=1, color = 'gray70') + 
    geom_point(data = data[data$marginal_muni > 0,], aes(marginal_muni, y), na.rm=T,size=1, color = 'gray70',shape = 18) + 
    stat_smooth(data = tres, aes(x = marginal_muni, y = zdv, group = elected_muni), method=loess ,na.rm=T,level=.95, colour='gray20', size=1.25) + 
    ggtitle(paste(v)) +
    coord_cartesian(y=c(-3,3)) +
    ylab('Standard deviations') +
    xlab('Margin') +
    theme_bw()
}

#ggsave(here('writing', 'img', 'fig_b1.pdf'), arrangeGrob(grobs = plot_municipal, ncol = 2), height = 11, width = 8.5, units = 'in')  ## save plot


# individual level

tt_individual <- unique(ver[abs(marginal) < 0.0025 & law_enforcement ==1  & abs(marginal)!=0, .(muni_code, marginal, elected, leftwing, female, age, pm, civil, log_donations, Brokers, Donors)])

dos <- subset(tt_individual, marginal > 0) 
uno <- subset(tt_individual, marginal < 0)

tres <- rbind(uno,dos) 
#tres <- tres[muni_code != 90581]

setnames(tres, old = c('leftwing',
                       'female',
                       'age',
                       'pm',
                       'civil',
                       'log_donations',
                       'Brokers',
                       'Donors'), 
         new = c("Leftwing_candidate",
                 "Female_candidate",
                 "Age",
                 "Patrolling_police",
                 "Investigative_police",
                 "Donations",
                 "Brokers",
                 "Donors"))

vars <- c("Leftwing_candidate",
          "Female_candidate",
          "Age",
          "Patrolling_police",
          "Investigative_police",
          "Donations",
          "Brokers",
          "Donors")

tres <- data.frame(tres)

plot_individual <- list() # this list will collect plots
for(v in vars){
  tres[paste0("dv")] <- tres[paste0(v)]
  tres$zdv <- I(tres$dv - mean(tres$dv,na.rm=TRUE)) / sd(tres$dv,na.rm=TRUE) #standardize so it can be all in the same plot
  y = tres$zdv
  
  cut <- cut(tres$marginal, 100, include.lowest = TRUE) 
  tmp <- aggregate(y, by=list(cut = cut), FUN=mean, na.rm=T)
  tmp1 <- aggregate((tres$marginal), by=list(cut = cut), FUN=mean, na.rm=T)
  data <- data.frame(marginal = tmp1$x, y = tmp$x)
  
  plot_individual[[v]] <- ggplot() +
    geom_point(data = data[data$marginal < 0,], aes(marginal, y), na.rm=T,size=1, color = 'gray70') + 
    geom_point(data = data[data$marginal > 0,], aes(marginal, y), na.rm=T,size=1, color = 'gray70',shape = 18) + 
    stat_smooth(data = tres, aes(x = marginal, y = zdv, group = elected), method=loess ,na.rm=T,level=.95, colour='gray20', size=1.25) + 
    ggtitle(paste(v)) +
    ylab('Standard deviations') +
    xlab('Margin') +
    coord_cartesian(y=c(-3,3)) +
    theme_bw()
}

#plots_individuais <-  grid.arrange(grobs = plot_individual, ncol = 2) ## display plot
#ggsave(here('writing', 'img', 'fig_b2.pdf'), arrangeGrob(grobs = plot_individual, ncol = 2), height = 11, width = 8.5, units = 'in')  ## save plot

