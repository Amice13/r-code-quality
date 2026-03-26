######################################
#### Main Analysis for TSMO Paper ####
######################################

# Set-up Packages, Functions, Data

packs <- c("tidyverse","multiwayvcov","sandwich","effects", 
           "magrittr","broom","dotwhisker","lmtest","stargazer","systemfit","multcomp") # Load Necessary Packages
lapply(packs,library,character.only = T) 

# Load Custom Functions

source("functions.R")

# Load Datasets created in data_prep script

all.working.datasets <- readRDS("all_working_datasets.rds") 

working.data.all.tsmos <- all.working.datasets[[1]]

working.data.hr.only <- all.working.datasets[[2]]

working.data.women.only <- all.working.datasets[[3]]

rm(all.working.datasets)


# Summary Statistics Table #

sumstats.data <- working.data.all.tsmos %>% 
  ungroup() %>% 
  select(polyarchy_lead,partip_lead,elec_lead,fr.assoc_lead,fre.exp_lead,
         tsmo.wtd.polyarchy,ioscore.polyarchy,dem.perf.poly,neighbors400.poly,
         cw,log_gdppc,log.tsmo.mems) %>% 
  summarytools::descr(., transpose = T) %>%
  rownames_to_column(var = "Variable") %>% 
  mutate(Variable = c("Cold War", "Global Dem. Econ. Perf.", "Free and Fair Elections","Free Association",
                      "Free Expression","IGO Score", "Number of TSMOs (log)", "GDP per capita (log)", "Regional Democracy Level",
                      "Participatory Democracy","Polyarchy","TSMO Diffusion Score"),
    n = nrow(working.data.all.tsmos) * Pct.Valid/100) %>% 
  select(Variable,n,Mean,Std.Dev,Min,Max) 

write_csv(sumstats.data,"tables and figures/main_sumstats.csv")
  


#### Main Text Models #####

# TSMO Wtd average #


attach(working.data.all.tsmos)

wtd.lead.mods <- tsmomodel(dvform = "lead",
                           ivform = "wtd",
                           controlvars = c("ioscore","dem.perf","cw","log_gdppc","neighbor","log.tsmo.mems"))

clustered <- map(wtd.lead.mods, ~ cluster.ses(.,clust = ccode))

detach(working.data.all.tsmos)

clustered <- map(clustered, function(x) {rownames(x)[2:5] <- c("tsmoscore","ioscore","dem.econ.perf","neighbors")
x })

stargazer(clustered[c(1,3,7,8,9)],
          omit = "ccode",
          type = "text",
          column.labels = c("Polyarchy","Participatory Democracy","Free and Fair Elections","Free Association","Free Expression"),
          out = "tables and figures/maintable.doc",
          covariate.labels = c("TSMO Diffusion","IO Score","Democracy Econ. Perf.","Geog. Diffusion","Cold War","GDP per cap (log","Number of TSMOs (log)"),
          star.cutoffs = c(0.05,0.01,0.001),
          )


# Main Models with Lagged DV added

attach(working.data.all.tsmos)

wtd.lead.mods.lagged.dv <- tsmomodel(dvform = "lead",
                           ivform = "wtd",
                           controlvars = c("ioscore","dem.perf","cw","log_gdppc","neighbor","log.tsmo.mems","lagged_dv"))

clustered.lagged.dv <- map(wtd.lead.mods.lagged.dv, ~ cluster.ses(.,clust = ccode))

detach(working.data.all.tsmos)

clustered.lagged.dv <- map(clustered.lagged.dv, function(x) {rownames(x)[2:6] <- c("tsmoscore","lagged_dv","ioscore","dem.econ.perf","neighbors")
x })


stargazer(clustered.lagged.dv[c(1,3,7,8,9)],
          omit = "ccode",
          type = "html",
          column.labels = c("Polyarchy","Participatory Democracy","Free and Fair Elections","Free Association","Free Expression"),
          out = "tables and figures/lagged_dv_table.doc",
          covariate.labels = c("TSMO Diffusion","Lagged DV","IO Score","Democracy Econ. Perf.","Geog. Diffusion","Cold War","GDP per cap (log","Number of TSMOs (log)"),
          star.cutoffs = c(0.05,0.01,0.001)
)


################################################
#### Imbens Tests for Omitted Variable Bias ####
################################################

# Participatory Democracy #

imbens.test.data <- working.data.all.tsmos %>% 
  mutate(partip_lead = scale(partip_lead),
         tsmo.wtd.partip = scale(tsmo.wtd.partip))

alpha <- rnorm(10000, 0,1) ## Generate 10000 alphas from a normal distribution
delta <- rnorm(10000, 0,1) ## Generate 10000 deltas from a normal distribution
X <- "ioscore.partip + dem.perf.partip + neighbors400.partip + log_gdppc + cw + log.tsmo.mems + as.factor(ccode)"
imbens.test.out<- lm(paste0("partip_lead~tsmo.wtd.partip+", X), imbens.test.data) ## regression without unobserved confounder U


## use the properties of logit transformation to generate the unobserved confounder U
genConfound<- function (x, y) {
  e <- rnorm(nrow(imbens.test.data), 0, 1)
  w <- x * imbens.test.data$tsmo.wtd.partip + y * imbens.test.data$partip_lead + e
  p <- exp(w)/(1+exp(w))
  U <- rbinom(nrow(imbens.test.data), 1, p)
  return(U)
}

## generate the estimated TE including the unobserved confounding covariate U
results.imbens.test <- NULL
system.time({
  for (i in seq_len(length(alpha))) {
    U <- genConfound(alpha[i], delta[i])
    corD <- cor(U, imbens.test.data$tsmo.wtd.partip, use="pairwise.complete.obs")
    corY <- cor(U, imbens.test.data$partip_lead, use="pairwise.complete.obs")
    estTE <- coef(lm(paste0("partip_lead ~ tsmo.wtd.partip + U +", X), imbens.test.data))["tsmo.wtd.partip"]
    names(estTE) <- NULL
    res <- c(estTE = estTE, corD = corD, corY = corY)
    results.imbens.test <- rbind(results.imbens.test, res)
    if(nrow(results.imbens.test) %in% seq(100,9900,100) ){ 
      print(paste0(as.character(nrow(results.imbens.test)), " replications completed."))
    } 
    
  }
})

results.imbens.test.partip <- results.imbens.test # Save replication results in object that won't be over-written later

imbens.test.ivcorrs <- abs(cor(imbens.test.data[c('ioscore.partip','dem.perf.partip','neighbors400.partip','log_gdppc','cw','log.tsmo.mems')],y = imbens.test.data$tsmo.wtd.partip, use = "complete.obs"))

imbens.test.dvcorrs <- abs(cor(imbens.test.data[c('ioscore.partip','dem.perf.partip','neighbors400.partip','log_gdppc','cw','log.tsmo.mems')],y = imbens.test.data$partip_lead, use = "complete.obs"))

imbens.test.vars <- c("IO Score","Democracy Econ Perf","Geog. Diffusion","GDP per cap (log)","Cold War","TSMO memberships (log)")

imbens.test.corrs <- data.frame(imbens.test.ivcorrs,imbens.test.dvcorrs,imbens.test.vars)

results.imbens.test <- as.data.frame(results.imbens.test) %>% mutate(corD = abs(corD),corY = abs(corY))

threshold <- 1.96* tidy(imbens.test.out)$std.error[2] # Set threshold for eliminating statistical significance

results.imbens.test$elim <- 0
results.imbens.test$elim[results.imbens.test$estTE <= threshold] <- 1 

plotdata <- results.imbens.test %>% 
  filter(corD > 0 & corY > 0 & elim == 1)

curveline <- data.frame(corD = seq(from = 0.5, to = 0.9,length.out = 20)) %>% # Create smoothed minimum value curve
  mutate(corY = map_dbl(corD, function(x) {
    min(plotdata$corY[plotdata$corD > x - 0.01 & plotdata$corD < x + 0.01])
  }),
  corY = case_when(corD < min(plotdata$corD) ~ 1,
                   corD > plotdata$corD[plotdata$corY == min(plotdata$corY)] ~ min(plotdata$corY),
                   corY == Inf ~ min(corY),
                   TRUE ~ corY),
  pred.corY = predict(lm(corY ~ corD + I(corD^2))))

curveline$pred.corY[curveline$corD > curveline$corD[curveline$pred.corY == min(curveline$pred.corY)]] <- min(curveline$pred.corY) 


partip.imbens.plot <- ggplot(curveline,aes(corD,pred.corY)) +
  geom_line() +
  geom_point(data = imbens.test.corrs,aes(x = imbens.test.ivcorrs,y = imbens.test.dvcorrs),shape = 3) +
  # geom_text(data = imbens.test.corrs, aes(x = imbens.test.ivcorrs,y = imbens.test.dvcorrs,label = imbens.test.vars),
  #           hjust = "inward", vjust = "inward",size = 3) +
  scale_y_continuous(limits = c(0,1),name = "Corr. with DV") +
  scale_x_continuous(limits = c(0,0.9),name = "Corr. With IV") +
  ggtitle("Participatory Democracy") +
  theme_minimal()
partip.imbens.plot

# Freedom of Expression #

imbens.test.data <- working.data.all.tsmos %>% 
  mutate(fre.exp_lead = scale(fre.exp_lead),
         tsmo.wtd.fre.exp = scale(tsmo.wtd.fre.exp))

alpha <- rnorm(10000, 0,1) ## Generate 10000 alphas from a normal distribution
delta <- rnorm(10000, 0,1) ## Generate 10000 deltas from a normal distribution
X <- "ioscore.fre.exp + dem.perf.poly + neighbors400.fre.exp + log_gdppc + cw + log.tsmo.mems + as.factor(ccode)"
imbens.test.out<- lm(paste0("fre.exp_lead~tsmo.wtd.fre.exp+", X), imbens.test.data) ## regression without unobserved confounder U


## use the properties of logit transformation to generate the unobserved confounder U
genConfound<- function (x, y) {
  e <- rnorm(nrow(imbens.test.data), 0, 1)
  w <- x * imbens.test.data$tsmo.wtd.fre.exp + y * imbens.test.data$fre.exp_lead + e
  p <- exp(w)/(1+exp(w))
  U <- rbinom(nrow(imbens.test.data), 1, p)
  return(U)
}

## generate the estimated TE including the unobserved confounding covariate U

results.imbens.test <- NULL
system.time({
  for (i in seq_len(length(alpha))) {
    U <- genConfound(alpha[i], delta[i])
    corD <- cor(U, imbens.test.data$tsmo.wtd.fre.exp, use="pairwise.complete.obs")
    corY <- cor(U, imbens.test.data$fre.exp_lead, use="pairwise.complete.obs")
    estTE <- coef(lm(paste0("fre.exp_lead ~ tsmo.wtd.fre.exp + U +", X), imbens.test.data))["tsmo.wtd.fre.exp"]
    names(estTE) <- NULL
    res <- c(estTE = estTE, corD = corD, corY = corY)
    results.imbens.test <- rbind(results.imbens.test, res)
    if(nrow(results.imbens.test) %in% seq(100,9900,100) ){ 
      print(paste0(as.character(nrow(results.imbens.test)), " replications completed."))
    } 
    
  }
})

results.imbens.test.fre.exp <- results.imbens.test

imbens.test.ivcorrs <- abs(cor(imbens.test.data[c('ioscore.fre.exp','dem.perf.poly','neighbors400.fre.exp','log_gdppc','cw','log.tsmo.mems')],y = imbens.test.data$tsmo.wtd.fre.exp, use = "complete.obs"))

imbens.test.dvcorrs <- abs(cor(imbens.test.data[c('ioscore.fre.exp','dem.perf.poly','neighbors400.fre.exp','log_gdppc','cw','log.tsmo.mems')],y = imbens.test.data$fre.exp_lead, use = "complete.obs"))

imbens.test.vars <- c("IO Score","Democracy Econ Perf","Geog. Diffusion","GDP per cap (log)","Cold War","TSMO memberships (log)")

imbens.test.corrs <- data.frame(imbens.test.ivcorrs,imbens.test.dvcorrs,imbens.test.vars)

results.imbens.test <- as.data.frame(results.imbens.test) %>% mutate(corD = abs(corD),corY = abs(corY))

threshold <- 1.96* tidy(imbens.test.out)$std.error[2]

results.imbens.test$elim <- 0
results.imbens.test$elim[results.imbens.test$estTE <= threshold] <- 1 

plotdata <- results.imbens.test %>% 
  filter(corD > 0 & corY > 0 & elim == 1)

curveline <- data.frame(corD = seq(from = 0.5, to = 0.9,length.out = 20)) %>% 
  mutate(corY = map_dbl(corD, function(x) {
    min(plotdata$corY[plotdata$corD > x - 0.025 & plotdata$corD < x + 0.025])
  }),
  corY = case_when(corD < min(plotdata$corD) ~ 1,
                   corD > plotdata$corD[plotdata$corY == min(plotdata$corY)] ~ min(plotdata$corY),
                   corY == Inf ~ min(corY),
                   TRUE ~ corY),
  pred.corY = predict(lm(corY ~ corD + I(corD^2))))

curveline$pred.corY[curveline$corD > curveline$corD[curveline$pred.corY == min(curveline$pred.corY)]] <- min(curveline$pred.corY) 


fre.exp.imbens.plot <- ggplot(curveline,aes(corD,pred.corY)) +
  geom_line() +
  geom_point(data = imbens.test.corrs,aes(x = imbens.test.ivcorrs,y = imbens.test.dvcorrs),shape = 3) +
  # geom_text(data = imbens.test.corrs, aes(x = imbens.test.ivcorrs,y = imbens.test.dvcorrs,label = imbens.test.vars),
  #           hjust = "inward", vjust = "inward",size = 3) +
  scale_y_continuous(limits = c(0,1),name = "Corr. with DV") +
  scale_x_continuous(limits = c(0,0.9),name = "Corr. With IV") +
  ggtitle("Freedom of Expression") +
  theme_minimal()
fre.exp.imbens.plot


# Freedom of Association #


imbens.test.data <- working.data.all.tsmos %>% 
  mutate(fr.assoc_lead = scale(fr.assoc_lead),
         tsmo.wtd.fr.assoc = scale(tsmo.wtd.fr.assoc))

alpha <- rnorm(10000, 0,1) ## Generate 10000 alphas from a normal distribution
delta <- rnorm(10000, 0,1) ## Generate 10000 deltas from a normal distribution
X <- "ioscore.fr.assoc + dem.perf.poly + neighbors400.fr.assoc + log_gdppc + cw + log.tsmo.mems + as.factor(ccode)"
imbens.test.out<- lm(paste0("fr.assoc_lead~tsmo.wtd.fr.assoc+", X), imbens.test.data) ## regression without unobserved confounder U


## use the properties of logit transformation to generate the unobserved confounder U
genConfound<- function (x, y) {
  e <- rnorm(nrow(imbens.test.data), 0, 1)
  w <- x * imbens.test.data$tsmo.wtd.fr.assoc + y * imbens.test.data$fr.assoc_lead + e
  p <- exp(w)/(1+exp(w))
  U <- rbinom(nrow(imbens.test.data), 1, p)
  return(U)
}

## generate the estimated TE including the unobserved confounding covariate U
results.imbens.test <- NULL
system.time({
  for (i in seq_len(length(alpha))) {
    U <- genConfound(alpha[i], delta[i])
    corD <- cor(U, imbens.test.data$tsmo.wtd.fr.assoc, use="pairwise.complete.obs")
    corY <- cor(U, imbens.test.data$fr.assoc_lead, use="pairwise.complete.obs")
    estTE <- coef(lm(paste0("fr.assoc_lead ~ tsmo.wtd.fr.assoc + U +", X), imbens.test.data))["tsmo.wtd.fr.assoc"]
    names(estTE) <- NULL
    res <- c(estTE = estTE, corD = corD, corY = corY)
    results.imbens.test <- rbind(results.imbens.test, res)
    if(nrow(results.imbens.test) %in% seq(100,9900,100) ){ 
      print(paste0(as.character(nrow(results.imbens.test)), " replications completed."))
    } 
    
  }
})

results.imbens.test.fr.assoc <- results.imbens.test

imbens.test.ivcorrs <- abs(cor(imbens.test.data[c('ioscore.fr.assoc','dem.perf.poly','neighbors400.fr.assoc','log_gdppc','cw','log.tsmo.mems')],y = imbens.test.data$tsmo.wtd.fr.assoc, use = "complete.obs"))

imbens.test.dvcorrs <- abs(cor(imbens.test.data[c('ioscore.fr.assoc','dem.perf.poly','neighbors400.fr.assoc','log_gdppc','cw','log.tsmo.mems')],y = imbens.test.data$fr.assoc_lead, use = "complete.obs"))

imbens.test.vars <- c("IO Score","Democracy Econ Perf","Geog. Diffusion","GDP per cap (log)","Cold War","TSMO memberships (log)")

imbens.test.corrs <- data.frame(imbens.test.ivcorrs,imbens.test.dvcorrs,imbens.test.vars)

results.imbens.test <- as.data.frame(results.imbens.test) %>% mutate(corD = abs(corD),corY = abs(corY))

threshold <- 1.96* tidy(imbens.test.out)$std.error[2]

results.imbens.test$elim <- 0
results.imbens.test$elim[results.imbens.test$estTE <= threshold] <- 1 

plotdata <- results.imbens.test %>% 
  filter(corD > 0 & corY > 0 & elim == 1)

curveline <- data.frame(corD = seq(from = 0.5, to = 0.9,length.out = 20)) %>% 
  mutate(corY = map_dbl(corD, function(x) {
    min(plotdata$corY[plotdata$corD > x - 0.025 & plotdata$corD < x + 0.025])
  }),
  corY = case_when(corD < min(plotdata$corD) ~ 1,
                   corD > plotdata$corD[plotdata$corY == min(plotdata$corY)] ~ min(plotdata$corY),
                   corY == Inf ~ min(corY),
                   TRUE ~ corY),
  pred.corY = predict(lm(corY ~ corD + I(corD^2))))

curveline$pred.corY[curveline$corD > curveline$corD[curveline$pred.corY == min(curveline$pred.corY)]] <- min(curveline$pred.corY) 

fr.assoc.imbens.plot <- ggplot(curveline,aes(corD,pred.corY)) +
  geom_line() +
  geom_point(data = imbens.test.corrs,aes(x = imbens.test.ivcorrs,y = imbens.test.dvcorrs),shape = 3) +
  # geom_text(data = imbens.test.corrs, aes(x = imbens.test.ivcorrs,y = imbens.test.dvcorrs,label = imbens.test.vars),
  #           hjust = "inward", vjust = "inward",size = 3) +
  scale_y_continuous(limits = c(0,1),name = "Corr. with DV") +
  scale_x_continuous(limits = c(0,0.9),name = "Corr. With IV") +
  ggtitle("Freedom of Association") +
  theme_minimal()
fr.assoc.imbens.plot


#### Test of Equality of Coefficients for Free Elections, Free Expression, and Freedom of Association ####

frefair.mod <- lm(frefair_lead ~ tsmo.wtd.frefair + frefair + ioscore.frefair + dem.perf.poly + cw + log_gdppc + neighbors400.frefair + log.tsmo.mems + factor(ccode),data = working.data.all.tsmos) %>% 
  summary()

fr.assoc.mod <- lm(fr.assoc_lead ~ tsmo.wtd.fr.assoc + fr.assoc + ioscore.fr.assoc + dem.perf.poly + cw + log_gdppc + neighbors400.fr.assoc + log.tsmo.mems + factor(ccode), data = working.data.all.tsmos) %>% 
  summary()

fre.exp.mod <- lm(fre.exp_lead ~ tsmo.wtd.fre.exp + fre.exp + ioscore.fre.exp + dem.perf.poly + cw + log_gdppc + neighbors400.fre.exp + log.tsmo.mems + factor(ccode), data = working.data.all.tsmos) %>% 
  summary()


equality.test <- function(beta1,beta2,sd1,sd2){
  (beta1 - beta2)/sqrt(sd1^2 + sd2^2)
}

equality.test.1 <- (frefair.mod$coefficients[2,1] - fr.assoc.mod$coefficients[2,1])/sqrt(frefair.mod$coefficients[2,2]^2 + fr.assoc.mod$coefficients[2,2]^2)


fitsur <- systemfit(list(frefair.reg = frefair.mod,
                        fr.assoc.reg = fr.assoc.mod),
                    data = working.data.all.tsmos,
                    method = "SUR")

glht(fitsur,linfct = c("frefair.reg_tsmo.wtd.frefair" - "fr.assoc.reg_tsmo.wtd.fr.assoc = 0")) %>% summary()

