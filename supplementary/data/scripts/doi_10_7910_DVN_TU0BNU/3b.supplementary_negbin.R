#Fit models & repeat all analyses using a negative binomial stochastic component
rm(list=ls())
library(rstanarm)
library(tidyverse)
library(bayesplot)

options(mc.cores = 4)

setwd("~/Documents/Projects/Non-RAND Projects/Police Shootings")
df <- readRDS(file = "Data/shooting_data_yearly.rds")

#FIT MODELS----
#Pooled
M1 <- stan_glm(n_shootings ~ 1, 
               offset = log(population_LEMAS), 
               data = df, 
               family = neg_binomial_2("log"),
               seed = 4641)
print(M1)
summary(M1)

#Random intercept
M2 <- stan_glmer(n_shootings ~ 1 + (1 | oricode), 
                 offset = log(population_LEMAS), 
                 data = df, 
                 family = neg_binomial_2("log"),
                 seed = 4641)
print(M2)
summary(M2)

#RI + socio-demographics
M3 <- stan_glmer(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
                   fborn_pct + ssi_pct + femalehh_pct + hhinc_median + lesshs_pct + 
                   moreba_pct + unemp_rate + fampov_rate + samehouse1yr_pct + hhsize +
                   vacancy_rate + owner_occ_pct + (1 | oricode), 
                 offset = log(population_LEMAS), 
                 data = df, QR=T,
                 family = neg_binomial_2("log"),
                 # prior = normal(0, 1.5, autoscale=TRUE), 
                 seed = 4641) 
print(M3)
summary(M3)

#RI + demands for services (crime/calls)
M4 <- stan_glmer(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
                   fborn_pct + ssi_pct + femalehh_pct + hhinc_median + lesshs_pct + 
                   moreba_pct + unemp_rate + fampov_rate + samehouse1yr_pct + hhsize +
                   vacancy_rate + owner_occ_pct  +
                   actual_murder + actual_index_violent  + gun_prev +
                   actual_assault_with_a_gun + actual_index_property + 
                   calls911_dispatched + officers_rate + (1 | oricode),
                 offset = log(population_LEMAS), 
                 data = df, QR=T,
                 family = neg_binomial_2("log"),
                 seed = 4641) 

print(M4)
summary(M4)

#RI+ LEMAS org characteristics
M5 <- stan_glmer(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
                   fborn_pct + ssi_pct + femalehh_pct + hhinc_median + lesshs_pct + 
                   moreba_pct + unemp_rate + fampov_rate + samehouse1yr_pct + hhsize +
                   vacancy_rate + owner_occ_pct  +
                   actual_murder + actual_index_violent  +  gun_prev +
                   actual_assault_with_a_gun + actual_index_property + 
                   calls911_dispatched + officers_rate +
                   investigate_UoFinjury + investigate_UoFdeath + investigate_otherdeath +  investigate_firegun + ccrb  +  
                   computer_perfsystem + equip_bluntprojectile +  equip_taser + union_authorized + policy_vehpursuit + 
                   document_displaygun  +  document_dischargegun  + officers_male_pct +  white_overrep + educ_requirement + 
                   cameras_numBWC + trainhrs_academy  +  trainhrs_fieldrecruits   + trainhrs_inservofficers  +  check_total +             
                   cp_total +  policy_total + (1|oricode),
                 offset = log(population_LEMAS), 
                 family = neg_binomial_2("log"),
                 data = df, 
                 QR = T,
                 seed = 4641)

print(M5)
summary(M5)

#ANALYSES----
library(haven)
library(sf)
library(ggpubr)
library(gt)

#0. Check model diagnostics----
#r-hat, neff, mixing of low autocorrelation chains
#launch_shinystan(M1, ppd = FALSE)
#launch_shinystan(M2, ppd = FALSE)
#launch_shinystan(M3, ppd = FALSE)
#launch_shinystan(M4, ppd = FALSE)
#launch_shinystan(M5, ppd = FALSE)

#1. Results Section 1----
##1.a) Random predictions against observed----
yrep1 <- posterior_predict(M1)
flag <- df$population_LEMAS>1000000 

hist(yrep1[2,flag]/df$population_LEMAS[flag]*100000, breaks = 20)

a <- NULL 
for(i in 1:4000){a[i] <- max(yrep1[i,flag]/df$population_LEMAS[flag]*100000)}
a <- max(a)

jpeg("Output/Model Checks/NegBin/sim_distribution_M1.jpeg",quality=100) 
plot(1, type="n", xlab="Shooting Rate (per 100,000)", ylab="Density", xlim=c(0,1.65), ylim=c(0,5), frame = F)
for(i in 1:4000){lines(density(yrep1[i,flag]/df$population_LEMAS[flag]*100000), col=scales::alpha("grey", .07))}
lines(density(df$n_shootings[flag]/df$population_LEMAS[flag]*100000), lwd = 3, col="red")
text(.7,3.5,"Posterior \nSimulations", col = "grey")
arrows(x0 = .51, y0=3.55, x1 = .3, y1=3.4, lwd=2, length = 0.18, col="grey")
text(1,1.5,"Observed \nValues", col = "red")
arrows(x0 = 1, y0=1.18, y1 = .22, lwd=2, length = 0.18, col="red")
dev.off()


##1.b) PPC Table----
tab_output <- NULL
for(i in 1:5){
  mod <- paste0("M",i)
  yrep1 <- posterior_predict(get(mod)) 
  
  ### Y max
  t <- apply(yrep1, 1, max)
  obs <- max(df$n_shootings)
  range_x <- range(c(t,obs))
  hist(t, xlim = range_x); abline(v=obs,col="blue",lwd=4)
  y_max <- mean(t >= obs)
  
  ### % 0s
  t <- apply(yrep1, 1, function(f){sum(f==0)})
  obs <- sum(df$n_shootings==0)
  range_x <- range(c(t,obs))
  hist(t, xlim = range_x, breaks=50); abline(v=obs,col="blue",lwd=4)
  prop_zero <- mean(t >= obs)
  
  #### Variance
  t <- apply(yrep1, 1, var)
  obs <- var(df$n_shootings)
  range_x <- range(c(t,obs))
  hist(t, xlim=range_x, breaks=50); abline(v=obs,col="blue",lwd=4)
  var_mod <- mean(t >= obs) 
  
  temp <- data.frame(y_max, prop_zero, var_mod, Model = i)
  tab_output <- rbind(tab_output, temp)
}

l <- round(tab_output,2) %>% gt()
gtsave(l, "Output/Model Checks/NegBin/PPCs.html")

##1.c) LOOIC----
loo1 <- loo(M1)
loo2 <- loo(M2)
loo3 <- loo(M3)
loo4 <- loo(M4)
loo5 <- loo(M5)
loo_compare(loo1, loo2)
loo_compare(loo2, loo3)
loo_compare(loo3, loo4)
loo_compare(loo4, loo5)
loo1; loo2; loo3; loo4; loo5

plot.dat <- cbind(loo1$estimates[,1], loo2$estimates[,1], loo3$estimates[,1], loo4$estimates[,1], loo5$estimates[,1])
colnames(plot.dat) <- c("M1", "M2", "M3", "M4", "M5")    

plot.dat <- plot.dat %>% as.data.frame() %>% gt()
gtsave(plot.dat, "Output/Model Checks/NegBin/LOOIC.html")

#Results Section 2----
##2.a) Look at how RI variance shrinks----
M1sims <- as.matrix(M1)
M2sims <- as.matrix(M2)
M3sims <- as.matrix(M3)
M4sims <- as.matrix(M4)
M5sims <- as.matrix(M5)

a <- mean(M2sims[,ncol(M2sims)])
quantile(M2sims[,ncol(M2sims)], probs = c(.1,.9))

b <- mean(M3sims[,ncol(M3sims)])
quantile(M3sims[,ncol(M3sims)], probs = c(.1,.9))

c <- mean(M4sims[,ncol(M4sims)])
quantile(M4sims[,ncol(M4sims)], probs = c(.1,.9))

d <- mean(M5sims[,ncol(M5sims)])
quantile(M5sims[,ncol(M5sims)], probs = c(.1,.9))

pd <- data.frame(SD = c(a,b,c,d), Model = c(2,3,4,5))
pd <-  pd %>% gt()
gtsave(pd, "Output/Model Checks/NegBin/RIvar.html")

##2.b) Simulations of changing Xs----
#as observed:
test <- posterior_epred(M5)
q <- rowSums(test)
mean(q); quantile(q, probs = c(0.025,0.975))

#I can also do this manually:
B <- M5sims[, -c(ncol(M5sims)-1,ncol(M5sims))]       
B <- cbind(B,1)

x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )

y_pred <- exp(x_new %*% t(B))
y_bar <- colSums(y_pred)
mean(y_bar); quantile(y_bar, probs = c(0.025,0.975))

#black and Hispanic comp = 0
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, c("blacknh_pct","hispanic_pct")] <- 0 

y_pred <- exp(x_new %*% t(B))
y_bar2 <- colSums(y_pred)
mean(y_bar2); quantile(y_bar2, probs = c(0.025,0.975))

#just black as 0
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, c("blacknh_pct")] <- 0 

y_pred <- exp(x_new %*% t(B))
y_bar3 <- colSums(y_pred)
mean(y_bar3); quantile(y_bar3, probs = c(0.025,0.975))

#just h as 0
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, c("hispanic_pct")] <- 0 

y_pred <- exp(x_new %*% t(B))
y_bar4 <- colSums(y_pred)
mean(y_bar4); quantile(y_bar4, probs = c(0.025,0.975))

#reduce disadvantage, increase advantage
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
good_things <-   c("hhinc_median", "moreba_pct", "samehouse1yr_pct",  "owner_occ_pct")
bad_things <- c("less18_pct",  "ssi_pct", "femalehh_pct", "lesshs_pct", "unemp_rate", "fampov_rate",  "hhsize", "vacancy_rate")
neutral <- "fborn_pct"

for(v in 1:length(good_things)){
  va <- good_things[v]
  x_new[, va] <- ifelse(x_new[, va] < quantile(x_new[, va], probs = 0.75), quantile(x_new[, va], probs = 0.75), x_new[, va])
}

for(v in 1:length(bad_things)){
  va <- bad_things[v]
  x_new[, va] <- ifelse(x_new[, va] > quantile(x_new[, va], probs = 0.25), quantile(x_new[, va], probs = 0.25), x_new[, va])
}

y_pred <- exp(x_new %*% t(B))
y_bar5 <- colSums(y_pred)
mean(y_bar5); quantile(y_bar5, probs = c(0.025,0.975))

#Everywhere is lowest crime quantile, for all crimes
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, colnames(x_new)[startsWith(colnames(x_new),"actual")]] <- 0

y_pred <- exp(x_new %*% t(B))
y_bar6 <- colSums(y_pred)
mean(y_bar6); quantile(y_bar6, probs = c(0.025,0.975))

#just murder/gun violence?
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, colnames(x_new)[startsWith(colnames(x_new),"actual_murder")]] <- 0
x_new[, colnames(x_new)[startsWith(colnames(x_new),"actual_assu")]] <- 0

y_pred <- exp(x_new %*% t(B))
y_bar7 <- colSums(y_pred)
mean(y_bar7); quantile(y_bar7, probs = c(0.025,0.975))

#lower gun prevalence
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )

x_new[, "gun_prev"] <- ifelse(x_new[, "gun_prev"] > quantile(df$gun_prev, probs=.1), 
                              quantile(df$gun_prev, probs=.1), x_new[, "gun_prev"])


y_pred <- exp(x_new %*% t(B))
y_bar7b <- colSums(y_pred)
mean(y_bar7b); quantile(y_bar7b, probs = c(0.025,0.975))

#what if calls were lower?
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, colnames(x_new)[startsWith(colnames(x_new),"calls911")]] <- 0

y_pred <- exp(x_new %*% t(B))
y_bar8 <- colSums(y_pred)
mean(y_bar8); quantile(y_bar8, probs = c(0.025,0.975))

#officers per population?
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, colnames(x_new)[startsWith(colnames(x_new),"officers_rate")]] <- 0

y_pred <- exp(x_new %*% t(B))
y_bar8b <- colSums(y_pred)
mean(y_bar8b); quantile(y_bar8b, probs = c(0.025,0.975))

#strict policies?
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, c("investigate_UoFinjury","investigate_UoFdeath", "investigate_otherdeath", "investigate_firegun",
          "policy_vehpursuit", "document_displaygunYes", "document_dischargegunYes")] <- 1
x_new[, c("document_displaygunNo", "document_dischargegunNo")] <- 0
x_new[, c("policy_total")] <- 15

y_pred <- exp(x_new %*% t(B))
y_bar9 <- colSums(y_pred)
mean(y_bar9); quantile(y_bar9, probs = c(0.025,0.975))

#hiring/training/personnel?
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, c("white_overrep", "educ_requirementL", "educ_requirementM")] <- 0
x_new[, c("check_total")] <- 12
x_new[, "officers_male_pct"] <- ifelse(x_new[, "officers_male_pct"] > quantile(x_new[, "officers_male_pct"], probs = 0.25), 
                                       quantile(x_new[, "officers_male_pct"], probs = 0.25), x_new[, "officers_male_pct"])
x_new[, colnames(x_new)[startsWith(colnames(x_new),"trainhrs_")]] <- 0
x_new[, c("trainhrs_academy[752,9800]", "trainhrs_fieldrecruits[505,4800]", "trainhrs_inservofficers[42,2080]")] <- 1

y_pred <- exp(x_new %*% t(B))
y_bar10 <- colSums(y_pred)
mean(y_bar10); quantile(y_bar10, probs = c(0.025,0.975))

#community policing
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, c("cp_total")] <- 5 

y_pred <- exp(x_new %*% t(B))
y_bar11 <- colSums(y_pred)
mean(y_bar11); quantile(y_bar11, probs = c(0.025,0.975))

#oversight
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, c("ccrb", "computer_perfsystem", "union_authorizedNone", "cameras_numBWC")] <- 1
x_new[, c("union_authorizedSome")] <- 0

y_pred <- exp(x_new %*% t(B))
y_bar12 <- colSums(y_pred)
mean(y_bar12); quantile(y_bar12, probs = c(0.025,0.975))

#less lethal equipment
x_new <- cbind(model.matrix(M5), model.matrix(~ oricode - 1, data=df), log(df$population_LEMAS) )
x_new[, c("equip_bluntprojectileSome", "equip_taserSome")] <- 0
x_new[, c("equip_bluntprojectileAll", "equip_taserAll")] <- 1

y_pred <- exp(x_new %*% t(B))
y_bar13 <- colSums(y_pred)
mean(y_bar13); quantile(y_bar13, probs = c(0.025,0.975))

#plot above as stacked densities:
plot.dat <- data.frame(x = c(y_bar, y_bar3, y_bar4, y_bar5, y_bar6, y_bar7, y_bar7b,
                             y_bar8, y_bar8b, y_bar9, y_bar10, y_bar11, y_bar12, y_bar13), 
                       Model = rep(c("Everything as Observed", "Black Population Set to 0", 
                                     "Hispanic Population Set to 0", "Reduced Social Disadvantages",
                                     "All Crimes at Lowest Quantile", "Gun Assault/Murder at Lowest Quantile", "Gun Prevalence Max 10th Percentile",
                                     "Officer Levels at Lowest Quantile", "911 Calls at Lowest Quantile",
                                     "Stricter Police Policies", "Stringent Personnel Selection & More Training",
                                     "More Community Policing", "Enhanced Officer Oversight", "Universal Less Lethal Equipement"
                       ), each = 4000))
plot.dat$Model <- factor(plot.dat$Model,
                         levels = c("Everything as Observed", "Black Population Set to 0", 
                                    "Hispanic Population Set to 0", "Reduced Social Disadvantages",
                                    "All Crimes at Lowest Quantile", "Gun Assault/Murder at Lowest Quantile","Gun Prevalence Max 10th Percentile",
                                    "Officer Levels at Lowest Quantile", "911 Calls at Lowest Quantile",
                                    "Stricter Police Policies", "Stringent Personnel Selection & More Training",
                                    "More Community Policing", "Enhanced Officer Oversight", "Universal Less Lethal Equipement"))
p <- ggplot(aes(x=x), data=plot.dat) + geom_density(aes(fill="red", alpha=.75), show.legend = F) + facet_wrap(~ Model, ncol=1) + 
  xlab("Simulated Number of Shootings") + ylab("Density") +
  theme_bw() + geom_vline(xintercept = median(y_bar), linetype="dashed")
ggsave("Output/Model Checks/NegBin/counterfactual_Xs.jpeg", plot= p, device="jpeg", width = 4, height = 13)

#Results Section 3----
##3.a) Caterpillar plot----
dat <- data.frame(M5sims) %>% select(starts_with("b.."))
m <- apply(dat,2,mean)
q <- apply(dat,2,function(f){quantile(f, probs = c(0.025,.975))})
q2 <- apply(dat,2,function(f){quantile(f, probs = c(0.25,.75))})

plot.dat <- data.frame(m, lb = q[1,], ub = q[2,], lb2 = q2[1,], ub2 = q2[2,])
plot.dat <- plot.dat[order(plot.dat$m),]
plot.dat$id <- 1:2727

g <- ggplot(aes(x=id, y=m), data=plot.dat) + 
  geom_errorbar(aes(ymin=lb, ymax=ub), size=.05) +
  geom_errorbar(aes(ymin=lb2, ymax=ub2), size=.05, col="red") +
  geom_point(color="darkred") +
  geom_hline(yintercept = 0, linewidth=2, linetype="dashed")+
  theme_bw() + xlab("Police Agency") + ylab("Random Intercept") +
  theme(axis.text.x = element_blank())
ggsave("Output/Model Checks/NegBin/caterpillar_plot.jpeg", plot= g, device="jpeg", width = 8, height = 4.5)

##3.b) Map the agencies----
#flag agencies that stand out in a big way (90% to one side of 0)
temp <- read_dta("Data/agency_level_merged.dta")
temp <- temp %>% select(oricode, crosswalk_agency_name, n_shootings)

dat2 <- dat[which(colMeans(dat > 0) > .9)]

a <- substr(colnames(dat2),23,29) #oris
b <- round(colMeans(dat2),2) #RI
c <- round(colMeans(dat2 > 0),2) # what % greater

high_RIs <- data.frame(oricode = a, RI = b, q = c)

df_trim <- df %>% transmute(oricode, population_LEMAS) %>% unique()

high_RIs <- left_join(high_RIs, temp, by= "oricode") %>% left_join(., df_trim, by="oricode")

high_RIs <- high_RIs %>% transmute(Agency = crosswalk_agency_name, oricode, RI, q, n_shootings, 
                                   population = population_LEMAS)
high_RIs <- high_RIs[order(high_RIs$RI, decreasing = T),]

dat2 <- dat[which(colMeans(dat < 0) > .9)]

a <- substr(colnames(dat2),23,29) #oris
b <- round(colMeans(dat2),2) #RI
c <- round(colMeans(dat2 < 0),2) # what % less

low_RIs <- data.frame(oricode = a, RI = b, q = c)
low_RIs <- left_join(low_RIs, temp, by= "oricode") %>% left_join(., df_trim, by="oricode")

low_RIs <- low_RIs %>% transmute(Agency = crosswalk_agency_name, oricode, RI, q, n_shootings, 
                                 population = population_LEMAS)
low_RIs <- low_RIs[order(low_RIs$RI, decreasing = F),]

low_RIs$type <- "Low RI"
high_RIs$type <- "High RI"
cases <- rbind(low_RIs, high_RIs)

geo_dat <- read_dta("Data/LawenforcementXY.dta")#based on po boxes
geo_dat$LEAR_ID <- as.character(geo_dat$LEAR_ID)

df2 <- read_dta("Data/agency_level_merged.dta")
df2 <- df2 %>% transmute(LEAR_ID, oricode)

geo_dat <- left_join(geo_dat, df2, by = "LEAR_ID")
geo_dat <- geo_dat %>% transmute(LEAR_ID, oricode, X, Y)

cases <- left_join(cases, geo_dat, by = "oricode")

#Manually complete/fix X,Y:
cases$X[cases$X==0] <- NA
cases$Y[cases$Y==0] <- NA
cases$X[cases$oricode=="MOSPD00"] <- -90.21946; cases$Y[cases$oricode=="MOSPD00"] <- 38.63574
cases$X[cases$oricode=="FL06400"] <- -81.05569; cases$Y[cases$oricode=="FL06400"] <- 29.17108
cases$X[cases$oricode=="NC07403"] <- -77.38353; cases$Y[cases$oricode=="NC07403"] <- 35.59926
cases$X[cases$oricode=="MT05601"] <- -108.53968; cases$Y[cases$oricode=="MT05601"] <- 45.78443
cases$X[cases$oricode=="AZ00717"] <- -111.76253; cases$Y[cases$oricode=="AZ00717"] <- 33.43028
cases$X[cases$oricode=="WA03204"] <- -117.40808; cases$Y[cases$oricode=="WA03204"] <- 47.66193
cases$X[cases$oricode=="CA03019"] <- -117.87993; cases$Y[cases$oricode=="CA03019"] <- 33.74093
cases$X[cases$oricode=="CA04313"] <- -121.87032; cases$Y[cases$oricode=="CA04313"] <- 37.32043
cases$X[cases$oricode=="UT01803"] <- -111.92883; cases$Y[cases$oricode=="UT01803"] <- 40.76443
cases$X[cases$oricode=="CA03300"] <- -117.39403; cases$Y[cases$oricode=="CA03300"] <- 33.94568
cases$X[cases$oricode=="VA04301"] <- -77.52900; cases$Y[cases$oricode=="VA04301"] <- 37.63021

pd <- st_as_sf(cases, coords = c("X","Y"))
st_crs(pd) <- 4326
pd <- st_transform(pd, crs = 4269)
#AK <- pd %>% filter(str_detect(pd$oricode, "AK") )
HI <- pd %>% filter(str_detect(pd$oricode, "HI"))
pd <- pd %>% filter(!str_detect(pd$oricode, "HI") & !str_detect(pd$oricode, "AK"))

states <- read_sf("Data/cb_2016_us_state_20m/")
st_crs(states)
HI_state <- states %>% filter(GEOID=="15")
#Alaska <- states %>% filter(GEOID=="02")
states <- states %>% filter(GEOID!="02" & GEOID!="72" & GEOID!="15") #to get rid of Hawaii

#Alaska2 <- st_crop(Alaska, y = c(xmin = -179.2, ymin = 51.21985, xmax = -128, ymax = 71.35257))#trims some Aleutians
#plot(st_geometry(states))
#plot(st_geometry(pd3), pch = 16, col = 'red', add=T)
#plot(st_geometry(pd2), pch = 16, col = 'blue', add=T)

map <- ggplot(data=states) + geom_sf() +  theme_void() +
  geom_point(
    aes(color = type, geometry = geometry),
    stat = "sf_coordinates", data=pd, size=3, shape = 21,stroke = 1) +
  theme(legend.position = "bottom", legend.title=element_blank()) ; map

map_cord <- ggplot(data=states) + geom_sf() + 
  geom_point(
    aes(color = type, geometry = geometry),
    stat = "sf_coordinates", data=pd, size=3, shape = 21,stroke = 1) +
  theme(legend.position = "bottom", legend.title=element_blank()) 

#map_AK <- ggplot(data=Alaska2) + geom_sf() +  theme_void() +
#  geom_point(
#    aes(color = type, geometry = geometry),
#    stat = "sf_coordinates", data=AK, size=3, shape = 21,stroke = 1) +
#  theme(legend.position = "none")  ; map_AK

map_HI <- ggplot(data=HI_state) + geom_sf() +  theme_void() +
  geom_point(
    aes(color = type, geometry = geometry),
    stat = "sf_coordinates", data=HI, size=3, shape = 21,stroke = 1) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=1)) ; map_HI

library(grid)
library(ggmap)

map_combo <- map + inset(ggplotGrob(map_HI), xmin = -125, xmax = -110, 
                         ymin = 21, ymax = 31) 

ggsave("Output/Model Checks/NegBin/map.pdf", plot = map_combo, width = 8, height = 6)

##3.c) Ratios of lambdas & basic info on the high/low RI agencies----
test <- posterior_epred(M5, re.form = ~0) 
test <- colMeans(test)
pd <- data.frame(oricode = df$oricode, lambda_all = M5$fitted.values, lambda_noRI = test)

flagged <- c(high_RIs$oricode, low_RIs$oricode)

pd <- pd %>% filter(oricode %in% flagged)
pd2 <- pd %>% group_by(oricode) %>% summarise(lambda_all = mean(lambda_all), 
                                              lambda_noRI = mean(lambda_noRI))
pd3 <- data.frame(oricode = pd2$oricode, ratio = round(pd2$lambda_noRI/pd2$lambda_all,2))
pd3 <- pd3[order(pd3$ratio, decreasing = T),]

cases2 <- left_join(cases, pd3, by="oricode") %>% select(-X,-Y,-LEAR_ID)
lows <- cases2 %>% filter(type == "Low RI") %>% select(-type, -oricode)
highs <- cases2 %>% filter(type == "High RI") %>% select(-type, -oricode)

l <- lows %>% gt()
h <- highs %>% gt()
gtsave(l, "Output/Model Checks/NegBin/RI_table_low.html")
gtsave(h, "Output/Model Checks/NegBin/RI_table_high.html")

##3.d) What if extreme RIs were more normal?----
#if high were lower
pd4 <- pd2 %>% filter(oricode %in% high_RIs$oricode)
pd4$lambda_all <- pd4$lambda_all*6
pd4$lambda_noRI <- pd4$lambda_noRI*6
pd4$reduction <- pd4$lambda_all - pd4$lambda_noRI

pd4 <- pd4 %>% transmute(oricode, fitted_wRI = round(lambda_all,2),
                         fitted_RIis0 = round(lambda_noRI, 2),
                         reduction = round(reduction,2))
pd4 <- pd4[order(pd4$reduction, decreasing = T),]
r <- sum(pd4$reduction) 

#if low were higher
pd5 <- pd2 %>% filter(oricode %in% low_RIs$oricode)
pd5$lambda_all <- pd5$lambda_all*6
pd5$lambda_noRI <- pd5$lambda_noRI*6
pd5$increase <- pd5$lambda_noRI - pd5$lambda_all 

pd5 <- pd5 %>% transmute(oricode, fitted_wRI = round(lambda_all,2),
                         fitted_RIis0 = round(lambda_noRI, 2),
                         increase = round(increase,2))
pd5 <- pd5[order(pd5$increase, decreasing = T),]
i <- sum(pd5$increase)

##3.e) What if RIs were like the best places?----
test <- posterior_linpred(M5, re.form = ~0) 
test <- colMeans(test)
pd <- data.frame(oricode = df$oricode, lambda_all = M5$fitted.values, lambda_noRI = test)
pd2 <- pd %>% group_by(oricode) %>% summarise(lambda_all = mean(lambda_all), 
                                              lambda_noRI = mean(lambda_noRI))

pd2$lambda_modRI <- exp(pd2$lambda_noRI-.5)

sum(pd2$lambda_modRI) #shootings per year
sum(M5$fitted.values)/6 #compared to the fitted number using estimated RIs
frac <- sum(pd2$lambda_modRI)/(sum(M5$fitted.values)/6) 

pd <- data.frame(reduction = r, increase = i, fraction_remaining = frac) %>% gt()
gtsave(pd, "Output/Model Checks/NegBin/hypotheticals.html")

