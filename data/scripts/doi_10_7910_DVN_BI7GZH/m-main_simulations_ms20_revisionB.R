#######################################################################################################
# Model and some basic model checks
# In simulations: time 0 of model corresponds to start time of mass vaccination (1950 in the US) 
# In plots: time 0 corresponds to the start of maternal vaccination.
#######################################################################################################
rm(list = ls())
setwd("~/Documents/_codes/_ms")
source("s-base_packages.R")
source("f-CreatePompMod.R")
source("f-ReformatSims.R")
library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)
library(pomp) #pomp dependencies do not work under 4.2.2.
library(viridis)
library(RColorBrewer)
library(ggnewscale)
library(data.table)
library(ggpubr) #for ggarrange; might need to install package 'vctrs' first if using lock file.
library(lemon) #for axis on facet_wrap and facet_grid 
library(stringr)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # For manuscript
theme_set(theme_classic(base_size = 18) + theme(panel.grid.minor = element_blank())) # For presentation
# renv::init() #to  initialize a lockfile
# renv::snapshot() #to get a snapshot the lockfile
# renv::restore() # to restore  lockfile

#######################################################################################################
# To run a few stochastic (or determinsitic) simulations. 
# For data with 100 simulations as in the manuscript, run code from the cluster and read in below
#######################################################################################################
# For stochastic models with 100 sims, go right away to the 'Get data' part around l. 104 
# To run a stochastic model with few simulations, see continue with the code below
type_sim <- "stoch" # Type of simulation: "det" (deterministic) or "stoch" (stochastic)
theta <- readRDS(file = "_data/model_pars_709090.rds") # Model parameters, make sure to adjust according to question!
debug_bool <- T

# Create age categories and test data ------------------------------------------
agecats_nm <- c("[0,0.17)", "[0.17,1.5)", "[1.5,2)", paste0("[", 1:73, ",", 2:74, ")")) # Age classes in the model
nages_mod <- length(agecats_nm) # 76 age categories
time_test <- seq(-100, 200, by = 1) # 1/12 for per month
data_test <- matrix(NA, nrow = length(time_test), ncol = nages_mod + 1, 
                    dimnames = list(NULL, c("time", paste0("CIobs_", 1:nages_mod)))) %>% 
  as.data.frame(.) %>%
  mutate(time = time_test)

# Create covariates ----------------------------
covars <- data.frame(time = seq(-100, 200, by = 1e-3))
STM_pars <- readRDS(sprintf("_data/mle_waning_model_%s_STM.rds", type_sim))
omegaC_vec <- STM_pars[c("omegaC1", "omegaC2")]
omegaT_vec <- STM_pars[c("omegaT1", "omegaT2")]
nbases <- length(omegaC_vec) + 1
omegaC_vec <- c(omegaC_vec, -sum(omegaC_vec))
omegaT_vec <- c(omegaT_vec, -sum(omegaT_vec))
seas_bases <- periodic.bspline.basis(x = covars$time, 
                                     nbasis = nbases, 
                                     degree = 3, 
                                     period = 1)
covars <- covars %>% 
  mutate(seasC = exp(seas_bases %*% omegaC_vec), 
         seasT = exp(seas_bases %*% omegaT_vec))

if(debug_bool) {
  matplot(covars$time, covars[, c("seasC", "seasT")], type = "l", lwd = 2, lty = 1,
          col = c("yellow", "green"), 
          xlab = "Time (months)", 
          ylab = "Seasonal forcing", 
          bty = "l", 
          #xlim = c(2000, 2001),
          las = 1)
  legend("topleft", c("5-10 y", "10-20 y"), col = c("yellow", "green"), lty = 1, lwd = 2, bty = "n")
}

# Create POMP model and parametrize it--------------------------------------------------------
# install.packages("pomp",repos="https://kingaa.github.io/")
library(pomp)
pMod <- CreatePompMod(nages_mod = nages_mod, 
                      nages_cmat = 15L, 
                      add_preschool_booster = F, 
                      dat_df = data_test, 
                      covars_df = covars, 
                      debug_bool = F)
saveRDS(pMod, file = "pMod709090.rds")
coef(pMod, names(theta)) <- unname(theta)

# Simulations without or with maternal immunization -------------------------------------------
# Parameters to change: v0, b1, b2
coef(pMod, "tau") <- log(2) / (180 / 365) #Maternal VE at 81%
p_mat <- parmat(params = coef(pMod), nrep = 4)
tM <- unname(coef(pMod, "tM"))
p_mat[c("v0", "b1", "b2"), 1] <- c(0.70, 0, 0) #Maternal immunization/blunting scenario
p_mat[c("v0", "b1", "b2"), 2] <- c(0.70, 0.05, 0) #Maternal immunization/blunting scenario
p_mat[c("v0", "b1", "b2"), 3] <- c(0.70, 0.10, 0) #Maternal immunization/blunting scenario
p_mat[c("v0", "b1", "b2"), 4] <- c(0.70, 0.20, 0) #Maternal immunization/blunting scenario

# Simulations in wide format
# Trajectory is for deterministic; simulate for stochastic
tjW <- simulate(nsim=100, object = pMod, 
                  params = p_mat, 
                  format = "data.frame") 
tjW <- trajectory(object = pMod, 
                  params = p_mat, 
                  format = "data.frame")
write.csv(tjW,"_matrix/709090_tjW", row.names = FALSE)

# Simulations in long format
tjL <- ReformatSims(sims_df = tjW, agecats_nm = agecats_nm)
tjL$age_no <- as.numeric(tjL$age_no)
summary(tjL)
write.csv(tjL,"_matrix/709090_tjL", row.names = FALSE)

#######################################################################################################
# Data for stochastic models having 100 sims from cluster
#######################################################################################################
# Get data for main results
# Main results: 70% maternal vaccination coverage, 90% for infants
tjL_7090 <- fread("_cluster/709090_b1_tjL") # 70% maternal vaccination coverage, 90% for infants

# Sensitivity analysis 1: Change maternal immunization coverage
tjL_5090 <- fread("_cluster/509090_b1_tjL") # 50% maternal vaccination coverage, 90% for infants
tjL_9090 <- fread("_cluster/909090_b1_tjLB3") # 90% maternal vaccination coverage, 90% for infants; the B3 is referring to three random sampling from binomial distributions at birth, which is the case for the scenarios below

# Sensitivity analysis 2: Change infant coverage of primary and booster immunization
tjL_7070 <- fread("_cluster/707070_b1_tjL") # 70% maternal vaccination coverage, 70% for infants
tjL_7080 <- fread("_cluster/708080_b1_tjL") # 70% maternal vaccination coverage, 80% for infants

# Sensitivity analysis 3: Change half life of duration of maternal immunization
tjL_7090_3mo <- fread("_cluster/709090_b1_tjL_3mo") #half life of maternally derived immunity of 3 months
tjL_7090_9mo <- fread("_cluster/709090_b1_tjL_9mo") #half life of maternally derived immunity of 9 months

# Sensitivity analysis 4: Change start of infant primary immunization
tjL_7090_1mo <- fread("_cluster/709090_b1_tjL_1mo") #primary series starting at 2 months
tjL_7090_4mo <- fread("_cluster/709090_b1_tjL_4mock") #primary series starting at 4 months
tjL_7090_4mo$age_nm2 <- ifelse(tjL_b1_7090_4mo$age_nm=="[0.25,1.5)", '[0.25,1.5)', tjL_b1_7090_4mo$age_nm2) #I made a little bug in the cluster code
tjL_7090_9mo <- fread("_cluster/709090_b1_tjL_9mo") #primary series starting at 9 months

# Sensitivity analysis 5: Change contact matrix
tjL_poland <- fread("_cluster/709090_b1_tjL_poland") #contact matrix for Poland
tjL_nl <- fread("_cluster/709090_b1_tjL_nl") #contact matrix for the Netherlands
tjL_be <- fread("_cluster/709090_b1_tjL_be") #contact matrix for Belgium
tjL_it <- fread("_cluster/709090_b1_tjL_italy") #contact matrix for Italy
tjL_lux <- fread("_cluster/709090_b1_tjL_lux") #contact matrix for Luxembourg
tjL_fin <- fread("_cluster/709090_b1_tjL_finland") #contact matrix for Finland
tjL_fr <- fread("_cluster/709090_b1_tjL_france") #contact matrix for France

# Sensitivity analysis 6: Starting maternal immunization 60 years after the start of primary immunization
tjL_60yrs <- fread("_cluster/709090_b1_tjL_60yrs") 

#######################################################################################################
# Main results with:
# (i) maternal immunization coverage at 70 and 
# (ii) infant primary and booster coverage at 90%
# (iii) primary series starting at 3 months 
# (iv) half life of maternal immunity at 6 months 
# (v) UK contact matrix
# (vi) start of maternal immunization 100 years after start of primary immunization 
# Followed by 6 sensitivity analyses with variation in these 6 parameters
#######################################################################################################
# Aggregate into broader age groups
tjL2 <- tjL_7090 %>% # Fill in the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() # First database
tjL2 <- tjL2 %>% filter(!grepl("2", .id)) #exclude the 5% blunting

tjL2_b2 <- tjL_7080 %>% # For a second file, fill in the second coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() # For plots with a second database
tjL2_b2 <- tjL2_b2 %>% filter(!grepl("2", .id)) #exclude the 5% blunting

# Aggregate across all age groups
tjL_all <- tjL_b1_7090 %>% 
  group_by(.id, time, var_nm, var_type) %>% 
  summarise(n = sum(n), N_age = sum(N_age)) %>% 
  ungroup()

#######################################################################################################
# Figures for check 
#######################################################################################################
#Check 1: Plot population size
head(tjL2) #Get tjL2 and tjL_all from code above
head(tjL_all) #Get tjL2 and tjL_all from code above
df_tidy_mean <- tjL_all %>%
  group_by(time) %>%
  summarise(n = n(),
            mean = mean(N_age),
            median = median(N_age),
            sd = sd(N_age),
            qlower = quantile(N_age, 0.025),
            qupper = quantile(N_age, 0.975))
summary(df_tidy_mean)
theme_set(theme_classic(base_size = 12))
pl <- ggplot() + 
  geom_ribbon(data = df_tidy_mean, aes(x = time, ymin=qlower, ymax=qupper),color="grey70", alpha=0.4) +
  geom_line(data = df_tidy_mean, mapping = aes(x = time, y = median), size=0.5) + 
  geom_line(data = tjL_all %>% filter(.id=='1_63'), mapping = aes(x = time, y = N_age), size=0.5, color='darkorchid2') + 
  labs(x = "Time (years)", y = "Population size") +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed") 
print(pl)
ggsave(filename = "_figsms4/check_popsize.pdf", width = 6, height = 5)

#Check 2: Plot incidence all ages combined with blunting (no individual simulation here, plot becomes unreadable)
#this is based on the same df_tidy_mean as above
head(tjL2) #Obtain this from the code above 
library(stringr)
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
head(tjL2b)
head(tjL_all)
df_tidy_mean <- tjL2b %>%
  group_by(blunt, time, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
summary(df_tidy_mean)
pl<- ggplot() +
  geom_line(data = df_tidy_mean %>% filter(var_nm=='CItot'), mapping = aes(x = time, y = median, color=factor(blunt))) + 
  scale_colour_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%' , '20%')) +
  labs(x = "Time since start infant vaccination (years)", y = "Incidence (per yr, per 100,000)") + 
  scale_y_log10() + xlim(-50, 200) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed") 
print(pl)
ggsave(filename = "_figsms4/check_incid3blunt.pdf", width = 6, height = 5)

#######################################################################################################
# Fig 1: Prisma literature search
#######################################################################################################
# Fig 1 is a Word document

#######################################################################################################
# Fig 2: Plot empirical estimates of RR 
#######################################################################################################
#mean<-c(82, 69, 29, 68, 43, -21, 81, 6, 66, 26, -1, 83.5, 55, 15.3)
#lci<-c(65, 8, -112, 37, -73, -242, 43, -165, 5, -80, -192, 74, 9, -13)
#uci<-c(91, 90, 76, 83, 81, 57, 94, 67, 88, 69, 65, 90, 77.5, 38)
rrmean<-c(0.19, 0.44, 0.54, 0.35, 0.80, 0.94, 0.19, 0.94, 0.34, 0.76, 1.50, 0.18, 0.52, 0.84, 0.26, 0.73, 0.71)
rrlci<-c(0.09, 0.14, 0.15, 0.16, 0.25, 0.28, 0.06, 0.33, 0.12, 0.28, 0.32, 0.11, 0.25, 0.61, 0.11, 0.39, 0.38)
rruci<-c(0.43, 1.33, 1.96, 0.76, 2.56, 3.16, 0.58, 2.65, 0.96, 2.04, 7.00, 0.29, 1.11, 1.15, 0.67, 1.34, 1.32)
time<-c(3.2, 3, 2.8, 2, 6, NA, NA, NA)
dose1<-c(1,2,3,1,2,3,1,2,3,1,2,1,2,3,1,2,3)
dose2<-c(0.9,1.9,2.9,0.95,1.95,2.95,1,2,3,1.1,2.1,1.05,2.05,3.05,1.13,2.13,3.13)
author<-c('UK_3yrs','UK_3yrs','UK_3yrs','UK_3yrs','UK_3yrs','UK_3yrs','California_4yrs','California_4yrs','California_4yrs','Australia_2yrs','Australia_2yrs', 'UK_6yrs','UK_6yrs','UK_6yrs', 'Weighted_mean', 'Weighted_mean', 'Weighted_mean')
#names<-c('Amir_2016A_dose1', 'Amir_2016A_dose2', 'Amir_2016A_dose3', 'Amir_2016B_dose1', 'Amir_2016B_dose2', 'Amir_2016B_dose3', 'Baxter_2017_dose1', 'Baxter_2017_dose2', 'Baxter_2017_dose3','Rowe_2021_dose1', 'Rowe_2021_dose2', 'Amir_2022_dose1', 'Amir_2022_dose2', 'Amir_2022_dose3')
data4<-cbind(rrmean, rrlci, rruci, dose1, dose2, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$dose2<-as.numeric(as.character(data4$dose2))
data4$author<-factor(data4$author, levels=c("Australia_2yrs", "California_4yrs", "UK_3yrs", "UK_6yrs", "Weighted_mean"))
summary(data4)
#data4$rrmean<-1-(data4$mean/100)
#data4$rrlci<-1-(data4$lci/100)
#data4$rruci<-1-(data4$uci/100)

theme_set(theme_classic(base_size = 12)) # For presentations: theme_set(theme_classic(base_size = 28)) 
meta <- ggplot(data4, aes(x=dose2, y=rrmean, color=author)) +
  geom_hline(yintercept = 1, linetype="dashed", size=1) +
  geom_linerange(aes(ymin=rrlci, ymax=rruci), size=0.5) + 
  geom_point(size=3) + 
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1", "black")) +
  scale_x_continuous('Dose of infant primary immunization', breaks=c(1, 2, 3), minor_breaks = NULL) +
  scale_y_continuous("Relative risk of pertussis", minor_breaks = NULL) + theme(legend.title.align = 0.5,legend.direction = "vertical", legend.box.just = "center") #+ theme(legend.position = "none") 
print(meta)
ggsave(filename = "_figsms4/Fig2_empiric6_rebuttal_legend.pdf", width = 6, height = 5)

#######################################################################################################
# Fig 2: Plot empirical estimates of RR for poster
#######################################################################################################
theme_set(theme_classic(base_size = 28))
fig2_poster <- ggplot(data4, aes(x=dose2, y=rrmean, color=author)) +
  geom_hline(yintercept = 1, linetype="dashed", size=1) +
  geom_linerange(aes(ymin=rrlci, ymax=rruci), size=0.5) + 
  geom_point(size=6) + 
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1", "black")) +
  scale_x_continuous('Dose of infant primary immunization', breaks=c(1, 2, 3), minor_breaks = NULL) +
  scale_y_continuous("Relative risk \nof pertussis", minor_breaks = NULL) + 
  theme(legend.title.align = 0.5,legend.direction = "vertical", legend.box.just = "center", legend.text = element_text(size = 18), legend.title = element_text(size = 18),) # + theme(legend.position = "none")
fig2_poster <- fig2_poster + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
fig2_poster
ggsave(filename = "_figsms4/Fig2_empiric6_poster_rebuttal.pdf", plot=fig2_poster, width = 8, height = 7)
ggsave(filename = "_figsms4/Fig2_empiric6_poster_rebuttal_legend.pdf", plot=fig2_poster, width = 8, height = 7)

#######################################################################################################
# Fig 3 to show honeymoon effects and susceptible dynamics
# Plots of stochastic models with ribbon formats for 100 simulations per scenario
#######################################################################################################
#Fig 3A_1sim: plot incidence all ages combined without blunting for 1 random simulation
tjL_all$n2 <- 100000 * tjL_all$n / tjL_all$N_age
fig3a<- ggplot() +
  geom_line(data = tjL_all %>% filter(.id=='1_63' & var_nm=='CItot'), mapping = aes(x = time, y = n2), size=0.5, color='darkorchid2') + 
  labs(x = "Time (years)", y = "Incidence (per yr, per 100,000)") +  
  xlim(-50, 200) +
  scale_y_log10(limits=c(10, 3000)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed") 
print(pl1)
ggsave(filename = "_figsms4/Fig3A_incid1.pdf", plot=fig3a,  width = 6, height = 5)

#Fig 3A_100sims: plot incidence all ages combined without blunting for 100 simulations
head(tjL_all) #Obtain this from the code above 
data_id=str_split_fixed(tjL_all$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL_all2<-cbind(data_id, tjL_all)
df_tidy_mean <- tjL_all2 %>%
  group_by(blunt, time, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
head(df_tidy_mean)
pl2<- ggplot() +
  geom_ribbon(data = df_tidy_mean %>% filter(blunt=='1' & var_nm=='CItot'), aes(x = time, ymin=qlower, ymax=qupper), color="grey70", alpha=0.4) +
  geom_line(data = df_tidy_mean %>% filter(blunt=='1'& var_nm=='CItot'), mapping = aes(x = time, y = median), color='black', size=0.5) + 
  #geom_line(data = tjL_all %>% filter(.id=='1_63' & var_nm=='CItot'), mapping = aes(x = time, y = 1e5 * n / N_age), size=0.5, color='darkorchid2') + 
  labs(x = "Time (years)", y = "Incidence (per yr, per 100,000)") +  
  xlim(-50, 200) +
  scale_y_log10(limits=c(10, 3000)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed") 
print(pl2)
ggsave(filename = "_figsms4/Fig3A_incid100.pdf", plot=pl2,  width = 6, height = 5)

#Fig 3B: Reproduce susceptible build up as in JAMA Ped paper
tjLS <- tjL_7090 %>% filter(.id=='1_63') # Choose one random simulation
tjLS <- tjLS %>% filter(var_nm=='S'|var_nm=='SV'|var_nm=='SMbar'|var_nm=='SVMbar'| var_nm=='SVM') # For all susceptibles: |var_nm=='SMbar'|var_nm=='SVMbar'| var_nm=='SVM'
tjLS2 <- tjLS %>% group_by(time, age_no, N_age) %>% summarise(n = sum(n)) %>% ungroup()
tjLS2$age_no <-as.numeric(as.character(tjLS2$age_no))
tjLS2 <- as.data.frame(tjLS2)
tjLS2$fraction <- tjLS2$n/tjLS2$N_age
summary(tjLS2)

#Plot susceptible dynamics
fig3b<-ggplot() + 
  geom_tile(data=tjLS2, aes(x=time,y=age_no, fill=fraction)) +
  scale_fill_gradient("Fraction \nsusceptible", low = "white", high = "purple") +
  theme(legend.title.align=0.5) +
  xlim(-50, 100) + # 200 to include maternal immunization
  labs(x = "Time (years)", y = "Age (years)", title = "") + 
  geom_vline(xintercept = 0, linetype = "dashed") # + geom_vline(xintercept = 100, linetype = "dashed") 
fig3b
ggsave(filename = "_figsms4/Fig3B_susceptibles_poster.pdf", plot = fig3b, width = 6, height = 5)

#Plot all dynamics together (S4A & S4C)
fig3_all <- ggarrange(fig3a, fig3b, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2, font.label=list(face = "plain", color="black", size=11))
fig3_all
ggsave(filename = "_figsms4/Fig3_all.pdf", plot = p4, width = 6, height = 8)

#######################################################################################################
# Fig 4: Incidence for all infants (2 age classes), incidence for infants from vaccinated mothers
#######################################################################################################
# Get data
head(tjL2) #Obtain this from the code above 
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
summary(tjL2b)

# Fig 4A: Incidence in newborns 
tjL2c <- tjL2b %>% filter(age_nm2=='[0,0.17)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>89 & time<126)
summary(df_tidy_meanb)

fig4A <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="0-2 months - All infants") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(fig4A)
ggsave(filename = "_figsms4/Fig4A_incid_age1.pdf", plot = fig4A, width = 6, height = 5)

# Fig 4B: Incidence in infant prim immunization
tjL2c <- tjL2b %>% filter(age_nm2=='[0.17,1.5)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>89 & time<126)
summary(df_tidy_meanb)

# Select the second age class for Fig 4B
fig4B <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - All infants") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center") 
print(fig4B)
ggsave(filename = "_figsms4/Fig4B_incid_age2.pdf", plot = fig4B, width = 6, height = 5)

# Fig 4C: Infants from unvaccinated mothers
# Get data per type of infection
tjL2age2<-tjL2b %>% filter(age_nm2=="[0.17,1.5)")
t3 <- subset(tjL2age2, var_nm == "CI3") #SV
summary(t3)
n3 <- t3 %>%
  group_by(blunt, time) %>%
  summarise( mean = mean(n),
             median = median(n),
             sd = sd(n),
             qlower = quantile(n, 0.05, na.rm=TRUE),
             qupper = quantile(n, 0.95, na.rm=TRUE))
summary(n3)

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
pl_unvacc <- ggplot() + 
  geom_ribbon(data = n3 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = n3  %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=1) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization (years)", y = "Incidence (per yr, per 100,000)") +
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - Infants from unvaccinated mothers") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(pl_unvacc)
ggsave(filename = "_figsms4/Fig4C_unvacc.pdf", plot = pl_unvacc, width = 6, height = 5)

# Fig 4D: Incidence in infants from vaccinated mothers
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2<-tjL2b %>% filter(age_nm2=="[0.17,1.5)")
tjL4num<-subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4num<-tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
summary(tjL4num)
names(tjL4num)[6]<-'num'
tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
summary(tjL4denom)
names(tjL4denom)[6]<-'denom'
tjL4ve<-cbind(tjL4num, tjL4denom[,6])
summary(tjL4ve)

num <- tjL4ve %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            mean = mean(num),
            median = median(num),
            sd = sd(num),
            qlower = quantile(num, 0.05, na.rm=TRUE),
            qupper = quantile(num, 0.95, na.rm=TRUE))
summary(num)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
pl_num <- ggplot() + 
  geom_ribbon(data = num %>% filter(time >= 90 & time < 200), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = num  %>% filter(time >= 90 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=1) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization (years)", y = "Incidence (per yr, per 100,000)") +
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - Infants from vaccinated mothers") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(pl_num)
ggsave(filename = "_figsms4/Fig4D_num.pdf", plot = pl_num, width = 6, height = 5)

# Fig 4all: Four figs together
fig4all <- ggarrange(fig4A, fig4B, pl_unvacc, pl_num, 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2, font.label=list(face = "plain", color="black", size=11),
                 common.legend = TRUE, legend="right")
fig4all
ggsave(filename = "_figsms4/Fig4_incid_4panels_v1.pdf", width = 10, height = 8)

#######################################################################################################
# Fig 4 ages 1 and 2 with blunting for presentation
#######################################################################################################
# Get data
head(tjL2) #Obtain this from the code above 
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
summary(tjL2b)

# Fig 4A: Incidence in newborns 
tjL2c <- tjL2b %>% filter(age_nm2=='[0,0.17)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>89 & time<126)
summary(df_tidy_meanb)

theme_set(theme_classic(base_size = 18))
fig4A <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(fig4A)
ggsave(filename = "_figsms4/Fig4A_incid_poster.pdf", plot = fig4A, width = 7, height = 5)

# Fig 4B: Incidence in infant prim immunization
tjL2c <- tjL2b %>% filter(age_nm2=='[0.17,1.5)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>89 & time<126)
summary(df_tidy_meanb)

# Select the second age class for Fig 4B
theme_set(theme_classic(base_size = 18))
fig4B <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(fig4B)
ggsave(filename = "_figsms4/Fig4B_incid_poster.pdf", plot = fig4B, width = 7, height = 5)

require(grid)
fig4all <- ggarrange(fig4A + rremove("xlab"), fig4B + rremove("xlab"), 
                     ncol = 2, nrow = 1, font.label=list(face = "plain", color="black", size=11),
                     common.legend = TRUE, legend="right")
fig4all <- annotate_figure(fig4all, bottom = textGrob("Time since start of maternal immunization program (years)", gp = gpar(cex = 1.4)))
fig4all
ggsave(filename = "_figsms4/Fig4_incid_2panels_poster.pdf", width = 10, height = 5)

#######################################################################################################
# Fig 4 all ages with blunting for presentation
#######################################################################################################
# Get data
head(tjL2) #Obtain this from the code above 
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
summary(tjL2b)

# Fig incidence in population 
tjL2c <- tjL2b %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
summary(df_tidy_meanb)

df_tidy_meanb  <- df_tidy_meanb %>% filter(blunt==1)
fig4age <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time, ymin=qlower, ymax=qupper), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time, y = median)) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start infant immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 50) + scale_y_log10(limits=c(10, 10000))+ 
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center") + theme(legend.position = "none") 
print(fig4age)
ggsave(filename = "_figsms4/Fig4A_incid_ageall.pdf", plot = fig4age, width = 6, height = 5)

tjL2d <- tjL2c %>% filter(blunt==1)
tjL2e <- tjL2d %>% filter(sim==63 | sim==33 | sim==3)
tjL2e <- tjL2e %>% filter(var_nm=='CItot')
summary(tjL2e)
df_tidy_meanb <- tjL2e %>%
  group_by(time, sim) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
summary(df_tidy_meanb)

fig4age <- ggplot() +
  geom_line(data = df_tidy_meanb, mapping = aes(x = time, y = median, colour=factor(sim))) + 
  scale_color_brewer(palette = "Reds", "Sims") +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start infant immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 50) + scale_y_log10(limits=c(50, 2000))+ 
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center") # + theme(legend.position = "none") 
print(fig4age)
ggsave(filename = "_figsms4/Fig4A_incid_ageall2.pdf", plot = fig4age, width = 6, height = 5)

#######################################################################################################
# Fig 4 age 1 without blunting for presentation
#######################################################################################################
# Get data
head(tjL2) #Obtain this from the code above 
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
summary(tjL2b)

# Fig 4A: Incidence in newborns 
tjL2c <- tjL2b %>% filter(age_nm2=='[0,0.17)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
tjL2c <- tjL2c %>% filter(blunt==1)
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>89 & time<126)
summary(df_tidy_meanb)

fig4A <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper), alpha=0.6) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median)) + 
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=13)) 
print(fig4A)
ggsave(filename = "_figsms4/Fig4A_incid_age1_poster.pdf", plot = fig4A, width = 6, height = 5)

#######################################################################################################
# Fig 5: RR (cumulative)
#######################################################################################################
tjL_7090 <- fread("_cluster/709090_b1_tjL") # 70% maternal vaccination coverage, 90% for infants

# Aggregate into broader age groups
tjL2 <- tjL_7090 %>% #Fill in the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL2 <- tjL2 %>% filter(!grepl("2", .id)) #exclude the 5% blunting; 0-10-20 are fine for this plot
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
head(tjL2b)
write.csv(tjL2b, "_figsms4/Fig5_data_tjL2b", row.names = FALSE)

# Fig 5A Manuscript: plot ribbons for first age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2b <- fread("_figsms4/Fig5_data_tjL2b")
tjL2age2 <- tjL2b %>% filter(age_nm2=="[0,0.17)") #tjL2b from above (check 4)
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI1" | var_nm == "CI2") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

df_ve1 <- tjL4 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))

#theme_set(theme_classic(base_size = 28) + theme(panel.grid.minor = element_blank())) #for presentations
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig5A <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  #theme(legend.title=element_text(size=24), legend.text=element_text(size=21)) + #for presentations
  ylim(0, 0.25) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
fig5A
# ggsave(filename = "_figsms4/Fig5A_RR_age1.pdf", plot = fig5A, width = 6, height = 5)

# Fig 5B Manuscript: plot ribbons for second age class
# Get ve cumulative per blunting level
# Step 1: get instantaneous error
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL2b %>% filter(age_nm2=="[0.17,1.5)") #tjL2b from above (check 4)
#tjL2age2 <- tjL50b %>% filter(age_nm2=="[0.17,1.5)") #for 50% coverage
#tjL2age2 <- tjL90b %>% filter(age_nm2=="[0.17,1.5)") #for 90% coverage
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)
summary(df_veall)

# Fig 5B manuscript
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig5B <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
fig5B
ggsave(filename = "_figsms4/Fig5B_RR_age2.pdf", plot = fig5B, width = 6, height = 5)

# Fig 5C: Plot with error bars
# Empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, time, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$time<-as.numeric(as.character(data4$time))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
# Simulation data
# For the selection of monitoring years, check with:
# tjL2check <- tjL2 %>% filter(var_nm=="M" & age_yr<=2 & .id==("1_1"))
# This check shows that at year 100, maternal compartments = 0, so yr 101 = 1 yr of data, yr 102 = 2 yrs, etc.
df_yr2 <- df_ve50 %>% filter(time == 102) # sample size 11, 
df_yr3 <- df_ve100 %>% filter(time == 103) # sample size 14
df_yr6a <- df_ve1000a %>% filter(time == 106) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time==106, 104, NA) # for aesthetic reasons, replace 6 yrs with 4, then manually replace 6 in pdf
df_yr6 <- df_ve1000 %>% filter(time == 106) # sample size 169
df_yr6$time <- ifelse(df_yr6$time==106, 105, NA) # for aesthetic reasons, replace 6 yrs with 5, then manually replace 6 in pdf
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
fig5C <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 7) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center")
fig5C
# ggsave(filename = "_figsms4/Fig5C_RR_rebuttal.pdf", plot = fig5C, width = 6, height = 5)

# Fig 5 ABC: Combine  into one figure
fig5all <- ggarrange(fig5A, fig5B, fig5C,
                 labels = c('A', 'B', 'C'),
                 ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11))
fig5all
ggsave(filename = "_figsms4/Fig5_3panels_rebuttal.pdf", plot = fig5all, width = 6, height = 10) # Adjust legends manually to make Fig5_3panels_v2

#######################################################################################################
# Fig 5 for poster
#######################################################################################################
# Fig 5B Poster: Plot ribbons without empirical data
theme_set(theme_classic(base_size = 28) + theme(panel.grid.minor = element_blank()))
fig5b_poster <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Years since start of maternal immunization program", y = "Relative risk of pertussis") +
  ylim(0, 3.5) + labs(title="3-18 months") +
  #new_scale_colour() +
  #geom_point(data=data4, mapping = aes(x = time+0.15, y = rrmean, color=Study), size=2.5) +
  #geom_errorbar(data = data4, aes(x = time+0.15, ymin=rrlci, ymax=rruci, color=Study, width = 0), width=0) +
  #scale_colour_manual("Empirical Study", values=c("#00CC00","black", "#F9A825", "royalblue1")) + 
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") #+ theme(legend.position = "none") + theme(legend.title=element_text(size=28), legend.text=element_text(size=24)) # to make plot without legend
fig5b_poster
fig5b_posterlegend<- ggdraw(align_legend(fig5b_poster)) #to center legend, see custom function below
fig5b_posterlegend
ggsave(filename = "_figsms4/Fig5B_presentation.pdf", plot=fig5b_poster, width = 8, height = 5)
ggsave(filename = "_figsms4/Fig5B_poster_v1.pdf", plot=fig5b_poster, width = 8, height = 7)
ggsave(filename = "_figsms4/Fig5B_posterlegend.pdf", plot=fig5b_posterlegend, width = 8, height = 7)

# Fig 5B Poster: Plot ribbons with empirical data but without sample size
rrmean<-c(0.44, 0.54, 0.80, 0.94, 0.94, 0.34, 1.50, 0.52, 0.841)
rrlci<-c(0.14, 0.15, 0.25, 0.28, 0.33, 0.12, 0.32, 0.25, 0.61)
rruci<-c(1.33, 1.96, 2.56, 3.16, 2.65, 0.96, 3.5, 1.11, 1.15)
time<-c(2.8, 3, 3.2, 3.4, 3.9, 4.1, 2, 5.9, 6.1) 
dose1<-c(2, 3, 2, 3, 2, 3, 2, 2, 3)
author<-c('UK_3yrs','UK_3yrs','UK_3yrs','UK_3yrs','California','California','Australia', 'UK_6yrs','UK_6yrs')
# Could change California to 6 yrs because it has 2 yrs pre-recommendation data with very low maternal coverage
data4<-cbind(time, rrmean, rrlci, rruci, dose1, author)
data4<-data.frame(data4)
data4$time<-as.numeric(as.character(data4$time))
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
theme_set(theme_classic(base_size = 20) + theme(panel.grid.minor = element_blank()))
fig5b_poster <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) + labs(title="3-18 months") + theme(plot.title = element_text(size=20)) +
  new_scale_colour() +
  geom_point(data = data4, mapping = aes(x = time, y = rrmean, color=author), size=2.5) +
  geom_errorbar(data = data4, aes(x = time, ymin = rrlci, ymax = rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Empirical Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) + 
  theme(legend.title=element_text(size=14), legend.text=element_text(size=14)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") #+ theme(legend.position = "none") + theme(legend.title=element_text(size=28), legend.text=element_text(size=24)) # to make plot without legend
fig5b_poster
ggsave(filename = "_figsms4/Fig5B_presentationC.pdf", plot=fig5b_poster, width = 8, height = 6)

# Fig 5C Poster: Plot with error bars
# Empirical data: See above
# Simulation data
df_yr2 <- df_ve50 %>% filter(time == 102)
df_yr3 <- df_ve100 %>% filter(time == 103)
df_yr6 <- df_ve100 %>% filter(time == 106)
df_yr6$time <- ifelse(df_yr6$time==106, 104, 104) #for aesthetic reasons, replace 6 yrs with 4
df_yr <-rbind(df_yr2, df_yr3, df_yr6)
summary(df_yr)
head(df_yr)
theme_set(theme_classic(base_size = 32) + theme(panel.grid.minor = element_blank()))
fig5c_poster <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=12, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Years since start of maternal immunization", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=Study), size=10) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=Study, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","black", "#F9A825", "royalblue1")) +
  ylim(0, 3.5) +
  annotate("text", x=2, y=0, label= "n=20", size=8) + #11
  annotate("text", x=3, y=0, label= "n=65", size=8) + #14
  annotate("text", x=4, y=0, label= "n=820", size=8) + #21 and 169
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center") + theme(legend.title=element_text(size=28), legend.text=element_text(size=26))
fig5c_poster
fig5c_posterlegend<- ggdraw(align_legend(fig5c_poster)) #to center legend, see custom function below
fig5c_posterlegend
ggsave(filename = "_figsms4/Fig5C_poster_v1.pdf", plot=fig5c_posterlegend, width = 12, height = 7)

#######################################################################################################
# Fig S1: Model schematic
#######################################################################################################
# This is ppt/pdf

#######################################################################################################
# Fig S2: validation RR for the first age class 
#######################################################################################################
# Validation RR for the first age class in the deterministic model
# Run first part in m-check_model.R
coef(pMod, c("v0", "b1", "b2")) <- c(0.70, 0, 0) #Maternal immunization coverage/blunting scenario
p_mat <- parmat(params = coef(pMod), nrep = 6)
tM <- unname(coef(pMod, "tM"))
p_mat[c("tau"), 1] <- log(2) / (30 / 365) #Maternal immunization/blunting scenario
p_mat[c("tau"), 2] <- log(2) / (90 / 365) #Maternal immunization/blunting scenario
p_mat[c("tau"), 3] <- log(2) / (180 / 365) #Maternal immunization/blunting scenario
p_mat[c("tau"), 4] <- log(2) / (274 / 365) #Maternal immunization/blunting scenario
p_mat[c("tau"), 5] <- log(2) / (365 / 365) #Maternal immunization/blunting scenario
p_mat[c("tau"), 6] <- 0

tjW <- trajectory(object = pMod, 
                  params = p_mat, 
                  format = "data.frame")

tjL <- ReformatSims(sims_df = tjW, agecats_nm = agecats_nm)
tjL$age_no <- as.numeric(tjL$age_no)
summary(tjL)

tjL2 <- tjL %>% 
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup()
tjL2 <- subset(tjL2, time >= 95)
summary(tjL2)

# Fig the first age class
ppv=0.70 # Note the Farrington formula does not work at coverage ppv=1
tjL2age1 <- tjL2 %>% filter(age_nm2=="[0,0.17)")
tjL4num <- subset(tjL2age1, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, time, age_nm2) %>% summarise(sum(n))
summary(tjL4num)
names(tjL4num)[4]<-'num'
tjL4denom <- subset(tjL2age1, var_nm == "CI1" | var_nm == "CI2") 
tjL4denom <- tjL4denom %>% group_by(.id, time, age_nm2) %>% summarise(sum(n))
summary(tjL4denom)
names(tjL4denom)[4]<-'denom'
tjL4ve <- cbind(tjL4num, tjL4denom[,4])
summary(tjL4ve)
tjL4ve$pcv <- tjL4ve$num/tjL4ve$denom
tjL4ve$ve <- (tjL4ve$pcv/(1-tjL4ve$pcv)) * ((1-ppv)/ppv) 
tjL4ve2 <- transform(tjL4ve, vesum = ave(ve, .id, FUN = cummean))
summary(tjL4ve2)
theme_set(theme_classic(base_size = 28))
ggplot() + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  geom_line(data = tjL4ve2 %>% filter(time >= 100), mapping = aes(x = time-100, y = 1-vesum, color=as.factor(.id))) + 
  scale_x_log10() + ylim(0, 1) +
  labs(x = "Time since start of maternal immunization (years)", y = "Effectiveness of maternal immunization") +
  scale_color_brewer(palette = "Reds", labels=c('1', '3' ,'6', '9','12', 'Inf')) + guides(color=guide_legend(title="Half-life\n[months]"))
ggsave(filename = "_figsms4/FigS2_RR_validation2.pdf", width = 6, height = 5)

#######################################################################################################
# Fig S3: Plot incidence for all ages in 4 panels
#######################################################################################################
head(tjL2b) #get from above
tjL2b$age_nm3 <- ifelse(tjL2b$age_nm2 == '[20,40)', '[20+]', ifelse(tjL2b$age_nm2 == '[40,60)', '[20+]', ifelse(tjL2b$age_nm2 == '[60,75]', '[20+]', tjL2b$age_nm2)))
tjL2b$age_nm3 <- factor(tjL2b$age_nm3, levels = c('[0,0.17)', '[0.17,1.5)', '[1.5,5)', '[5,10)', '[10,20)', '[20+]'))
tjL2b_sub <- tjL2b %>% filter(age_nm3=='[1.5,5)'| age_nm3=='[5,10)'| age_nm3=='[10,20)'| age_nm3=='[20+]')
head(tjL2b_sub)
df_tidy_mean_sub <- tjL2b_sub %>%
  group_by(blunt, time, age_nm3, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_mean_sub <- df_tidy_mean_sub %>% filter(time>89 & time<126)
summary(df_tidy_mean_sub)

levels(df_tidy_mean_sub$age_nm3)[levels(df_tidy_mean_sub$age_nm3)=='[1.5,5)'] <- "1.5-5 years"
levels(df_tidy_mean_sub$age_nm3)[levels(df_tidy_mean_sub$age_nm3)=='[5,10)'] <- "5-10 years"
levels(df_tidy_mean_sub$age_nm3)[levels(df_tidy_mean_sub$age_nm3)=='[10,20)'] <- "10-20 years"
levels(df_tidy_mean_sub$age_nm3)[levels(df_tidy_mean_sub$age_nm3)=='[20+]'] <- "20+ years"
pl4 <- ggplot() +
  geom_ribbon(data = df_tidy_mean_sub %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = df_tidy_mean_sub %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-10, 25) + #scale_y_log10(limits=c(10, 10000)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~age_nm3, scales = 'free') #+ theme(axis.text.x=element_blank())
print(pl4)
ggsave(filename = "_figsms4/FigS3_incid_allages.pdf", width = 6, height = 5)

#######################################################################################################
# Fig S4: Instant vs. cumulative RR
#######################################################################################################
head(tjL2) #get from above
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
summary(data_id)
tjL2b<-cbind(data_id, tjL2)
head(tjL2b)
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2<-tjL2b %>% filter(age_nm2=="[0.17,1.5)")
tjL4num<-subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4num<-tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
summary(tjL4num)
names(tjL4num)[6]<-'num'
tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
summary(tjL4denom)
names(tjL4denom)[6]<-'denom'
tjL4ve<-cbind(tjL4num, tjL4denom[,6])
summary(tjL4ve)
tjL4ve$pcv<-tjL4ve$num/tjL4ve$denom
tjL4ve$ve<-(tjL4ve$pcv/(1-tjL4ve$pcv)) * ((1-ppv)/ppv) 
summary(tjL4ve)

df_tidy_mean <- tjL4ve %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            mean = mean(ve),
            median = median(ve),
            sd = sd(ve),
            qlower = quantile(ve, 0.05, na.rm=TRUE),
            qupper = quantile(ve, 0.95, na.rm=TRUE))
summary(df_tidy_mean)

# Plot S4A with instant RR
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
pl_instant <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_tidy_mean %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_tidy_mean  %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +
  labs(title="Instantaneous RR") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(pl_instant)
ggsave(filename = "_figsms4/FigS4_instant.pdf", width = 6, height = 9)

#Plot S4B: This is the same as Fig 5B
#Get data from 5B above
pl_cumul<- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + #Greens, Purples, Greys; see: https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="Cumulative RR") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) + 
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
pl_cumul

figs4 <- ggarrange(pl_instant, pl_cumul, 
                   labels = c("A", "B"),
                   ncol = 1, nrow = 2, font.label=list(face = "plain", color="black", size=11))
figs4 
ggsave(filename = "_figsms3/Figs4_instant_cumul.pdf", width = 6, height = 6)

#######################################################################################################
# Fig S5 & S6: Sensitivity analyses 1: 50 and 90% maternal coverage
#######################################################################################################
tjL_5090 <- fread("_cluster/509090_b1_tjL") # 70% maternal vaccination coverage, 90% for infants
tjL_5090 <- tjL_5090 %>% filter(age_nm2=="[0,0.17)" | age_nm2=="[0.17,1.5)") # This early selection is to speed things up
tjL50 <- tjL_5090 %>% #Insert the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() #for b1
tjL50 <- tjL50 %>% filter(!grepl("2", .id)) #exclude the 5% blunting
#tjL50 <- tjL50 %>% filter(!grepl("3", .id)) #exclude the 10% blunting
data_id=str_split_fixed(tjL50$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL50b<-cbind(data_id, tjL50)
tjL50b$n2<-1e5 * tjL50b$n / tjL50b$N_age
tjL50b<-tjL50b %>% filter(time>0)
tjL50b$group <- '50% maternal coverage'
write.csv(tjL50b,"_figsms4/FigS6_data_tjL50", row.names = FALSE)

tjL_9090 <- fread("_cluster/909090_b1_tjLB3") # 90% maternal vaccination coverage, 90% for infants; the B3 is referring to three random samplings from binomial distributions at birth
tjL_9090 <- tjL_9090 %>% filter(age_nm2=="[0,0.17)" | age_nm2=="[0.17,1.5)")
tjL90 <- tjL_9090 %>% #Insert the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() #for b1
tjL90 <- tjL90 %>% filter(!grepl("2", .id)) #exclude the 5% blunting
#tjL90 <- tjL90 %>% filter(!grepl("3", .id)) #exclude the 10% blunting
data_id=str_split_fixed(tjL90$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL90b<-cbind(data_id, tjL90)
tjL90b$n2<-1e5 * tjL90b$n / tjL90b$N_age
tjL90b<-tjL90b %>% filter(time>0)
tjL90b$group <- '90% maternal coverage'
write.csv(tjL90b,"_figsms4/FigS6_data_tjL90", row.names = FALSE)

# Fig S5 Incidence
df_50 <- tjL50b %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

df_90 <- tjL90b %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
head(df_50)
head(df_90)

df_50_sub<-df_50 %>% filter(age_nm2=='[0,0.17)'| age_nm2=='[0.17,1.5)') #[0,0.17) for 1 month
df_50_sub$age_nm2 <- as.factor(df_50_sub$age_nm2)
levels(df_50_sub$age_nm2)
levels(df_50_sub$age_nm2)[levels(df_50_sub$age_nm2)=="[0,0.17)"] <- "newborn"
levels(df_50_sub$age_nm2)[levels(df_50_sub$age_nm2)=="[0.17,1.5)"] <- "primary immunization"
head(df_50_sub)
df_90_sub<-df_90 %>% filter(age_nm2=='[0,0.17)'| age_nm2=='[0.17,1.5)')
df_90_sub$age_nm2 <- as.factor(df_90_sub$age_nm2)
levels(df_90_sub$age_nm2)
levels(df_90_sub$age_nm2)[levels(df_90_sub$age_nm2)=="[0,0.17)"] <- "newborn"
levels(df_90_sub$age_nm2)[levels(df_90_sub$age_nm2)=="[0.17,1.5)"] <- "primary immunization"
summary(df_90_sub)
df <- rbind(df_50_sub, df_90_sub)
df <- df %>% filter(time>89 & time<126)

figs5 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_ribbon(data = df %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization (years)", y = "Incidence (per yr, per 100,000)") + 
  facet_rep_grid(age_nm2 ~ group, scales = 'free') + 
  theme(axis.line=element_line()) + scale_x_continuous(limits=c(-10,15))
figs5
ggsave(filename = "_figsms4/FigS5_incid_mat5090.pdf", width = 6, height = 5)

# Fig S6 Relative Risk: Combine fig5 into one figure
# Fig S6A: plot ribbons for first age class
# Get ve cumulative per blunting level
ppv=0.50 #insert the maternal vaccination coverage here
tjL2age2 <- tjL50b %>% filter(age_nm2=="[0,0.17)") #tjL50b for 50% coverage, tjL90b for 90% coverage
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI1" | var_nm == "CI2") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

df_ve1 <- tjL4 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig50a <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 0.25) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="50% coverage\n0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig50a #fig90a

# Fig S6B Manuscript: plot ribbons for second age class
# Get ve cumulative per blunting level
ppv=0.50 #insert the maternal vaccination coverage here
tjL2age2 <- tjL50b %>% filter(age_nm2=="[0.17,1.5)") #for 50% coverage
#tjL2age2 <- tjL90b %>% filter(age_nm2=="[0.17,1.5)") #for 90% coverage
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
tjL4$pcv95  <- ifelse(tjL4$pcv95 == 1, 0.99, tjL4$pcv95) # rounding to avoid RR at infinity when sample sizes are low
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
tjL4$pcv975  = ifelse(tjL4$pcv975 == 1, 0.99, tjL4$pcv975)
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
tjL4$pcv1975a  = ifelse(tjL4$pcv1975a == 1, 0.99, tjL4$pcv1975a)
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169
tjL4$pcv1975  = ifelse(tjL4$pcv1975 == 1, 0.99, tjL4$pcv1975)
hist(tjL4$pcv95) # due to low sample sizes, many pcv = 1, which end up un RR = infinity
hist(tjL4$pcv1025a) # due to low sample sizes, many pcv = 1, which end up un RR = infinity
hist(tjL4$pcv1975a) # due to low sample sizes, many pcv = 1, which end up un RR = infinity

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)
summary(df_veall)

# Fig S6B manuscript
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig50b <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig50b #fig90b

# Fig S6C: Plot with error bars
# Empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, dose1, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
# Simulation data
df_yr2 <- df_ve50 %>% filter(time == 102) #sample size 11
df_yr3 <- df_ve100 %>% filter(time == 103) # sample size 14
df_yr6a <- df_ve1000a %>% filter(time == 106) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time==106, 104, NA) #for aesthetic reasons, replace 6 yrs with 4
df_yr6 <- df_ve1000 %>% filter(time == 106) # sample size 169
df_yr6$time <- ifelse(df_yr6$time==106, 105, NA) #for aesthetic reasons, replace 6 yrs with 5
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
fig50c <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 8.2) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center") + theme(legend.position = "none")
fig50c #fig90c

# Fig S6 ABC: Combine  into one figure
figS6_50 <- ggarrange(fig50a, fig50b, fig50c,
                     labels = c('A', 'B', 'C'),
                     ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS6_50

figS6_90 <- ggarrange(fig90a, fig90b, fig90c,
                      labels = c('D', 'E', 'F'),
                      ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS6_90

# Merging legends
plots <- ggarrange(figS6_50, figS6_90, legend = 'none', nrow =1, ncol = 2)
plots
legend <- get_legend(fig90c) 
figS6 <- ggarrange(plots, legend, widths = c(0.85, 0.15))
figS6
ggsave(filename = "_figsms4/FigS6_RR_mat5090.pdf", plot = figS6, width = 8, height = 8)

#######################################################################################################
#  Fig S7 & S8: sensitivity analysis 2: for 70 and 80% primary series coverage
#######################################################################################################
tjL_7070 <- fread("_cluster/707070_b1_tjL") # 70% vaccination coverage for infants
tjL_7070 <- tjL_7070 %>% filter(age_nm2=="[0,0.17)" | age_nm2=="[0.17,1.5)") # This early selection is to speed things up
tjL70 <- tjL_7070 %>% #Insert the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() #for b1
tjL70 <- tjL70 %>% filter(!grepl("2", .id)) #exclude the 5% blunting
data_id=str_split_fixed(tjL70$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL70b<-cbind(data_id, tjL70)
tjL70b$n2<-1e5 * tjL70b$n / tjL70b$N_age
tjL70b<-tjL70b %>% filter(time>0)
tjL70b$group <- '70% primary coverage'
write.csv(tjL70b,"_figsms4/FigS8_data_tjL70", row.names = FALSE)

tjL_7080 <- fread("_cluster/708080_b1_tjL") # 80% vaccination coverage for infants
tjL_7080 <- tjL_7080 %>% filter(age_nm2=="[0,0.17)" | age_nm2=="[0.17,1.5)") # This early selection is to speed things up
tjL80 <- tjL_7080 %>% #Insert the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() #for b1
tjL80 <- tjL80 %>% filter(!grepl("2", .id)) #exclude the 5% blunting
data_id=str_split_fixed(tjL80$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL80b<-cbind(data_id, tjL80)
tjL80b$n2<-1e5 * tjL80b$n / tjL80b$N_age
tjL80b<-tjL80b %>% filter(time>0)
tjL80b$group <- '80% primary coverage'
write.csv(tjL80b,"_figsms4/FigS8_data_tjL80", row.names = FALSE)

# Fig S7 Incidence
df_70 <- tjL70b %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

df_80 <- tjL80b %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

df_70_sub<-df_70 %>% filter(age_nm2=='[0,0.17)'| age_nm2=='[0.17,1.5)') #[0,0.17) for 1 month
df_70_sub$age_nm2 <- as.factor(df_70_sub$age_nm2)
levels(df_70_sub$age_nm2)
levels(df_70_sub$age_nm2)[levels(df_70_sub$age_nm2)=="[0,0.17)"] <- "newborn"
levels(df_70_sub$age_nm2)[levels(df_70_sub$age_nm2)=="[0.17,1.5)"] <- "primary immunization"
head(df_70_sub)
df_80_sub<-df_80 %>% filter(age_nm2=='[0,0.17)'| age_nm2=='[0.17,1.5)')
df_80_sub$age_nm2 <- as.factor(df_80_sub$age_nm2)
levels(df_80_sub$age_nm2)
levels(df_80_sub$age_nm2)[levels(df_80_sub$age_nm2)=="[0,0.17)"] <- "newborn"
levels(df_80_sub$age_nm2)[levels(df_80_sub$age_nm2)=="[0.17,1.5)"] <- "primary immunization"
summary(df_80_sub)
df <- rbind(df_70_sub, df_80_sub)
df <- df %>% filter(time>89 & time<126)

figs7 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_ribbon(data = df %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization (years)", y = "Incidence (per yr, per 100,000)") + 
  facet_rep_grid(age_nm2 ~ group, scales = 'free') + 
  theme(axis.line=element_line()) + scale_x_continuous(limits=c(-10,15))
figs7
ggsave(filename = "_figsms4/FigS7_incid_prim7080.pdf", width = 6, height = 5)

# Fig S8 Relative Risk: Combine fig5 into one figure
# Fig S8A: plot ribbons for first age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL80b %>% filter(age_nm2=="[0,0.17)") #tjL70b for 70% infant coverage or tjL80b for 80% infant coverage
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI1" | var_nm == "CI2") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

df_ve1 <- tjL4 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig80a <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 0.25) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="80% primary coverage\n0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig80a #fig80a

# Fig S8B Manuscript: plot ribbons for second age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL80b %>% filter(age_nm2=="[0.17,1.5)") #tjL70b for 70% or tjL80b 80% coverage
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
tjL4$pcv95  <- ifelse(tjL4$pcv95 == 1, 0.98, tjL4$pcv95) # rounding to avoid RR at infinity 
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
tjL4$pcv975  <- ifelse(tjL4$pcv975 == 1, 0.98, tjL4$pcv975) # rounding to avoid RR at infinity 
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
tjL4$pcv1975a <- ifelse(tjL4$pcv1975a == 1, 0.98, tjL4$pcv1975a) # rounding to avoid RR at infinity 
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169
tjL4$pcv1975 <- ifelse(tjL4$pcv1975 == 1, 0.98, tjL4$pcv1975) # rounding to avoid RR at infinity 

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)
summary(df_veall)

# Fig S8B manuscript
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig80b <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig80b #fig80b

# Fig S8C: Plot with error bars
# Empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, dose1, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
# Simulation data
df_yr2 <- df_ve50 %>% filter(time == 102) #sample size 11
df_yr3 <- df_ve100 %>% filter(time == 103) # sample size 14
df_yr6a <- df_ve1000a %>% filter(time == 106) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time==106, 104, NA) #for aesthetic reasons, replace 6 yrs with 4
df_yr6 <- df_ve1000 %>% filter(time == 106) # sample size 169
df_yr6$time <- ifelse(df_yr6$time==106, 105, NA) #for aesthetic reasons, replace 6 yrs with 5
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
fig80c <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 8.3) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig80c #fig70c

# Fig S8 ABC: Combine  into one figure
figS8_70 <- ggarrange(fig70a, fig70b, fig70c,
                      labels = c('A', 'B', 'C'),
                      ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS8_70

figS8_80 <- ggarrange(fig80a, fig80b, fig80c,
                      labels = c('D', 'E', 'F'),
                      ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS8_80

# Merging legends
plots <- ggarrange(figS8_70, figS8_80, legend = 'none', nrow =1, ncol = 2)
plots
legend <- get_legend(fig80c) # redo fig c with legend
figS8 <- ggarrange(plots, legend, widths = c(0.85, 0.15))
figS8
ggsave(filename = "_figsms4/FigS8_RR_prim7080.pdf", plot = figS8, width = 8, height = 8)

#######################################################################################################
# Fig S9 & S10: sensitivity analysis 3: for 4mo & 12mo average duration of maternal derived immunity
#######################################################################################################
tjL_4 <- fread("_cluster/709090_b1_tjL_mat_3mo") # 4 months maternal derived immunity (3 months refers to the half life of maternal derived immunity with average half life/log(2))
tjL4 <- tjL_4 %>% #Insert the right file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL4 <- tjL4 %>% filter(!grepl("2", .id)) # exclude the 5% blunting
data_id=str_split_fixed(tjL4$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL4b<-cbind(data_id, tjL4)
tjL4b$n2<-1e5 * tjL4b$n / tjL4b$N_age
tjL4b<-tjL4b %>% filter(time>0)
tjL4b$group <- '4 months'
write.csv(tjL4b,"_figsms4/FigS10_data_4mo", row.names = FALSE)

tjL_12 <- fread("_cluster/709090_b1_tjL_mat_12mo") # 12 months maternal derived immunity
tjL12 <- tjL_12 %>% #Insert the right file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL12 <- tjL12 %>% filter(!grepl("2", .id)) #exclude the 5% blunting
tjL12b <- cbind(data_id, tjL12) #data_id comes from the code in tjL4
tjL12b$n2 <- 1e5 * tjL12b$n / tjL12b$N_age
tjL12b <- tjL12b %>% filter(time>0)
tjL12b$group <- '12 months'
write.csv(tjL12b,"_figsms4/FigS10_data_12mo", row.names = FALSE)

# Fig S9 Incidence
df_4 <- tjL4b %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

df_12 <- tjL12b %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
head(df_4)
head(df_12)

df_4_sub<-df_4 %>% filter(age_nm2=='[0,0.17)'| age_nm2=='[0.17,1.5)') 
df_4_sub$age_nm2 <- as.factor(df_4_sub$age_nm2)
levels(df_4_sub$age_nm2)
levels(df_4_sub$age_nm2)[levels(df_4_sub$age_nm2)=="[0,0.17)"] <- "newborn"
levels(df_4_sub$age_nm2)[levels(df_4_sub$age_nm2)=="[0.17,1.5)"] <- "primary immunization"
summary(df_4_sub)
df_12_sub<-df_12 %>% filter(age_nm2=='[0,0.17)'| age_nm2=='[0.17,1.5)')
df_12_sub$age_nm2 <- as.factor(df_12_sub$age_nm2)
levels(df_12_sub$age_nm2)
levels(df_12_sub$age_nm2)[levels(df_12_sub$age_nm2)=="[0,0.17)"] <- "newborn"
levels(df_12_sub$age_nm2)[levels(df_12_sub$age_nm2)=="[0.17,1.5)"] <- "primary immunization"
summary(df_12_sub)
df <- rbind(df_4_sub, df_12_sub)
df$group <- factor(df$group, levels = c("4 months", "12 months"))
df <- df %>% filter(time>89 & time<126)

figs7 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_ribbon(data = df %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  facet_rep_grid(age_nm2 ~ group, scales = 'free') + 
  theme(axis.line=element_line()) + scale_x_continuous(limits=c(-10,15))
figs7
ggsave(filename = "_figsms4/FigS9_incid_mat_412mo.pdf", width = 6, height = 5)

# Fig S10 Relative Risk
# Fig S10A: plot ribbons for first age class
ppv=0.70 #insert the maternal vaccination coverage here
#tjL2age2 <- tjL4b %>% filter(age_nm2=="[0,0.17)") #for an average duration of maternal derived immunity of 4 months
tjL2age2 <- tjL12b %>% filter(age_nm2=="[0.17,1.5)") #for an average duration of maternal derived immunity of 12 months
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI1" | var_nm == "CI2") 
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

df_ve1 <- tjL4 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig50a <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  #ylim(0, 0.31) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="Average duration of maternally derived immunity of 4 months \nIn newborns 0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig50a

fig90a <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  #ylim(0, 0.31) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="Average duration of maternally derived immunity of 12 months \nIn newborns 0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig90a 

# Fig S10B Manuscript: plot ribbons for second age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL4b %>% filter(age_nm2=="[0.17,1.5)") #for an average duration of maternal derived immunity of 4 months
#tjL2age2 <- tjL12b %>% filter(age_nm2=="[0.17,1.5)") #for an average duration of maternal derived immunity of 12 months
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)
summary(df_veall)

# Fig S10B
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig50b <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig50b 

fig90b <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig90b 

# Fig S10C: Plot with error bars
# Empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, dose1, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
# Simulation data
df_yr2 <- df_ve50 %>% filter(time == 102) #sample size 11
df_yr3 <- df_ve100 %>% filter(time == 103) # sample size 14
df_yr6a <- df_ve1000a %>% filter(time == 106) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time==106, 104, NA) #for aesthetic reasons, replace 6 yrs with 4
df_yr6 <- df_ve1000 %>% filter(time == 106) # sample size 169
df_yr6$time <- ifelse(df_yr6$time==106, 105, NA) #for aesthetic reasons, replace 6 yrs with 5
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
fig50c <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 7) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig50c 

fig90c <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 7) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") #+ theme(legend.position = "none") # to make plot without legend
fig90c

# Fig S10 ABC: Combine  into one figure
figS10_4mo <- ggarrange(fig50a, fig50b, fig50c,
                        labels = c('A', 'B', 'C'),
                        ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS10_4mo

figS10_12mo <- ggarrange(fig90a, fig90b, fig90c,
                        labels = c('D', 'E', 'F'),
                        ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS10_12mo

# Merging legends
plots <- ggarrange(figS10_4mo, figS10_12mo, legend = 'none', nrow =1, ncol = 2)
plots
legend <- get_legend(fig90c) 
figS10 <- ggarrange(plots, legend, widths = c(0.85, 0.15))
figS10
ggsave(filename = "_figsms4/FigS10_RR_mat412.pdf", plot = figS10, width = 9, height = 9)

#######################################################################################################
# Fig S11 & S12: sensitivity analysis 4: start of infant primary immunization at 2, 4 or 9 months
#######################################################################################################
# Get the data
tjL_1mo <- fread("_cluster/709090_b1_tjL_prim_1mo") # primary series starting at 2 months
tjL_1mo <- tjL_1mo %>% filter(age_nm2=='[0,0.08)'| age_nm2=='[0.08,1.5)') 
tjL1mo <- tjL_1mo %>% #Insert the right file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL1mo <- tjL1mo %>% filter(!grepl("2", .id)) #exclude the 5% blunting
data_id=str_split_fixed(tjL1mo$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL1mob<-cbind(data_id, tjL1mo)
tjL1mob$n2<-1e5 * tjL1mob$n / tjL1mob$N_age
tjL1mob<-tjL1mob %>% filter(time>0)
tjL1mob$group <- '2 months'
write.csv(tjL1mob,"_figsms4/FigS12_data_1mo", row.names = FALSE)

tjL_4mo <- fread("_cluster/709090_b1_tjL_prim_4mo") #primary series starting at 4 months
tjL_4mo$age_nm2 <- ifelse(tjL_4mo$age_nm=="[0.25,1.5)", '[0.25,1.5)', tjL_4mo$age_nm2) #I had a minor bug in the cluster code 
levels(as.factor(tjL_4mo$age_nm2)) #check that the bug is fixed: age_nm2 should have a group '[0.25,1.5)'
tjL_4mo <- tjL_4mo %>% filter(age_nm2=='[0,0.25)'| age_nm2=='[0.25,1.5)') 
tjL4mo <- tjL_4mo %>% #Insert the right file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL4mo <- tjL4mo %>% filter(!grepl("2", .id)) #exclude the 5% blunting
data_id=str_split_fixed(tjL4mo$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL4mob<-cbind(data_id, tjL4mo)
tjL4mob$n2<-1e5 * tjL4mob$n / tjL4mob$N_age
tjL4mob<-tjL4mob %>% filter(time>0)
tjL4mob$group <- '4 months'
write.csv(tjL4mob,"_figsms4/FigS12_data_4mo", row.names = FALSE)

tjL_9mo <- fread("_cluster/709090_b1_tjL_prim_9mo") #primary series starting at 9 months
# levels(as.factor(tjL_9mo$age_nm2)) # the bug is fixed: age_nm2 should has a group '[0.75,1.5)'
tjL_9mo <- tjL_9mo %>% filter(age_nm2=='[0,0.75)'| age_nm2=='[0.75,1.5)') 
tjL9mo <- tjL_9mo %>% #Insert the right file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL9mo <- tjL9mo %>% filter(!grepl("2", .id)) #exclude the 5% blunting
data_id=str_split_fixed(tjL9mo$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL9mob<-cbind(data_id, tjL9mo)
tjL9mob$n2<-1e5 * tjL9mob$n / tjL9mob$N_age
tjL9mob<-tjL9mob %>% filter(time>0)
tjL9mob$group <- '9 months'
write.csv(tjL9mob,"_figsms4/FigS12_data_9mo", row.names = FALSE)

# Sort out data step 2
df1mo <- tjL1mob %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

df4mo <- tjL4mob %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

df9mo <- tjL9mob %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

# Sort out data step 3
df1mo_sub<-df1mo %>% filter(age_nm2=='[0,0.08)'| age_nm2=='[0.08,1.5)') 
df1mo_sub$age_nm2 <- as.factor(df1mo_sub$age_nm2)
levels(df1mo_sub$age_nm2)
levels(df1mo_sub$age_nm2)[levels(df1mo_sub$age_nm2)=="[0,0.08)"] <- "newborn"
levels(df1mo_sub$age_nm2)[levels(df1mo_sub$age_nm2)=="[0.08,1.5)"] <- "primary immunization"
summary(df1mo_sub)
df4mo_sub<-df4mo %>% filter(age_nm2=='[0,0.25)'| age_nm2=='[0.25,1.5)')
df4mo_sub$age_nm2 <- as.factor(df4mo_sub$age_nm2)
levels(df4mo_sub$age_nm2)
levels(df4mo_sub$age_nm2)[levels(df4mo_sub$age_nm2)=="[0,0.25)"] <- "newborn"
levels(df4mo_sub$age_nm2)[levels(df4mo_sub$age_nm2)=="[0.25,1.5)"] <- "primary immunization"
summary(df4mo_sub)
df9mo_sub<-df9mo %>% filter(age_nm2=='[0,0.75)'| age_nm2=='[0.75,1.5)')
df9mo_sub$age_nm2 <- as.factor(df9mo_sub$age_nm2)
levels(df9mo_sub$age_nm2)
levels(df9mo_sub$age_nm2)[levels(df9mo_sub$age_nm2)=="[0,0.75)"] <- "newborn"
levels(df9mo_sub$age_nm2)[levels(df9mo_sub$age_nm2)=="[0.75,1.5)"] <- "primary immunization"
summary(df9mo_sub)
df <- rbind(df1mo_sub, df4mo_sub, df9mo_sub)
df <- df %>% filter(time>89 & time<126)

# Fig S11 Incidence
figs11<- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_ribbon(data = df %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  facet_rep_grid(age_nm2 ~ group, scales = 'free') + 
  theme(axis.line=element_line()) + scale_x_continuous(limits=c(-10,15))
figs11
ggsave(filename = "_figsms4/FigS11_incid_249mo.pdf", width = 8, height = 5)

# Fig S12 Relative Risk
# Fig S12A: plot ribbons for first age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL1mob %>% filter(age_nm2=="[0,0.08)") #this is for 2 months
# tjL2age2 <- tjL4mob %>% filter(age_nm2=="[0,0.25)") #this is for 4 months
# tjL2age2 <- tjL9mob %>% filter(age_nm2=="[0,0.75)") #this is for 9 months
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI1" | var_nm == "CI2") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

df_ve1 <- tjL4 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig1mo <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 0.31) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="Primary immunization at 2 months \nNewborns 0-1 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig1mo

fig4mo <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 0.31) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="Primary immunization at 4 months \nNewborns 0-3 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig4mo 

fig9mo <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 0.51) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="Primary immunization at 9 months \nNewborns 0-8 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig9mo 

# Fig S12B Manuscript: plot ribbons for second age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL1mob %>% filter(age_nm2=="[0.08,1.5)") #for 2 months
#tjL2age2 <- tjL4mob %>% filter(age_nm2=="[0.25,1.5)") #for 4 months
#tjL2age2 <- tjL9mob %>% filter(age_nm2=="[0.75,1.5)") #for 9 months
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
tjL4$pcv95  <- ifelse(tjL4$pcv95 == 1, 0.98, tjL4$pcv95) # rounding to avoid RR at infinity 
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
tjL4$pcv975  <- ifelse(tjL4$pcv975 == 1, 0.98, tjL4$pcv975) # rounding to avoid RR at infinity 
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
tjL4$pcv1975a  <- ifelse(tjL4$pcv1975a == 1, 0.98, tjL4$pcv1975a) # rounding to avoid RR at infinity 
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169
tjL4$pcv1975 <- ifelse(tjL4$pcv1975 == 1, 0.98, tjL4$pcv1975) # rounding to avoid RR at infinity 

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)
summary(df_veall)

# Fig S12B manuscript
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig1mob <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="2-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig1mob 

fig4mob <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="4-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig4mob 

fig9mob <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="9-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig9mob 

# Fig S12C: Plot with error bars
# Empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, dose1, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
# Simulation data
df_yr2 <- df_ve50 %>% filter(time == 102) #sample size 11
df_yr3 <- df_ve100 %>% filter(time == 103) # sample size 14
df_yr6a <- df_ve1000a %>% filter(time == 106) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time==106, 104, NA) #for aesthetic reasons, replace 6 yrs with 4
df_yr6 <- df_ve1000 %>% filter(time == 106) # sample size 169
df_yr6$time <- ifelse(df_yr6$time==106, 105, NA) #for aesthetic reasons, replace 6 yrs with 5
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
fig1moc <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 8.2) +
  labs(title="2-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig1moc 

fig4moc <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 8.2) +
  labs(title="4-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig4moc

fig9moc <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 8.2) +
  labs(title="9-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") #+ theme(legend.position = "none") # to make plot without legend
fig9moc

# Fig S12 ABC: Combine  into one figure
figS12A <- ggarrange(fig1mo, fig4mo, fig9mo,
                     labels = c('A', 'D', 'G'),
                     ncol = 3, nrow = 1, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS12A

figS12B <- ggarrange(fig1mob, fig4mob, fig9mob,
                         labels = c('B', 'E', 'H'),
                         ncol = 3, nrow = 1, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS12B

figS12C <- ggarrange(fig1moc, fig4moc, fig9moc,
                     labels = c('C', 'F', 'I'),
                     ncol = 3, nrow = 1, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS12C

# Merging legends
legend <- get_legend(fig9moc)
plots2 <- ggarrange(figS12A, figS12B, figS12C, legend = 'none', nrow =3, ncol = 1) #to have 9 panels in 1 plot
plots2
plots2legend <- ggarrange(plots2, legend, widths = c(0.85, 0.15)) #to have 9 panels in 1 plot with legend
plots2legend
ggsave(filename = "_figsms4/FigS12_RR_249mo.pdf", plot = plots2legend, width = 12, height = 9)

#######################################################################################################
# Fig S13: Sensitivity analysis 5: Plot contact matrices in one plot
#######################################################################################################
# Please find the script for this figure in the R file m-create_create_contact_matrix.R

#######################################################################################################
# Fig S14 & S15: Sensitivity analysis 5: Plot incidence for all contact matrices
#######################################################################################################
# Get original data
rm(list = ls())
tjL_uk <- fread("_cluster/709090_b1_tjL") #data with contact matrix for UK
tjL_poland <- fread("_cluster/709090_b1_tjL_poland") #data with contact matrix for Poland
tjL_nl <- fread("_cluster/709090_b1_tjL_nl") #data with contact matrix for the Netherlands
tjL_be <- fread("_cluster/709090_b1_tjL_be") #data with contact matrix for Belgium
tjL_it <- fread("_cluster/709090_b1_tjL_italy") #data with contact matrix for Italy
tjL_lux <- fread("_cluster/709090_b1_tjL_lux") #data with contact matrix for Luxembourg
tjL_fin <- fread("_cluster/709090_b1_tjL_finland") #data with contact matrix for Finland
tjL_fr <- fread("_cluster/709090_b1_tjL_france") #data with contact matrix for France

# Transform data to format for figure
# Due to the size of the original files, we gotta do this one datafile at the time 
tjL2 <- tjL_uk %>% #Fill in the data file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL2 <- tjL2 %>% filter(!grepl("2", .id)) #exclude the 5% blunting; 0-10-20 are fine for this plot
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age

# Data for age 1
tjL2b_sub <- tjL2b %>% filter(age_nm2=='[0,0.17)')
head(tjL2b_sub)
df_tidy_mean_sub <- tjL2b_sub %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
write.csv(df_tidy_mean_sub,"_figsms4/FigS14_data_uk_age1b", row.names = FALSE)

# Data for age 2
tjL2b_sub <- tjL2b %>% filter(age_nm2=='[0.17,1.5)')
head(tjL2b_sub)
df_tidy_mean_sub <- tjL2b_sub %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
write.csv(df_tidy_mean_sub,"_figsms4/FigS14_data_uk_age2b", row.names = FALSE)

# Incidence figure for age 1
uk <- fread("_figsms4/FigS14_data_uk_age1")
uk$country <- 'United Kingdom'
poland <- fread("_figsms4/FigS14_data_poland_age1")
poland$country <- 'Poland'
nl <- fread("_figsms4/FigS14_data_nl_age1")
nl$country <- 'Netherlands'
be <- fread("_figsms4/FigS14_data_be_age1")
be$country <- 'Belgium'
it <- fread("_figsms4/FigS14_data_it_age1")
it$country <- 'Italy'
lux <- fread("_figsms4/FigS14_data_lux_age1")
lux$country <- 'Luxembourg'
fin <- fread("_figsms4/FigS14_data_fin_age1")
fin$country <- 'Finland'
france <- fread("_figsms4/FigS14_data_fr_age1")
france$country <- 'France'
all_age1 <- rbind(uk, poland, nl, be, it, lux, fin, france)
summary(all_age1)

all_age1 <- all_age1 %>% filter(time>89 & time<126)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank()))
pl_contact_age1 <- ggplot() +
  geom_ribbon(data = all_age1 %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = all_age1 %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_rep_wrap(~country, ncol=2, scales='free') +
  theme(axis.line=element_line()) + scale_x_continuous(limits=c(-10,15)) #+ theme(axis.text.x=element_blank()) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank())
pl_contact_age1 <- ggdraw(align_legend(pl_contact_age1)) #to center legend, see custom function below
print(pl_contact_age1)
ggsave(filename = "_figsms4/FigS14_incid_contactmatrices_age1.pdf", plot=pl_contact_age1, width = 8, height = 9)

# Incidence figure for age 2
uk <- fread("_figsms4/FigS15_data_uk_age2")
uk$country <- 'United Kingdom'
poland <- fread("_figsms4/FigS15_data_poland_age2")
poland$country <- 'Poland'
nl <- fread("_figsms4/FigS15_data_nl_age2")
nl$country <- 'Netherlands'
be <- fread("_figsms4/FigS15_data_be_age2")
be$country <- 'Belgium'
it <- fread("_figsms4/FigS15_data_it_age2")
it$country <- 'Italy'
lux <- fread("_figsms4/FigS15_data_lux_age2")
lux$country <- 'Luxembourg'
fin <- fread("_figsms4/FigS15_data_fin_age2")
fin$country <- 'Finland'
france <- fread("_figsms4/FigS15_data_fr_age2")
france$country <- 'France'
all_age2 <- rbind(uk, poland, nl, be, it, lux, fin, france)
summary(all_age2)

all_age2 <- all_age2 %>% filter(time>89 & time<126)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank()))
pl_contact_age2 <- ggplot() +
  geom_ribbon(data = all_age2 %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = all_age2 %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_rep_wrap(~country, ncol=2, scales='free') +
  theme(axis.line=element_line()) + scale_x_continuous(limits=c(-10,15)) #+ theme(axis.text.x=element_blank()) +
theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank())
pl_contact_age2 <- ggdraw(align_legend(pl_contact_age2)) #to center legend, see custom function below
print(pl_contact_age2)
ggsave(filename = "_figsms4/FigS15_incid_contactmatrices_age2.pdf", plot=pl_contact_age2, width = 8, height = 9)

#######################################################################################################
# Fig S16: Get RR data for all contact matrices
#######################################################################################################
# Get original data
rm(list = ls())
tjL_uk <- fread("_cluster/709090_b1_tjL") #data with contact matrix for UK
tjL_poland <- fread("_cluster/709090_b1_tjL_poland") #data with contact matrix for Poland
tjL_nl <- fread("_cluster/709090_b1_tjL_nl") #data with contact matrix for the Netherlands
tjL_be <- fread("_cluster/709090_b1_tjL_be") #data with contact matrix for Belgium
tjL_it <- fread("_cluster/709090_b1_tjL_italy") #data with contact matrix for Italy
tjL_lux <- fread("_cluster/709090_b1_tjL_lux") #data with contact matrix for Luxembourg
tjL_fin <- fread("_cluster/709090_b1_tjL_finland") #data with contact matrix for Finland
tjL_fr <- fread("_cluster/709090_b1_tjL_france") #data with contact matrix for France

# Aggregate into broader age groups
tjL2 <- tjL_uk %>% filter(age_nm2=="[0,0.17)" | age_nm2=="[0.17,1.5)")
tjL2 <- tjL2 %>% #Fill in the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL2 <- tjL2 %>% filter(!grepl("2", .id)) #exclude the 5% blunting; 0-10-20 are fine for this plot
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
write.csv(tjL2b,"_figsms4/FigS16_data_uk_tjL2b", row.names = FALSE)
write.csv(tjL2b,"_figsms4/FigS16_data_poland_tjL2b", row.names = FALSE)
write.csv(tjL2b,"_figsms4/FigS16_data_nl_tjL2b", row.names = FALSE)
write.csv(tjL2b,"_figsms4/FigS16_data_be_tjL2b", row.names = FALSE)
write.csv(tjL2b,"_figsms4/FigS16_data_it_tjL2b", row.names = FALSE)
write.csv(tjL2b,"_figsms4/FigS16_data_lux_tjL2b", row.names = FALSE)
write.csv(tjL2b,"_figsms4/FigS16_data_fin_tjL2b", row.names = FALSE)
write.csv(tjL2b,"_figsms4/FigS16_data_fr_tjL2b", row.names = FALSE)

#######################################################################################################
# Fig S16: Sensitivity analysis 5: Plot RR for all contact matrices for age 1
#######################################################################################################
# Get the data for the first age class
uk <- fread("_figsms4/FigS16_data_uk")
poland <- fread("_figsms4/FigS16_data_poland")
nl <- fread("_figsms4/FigS16_data_nl")
be <- fread("_figsms4/FigS16_data_be")
it <- fread("_figsms4/FigS16_data_it")
lux <- fread("_figsms4/FigS16_data_lux")
fin <- fread("_figsms4/FigS16_data_fin")
fr <- fread("_figsms4/FigS16_data_fr")

uk_age1 <- uk %>% filter(age_nm2=="[0,0.17)") 
poland_age1 <- poland %>% filter(age_nm2=="[0,0.17)") 
nl_age1 <- nl %>% filter(age_nm2=="[0,0.17)") 
be_age1 <- be %>% filter(age_nm2=="[0,0.17)") 
it_age1 <- it %>% filter(age_nm2=="[0,0.17)") 
lux_age1 <- lux %>% filter(age_nm2=="[0,0.17)") 
fin_age1 <- fin %>% filter(age_nm2=="[0,0.17)") 
fr_age1 <- fr %>% filter(age_nm2=="[0,0.17)") 

# Get the data in the right format
uk_num <- subset(uk_age1, var_nm == "CI1") 
uk_num <- uk_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(uk_num)[6]<-'num'
uk_denom <- subset(uk_age1, var_nm == "CI1" | var_nm == "CI2")
uk_denom <- uk_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(uk_denom)[6] <-'denom'
uk <- cbind(uk_num, uk_denom[,6])
uk$pcv <- uk$num/uk$denom
ppv=0.70 #insert the maternal vaccination coverage here
uk$ve <- (uk$pcv/(1-uk$pcv)) * ((1-ppv)/ppv)
ukve1 <- uk %>% filter(blunt == 1)
ukve1 <- transform(ukve1, vesum = ave(ve, sim, FUN = cummean))
ukve3 <- uk %>% filter(blunt == 3)
ukve3 <- transform(ukve3, vesum = ave(ve, sim, FUN = cummean))
ukve4 <- uk %>% filter(blunt == 4)
ukve4 <- transform(ukve4, vesum = ave(ve, sim, FUN = cummean))
uk <- rbind(ukve1, ukve3, ukve4)
df_uk <- uk %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_uk$country <- 'United Kingdom'

poland_num <- subset(poland_age1, var_nm == "CI1") 
poland_num <- poland_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(poland_num)[6]<-'num'
poland_denom <- subset(poland_age1, var_nm == "CI1" | var_nm == "CI2") 
poland_denom <- poland_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(poland_denom)[6] <-'denom'
poland <- cbind(poland_num, poland_denom[,6])
poland$pcv <- poland$num/poland$denom
ppv=0.70 #insert the maternal vaccination coverage here
poland$ve <- (poland$pcv/(1-poland$pcv)) * ((1-ppv)/ppv)
polandve1 <- poland %>% filter(blunt == 1)
polandve1 <- transform(polandve1, vesum = ave(ve, sim, FUN = cummean))
polandve3 <- poland %>% filter(blunt == 3)
polandve3 <- transform(polandve3, vesum = ave(ve, sim, FUN = cummean))
polandve4 <- poland %>% filter(blunt == 4)
polandve4 <- transform(polandve4, vesum = ave(ve, sim, FUN = cummean))
poland <- rbind(polandve1, polandve3, polandve4)
df_poland <- poland %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_poland$country <- 'Poland'

nl_num <- subset(nl_age1, var_nm == "CI1") 
nl_num <- nl_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(nl_num)[6]<-'num'
nl_denom <- subset(nl_age1, var_nm == "CI1" | var_nm == "CI2")
nl_denom <- nl_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(nl_denom)[6] <-'denom'
nl <- cbind(nl_num, nl_denom[,6])
nl$pcv <- nl$num/nl$denom
ppv=0.70 #insert the maternal vaccination coverage here
nl$ve <- (nl$pcv/(1-nl$pcv)) * ((1-ppv)/ppv)
nlve1 <- nl %>% filter(blunt == 1)
nlve1 <- transform(nlve1, vesum = ave(ve, sim, FUN = cummean))
nlve3 <- nl %>% filter(blunt == 3)
nlve3 <- transform(nlve3, vesum = ave(ve, sim, FUN = cummean))
nlve4 <- nl %>% filter(blunt == 4)
nlve4 <- transform(nlve4, vesum = ave(ve, sim, FUN = cummean))
nl <- rbind(nlve1, nlve3, nlve4)
df_nl <- nl %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_nl$country <- 'Netherlands'

be_num <- subset(be_age1, var_nm == "CI1") 
be_num <- be_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(be_num)[6]<-'num'
be_denom <- subset(be_age1, var_nm == "CI1" | var_nm == "CI2")
be_denom <- be_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(be_denom)[6] <-'denom'
be <- cbind(be_num, be_denom[,6])
be$pcv <- be$num/be$denom
ppv=0.70 #insert the maternal vaccination coverage here
be$ve <- (be$pcv/(1-be$pcv)) * ((1-ppv)/ppv)
beve1 <- be %>% filter(blunt == 1)
beve1 <- transform(beve1, vesum = ave(ve, sim, FUN = cummean))
beve3 <- be %>% filter(blunt == 3)
beve3 <- transform(beve3, vesum = ave(ve, sim, FUN = cummean))
beve4 <- be %>% filter(blunt == 4)
beve4 <- transform(beve4, vesum = ave(ve, sim, FUN = cummean))
be <- rbind(beve1, beve3, beve4)
df_be <- be %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_be$country <- 'Belgium'

it_num <- subset(it_age1, var_nm == "CI1") 
it_num <- it_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(it_num)[6]<-'num'
it_denom <- subset(it_age1, var_nm == "CI1" | var_nm == "CI2")
it_denom <- it_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(it_denom)[6] <-'denom'
it <- cbind(it_num, it_denom[,6])
it$pcv <- it$num/it$denom
ppv=0.70 #insert the maternal vaccination coverage here
it$ve <- (it$pcv/(1-it$pcv)) * ((1-ppv)/ppv)
itve1 <- it %>% filter(blunt == 1)
itve1 <- transform(itve1, vesum = ave(ve, sim, FUN = cummean))
itve3 <- it %>% filter(blunt == 3)
itve3 <- transform(itve3, vesum = ave(ve, sim, FUN = cummean))
itve4 <- it %>% filter(blunt == 4)
itve4 <- transform(itve4, vesum = ave(ve, sim, FUN = cummean))
it <- rbind(itve1, itve3, itve4)
df_it <- it %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_it$country <- 'Italy'

lux_num <- subset(lux_age1, var_nm == "CI1") 
lux_num <- lux_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(lux_num)[6]<-'num'
lux_denom <- subset(lux_age1, var_nm == "CI1" | var_nm == "CI2")
lux_denom <- lux_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(lux_denom)[6] <-'denom'
lux <- cbind(lux_num, lux_denom[,6])
lux$pcv <- lux$num/lux$denom
ppv=0.70 #insert the maternal vaccination coverage here
lux$ve <- (lux$pcv/(1-lux$pcv)) * ((1-ppv)/ppv)
luxve1 <- lux %>% filter(blunt == 1)
luxve1 <- transform(luxve1, vesum = ave(ve, sim, FUN = cummean))
luxve3 <- lux %>% filter(blunt == 3)
luxve3 <- transform(luxve3, vesum = ave(ve, sim, FUN = cummean))
luxve4 <- lux %>% filter(blunt == 4)
luxve4 <- transform(luxve4, vesum = ave(ve, sim, FUN = cummean))
lux <- rbind(luxve1, luxve3, luxve4)
df_lux <- lux %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_lux$country <- 'Luxembourg'

fin_num <- subset(fin_age1, var_nm == "CI1") 
fin_num <- fin_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(fin_num)[6]<-'num'
fin_denom <- subset(fin_age1, var_nm == "CI1" | var_nm == "CI2")
fin_denom <- fin_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(fin_denom)[6] <-'denom'
fin <- cbind(fin_num, fin_denom[,6])
fin$pcv <- fin$num/fin$denom
ppv=0.70 #insert the maternal vaccination coverage here
fin$ve <- (fin$pcv/(1-fin$pcv)) * ((1-ppv)/ppv)
finve1 <- fin %>% filter(blunt == 1)
finve1 <- transform(finve1, vesum = ave(ve, sim, FUN = cummean))
finve3 <- fin %>% filter(blunt == 3)
finve3 <- transform(finve3, vesum = ave(ve, sim, FUN = cummean))
finve4 <- fin %>% filter(blunt == 4)
finve4 <- transform(finve4, vesum = ave(ve, sim, FUN = cummean))
fin <- rbind(finve1, finve3, finve4)
df_fin <- fin %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_fin$country <- 'Finland'

fr_num <- subset(fr_age1, var_nm == "CI1") 
fr_num <- fr_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(fr_num)[6]<-'num'
fr_denom <- subset(fr_age1, var_nm == "CI1" | var_nm == "CI2")
fr_denom <- fr_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(fr_denom)[6] <-'denom'
fr <- cbind(fr_num, fr_denom[,6])
fr$pcv <- fr$num/fr$denom
ppv=0.70 #insert the maternal vaccination coverage here
fr$ve <- (fr$pcv/(1-fr$pcv)) * ((1-ppv)/ppv)
frve1 <- fr %>% filter(blunt == 1)
frve1 <- transform(frve1, vesum = ave(ve, sim, FUN = cummean))
frve3 <- fr %>% filter(blunt == 3)
frve3 <- transform(frve3, vesum = ave(ve, sim, FUN = cummean))
frve4 <- fr %>% filter(blunt == 4)
frve4 <- transform(frve4, vesum = ave(ve, sim, FUN = cummean))
fr <- rbind(frve1, frve3, frve4)
df_fr <- fr %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_fr$country <- 'France'

# Put all the countries together
# all_age1 <- rbind(df_uk, df_poland) # for test
all_age1 <- rbind(df_uk, df_poland, df_nl, df_be, df_it, df_lux, df_fin, df_fr)
summary(all_age1)

# Plot
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
figs16 <- ggplot() + 
  geom_ribbon(data = all_age1 %>% filter(time >= 100 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = all_age1 %>% filter(time >= 100 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0.05, 0.25) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  facet_rep_wrap(~country, ncol=2, scales='free') +
  theme(axis.line=element_line()) + 
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank())
print(figs16)
figs16 <- ggdraw(align_legend(figs16)) #to center legend, see custom function below
print(figs16)
ggsave(filename = "_figsms4/FigS16_RR_contactmatrices_age1.pdf", width = 8, height = 9)

#######################################################################################################
# Fig S17: Sensitivity analysis 5: Plot RR for all contact matrices for age 2
#######################################################################################################
# Get the data for the second age class
uk <- fread("_figsms4/FigS16_data_uk") #This is the tjL2b file for each country
poland <- fread("_figsms4/FigS16_data_poland")
nl <- fread("_figsms4/FigS16_data_nl")
be <- fread("_figsms4/FigS16_data_be")
it <- fread("_figsms4/FigS16_data_it")
lux <- fread("_figsms4/FigS16_data_lux")
fin <- fread("_figsms4/FigS16_data_fin")
fr <- fread("_figsms4/FigS16_data_fr")

uk_age2 <- uk %>% filter(age_nm2=="[0.17,1.5)" & time>100) 
poland_age2 <- poland %>% filter(age_nm2=="[0.17,1.5)" & time>100) 
nl_age2 <- nl %>% filter(age_nm2=="[0.17,1.5)" & time>100) 
be_age2 <- be %>% filter(age_nm2=="[0.17,1.5)" & time>100) 
it_age2 <- it %>% filter(age_nm2=="[0.17,1.5)" & time>100) 
lux_age2 <- lux %>% filter(age_nm2=="[0.17,1.5)" & time>100) 
fin_age2 <- fin %>% filter(age_nm2=="[0.17,1.5)" & time>100) 
fr_age2 <- fr %>% filter(age_nm2=="[0.17,1.5)" & time>100) 

# Get the data per country
uk_num <- subset(uk_age2, var_nm == "CI4" | var_nm =="CI5") 
uk_num <- uk_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(uk_num)[6]<-'num'
uk_denom <- subset(uk_age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5")
uk_denom <- uk_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(uk_denom)[6] <-'denom'
uk <- cbind(uk_num, uk_denom[,6])
uk$pcv <- uk$num/uk$denom
ppv=0.70 #insert the maternal vaccination coverage here
uk$ve <- (uk$pcv/(1-uk$pcv)) * ((1-ppv)/ppv)
ukve1 <- uk %>% filter(blunt == 1)
ukve1 <- transform(ukve1, vesum = ave(ve, sim, FUN = cummean))
ukve3 <- uk %>% filter(blunt == 3)
ukve3 <- transform(ukve3, vesum = ave(ve, sim, FUN = cummean))
ukve4 <- uk %>% filter(blunt == 4)
ukve4 <- transform(ukve4, vesum = ave(ve, sim, FUN = cummean))
uk <- rbind(ukve1, ukve3, ukve4)
df_uk <- uk %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_uk$country <- 'United Kingdom'

poland_num <- subset(poland_age2,  var_nm == "CI4" | var_nm =="CI5") 
poland_num <- poland_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(poland_num)[6]<-'num'
poland_denom <- subset(poland_age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") 
poland_denom <- poland_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(poland_denom)[6] <-'denom'
poland <- cbind(poland_num, poland_denom[,6])
poland$pcv <- poland$num/poland$denom
ppv=0.70 #insert the maternal vaccination coverage here
poland$ve <- (poland$pcv/(1-poland$pcv)) * ((1-ppv)/ppv)
polandve1 <- poland %>% filter(blunt == 1)
polandve1 <- transform(polandve1, vesum = ave(ve, sim, FUN = cummean))
polandve3 <- poland %>% filter(blunt == 3)
polandve3 <- transform(polandve3, vesum = ave(ve, sim, FUN = cummean))
polandve4 <- poland %>% filter(blunt == 4)
polandve4 <- transform(polandve4, vesum = ave(ve, sim, FUN = cummean))
poland <- rbind(polandve1, polandve3, polandve4)
df_poland <- poland %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_poland$country <- 'Poland'

nl_num <- subset(nl_age2,  var_nm == "CI4" | var_nm =="CI5") 
nl_num <- nl_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(nl_num)[6]<-'num'
nl_denom <- subset(nl_age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5")
nl_denom <- nl_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(nl_denom)[6] <-'denom'
nl <- cbind(nl_num, nl_denom[,6])
nl$pcv <- nl$num/nl$denom
ppv=0.70 #insert the maternal vaccination coverage here
nl$ve <- (nl$pcv/(1-nl$pcv)) * ((1-ppv)/ppv)
nlve1 <- nl %>% filter(blunt == 1)
nlve1 <- transform(nlve1, vesum = ave(ve, sim, FUN = cummean))
nlve3 <- nl %>% filter(blunt == 3)
nlve3 <- transform(nlve3, vesum = ave(ve, sim, FUN = cummean))
nlve4 <- nl %>% filter(blunt == 4)
nlve4 <- transform(nlve4, vesum = ave(ve, sim, FUN = cummean))
nl <- rbind(nlve1, nlve3, nlve4)
df_nl <- nl %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_nl$country <- 'Netherlands'

be_num <- subset(be_age2,  var_nm == "CI4" | var_nm =="CI5") 
be_num <- be_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(be_num)[6]<-'num'
be_denom <- subset(be_age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5")
be_denom <- be_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(be_denom)[6] <-'denom'
be <- cbind(be_num, be_denom[,6])
be$pcv <- be$num/be$denom
ppv=0.70 #insert the maternal vaccination coverage here
be$ve <- (be$pcv/(1-be$pcv)) * ((1-ppv)/ppv)
beve1 <- be %>% filter(blunt == 1)
beve1 <- transform(beve1, vesum = ave(ve, sim, FUN = cummean))
beve3 <- be %>% filter(blunt == 3)
beve3 <- transform(beve3, vesum = ave(ve, sim, FUN = cummean))
beve4 <- be %>% filter(blunt == 4)
beve4 <- transform(beve4, vesum = ave(ve, sim, FUN = cummean))
be <- rbind(beve1, beve3, beve4)
df_be <- be %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_be$country <- 'Belgium'

it_num <- subset(it_age2,  var_nm == "CI4" | var_nm =="CI5") 
it_num <- it_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(it_num)[6]<-'num'
it_denom <- subset(it_age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5")
it_denom <- it_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(it_denom)[6] <-'denom'
it <- cbind(it_num, it_denom[,6])
it$pcv <- it$num/it$denom
ppv=0.70 #insert the maternal vaccination coverage here
it$ve <- (it$pcv/(1-it$pcv)) * ((1-ppv)/ppv)
itve1 <- it %>% filter(blunt == 1)
itve1 <- transform(itve1, vesum = ave(ve, sim, FUN = cummean))
itve3 <- it %>% filter(blunt == 3)
itve3 <- transform(itve3, vesum = ave(ve, sim, FUN = cummean))
itve4 <- it %>% filter(blunt == 4)
itve4 <- transform(itve4, vesum = ave(ve, sim, FUN = cummean))
it <- rbind(itve1, itve3, itve4)
df_it <- it %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_it$country <- 'Italy'

lux_num <- subset(lux_age2,  var_nm == "CI4" | var_nm =="CI5") 
lux_num <- lux_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(lux_num)[6]<-'num'
lux_denom <- subset(lux_age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5")
lux_denom <- lux_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(lux_denom)[6] <-'denom'
lux <- cbind(lux_num, lux_denom[,6])
lux$pcv <- lux$num/lux$denom
ppv=0.70 #insert the maternal vaccination coverage here
lux$ve <- (lux$pcv/(1-lux$pcv)) * ((1-ppv)/ppv)
luxve1 <- lux %>% filter(blunt == 1)
luxve1 <- transform(luxve1, vesum = ave(ve, sim, FUN = cummean))
luxve3 <- lux %>% filter(blunt == 3)
luxve3 <- transform(luxve3, vesum = ave(ve, sim, FUN = cummean))
luxve4 <- lux %>% filter(blunt == 4)
luxve4 <- transform(luxve4, vesum = ave(ve, sim, FUN = cummean))
lux <- rbind(luxve1, luxve3, luxve4)
df_lux <- lux %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_lux$country <- 'Luxembourg'

fin_num <- subset(fin_age2,  var_nm == "CI4" | var_nm =="CI5") 
fin_num <- fin_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(fin_num)[6]<-'num'
fin_denom <- subset(fin_age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5")
fin_denom <- fin_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(fin_denom)[6] <-'denom'
fin <- cbind(fin_num, fin_denom[,6])
fin$pcv <- fin$num/fin$denom
ppv=0.70 #insert the maternal vaccination coverage here
fin$ve <- (fin$pcv/(1-fin$pcv)) * ((1-ppv)/ppv)
finve1 <- fin %>% filter(blunt == 1)
finve1 <- transform(finve1, vesum = ave(ve, sim, FUN = cummean))
finve3 <- fin %>% filter(blunt == 3)
finve3 <- transform(finve3, vesum = ave(ve, sim, FUN = cummean))
finve4 <- fin %>% filter(blunt == 4)
finve4 <- transform(finve4, vesum = ave(ve, sim, FUN = cummean))
fin <- rbind(finve1, finve3, finve4)
df_fin <- fin %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_fin$country <- 'Finland'

fr_num <- subset(fr_age2,  var_nm == "CI4" | var_nm =="CI5") 
fr_num <- fr_num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(fr_num)[6]<-'num'
fr_denom <- subset(fr_age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5")
fr_denom <- fr_denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(fr_denom)[6] <-'denom'
fr <- cbind(fr_num, fr_denom[,6])
fr$pcv <- fr$num/fr$denom
ppv=0.70 #insert the maternal vaccination coverage here
fr$ve <- (fr$pcv/(1-fr$pcv)) * ((1-ppv)/ppv)
frve1 <- fr %>% filter(blunt == 1)
frve1 <- transform(frve1, vesum = ave(ve, sim, FUN = cummean))
frve3 <- fr %>% filter(blunt == 3)
frve3 <- transform(frve3, vesum = ave(ve, sim, FUN = cummean))
frve4 <- fr %>% filter(blunt == 4)
frve4 <- transform(frve4, vesum = ave(ve, sim, FUN = cummean))
fr <- rbind(frve1, frve3, frve4)
df_fr <- fr %>%
  group_by(blunt, time, age_nm2) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))
df_fr$country <- 'France'

# Put all the countries together
# all_age1 <- rbind(df_uk, df_poland) # for test
all_age2 <- rbind(df_uk, df_poland, df_nl, df_be, df_it, df_lux, df_fin, df_fr)
summary(all_age2)

# Plot
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
figs17 <- ggplot() + 
  geom_ribbon(data = all_age2 %>% filter(time >= 100 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = all_age2 %>% filter(time >= 100 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=1) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  facet_rep_wrap(~country, ncol=2, scales='free') +
  theme(axis.line=element_line()) + 
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank())
print(figs17)
ggsave(filename = "_figsms4/FigS17_RR_contactmatrices_age2.pdf", width = 8, height = 9)

#######################################################################################################
# Fig S18: Sensitivity analysis 5: Plot RR from simulations and empirical data
#######################################################################################################
# Get the empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, time, dose1, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$time<-as.numeric(as.character(data4$time))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)

# Get data per country
uk <- fread("_figsms4/FigS16_data_uk") #This is the tjL2b file
poland <- fread("_figsms4/FigS16_data_poland")
nl <- fread("_figsms4/FigS16_data_nl")
be <- fread("_figsms4/FigS16_data_be")
it <- fread("_figsms4/FigS16_data_it")
lux <- fread("_figsms4/FigS16_data_lux")
fin <- fread("_figsms4/FigS16_data_fin")
fr <- fread("_figsms4/FigS16_data_fr")

# Format the data such that it can be used 
# Repeat this for every country
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- uk %>% filter(age_nm2=="[0.17,1.5)") #fill in data per country
tjL2age2 <- tjL2age2 %>% filter(time>100)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
#tjL4$pcv95  <- ifelse(tjL4$pcv95 == 1, 0.98, tjL4$pcv95) # rounding to avoid RR at infinity 
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
#tjL4$pcv975  <- ifelse(tjL4$pcv975 == 1, 0.98, tjL4$pcv975) # rounding to avoid RR at infinity 
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
#tjL4$pcv1975a  <- ifelse(tjL4$pcv1975a == 1, 0.98, tjL4$pcv1975a) # rounding to avoid RR at infinity 
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169
#tjL4$pcv1975  <- ifelse(tjL4$pcv1975 == 1, 0.98, tjL4$pcv1975) # rounding to avoid RR at infinity 

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)
summary(df_veall)

uk <- df_veall
uk$country <- 'United Kingdom'
write.csv(uk, "_figsms4/FigS18_data_uk", row.names = FALSE)
poland <- df_veall
poland$country <- 'Poland'
write.csv(poland,"_figsms4/FigS18_data_poland", row.names = FALSE)
nl <- df_veall
nl$country <- 'Netherlands'
write.csv(nl,"_figsms4/FigS18_data_nl", row.names = FALSE)
be <- df_veall
be$country <- 'Belgium'
write.csv(be,"_figsms4/FigS18_data_be", row.names = FALSE)
it <- df_veall
it$country <- 'Italy'
write.csv(it,"_figsms4/FigS18_data_it", row.names = FALSE)
lux <- df_veall
lux$country <- 'Luxembourg'
write.csv(lux,"_figsms4/FigS18_data_lux", row.names = FALSE)
fin <- df_veall
fin$country <- 'Finland'
write.csv(fin,"_figsms4/FigS18_data_fin", row.names = FALSE)
fr <- df_veall
fr$country <- 'France'
write.csv(fr,"_figsms4/FigS18_data_fr", row.names = FALSE)

# Get the data of all the countries together
uk <- fread("_figsms4/FigS18_data_uk")
poland <- fread("_figsms4/FigS18_data_poland")
nl <- fread("_figsms4/FigS18_data_nl")
be <- fread("_figsms4/FigS18_data_be")
it <- fread("_figsms4/FigS18_data_it")
lux <- fread("_figsms4/FigS18_data_lux")
fin <- fread("_figsms4/FigS18_data_fin")
fr <- fread("_figsms4/FigS18_data_fr")
df_vecountry <- rbind(uk, poland, nl, be, it, lux, fin, fr)
summary(df_vecountry)

# Select years from simulation data
# Simulation data
df_yr2 <- df_vecountry %>% filter(sample == '11 samples' & time == 102) # sample size 11, 
df_yr3 <- df_vecountry %>% filter(sample == '14 samples' & time == 103) # sample size 14
df_yr6a <- df_vecountry %>% filter(sample == '21 samples' & time == 106) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time==106, 104, NA) # for aesthetic reasons, replace 6 yrs with 4, then manually replace 6 in pdf
df_yr6 <- df_vecountry %>% filter(sample == '169 samples' & time == 106) # sample size 169
df_yr6$time <- ifelse(df_yr6$time==106, 105, NA) # for aesthetic reasons, replace 6 yrs with 5, then manually replace 6 in pdf
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)

# Plot
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
figs18 <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 10.5) +
  #labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  facet_rep_wrap(~country, ncol=2, scales='free') +
  theme(axis.line=element_line()) + 
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") # + theme(legend.position = "none") # + to make plot without legend

figs18 
ggsave(filename = "_figsms4/FigS18_RR_age2.pdf", width = 8, height = 9)

#######################################################################################################
# Fig S19: sensitivity analysis 6: Incidence with 60 year interval between infant and maternal immunization
#######################################################################################################
tjL_60yrs <- fread("_cluster/709090_b1_tjL_60yrs") 
tjL2 <- tjL_60yrs %>% #Fill in the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() #first database
tjL2 <- tjL2 %>% filter(!grepl("2", .id)) #exclude the 5% blunting
head(tjL2) #Obtain this from the code above 
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
summary(tjL2b)

# Fig S17A: Incidence in newborns 
tjL2c <- tjL2b %>% filter(age_nm2=='[0,0.17)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>49 & time<126)
summary(df_tidy_meanb)

figS19A <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time-60, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time-60, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="0-2 months - All infants") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(figS19A)

# Fig S19B: Incidence in infant prim immunization
tjL2c <- tjL2b %>% filter(age_nm2=='[0.17,1.5)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>49 & time<126)
summary(df_tidy_meanb)

# Select the second age class for Fig 5
figS19B <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time-60, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time-60, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - All infants") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(figS19B)

# Fig S19C: Infants from unvaccinated mothers
# Get data per type of infection
tjL2age2<-tjL2b %>% filter(age_nm2=="[0.17,1.5)")
t3 <- subset(tjL2age2, var_nm == "CI3") #SV
summary(t3)
n3 <- t3 %>%
  group_by(blunt, time) %>%
  summarise( mean = mean(n),
             median = median(n),
             sd = sd(n),
             qlower = quantile(n, 0.05, na.rm=TRUE),
             qupper = quantile(n, 0.95, na.rm=TRUE))
summary(n3)

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
pl_unvacc <- ggplot() + 
  geom_ribbon(data = n3 %>% filter(time >= 55 & time < 200), aes(x = time-60, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = n3  %>% filter(time >= 55 & time < 200), mapping = aes(x = time-60, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") +
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - Infants from unvaccinated mothers") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(pl_unvacc)

# Fig S19D: Incidence in infants from vaccinated mothers
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2<-tjL2b %>% filter(age_nm2=="[0.17,1.5)")
tjL4num<-subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4num<-tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
summary(tjL4num)
names(tjL4num)[6]<-'num'
tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
summary(tjL4denom)
names(tjL4denom)[6]<-'denom'
tjL4ve<-cbind(tjL4num, tjL4denom[,6])
summary(tjL4ve)

num <- tjL4ve %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            mean = mean(num),
            median = median(num),
            sd = sd(num),
            qlower = quantile(num, 0.05, na.rm=TRUE),
            qupper = quantile(num, 0.95, na.rm=TRUE))
summary(num)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
pl_num <- ggplot() + 
  geom_ribbon(data = num %>% filter(time >= 50 & time < 200), aes(x = time-60, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = num  %>% filter(time >= 50 & time < 200), mapping = aes(x = time-60, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") +
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - Infants from vaccinated mothers") +theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(pl_num)

# Fig S19all: Four figs together
figS19all <- ggarrange(figS19A, figS19B, pl_unvacc, pl_num, 
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2, font.label=list(face = "plain", color="black", size=11),
                     common.legend = TRUE, legend="right")
figS19all
ggsave(filename = "_figsms4/FigS19_incid_4panels_v1.pdf", plot = figS17all, width = 10, height = 8)

#######################################################################################################
# Fig S20: sensitivity analysis 6: RR with 60 year interval between infant and maternal immunization
#######################################################################################################
tjL_60yrs <- fread("_cluster/709090_b1_tjL_60yrs") # 70% maternal vaccination coverage, 90% for infants

# Aggregate into broader age groups
tjL2 <- tjL_60yrs %>% #Fill in the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL2 <- tjL2 %>% filter(!grepl("2", .id)) #exclude the 5% blunting; 0-10-20 are fine for this plot
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
head(tjL2b)
write.csv(tjL2b,"_figsms4/FigS20_data_60yrs", row.names = FALSE)

# Fig S20A Manuscript: plot ribbons for first age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2b <- fread("_figsms4/FigS20_data_60yrs")
tjL2age2 <- tjL2b %>% filter(age_nm2=="[0,0.17)") #tjL2b from above (check 4)
tjL2age2 <- tjL2age2 %>% filter(time>60)
tjL4num <- subset(tjL2age2, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI1" | var_nm == "CI2") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>60)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

df_ve1 <- tjL4 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
figS20A <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 55 & time < 200), aes(x = time-60, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 55 & time < 200), mapping = aes(x = time-60, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 0.3) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
figS20A

# Fig S20B Manuscript: plot ribbons for second age class
# Get ve cumulative per blunting level
# Step 1: get instantaneous error
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL2b  %>% filter(age_nm2=="[0.17,1.5)") #tjL2b from above (check 4)
tjL2age2 <- tjL2age2 %>% filter(time>60)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>60)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
tjL4$pcv95  <- ifelse(tjL4$pcv95 == 1, 0.98, tjL4$pcv95) # rounding to avoid RR at infinity 
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
tjL4$pcv975  <- ifelse(tjL4$pcv975 == 1, 0.98, tjL4$pcv975) # rounding to avoid RR at infinity 
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
tjL4$pcv1975a  <- ifelse(tjL4$pcv1975a == 1, 0.98, tjL4$pcv1975a) # rounding to avoid RR at infinity 
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169
tjL4$pcv1975  <- ifelse(tjL4$pcv1975 == 1, 0.98, tjL4$pcv1975) # rounding to avoid RR at infinity 

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)
summary(df_veall)

# Fig S20B manuscript
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
figS20B <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 55 & time < 200), aes(x = time-60, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 55 & time < 200), mapping = aes(x = time-60, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunzation program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
figS20B

# Fig S20C: Plot with error bars
# Empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, dose1, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
# Simulation data
df_yr2 <- df_ve50 %>% filter(time == 62) # sample size 11, 
df_yr3 <- df_ve100 %>% filter(time == 63) # sample size 14
df_yr6a <- df_ve1000a %>% filter(time == 66) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time== 66, 64, NA) # for aesthetic reasons, replace 6 yrs with 4, then manually replace 6 in pdf
df_yr6 <- df_ve1000 %>% filter(time == 66) # sample size 169
df_yr6$time <- ifelse(df_yr6$time== 66, 65, NA) # for aesthetic reasons, replace 6 yrs with 5, then manually replace 6 in pdf
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
figS20C <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-60-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-60-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 8.2) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center")
figS20C

# Fig 20 ABC: Combine  into one figure
figS20all <- ggarrange(figS20A, figS20B, figS20C,
                     labels = c('A', 'B', 'C'),
                     ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) 
figS20all
ggsave(filename = "_figsms4/FigS20_60years.pdf", plot = figS20all, width = 6, height = 10) 
# Adjust legends manually to make FigS20_3panels_v1

#######################################################################################################
# Sensitivity analysis 7: Fig S21, S22: RR with blunting levels >=40%
#######################################################################################################
tjL <- fread("_cluster/709090_b1_tjL_base") # 70% maternal vaccination coverage, 90% for infants, high blunting
# tjL <- fread("_cluster/709090_b1_tjL_base2") # base and base2 are the same datasets, independently generated and both are correct 

# Aggregate into broader age groups
tjL2 <- tjL %>% #Fill in the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() 
tjL2 <- tjL2 %>% filter(!grepl("4", .id)) #exclude the 80% blunting; I guess that 0-40-60 will be fine 
data_id=str_split_fixed(tjL2$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL2b<-cbind(data_id, tjL2)
tjL2b$n2<-1e5 * tjL2b$n / tjL2b$N_age
head(tjL2b)

# Fig 4A: Incidence in newborns 
tjL2c <- tjL2b %>% filter(age_nm2=='[0,0.17)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>89 & time<126)
summary(df_tidy_meanb)

fig4A <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '40%', '60%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '40%', '60%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="0-2 months - All infants") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(fig4A)

# Fig 4B: Incidence in infant prim immunization
tjL2c <- tjL2b %>% filter(age_nm2=='[0.17,1.5)')
tjL2c <- tjL2c %>% filter(var_nm=='CItot')
df_tidy_meanb <- tjL2c %>%
  group_by(blunt, time, age_nm2, var_nm) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))
df_tidy_meanb <- df_tidy_meanb %>% filter(time>89 & time<126)
summary(df_tidy_meanb)

fig4B <- ggplot() +
  geom_ribbon(data = df_tidy_meanb %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '40%', '60%')) +
  geom_line(data = df_tidy_meanb %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '40%', '60%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") + 
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - All infants") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(fig4B)

# Fig 4C: Incidence in infants from unvaccinated mothers
tjL2age2<-tjL2b %>% filter(age_nm2=="[0.17,1.5)")
t3 <- subset(tjL2age2, var_nm == "CI3") #SV
summary(t3)
n3 <- t3 %>%
  group_by(blunt, time) %>%
  summarise( mean = mean(n),
             median = median(n),
             sd = sd(n),
             qlower = quantile(n, 0.05, na.rm=TRUE),
             qupper = quantile(n, 0.95, na.rm=TRUE))
summary(n3)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
pl_unvacc <- ggplot() + 
  geom_ribbon(data = n3 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = n3  %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=1) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") +
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - Infants from unvaccinated mothers") + theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(pl_unvacc)

# Fig 4D: Incidence in infants from vaccinated mothers
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2<-tjL2b %>% filter(age_nm2=="[0.17,1.5)")
tjL4num<-subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4num<-tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
summary(tjL4num)
names(tjL4num)[6]<-'num'
tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
summary(tjL4denom)
names(tjL4denom)[6]<-'denom'
tjL4ve<-cbind(tjL4num, tjL4denom[,6])
summary(tjL4ve)

num <- tjL4ve %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            mean = mean(num),
            median = median(num),
            sd = sd(num),
            qlower = quantile(num, 0.05, na.rm=TRUE),
            qupper = quantile(num, 0.95, na.rm=TRUE))
summary(num)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
pl_num <- ggplot() + 
  geom_ribbon(data = num %>% filter(time >= 90 & time < 200), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = num  %>% filter(time >= 90 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=1) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Incidence (per yr, per 100,000)") +
  xlim(-5, 15) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="3-18 months - Infants from vaccinated mothers") +theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
print(pl_num)

# Fig 4all: Four figs together
fig4all <- ggarrange(fig4A, fig4B, pl_unvacc, pl_num, 
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2, font.label=list(face = "plain", color="black", size=11),
                     common.legend = TRUE, legend="right")
fig4all
ggsave(filename = "_figsms4/FigS21_Incid_highblunt.pdf", width = 10, height = 8)

# Fig 22A Manuscript: plot ribbons for first age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL2b %>% filter(age_nm2=="[0,0.17)") #tjL2b from above (check 4)
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI1" | var_nm == "CI2") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 2)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 3)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

df_ve1 <- tjL4 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig5A <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '40%', '60%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '40%', '60%')) +
  labs(x = "Time since start pf maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 0.25) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
fig5A

# Fig 22B Manuscript: plot ribbons for second age class
# Get ve cumulative per blunting level
# Step 1: get instantaneous error
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL2b %>% filter(age_nm2=="[0.17,1.5)") 
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 2)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 3)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
tjL4$pcv95  <- ifelse(tjL4$pcv95 == 1, 0.98, tjL4$pcv95) # rounding to avoid RR at infinity 
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
tjL4$pcv975  = ifelse(tjL4$pcv975 == 1, 0.98, tjL4$pcv975) # rounding to avoid RR at infinity 
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
tjL4$pcv1975a  = ifelse(tjL4$pcv1975a == 1, 0.98, tjL4$pcv1975a) # rounding to avoid RR at infinity 
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169
tjL4$pcv1975  = ifelse(tjL4$pcv1975 == 1, 0.98, tjL4$pcv1975)

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 2)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 2)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 2)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 2)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 2)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 2)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 2)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 2)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)

# Fig 22B manuscript
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig5B <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '40%', '60%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '40%', '60%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  #ylim(0, 3.5) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center")
fig5B

# Fig 22C: Plot with error bars
# Empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, dose1, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
# Simulation data
df_yr2 <- df_ve50 %>% filter(time == 102) # sample size 11, 
df_yr3 <- df_ve100 %>% filter(time == 103) # sample size 14
df_yr6a <- df_ve1000a %>% filter(time == 106) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time==106, 104, NA) # for aesthetic reasons, replace 6 yrs with 4, then manually replace 6 in pdf
df_yr6 <- df_ve1000 %>% filter(time == 106) # sample size 169
df_yr6$time <- ifelse(df_yr6$time==106, 105, NA) # for aesthetic reasons, replace 6 yrs with 5, then manually replace 6 in pdf
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
fig5C <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '40%', '60%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 15.6) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center")
fig5C

# Fig Rebut ABC: Combine  into one figure
figs_rebut_all <- ggarrange(fig5A, fig5B, fig5C,
                       labels = c('A', 'B', 'C'),
                       ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) 
figs_rebut_all
ggsave(filename = "_figsms4/FigS22_RR_highblunt.pdf", plot = figs_rebut_all, width = 6, height = 10) 

#######################################################################################################
# Sensitivity analysis 9: Fig S23-S24: Incidence and RR with q1 0.045 and 0.135 (base value=0.09)
#######################################################################################################
tjL_q1 <- fread("_cluster/709090_b1_tjL_q1") # 50% susceptibility in 0-9 yrs: q1=0.045
tjL70 <- tjL_q1 %>% #Insert the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() #for b1
tjL70 <- tjL70 %>% filter(!grepl("2", .id)) #exclude the 5% blunting
data_id=str_split_fixed(tjL70$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL70b<-cbind(data_id, tjL70)
tjL70b$n2<-1e5 * tjL70b$n / tjL70b$N_age
tjL70b<-tjL70b %>% filter(time>0)
tjL70b$group <- '50% transmission'

tjL_q2 <- fread("_cluster/709090_b1_tjL_q2") # 150% susceptibility in 0-9 yrs: q1=0.13.5
tjL80 <- tjL_q2 %>% #Insert the coverage file here
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n),  N_age = sum(N_age)) %>% 
  ungroup() #for b1
tjL80 <- tjL80 %>% filter(!grepl("2", .id)) #exclude the 5% blunting
data_id=str_split_fixed(tjL80$.id, "_", 2)
data_id=data.frame(data_id)
names(data_id)[1] = 'blunt'
names(data_id)[2] = 'sim'
data_id$blunt = as.numeric(as.character(data_id$blunt))
data_id$sim = as.numeric(as.character(data_id$sim))
tjL80b<-cbind(data_id, tjL80)
tjL80b$n2<-1e5 * tjL80b$n / tjL80b$N_age
tjL80b<-tjL80b %>% filter(time>0)
tjL80b$group <- '150% transmission'

df_70 <- tjL70b %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

df_80 <- tjL80b %>%
  group_by(blunt, time, age_nm2, var_nm, group) %>%
  summarise(n=n(),
            mean = mean(n2),
            median = median(n2),
            sd = sd(n2),
            qlower = quantile(n2, 0.025),
            qupper = quantile(n2, 0.975))

df_70_sub<-df_70 %>% filter(age_nm2=='[0,0.17)'| age_nm2=='[0.17,1.5)') 
df_70_sub$age_nm2 <- as.factor(df_70_sub$age_nm2)
levels(df_70_sub$age_nm2)
levels(df_70_sub$age_nm2)[levels(df_70_sub$age_nm2)=="[0,0.17)"] <- "newborn"
levels(df_70_sub$age_nm2)[levels(df_70_sub$age_nm2)=="[0.17,1.5)"] <- "primary immunization"
head(df_70_sub)
df_80_sub<-df_80 %>% filter(age_nm2=='[0,0.17)'| age_nm2=='[0.17,1.5)')
df_80_sub$age_nm2 <- as.factor(df_80_sub$age_nm2)
levels(df_80_sub$age_nm2)
levels(df_80_sub$age_nm2)[levels(df_80_sub$age_nm2)=="[0,0.17)"] <- "newborn"
levels(df_80_sub$age_nm2)[levels(df_80_sub$age_nm2)=="[0.17,1.5)"] <- "primary immunization"
summary(df_80_sub)
df <- rbind(df_70_sub, df_80_sub)
df <- df %>% filter(time>89 & time<126)
df$group <- factor(df$group, levels=c("50%", "150%"))
summary(df)

# Fig S23 Incidence
figS23 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_ribbon(data = df %>% filter(var_nm=='CItot'), aes(x = time-100, ymin=qlower, ymax=qupper, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df %>% filter(var_nm=='CItot'), mapping = aes(x = time-100, y = median, color=factor(blunt))) + 
  scale_color_brewer(palette = "Reds", "Blunting", labels=c('0%', '10%', '20%')) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank()) + 
  labs(x = "Time since start of maternal immunization (years)", y = "Incidence (per yr, per 100,000)") + 
  facet_rep_wrap(age_nm2 ~ group, scales = 'free') + 
  theme(axis.line=element_line()) + scale_x_continuous(limits=c(-10,15))
figS23
ggsave(filename = "_figsms4/FigS23_incid_q.pdf", width = 6, height = 5)

# Fig S24 Relative Risk: Combine fig5 into one figure
# Fig S24A: plot ribbons for first age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL80b %>% filter(age_nm2=="[0,0.17)") #adjust for 3% to 30% suscpetibles
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI1") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI1" | var_nm == "CI2") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

df_ve1 <- tjL4 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE))

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig80a <- ggplot() + 
  geom_ribbon(data = df_ve1 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve1 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 0.35) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  labs(title="150% transmission\n0-2 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) + #3 or 30% susceptibles
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig80a #fig80a

# Fig S24B Manuscript: plot ribbons for second age class
# Get ve cumulative per blunting level
ppv=0.70 #insert the maternal vaccination coverage here
tjL2age2 <- tjL70b %>% filter(age_nm2=="[0.17,1.5)") #for 70% or80% coverage
tjL2age2 <- tjL2age2 %>% filter(time>0)
tjL4num <- subset(tjL2age2, var_nm == "CI4" | var_nm =="CI5") 
tjL4num <- tjL4num %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n)) #for the instantaneous sum
names(tjL4num)[6]<-'num'
summary(tjL4num)

tjL4denom<-subset(tjL2age2, var_nm == "CI3" | var_nm == "CI4" | var_nm =="CI5") #tjL4 when subsetting id's
tjL4denom<-tjL4denom %>% group_by(.id, blunt, sim, time, age_nm2) %>% summarise(sum(n))
names(tjL4denom)[6]<-'denom'
head(tjL4denom)

tjL4 <- cbind(tjL4num, tjL4denom[,6]) 
tjL4 <- tjL4 %>% filter(time>100)
tjL4$pcv <- tjL4$num/tjL4$denom
tjL4$pcv  <- ifelse(tjL4$pcv == 1, 0.98, tjL4$pcv) # rounding to avoid RR at infinity 
tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4ve1 <- tjL4 %>% filter(blunt == 1)
tjL4ve1 <- transform(tjL4ve1, vesum = ave(ve, sim, FUN = cummean))
tjL4ve3 <- tjL4 %>% filter(blunt == 3)
tjL4ve3 <- transform(tjL4ve3, vesum = ave(ve, sim, FUN = cummean))
tjL4ve4 <- tjL4 %>% filter(blunt == 4)
tjL4ve4 <- transform(tjL4ve4, vesum = ave(ve, sim, FUN = cummean))
tjL4 <- rbind(tjL4ve1, tjL4ve3, tjL4ve4)
summary(tjL4)

# Step 2: add measurement error
tjL4$num25 <- qbinom(p = 0.025, size = 11, prob = tjL4$pcv)
tjL4$num95 <- qbinom(p = 0.975, size = 11, prob = tjL4$pcv)
tjL4$pcv25 <- tjL4$num25 / 11
tjL4$pcv95 <- tjL4$num95 / 11
tjL4$pcv95  <- ifelse(tjL4$pcv95 == 1, 0.98, tjL4$pcv95) # rounding to avoid RR at infinity 
tjL4$num125 <- qbinom(p = 0.025, size = 14, prob = tjL4$pcv)
tjL4$num975 <- qbinom(p = 0.975, size = 14, prob = tjL4$pcv)
tjL4$pcv125 <- tjL4$num125 / 14
tjL4$pcv975 <- tjL4$num975 / 14
tjL4$pcv975  = ifelse(tjL4$pcv975 == 1, 0.98, tjL4$pcv975) # rounding to avoid RR at infinity 
tjL4$num1025a <- qbinom(p = 0.025, size = 21, prob = tjL4$pcv)
tjL4$num1975a <- qbinom(p = 0.975, size = 21, prob = tjL4$pcv)
tjL4$pcv1025a <- tjL4$num1025a / 21
tjL4$pcv1975a <- tjL4$num1975a / 21
tjL4$pcv1975a  = ifelse(tjL4$pcv1975a == 1, 0.98, tjL4$pcv1975a) # rounding to avoid RR at infinity 
tjL4$num1025 <- qbinom(p = 0.025, size = 169, prob = tjL4$pcv)
tjL4$num1975 <- qbinom(p = 0.975, size = 169, prob = tjL4$pcv)
tjL4$pcv1025 <- tjL4$num1025 / 169
tjL4$pcv1975 <- tjL4$num1975 / 169
tjL4$pcv1975  = ifelse(tjL4$pcv1975 == 1, 0.98, tjL4$pcv1975)

tjL4$ve <- (tjL4$pcv/(1-tjL4$pcv)) * ((1-ppv)/ppv)
tjL4$ve25 <- (tjL4$pcv25/(1-tjL4$pcv25)) * ((1-ppv)/ppv)
tjL4$ve95 <- (tjL4$pcv95/(1-tjL4$pcv95)) * ((1-ppv)/ppv)
tjL4$ve125 <- (tjL4$pcv125/(1-tjL4$pcv125)) * ((1-ppv)/ppv)
tjL4$ve975 <- (tjL4$pcv975/(1-tjL4$pcv975)) * ((1-ppv)/ppv)
tjL4$ve1025a <- (tjL4$pcv1025a/(1-tjL4$pcv1025a)) * ((1-ppv)/ppv)
tjL4$ve1975a <- (tjL4$pcv1975a/(1-tjL4$pcv1975a)) * ((1-ppv)/ppv)
tjL4$ve1025 <- (tjL4$pcv1025/(1-tjL4$pcv1025)) * ((1-ppv)/ppv)
tjL4$ve1975 <- (tjL4$pcv1975/(1-tjL4$pcv1975)) * ((1-ppv)/ppv)

# Step 3: make ve cumulative
tjL4ve1_25 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_25 <- transform(tjL4ve1_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve3_25 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_25 <- transform(tjL4ve3_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4ve4_25 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_25 <- transform(tjL4ve4_25, ve25sum = ave(ve25, sim, FUN = cummean))
tjL4_25 <-rbind(tjL4ve1_25, tjL4ve3_25, tjL4ve4_25)

tjL4ve1_95 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_95 <- transform(tjL4ve1_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve3_95 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_95 <- transform(tjL4ve3_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4ve4_95 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_95 <- transform(tjL4ve4_95, ve95sum = ave(ve95, sim, FUN = cummean))
tjL4_95 <-rbind(tjL4ve1_95, tjL4ve3_95, tjL4ve4_95)

tjL4ve1_125 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_125 <- transform(tjL4ve1_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve3_125 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_125 <- transform(tjL4ve3_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4ve4_125 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_125 <- transform(tjL4ve4_125, ve125sum = ave(ve125, sim, FUN = cummean))
tjL4_125 <-rbind(tjL4ve1_125, tjL4ve3_125, tjL4ve4_125)

tjL4ve1_975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_975 <- transform(tjL4ve1_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve3_975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_975 <- transform(tjL4ve3_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4ve4_975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_975 <- transform(tjL4ve4_975, ve975sum = ave(ve975, sim, FUN = cummean))
tjL4_975 <-rbind(tjL4ve1_975, tjL4ve3_975, tjL4ve4_975)

tjL4ve1_1025a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025a <- transform(tjL4ve1_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025a <- transform(tjL4ve3_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025a <- transform(tjL4ve4_1025a, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025a <-rbind(tjL4ve1_1025a, tjL4ve3_1025a, tjL4ve4_1025a)

tjL4ve1_1975a <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975a <- transform(tjL4ve1_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve3_1975a <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975a <- transform(tjL4ve3_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4ve4_1975a <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975a <- transform(tjL4ve4_1975a, ve1975sum = ave(ve1975a, sim, FUN = cummean))
tjL4_1975a <-rbind(tjL4ve1_1975a, tjL4ve3_1975a, tjL4ve4_1975a)

tjL4ve1_1025 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1025 <- transform(tjL4ve1_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve3_1025 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1025 <- transform(tjL4ve3_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4ve4_1025 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1025 <- transform(tjL4ve4_1025, ve1025sum = ave(ve1025, sim, FUN = cummean))
tjL4_1025 <-rbind(tjL4ve1_1025, tjL4ve3_1025, tjL4ve4_1025)

tjL4ve1_1975 <- tjL4 %>% filter(blunt == 1)
tjL4ve1_1975 <- transform(tjL4ve1_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve3_1975 <- tjL4 %>% filter(blunt == 3)
tjL4ve3_1975 <- transform(tjL4ve3_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4ve4_1975 <- tjL4 %>% filter(blunt == 4)
tjL4ve4_1975 <- transform(tjL4ve4_1975, ve1975sum = ave(ve1975, sim, FUN = cummean))
tjL4_1975 <-rbind(tjL4ve1_1975, tjL4ve3_1975, tjL4ve4_1975)

tjL5 <- subset(tjL4, select = c(.id, blunt, sim, time, age_nm2, vesum))
tjL5 <- cbind(tjL5, tjL4_25$ve25sum, tjL4_95$ve95sum, tjL4_125$ve125sum, tjL4_975$ve975sum, tjL4_1025a$ve1025sum, tjL4_1975a$ve1975sum, tjL4_1025$ve1025sum, tjL4_1975$ve1975sum)
head(tjL5)
names (tjL5) [7] <- 've25'
names (tjL5) [8] <- 've95'
names (tjL5) [9] <- 've125'
names (tjL5) [10] <- 've975'
names (tjL5) [11] <- 've1025a'
names (tjL5) [12] <- 've1975a'
names (tjL5) [13] <- 've1025'
names (tjL5) [14] <- 've1975'
head(tjL5)
summary(tjL5)

df_ve2 <- tjL5 %>%
  group_by(blunt, time) %>%
  summarise(n=n(),
            median = median(vesum),
            ql = quantile(vesum, 0.025, na.rm=TRUE),
            qu = quantile(vesum, 0.975, na.rm=TRUE),
            ql50 = median(ve25),
            qu50 = median(ve95), 
            ql100 = median(ve125),
            qu100 = median(ve975), 
            ql1000a = median(ve1025a),
            qu1000a = median(ve1975a),
            ql1000 = median(ve1025),
            qu1000 = median(ve1975))

df_ve50 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql50", "qu50")]
df_ve50$sample <- "11 samples"
names(df_ve50) [6] <-"ql100"
names(df_ve50) [7] <-"qu100"
df_ve100 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql100", "qu100")]
df_ve100$sample <- "14 samples"
df_ve1000a <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000a", "qu1000a")]
df_ve1000a$sample <- "21 samples"
names(df_ve1000a) [6] <-"ql100"
names(df_ve1000a) [7] <-"qu100"
df_ve1000 <- df_ve2[, c("blunt", "time", "median", "ql", "qu", "ql1000", "qu1000")]
df_ve1000$sample <- "169 samples"
names(df_ve1000) [6] <-"ql100"
names(df_ve1000) [7] <-"qu100"
df_veall <-rbind(df_ve50, df_ve100, df_ve1000a, df_ve1000)

# Fig S8B manuscript
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) #for manuscript
fig70b <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(data = df_ve2 %>% filter(time >= 95 & time < 200), aes(x = time-100, ymin=ql, ymax=qu, fill=factor(blunt)), alpha=0.6) +
  scale_fill_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) + 
  geom_line(data = df_ve2 %>% filter(time >= 95 & time < 200), mapping = aes(x = time-100, y = median, color=factor(blunt)), size=2) + 
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '10%', '20%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  ylim(0, 3.5) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 75), limits=c(1,75)) +  #For plot without empirical estimates: skip next line
  theme(legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.box.just = "center") + theme(legend.position = "none") # to make plot without legend
fig70b #fig80b

# Fig S24C: Plot with error bars
# Empirical data
rrmean<-c(0.54, 0.94, 0.34, 1.50, 0.84)
rrlci<-c(0.15, 0.28, 0.12, 0.32, 0.61)
rruci<-c(1.96, 3.16, 0.96, 7, 1.15)
time<-c(3, 3.05, 4, 2, 5)
dose1<-c(3, 3, 3, 2, 3)
author<-c('UK_3yrs', 'UK_3yrs','California','Australia', 'UK_6yrs')
data4<-cbind(rrmean, rrlci, rruci, dose1, author)
data4<-data.frame(data4)
data4$rrmean<-as.numeric(as.character(data4$rrmean))
data4$rrlci<-as.numeric(as.character(data4$rrlci))
data4$rruci<-as.numeric(as.character(data4$rruci))
data4$dose1<-as.numeric(as.character(data4$dose1))
data4$author<-factor(data4$author, levels=c("Australia", "California", "UK_3yrs", "UK_6yrs"))
summary(data4)
# Simulation data
df_yr2 <- df_ve50 %>% filter(time == 102) # sample size 11, 
df_yr3 <- df_ve100 %>% filter(time == 103) # sample size 14
df_yr6a <- df_ve1000a %>% filter(time == 106) # sample size 21
df_yr6a$time <- ifelse(df_yr6a$time==106, 104, NA) # for aesthetic reasons, replace 6 yrs with 4, then manually replace 6 in pdf
df_yr6 <- df_ve1000 %>% filter(time == 106) # sample size 169
df_yr6$time <- ifelse(df_yr6$time==106, 105, NA) # for aesthetic reasons, replace 6 yrs with 5, then manually replace 6 in pdf
df_yr <-rbind(df_yr2, df_yr3, df_yr6a, df_yr6)
summary(df_yr)
head(df_yr)

theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank())) # for manuscript
fig70c <- ggplot() + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(data = df_yr, aes(x = time-100-0.1, ymin=ql100, ymax=qu100, color=factor(blunt), width = 0), width=0, position=position_dodge(width=0.2)) +
  geom_point(data = df_yr, aes(x = time-100-0.1, y=median, color=factor(blunt)), shape=18, size=3, position=position_dodge(width=0.2)) +
  scale_color_brewer(palette = "Reds", "Simulated \nblunting", labels=c('0%', '40%', '60%')) +
  labs(x = "Time since start of maternal immunization program (years)", y = "Relative risk of pertussis") +
  scale_x_continuous(breaks=c(2,3,4,5)) +
  new_scale_colour() +
  geom_point(data=data4, mapping = aes(x = time + 0.05, y = rrmean, color=author), size=2) +
  geom_errorbar(data = data4, aes(x = time + 0.05, ymin=rrlci, ymax=rruci, color=author, width = 0), width=0) +
  scale_colour_manual("Study", values=c("#00CC00","purple", "#F9A825", "royalblue1")) +
  ylim(-0.1, 8.2) +
  labs(title="3-18 months") + theme(plot.title = element_text(hjust = 0.5, size=12)) +
  annotate("text", x=2, y=-0.1, label= "n=11", size=3) + #11
  annotate("text", x=3, y=-0.1, label= "n=14", size=3) + #14
  annotate("text", x=4, y=-0.1, label= "n=21", size=3) + #21
  annotate("text", x=5, y=-0.1, label= "n=169", size=3) + #169
  theme(legend.title.align = 0.5, legend.direction = "vertical", legend.box.just = "center")  + theme(legend.position = "none") 
fig70c #fig80c

# Fig S24 ABC: Combine  into one figure
figS24_70 <- ggarrange(fig70a, fig70b, fig70c,
                      labels = c('A', 'B', 'C'),
                      ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS24_70

figS24_80 <- ggarrange(fig80a, fig80b, fig80c,
                      labels = c('D', 'E', 'F'),
                      ncol = 1, nrow = 3, font.label=list(face = "plain", color="black", size=11)) #also looks good with: common.legend = TRUE, legend="right"
figS24_80

# Merging legends
plots <- ggarrange(figS24_70, figS24_80, legend = 'none', nrow =1, ncol = 2)
plots
legend <- get_legend(fig80c) # rerun the code of the last lot but without theme(legend.position = "none")
figS24 <- ggarrange(plots, legend, widths = c(0.85, 0.15))
figS24
ggsave(filename = "_figsms4/FigS24_RR_q_v2.pdf", plot = figS24, width = 8, height = 8)
