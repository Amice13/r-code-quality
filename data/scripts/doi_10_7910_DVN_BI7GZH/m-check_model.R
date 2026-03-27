#######################################################################################################
# Basic model checks
# Time 0 of model corresponds to start time of mass vaccination (1950 in the US) 
#######################################################################################################
rm(list = ls())
setwd("~/Documents/_codes/_ms") #in this folder for now adjust stoch or det file names according to needs
source("s-base_packages.R")
source("f-CreatePompMod.R")
source("f-ReformatSims.R")
par(bty = "l", las = 1)
theme_set(theme_bw(base_size = 12) + theme(panel.grid.minor = element_blank()))
type_sim <- "stoch" # Type of simulation: "det" (deterministic) or "stoch" (stochastic)
mle <- readRDS(file = ifelse(type_sim == "det", "_data/mle_waning_model_det_STM.rds", "_data/mle_waning_model_stoch_STM.rds"))
debug_bool <- T
#mle[2] <- 0.03 #for sensitivity analysis
#mle[2] <- 0.3 #for sensitivity analysis
#dfmle <- data.frame(mle) #to check

# Create age categories and test data ------------------------------------------
av1<-2 #end of the first age class
agecats_nm <- c("[0,0.17)", "[0.17,1.5)", "[1.5,2)", paste0("[", 1:73, ",", 2:74, ")")) # Age classes in the model
nages_mod <- length(agecats_nm) # 76 age categories
time_test <- seq(-100, 200, by = 1) #1/12 for monthly time intervals
data_test <- matrix(NA, nrow = length(time_test), ncol = nages_mod + 1, 
                    dimnames = list(NULL, c("time", paste0("CIobs_", 1:nages_mod)))) %>% 
  as.data.frame(.) %>%
  mutate(time = time_test)

# Load initial population sizes and create covariates ----------------------------
b <- 1/75 # Birth rate, per year
delta <- c(12/av1, 12/(18-av1), 12/6, rep(1, 73)) # Aging rates, per year 
Ntot <- 1e7 # Total population size
N0 <- setNames(object = b / delta, nm = agecats_nm) * Ntot
covars <- data.frame(time = seq(-100, 200, by = 1e-3))

omegaC_vec <- mle[c("omegaC1", "omegaC2")]
omegaT_vec <- mle[c("omegaT1", "omegaT2")]
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

matplot(covars$time, covars[, c("seasC", "seasT")], type = "l", lwd = 2, lty = 1,
        col = c("yellow", "green"), 
        xlab = "Time (months)", 
        ylab = "Seasonal forcing", 
        bty = "l", 
        #xlim = c(2000, 2001),
        las = 1)
legend("topleft", c("5-10 y", "10-20 y"), col = c("yellow", "green"), lty = 1, lwd = 2, bty = "n")

# Contact matrix ----------------------------------------------------------
C_mat <- readRDS(file = "_data/contact_matrix_UK_symm2.rds") %>% 
  .[["matrix"]] %>% 
  as.matrix()

# Convert daily rate to yearly rate
C_mat <- 365 * C_mat
nages_cmat <- ncol(C_mat) # No of age groups in contact matrix

# Initial conditions ------------------------------------------------------
# Create vector with all age groups, divide the fraction within each age group
# Instead we decided to take the values from the JAMA Ped paper Fig 1C pre-vaccine era and insert all values here.
# Initial conditions ------------------------------------------------------
# Create vector with all age groups, divide the fraction within each age group
# Instead we decided to take the values from the JAMA Ped paper Fig 1C pre-vaccine era and insert all values here.
frac0_new <- c(setNames(object = rep(0, nages_mod), nm = paste0("VM-", 1:nages_mod, "-0")), 
               setNames(object = rep(0, nages_mod), nm = paste0("VMbar-", 1:nages_mod, "-0")),
               setNames(object = rep(0, nages_mod), nm = paste0("V-", 1:nages_mod, "-0")), 
               setNames(object = rep(0, nages_mod), nm = paste0("M-", 1:nages_mod, "-0")), 
               setNames(object = rep(0, nages_mod), nm = paste0("MV-", 1:nages_mod, "-0")),
               setNames(object = rep(0, nages_mod), nm = paste0("SMbar-", 1:nages_mod, "-0")), 
               setNames(object = c(1-0.0003, rep(0.0001, nages_mod-1)), nm = paste0("S-", 1:nages_mod, "-0")), 
               setNames(object = c(0.0001, rep(0.0001, nages_mod-1)), nm = paste0("E-", 1:nages_mod, "-0")),
               setNames(object = c(0.0001,  rep(0.0001, nages_mod-1)), nm = paste0("I-", 1:nages_mod, "-0")), 
               setNames(object = rep(0, nages_mod), nm = paste0("SV-", 1:nages_mod, "-0")),
               setNames(object = rep(0, nages_mod), nm = paste0("SVMbar-", 1:nages_mod, "-0")), 
               setNames(object = rep(0, nages_mod), nm = paste0("SVM-", 1:nages_mod, "-0")),
               setNames(object = c(0.0001, rep(1-0.0003, nages_mod-1)), nm = paste0("R-", 1:nages_mod, "-0")), 
               setNames(object = rep(0, nages_mod), nm = paste0("CI1-", 1:nages_mod, "-0")),
               setNames(object = rep(0, nages_mod), nm = paste0("CI2-", 1:nages_mod, "-0")),
               setNames(object = rep(0, nages_mod), nm = paste0("CI3-", 1:nages_mod, "-0")),
               setNames(object = rep(0, nages_mod), nm = paste0("CI4-", 1:nages_mod, "-0")),
               setNames(object = rep(0, nages_mod), nm = paste0("CI5-", 1:nages_mod, "-0")))
names(frac0_new) <- str_replace_all(names(frac0_new), "-0", ".0")
names(frac0_new) <- str_replace_all(names(frac0_new), "-", "_")
summary(frac0_new) 

# Check that initial conditions are correctly assigned 
print(sum(frac0_new)) # Check sum of fractions, should equal no age groups

# Calculate initial conditions
states0 <- frac0_new * rep(x = N0, times = length(frac0_new) / nages_mod)
sum(states0) #Should be equal to Ntot, 10M for now
stopifnot(all.equal(sum(states0), Ntot))

# Create POMP model and parametrize it--------------------------------------------------------
pMod <- CreatePompMod(nages_mod = nages_mod, 
                      nages_cmat = nages_cmat, 
                      add_preschool_booster = F, 
                      dat_df = data_test, 
                      covars_df = covars, 
                      debug_bool = F)
saveRDS(pMod, file = "_data/pMod_q2.rds")
# Parameters (estimates from Domenech et al., STM 2018)
# Watch out for the tricky part here: need a new bake for every time
# for stochastic, replace mpiib2 by stochtest
theta <- bake(file = "_data/model_pars_q2.rds", # Refresh name at every run
              expr = {
                c(
                  setNames(object = as.numeric(t(C_mat)), 
                           nm = paste0("CM_", 1:(nages_cmat * nages_cmat))), # Contact matrix (yearly rates)
                  setNames(object = c(rep(mle["q1-1"], 11), 
                                      rep(prod(mle[c("q1-1", "q1-2")]), 10), 
                                      rep(prod(mle[c("q1-1", "q1-2", "q1-3")]), nages_mod - 21)), 
                           nm =  paste0("q_", 1:nages_mod)), # Susceptibilities
                  setNames(object = N0, paste0("N_", 1:nages_mod)), # Population sizes
                  setNames(object = delta, paste0("delta_", 1:nages_mod)),
                  setNames(object = c(0.15, 0.15, rep(0.15 * 0.5, 4), rep(0.15 * 0.25, nages_mod - 6)), 
                           nm = paste0("rho_", 1:nages_mod)), # Reporting probabilities of primary infections
                  "tV" = 0, # Start time of routine vaccination
                  "v1" = 0.90, # Vaccine coverage for primary course of vaccination
                  "v2" = 0.90, # Vaccine coverage for vaccine booster doses
                  "epsilon" = 1 - unname(mle["epsA"]), # Initial vaccine coverage
                  "alphaV" = unname(mle["alphaV"]), 
                  "tM" = 100, # Start time of maternal immunization
                  "v0" = 0.70, # Maternal immunization coverage
                  "epsilonM" = 0.95, # Effectiveness of maternal Ab
                  "tau" = log(2) / (180 / 365), # 1 / average duration of maternal Ab; standard 180, 250 for 1 year of average duration of maternal induced protection
                  "b1" = 0, # Blunting effect of maternal Ab on initial vaccine effectiveness
                  "b2" = 0, # Blunting effect of maternal Ab on duration of vaccine protection
                  "sigma" = 365 / 8, # 1 / average latent period
                  "gamma" = 365 / 15, # 1 / average infectious period #theta deleted here
                  "iota" = 1e-9, # Imported no of cases
                  "mu" = b, # Birth rate (per year)
                  "N_tot" = Ntot, # Total population size
                  "kC" = 1e-8,  # Case reporting over-dispersion
                  states0 # Initial conditions
                )
              }
)

#leave out 2
coef(pMod, names(theta)) <- unname(theta) 

# Check initial conditions and parameters ---------------------------------
x0 <- rinit(pMod)
stopifnot(all.equal(sum(x0), Ntot))
for(i in 1:nages_mod) {
  print(sprintf("Age group %d, pop size = %.2f", i, sum(x0[grepl(pattern = paste0("_", i), x = rownames(x0)), ])))
} #size per age class should be proportional to its ageing rate; ageing rates are adjusted in the deltas l 29

# Parameters
pars <- coef(pMod)
print(pars[grep("q_", names(pars))])
print(pars[grep("rho_", names(pars))])
print(pars[grep("N", names(pars))]) #Ntot should be 10M; others proportional to 
print(pars[grep("alphaV", names(pars))]) #can vary twofold between det and stoch
print(pars[grep("epsilon", names(pars))])

# Simulations without or with maternal immunization -------------------------------------------
# Parameters to change: v0, b1, b2
coef(pMod, "tau") <- log(2) / (180 / 365) #Maternal VE at 81%
p_mat <- parmat(params = coef(pMod), nrep = 3)
tM <- unname(coef(pMod, "tM"))
p_mat[c("v0", "b1", "b2"), 1] <- c(0.70, 0, 0) #Maternal immunization/blunting scenario
p_mat[c("v0", "b1", "b2"), 2] <- c(0.70, 0.10, 0) #Maternal immunization/blunting scenario
p_mat[c("v0", "b1", "b2"), 3] <- c(0.70, 0.20, 0) #Maternal immunization/blunting scenario

# Run one to check simulation ----------------------------------------------------------
tjW <- simulate(object = pMod, nsim=3, format = "data.frame") 

# Check total population size
print(summary(rowSums(select(tjW, VM_1:R_76)))) 
tjL <- ReformatSims(sims_df = tjW, agecats_nm = agecats_nm)
tjL <- as.data.frame(tjL)
summary(tjL)
head(tjL)

# Aggregate into broader age groups
tjL2 <- tjL %>% 
  group_by(.id, time, var_nm, var_type, age_nm2) %>% 
  summarise(n = sum(n), N_age = sum(N_age)) %>% 
  ungroup()
tjL2<-as.data.frame(tjL2)
head(tjL2)
summary(tjL2)

# Aggregate across all age groups
tjL_all <- tjL %>% 
  group_by(.id, time, var_nm, var_type) %>% 
  summarise(n = sum(n), N_age = sum(N_age)) %>% 
  ungroup()
tjL_all<-as.data.frame(tjL_all)
head(tjL_all)
summary(tjL_all)

#######################################################################################################
# Population sizes
#######################################################################################################
#Population sizes: all simulations
summary(tjL_all)
pl <- ggplot(data = tjL_all, mapping = aes(x = time, y = N_age, color=.id)) + 
  geom_line(size=1) + 
  labs(x = "Time (years)", y = "Population size", title = "") + ylim(9900000, 10100000) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed")
  #guides(color=guide_legend(title="Sims")) +
  #+ ylim(9999000, 10001000)
print(pl)
ggsave(filename = "_figures/_matrix/909090_pop.pdf", plot = pl, width = 8, height = 8)

#Population sizes: plot distribution
pl <- ggplot(data = tjL_all, mapping = aes(x = N_age, color = .id)) + 
  geom_density() + 
  # geom_vline(xintercept = 10000000, linetype = "dashed") + #fill in median
  # geom_vline(xintercept = 10012913, linetype = "dashed") + #fill in median
  labs(x = "Population size", title = "Check population size") +
  guides(color=guide_legend(title="Sims")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(pl)
ggsave(filename = "matrix_010_matthieu.pdf", plot = pl, width = 8, height = 8)

# Population sizes: with mean and 95%CI
df_tidy_mean <- tjL_all %>%
  group_by(time) %>%
  summarise(n = n(),
            mean = mean(N_age),
            median = median(N_age),
            sd = sd(N_age)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = mean - qt((1-0.95)/2, n - 1) * sem)
summary(df_tidy_mean)

pl <- ggplot(data = df_tidy_mean) + 
  geom_line(mapping = aes(x = time, y = mean), size=0.5) + 
  geom_ribbon(aes(x = time, ymin=CI_lower,ymax=CI_upper),color="grey70",alpha=0.4) +
  labs(x = "Time (years)", y = "Population size") +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed") +
  theme(text=element_text(size=22))
print(pl)
ggsave(filename = "_matrix/_figs/909090_popsize.pdf", plot = pl, width = 8, height = 8)

# Age-specific population size: all age classes
summary(tjL2)
pl <- ggplot(data = tjL2, 
            mapping = aes(x = time, y = N_age, color = age_nm2, group = age_nm2))  + geom_line(size=1) + 
            labs(x = "Time (years)", y = "Population size", col='age class') + theme(text=element_text(size=22))
print(pl)  
ggsave(filename = "_figures/stoch/popsize_age_det.pdf", plot = pl, width = 8, height = 8)

# Age-specific population size: zoom first n classes
tjL3<-subset(tjL, age_yr<4)
pl <- ggplot(data = tjL3, 
            mapping = aes(x = time, y = N_age, color = age_no, group = age_no))  + geom_line(size=1) + 
            labs(x = "Time (years)", y = "Population size", title = "Check population sizes", col='age class')  + theme(text=element_text(size=22))
print(pl)  

tjL3<-tjL2 %>% filter(age_nm2=="[0,0.17)"|age_nm2=="[0.17,1.5)"|age_nm2=="[1.5,5)"|age_nm2=="[5,10)")
pl <- ggplot(data = tjL3, 
             mapping = aes(x = time, y = N_age, color = age_nm2, group = age_nm2))  + geom_line(size=1) + 
  labs(x = "Time (years)", y = "Population size", title = "Check population sizes", col='age class')  + theme(text=element_text(size=22))
print(pl)

# Age-specific population size: all ages per year
N_sum2<-aggregate(tjL$N_age, by=list(time=tjL$time, age=tjL$age_no), FUN=sum)
N_sum2<-cbind(time_test, N_sum2)
names(N_sum2)[4]='popsize'
summary(N_sum2)
pl <- ggplot(data = N_sum2, aes(x = time_test, y = popsize,  color = age, group = age)) + geom_line() + 
  labs(x = "Time (years)", y = "Population size", title = "Check population sizes") 
print(pl) 

#######################################################################################################
# Incidence
#######################################################################################################
# Total incidence
tjL_CItot<-tjL_all %>% filter(var_nm %in% c("CI2", "CI3"))
summary(tjL_CItot)                   
pl <- ggplot(data = tjL_all %>% filter(var_nm %in% c('CItot'), time>=-50 & time<=100), mapping = aes(x = time, y = 1e5 * n / N_age, color=.id)) + #c('S', 'E', 'I', 'R', 'V', 'SV')
  geom_line() + 
  #facet_wrap(~ var_nm, ncol = 1, scales = "free_y") +
  scale_y_log10(limits=c(10, 10000)) + #xlim(-20, 50) +
  labs(x = "Year \n(Year 0 = start infant vaccination)", y = "Incidence (per yr, per 100,000)", title = "") + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 100, linetype = "dashed") + #guides(color=guide_legend(title="Comp")) +
  guides(color=guide_legend(title="Sims")) 
print(pl)
ggsave(filename = "_figures/Fig4_future.pdf", plot = pl, width = 9, height = 8)
ggsave(filename = "C:Desktop/Fig4_future.pdf", plot = pl, width = 8, height = 8)

# Total incidence: plot distribution
tjL_check = tjL_all %>% filter(var_nm %in% c("CItot") & time >= -50)
summary(tjL_check)
summary(1e5 * tjL_check$n / tjL_check$N_age)
pl <- ggplot(data = tjL_all %>% filter(var_nm %in% c("CItot") & time >= -50), mapping = aes(x = 1e5 * n / N_age, color = .id))  + geom_density() + 
  geom_vline(xintercept = 4922, linetype = "dashed") + #fill in median
  labs(x = "Incidence (per yr, per 100,000)", title = "Check incidence") +
  guides(color=guide_legend(title="Sims")) #+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(pl)
ggsave(filename = "_figures/stoch/oct25_incid_dist_0vacc.pdf", plot = pl, width = 8, height = 8)

# Total incidence: plot distribution per vaccination interval
tjL_pre = tjL_all %>% filter(var_nm %in% c("CItot") & time >= -50 & time < 0)
tjL_pre$standincid = 1e5 * tjL_pre$n / tjL_pre$N_age
summary(tjL_pre)
pl <- ggplot(data = tjL_all %>% filter(var_nm %in% c("CItot") & time >= -50 & time < 0), mapping = aes(x = 1e5 * n / N_age, color = .id))  + geom_density() + 
  geom_vline(xintercept = 4929, linetype = "dashed") + #fill in median
  labs(x = "Incidence (per yr, per 100,000)", title = "Before vaccination") +
  guides(color=guide_legend(title="Sims")) #+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(pl)
ggsave(filename = "_figures/stoch/oct25_incid_dist_inf_vacc90_time0.pdf", plot = pl, width = 8, height = 8)

# Total incidence: plot distribution per vaccination interval
tjL_post = tjL_all %>% filter(var_nm %in% c("CItot") & time >= 50)
tjL_post = tjL_post %>% filter(time <100 | time >150)
tjL_post$group = ifelse(tjL_post$time <100, "infant", "infant+mat")
tjL_post$standincid = 1e5 * tjL_post$n / tjL_post$N_age
library(data.table)
setDT(tjL_post)
tjL_post[, as.list(summary(standincid)), by = group]
summary(tjL_post)
tjL_post %>% group_by(group) %>% summarize(mean = mean(standincid), sum = sum(standincid), q95low = quantile(standincid, 0.025), q95up = quantile(standincid, 0.975))
pl <- ggplot(data = tjL_post, mapping = aes(x = standincid, color= .id, shape=group))  + geom_density() + 
  # geom_vline(xintercept = 245, linetype = "dashed") + #fill in median
  # geom_vline(xintercept = 319, linetype = "dashed") + #fill in median
  labs(x = "Incidence (per yr, per 100,000)", title = "Infant vacc w/without maternal vacc") +
  guides(color=guide_legend(title="Sims")) #+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(pl)
ggsave(filename = "_figures/stoch/oct25_incid_dist_inf_vacc90_time50.pdf", plot = pl, width = 8, height = 8)

pl <- ggplot(data = tjL_all %>% filter(var_nm %in% c("CItot", time >= -50 & time <0)), mapping = aes(x = 1e5 * n / N_age, color = .id))  + geom_density() + 
  #geom_vline(xintercept = 10000000, linetype = "dashed") +
  #geom_vline(xintercept = 10015157, linetype = "dashed") + #fill in median
  labs(x = "Incidence (per yr, per 100,000)", title = "Check incidence") +
  guides(color=guide_legend(title="Sims")) #+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(pl)
ggsave(filename = "_figures/stoch/oct25_incid_dist_inf_vacc90.pdf", plot = pl, width = 8, height = 8)

# Age-specific incidence
pl <- ggplot(data = tjL2 %>% filter(var_nm %in% c("CItot"), time >= -50, .id=='1'), mapping = aes(x = time, y = 1e5 * n / N_age, color = age_nm2)) + 
  geom_line() + 
  theme_classic() + theme(legend.position = "top") + 
  geom_vline(xintercept = coef(pMod, 'tV'), linetype = "dashed") + 
  geom_vline(xintercept = coef(pMod, 'tM'), linetype = "dashed") + 
  scale_color_brewer(palette = "Reds") + 
  scale_y_log10(limits=c(0.001, 1000000)) +  theme(text=element_text(size=22)) +
  labs(x = "Time (years)", y = "Incidence (per yr, per 100,000)", color = "Age group (yr)")
print(pl)
ggsave(filename = "_figures/stoch/oct26_incid_age_stoch.pdf", plot = pl, width = 9, height = 8)

#######################################################################################################
# Compartment dynamics
#######################################################################################################
# Numbers per compartment per age
summary(tjL2)
levels(tjL2$var_nm)
tjL3<-tjL2 %>% filter(age_nm2=="[0.17,1.5)") # the change in population size is most pronounced in the second age class
summary(tjL3)
pl <- ggplot(data = tjL3 %>% filter(var_nm %in% c( "M", "SMbar", "S", "E", "I", "R")), 
  mapping = aes(x = time, y = n, color = var_nm, group = var_nm))  + 
  geom_line(size=1) + 
  labs(x = "Time (years)", y = "Population size", title = "Check population sizes", col='age class')  + theme(text=element_text(size=22))
print(pl)  

#Numbers per compartment: compartment subsets
summary(tjL_check7)
tjL_check1<- tjL_all %>% filter( var_nm=='CI1'| var_nm=='CI2'| var_nm=='CI3'| var_nm=='CI4'| var_nm=='CI5') #R too high in the first 100 years - but S and R symmetrical?
tjL_check2<- tjL_all %>% filter( var_nm=='M'| var_nm=='SMbar'| var_nm=='VM'| var_nm=='MV'| var_nm=='VMbar'|var_nm=='SVMbar'|  var_nm=='SVM' | var_nm=='CI1' | var_nm=='CI4' |  var_nm=='CI5') #seem okay
tjL_check3<- tjL_all %>% filter( var_nm=='VM'| var_nm=='MV'| var_nm=='SVM') #MV stays at just over 4000.
tjL_check4<- tjL_all %>% filter( var_nm=='VMbar'| var_nm=='SVMbar') #seem okay
tjL_check5<- tjL_all %>% filter( var_nm=='S' | var_nm=='R') 
tjL_check6<- tjL_all %>% filter( var_nm=='CI2' | var_nm=='S'|  var_nm=='E' | var_nm=='I') #E and I overlap, so that is okay
tjL_check7<- tjL_all %>% filter( !var_nm=='CI1' &  !var_nm=='CI2' & !var_nm=='CI3' &  !var_nm=='CI4' &  !var_nm=='CI5')
tjL_check8<- tjL_all %>% filter( var_nm=='CI2' | var_nm=='CI3' | var_nm=='V' | var_nm=='SV') #R too high in the first 100 years - but S and R symmetrical?
tjL_check9<- tjL_all %>% filter( !var_nm=='CI1' &  !var_nm=='CI2' & !var_nm=='CI3' &  !var_nm=='CI4' &  !var_nm=='CI5' & !var_nm=='CIobs' )

tjL_all5 <- tjL_check5 %>% 
  group_by(.id, time, var_nm) %>% 
  summarise(n = sum(n), N_age = sum(N_age)) %>% 
  ungroup()
summary(tjL_all5)

pl<-ggplot(data = tjL_all5 %>% filter(.id=='1'), mapping = aes(x = time, y = n , color = var_nm, group = var_nm)) + #, color = var_nm, group = var_nm
  geom_line(size=1) + 
  #xlim(-100, 0) + # depending on compartments
  #ylim(-0.5, 0.5) + # depending on compartments
  #scale_y_log10(limits=c(0.1, 200000)) + # depending on compartments
  labs(x = "Time (years)", y = "Counts per Comp",  title = "") +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed") +
  guides(color=guide_legend(title="Comp")) 
print(pl)
ggsave(filename = "matrix_010_check0.pdf", plot =pl, width = 8, height = 8)

#######################################################################################################
# End
#######################################################################################################

