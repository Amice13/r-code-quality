####################################################################
####################################################################
## Replication Material
##
## Michael Jankowski and Stefan M?ller
##
## michael.jankowski@uol.de | stefan.mueller@ucd.ie
## 
## Incumbency Advantage in Lower-Order PR Elections:
## Evidence from the Irish Context, 1942-2019
## 
## Electoral Studies

## File: 02_rdd_analysis.R

## See 00_description_data_and_scripts.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################


library(ggplot2)    # CRAN v3.3.3
library(dplyr)      # CRAN v1.0.4
library(rdd)        # CRAN v0.57
library(rdrobust)   # CRAN v0.99.9
library(gridExtra)  # CRAN v2.3
library(rddensity)  # CRAN v2.1
library(kableExtra) # CRAN v1.3.1
library(purrr)      # CRAN v0.3.4
library(tidyr)      # CRAN v1.1.2
library(stringr)    # CRAN v1.4.0

source("helper_functions.R")

# custom ggplot2 plotting scheme
theme_baser <- function (){
  theme_minimal()  %+replace%
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold",
                                    margin=margin(0,0,5,0)),
          panel.border = element_rect(fill=NA,color="black", size=0.5,
                                      linetype="solid"),
          legend.title = element_text(size = 15),
          title = element_text(size = 16, vjust = 1.5, hjust = 0),
          legend.position = "bottom",
          axis.ticks = element_line(size = 0.3),
          axis.ticks.length = unit(0.2,"cm"),
          legend.text=element_text(size = 13),
          strip.text = element_text(size = 16, hjust = 0.5, face = "bold",
                                    margin=margin(b=5, r = 5, l = 5, t = 5)),
          axis.text = element_text(colour="black", size = 13),
          axis.title = element_text(size = 13, hjust = 0.5))
}

theme_set(theme_baser())


# load data from local elections
dat_local <- readRDS(file = "data_local_elections_complete.rds")

# load data from general elections
dat_general <- readRDS("data_general_elections_complete.rds")

## Figure 02 ----


# rerun and reelected plots for full sample of local elections

full_a <- ggrdplot(dat_local$reran, dat_local$vote_margin, c = 0.5, 
                   subset = dat_local$vote_margin > 0.45 & dat_local$vote_margin < 0.55,
                   nbins = 7,
                   ci = 95) +
  labs(x = "Vote margin in t", y = "Pr(Rerun) in t+1 (and 95% CIs)") +
  ggtitle("a) Rerunning") +
  lims(y = c(-0.1,1)) + 
  theme_baser()

full_b <- ggrdplot(dat_local$reelected, dat_local$vote_margin, c = 0.5, 
                   subset = dat_local$vote_margin > 0.45 & dat_local$vote_margin < 0.55, 
                   nbins = 7,
                   ci = 95) +
  labs(x = "Vote margin in t", y = "Pr(Rerun & reelected in t+1) (and 95% CIs)") +
  ggtitle("b) Reruning and Reelected") +
  lims(y = c(-0.1,1)) + 
  theme_baser()

full_ab <- gridExtra::grid.arrange(full_a,
                                   full_b,
                                   ncol = 2)

ggsave(full_ab, 
       file = "fig_02.pdf", 
       width = 8, 
       height = 4)
ggsave(full_ab, 
       file = "fig_02.eps", 
       width = 8, 
       height = 4)


## Table 02 ----

a <- rdrobust(dat_local$reran, dat_local$vote_margin, c = 0.5, level = 95)
b <- rdrobust_ik(dat_local$reran, dat_local$vote_margin, c = 0.5, level = 95)
c <- rdrobust(dat_local$reran, dat_local$vote_margin, c = 0.5, level = 95, bwselect = "cerrd")
d <- rdrobust(dat_local$reelected, dat_local$vote_margin, c = 0.5, level = 95)
e <- rdrobust_ik(dat_local$reelected, dat_local$vote_margin, c = 0.5, level = 95)
f <- rdrobust(dat_local$reelected, dat_local$vote_margin, c = 0.5, level = 95, bwselect = "cerrd")

# create table
create_rd_table(list(a, b, c, d, e, f))


## Table A01 ---

# Table A01 only explains the quota in PR-STV and does 
# not require any replication code


## Table A02 ----

# winner and loser same party

dat_local_sameparty <- filter(dat_local, party_same_winner_loser == TRUE)

a_same <- rdrobust(dat_local_sameparty$reran, dat_local_sameparty$vote_margin, c = 0.5, level = 95)
b_same <- rdrobust_ik(dat_local_sameparty$reran, dat_local_sameparty$vote_margin, c = 0.5, level = 95)
c_same <- rdrobust(dat_local_sameparty$reran, dat_local_sameparty$vote_margin, c = 0.5, level = 95, bwselect = "cerrd")
d_same <- rdrobust(dat_local_sameparty$reelected, dat_local_sameparty$vote_margin, c = 0.5, level = 95)
e_same <- rdrobust_ik(dat_local_sameparty$reelected, dat_local_sameparty$vote_margin, c = 0.5, level = 95)
f_same <- rdrobust(dat_local_sameparty$reelected, dat_local_sameparty$vote_margin, c = 0.5, level = 95, bwselect = "cerrd")

# create table
create_rd_table(list(a_same, b_same, c_same, d_same, e_same, f_same))


## Table A03 ----

dat_local_not_sameparty <- filter(dat_local, party_same_winner_loser == FALSE)

# winner and loser different party
a_not_same <- rdrobust(dat_local_not_sameparty$reran, dat_local_not_sameparty$vote_margin, c = 0.5, level = 95)
b_not_same <- rdrobust_ik(dat_local_not_sameparty$reran, dat_local_not_sameparty$vote_margin, c = 0.5, level = 95)
c_not_same <- rdrobust(dat_local_not_sameparty$reran, dat_local_not_sameparty$vote_margin, c = 0.5, level = 95, bwselect = "cerrd")
d_not_same <- rdrobust(dat_local_not_sameparty$reelected, dat_local_not_sameparty$vote_margin, c = 0.5, level = 95)
e_not_same <- rdrobust_ik(dat_local_not_sameparty$reelected, dat_local_not_sameparty$vote_margin, c = 0.5, level = 95)
f_not_same <- rdrobust(dat_local_not_sameparty$reelected, dat_local_not_sameparty$vote_margin, c = 0.5, level = 95, bwselect = "cerrd")


# create table
create_rd_table(list(a_not_same, b_not_same, c_not_same, 
                     d_not_same, e_not_same, f_not_same))



## Table 03 ----

# general elections (Redmond and Regan)

g <- rdrobust(dat_general$nextwin, dat_general$share, c = 0.5, level = 95)
h <- rdrobust_ik(dat_general$nextwin, dat_general$share, c = 0.5, level = 95)
i <- rdrobust(dat_general$nextwin, dat_general$share, c = 0.5, level = 95, bwselect = "cerrd")

# create table
create_rd_table(list(g, h, i))



## Figure 03 ----

# RDD analysis for small but overlapping time periods 


# function to split data into periods of four elections
four_year_period <- function(x, vec){
  
  vec[x:c(x+2)]
  
}

general_years <- sort(unique(dat_general$year))
local_years <- sort(unique(dat_local$election_year))

general_periods <- lapply(1:(length(general_years)-2), function(x) four_year_period(x, general_years)) 
local_periods <- lapply(1:(length(local_years)-2), function(x) four_year_period(x, local_years)) 

# function to apply RDD analysis to each time period
apply_rdd_to_periods <- function(period, 
                                 election_data = dat_local,
                                 level = "Local"){
  
  cat(period, "\n")
  
  if(level == "General"){
    
    dat_national <- dat_general %>%
      filter(!is.na(share)) %>% 
      filter(year >= min(period) & year <= max(period))
    
    eff_all <- rdrobust(dat_national$nextwin, 
                        dat_national$share, 
                        c = 0.5, 
                        level = 90) %>% 
      extract_estimate(paste0(min(period), "-", max(period), "_General Elections"))
    
  } else{
    
    dat <- election_data %>% 
      filter(!is.na(vote_margin)) %>% 
      filter(election_year >= min(period) & election_year <= max(period))
    
    eff_all <- rdrobust(dat$reelected, 
                        dat$vote_margin, 
                        c = 0.5, 
                        level = 90) %>% 
      extract_estimate(paste0(min(period), "-", max(period), "_Local Elections"))
    
  }
  
  eff_all
  
}


# apply function to general elections
general_trends <- map_df(general_periods, 
                         function(x) apply_rdd_to_periods(x,
                                                          election_data = dat_general,
                                                          level = "General"))


# apply function to local elections
local_trends <- map_df(local_periods, 
                       function(x) apply_rdd_to_periods(x,
                                                        dat_local,
                                                        "Local"))

# bind results from general and local elections
trends <- bind_rows(general_trends,
                    local_trends) 

trend_years <- trends %>% 
  separate(name, into = c("years", "type"), sep = "_") %>% 
  mutate(all = FALSE) %>% 
  mutate(type = str_replace_all(type, "Local", "a) Local")) %>% 
  mutate(type = str_replace_all(type, "General", "b) General"))


# run RDDs for full time period for local and general elections
eff_local_all <- rdrobust(dat_local$reelected, 
                          dat_local$vote_margin, 
                          c = 0.5, 
                          level = 95) %>% 
  extract_estimate("mod") %>% 
  mutate(type = "a) Local Elections",
         years = "All elections")

eff_general_all <- rdrobust(dat_general$nextwin, 
                            dat_general$share, 
                            c = 0.5, 
                            level = 95) %>% 
  extract_estimate("mod") %>% 
  mutate(type = "b) General Elections",
         years = "All elections")

# bind these two dataframes
df_overall <- rbind(eff_general_all,
                    eff_local_all) %>% 
  mutate(all = TRUE)


# get dataframe with subsets and the full period for local and general elections 
trend_years_overall <- bind_rows(trend_years, df_overall)


ggplot(trend_years_overall, 
       aes(x = years, 
           y = est, 
           ymin = llci, 
           colour = all,
           shape = all,
           ymax = ulci)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_pointrange(size = 0.9) +
  geom_text(aes(y = ulci+0.08, x = years, 
                label = Neff), size = 3, angle = 90) +
  facet_wrap(~ type, scales = "free_x", nrow = 1) +
  ylab("RDD Estimate of Incumbency (and 95% CIs)") +
  xlab(NULL) +
  scale_colour_manual(values = c("black", "darkred")) +
  scale_y_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
  theme(legend.position = "none") + 
  theme_baser() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1, size = 10),
        legend.position = "none")
ggsave("fig_03.pdf",
       width = 8,
       height = 5)
ggsave("fig_03.eps",
       width = 8,
       height = 5)



## Figure 04 ----

# create a variable that checks whether TD was elected in t and t+1
dat_local$td_on_list_elected_combo <- paste0(dat_local$td_on_list_elected,"-",
                                             dat_local$td_on_list_elected_t1)

# dat_local pre/post treatment
dat_local_pt <- dat_local %>% filter(election_year < 2004)
dat_local_post <- dat_local %>% filter(election_year >= 2004)

# point Estimates (based on MSE, IK, and CER Bandwidth)

aRR <- rdrobust(dat_local_pt$reelected[dat_local_pt$td_on_list_elected == F],
                dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected == F],
                c = 0.5, level = 95)

bRR <- rdrobust_ik(dat_local_pt$reelected[dat_local_pt$td_on_list_elected == F],
                   dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected == F],
                   c = 0.5, level = 95)

cRR <- rdrobust(dat_local_pt$reelected[dat_local_pt$td_on_list_elected == F], 
                dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected == F],
                c = 0.5, level = 95, bwselect = "cerrd")

dRR <- rdrobust(dat_local_pt$reelected[dat_local_pt$td_on_list_elected == T],
                dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected == T],
                c = 0.5, level = 95)

eRR <- rdrobust_ik(dat_local_pt$reelected[dat_local_pt$td_on_list_elected == T],
                   dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected == T],
                   c = 0.5, level = 95)

fRR <- rdrobust(dat_local_pt$reelected[dat_local_pt$td_on_list_elected == T],
                dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected == T],
                c = 0.5, level = 95, bwselect = "cerrd")

gRR <- rdrobust(dat_local_post$reelected, dat_local_post$vote_margin,
                c = 0.5, level = 95)

hRR <- rdrobust_ik(dat_local_post$reelected, dat_local_post$vote_margin,
                   c = 0.5, level = 95)

iRR <- rdrobust(dat_local_post$reelected, dat_local_post$vote_margin, 
                c = 0.5, level = 95, bwselect = "cerrd")


aTdf <- extract_estimate(dRR, "Dual Mandate\n(1942-1999)\nTD elected in t")
bTdf <- extract_estimate(eRR, "Dual Mandate\n(1942-1999)\nTD elected in t")
dTdf <- extract_estimate(fRR, "Dual Mandate\n(1942-1999)\nTD elected in t")

aFdf <- extract_estimate(aRR, "Dual Mandate\n(1942-1999)\nNo TD elected in t")
bFdf <- extract_estimate(bRR, "Dual Mandate\n(1942-1999)\nNo TD elected in t")
dFdf <- extract_estimate(cRR, "Dual Mandate\n(1942-1999)\nNo TD elected in t")

apostdf <- extract_estimate(gRR, "Post Dual Mandate\n(2004-2019)\nAll constituencies")
bpostdf <- extract_estimate(hRR, "Post Dual Mandate\n(2004-2019)\nAll constituencies")
dpostdf <- extract_estimate(iRR, "Post Dual Mandate\n(2004-2019)\nAll constituencies")

ests <- bind_rows(list(aTdf, bTdf, dTdf, aFdf, bFdf, dFdf, apostdf, bpostdf, dpostdf))

ggplot(ests, aes(x = name, y = est, ymin = llci, ymax = ulci, color = bw,
                 shape = bw)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.9) +
  geom_text(aes(label = Neff, y = 0.05+ulci), 
            position = position_dodge(width = 0.5),
            size = 4, color = "black") +
  ylab("Effect of Incumbency\n(RDD estimate and 95% CIs)") +
  xlab("") +
  scale_shape_manual(name = "Bandwidth", values = c(1, 15, 16)) +
  scale_color_grey(name = "Bandwidth", start = 0.3, end = 0.7) +
  theme_baser() +
  theme(legend.position = "bottom",
        legend.title = element_text(vjust = 0.5)) 
ggsave("fig_04.pdf", 
       width = 8, height = 5.5)
ggsave("fig_04.eps", 
       width = 8, height = 5.5)

## Table A 04 ----

# report detailed estimates from analysis above
create_rd_table(list(aRR, bRR, cRR, dRR, eRR, fRR, gRR, hRR, iRR))


## Figure 05 ----

# pre-determined outcomes
dat_local$ff <- as.numeric(dat_local$party == "FF")
dat_local$fg <- as.numeric(dat_local$party == "FG")
dat_local$ind <- as.numeric(dat_local$party == "IND")
dat_local$other <- as.numeric(dat_local$party == "OTHER")
dat_local$gp <- as.numeric(dat_local$party == "GP")
dat_local$pd <- as.numeric(dat_local$party == "PD")
dat_local$sf <- as.numeric(dat_local$party == "SF")
dat_local$wp <- as.numeric(dat_local$party == "WP")



pre_inc <- rdrobust(dat_local$reelected_lag, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Elected in t-1")
pre_run <- rdrobust(dat_local$reran_lag, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Candidate in t-1")
pre_fg <- rdrobust(dat_local$ff, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Party = Fianna Fail")
pre_ff <- rdrobust(dat_local$fg, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Party = Fine Gael")
pre_ind <- rdrobust(dat_local$ind, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Independent")
pre_other <- rdrobust(dat_local$other, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Party = Other")
pre_gp <- rdrobust(dat_local$gp, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Party = Green Party")
pre_pd <- rdrobust(dat_local$pd, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Party = Progressive Democrats")
pre_wp <- rdrobust(dat_local$wp, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Party = Workers' Party")
pre_sf <- rdrobust(dat_local$sf, dat_local$vote_margin, c = 0.5) %>% extract_estimate("Party = Sinn Fein")


# create "female" if candidate was classified as female
dat_local_gender <- dat_local %>% 
  mutate(female = ifelse(gender == "Female", 1, 0)) %>% 
  filter(gender %in% c("Male", "Female")) # remove 871 names that could not be classified (lack of first names)
nrow(dat_local_gender)
nrow(dat_local)
table(dat_local$gender)

# check how many people not classified in vote margin
dat_margin_gender <- dat_local %>% 
  filter(!is.na(vote_margin))  
table(dat_margin_gender$gender)


pre_gender <- rdrobust(dat_local_gender$female, dat_local_gender$vote_margin, c = 0.5) %>% extract_estimate("Gender = Female")

# bind all results to a single data frame
pre_list <- bind_rows(list(pre_inc, pre_run, pre_fg, pre_ff, pre_ind, pre_other,
                           pre_gp, pre_wp, pre_sf, pre_pd, pre_gender))

ggplot(pre_list, aes(y = est, x = name, ymin = llci, ymax = ulci)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_pointrange(size = 0.9) +
  coord_flip() +
  scale_y_continuous(limits = c(-0.17, 0.17), 
                     breaks = c(-0.2, -0.1, 0, 0.1, 0.2)) +
  xlab("Outcome") +
  ylab("RDD estimate (and 95% CIs)") + 
  theme_baser()
ggsave("fig_05.pdf", width = 8, height = 4)
ggsave("fig_05.eps", width = 8, height = 4)


## Figure A12 ----

# note that Figure A12 is a screenshot of a website 
# and therefore does not require any code


## Figure A13 ----

## McCrary test for all candidates

margin_all <- dat_local$vote_margin[!is.na(dat_local$vote_margin)]

summary(rddensity(X = margin_all, c = .5))

rdplot_all <- rddensity(X = margin_all, c = .5) %>%
  rdplotdensity(X = margin_all)

rdplotLeft_all <- as.data.frame(rdplot_all$Estl$Estimate)
rdplotLeft_all$cil <- rdplotLeft_all$f_q - qnorm(0.975) * rdplotLeft_all$se_q
rdplotLeft_all$ciu <- rdplotLeft_all$f_q + qnorm(0.975) * rdplotLeft_all$se_q

# CI: above the cutoff
rdplotRight_all <- as.data.frame(rdplot_all$Estr$Estimate)
rdplotRight_all$cil <- rdplotRight_all$f_q - qnorm(0.975) * rdplotRight_all$se_q
rdplotRight_all$ciu <- rdplotRight_all$f_q + qnorm(0.975) * rdplotRight_all$se_q

# histogram
NLeft_all <- sum(margin_all >= min(rdplotLeft_all$grid) & margin_all < 0)
histNLeft_all <- ceiling(min(sqrt(NLeft_all), 10 * log(NLeft_all) / log(10)))

NRight_all <- sum(margin_all <=max (rdplotRight_all$grid) & margin_all >= 0)
histNRight_all <- ceiling(min(sqrt(NRight_all), 10 * log(NRight_all) / log(10)))

histBreaks_all <- c(seq(min(rdplotLeft_all$grid), 0.5, length.out = histNRight_all + 1), seq(0.5, max(rdplotRight_all$grid), length.out = histNRight_all+1)[2:(histNRight_all+1)])
histScale_all <- mean(margin_all >= min(rdplotLeft_all$grid) & margin_all <= max(rdplotRight_all$grid))

mccrary_a <- ggplot() + 
  geom_histogram(data = as.data.frame(margin_all), 
                 aes(x = margin_all, 
                     y = ..density.. * histScale_all),
                 breaks = histBreaks_all, 
                 fill = "steelblue", 
                 col = "white",
                 alpha = 0.2) +
  geom_ribbon(data = rdplotLeft_all, 
              aes(x = grid, 
                  ymin = cil, 
                  ymax = ciu), 
              alpha = 0.4, 
              fill = "darkred") + 
  geom_ribbon(data = rdplotRight_all, 
              aes(x = grid, 
                  ymin = cil, 
                  ymax = ciu), 
              alpha=0.4, 
              fill="darkgreen") + 
  geom_line(data = rdplotLeft_all, 
            aes(x=grid, y=f_p), 
            col = "darkred") +
  geom_line(data=rdplotRight_all, aes(x = grid, y = f_p), 
            col = "darkgreen") + 
  labs(x = "Vote Margin", 
       y = "Density") +
  ggtitle("a) All Candidates") +
  theme_baser()


# McCrary test for incumbents only

margin_incumbents <- dat_local$vote_margin[!is.na(dat_local$vote_margin) & dat_local$reelected_lag == 1]

rdplot <- rddensity(X = margin_incumbents, c = .5) %>%
  rdplotdensity(X = margin_incumbents)

summary(rddensity(X = margin_incumbents, c = .5))

rdplotLeft <- as.data.frame(rdplot$Estl$Estimate)
rdplotLeft$cil <- rdplotLeft$f_q - qnorm(0.975) * rdplotLeft$se_q
rdplotLeft$ciu <- rdplotLeft$f_q + qnorm(0.975) * rdplotLeft$se_q

# CI: above the cutoff
rdplotRight <- as.data.frame(rdplot$Estr$Estimate)
rdplotRight$cil <- rdplotRight$f_q - qnorm(0.975) * rdplotRight$se_q
rdplotRight$ciu <- rdplotRight$f_q + qnorm(0.975) * rdplotRight$se_q

# histogram
NLeft <- sum(margin_incumbents >= min(rdplotLeft$grid) & margin_incumbents < 0)
histNLeft <- ceiling(min(sqrt(NLeft), 10 * log(NLeft)/log(10)))

NRight <- sum(margin_incumbents <= max(rdplotRight$grid) & margin_incumbents >= 0)
histNRight <- ceiling(min(sqrt(NRight), 10 * log(NRight)/log(10)))

histBreaks <- c(seq(min(rdplotLeft$grid), 0.5, length.out = histNRight+1), seq(0.5, max(rdplotRight$grid), length.out = histNRight+1)[2:(histNRight+1)])
histScale <- mean(margin_incumbents >= min(rdplotLeft$grid) & margin_incumbents <= max(rdplotRight$grid))

mccrary_b <- ggplot() + theme_baser() + 
  geom_histogram(data=as.data.frame(margin_all), 
                 aes(x = margin_all, 
                     y = ..density.. * histScale),
                 breaks = histBreaks, 
                 fill = "steelblue", 
                 col = "white",
                 alpha = 0.2) +
  geom_ribbon(data = rdplotLeft, 
              aes(x = grid, 
                  ymin = cil, 
                  ymax = ciu), 
              alpha = 0.4, 
              fill = "darkred") + 
  geom_ribbon(data = rdplotRight, 
              aes(x = grid, 
                  ymin = cil, 
                  ymax = ciu), 
              alpha=0.4, 
              fill="darkgreen") + 
  geom_line(data = rdplotLeft , 
            aes(x=grid, y=f_p), 
            col = "darkred") +
  geom_line(data=rdplotRight, aes(x=grid, y=f_p), 
            col = "darkgreen") + 
  labs(x = "Vote Margin", 
       y = "Density") +
  ggtitle("b) Only Incumbents") +
  theme_baser()
mccrary_b

mccrary <- gridExtra::grid.arrange(mccrary_a,
                                   mccrary_b,
                                   ncol = 2)

ggsave(mccrary, 
       file = "fig_a13.pdf", 
       width = 8, 
       height = 4)


## Figure A14 ----

# different bandwidth sizes for local elections

window_rd(dat_local$reelected, dat_local$vote_margin, seq(0.003, 0.11, 0.002)) +
  scale_y_continuous(limits = c(-0.5, 0.5),
                     breaks = c(seq(-0.5, 0.5, 0.1))) + 
  theme_baser() +
  labs(y = "RDD estimate (and 95% CIs)") +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5))
ggsave("fig_a14.pdf", width = 8, height = 4)


## Figure A15 ----

# different bandwidth sizes for general elections

window_rd(dat_general$nextwin, dat_general$share, seq(0.003, 0.11, 0.002)) +
  scale_y_continuous(limits = c(-0.5, 0.5),
                     breaks = c(seq(-0.5, 0.5, 0.1))) +
  labs(y = "RDD estimate (and 95% CIs)") +
  theme_baser() +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5))
ggsave("fig_a15.pdf", width = 8, height = 4)


## Figure A16 ----

# placebo cut-off: local elections

placebo_rd(dat_local$reelected, dat_local$vote_margin, 
           range = seq(0.46, 0.54, 0.01)) +
  scale_colour_grey(start = 0.6, end = 0) +
  geom_pointrange(size = 0.9) +
  theme_baser() +
  labs(y = "RDD estimate (and 95% CIs)")
ggsave("fig_a16.pdf", 
       width = 8, height = 4)


## Figure A17 ----

# placebo cut-off: local elections

placebo_rd(dat_general$nextwin, dat_general$share, 
           range = seq(0.46, 0.54, 0.01)) +
  scale_colour_grey(start = 0.6, end = 0) +
  geom_pointrange(size = 0.9) +
  theme_baser() +
  labs(y = "RDD estimate (and 95% CIs)")
ggsave("fig_a17.pdf", 
       width = 8, height = 4)



## Figure A18 ----

# plot that produces RDD plots for four subsets

full_00_reelected <- ggrdplot(dat_local_pt$reelected[dat_local_pt$td_on_list_elected_combo == "0-0"], 
                              dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "0-0"], c = 0.5, 
                              subset = dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "0-0"] > 0.45 & dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "0-0"] < 0.55, 
                              nbins = 7,
                              ci = 95) +
  labs(x = "Vote margin in t", y = "Pr(Rerun & Reelected) in t+1 (and 95% CIs)") +
  ggtitle("a) No TD on list elected in t and t+1") +
  lims(y = c(-0.1, 1)) +
  theme_baser()

full_01_reelected <- ggrdplot(dat_local_pt$reelected[dat_local_pt$td_on_list_elected_combo == "0-1"], 
                              dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "0-1"], c = 0.5, 
                              subset = dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "0-1"] > 0.45 & dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "0-1"] < 0.55, 
                              nbins = 7,
                              ci = 95) +
  labs(x = "Vote margin in t", y = "Pr(Rerun & Reelected) in t+1 (and 95% CIs)") +
  ggtitle("b) No TD on list elected in t but in t+1") +
  lims(y = c(-0.1, 1)) +
  theme_baser()

full_10_reelected <- ggrdplot(dat_local_pt$reelected[dat_local_pt$td_on_list_elected_combo == "1-0"], 
                              dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "1-0"], c = 0.5, 
                              subset = dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "1-0"] > 0.45 & dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "1-0"] < 0.55, 
                              nbins = 7,
                              ci = 95) +
  labs(x = "Vote margin in t", y = "Pr(Rerun & Reelected) in t+1 (and 95% CIs)") +
  ggtitle("c) TD on list elected in t but not in t+1") +
  lims(y = c(-0.1, 1)) +
  theme_baser()

full_11_reelected <- ggrdplot(dat_local_pt$reelected[dat_local_pt$td_on_list_elected_combo == "1-1"], 
                              dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "1-1"], c = 0.5, 
                              subset = dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "1-1"] > 0.45 & dat_local_pt$vote_margin[dat_local_pt$td_on_list_elected_combo == "1-1"] < 0.55, 
                              nbins = 7,
                              ci = 95) +
  labs(x = "Vote margin in t", y = "Pr(Rerun & Reelected) in t+1 (and 95% CIs)") +
  ggtitle("d) TD on list elected in t and t+1") +
  lims(y = c(-0.1, 1)) +
  theme_baser()

full_0011_reelected <- gridExtra::grid.arrange(full_00_reelected,
                                               full_01_reelected,
                                               full_10_reelected, 
                                               full_11_reelected,
                                               ncol = 2)

ggsave(full_0011_reelected, 
       file = "fig_a18.pdf", 
       width = 10, 
       height = 8)



# Figure A19 ----

# RDDs with covariates

dat_local$FF <- as.numeric(dat_local$party_aggregated == "FF")
dat_local$FG <- as.numeric(dat_local$party_aggregated == "FG")
dat_local$IND <- as.numeric(dat_local$party_aggregated == "IND")
dat_local$LAB <- as.numeric(dat_local$party_aggregated == "LAB")

covariates <- dat_local[,c("FF",
                           "FG",
                           "IND",
                           "LAB",
                           "n_candidates_const")]

a_cov <- rdrobust(dat_local$reran, dat_local$vote_margin, c = 0.5, level = 95, covs = covariates)
b_cov <- rdrobust_ik(dat_local$reran, dat_local$vote_margin, c = 0.5, level = 95, covs = covariates)
c_cov <- rdrobust(dat_local$reran, dat_local$vote_margin, c = 0.5, level = 95, bwselect = "cerrd", covs = covariates)
d_cov <- rdrobust(dat_local$reelected, dat_local$vote_margin, c = 0.5, level = 95, covs = covariates)
e_cov <- rdrobust_ik(dat_local$reelected, dat_local$vote_margin, c = 0.5, level = 95, covs = covariates)
f_cov <- rdrobust(dat_local$reelected, dat_local$vote_margin, c = 0.5, level = 95, bwselect = "cerrd", covs = covariates)

# create regression table
create_rd_table(list(a_cov, b_cov, c_cov, d_cov, e_cov, f_cov))

## compare coefficients covariates/no covariates

adf <- extract_estimate(a, "Rerun_No covariates")
bdf <- extract_estimate(b, "Rerun_No covariates")
cdf <- extract_estimate(c, "Rerun_No covariates")
ddf <- extract_estimate(d, "Rerunning and Reelected_No covariates")
edf <- extract_estimate(e, "Rerunning and Reelected_No covariates")
fdf <- extract_estimate(f, "Rerunning and Reelected_No covariates")

adf_cov <- extract_estimate(a_cov, "Rerun_Covariates")
bdf_cov <- extract_estimate(b_cov, "Rerun_Covariates")
cdf_cov <- extract_estimate(c_cov, "Rerun_Covariates")
ddf_cov <- extract_estimate(d_cov, "Rerunning and Reelected_Covariates")
edf_cov <- extract_estimate(e_cov, "Rerunning and Reelected_Covariates")
fdf_cov <- extract_estimate(f_cov, "Rerunning and Reelected_Covariates")


dat_nocontrols_covariates <- bind_rows(
  adf, bdf, cdf, ddf, edf, fdf,
  adf_cov, bdf_cov, cdf_cov, ddf_cov, edf_cov, fdf_cov
)

dat_nocontrols_covariates <- dat_nocontrols_covariates %>% 
  separate(name, into = c("type", "covariates_dummy"), sep = "_")


ggplot(dat_nocontrols_covariates, aes(x = bw, 
                                      y = est, ymin = llci,
                                      ymax = ulci, color = covariates_dummy,
                                      shape = covariates_dummy)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.9) +
  ylab("Effect of Incumbency\n(RDD estimate and 95% CIs)") +
  xlab("") +
  facet_wrap(~type) +
  scale_shape_manual(name = NULL,  values = c(1, 15, 16)) +
  scale_color_grey(name = NULL, start = 0.3, end = 0.7) +
  theme_baser() +
  theme(legend.position = "bottom",
        legend.title = element_text(vjust = 0.5)) 
ggsave("fig_a19.pdf", 
       width = 8, height = 4)

