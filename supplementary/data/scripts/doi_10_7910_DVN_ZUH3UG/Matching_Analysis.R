## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Results after proximity matching
## Figures reproduced in this file: 1, 2, A4
##
### ### ### ###

rm(list = ls())

## packages

library(car)
library(dplyr)
library(stargazer)
library(pbapply)
library(ggplot2)
library(grid)
library(gridExtra)

#### matching results ####

## Declare the column classes for the main data file 

# colclasses <- c("character", rep("numeric", 17), 
#                 "factor", 
#                 rep('numeric', 19))

## Load the main data file

inher <- read.csv('Data_Main.csv')

## Drop the prefix from AGS
## This makes the AGS lookup much faster

inher$AGS <- substr(inher$AGS, 5, 12)

#### control flow commands ####

dist = 'replace'

#### actual estimation ####

## paired t test to get matchig results

## this function performs the paired t test after matching
## the inputs are 
##    dep_var : the depdendet variable (string)
##    t_AGS : a list of AGS (area codes) for the treated municipalities
##    c_AGS : a list of AGS (area codes) for the control municipalities
## ## ## ## 

my_t2 <- function(dep_var, t_AGS, c_AGS) {
  
  ## convert the AGS to indices : treatment
  ## the 'ifelse' makes sure that the AGS actually show up in the data 
  
  t_id <- sapply(t_AGS, function(ags) {
    ifelse(length(which(inher$AGS == ags)) > 0, which(inher$AGS == ags), NA)
  }, simplify = T)
  
  ## convert the AGS to indices : control
  ## the 'ifelse' makes sure that the AGS actually show up in the data 
  
  c_id <- sapply(c_AGS, function(ags) {
    ifelse(length(which(inher$AGS == ags)) > 0, which(inher$AGS == ags), NA)
  }, simplify = T)
  
  ## now, we check if any of the AGS could not be found, and remove them

  miss_t_id <- which(sapply(t_id, is.na) == T)
  miss_c_id <- which(sapply(c_id, is.na) == T)
  
  miss_all <- unique(c(miss_t_id, miss_c_id))
  
  ## remove from the list of treatment / control indices
  
  
  if (!(length(miss_t_id) == 0 & length(miss_c_id) == 0)) {
    t_id <- t_id[-miss_all]
    c_id <- c_id[-miss_all]
  
  }
  
  ## do the paired t test
  
  t <- t.test(inher[, dep_var][t_id],              
              inher[, dep_var][c_id],
              paired = T)
  
  ## get the number of non-missing observations 
  
  n_c <- sum(!is.na(inher[, dep_var][c_id]))
  n_t <- sum(!is.na(inher[, dep_var][t_id]))
  n <- min(n_c, n_t) * 2
  
  ## get the mean of the dv over all observations
  
  mean_dv <- mean(inher[c(t_id, c_id), dep_var], na.rm = T)
  
  ## get the treatment effect
  
  coef <- t$estimate
  
  ## get the CI lower and upper bounds
  
  lower <- t$conf.int[1]
  upper <- t$conf.int[2]
  
  ## return list of desired quantities
  
  c(dep_var, round(c(coef, lower, upper, mean_dv), 4), n)
}

## plot function

plot_func <- function(res_df) {
  pd <- position_dodge(0.4)
  
  p33 <- ggplot(data = res_df, aes(y = diff, x = outcome)) +
    theme_bw() + xlab("") + ylab("")
  
  p33 <- p33 + geom_hline(yintercept = 0, linetype = "dashed")
  p33 <- p33 + geom_errorbar(aes(ymin= lower, ymax= upper),
                             width=0, alpha = 1, size = 0.4)
  p33 <- p33 + geom_point(aes(y = diff, x = outcome), fill = "white",
                          size=3.5, position = pd, shape = 21) 
  p33 <- p33 + theme(legend.title=element_blank())
  p33 <- p33 + theme(panel.grid.major = element_blank())
  p33 <- p33 + scale_color_grey(start = 0, end = 0)
  p33 <- p33 + coord_flip() +  ylab("") + theme(legend.position = "bottom")
  p33
}

#### RESULTS: Female representation ####

load("Matches.RData")

## Drop the prefix from AGS
## This makes the AGS lookup much faster

match_repl_dist$c_ags <- substr(match_repl_dist$c_ags, 5, 12)
match_repl_dist$t_ags <- substr(match_repl_dist$t_ags, 5, 12)

## get treatment and control AGS

t_ags_repl = match_repl_dist$t_ags
c_ags_repl = match_repl_dist$c_ags

## get results: FEMALE SHARES

dep_vars_fem <- c("gem_women_share")
dep_vars_fem_std <- c("gem_women_share_z")

## standardize

for (j in 1:length(dep_vars_fem)) {
  inher[, dep_vars_fem_std[j]] <- as.numeric(scale(inher[, dep_vars_fem[j]]))
}

## get results 

all_m2_fem <- t(sapply(dep_vars_fem, my_t2, 
                t_AGS = t_ags_repl,
                c_AGS = c_ags_repl))

## prep for plot: female shares

all_fem <- data.frame(all_m2_fem, stringsAsFactors = F)

## convert columns to numeric for plotting

for (j in 2:6) all_fem[, j] <- as.numeric(all_fem[, j])

## rename columns 

colnames(all_fem) <- c("outcome", "diff", "lower", "upper","mean", "n")

## rename outcome 

all_fem$outcome <- c("Female representation")

## add the number of observations 

all_fem$outcome <- paste0(all_fem$outcome, "\n(N = ", all_fem$n, ")", 
                          "\n(Mean = ", all_fem$mean, ")")

## Now, we use the plot function to plot the results 

plot_func(all_fem)

## Save (optional)

# ggsave("", 
#        width = 6.5, height = 3, device = cairo_pdf)

#### FIGURE A4: Income and Income inequality data ####

## Now, we declare the names of the logged dependent variables

fdz_depvars_log <- c("log_suminc31_gini",
                     "log_suminc31_mean", 
                     "log_suminc31_median")

## We now run the main analysis
## First, for the level variables

all_m2_fdz_tot <- t(sapply(fdz_depvars_log[2:3], my_t2, 
                           t_AGS = t_ags_repl,
                           c_AGS = c_ags_repl))

## Then, for the inequality variable

all_m2_fdz_ineq <- t(sapply(fdz_depvars_log[1], my_t2, 
                            t_AGS = t_ags_repl,
                            c_AGS = c_ags_repl))

## Before plotting the results, we convert the results to data frames
## We also convert the relevant columns to numeric variables

all_m2_fdz_tot <- data.frame(all_m2_fdz_tot, stringsAsFactors = F)
all_m2_fdz_ineq <- data.frame(all_m2_fdz_ineq, stringsAsFactors = F)

## To numeric

for (j in 2:6) all_m2_fdz_ineq[, j] <- as.numeric(all_m2_fdz_ineq[, j])
for (j in 2:6) all_m2_fdz_tot[, j] <- as.numeric(all_m2_fdz_tot[, j])

## Rename the columns prior to plotting

colnames(all_m2_fdz_ineq) <- colnames(all_m2_fdz_tot) <- 
  c("outcome", "diff", "lower", "upper", "mean", "n")

## Declare outcomes

all_m2_fdz_tot$outcome <- c("Log mean income", 
                        "Log median income")
all_m2_fdz_ineq$outcome <- c("Log GINI")

## Add number of observations

all_m2_fdz_ineq$outcome <- paste0(all_m2_fdz_ineq$outcome, 
                                  "\n(N = ", all_m2_fdz_ineq$n, ")")
all_m2_fdz_tot$outcome <- paste0(all_m2_fdz_tot$outcome, 
                                  "\n(N = ", all_m2_fdz_tot$n, ")")

## change order of factor levels to make the plot look better

all_m2_fdz_tot$outcome <- factor(all_m2_fdz_tot$outcome, 
                                  levels = all_m2_fdz_tot$outcome[2:1])

## Combine both data frame prior to plotting

all_m2_fdz <- rbind(all_m2_fdz_ineq,
                    all_m2_fdz_tot)

## Make the final plot via the plotting function (Figure A3)

plot_func(all_m2_fdz)

ggsave("plot_match1_fdz_combined.pdf", 
       width = 6.5, height = 3, device = cairo_pdf)

## ## ## ## ## ## ## ## ##

#### RESULTS: Rotary ####

## Standaridze outcomes

inher$rot_adel_int1_z <- scale(inher$rot_adel_int1) # Share of nobles in rotary
inher$rot_uradel_int_z <- scale(inher$rot_uradel_int) # share of Ancient nobles (Uradel)
inher$rot_fem_share_z <- scale(inher$rot_fem_share) # Share of women in Rotary

## Declare dependent variables, 

dep_vars_rot <- c("rot_adel_int1", "rot_uradel_int", 'rot_fem_share')
dep_vars_rot_std <- c("rot_adel_int1_z", "rot_uradel_int_z", "rot_fem_share_z")

## Estimate treatment effect 

all_rot <- t(sapply(dep_vars_rot_std, my_t2, 
                    t_AGS = t_ags_repl,
                    c_AGS = c_ags_repl))

## To DF

all_rot <- data.frame(all_rot, stringsAsFactors = F)

## Columns to numeric

for (j in 2:6) all_rot[, j] <- as.numeric(all_rot[, j])

## Rename Columns for plotting

colnames(all_rot) <- c("outcome", "diff", "lower", "upper","mean", "n")

## Rename outcomes for plotting 

all_rot$outcome <- c("Nobility in Rotary", 'Ancient Nobility in Rotary',
                     'Females in Rotary')

## Add number of observations to plot 

all_rot$outcome <- paste0(all_rot$outcome, "\n(N = ", all_rot$n, ")")

## Relevel outcome variable for plotting

all_rot$outcome <- factor(all_rot$outcome, levels = all_rot$outcome[3:1])

#### FIGURE 1 : Combined matching results ####

## Merge the Female representation and Rotary results data frames

all_merged <- rbind(all_fem, all_rot)

## Relevel the outcome for plotting 

all_merged$outcome <- factor(all_merged$outcome,
                             levels = all_merged$outcome[4:1])

## Plot using the plotting function

plot_func(all_merged)

## Save (optional) 

#  ggsave("",
#        width = 6.5, height = 4, device = cairo_pdf)

#### FIGURE 2 : Distance sensitivity analysis ####

## First, we create the 'equality index', which is the sum of the 
## female representation variable, and the nobility presence in the Rotary clubs.
## Note that the Rotary presence enters negatively
## (Both variables are standardized before taking the sum)

inher$eq_index <- inher$gem_women_share_z - inher$rot_adel_int1_z

## Now, we generate a list of distance for the sensitivity analysis
## The goal here is to restrict the sample to matches where the two municipalities
## are within X kilometers of each other, and then to see whether the treatment effect changes 

## We first get the median and the maximum of the distribution of distances

range_km <- quantile(match_repl_dist$dist_km, c(0.5, 1)) 

## We then generate a vector of 20 evenly-spaced points on this interval

range_km <- seq(range_km[1], range_km[2], length.out = 20)

## Now, we sequentially restrict the sample to matches that are within 
## a certain distance of each other.
## 
## 

out_dist_index <- pblapply(range_km, function(d) {
  ## get match
  match_temp <- match_repl_dist[match_repl_dist$dist_km < d, ]
  
  ## get t_id and c_id
  
  t_ags_temp <- match_temp$t_ags
  c_ags_temp <- match_temp$c_ags
  
  ## do t test
  
  out_temp <- t(my_t2('eq_index', t_AGS = t_ags_temp, c_AGS = c_ags_temp))
  out_temp <- data.frame(out_temp, stringsAsFactors = F)
  colnames(out_temp) <- c("outcome", "diff", "lower", "upper", "mean", "n")
  out_temp$dist <- d
  
  ## return
  
  out_temp
})

## to df, to numeric for all three outcomes

out_dist_index <- do.call('rbind', out_dist_index)
for (j in 2:6) out_dist_index[, j] <- as.numeric(out_dist_index[, j])

## The plot consists of two parts - the actual estimates 
## and the observations conditional on the maximum distance 

p1 <- ggplot(data = out_dist_index, aes(x = dist, y = diff)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
  geom_errorbar(aes(x = dist, ymin = lower, ymax = upper), width = 0) +
  geom_point(shape = 21, fill = 'white', size = 2.6) +
  scale_x_continuous(breaks = NULL, limits = c(2.5, 29),
                     labels = NULL) +
  scale_y_continuous(labels = function(x) format(x, width = 2)) +
  xlab('') + ylab("")
p1

## index outcome number of observations

p2 <- ggplot(data = out_dist_index, aes(x = dist, y = n)) +
  geom_line() + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25), limits = c(2.5, 29)) +
  xlab('Distance (km)') + ylab("N") + 
  scale_y_continuous(labels = function(x) format(x, width = 2))
p2 

## Now, we combine the two plots

p1 <- ggplot_gtable(ggplot_build(p1))
p2 <- ggplot_gtable(ggplot_build(p2))

maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3])
p1$widths[2:3] <- maxWidth
p2$widths[2:3] <- maxWidth

## We use grid.arrange two make the final plot 

grid.arrange(p1,p2,ncol=1, nrow =2, 
             widths=c(2), heights=c(4.5, 2))
g <- arrangeGrob(p1,p2,ncol=1, nrow =2,
                 widths=c(2), heights=c(4.5, 2))

## Save (Optional)

# ggsave("",
#        width = 6.5, height = 5, plot = g, device = cairo_pdf)
