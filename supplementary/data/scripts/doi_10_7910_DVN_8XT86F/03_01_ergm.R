##############################################################################
#
#                             Replication scripts
#                                    ERGM
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# Note
# To speed up replication time, we suggest to run this script 
# while completing the other scripts on separate instances  of R

# WARNING: you need access to parallel computing to run this script

# set seed
set.seed(123456)

# load libraries
library(statnet); library(snow); library(rlecuyer); library(parallel); 
library(doSNOW); library(econet); library(tidyr); library(ggplot2)
library(purrr)

# load data
load("replication_data.RData")
load("network.rda")

# load function to plot gof
source("plot_my_gof.R")

# set number of cores for parallel computing
np = detectCores()
# WARNING:
# detectCores() is equal to the total number of cores on your machine
# if you are running other tasks on your machine, you should decrease
# this value to the number of available cores on your machine.

#-----------------------------------------------------------------------------
# Fit ERGM
#-----------------------------------------------------------------------------

# WARNING: This may take a long time to run (i.e. more than a day).
fit <- ergm(network ~ nodematch("party") + nodematch("gender", diff = T, keep = 2) +
              nodematch("nowhite", diff = T, keep = 2) + 
              nodemix("nchair", base = c(1, 4)) + 
              absdiff("dw", pow = 1) + 
              nodematch("state") + smalldiff("seniority", 8) + 
              edgecov("com_mat") + 
              edgecov("alumni") + 
              gwesp(2, T) + gwidegree(0.35, T), 
            constraints = ~ edges,
            target.stats = setNames(c(11389, 765, 299, 1083, 485, 3565, 1359, 
                                      13445, 3801, 84, 81113, 587), 
                                    c("nodematch.party", "nodematch.gender.1", 
                                      "nodematch.nowhite.1", "mix.nchair.1.0", 
                                      "mix.nchair.0.1", "absdiff.dw", "nodematch.state", 
                                      "smalldiff.seniority8", "edgecov.com_mat", 
                                      "edgecov.alumni", "gwesp.fixed.2", 
                                      "gwideg.fixed.0.35")),
            control = control.ergm(init = c(nodematch.party = 0.222081306768973, nodematch.gender.1 = 0.0792685675604226, 
                                            nodematch.nowhite.1 = 0.170442393337002, mix.nchair.1.0 = 0.572711286847399, 
                                            mix.nchair.0.1 = -0.513184875666672, absdiff.dw = -0.128190920562295, 
                                            nodematch.state = 0.741901076189042, smalldiff.seniority8 = 0.0315385231136044, 
                                            edgecov.com_mat = 0.231877303154268, edgecov.alumni = -0.152285381538464, 
                                            gwesp.fixed.2 = 0.382963786306155, gwideg.fixed.0.35 = 62.1568772459143), 
                                   parallel = np, parallel.type = "SOCK", 
                                   MCMLE.maxit = 500), verbose = T)

#-----------------------------------------------------------------------------
# Print table A8
# Note: depending on the internal seed chosen by the function "ergm",
# results might be slightly different from table A8.
# However, the signs and the statistical significance of the 
# estimates will be consistent.
#-----------------------------------------------------------------------------

summary(fit)

#-----------------------------------------------------------------------------
# Plot GOF (Figure A1)
#-----------------------------------------------------------------------------

figure_a1 <- my.gof(g = network, fit = fit)
figure_a1[[1]]
figure_a1[[2]]
figure_a1[[3]]

#-----------------------------------------------------------------------------
# Simulate networks
#-----------------------------------------------------------------------------

# set number of simulations from ergm
nsim <- 500

# simulate networks
# WARNING: This may take a long time to run (i.e. more than a day).
sim_nets <- simulate(fit, nsim = nsim)

# store ids to assign
ids <- unlist(subset(db, time %in% 5, select = "id_1"))

# transform networks in adjacency matrices
adj_mat <- list()
for(i in 1:length(sim_nets)) {
  tmp <- sim_nets[[i]][,]
  rownames(tmp) <- colnames(tmp) <- ids
  adj_mat[[i]] <- tmp
}

# Free up RAM
remove(sim_nets, tmp); gc()

# column-normalize adjacency matrices
adj_mat <- lapply(adj_mat, function(m) {
  m_sum <- apply(m, 2, sum)
  m_sum <- ifelse(is.infinite(1/m_sum), 0 , 1/m_sum)
  m <- m %*% diag(m_sum)
  m
})


#-----------------------------------------------------------------------------
# Fit model with peer effects using simulated networks
#-----------------------------------------------------------------------------

# create data
db <- subset(db, time %in% 5)

# prepare for parallel computing
cl <- makeCluster(6)
registerDoSNOW(cl)
iterations <- nsim
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader")

# start parallel computing
# WARNING: This may take a very long time to run (i.e. more than two months).
result <- foreach(i = 1:iterations, .packages = c("econet"), 
                  .options.snow = opts, .errorhandling='remove') %dopar%
                  {
                    
                    tmp <- net_dep(formula = ff, data = db,
                                   G = adj_mat[[i]], model = "model_B", estimation = "NLLS",
                                   hypothesis = "lim", endogeneity = FALSE,
                                   to_weight = db$weights)
                    
                    # remove original data and keep only results
                    tmp[[1]]$data <- NULL
                    return(tmp)
                  }

# Store simulate autoregressive parameter
phi <- do.call("c", lapply(result, function(x) coef(x$second_step)["phi"]))
phi <- data.frame(phi = phi)

#-----------------------------------------------------------------------------
# Plot figure 2
#-----------------------------------------------------------------------------

figure_2 <- ggplot(data = phi, aes(phi)) + geom_histogram() + theme_bw() +
  labs(title = expression("Simulated"~phi~"distribution"), 
       x = expression(phi), y = "frequency") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(hjust = 1, size = 10),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5)) +
  scale_x_continuous(breaks = round(c(min(phi$phi), median(phi$phi), max(phi$phi)), 2))

figure_2
