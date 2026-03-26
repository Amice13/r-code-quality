##############################################################################
#
#                             Replication scripts
#                                 Horse Race
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# Note 1:
# This script should be run after completing
# 01_01_main_estimates_point_estimates

# Note 2
# To speed up replication time, this script may be run concurrently with
# 02_01_main_estimates_school_fixed_effects_point_estimates
# 02_03_party_effects_ties_effects_timing_point_estimates
# by using two additional instances of R

# load data
load("replication_data.RData")

# load libraries
library(econet); library(spatstat.utils)

#-----------------------------------------------------------------------------
# Compute centralities
#-----------------------------------------------------------------------------

# index time
tt <- db$time; k <- unique(tt)

# begin loop to compute alumni network centralities in each congress
alumni_network_cent_mes <- Reduce("rbind",lapply(k, function(kk){
    tmp <- alumni_network[tt==kk,tt==kk]
    econet:::compute_centralities(tmp,directed = TRUE, 
                                  weighted = FALSE, normalization = NULL)
}))

# begin loop to compute cosponsorship network centralities in each congress
cosponsorship_network_cent_mes <- Reduce("rbind",lapply(k, function(kk){
    tmp <- cosponsorship_network[tt==kk,tt==kk]
    econet:::compute_centralities(tmp, directed = TRUE, 
                                  weighted = FALSE, normalization = NULL)      
}))

# Ressign name to cosponsorship network centralities
colnames(alumni_network_cent_mes)[colnames(alumni_network_cent_mes) == "outdegree"] <- "degree"
colnames(alumni_network_cent_mes)[colnames(alumni_network_cent_mes) == "outcloseness"] <- "closeness"
colnames(cosponsorship_network_cent_mes)[colnames(cosponsorship_network_cent_mes) == "outdegree"] <- "degree"
colnames(cosponsorship_network_cent_mes)[colnames(cosponsorship_network_cent_mes) == "outcloseness"] <- "closeness"

# store centrality names
centralities <- c("degree", "betweenness", "closeness", "eigenvector")

# Select network centralities
selected_measures <- colnames(alumni_network_cent_mes)[colnames(alumni_network_cent_mes)%in%centralities]
alumni_network_cent_mes  <- as.matrix(alumni_network_cent_mes [, c(selected_measures)])
cosponsorship_network_cent_mes <- as.matrix(cosponsorship_network_cent_mes[, c(selected_measures)])

# Reassign name to centrality measures
colnames(alumni_network_cent_mes) <- paste0("instrument_", colnames(alumni_network_cent_mes))

# cbind db, cosponsorship_network_cent_mes and alumni_network_cent_mes
data <- cbind(db, cosponsorship_network_cent_mes, alumni_network_cent_mes)

#-----------------------------------------------------------------------------
# Create endogeneity correction
#-----------------------------------------------------------------------------

ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

regressors <- paste(termsinformula(ff), collapse = " + ")
dependent <- as.character(ff)[2]

# Loop: iteratively create a formula, run ols, and generate unobservables
first_step_list <- list()
unobs_cent_mes  <- list()

for(i in 1:ncol(alumni_network_cent_mes)){
    to_fit <- formula(paste0(colnames(cosponsorship_network_cent_mes)[i], "~",
                             regressors, " + ", 
                             colnames(alumni_network_cent_mes)[i]))
    first_step_list[[i]] <- lm(to_fit, data)
    unobs_cent_mes[[i]] <- residuals(first_step_list[[i]])
}

# Store unobservables in data
unobs_cent_mes <- as.matrix(Reduce("cbind", unobs_cent_mes))
colnames(unobs_cent_mes) <- paste0("unobs_", centralities)
data <- cbind(data, unobs_cent_mes)

# Rescale variables
data[, "betweenness"] <- data[, "betweenness"] * 1e+02
data[, "instrument_betweenness"] <- data[, "instrument_betweenness"] * 1e+02
data[, "unobs_betweenness"] <- data[, "unobs_betweenness"] * 10
data[, "closeness"] <- data[, "closeness"] * 1e+06
data[, "instrument_closeness"] <- data[, "instrument_closeness"] * 1e+06
data[, "unobs_closeness"] <- data[, "unobs_closeness"] * 1e+06
data[, "eigenvector"] <- data[, "eigenvector"] * 100
data[, "instrument_eigenvector"] <- data[, "instrument_eigenvector"] * 100
data[, "unobs_eigenvector"] <- data[, "unobs_eigenvector"] * 100

#-----------------------------------------------------------------------------
# Model with centrality measures (not controlling endogeneity)
#-----------------------------------------------------------------------------

horse_main_no_unobs <- list()
horse_main_no_unobs[[1]] <- lm(formula(les ~ gender + nowhite + party + seniority + margin + dw +  deleg_size + nchair + maj_leader + min_leader + speaker +  time + degree), data = data)
horse_main_no_unobs[[2]] <- lm(formula(les ~ gender + nowhite + party + seniority + margin + dw +  deleg_size + nchair + maj_leader + min_leader + speaker +  time + betweenness), data = data)
horse_main_no_unobs[[3]] <- lm(formula(les ~ gender + nowhite + party + seniority + margin + dw +  deleg_size + nchair + maj_leader + min_leader + speaker +  time + closeness), data = data)
horse_main_no_unobs[[4]] <- lm(formula(les ~ gender + nowhite + party + seniority + margin + dw +  deleg_size + nchair + maj_leader + min_leader + speaker +  time + eigenvector), data = data)

#-----------------------------------------------------------------------------
# Model with centrality measures (controlling endogeneity)
#-----------------------------------------------------------------------------

# function to perform bootstrap
lm_to_boot <- function(lm.object, R, group){
    boot.list <- list()
    res <- residuals(lm.object)
    mframe <- model.frame(lm.object)
    nobs <- nrow(mframe)
    id_group <- unique(group)
    
    for (i in 1:R) {
        xx <- 1:nobs
        for(n in 1:length(id_group)){
            sel <- which(group %in% id_group[n])
            if(length(sel)==1){
                xx[sel] <- xx[sel]
            } else {
                xx[sel] <- sample(xx[sel], replace = T)
            }
        }
        
        mf <- mframe
        les <- mf[,"les"] 
        mf[,"les"] <- les - res[xx]
        rs.lm <- update(lm.object, data = mf)
        boot.list[[i]] <- coef(rs.lm)
    }
    return(boot.list) 
}

# function to compute standard errors and pvalues
booted <- function(fit, x) {
    beta.estimate <- coef(fit)
    std.error <- x
    t_value <- beta.estimate/std.error
    p_value <- 2*pt(-abs(t_value), df = nobs(fit) - length(beta.estimate))
    res <- cbind(beta.estimate, std.error, t_value, p_value)
    return(res)
}

# run iteratively ols and bootstraps
horse_main_unobs <- list()
booted_horse_ols <- list()
for(i in 1:ncol(cosponsorship_network_cent_mes)){
    to_fit <- formula(paste0(dependent," ~ ", regressors,
                             " + ", colnames(cosponsorship_network_cent_mes)[i],
                             " + ", colnames(unobs_cent_mes)[i]))
    horse_main_unobs[[i]] <- lm(to_fit, data)
    fit <- lm_to_boot(lm.object = horse_main_unobs[[i]], R = 500, group = db$id_2)
    fit <- do.call("cbind", fit)
    booted_horse_ols[[i]] <- apply(fit,1,sd)
}

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

# These are the point estimates for column c(1:8) in Table A.3
fit_horse_ols <- list(horse_main_no_unobs[[1]], horse_main_unobs[[1]],
                      horse_main_no_unobs[[2]], horse_main_unobs[[2]],
                      horse_main_no_unobs[[3]], horse_main_unobs[[3]],
                      horse_main_no_unobs[[4]], horse_main_unobs[[4]])

#-----------------------------------------------------------------------------
# Model with peer effects (controlling endogeneity)
#-----------------------------------------------------------------------------

ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
                   deleg_size + nchair + maj_leader + min_leader + speaker + 
                   degree + unobs_degree + 
                   betweenness + unobs_betweenness +
                   closeness + unobs_closeness +
                   eigenvector + unobs_eigenvector + 
                   unobs + time")

load("main_estimates.rda")
data$unobs <- main_estimates[[3]]$second_step$data$unobservables

horse_nls <- net_dep(formula = ff, data = data,
                     G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                     hypothesis = "lim", endogeneity = FALSE,
                     to_weight = db$weights,
                     time_fixed_effect = "time")

# print point estimates column 9 table A.3
summary(horse_nls)

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

save(horse_nls, file = "horse_nls.rda")
