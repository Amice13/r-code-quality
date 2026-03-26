##############################################################################
#
#                             Replication scripts
#                             Table A4 - Appendix
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# load data
load("replication_data.RData")
load("main_estimates.rda")

# load libraries
library(econet); library(igraph); library(Matrix)

# store results from table 2 column 2
fit <- main_estimates[[2]]$second_step

#-----------------------------------------------------------------------------
# Centrality measures
#-----------------------------------------------------------------------------

# Loop each network and compute centrality measures
cents <- lapply(1:5, function(x) {
  
  # subset network
  sel <- which(db$time == x)
  tmp <- cosponsorship_network[sel, sel]
  
  # compute standard measures of centrality
  df <- econet:::compute_centralities(tmp, 
                                      directed =  TRUE, 
                                      weighted = FALSE, 
                                      normalization = NULL)      
  
  # compute weighted Bonacich centrality
  n <- nrow(tmp)
  I <- diag(n)
  specification <- summary(fit)$formula
  X_names <- econet:::select_X(specification)
  X <- Reduce("cbind",fit$data[names(fit$data) %in% X_names])
  if(is.null(dim(X))){ X <- matrix(X) }
  colnames(X) <- X_names
  beta_names <- econet:::select_beta(specification)
  beta <- coef(fit)
  beta <- beta[names(beta) %in% beta_names]
  beta <- beta[order(match(names(beta), beta_names))]
  bX <- X %*% beta
  bX <- bX[sel]
  bon <- solve(I - coef(fit)["phi"] * tmp) %*% bX
  
  # store results
  res <- data.frame(deg = df$outdegree,
             btw = df$betweenness,
             clo = df$outcloseness,
             evc = df$eigenvector,
             bon = bon, 
             les = db[sel, "les"],
             id_1 = db[sel, "id_1"])
  
  return(res)
})

# select top centrality measures
high.cents <- lapply(1:5, function(x) {
  
  # select top 10 central Congress member for each metric
  res <- lapply(1:5, function(y) {
    tmp <- cents[[x]][order(cents[[x]][, y], decreasing = T), "id_1"][1:10]
    return(tmp)
  })
  
  # merge results
  res <- do.call("cbind", res)
  colnames(res) <- colnames(cents[[x]])[1:5]
  
  # store results
  res <- data.frame(time = x, res, stringsAsFactors = F)
  
  return(res)
})

#-----------------------------------------------------------------------------
# Associate LES to centrality measures
#-----------------------------------------------------------------------------

les.high.cents <- lapply(1:5, function(x) {
  # select one congress
  tmp <- db[db$time == x, ]
  # select top centrality measures
  tmp.high.cents <- high.cents[[x]]
  # associate LES to each centrality measure
  tmp_les <- lapply(2:6, function(z) {
    # select Congress members
    sel <- which(as.character(tmp$id_1) %in% tmp.high.cents[, z])
    top <- tmp[sel, ]
    # select LES
    top <- top[order(match(c(top$id_1), tmp.high.cents[, z])), "les"]
  })
  
  # store results
  res <- do.call("cbind", tmp_les)
  colnames(res) <- c("Degree", "Betweenness", "Closeness", "Eigenvector", "Katz-Bonacich")
  
  return(res)
})

#-----------------------------------------------------------------------------
# Print results table A.4
#-----------------------------------------------------------------------------

# print results panel a table A.4
les.high.cents[[1]][, c(5, 1:4)]

# print results panel b table A.4
les.high.cents[[2]][, c(5, 1:4)]

# print results panel c table A.4
les.high.cents[[3]][, c(5, 1:4)]

# print results panel d table A.4
les.high.cents[[4]][, c(5, 1:4)]

# print results panel e table A.4
les.high.cents[[5]][, c(5, 1:4)]
