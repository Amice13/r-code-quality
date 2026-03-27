#------------------------------------------------------------------------------#
# Alternative thresholding approach: Backbones
#------------------------------------------------------------------------------#

library(network)
library(btergm)
library(ggplot2)
library(patchwork)
library(parallel)
library(foreach)


# setup of analysis and select weapon level
start = 1920
end = 1936
maxlag = 4


# load data
load(file = "data/data_ready2.rda")


# select data without threshold
transfers = transfers_adapted_lvl_150


# set included countries (balanced panel)
ind = (exist[, 1928 - 1918] == 1) & (colony[, 1928 - 1918] == 0)
n = sum(ind)


# check density of backbone network
glnet = list()
glnet2 = list()
bbnet = list()
bbnet2 = list()
bbnet5 = list()

for (i in seq_along(transfers)) {
  bbnet[[i]] = network(
    backbone::disparity(
      W = transfers[[i]][ind, ind]
      , alpha = 0.1
      , narrative = FALSE
    )
    , directed = TRUE
  )

  bbnet2[[i]] = network(
    backbone::disparity(
      W = transfers[[i]][ind, ind]
      , alpha = 0.05
      , narrative = FALSE
    )
    , directed = TRUE
  )

  bbnet5[[i]] = network(
    backbone::disparity(
      W = transfers[[i]][ind, ind]
      , alpha = 0.5
      , narrative = FALSE
    )
    , directed = TRUE
  )

  glnet[[i]] = network(
    backbone::global(
      W = transfers[[i]][ind, ind]
      , upper = function(x) x > 0
      , narrative = FALSE
    )
    , directed = TRUE
  )

  glnet2[[i]] = network(
    backbone::global(
      W = transfers[[i]][ind, ind]
      , upper = function(x) x > 0.1
      , narrative = FALSE
    )
    , directed = TRUE
  )
}

dens = data.frame(
  "backbone a0.1" = unlist(lapply(X = bbnet, FUN = network.density)),
  "backbone a0.05" = unlist(lapply(X = bbnet2, FUN = network.density)),
  "backbone a0.5" = unlist(lapply(X = bbnet5, FUN = network.density)),
  "0 USD" = unlist(lapply(X = glnet, FUN = network.density)),
  "1000 USD" = unlist(lapply(X = glnet2, FUN = network.density))
)

matplot(dens, type = "l")
legend("right", colnames(dens), col = seq_len(ncol(dens)), cex = 0.8, fill = seq_len(ncol(dens)))


# add results list
results = data.frame(
  "model" = character(),
  "year" = integer(),
  "name" = numeric(),
  "estimate" = numeric(),
  "se" = numeric(),
  "failure" = logical()
)

# checkme
bbnet = bbnet5

#------------------------------------------------------------------------------#
# Backbone of network by sparsity design.
# Model and decay parameter as choosen in main specification.
#------------------------------------------------------------------------------#

model = net ~ edges + mutual +
  gwodegree(decay = 0.1, fixed = TRUE) +
  gwidegree(decay = 0.1, fixed = TRUE) +
  dgwesp(decay = 1, fixed = TRUE, type = "OTP") +
  dgwesp(decay = 1, fixed = TRUE, type = "OSP") +
  dgwesp(decay = 1, fixed = TRUE, type = "ISP") +
  nodeocov("gdppc") +
  nodeicov("gdppc") +
  absdiff("polity2") +
  diff("cinc", dir = "tail-head") +
  edgecov(defense) +
  edgecov(com_block) +
  edgecov(com_language) +
  edgecov(log_cdistance) +
  edgecov(pathdep)


# sliding window estimation
for (i in (start + maxlag):end) {

  # setup
  net = list()
  pathdep = list()
  help = list()
  defense = list()
  com_block = list()

  # include 3 lagged networks, at the end of the list the most recent
  net[[4]] = bbnet[[i - 1918]]
  net[[3]] = bbnet[[i - 1 - 1918]]
  net[[2]] = bbnet[[i - 2 - 1918]]
  net[[1]] = bbnet[[i - 3 - 1918]]

  # define path dependency
  pathdep[[4]] = bbnet[[i - 1 - 1918]]
  pathdep[[3]] = bbnet[[i - 2 - 1918]]
  pathdep[[2]] = bbnet[[i - 3 - 1918]]
  pathdep[[1]] = bbnet[[i - 4 - 1918]]

  # atop defense
  defense[[4]] = network(atop_defense[[i - 1 - 1918]][ind, ind], directed = TRUE)
  defense[[3]] = network(atop_defense[[i - 2 - 1918]][ind, ind], directed = TRUE)
  defense[[2]] = network(atop_defense[[i - 3 - 1918]][ind, ind], directed = TRUE)
  defense[[1]] = network(atop_defense[[i - 4 - 1918]][ind, ind], directed = TRUE)

  # common block
  com_block[[4]] = common_block[[i - 1 - 1918]][ind, ind]
  com_block[[3]] = common_block[[i - 2 - 1918]][ind, ind]
  com_block[[2]] = common_block[[i - 3 - 1918]][ind, ind]
  com_block[[1]] = common_block[[i - 4 - 1918]][ind, ind]

  # include nodal covariates lagged by 1 time period for each network as vertex attr
  set.vertex.attribute(net[[4]], "gdppc", log(maddison_gdppc[ind, i - 1 - 1918]))
  set.vertex.attribute(net[[3]], "gdppc", log(maddison_gdppc[ind, i - 2 - 1918]))
  set.vertex.attribute(net[[2]], "gdppc", log(maddison_gdppc[ind, i - 3 - 1918]))
  set.vertex.attribute(net[[1]], "gdppc", log(maddison_gdppc[ind, i - 4 - 1918]))

  set.vertex.attribute(net[[4]], "cinc", nmc_cinc[ind, i - 1 - 1918] * 100)
  set.vertex.attribute(net[[3]], "cinc", nmc_cinc[ind, i - 2 - 1918] * 100)
  set.vertex.attribute(net[[2]], "cinc", nmc_cinc[ind, i - 3 - 1918] * 100)
  set.vertex.attribute(net[[1]], "cinc", nmc_cinc[ind, i - 4 - 1918] * 100)

  set.vertex.attribute(net[[4]], "polity2", polity_polity2[ind, i - 1 - 1918])
  set.vertex.attribute(net[[3]], "polity2", polity_polity2[ind, i - 2 - 1918])
  set.vertex.attribute(net[[2]], "polity2", polity_polity2[ind, i - 3 - 1918])
  set.vertex.attribute(net[[1]], "polity2", polity_polity2[ind, i - 4 - 1918])

  # capital, minimal distance, common language, all time-invariant
  log_cdistance = log(capdist[ind, ind] + 1)
  log_mdistance = log(mindist[ind, ind] + 1)
  com_language = common_language[ind, ind]

  # estimate model
  fit = mtergm(
    formula = model
    , verbose = TRUE
    , control = ergm::control.ergm(
      parallel = 4L
      , seed = 0L
      , main.method = "MCMLE"
      , init.method = "MPLE"
      , MCMC.runtime.traceplot = FALSE
    )
  )

  # save results
  tmp = data.frame(
    "model" = "backbone",
    "year" = i,
    "name" = names(fit@coef),
    "estimate" = fit@coef,
    "se" = fit@se,
    "failure" = fit@ergm$failure
  )

  results = rbind(results, tmp)

  cat("\nYear", i, "estimated.\n")
}



#------------------------------------------------------------------------------#
# Save Results
#------------------------------------------------------------------------------#

# add confidence intervals
results$lci = results$estimate - qnorm(0.975) * results$se
results$uci = results$estimate + qnorm(0.975) * results$se

# remove row names
rownames(results) = NULL

# check if any ergm failed
any(results$failure)

# save
saveRDS(results, file = "models/C_estimates_robustness_backbone.rds")




# simple plot
pdf(
  paste0("results/C_estimates_robustness_backbone.pdf")
  , paper = "a4"
  , height = 10.5
)
par(mfrow = c(4, 2))
variables = tmp$name
for (vl in variables) {
  tmp = results[results$name == vl, ]
  plot(
    x = tmp$year, y = tmp$estimate,
    type = "b", pch = 20,
    ylab = "Estimate", xlab = "Year", main = vl,
    ylim = c(
      min(tmp$estimate - 2.1 * tmp$se),
      max(tmp$estimate + 2.1 * tmp$se)
    )
  )
  arrows(
    x0 = tmp$year, y0 = tmp$estimate + qnorm(0.975) * tmp$se,
    x1 = tmp$year, y1 = tmp$estimate - qnorm(0.975) * tmp$se,
    code = 3, angle = 90, length = 0.025
  )
  if ((min(tmp$estimate - 2.1 * tmp$se) <= 0) |
      (0 <= max(tmp$estimate + 2.1 * tmp$se))) {
    abline(h = 0, col = "grey")
  }
}
dev.off()


print(Sys.time())
