#------------------------------------------------------------------------------#
# Estimate main model specification
#------------------------------------------------------------------------------#

library(network)
library(ergm)
library(btergm)


# setup
start = 1920
end = 1936
maxlag = 4


# load data
load(file = "data/data_ready.rda")


# load relevant ergMargins functions
# the cran version is not up to date, we use the version found on github and replace
# edgecov.offset by offset(edgecov.offsmat) to restore functionality
ergMargins_func = list.files(path = file.path("utils", "ergMargins"))
lapply(ergMargins_func, function(char) source(file = file.path("utils", "ergMargins", char)))


#------------------------------------------------------------------------------#
# Main specification: Edges, Mutual, Indegree, Outdegree, GWESP: OTP OSP ISP.
# Decay param default chosen
#------------------------------------------------------------------------------#


# select data threshold at 100USD
transfers = transfers_adapted_lvl_150_binary_100


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


# we exclude colonies and include only countries present at given time point
# note: we have almost a balanced panel, only assuming egypt and ireland in 1920
ind = (exist[, 1928 - 1918] == 1) & (colony[, 1928 - 1918] == 0)
n = sum(ind)


# init results list
results = list()
simulations = list()
avg_marginaleff = list()

# sliding window estimation
for (i in (start + maxlag):end) {

  # setup
  net = list()
  pathdep = list()
  help = list()
  defense = list()
  com_block = list()

  # include 3 lagged networks, at the end of the list the most recent
  net[[4]] = network(transfers[[i - 1918]][ind, ind], directed = TRUE)
  net[[3]] = network(transfers[[i - 1 - 1918]][ind, ind], directed = TRUE)
  net[[2]] = network(transfers[[i - 2 - 1918]][ind, ind], directed = TRUE)
  net[[1]] = network(transfers[[i - 3 - 1918]][ind, ind], directed = TRUE)

  # define path dependency
  pathdep[[4]] = network(transfers[[i - 1 - 1918]][ind, ind], directed = TRUE)
  pathdep[[3]] = network(transfers[[i - 2 - 1918]][ind, ind], directed = TRUE)
  pathdep[[2]] = network(transfers[[i - 3 - 1918]][ind, ind], directed = TRUE)
  pathdep[[1]] = network(transfers[[i - 4 - 1918]][ind, ind], directed = TRUE)

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
  control = ergm::control.ergm(parallel = 4L, seed = 1, main.method = "MCMLE", init.method = "CD")
  fit = mtergm(model, verbose = TRUE, control = control)

  # estimate average marginal effects
  ame = data.frame(
    rbind(
      ergm.AME(model = fit, var1 = "diff.tail-head.cinc"),
      ergm.AME(model = fit, var1 = "edgecov.defense")
    ),
    year = i
  )

  # compute goodness of fit statistics
  stats = c(dsp, esp, odeg, ideg, geodesic, rocpr)
  sim = gof(fit, statistics = stats, verbose = TRUE, nsim = 1000L, parallel = "snow", ncpus = 4L)

  results[[i + 1 - start - maxlag]] = fit
  simulations[[i + 1 - start - maxlag]] = sim
  avg_marginaleff[[i + 1 - start - maxlag]] = ame
  cat("\nYear", i, "estimated.\n")
}


# extract estimates
tmp = list()

for (i in 1:length(results)) {
  tmp[[i]] = data.frame(
    "estimate" = results[[i]]@coef,
    "se" = results[[i]]@se,
    "year" = start + maxlag + i - 1,
    "name" = names(results[[i]]@coef),
    "failure" = results[[i]]@ergm$failure
  )
}

coefs = do.call(rbind, tmp)
rownames(coefs) = NULL
names = unique(coefs$name)

# avg_marginaleff = rbind(avg_marginaleff)
# colnames(avg_marginaleff) = c("coef", "ame", "se", "z", "p", "year")

# check
any(coefs$failure)


# save
save.image("models/A_main_workspace.rda")


# load
# load(file = "models/A_main_workspace.rda")


#------------------------------------------------------------------------------#
# Preliminary plots and analysis.
#------------------------------------------------------------------------------#

# gof statistics
if (TRUE) {
  pdf(
    paste0("results/A_main_gof_statistics.pdf"),
    paper = "a4r", width = 10
  )
  par(mfrow = c(2, 3))
  for (i in 1:length(simulations)) {
    plot(simulations[[i]])
  }
  dev.off()
}


# simple plot
pdf(
  paste0("results/A_main_plot.pdf")
  , paper = "a4"
  , height = 10.5
)
par(mfrow = c(4, 2))
for (vl in names) {
  tmp = coefs[coefs$name == vl, ]
  plot(
    x = tmp$year, y = tmp$estimate, type = "b", pch = 20,
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
