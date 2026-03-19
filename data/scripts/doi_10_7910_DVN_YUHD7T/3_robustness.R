#------------------------------------------------------------------------------#
# Estimate robustness checks.
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


# we exclude colonies and include only countries present at given time point
# note: we have almost a balanced panel, only assuming egypt and ireland in 1920
ind = (exist[, 1928 - 1918] == 1) & (colony[, 1928 - 1918] == 0)
n = sum(ind)


# add results list
results = data.frame(
  "model" = character(),
  "year" = integer(),
  "name" = numeric(),
  "estimate" = numeric(),
  "se" = numeric(),
  "failure" = logical()
)


#------------------------------------------------------------------------------#
# Robustness Model 1 + 2
# Levels of SALW Network Threshold
# Model and decay parameter as choosen before.
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

for (threshold in 1:2) {

  # load corresponding data set
  transfers = switch(threshold,
    transfers_adapted_lvl_150_binary,
    transfers_adapted_lvl_150_binary_150
  )

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
    fit = mtergm(model, verbose = TRUE, control = ergm::control.ergm(parallel = 4L, seed = 0, main.method = "MCMLE", init.method = "CD"))

    # save results
    tmp = data.frame(
      "model" = ifelse(threshold == 1, "50", "150"),
      "year" = i,
      "name" = names(fit@coef),
      "estimate" = fit@coef,
      "se" = fit@se,
      "failure" = fit@ergm$failure
    )

    results = rbind(results, tmp)

    cat("\nYear", i, "estimated.\n")
  }
}



#------------------------------------------------------------------------------#
# Robustness Model 3
# Select 'conventional' decay values. Other approaches as transitive + intransitive
# or triadcensus(levels = c(13, 9, 5)) lead to degenerated models.
#------------------------------------------------------------------------------#

transfers = transfers_adapted_lvl_150_binary_100

# estimate with default values in decay
model = net ~ edges + mutual +
  gwodegree(decay = 0.5, fixed = TRUE) +
  gwidegree(decay = 0.5, fixed = TRUE) +
  dgwesp(decay = 1.5, fixed = TRUE, type = "OTP") +
  dgwesp(decay = 1.5, fixed = TRUE, type = "OSP") +
  dgwesp(decay = 1.5, fixed = TRUE, type = "ISP") +
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
  fit = mtergm(model, verbose = TRUE, control = ergm::control.ergm(parallel = 4L, seed = 0, main.method = "MCMLE", init.method = "CD"))

  # save results
  tmp = data.frame(
    "model" = "default",
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
# Robustness Model 5
# Change window length, estimate model over 3 time periods with one lag.
#------------------------------------------------------------------------------#

transfers = transfers_adapted_lvl_150_binary_100

# same model from main specification
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
for (i in (start + maxlag - 1):end) {

  # setup
  net = list()
  pathdep = list()
  help = list()
  defense = list()
  com_block = list()

  # include 3 lagged networks, at the end of the list the most recent
  net[[3]] = network(transfers[[i - 1918]][ind, ind], directed = TRUE)
  net[[2]] = network(transfers[[i - 1 - 1918]][ind, ind], directed = TRUE)
  net[[1]] = network(transfers[[i - 2 - 1918]][ind, ind], directed = TRUE)

  # define path dependency
  pathdep[[3]] = network(transfers[[i - 1 - 1918]][ind, ind], directed = TRUE)
  pathdep[[2]] = network(transfers[[i - 2 - 1918]][ind, ind], directed = TRUE)
  pathdep[[1]] = network(transfers[[i - 3 - 1918]][ind, ind], directed = TRUE)

  # atop defense
  defense[[3]] = network(atop_defense[[i - 1 - 1918]][ind, ind], directed = TRUE)
  defense[[2]] = network(atop_defense[[i - 2 - 1918]][ind, ind], directed = TRUE)
  defense[[1]] = network(atop_defense[[i - 3 - 1918]][ind, ind], directed = TRUE)

  # common block
  com_block[[3]] = common_block[[i - 1 - 1918]][ind, ind]
  com_block[[2]] = common_block[[i - 2 - 1918]][ind, ind]
  com_block[[1]] = common_block[[i - 3 - 1918]][ind, ind]

  # include nodal covariates lagged by 1 time period for each network as vertex attr
  set.vertex.attribute(net[[3]], "gdppc", log(maddison_gdppc[ind, i - 1 - 1918]))
  set.vertex.attribute(net[[2]], "gdppc", log(maddison_gdppc[ind, i - 2 - 1918]))
  set.vertex.attribute(net[[1]], "gdppc", log(maddison_gdppc[ind, i - 3 - 1918]))

  set.vertex.attribute(net[[3]], "cinc", nmc_cinc[ind, i - 1 - 1918] * 100)
  set.vertex.attribute(net[[2]], "cinc", nmc_cinc[ind, i - 2 - 1918] * 100)
  set.vertex.attribute(net[[1]], "cinc", nmc_cinc[ind, i - 3 - 1918] * 100)

  set.vertex.attribute(net[[3]], "polity2", polity_polity2[ind, i - 1 - 1918])
  set.vertex.attribute(net[[2]], "polity2", polity_polity2[ind, i - 2 - 1918])
  set.vertex.attribute(net[[1]], "polity2", polity_polity2[ind, i - 3 - 1918])

  # capital, minimal distance, common language, all time-invariant
  log_cdistance = log(capdist[ind, ind] + 1)
  log_mdistance = log(mindist[ind, ind] + 1)
  com_language = common_language[ind, ind]

  # estimate model
  fit = mtergm(model, verbose = TRUE, control = ergm::control.ergm(parallel = 4L, seed = 0, main.method = "MCMLE", init.method = "CD"))

  # save results
  tmp = data.frame(
    "model" = "window",
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
# Robustness Model 6
# CINC Difference with Nodal Covariates exchanged.
# Model and decay parameter as choosen before.
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
  nodeocov("cinc") +
  nodeicov("cinc") +
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
  fit = mtergm(model, verbose = TRUE, control = ergm::control.ergm(parallel = 4L, seed = 0, main.method = "MCMLE", init.method = "CD"))

  # save results
  tmp = data.frame(
    "model" = "cinc_nodal",
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
# Robustness Model 7
# Interact CINC Difference with Defense
# Model and decay parameter as choosen before.
#------------------------------------------------------------------------------#

interact = list()

model = net ~ edges + mutual +
  gwodegree(decay = 1, fixed = TRUE) + # CHECKME
  gwidegree(decay = 1, fixed = TRUE) +
  dgwesp(decay = 1, fixed = TRUE, type = "OTP") +
  dgwesp(decay = 1, fixed = TRUE, type = "OSP") +
  dgwesp(decay = 1, fixed = TRUE, type = "ISP") +
  nodeocov("gdppc") +
  nodeicov("gdppc") +
  absdiff("polity2") +
  diff("cinc", dir = "tail-head") +
  edgecov(defense) +
  edgecov(interact) +
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

  # add interaction
  interact[[4]] = outer(nmc_cinc[ind, i - 1 - 1918] * 100, nmc_cinc[ind, i - 1 - 1918] * 100, FUN = "-") * atop_defense[[i - 1 - 1918]][ind, ind]
  interact[[3]] = outer(nmc_cinc[ind, i - 2 - 1918] * 100, nmc_cinc[ind, i - 2 - 1918] * 100, FUN = "-") * atop_defense[[i - 2 - 1918]][ind, ind]
  interact[[2]] = outer(nmc_cinc[ind, i - 3 - 1918] * 100, nmc_cinc[ind, i - 2 - 1918] * 100, FUN = "-") * atop_defense[[i - 3 - 1918]][ind, ind]
  interact[[1]] = outer(nmc_cinc[ind, i - 4 - 1918] * 100, nmc_cinc[ind, i - 4 - 1918] * 100, FUN = "-") * atop_defense[[i - 4 - 1918]][ind, ind]

  # capital, minimal distance, common language, all time-invariant
  log_cdistance = log(capdist[ind, ind] + 1)
  log_mdistance = log(mindist[ind, ind] + 1)
  com_language = common_language[ind, ind]

  # estimate model
  fit = mtergm(model, verbose = TRUE, control = ergm::control.ergm(parallel = 4L, seed = 0, main.method = "MCMLE", init.method = "CD"))

  # save results
  tmp = data.frame(
    "model" = "cinc_interaction",
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
# Robustness Model 8
# Substitute CINC with Military Expenditures
# Model and decay parameter as choosen before.
#------------------------------------------------------------------------------#

model = net ~ edges + mutual +
  gwodegree(decay = 0.1, fixed = TRUE) + #CHECKME
  gwidegree(decay = 0.1, fixed = TRUE) +
  dgwesp(decay = 1, fixed = TRUE, type = "OTP") +
  dgwesp(decay = 1, fixed = TRUE, type = "OSP") +
  dgwesp(decay = 1, fixed = TRUE, type = "ISP") +
  nodeocov("gdppc") +
  nodeicov("gdppc") +
  absdiff("polity2") +
  diff("milexp", dir = "tail-head") +
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

  set.vertex.attribute(net[[4]], "milexp", log(nmc_milexp[ind, i - 1 - 1918]))
  set.vertex.attribute(net[[3]], "milexp", log(nmc_milexp[ind, i - 2 - 1918]))
  set.vertex.attribute(net[[2]], "milexp", log(nmc_milexp[ind, i - 3 - 1918]))
  set.vertex.attribute(net[[1]], "milexp", log(nmc_milexp[ind, i - 4 - 1918]))

  set.vertex.attribute(net[[4]], "polity2", polity_polity2[ind, i - 1 - 1918])
  set.vertex.attribute(net[[3]], "polity2", polity_polity2[ind, i - 2 - 1918])
  set.vertex.attribute(net[[2]], "polity2", polity_polity2[ind, i - 3 - 1918])
  set.vertex.attribute(net[[1]], "polity2", polity_polity2[ind, i - 4 - 1918])

  # capital, minimal distance, common language, all time-invariant
  log_cdistance = log(capdist[ind, ind] + 1)
  log_mdistance = log(mindist[ind, ind] + 1)
  com_language = common_language[ind, ind]

  # estimate model
  fit = mtergm(model, verbose = TRUE, control = ergm::control.ergm(parallel = 4L, seed = 0, main.method = "MCMLE", init.method = "CD"))

  # save results
  tmp = data.frame(
    "model" = "cinc_milexp",
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
saveRDS(results, file = "models/C_estimates_robustness.rds")

# load
# results = readRDS(file = "models/C_estimates_robustness.rds")
