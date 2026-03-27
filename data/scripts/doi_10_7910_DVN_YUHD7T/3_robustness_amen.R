#------------------------------------------------------------------------------#
# Alternative modelling approach: Additive and Multiplicative Effects Network Model
#------------------------------------------------------------------------------#

library(amen)
library(ggplot2)
library(patchwork)
library(parallel)
library(foreach)


# setup of analysis and select weapon level
start = 1920
end = 1936
maxlag = 4


# load data
load(file = "data/data_ready.rda")


# select data threshold at 100USD
transfers = transfers_adapted_lvl_150_binary_100


# transform
nmc_cinc = nmc_cinc * 100


# save
results = list()


# set included countries (balanced panel)
ind = (exist[, 1928 - 1918] == 1) & (colony[, 1928 - 1918] == 0)
n = sum(ind)


# setup parallel computing
ncpus = 4L
mc = parallel::makeCluster(ncpus, type = "PSOCK")
doParallel::registerDoParallel(cl = mc)


# estimate
results = foreach(i = (start + maxlag):end, .packages = c("amen")) %dopar% {

  # outcome
  Y = array(
    c(
      transfers[[i - 3 - 1918]][ind, ind],
      transfers[[i - 2 - 1918]][ind, ind],
      transfers[[i - 1 - 1918]][ind, ind],
      transfers[[i - 1918]][ind, ind]
    ),
    dim = c(n, n, maxlag)
  )

  n = dim(Y)[1]
  t = dim(Y)[3]


  # nodal covariates
  Xnode = array(NA, dim = c(n, 1, t))
  dimnames(Xnode)[[2]] = c("gdppc")

  Xnode[, 1, ] = array(
    c(
      log(maddison_gdppc[ind, i - 4 - 1918]),
      log(maddison_gdppc[ind, i - 3 - 1918]),
      log(maddison_gdppc[ind, i - 2 - 1918]),
      log(maddison_gdppc[ind, i - 1 - 1918])
    ),
    dim = c(n, maxlag)
  )


  # dyadic covariates
  Xdyad = array(NA, dim = c(n, n, 7, t))

  dimnames(Xdyad)[[3]] = c(
    "polity2_absdiff", "cinc_diff", "defense", "com_block", "com_language", "log_cdistance", "pathdep"
  )

  Xdyad[, , 1, ] = array(
    c(
      abs(outer(polity_polity2[ind, i - 4 - 1918], polity_polity2[ind, i - 4 - 1918], "-")),
      abs(outer(polity_polity2[ind, i - 3 - 1918], polity_polity2[ind, i - 3 - 1918], "-")),
      abs(outer(polity_polity2[ind, i - 2 - 1918], polity_polity2[ind, i - 2 - 1918], "-")),
      abs(outer(polity_polity2[ind, i - 1 - 1918], polity_polity2[ind, i - 1 - 1918], "-"))
    ),
    dim = c(n, n, maxlag)
  )

  Xdyad[, , 2, ] = array(
    c(
      outer(nmc_cinc[ind, i - 4 - 1918], nmc_cinc[ind, i - 4 - 1918], "-"),
      outer(nmc_cinc[ind, i - 3 - 1918], nmc_cinc[ind, i - 3 - 1918], "-"),
      outer(nmc_cinc[ind, i - 2 - 1918], nmc_cinc[ind, i - 2 - 1918], "-"),
      outer(nmc_cinc[ind, i - 1 - 1918], nmc_cinc[ind, i - 1 - 1918], "-")
    ),
    dim = c(n, n, maxlag)
  )

  Xdyad[, , 3, ] = array(
    c(
      atop_defense[[i - 4 - 1918]][ind, ind],
      atop_defense[[i - 3 - 1918]][ind, ind],
      atop_defense[[i - 2 - 1918]][ind, ind],
      atop_defense[[i - 1 - 1918]][ind, ind]
    ),
    dim = c(n, n, maxlag)
  )

  Xdyad[, , 4, ] = array(
    c(
      common_block[[i - 4 - 1918]][ind, ind],
      common_block[[i - 3 - 1918]][ind, ind],
      common_block[[i - 2 - 1918]][ind, ind],
      common_block[[i - 1 - 1918]][ind, ind]
    ),
    dim = c(n, n, maxlag)
  )

  Xdyad[, , 5, ] = array(
    c(
      common_language[ind, ind],
      common_language[ind, ind],
      common_language[ind, ind],
      common_language[ind, ind]
    ),
    dim = c(n, n, maxlag)
  )

  Xdyad[, , 6, ] = array(
    c(
      log(capdist[ind, ind] + 1),
      log(capdist[ind, ind] + 1),
      log(capdist[ind, ind] + 1),
      log(capdist[ind, ind] + 1)
    ),
    dim = c(n, n, maxlag)
  )

  Xdyad[, , 7, ] = array(
    c(
      transfers[[i - 4 - 1918]][ind, ind],
      transfers[[i - 3 - 1918]][ind, ind],
      transfers[[i - 2 - 1918]][ind, ind],
      transfers[[i - 1 - 1918]][ind, ind]
    ),
    dim = c(n, n, maxlag)
  )


  # estimate model
  fitAME = ame_rep(
    Y = Y, Xdyad = Xdyad, Xrow = Xnode, Xcol = Xnode,
    family = "bin", R = 2L, rvar = TRUE, cvar = TRUE, dcor = TRUE,
    intercept = TRUE, symmetric = FALSE, seed = 1,
    nscan = 2 * 10000, burn = 2 * 500, odens = 25,
    plot = FALSE, print = FALSE, gof = TRUE
  )

  return(fitAME)

  # plot(fitAME)
  # summary(fitAME)
  # gofstats(fitAME)
}

# stop parallel computing
parallel::stopCluster(cl = mc)


# extract posterior estimates + sd
tmp = list()

for (i in seq_along(results)) {
  year = start + maxlag - 1 + i
  tmp[[i]] = data.frame(
    "coef" = colnames(results[[i]]$BETA),
    "pmean" = apply(results[[i]]$BETA, 2, mean),
    "psd" = apply(results[[i]]$BETA, 2, sd),
    "year" = year
  )
}

pestimates = do.call(rbind, tmp)


# save
save(results, pestimates, file = "models/Z_amen_estimates.rda")


# load
# load(file = paste0("models/Z_amen_estimates.rda"))


# plot gof statistics
pdf(paste0("results/Z_amen_gof_statistics.pdf"), paper = "a4r", width = 10.5)
par(mfrow = c(3, 2))
for (fit in results) {
  plot(fit)
}
dev.off()


# plot posterior estimates
pdf(paste0("results/Z_amen_estimates_plot.pdf"), paper = "a4", height = 10.5)
par(mfrow = c(4, 2))
for (coef in unique(pestimates$coef)) {
  tmp = pestimates[pestimates$coef == coef, ]
  plot(
    x = tmp$year, y = tmp$pmean, type = "b", pch = 20,
    ylab = "Estimate", xlab = "Year", main = paste(coef),
    ylim = c(
      min(tmp$pmean - 2.1 * tmp$psd),
      max(tmp$pmean + 2.1 * tmp$psd)
    )
  )
  arrows(
    x0 = tmp$year, y0 = tmp$pmean + qnorm(0.975) * tmp$psd,
    x1 = tmp$year, y1 = tmp$pmean - qnorm(0.975) * tmp$psd,
    code = 3, angle = 90, length = 0.025
  )
  if ((min(tmp$pmean - 2.1 * tmp$psd) <= 0) |
    (0 <= max(tmp$pmean + 2.1 * tmp$psd))) {
    abline(h = 0, col = "grey")
  }
}
dev.off()


# plot additive multiplicative effects
countrycodes = nodelist$stateabb[ind]

pdf(paste0("results/Z_amen_random_effects.pdf"), paper = "a4r", width = 10.5)
par(mfrow = c(1, 4))
for (fit in results) {

  # additive effects
  tmp = data.frame(countrycodes, fit$APM, fit$BPM, stringsAsFactors = F)

  tmp = tmp[order(tmp$fit.APM), ]
  plot(tmp$fit.APM, seq_along(tmp$fit.APM), ylab = "", xlab = expression(a[i]), pch = 16, yaxt = "n", main = "Sender Effect")
  axis(2, at = seq_along(tmp$fit.APM), labels = tmp$countrycodes, las = 1, cex = 0.8)

  tmp = tmp[order(tmp$fit.BPM), ]
  plot(tmp$fit.BPM, seq_along(tmp$fit.BPM), ylab = "", xlab = expression(a[i]), pch = 16, yaxt = "n", main = "Receiver Effect")
  axis(2, at = seq_along(tmp$fit.BPM), labels = tmp$countrycodes, las = 1, cex = 0.8)
}

par(mfrow = c(1, 1))
for (i in seq_along(results)) {
  fit = results[[i]]
  year = c((start + maxlag):end)[i]
  Y = transfers[[year - 1918]][ind, ind]

  # multiplicative effects
  # groups with common receiving pattern
  circplot(Y, U = fit$U, V = fit$V, row.names = nodelist$stateabb[ind], col.names = nodelist$stateabb[ind])
}

dev.off()


# plot figure for the appendix

# setup
pestimates$lci = pestimates$pmean - qnorm(0.975) * pestimates$psd
pestimates$uci = pestimates$pmean + qnorm(0.975) * pestimates$psd
slwin = paste(as.character(start:(end - maxlag)), "-", as.character((start + maxlag):end))

tmp = subset(pestimates, coef == "intercept")
p1 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Intercept") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == ".row")
p2 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Sender Effect") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == ".col")
p3 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Receiver Effect") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == "polity2_absdiff.dyad")
p4 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Difference in Polity (abs)") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == "cinc_diff.dyad")
p5 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Difference in CINC \n(Sender-Receiver)") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == "defense.dyad")
p6 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Defense Alliance") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == "com_block.dyad")
p7 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Common Trade Block") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == "com_language.dyad")
p8 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Common Language") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == "log_cdistance.dyad")
p9 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Distance (log)") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

tmp = subset(pestimates, coef == "pathdep.dyad")
p10 = ggplot(tmp, aes(x = year, y = pmean, ymin = lci, ymax = uci)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = expression(theta), title = "Path Dependency") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

patch = (p1 + p2 + p3 + p10 + plot_layout(nrow = 1)) / (p4 + p5 + p6) / (p7 + p8 + p9) +
  plot_annotation(
    caption = "Posterior mean estimates with 95% Credible Intervals.",
    theme = theme(text = element_text(family = "serif", size = 12))
  )

ggsave("results/Z_amen_estimates_plot_gg.pdf", patch, width = 10, height = 0.9 * sqrt(2) * 10, device = cairo_pdf)
