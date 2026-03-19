#------------------------------------------------------------------------------#
# Plot of estimated coefficients from main model specification.
#------------------------------------------------------------------------------#

library(network)
library(ergm)
library(btergm)
library(ggplot2)

library(patchwork)
library(gridExtra)
library(grid)
library(ggpubr)

library(scales)
library(dplyr)
library(coda)
library(tidyr)

library(kableExtra)
library(forcats)

# load results from main specification
load(file = "models/A_main_workspace.rda")


# extract estimates and add confidence intervals
head(coefs)
coefs$lci = coefs$estimate - qnorm(0.975) * coefs$se
coefs$uci = coefs$estimate + qnorm(0.975) * coefs$se

names = unique(coefs$name)


# define custom ggplot theme
custom_theme = theme_classic() +
  theme(
    text = element_text(family = "serif", size = 10)
    , axis.line.y = element_blank()
    , axis.ticks.y = element_blank()
    , strip.background = element_blank()
    , strip.text = element_text(hjust = 0)
    , plot.title = element_text(hjust = 0)
    , plot.subtitle = element_text(hjust = 0)
    , plot.margin = margin(0, 4, 0, 0, unit = "mm")
    , panel.spacing = unit(5, "mm")
  )


# axis ticks
slwin = paste(as.character(1921:1933), "-", as.character(1924:1936))
slwin2 = paste(as.character(1921:1934), "-", as.character(1923:1936))



#------------------------------------------------------------------------------#
# Figure Main Model
#------------------------------------------------------------------------------#

tmp_names = c(
  "mutual",
  "gwesp.OTP.fixed.1",
  "gwesp.OSP.fixed.1",
  "gwesp.ISP.fixed.1",
  "gwodeg.fixed.0.1",
  "gwideg.fixed.0.1",
  "nodeocov.gdppc",
  "nodeicov.gdppc",
  "absdiff.polity2",
  "diff.tail-head.cinc",
  "edgecov.defense",
  "edgecov.com_block"
)

coef_name = c(
  "Reciprocity" = "mutual",
  "Outgoing Two-path\n(Geometrically weighted)" = "gwesp.OTP.fixed.1",
  "Outgoing Shared Partner\n(Geometrically weighted)" = "gwesp.OSP.fixed.1",
  "Incoming Shared Partner\n(Geometrically weighted)" = "gwesp.ISP.fixed.1",
  "Out-degree\n(Geometrically weighted)" = "gwodeg.fixed.0.1",
  "In-degree\n(Geometrically weighted)" = "gwideg.fixed.0.1",
  "GDP per capita\nSender (log)" = "nodeocov.gdppc",
  "GDP per capita\nReceiver (log)" = "nodeicov.gdppc",
  "Difference in Polity\n(abs)" = "absdiff.polity2",
  "Difference in CINC\n(Sender - Receiver)" = "diff.tail-head.cinc",
  "Defence Alliance" = "edgecov.defense",
  "Common Trade Block" = "edgecov.com_block"
)

df = coefs
df = filter(df, name %in% tmp_names)
df = mutate(df, name = fct_relevel(as_factor(name), tmp_names))
df = mutate(df, name = fct_recode(name, !!!coef_name))

pl = ggplot(data = df, mapping = aes(x = year, y = estimate)) +
  geom_point(size = 1) +
  geom_linerange(mapping = aes(ymin = lci, ymax = uci), alpha = 0.6) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~name, scales = "free_x") +
  coord_flip() +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = "", y = expression(theta)) +
  custom_theme

ggsave(
  filename = file.path("figures", "fig_main_results_selection.pdf")
  , plot = pl
  , height = 220
  , width = 190
  , units = "mm"
)



#------------------------------------------------------------------------------#
# Figure main model with all covariates
#------------------------------------------------------------------------------#

tmp_names = c(
  "mutual",
  "gwesp.OTP.fixed.1",
  "gwesp.OSP.fixed.1",
  "gwesp.ISP.fixed.1",
  "gwodeg.fixed.0.1",
  "gwideg.fixed.0.1",
  "nodeocov.gdppc",
  "nodeicov.gdppc",
  "absdiff.polity2",
  "diff.tail-head.cinc",
  "edgecov.defense",
  "edgecov.com_block",
  "edgecov.com_language",
  "edgecov.log_cdistance",
  "edgecov.pathdep"
)

coef_name = c(
  "Reciprocity" = "mutual",
  "Outgoing Two-path\n(Geometrically weighted)" = "gwesp.OTP.fixed.1",
  "Outgoing Shared Partner\n(Geometrically weighted)" = "gwesp.OSP.fixed.1",
  "Incoming Shared Partner\n(Geometrically weighted)" = "gwesp.ISP.fixed.1",
  "Out-degree\n(Geometrically weighted)" = "gwodeg.fixed.0.1",
  "In-degree\n(Geometrically weighted)" = "gwideg.fixed.0.1",
  "GDP per capita\nSender (log)" = "nodeocov.gdppc",
  "GDP per capita\nReceiver (log)" = "nodeicov.gdppc",
  "Difference in Polity\n(abs)" = "absdiff.polity2",
  "Difference in CINC\n(Sender - Receiver)" = "diff.tail-head.cinc",
  "Defence Alliance" = "edgecov.defense",
  "Common Trade Block" = "edgecov.com_block",
  "Common Language" = "edgecov.com_language",
  "Distance (log)" = "edgecov.log_cdistance",
  "Path Dependency" = "edgecov.pathdep"
)

df = coefs
df = filter(df, name %in% tmp_names)
df = mutate(df, name = fct_relevel(as_factor(name), tmp_names))
df = mutate(df, name = fct_recode(name, !!!coef_name))

pl = ggplot(data = df, mapping = aes(x = year, y = estimate)) +
  geom_point(size = 1) +
  geom_linerange(mapping = aes(ymin = lci, ymax = uci), alpha = 0.6) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~name, scales = "free_x") +
  coord_flip() +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin, trans = "reverse") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = "", y = expression(theta)) +
  custom_theme

ggsave(
  filename = file.path("figures", "fig_main_results_complete.pdf")
  , plot = pl
  , height = 270
  , width = 190
  , units = "mm"
)


#------------------------------------------------------------------------------#
# Figure Average Marginal Effects
#------------------------------------------------------------------------------#

#
avg_marginaleff = lapply(avg_marginaleff, function(df) {
  df$var = rownames(df)
  df
})


avg_marginaleff = do.call(rbind, avg_marginaleff)
colnames(avg_marginaleff) = c("ame", "se", "z", "p", "year", "var")

avg_marginaleff = mutate(avg_marginaleff, var = ifelse(var == "diff.tail-head.cinc", "Difference in CINC\n(Sender - Receiver)", var))
avg_marginaleff = mutate(avg_marginaleff, var = ifelse(var == "edgecov.defense", "Defence Alliance", var))

pl = ggplot(avg_marginaleff, aes(x = year, y = ame)) +
  geom_errorbar(aes(ymin = ame - qnorm(0.975) * se, ymax = ame + qnorm(0.975) * se), width = 0.2) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
  facet_wrap(facets = ~ var, scales = "free_y", ncol = 1) +
  ylab(label = "Average marginal effect") + xlab(label = "Window") +
  scale_x_continuous(breaks = c(1924:1936), labels = paste(as.character(1921:1933), "-", as.character(24:36))) +
  scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
  custom_theme +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path("figures", "fig_main_results_ame.pdf")
  , plot = pl
  , height = 90
  , width = 190
  , units = "mm"
)



#------------------------------------------------------------------------------#
# Figure goodness of fit statistics
#------------------------------------------------------------------------------#

# only plot every second year (for visibility)
pdf(file = "figures/fig_main_gof_statistics.pdf", width = 10, height = 10 * sqrt(2))
par(mfrow = c(6, 3), family = "serif")
for (t in c(2, 4, 6, 8, 10, 12)) {
  tmp = simulations[[t]]
  plot(tmp$`Indegree`, main = "")
  plot(tmp$`Geodesic distances`, main = paste("Year", slwin[t]), ylab = "")
  plot(tmp$`Edge-wise shared partners`, main = "", ylab = "")
}
dev.off()



#------------------------------------------------------------------------------#
# Figure MCMC Diagnostics
#------------------------------------------------------------------------------#

# Does ERGM indicate failure of convergence? - All FALSE.
for (t in seq_along(results)) {
  tmp = results[[t]]
  print(tmp@ergm$failure)
}

# Output Trace Plots
pdf(file = "figures/fig_mcmc_diagnostics.pdf", width = 19 / 2.54, height = 27 / 2.54)
par(mfrow = c(4, 2))
for (t in seq_along(results)) {
  tmp = results[[t]]
  plot(tmp@ergm$sample[, 1:16], smooth = TRUE, auto.layout = FALSE)
  # plot(checkdegeneracy(tmp, which = "plots", vars.per.page = 8))
}
dev.off()



#------------------------------------------------------------------------------#
# Robustness Checks Plots for the Appendix
#------------------------------------------------------------------------------#

# amen model
env_amen = new.env()
load(file = "models/Z_amen_estimates.rda", envir = env_amen)

df_amen = env_amen$pestimates
df_amen = rename(df_amen, name = coef, estimate = pmean, se = psd)

df_amen$model = "amen"
df_amen$failure = NA

df_amen$lci = df_amen$estimate - qnorm(0.975) * df_amen$se
df_amen$uci = df_amen$estimate + qnorm(0.975) * df_amen$se

rm(env_amen)


# backbone network - disparity
df_backbone = readRDS(file = "models/C_estimates_robustness_backbone.rds")


# bind all to one data.frame
df = readRDS(file = "models/C_estimates_robustness.rds")
df = rbind(df, df_amen, df_backbone)

rm(df_amen, df_backbone)


# prepare correct naming scheme amen vs ergm
df$name[df$name == "intercept"] = "edges"
df$name[df$name == ".row"] = "nodeocov.gdppc"
df$name[df$name == ".col"] = "nodeicov.gdppc"
df$name[df$name == "polity2_absdiff.dyad"] = "absdiff.polity2"
df$name[df$name == "cinc_diff.dyad"] = "diff.tail-head.cinc"
df$name[df$name == "defense.dyad"] = "edgecov.defense"
df$name[df$name == "com_block.dyad"] = "edgecov.com_block"
df$name[df$name == "com_language.dyad"] = "edgecov.com_language"
df$name[df$name == "log_cdistance.dyad"] = "edgecov.log_cdistance"
df$name[df$name == "pathdep.dyad"] = "edgecov.pathdep"


# align both decay valued network stats such that these are shown in one row
df$name[df$name == "gwesp.OTP.fixed.1"] = "gwesp.OTP.fixed"
df$name[df$name == "gwesp.OSP.fixed.1"] = "gwesp.OSP.fixed"
df$name[df$name == "gwesp.ISP.fixed.1"] = "gwesp.ISP.fixed"
df$name[df$name == "gwodeg.fixed.0.1"] = "gwodeg.fixed"
df$name[df$name == "gwideg.fixed.0.1"] = "gwideg.fixed"

df$name[df$name == "gwesp.OTP.fixed.1.5"] = "gwesp.OTP.fixed"
df$name[df$name == "gwesp.OSP.fixed.1.5"] = "gwesp.OSP.fixed"
df$name[df$name == "gwesp.ISP.fixed.1.5"] = "gwesp.ISP.fixed"
df$name[df$name == "gwodeg.fixed.0.5"] = "gwodeg.fixed"
df$name[df$name == "gwideg.fixed.0.5"] = "gwideg.fixed"

df$name[df$name == "gwodeg.fixed.1"] = "gwodeg.fixed"
df$name[df$name == "gwideg.fixed.1"] = "gwideg.fixed"

df = select(df, estimate, se, year, name, lci, uci, model)



#------------------------------------------------------------------------------#
# Network Sparsification
#------------------------------------------------------------------------------#

order_of_models = c("50", "150", "backbone")
order_of_vars = c(
  "edges",
  "mutual",
  "gwesp.OTP.fixed",
  "gwesp.OSP.fixed",
  "gwesp.ISP.fixed",
  "gwodeg.fixed",

  "gwideg.fixed",
  "nodeocov.gdppc",
  "nodeicov.gdppc",
  "absdiff.polity2",
  "diff.tail-head.cinc",
  "edgecov.defense",

  "edgecov.com_block",
  "edgecov.com_language",
  "edgecov.log_cdistance",
  "edgecov.pathdep"
)

plist = list()
i = 0

for (v in order_of_vars) {
  for (m in order_of_models) {
    i = i + 1

    tmp = subset(df, model == m & name == v)

    if (nrow(tmp) == 0) {
      plist[[i]] = plot_spacer()
      next
    }

    ylab = case_when(
      v == "edges" ~ "Edges",
      v == "mutual" ~ "Reciprocity",
      v == "gwodeg.fixed" ~ "Out-degree",
      v == "gwideg.fixed" ~ "In-degree",
      v == "gwesp.OTP.fixed" ~ "Outgoing Two-path",
      v == "gwesp.OSP.fixed" ~ "Outgoing Shared Partner",
      v == "gwesp.ISP.fixed" ~ "Incoming Shared Partner",
      v == "nodeocov.gdppc" ~ "GDP per capita (log)\nSender",
      v == "nodeicov.gdppc" ~ "GDP per capita (log)\nReceiver",
      v == "nodeocov.cinc" ~ "CINC\nSender",
      v == "nodeicov.cinc" ~ "CINC\nReceiver",
      v == "absdiff.polity2" ~ "Difference in Polity\n(abs)",
      v == "diff.tail-head.cinc" ~ "Difference in CINC\n(Sender - Receiver)",
      v == "diff.tail-head.milexp" ~ "Difference in MilExp (log)\n(Sender - Receiver)",
      v == "edgecov.defense" ~ "Defence Alliance",
      v == "edgecov.interact" ~ "CINC x Defence Interaction",
      v == "edgecov.com_block" ~ "Common Trade Block",
      v == "edgecov.com_language" ~ "Common Language",
      v == "edgecov.log_cdistance" ~ "Distance (log)",
      v == "edgecov.pathdep" ~ "Path Dependency"
    )

    title = case_when(
      m == "50" ~ "Threshold of 50 USD",
      m == "150" ~ "Threshold of 150 USD",
      m == "backbone" ~ "Disparity filter (alpha=0.5)"
    )

    x_labels = slwin
    x_breaks = c(1924:1936)
    x_limits = c(1924, 1936)

    plist[[i]] = ggplot(data = tmp, aes(x = year, y = estimate, ymin = lci, ymax = uci)) +
      geom_point(size = 1.5) +
      geom_linerange(alpha = 0.6) +
      coord_flip() +
      labs(x = ylab, y = expression(theta), subtitle = title) +
      geom_hline(yintercept = 0, lty = 2) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, trans = "reverse") +
      scale_y_continuous(labels = label_number(accuracy = 0.01)) +
      custom_theme

    if (!(v %in% c("edges", "gwideg.fixed", "edgecov.com_block"))) {
      plist[[i]] = plist[[i]] + theme(plot.title = element_blank(), plot.subtitle = element_blank())
    }
  }
}



ggsave(
  filename = file.path("figures", "fig_robustness_page1.pdf"),
  plot = Reduce("+", plist[1:18]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)

ggsave(
  filename = file.path("figures", "fig_robustness_page2.pdf"),
  plot = Reduce("+", plist[19:36]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)

ggsave(
  filename = file.path("figures", "fig_robustness_page3.pdf"),
  plot = Reduce("+", plist[37:60]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)





#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#

order_of_models = c("default", "amen", "window")
order_of_vars = c(
  "edges",
  "mutual",
  "gwesp.OTP.fixed",
  "gwesp.OSP.fixed",
  "gwesp.ISP.fixed",
  "gwodeg.fixed",

  "gwideg.fixed",
  "nodeocov.gdppc",
  "nodeicov.gdppc",
  "absdiff.polity2",
  "diff.tail-head.cinc",
  "edgecov.defense",

  "edgecov.com_block",
  "edgecov.com_language",
  "edgecov.log_cdistance",
  "edgecov.pathdep"
)

order_of_vars = c(
  "edges",
  "mutual",
  "gwesp.OTP.fixed",
  "gwesp.OSP.fixed",
  "gwesp.ISP.fixed",
  "gwodeg.fixed",

  "gwideg.fixed",
  "nodeocov.gdppc",
  "nodeicov.gdppc",
  "absdiff.polity2",
  "diff.tail-head.cinc",
  "edgecov.defense",

  "edgecov.com_block",
  "edgecov.com_language",
  "edgecov.log_cdistance",
  "edgecov.pathdep"
)

plist = list()
i = 0

for (v in order_of_vars) {
  for (m in order_of_models) {
    i = i + 1

    tmp = subset(df, model == m & name == v)

    if (nrow(tmp) == 0) {
      plist[[i]] = plot_spacer()
      next
    }

    ylab = case_when(
      v == "edges" ~ "Edges",
      v == "mutual" ~ "Reciprocity",
      v == "gwodeg.fixed" ~ "Out-degree",
      v == "gwideg.fixed" ~ "In-degree",
      v == "gwesp.OTP.fixed" ~ "Outgoing Two-path",
      v == "gwesp.OSP.fixed" ~ "Outgoing Shared Partner",
      v == "gwesp.ISP.fixed" ~ "Incoming Shared Partner",
      v == "nodeocov.gdppc" ~ "GDP per capita (log)\nSender",
      v == "nodeicov.gdppc" ~ "GDP per capita (log)\nReceiver",
      v == "nodeocov.cinc" ~ "CINC\nSender",
      v == "nodeicov.cinc" ~ "CINC\nReceiver",
      v == "absdiff.polity2" ~ "Difference in Polity\n(abs)",
      v == "diff.tail-head.cinc" ~ "Difference in CINC\n(Sender - Receiver)",
      v == "diff.tail-head.milexp" ~ "Difference in MilExp (log)\n(Sender - Receiver)",
      v == "edgecov.defense" ~ "Defence Alliance",
      v == "edgecov.interact" ~ "CINC x Defence Interaction",
      v == "edgecov.com_block" ~ "Common Trade Block",
      v == "edgecov.com_language" ~ "Common Language",
      v == "edgecov.log_cdistance" ~ "Distance (log)",
      v == "edgecov.pathdep" ~ "Path Dependency"
    )

    if (m == "amen" && ylab == "Edges") ylab = "Intercept"

    title = case_when(
      m == "default" ~ "Alternative Decay\nSpecification",
      m == "amen" ~ "AME Model",
      m == "window" ~ "Window Length (T=3)"
    )

    x_labels = slwin
    x_breaks = c(1924:1936)
    x_limits = c(1924, 1936)

    if (m == "window") {
      x_labels = slwin2
      x_breaks = c(1923:1936)
      x_limits = c(1923, 1936)
    }

    plist[[i]] = ggplot(data = tmp, aes(x = year, y = estimate, ymin = lci, ymax = uci)) +
      geom_point(size = 1.5) +
      geom_linerange(alpha = 0.6) +
      coord_flip() +
      labs(x = ylab, y = expression(theta), subtitle = title) +
      geom_hline(yintercept = 0, lty = 2) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, trans = "reverse") +
      scale_y_continuous(labels = label_number(accuracy = 0.01)) +
      custom_theme

    if (!(v %in% c("edges", "Intercept", "gwideg.fixed", "edgecov.com_block"))) {
      plist[[i]] = plist[[i]] + theme(plot.title = element_blank(), plot.subtitle = element_blank())
    }
  }
}



ggsave(
  filename = file.path("figures", "fig_robustness_page4.pdf"),
  plot = Reduce("+", plist[1:18]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)

ggsave(
  filename = file.path("figures", "fig_robustness_page5.pdf"),
  plot = Reduce("+", plist[19:36]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)

ggsave(
  filename = file.path("figures", "fig_robustness_page6.pdf"),
  plot = Reduce("+", plist[37:60]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)






#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#

order_of_models = c("cinc_nodal", "cinc_interaction", "cinc_milexp")
order_of_vars = c(
  "edges",
  "mutual",
  "gwesp.OTP.fixed",
  "gwesp.OSP.fixed",
  "gwesp.ISP.fixed",
  "gwodeg.fixed",

  "gwideg.fixed",
  "nodeocov.gdppc",
  "nodeicov.gdppc",
  "nodeocov.cinc",
  "nodeicov.cinc",
  "absdiff.polity2",

  "diff.tail-head.cinc",
  "diff.tail-head.milexp",
  "edgecov.defense",
  "edgecov.interact",
  "edgecov.com_block",
  "edgecov.com_language",

  "edgecov.log_cdistance",
  "edgecov.pathdep"
)


plist = list()
i = 0

for (v in order_of_vars) {
  for (m in order_of_models) {
    i = i + 1

    tmp = subset(df, model == m & name == v)

    if (nrow(tmp) == 0) {
      plist[[i]] = plot_spacer()
      next
    }

    ylab = case_when(
      v == "edges" ~ "Edges",
      v == "mutual" ~ "Reciprocity",
      v == "gwodeg.fixed" ~ "Out-degree",
      v == "gwideg.fixed" ~ "In-degree",
      v == "gwesp.OTP.fixed" ~ "Outgoing Two-path",
      v == "gwesp.OSP.fixed" ~ "Outgoing Shared Partner",
      v == "gwesp.ISP.fixed" ~ "Incoming Shared Partner",
      v == "nodeocov.gdppc" ~ "GDP per capita (log)\nSender",
      v == "nodeicov.gdppc" ~ "GDP per capita (log)\nReceiver",
      v == "nodeocov.cinc" ~ "CINC\nSender",
      v == "nodeicov.cinc" ~ "CINC\nReceiver",
      v == "absdiff.polity2" ~ "Difference in Polity\n(abs)",
      v == "diff.tail-head.cinc" ~ "Difference in CINC\n(Sender - Receiver)",
      v == "diff.tail-head.milexp" ~ "Difference in MilExp (log)\n(Sender - Receiver)",
      v == "edgecov.defense" ~ "Defence Alliance",
      v == "edgecov.interact" ~ "CINC x Defence Interaction",
      v == "edgecov.com_block" ~ "Common Trade Block",
      v == "edgecov.com_language" ~ "Common Language",
      v == "edgecov.log_cdistance" ~ "Distance (log)",
      v == "edgecov.pathdep" ~ "Path Dependency"
    )

    if (m == "amen" && ylab == "Edges") ylab = "Intercept"

    title = case_when(
      m == "cinc_nodal" ~ "CINC introduced as\nNodal Covariate",
      m == "cinc_interaction" ~ "Adding CINC-Defence\nInteraction",
      m == "cinc_milexp" ~ "Replacement of CINC\nby Military Expenditure (log)"
    )

    x_labels = slwin
    x_breaks = c(1924:1936)
    x_limits = c(1924, 1936)

    plist[[i]] = ggplot(data = tmp, aes(x = year, y = estimate, ymin = lci, ymax = uci)) +
      geom_point(size = 1.5) +
      geom_linerange(alpha = 0.6) +
      coord_flip() +
      labs(x = ylab, y = expression(theta), subtitle = title) +
      geom_hline(yintercept = 0, lty = 2) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, trans = "reverse") +
      scale_y_continuous(labels = label_number(accuracy = 0.01)) +
      custom_theme

    if (!(v %in% c("edges", "gwideg.fixed", "diff.tail-head.cinc", "edgecov.log_cdistance"))) {
      plist[[i]] = plist[[i]] + theme(plot.title = element_blank(), plot.subtitle = element_blank())
    }
  }
}



ggsave(
  filename = file.path("figures", "fig_robustness_page7.pdf"),
  plot = Reduce("+", plist[1:18]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)

ggsave(
  filename = file.path("figures", "fig_robustness_page8.pdf"),
  plot = Reduce("+", plist[19:36]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)

ggsave(
  filename = file.path("figures", "fig_robustness_page9.pdf"),
  plot = Reduce("+", plist[37:54]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)

ggsave(
  filename = file.path("figures", "fig_robustness_page10.pdf"),
  plot = Reduce("+", plist[55:72]) + plot_layout(ncol = 3, nrow = 6),
  height = 270, width = 190, units = "mm"
)




#------------------------------------------------------------------------------#
# Main vs Commercial Model Comparison
#------------------------------------------------------------------------------#

rm(list = ls())

load(file = "models/A_main_workspace.rda")
load(file = "models/A_commercial_results.rda")

# axis ticks
slwin = paste(as.character(1921:1933), "-", as.character(1924:1936))

df1 = data.frame(
  start = 1924:1936,
  Full_AIC = sapply(results, function(x) x@aic[[1]]),
  Full_BIC = sapply(results, function(x) x@bic[[1]]),
  Commercial_AIC = sapply(results_commercial, function(x) x@aic[[1]]),
  Commercial_BIC = sapply(results_commercial, function(x) x@bic[[1]])
)
df1 = mutate(df1, diff_AIC = Commercial_AIC - Full_AIC, diff_BIC = Commercial_BIC - Full_BIC)
df1 = pivot_longer(df1, cols = 2:7, names_to = "id")
df1 = separate(data = df1, col = id, into = c("Model", "Metric"), sep = "\\_")

pl1 = ggplot(data = subset(df1, Model == "diff"), mapping = aes(x = start, y = value, group = Metric)) +
  geom_line(mapping = aes(linetype = Metric), show.legend = TRUE) +
  geom_hline(yintercept = 0, color = "grey") +
  scale_color_manual(name = "Metrics", values = c("AIC", "BIC")) +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin) +
  xlab(label = "") + ylab("Difference") +
  labs(title = "Model Comparison: AIC / BIC", subtitle = "Difference: Commercial - Complete") +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


df2 = data.frame(
  start = 1924:1936
  , Full_aucpr = sapply(simulations, FUN = function(x) x$`Tie prediction`$auc.pr)
  , Commercial_aucpr = sapply(simulations_commercial, FUN = function(x) x$`Tie prediction`$auc.pr)
)
df2 = mutate(df2, diff_aucpr = Full_aucpr - Commercial_aucpr)
df2 = pivot_longer(df2, cols = 2:4, names_to = "id")
df2 = separate(data = df2, col = id, into = c("Model", "Metric"), sep = "\\_")

pl2 = ggplot(data = subset(df2, Model == "diff"), mapping = aes(x = start, y = value)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "grey") +
  scale_x_continuous(breaks = c(1924:1936), labels = slwin) +
  xlab(label = "") + ylab("Difference") +
  labs(title = "Model Comparison: AUC PR", subtitle = "Difference: Complete - Commercial ") +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# save
# patch = pl1 + pl2
# ggsave(
#   filename = "figures/fig_appendix_model_comparison.pdf"
#   , plot = patch
#   , height = 190 / 2
#   , width = 190
#   , units = "mm"
# )

ggsave(
  filename = "figures/fig_appendix_model_comparison.pdf"
  , plot = pl1
  , height = 190 / 2
  , width = 190
  , units = "mm"
)


# comparison table
df = rbind(df1, df2)
df = aggregate(value ~ Model + Metric, FUN = mean, data = df)
df = filter(df, Model != "diff")
df = pivot_wider(data = df, names_from = "Metric", values_from = "value")

sink("tables/latex_model_performance.txt")
kbl(
  x = df[, c(1, 2, 4, 3)],
  booktabs = T,
  format = "latex",
  digits = 2,
  escape = F,
  linesep = "",
  caption = "Average in-sample model performance measures of the full model and a commercial model that drops the OSP, Polity, CINC, and Defence Alliance terms",
  label = "model_performance"
)
sink()
