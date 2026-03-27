# ====
# Appendix Figure C.2 Replication
# Counterfactual Estimator Results: New Greenfield FDI and Export Extensive Margins.
# input: extensive_margin_paneldata_Export.csv
# R version 4.4.1 (2024-06-14)
# ====

################################################################################
## setup -----
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(fect)

# set main directory
REPLICATION_DATA_DIR <-  "data/"
REPLICATION_FIG_DIR <- "output/"


################################################################################
## 0. load data  ------
################################################################################
# read data
data.plot.panel.used <- read_csv(file = paste0(REPLICATION_DATA_DIR,
                                               "extensive_margin_paneldata_Export.csv"))


################################################################################
## 1. Figure C.2, panel (a): Dynamic Treatment Effects (FEct)  ------
################################################################################
# fit model
model.fect <- fect(product.len ~ total.fdi_bin
                   + lngdp_WDI_PW_lag
                   + lnpop_WDI_PW_lag
                   + polity2_P4_lag
                   + lnexport_lag,
                   data = data.plot.panel.used,
                   #na.rm = TRUE,
                   method = "fe",
                   index = c("iso3c", "year"),
                   se = TRUE,
                   parallel = TRUE,
                   seed = 1234,
                   nboots = 1000,
                   proportion = 0,
                   force = "two-way")

# summary
print(model.fect)
print(model.fect$est.avg)
fect.output <- as.matrix(model.fect$est.att)
print(fect.output)

# plot
p.fect <- plot(model.fect,
               main = "",
               cex.text = 0.8,
               cex.lab = 1,
               proportion = 0) +
  scale_x_continuous("\nYears relative to treatment") +
  scale_y_continuous("Effect of new greenfield FDI on extensive margin\n",
                     breaks = c(-1000, -500, 0, 500, 1000),
                     labels = c(-1000, -500, 0, 500, 1000),
                     limits = c(-1250, 1250)) +
  theme(panel.grid = element_blank(),
  axis.text.x = element_text(color = "black"),
  axis.text.y = element_text(color = "black"))

p.fect

# save
pdf(file = paste(REPLICATION_FIG_DIR, "Fig_C2_a.pdf", sep = ""),
    width = 6, height = 6)
p.fect
dev.off()


################################################################################
## 2. Figure C.2, panel (b): Testing No Pretrend  ------
################################################################################
# plot
p.pre.trend <- plot(model.fect,
                    type = "equiv",
                    cex.legend = 0.6,
                    legendOff = TRUE,
                    main = "",
                    cex.text = 0.8,
                    proportion = 0,
                    stats.pos = c(-11, 650)) +
  scale_y_continuous("Effect of new greenfield FDI on extensive margin\n",
                     breaks = c(-600, -400, -200, 0, 200, 400, 600),
                     labels = c(-600, -400, -200, 0, 200, 400, 600),
                     limits = c(-650, 650)) +
  scale_x_continuous("\nYears relative to treatment",
                     breaks = c(-10, -5, 0),
                     labels = c(-10, -5, 0),
                     limits = c(-12, 1)) +
  theme(panel.grid = element_blank(),
  axis.text.x = element_text(color = "black"),
  axis.text.y = element_text(color = "black"))

p.pre.trend

# save
pdf(file = paste(REPLICATION_FIG_DIR, "Fig_C2_b.pdf", sep = ""),
    width = 6, height = 6)
p.pre.trend
dev.off()


################################################################################
## 3. Figure C.2, panel (c): Placebo Test  ------
################################################################################
# fit model
model.fect.p <- fect(product.len ~ total.fdi_bin
                     + lngdp_WDI_PW_lag
                     + lnpop_WDI_PW_lag
                     + polity2_P4_lag
                     + lnexport_lag,
                     data = data.plot.panel.used,
                     na.rm = TRUE,
                     index = c("iso3c", "year"),
                     force = "two-way",
                     parallel = TRUE,
                     se = TRUE,
                     CV = 0,
                     seed = 1234,
                     nboots = 1000,
                     placeboTest = TRUE,
                     placebo.period = c(-1, 0))

# summary
print(model.fect.p)
fect.p.output <- as.matrix(model.fect.p$est.att)
print(fect.p.output)

# plot
p.placebo <- plot(model.fect.p,
                  cex.text = 0.8,
                  proportion = 0,
                  stats = c("placebo.p","equiv.p"),
                  main = "",
                  stats.pos = c(-11, 1750)) +
  scale_y_continuous("Effect of new greenfield FDI on extensive margin\n",
                     breaks = c(-1500, -1000, 500, 0, 500, 1000, 1500),
                     labels = c(-1500, -1000, 500, 0, 500, 1000, 1500),
                     limits = c(-1750, 1750)) +
  scale_x_continuous("\nYears relative to treatment",
                     limits = c(-12, 12)) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"))

p.placebo

# save
pdf(file = paste(REPLICATION_FIG_DIR, "Fig_C2_c.pdf", sep = ""),
    width = 6, height = 6)
p.placebo
dev.off()



