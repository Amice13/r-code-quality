################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-09
# Purpose: Combine authoritarianism plots for Figure G in the supporting info
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/groups_and_scales.csv
# NOTES: MUST FIRST RUN:
#                       1. 'Analysis\Authoritarianism\auth1-run_models.R'
#                       2. 'Analysis\Authoritarianism\auth2-coef_plot.R'
#                       3. 'Analysis\Authoritarianism\auth3-person_locations.R'
#                       4. 'Analysis\Authoritarianism\auth4-item_locationst.R'
#
# Questions: mpietryka@fsu.edu
################################################################################


width  <- 8
height <- 8
dev.off()
graphics.off()
windows(width, height)

auth_plot <- ggpubr::ggarrange(
  ggpubr::ggarrange(coef_plot, loc_plot,
          ncol = 2,
          align = "h",
          labels = c("A", "B"),
          widths = c(1, 1)),
  item_plot,
  nrow = 2,
  labels = c("", "C"),
  heights = c(1, 1),
  align = "hv"
)

auth_plot

pdf("auth_plot.pdf", width, height)
auth_plot
dev.off()

tiff("auth_plot.tiff",
     width,
     height,
     res = 600,
     compression = "lzw",
     units="in")
auth_plot
dev.off()



