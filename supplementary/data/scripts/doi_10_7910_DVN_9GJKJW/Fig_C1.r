# ====
# Figure C1 Replication:
# Extensive lineplots by economy size
# input: extensive_margin_lineplot_Export.csv, extensive_margin_lineplot_Import_all.csv
# ====

################################################################################
## setup -----
################################################################################
# clean slate
rm(list = ls())
date()

library(tidyverse)

# set main directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"

################################################################################

for (VERSION in c("Export", "Import_all")){
  
  # load data 
  dat.plot <- read_csv(file = paste0(REPLICATION_DATA_DIR,
                                     "extensive_margin_lineplot_", VERSION, ".csv"))

dat.plot %>% glimpse()

  # average yearly extensive margin for each total fdi tertile group
  dat.plot.byfdigroup <- dat.plot %>%
    group_by(fdi.tertile.group, year) %>%
    summarise(product.len.average = mean(product.len, na.rm = T)) %>%
    ungroup()
 
  dat.plot.byfdigroup %>% glimpse()

  # plot setting
  axis.title.size <- 22

  if (VERSION == "Export"){
    YLAB_TITLE <- paste("Extensive margin in exports")
    YLIM_MIN <- 1450
    YLIM_MAX <- 4500
    YLAB_BREAKS <- seq(1000, 5000, 1000)
    LABEL_Yax <- c(1950, 3150, 4300)
    
  }else if (VERSION == "Import_all"){
    YLAB_TITLE <- paste("Extensive margin in imports (all)")
    YLIM_MIN <- 3000
    YLIM_MAX <- 4800
    YLAB_BREAKS <- seq(1000, 5000, 500)
    LABEL_Yax <- c(3800, 4300, 4600)
  }

  GROUPING <- "fdi.tertile.group"
  GROUP_label <- c("Low FDI", "Medium FDI", "High FDI")

  lineplot.fdi.extensive <- dat.plot.byfdigroup %>%
    ggplot(aes(year, product.len.average, group = fdi.tertile.group)) +
    # geom_line(aes_string(color = GROUPING,
                        #  lty = GROUPING)) +
    geom_line(aes(color = fdi.tertile.group,
                  linetype = fdi.tertile.group)) +
    theme_bw() +
    scale_y_continuous(YLAB_TITLE,
                       breaks = YLAB_BREAKS,
                       labels = YLAB_BREAKS,
                       limits = c(YLIM_MIN, YLIM_MAX)) +
    scale_color_manual(values = c("66-100" = "red",
                                  "33-66" = "blue",
                                  "0-33" = "black")) +
    ggplot2:: annotate("text", x = c(2015, 2015, 2015), y = LABEL_Yax,
                       label = c("Low FDI", "Medium FDI", "High FDI"),
                       size = 8) +
    theme(
      legend.position = "none",
      legend.text = element_text(size = axis.title.size),
      plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm"),
      axis.title.y = element_text(size = axis.title.size,
                                  margin = margin(0, 20, 0, 0)),
      axis.title.x = element_text(size = axis.title.size,
                                  margin = margin(20, 0, 0, 0)),
      axis.text.y = element_text(size = axis.title.size + 2, color = "black"),
      axis.text.x = element_text(size = axis.title.size + 2, color = "black"),
      strip.background = element_blank(),
      strip.text = element_text(size = axis.title.size + 2,
                                margin = margin(20, 0, 20, 0)),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 1.25)
    )

  lineplot.fdi.extensive

  ggsave(lineplot.fdi.extensive,
    file = paste0(REPLICATION_FIG_DIR, "FC1_extensive_margin_lineplot_", VERSION, ".pdf"), width = 7.5, height = 6.5)
}
