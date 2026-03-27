#######################################################################
# Copyright (C) 2018  George Githinji

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#######################################################################


library(tidyverse)
library(reshape2)
library(patchwork)

plot.concordance <- function (url,dataset,chart_label){
  dta <- read.csv(url,header = T,sep = ",")
  dta.m <- melt(dta,id.vars = c("position","concordance_level","type"))
  
  plot <-  ggplot(dta.m,aes(type,reorder(factor(position),-concordance_level))) +
    geom_tile(aes(fill=factor(concordance_level)), colour="white") +
    scale_fill_brewer(palette="Oranges",name="concordance accuracy") +
    labs(title="",
         subtitle = chart_label,
      x = "",
      y = "",
      caption=dataset) +
    theme_bw() +
    theme(axis.text.x = element_text(
      hjust = 0.95,
      size = 12,
      face = "bold",
      vjust=0.2),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
  return(plot)
}
 
#TODO 
# 1. Refactor to read from all_combined.csv
url1 <- "data/artificial_data/combined_sample1.csv"
url2 <- "data/artificial_data/combined_sample2.csv"
url3 <- "data/artificial_data/combined_sample3.csv"
url4 <- "data/artificial_data/combined_sample4.csv"
url5 <- "data/artificial_data/combined_sample5.csv"
url6 <- "data/artificial_data/combined_sample6.csv"
url7 <- "data/artificial_data/combined_sample7.csv"
url8 <- "data/artificial_data/combined_sample8.csv"


plot1 <- plot.concordance(url1,"X20","A")
plot2 <- plot.concordance(url2,"X50","B")
plot3 <- plot.concordance(url3,"X100","C")
plot4 <- plot.concordance(url4,"X500","D")
plot5 <- plot.concordance(url5,"X1,000","E")
plot6 <- plot.concordance(url6,"X2,000","F")
plot7 <- plot.concordance(url7,"X5,000","G")
plot8 <- plot.concordance(url8,"X10,000","H")

concordancy_accuracy_plot <-  plot1 + theme(legend.position = "none") +
  plot2 + theme(legend.position = "none") +
  plot3 + theme(legend.position = "none") +
  plot4 + theme(legend.position = "none") +
  plot5 + theme(legend.position = "none") +
  plot6 + theme(legend.position = "none") +
  plot7 + theme(legend.position = "none") +
  plot8  +
  plot_layout(nrow = 1)

pdf(file = "figures/figure2.pdf",height = 10,width = 10,onefile = FALSE)
  concordancy_accuracy_plot 
dev.off()
