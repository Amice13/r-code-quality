library(sf)
library(tidyverse)

load("../data/figure1Arep.rdata")

png("brm_results/figures/Figure1A.png",width = 20, height = 8, units = "cm", res = 2000)
ggplot() + geom_sf(data = pgjoinplot,aes(fill=log(nevents)),color=NA) + geom_sf(data=ocean,fill="white",color=NA) + scale_fill_viridis_c() +
  theme(plot.margin=margin(-0.9,0,-2.2,-1,"cm"),
        legend.title = element_text(angle = 90),
        legend.margin=margin(0,0,0,0,"cm"),
        legend.box.margin=margin(0,0,0,-1,"cm"),
        panel.background = element_rect(fill = 'white'),
        panel.spacing = unit(0, "cm"),
        legend.spacing = unit(0, "cm")) + 
  guides(fill = guide_legend(title.position = "right",
                             title = "Flood frequency (ln)")) 
dev.off()