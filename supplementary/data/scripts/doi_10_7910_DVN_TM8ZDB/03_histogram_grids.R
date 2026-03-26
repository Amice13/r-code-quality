setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../output/bottom_hists")

library(cowplot)
library(gridExtra)
library(ggplot2)


hist_grid = function(f, titletext){ # f is a vec of filenames
  f.2 = tolower(gsub("_.*.rds", "", f))
  
  for(i in 1:length(f)){
    assign(f.2[i], readRDS(f[i]))
  }
  
  az = az + labs(title = "AZ") +
    theme(plot.title = element_text(color = "purple", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    ylab("")
  tmp = ggplot_build(az)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  az = ggplot_gtable(tmp)
  az = ggplotify::as.ggplot(az)
  
  ga = ga + labs(title = "GA") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(ga)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  ga = ggplot_gtable(tmp)
  ga = ggplotify::as.ggplot(ga)
  
  ia = ia + labs(title = "IA") +
    theme(plot.title = element_text(color = "purple", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(ia)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  ia = ggplot_gtable(tmp)
  ia = ggplotify::as.ggplot(ia)
  
  ma = ma + labs(title = "MA") +
    theme(plot.title = element_text(color = "steelblue3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(ma)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  ma = ggplot_gtable(tmp)
  ma = ggplotify::as.ggplot(ma)
  
  md = md + labs(title = "MD") +
    theme(plot.title = element_text(color = "steelblue3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(md)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  md = ggplot_gtable(tmp)
  md = ggplotify::as.ggplot(md)
  
  mn = mn + labs(title = "MN") +
    theme(plot.title = element_text(color = "black", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(mn)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  tmp$data[[3]]$colour = "black"
  tmp$data[[3]]$size = 1.5
  mn = ggplot_gtable(tmp)
  mn = ggplotify::as.ggplot(mn)
  
  nc = nc + labs(title = "NC") +
    theme(plot.title = element_text(color = "black", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(nc)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  tmp$data[[3]]$colour = "black"
  tmp$data[[3]]$size = 1.5
  nc = ggplot_gtable(tmp)
  nc = ggplotify::as.ggplot(nc)
  
  nm = nm+ labs(title = "NM") +
    theme(plot.title = element_text(color = "steelblue3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(nm)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  nm = ggplot_gtable(tmp)
  nm = ggplotify::as.ggplot(nm)
  
  oh = oh + labs(title = "OH") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(oh)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  oh = ggplot_gtable(tmp)
  oh = ggplotify::as.ggplot(oh)
  
  or = or + labs(title = "OR") +
    theme(plot.title = element_text(color = "steelblue3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(or)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  or = ggplot_gtable(tmp)
  or = ggplotify::as.ggplot(or)
  
  pa = pa + labs(title = "PA") +
    theme(plot.title = element_text(color = "black", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(pa)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  tmp$data[[3]]$colour = "black"
  tmp$data[[3]]$size = 1.5
  pa = ggplot_gtable(tmp)
  pa = ggplotify::as.ggplot(pa)
  
  tx = tx + labs(title = "TX") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(tx)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  tx = ggplot_gtable(tmp)
  tx = ggplotify::as.ggplot(tx)
  
  va = va + labs(title = "VA") +
    theme(plot.title = element_text(color = "black", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(va)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  tmp$data[[3]]$colour = "black"
  tmp$data[[3]]$size = 1.5
  va = ggplot_gtable(tmp)
  va = ggplotify::as.ggplot(va)
  
  wi = wi + labs(title = "WI") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(wi)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  wi = ggplot_gtable(tmp)
  wi = ggplotify::as.ggplot(wi)
  
  #plot_grid(az,ga,ia,md,mn,nc,nm,oh,or,pa,tx,va,wi,
  #          nrow=4, ncol=4)
  
  plot_grid(ma,md,nm,or,
            az,ia,NA,NA,
            ga,tx,wi,oh,
            mn,nc,pa,va,NA,
            nrow=4, ncol=4)
  fname = paste0("../../output/plots/", titletext ,"_histograms.png")
  ggsave(fname, width=2000, height=1500, units="px")
  
}

hist_grid_cond = function(f, titletext){ # f is a vec of filenames
  f.2 = tolower(gsub("_.*.rds", "", f))
  
  for(i in 1:length(f)){
    assign(f.2[i], readRDS(f[i]))
  }
  
  az = az + labs(title = "AZ") +
    theme(plot.title = element_text(color = "purple", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    ylab("")
  tmp = ggplot_build(az)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  az = ggplot_gtable(tmp)
  az = ggplotify::as.ggplot(az)
  
  ga = ga + labs(title = "GA") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(ga)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  ga = ggplot_gtable(tmp)
  ga = ggplotify::as.ggplot(ga)
  
  ia = ia + labs(title = "IA") +
    theme(plot.title = element_text(color = "purple", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(ia)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  ia = ggplot_gtable(tmp)
  ia = ggplotify::as.ggplot(ia)
  
  ma = ma + labs(title = "MA") +
    theme(plot.title = element_text(color = "steelblue3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(ma)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  ma = ggplot_gtable(tmp)
  ma = ggplotify::as.ggplot(ma)
  
  md = md + labs(title = "MD") +
    theme(plot.title = element_text(color = "steelblue3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(md)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  md = ggplot_gtable(tmp)
  md = ggplotify::as.ggplot(md)
  
  mn = mn + labs(title = "MN") +
    theme(plot.title = element_text(color = "black", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(mn)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  mn = ggplot_gtable(tmp)
  mn = ggplotify::as.ggplot(mn)
  
  mnvetoed = mnvetoed + labs(title = "MN Vetoed") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(mnvetoed)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  mnvetoed = ggplot_gtable(tmp)
  mnvetoed = ggplotify::as.ggplot(mnvetoed)
  
  nc12 = nc12 + labs(title = "NC12") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(nc12)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  nc12 = ggplot_gtable(tmp)
  nc12 = ggplotify::as.ggplot(nc12)
  
  
  nc16 = nc16 + labs(title = "NC16") +
    theme(plot.title = element_text(color = "black", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(nc16)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  nc16 = ggplot_gtable(tmp)
  nc16 = ggplotify::as.ggplot(nc16)
  
  
  nm = nm+ labs(title = "NM") +
    theme(plot.title = element_text(color = "steelblue3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(nm)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  nm = ggplot_gtable(tmp)
  nm = ggplotify::as.ggplot(nm)
  
  oh = oh + labs(title = "OH") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(oh)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  oh = ggplot_gtable(tmp)
  oh = ggplotify::as.ggplot(oh)
  
  or = or + labs(title = "OR") +
    theme(plot.title = element_text(color = "steelblue3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(or)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  or = ggplot_gtable(tmp)
  or = ggplotify::as.ggplot(or)
  
  pa12 = pa12 + labs(title = "PA12") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(pa12)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  pa12 = ggplot_gtable(tmp)
  pa12 = ggplotify::as.ggplot(pa12)
  
  pa18 = pa18 + labs(title = "PA18") +
    theme(plot.title = element_text(color = "black", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(pa18)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  pa18 = ggplot_gtable(tmp)
  pa18 = ggplotify::as.ggplot(pa18)
  
  tx = tx + labs(title = "TX") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(tx)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  tx = ggplot_gtable(tmp)
  tx = ggplotify::as.ggplot(tx)
  
  va12 = va12 + labs(title = "VA12") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(va12)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  va12 = ggplot_gtable(tmp)
  va12 = ggplotify::as.ggplot(va12)
  
  va16 = va16 + labs(title = "VA16") +
    theme(plot.title = element_text(color = "black", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(va16)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  va16 = ggplot_gtable(tmp)
  va16 = ggplotify::as.ggplot(va16)
  
  wi = wi + labs(title = "WI") +
    theme(plot.title = element_text(color = "firebrick3", size=22),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    ylab("")
  tmp = ggplot_build(wi)
  tmp$data[[1]]$fill = "#8b8680"
  tmp$data[[2]]$colour = "black"
  tmp$data[[2]]$size = 1.5
  wi = ggplot_gtable(tmp)
  wi = ggplotify::as.ggplot(wi)
  
  #plot_grid(az,ga,ia,md,mn,nc,nm,oh,or,pa,tx,va,wi,
  #          nrow=4, ncol=4)
  
  plot_grid(ma,md,nm,or,
            az,ia,NA,NA,
            ga,tx,wi,oh,
            mnvetoed,nc12,pa12,va12,
            mn,nc16,pa18,va16,NA,
            nrow=5, ncol=4)
 
  fname = paste0("../../output/plots/", titletext ,"_conditional_histograms.png")
  ggsave(fname, width=2000, height=1500, units="px")
  
}


#### Firms marginal
f = list.files()
f = f[grepl("firms.rds", f)]

hist_grid(f, "firms")

###### Firms conditional
f = list.files()
f = f[grepl("firms_cond", f)]

hist_grid_cond(f, "firms")


#### Rich voter plots, marginal ####

f = list.files()
f = f[grepl("topincome.rds", f)]

hist_grid(f, "rich")

#### Rich voter plots, conditional ####

f = list.files()
f = f[grepl("topincome_cond.rds", f)]

hist_grid(f, "rich")


#### Donors, marginal ####

f = list.files()
f = f[grepl("donors200k.rds", f)]

hist_grid(f, "donors")

#### Donors, conditional ####

f = list.files()
f = f[grepl("donors200k_cond.rds", f)]

hist_grid(f, "donors")

