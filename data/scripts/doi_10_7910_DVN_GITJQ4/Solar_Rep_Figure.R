## 
## Chao-Yo Cheng
## ccheng11@ucla.edu
## October 10, 2017
## 
## Solar_Replication_Figure.R
## 
## Replication code for:
## Aklin, Michael, Chao-Yo Cheng, and Johannes Urpelainen. 2017. 
##   "Geography, Community, Household: Adoption of Distributed Solar
##   Power across India." Energy for Sustainable Development.
##   Forthcoming.
## 

rm(list=ls())
setwd("C:/...") # Set local working directory
load("DataverseCensus.RData")

##### Maps #####
library(maps); library(mapdata); library(maptools)
india = readShapePoly("Maps/2011_Dist")
mdta = india@data; head(mdta)
mdta$check = 1
mdta$id = paste(mdta$ST_CEN_CD, mdta$censuscode, sep="-")
india@data = mdta

imap = function(dv){
  vars = c("c11_2011_vill_code", "c11_2011_st_code", "c11_2011_dist_code", dv)
  pdta = dta[,vars]; colnames(pdta) = c("c11_2011_vill_code", "c11_2011_st_code", "c11_2011_dist_code", "dv")
  pdta = pdta[duplicated(pdta) %in% F,]
  
  library(doBy)
  pdta = summaryBy(dv ~ as.factor(c11_2011_st_code) + as.factor(c11_2011_dist_code), data=subset(pdta, !is.na(dv)), FUN=c(mean))
  pdta$id = paste(pdta$c11_2011_st_code, pdta$c11_2011_dist_code, sep="-")
  
  mapall = merge(mdta, pdta, by="id", all=T)
  india@data = mapall
  
  brks = quantile(india@data$dv.mean, na.rm=TRUE)
  brks2 = round(brks, 2)
  lab = c(paste(brks2[1], brks2[2], sep="-"), paste(brks2[2], brks2[3], sep="-"), paste(brks2[3], brks2[4], sep="-"), paste(brks2[4], brks2[5], sep="-"))
  
  cols = rev(heat.colors(length(brks), alpha=1))
  dens = (2:length(brks))*3
  
  plot(india, border="black", col=cols[findInterval(india@data$dv.mean, brks, all.inside=TRUE)])
  legend("topright", legend=lab, bty="n", inset=0.05, lty=c(1,1,1,1,1), lwd=c(5,5,5,5,5), cex=0.6, col=cols)
}

pdf("../Manuscript/Figures/map_solar_dummy.pdf", height=5, width=5)
imap("c11_2011_solar_light_dummy")
dev.off()

pdf("../Manuscript/Figures/map_pop.pdf", height=5, width=5)
imap("c11_2011_total_pop_original")
dev.off()

pdf("../Manuscript/Figures/map_pucca.pdf", height=5, width=5)
imap("c11_2011_pucca_road")
dev.off()

pdf("../Manuscript/Figures/map_bank.pdf", height=5, width=5)
imap("c11_2011_asset_availing_bank")
dev.off()

pdf("../Manuscript/Figures/map_noasset.pdf", height=5, width=5)
imap("c11_2011_asset_none")
dev.off()

pdf("../Manuscript/Figures/map_power01.pdf", height=5, width=5)
imap("c01_2001_power_supl")
dev.off()

##### Scatterplots (Figure 2) #####
scatter = function(dv, dvl){
  vars = c(dv, "c11_2011_solar_light", "c11_2011_total_pop", "c11_2011_dist_code", "c11_2011_st_code", "c11_2011_vill_code")
  subdta = dta[,vars]
  subdta = subdta[duplicated(subdta) %in% F,]
  subdta = na.omit(subdta)
  colnames(subdta) = c("dv", "c11_2011_solar_light", "c11_2011_total_pop", "c11_2011_dist_code", "c11_2011_st_code", "c11_2011_vill_code")
  
  library(doBy)
  elec_mean = summaryBy(dv + c11_2011_solar_light ~ as.factor(c11_2011_vill_code), data=subdta, FUN=c(mean,length))
  head(elec_mean)
  
  lb = min(elec_mean$dv.mean, na.rm=T)
  ub = max(elec_mean$dv.mean, na.rm=T)

  elec_mean$bin = cut(elec_mean$dv.mean, breaks=15, include.lowest=T)
  elec_mean2 = summaryBy(c11_2011_solar_light.mean + c11_2011_solar_light.length ~ bin, data = subset(elec_mean, !is.na(c11_2011_solar_light.mean)), FUN = c(mean,length))
  elec_mean2$binlabs = seq(from=lb+0.0025, to=ub-0.0025, length.out=length(unique(elec_mean$bin)))
  
  library(ggplot2)
  ggplot(elec_mean2, aes(x=binlabs, y=c11_2011_solar_light.mean.mean)) +
    geom_point(aes(size=elec_mean2[,4]), shape=1) +
    xlab(dvl) + ylab("Solar Energy for Lighting") +
    stat_smooth(method="lm", formula=y~x, size=0.6, se=F, colour="red") +
    stat_smooth(method="loess", formula=y~x, size=0.6, se=F, colour="blue") + 
    theme_bw() + theme(legend.position="none", text = element_text(size=15))
}

pdf("../Manuscript/Figures/scatter_elec.pdf", height=5, width=5)
scatter("c11_2011_elec_light", "Use Grid Electricity for Lighting")
dev.off()

pdf("../Manuscript/Figures/scatter_ghi.pdf", height=5, width=5)
scatter("GHI_AnnGLO", "Solar Radiation")
dev.off()

pdf("../Manuscript/Figures/scatter_dist.pdf", height=5, width=5)
scatter("c01_2001_dist_town", "Distance to the Nearest Town (log)")
dev.off()

pdf("../Manuscript/Figures/scatter_pop.pdf", height=5, width=5)
scatter("c11_2011_total_pop", "Total Population (log)")
dev.off()

pdf("../Manuscript/Figures/scatter_bank.pdf", height=5, width=5)
scatter("c11_2011_asset_availing_bank", "Share of HHs with Banking")
dev.off()

pdf("../Manuscript/Figures/scatter_noasset.pdf", height=5, width=5)
scatter("c11_2011_asset_none", "Share of HHs Lacking Assets")
dev.off()

##### Scatterplots (Figure Ag) #####
##### Exclude villages with grid electricity
scatter2 = function(dv, dvl){
  vars = c(dv, "c11_2011_solar_light", "c11_2011_total_pop", "c11_2011_dist_code", "c11_2011_st_code", "c11_2011_vill_code")
  subdta2 = dta2[,vars]
  subdta2 = na.omit(subdta2)
  subdta2 = subdta2[duplicated(subdta2) %in% F,]
  colnames(subdta2) = c("dv", "c11_2011_solar_light", "c11_2011_total_pop", "c11_2011_dist_code", "c11_2011_st_code", "c11_2011_vill_code")
  
  library(doBy)
  elec_mean = summaryBy(dv + c11_2011_solar_light ~ as.factor(c11_2011_vill_code), data=subdta2, FUN=c(mean,length))
  summary(elec_mean)
  
  lb = min(elec_mean$dv.mean, na.rm=T)
  ub = max(elec_mean$dv.mean, na.rm=T)
  
  elec_mean$bin = cut(elec_mean$dv.mean, breaks=15, include.lowest=T)
  elec_mean2 = summaryBy(c11_2011_solar_light.mean + c11_2011_solar_light.length ~ bin, data = subset(elec_mean, !is.na(c11_2011_solar_light.mean)), FUN = c(mean,length))
  elec_mean2$binlabs = seq(from=lb+0.0025, to=ub-0.0025, length.out=length(unique(elec_mean$bin)))
  
  library(ggplot2)
  ggplot(elec_mean2, aes(x=binlabs, y=c11_2011_solar_light.mean.mean)) +
    geom_point(aes(size=c11_2011_solar_light.length.length), shape=1) +
    xlab(dvl) + ylab("Solar Energy for Lighting") +
    stat_smooth(method="lm", formula=y~x, size=0.6, se=F, colour="red") +
    stat_smooth(method="loess", formula=y~x, size=0.6, se=F, colour="blue") + 
    theme_bw() + theme(legend.position="none", text = element_text(size=15))
}

pdf("../Manuscript/Figures/scatter2_ghi.pdf", height=6, width=6)
scatter2("GHI_AnnGLO", "Solar Radiation")
dev.off()

pdf("../Manuscript/Figures/scatter2_dist.pdf", height=6, width=6)
scatter2("c01_2001_dist_town", "Distance to the Nearest Town (log)")
dev.off()

pdf("../Manuscript/Figures/scatter2_pop.pdf", height=6, width=6)
scatter2("c11_2011_total_pop", "Total Population (log)")
dev.off()

pdf("../Manuscript/Figures/scatter2_bank.pdf", height=6, width=6)
scatter2("c11_2011_asset_availing_bank", "Share of HHs with Banking")
dev.off()

pdf("../Manuscript/Figures/scatter2_noasset.pdf", height=6, width=6)
scatter2("c11_2011_asset_none", "Share of HHs Lacking Assets")
dev.off()
