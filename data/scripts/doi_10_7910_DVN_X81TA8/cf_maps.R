library(ggplot)
library(viridis)
library(sp)
library(rgdal)
library(rgeos)
library(raster)


theme_map_black <- function(...) {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text( color = "#000000"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #plot.background = element_blank(),
      panel.background = element_rect(fill = "black", color = NA), 
      plot.background = element_rect(fill = "black", color = NA), 
      #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.text = element_text( color = "#000000"),
      legend.title = element_text( color = "#000000"),
      panel.border = element_blank(),
      ...
    )
}


#### #### #### #### #### #### #### #### #### 
wgs84 <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
### Country borders
countries <- readOGR(dsn="data/cshapes_col",layer="cshapes_col")
countries <- spTransform(countries,crs(wgs84))

cshp_cropped <- crop(countries,extent(-23,55,-38,39))

cshp_hance <- cshp_cropped[cshp_cropped$cowcode%in%c(404:553,625,626),]
cshp_afr <- cshp_cropped[cshp_cropped$cowcode%in%c(404:626,651) & cshp_cropped$cowcode%nin%c(590,591,581),]

# fortify for ggplot
cshp_cropped_fort <- fortify(cshp_cropped)
cshp_afr_fort <- fortify(cshp_cropped)
cshp_hance_fort <- fortify(cshp_hance)


# load grid id raster
r25.id <- readRDS("data/r25.rds")

################################################
################################################
#### Plot distribution of counterfactual cells from RI analysis

saveRDS(cf.df.kppm,"data/cf_df_kppm.rds")
cf.df.kppm <- readRDS("data/cf_df_kppm.rds")

coordinates(cf.df.kppm) <-cf.df.kppm[,c("Longitude","Latitude")]
names(cf.df.kppm)
crs(cf.df.kppm) <- crs(r25.id)
gridded(cf.df.kppm) <-T

r.cf <- raster(cf.df.kppm[,"cf.freq"])
r.cf2 <- raster(cf.df.kppm[,"cf.freq.cuts"])
plot(r.cf2)


map.cf <- rasterToPoints(r.cf)
#Make the points a dataframe for ggplot
map.cf <- data.frame(map.cf)
#Make appropriate column headings
colnames(map.cf) <- c("Longitude", "Latitude", "cf")
describe(map.cf)
## set zero values to na
map.cf$cf[map.cf$cf==0] <- NA

map.cf2 <- rasterToPoints(r.cf2)
#Make the points a dataframe for ggplot
map.cf2 <- data.frame(map.cf2)
#Make appropriate column headings
colnames(map.cf2) <- c("Longitude", "Latitude", "cf")
describe(map.cf2)
## set zero values to na
map.cf2$cf[map.cf2$cf==0] <- NA

map.cf2$cf2 <- ifelse(map.cf2$cf==1,"<30",
                      ifelse(map.cf2$cf==2,"30-74",
                             ifelse(map.cf2$cf==3,"75-149", 
                                    ifelse(map.cf2$cf==4,"150-300",">300"))))

map.cf2$cf2 <- factor(map.cf2$cf2,levels=c("<30","30-74","75-149", "150-300",">300"))




library(viridis)
cols <- viridis(5,begin=0.3,end=0.9)


p1 <- ggplot() + 
  geom_polygon(data = cshp_afr_fort, aes(x=long, y=lat, group=group), color=NA, show.legend = F,fill="grey25") + 
  geom_raster(data=map.cf2,aes(y=Latitude, x=Longitude,fill=cf2))  +
  scale_fill_manual(na.value="transparent",values=c("<30"=cols[1],"30-74"=cols[2],"75-149"=cols[3],"150-300"=cols[4],">300"=cols[5]),
                    name="Counterfactual Cash Crop Cells\n(sampled in N of 1000 draws)",
                    guide= guide_legend(
                      direction = "horizontal",
                      label.position = "bottom",
                      title.position = "top",
                      keyheight = unit(2.5, units = "mm"),
                      keywidth  = unit(18, units = "mm")
                    )) +
  geom_path(data = cshp_hance_fort, aes(x = long, y = lat, group = group), color = "white", size=0.6) +
  theme_map_black() + theme(legend.position = "bottom",legend.text = element_text(color="white",size=14),legend.title = element_text(color="white",size=16)) 
p
p +  ggsave("output/tables_wd/Map_CF_crops_kppm_all.pdf", width=11, height=12) 
guide= "none"

p2 <- ggplot() + 
  geom_polygon(data = cshp_afr_fort, aes(x=long, y=lat, group=group), color=NA, show.legend = F,fill="grey25") + 
  geom_raster(data=map.cf2,aes(y=Latitude, x=Longitude,fill=cf2))  +
  scale_fill_manual(na.value="transparent",values=c("<30"=cols[1],"30-74"=cols[2],"75-149"=cols[3],"150-300"=cols[4],">300"=cols[5]),
                    name="Counterfactual Cash Crop Cells\n(sampled in N of 1000 draws)",
                    guide= "none") +
  geom_point(data=sim.df, aes(x=Longitude,y=Latitude,color=resource),  alpha=1, stroke=.8,shape=20,size=0.3) +
  scale_color_manual(name="One Simulation",
                     values=c("Cash Crops"="#FB9E07FF"),
                     guide = guide_legend(
                       title.position="top",
                       title.hjust =0.5,
                       label.position="left",
                       override.aes = list(size = 3)
                     )) +
  geom_path(data = cshp_hance_fort, aes(x = long, y = lat, group = group), color = "white", size=0.6) +
  theme_map_black() + theme(legend.position = "bottom",legend.text = element_text(color="white",size=14),legend.title = element_text(color="white",size=16)) 
p
p +  ggsave("output/tables_wd/Map_CF_crops_kppm_add.pdf", width=11, height=12) 

p.out <- p1 | p2
p.out +  ggsave("output/tables_wd/Map_CF_crops_kppm_both.pdf", width=18, height=10.5) 



################################################
################################################
#### Plot distribution of counterfactual cells from RI analysis PM

cf.df.kppm <- readRDS("data/cf_df_ppm.rds")

coordinates(cf.df.kppm) <-cf.df.kppm[,c("Longitude","Latitude")]
names(cf.df.kppm)
crs(cf.df.kppm) <- crs(r25.id)
gridded(cf.df.kppm) <-T

r.cf <- raster(cf.df.kppm[,"cf.freq"])
r.cf2 <- raster(cf.df.kppm[,"cf.freq.cuts"])
plot(r.cf2)


map.cf <- rasterToPoints(r.cf)
#Make the points a dataframe for ggplot
map.cf <- data.frame(map.cf)
#Make appropriate column headings
colnames(map.cf) <- c("Longitude", "Latitude", "cf")
describe(map.cf)
## set zero values to na
map.cf$cf[map.cf$cf==0] <- NA

map.cf2 <- rasterToPoints(r.cf2)
#Make the points a dataframe for ggplot
map.cf2 <- data.frame(map.cf2)
#Make appropriate column headings
colnames(map.cf2) <- c("Longitude", "Latitude", "cf")
describe(map.cf2)
## set zero values to na
map.cf2$cf[map.cf2$cf==0] <- NA

map.cf2$cf2 <- ifelse(map.cf2$cf==1,"<30",
                      ifelse(map.cf2$cf==2,"30-74",
                             ifelse(map.cf2$cf==3,"75-149", 
                                    ifelse(map.cf2$cf==4,"150-300",">300"))))

map.cf2$cf2 <- factor(map.cf2$cf2,levels=c("<30","30-74","75-149", "150-300",">300"))




library(viridis)
cols <- viridis(5,begin=0.3,end=0.9)


p1 <- ggplot() + 
  geom_polygon(data = cshp_afr_fort, aes(x=long, y=lat, group=group), color=NA, show.legend = F,fill="grey25") + 
  geom_raster(data=map.cf2,aes(y=Latitude, x=Longitude,fill=cf2))  +
  scale_fill_manual(na.value="transparent",values=c("<30"=cols[1],"30-74"=cols[2],"75-149"=cols[3],"150-300"=cols[4],">300"=cols[5]),
                    name="Counterfactual Cash Crop Cells\n(sampled in N of 1000 draws)",
                    guide= guide_legend(
                      direction = "horizontal",
                      label.position = "bottom",
                      title.position = "top",
                      keyheight = unit(2.5, units = "mm"),
                      keywidth  = unit(18, units = "mm")
                    )) +
  geom_path(data = cshp_hance_fort, aes(x = long, y = lat, group = group), color = "white", size=0.6) +
  theme_map_black() + theme(legend.position = "bottom",legend.text = element_text(color="white",size=14),legend.title = element_text(color="white",size=16)) 
p1
#p +  ggsave("output/tables_wd/Map_CF_crops_kppm_all.pdf", width=11, height=12) 


kppm.crops.sim <- readRDS("data/ppm_sims.rds")
sim.df <- data.frame(Longitude=kppm.crops.sim[[400]]$x,Latitude=kppm.crops.sim[[400]]$y,resource="Cash Crops")


p2 <- ggplot() + 
  geom_polygon(data = cshp_afr_fort, aes(x=long, y=lat, group=group), color=NA, show.legend = F,fill="grey25") + 
  geom_raster(data=map.cf2,aes(y=Latitude, x=Longitude,fill=cf2))  +
  scale_fill_manual(na.value="transparent",values=c("<30"=cols[1],"30-74"=cols[2],"75-149"=cols[3],"150-300"=cols[4],">300"=cols[5]),
                    name="Counterfactual Cash Crop Cells\n(sampled in N of 1000 draws)",
                    guide= "none") +
  geom_point(data=sim.df, aes(x=Longitude,y=Latitude,color=resource),  alpha=1, stroke=.8,shape=20,size=0.3) +
  scale_color_manual(name="One Simulation",
                     values=c("Cash Crops"="red"),
                     guide = guide_legend(
                       title.position="top",
                       title.hjust =0.5,
                       label.position="left",
                       override.aes = list(size = 3)
                     )) +
  geom_path(data = cshp_hance_fort, aes(x = long, y = lat, group = group), color = "white", size=0.6) +
  theme_map_black() + theme(legend.position = "bottom",legend.text = element_text(color="white",size=14),legend.title = element_text(color="white",size=16)) 
p2
#p +  ggsave("output/tables_wd/Map_CF_crops_kppm_add.pdf", width=11, height=12) 
library(patchwork)
p.out <- p1 | p2
p.out +  ggsave("output/tables_wd/Map_CF_crops_ppm_both.pdf", width=18, height=10.5) 







