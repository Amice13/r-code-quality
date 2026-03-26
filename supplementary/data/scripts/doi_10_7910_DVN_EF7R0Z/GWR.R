# Produces geographically weighted regression results which show geographic variation in the effects
# Requires:
#     - full_upsampled_10.Rdata
#     - data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp 
# National borders from https://github.com/nvkelso/natural-earth-vector/blob/master/50m_cultural/ne_50m_admin_0_countries.shp because they are 
# lower resolution and plot easier
# Produces:
#     - GWR_democ.pdf
#     - GWR_elec.pdf
#     - GWR_close80.pdf
#     - GWR_close90.pdf

# if not already installed
# install.packages("tidyverse","lfe","sf","ggplot2")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(tidyverse)
library(lfe)
library(sf)
library(ggplot2)

# Use upsampled data to reduce computational load
load("../data/output/full_upsampled_10.Rdata")
full$Polity_class_numeric<-as.numeric(as.factor(full$Polity_class))

### Democracy analysis ####
# get the coordinates for each decimal degree, the center of each regression kernel
coordinates<-full %>% dplyr::select(x,y) %>% mutate(x=round(x,0),y=round(y,0)) %>% distinct(x,y) %>% arrange(x) %>% arrange(y)

# cut down to the necessary variables
full_clipped<-full%>% dplyr::select(x,y,forest.diff,forest.l,nn_forest.l,election_DPI,democracy_BX,close80,close90,Polity_class,Polity_class_numeric,margin.norm,PCGDP.l,PCGDP.change.l,Pop.growth.l,year,un,FID)

# initialize outcome storage objects
outcome<-list(NA)
coordinates_out<-list(NA)

system.time(
    for(i in 1:length(coordinates$x)){
        # get location of kernel center
        x1<-as.integer(coordinates[i,1])
        y1<-as.integer(coordinates[i,2])
        # clip dataset to within 2 decimal degrees of latitude and longitude of kernel center (boxcar kernel). Other kernels really didn't change much.
        small<-full_clipped %>% filter(abs(x-x1)<=2,abs(y-y1)<=2)
        # run regression, save results in a dataset
        tryCatch({
            suppressWarnings(coefs<-summary(felm(forest.diff ~ forest.l + nn_forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID|0|un, data=small))$coefficients)
            # store coefficients
            outcome[[i]]<-coefs
            # store coordinates of that regression
            coordinates_out[[i]]<-c(x1,y1)},
            error=function(e){
                e
                outcome[[i]]<-NA
            })
        
        # show progress
        if(i%%100==0){print(round(i/length(coordinates$x),2))}
    })
# save coefficients
save(outcome, file="../data/output/spatial_democracy.Rdata")
# save coordinates
save(coordinates_out, file = "../data/output/spatial_democracy_coords.Rdata")


#### election analysis ####
# same procedures as above
outcome1<-list(NA)
coordinates1_out<-list(NA)

system.time(
    for(i in 1:length(coordinates$x)){
        x1<-as.integer(coordinates[i,1])
        y1<-as.integer(coordinates[i,2])
        small<-full_clipped %>% filter(abs(x-x1)<=2,abs(y-y1)<=2)
        tryCatch({
            suppressWarnings(coefs<-summary(felm(forest.diff ~ forest.l + nn_forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI|FID|0|un, data=small))$coefficients)
            outcome1[[i]]<-coefs
            coordinates1_out[[i]]<-c(x1,y1)},
            error=function(e){
                e
                outcome1[[i]]<-NA
            })
        
        # print(i)
        if(i%%100==0){print(round(i/length(coordinates$x),2))}
    })

save(outcome1, file="../data/output/spatial_election_nointeraction.Rdata")
save(coordinates1_out, file =  "../data/output/spatial_election_coords_nointeraction.Rdata")


### Margin<20 analysis####

outcome2<-list(NA)
coordinates2_out<-list(NA)

system.time(
    for(i in 1:length(coordinates$x)){
        x1<-as.integer(coordinates[i,1])
        y1<-as.integer(coordinates[i,2])
        small<-full_clipped %>% filter(abs(x-x1)<=2,abs(y-y1)<=2)
        tryCatch({
            suppressWarnings(coefs<-summary(felm(forest.diff ~ forest.l + nn_forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + close80|FID|0|un, data=small))$coefficients)
            outcome2[[i]]<-coefs
            coordinates2_out[[i]]<-c(x1,y1)},
            error=function(e){
                e
                outcome2[[i]]<-NA
            })
        
        # print(i)
        if(i%%100==0){print(round(i/length(coordinates$x),2))}
    })


save(outcome2, file="../data/output/spatial_close80_nointeraction.Rdata")
save(coordinates2_out, file="../data/output/spatial_close80_coords_nointeraction.Rdata")


#### Election<10 analysis ####

outcome3<-list(NA)
coordinates3_out<-list(NA)

system.time(
    for(i in 1:length(coordinates$x)){
        x1<-as.integer(coordinates[i,1])
        y1<-as.integer(coordinates[i,2])
        small<-full_clipped %>% filter(abs(x-x1)<=2,abs(y-y1)<=2)
        tryCatch({
            suppressWarnings(coefs<-summary(felm(forest.diff ~ forest.l + nn_forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + close90|FID|0|un, data=small))$coefficients)
            outcome3[[i]]<-coefs
            coordinates3_out[[i]]<-c(x1,y1)},
            error=function(e){
                e
                outcome3[[i]]<-NA
            })
        
        # print(i)
        if(i%%100==0){print(round(i/length(coordinates$x),2))}
    })

save(outcome3, file="../data/output/spatial_close90_nointeraction.Rdata")
save(coordinates3_out, file="../data/output/spatial_close90_coords_nointeraction.Rdata")


### Plotting ####
library(sf)
library(raster)
# load countries shapefiles
countries1<-read_sf("../data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")

#### PRODUCES SI FIGURE 4 ####

load("../data/output/spatial_democracy.Rdata")
load("../data/output/spatial_democracy_coords.Rdata")

# get coefficient of interest, coordinates into a dataframe
test<-data.frame(coef=unlist(map(outcome, (function(x) if ("democracy_BX" %in% rownames(x)) {x["democracy_BX",1]} else {NA}))),
                 SE=unlist(map(outcome, (function(x) if ("democracy_BX" %in% rownames(x)) {x["democracy_BX",2]} else {NA}))),
                 x=unlist(map(coordinates_out, (function(x) if(length(x)>0) {x[1]} else{NA}))),
                 y=unlist(map(coordinates_out, (function(x) if(length(x)>0) {x[2]} else{NA}))))

mytheme <- theme(axis.line = element_line(size = 1, colour = "black"),
                 panel.background = element_rect(fill = "white"))

# plot, set ceiling and floor values
democ_map<-test %>% mutate(coef=replace(coef, coef<= -10, -10)) %>%
    mutate(coef=replace(coef, coef>= 10, 10)) %>%
    ggplot() + geom_point(aes(x,y,color=coef),size=0.1) + mytheme + theme_void() +
    scale_color_gradient2(limits=c(-10,10), breaks=c(-10,-5,0,5,10),labels=c("<-10",-5,0,5,">10"),
                          midpoint=0, low="chocolate4", mid="white",
                          high="green4", space ="Lab", name="Coefficient") +
    geom_sf(data = countries1, colour = "black", fill = NA) +
    theme(panel.grid.major = element_line(colour = "transparent"),
          legend.position = c(.7,.11),
          legend.direction = "horizontal")
ggsave(filename = "../figures/GWR_democ.pdf",
       plot = democ_map,device = "pdf",width = 7,height = 3.7,units = "in")


#### PRODUCES SI FIGURE 5 ####

load("../data/output/spatial_election_nointeraction.Rdata")
load("../data/output/spatial_election_coords_nointeraction.Rdata")
mytheme <- theme(axis.line = element_line(size = 1, colour = "black"),
                 panel.background = element_rect(fill = "white"))
test<-data.frame(coef=unlist(map(outcome1, (function(x) if ("election_DPI" %in% rownames(x)) {x["election_DPI",1]} else {NA}))),
                 SE=unlist(map(outcome1, (function(x) if ("election_DPI" %in% rownames(x)) {x["election_DPI",2]} else {NA}))),
                 x=unlist(map(coordinates1_out, (function(x) if(length(x)>0) {x[1]} else{NA}))),
                 y=unlist(map(coordinates1_out, (function(x) if(length(x)>0) {x[2]} else{NA}))))

elec_map<-test %>% mutate(coef=replace(coef, coef<= -10, -10)) %>%
    mutate(coef=replace(coef, coef>= 10, 10)) %>%
    ggplot() + geom_point(aes(x,y,color=coef),size=0.1) + mytheme + theme_void() +
    scale_color_gradient2(limits=c(-10,10), breaks=c(-10,-5,0,5,10),labels=c("<-10",-5,0,5,">10"),
                          midpoint=0, low="chocolate4", mid="white",
                          high="green4", space ="Lab", name="Coefficient") +
    geom_sf(data = countries1, colour = "black", fill = NA) +
    theme(panel.grid.major = element_line(colour = "transparent"),
          legend.position = c(.7,.11),
          legend.direction = "horizontal")
ggsave(filename = "../figures/GWR_elec.pdf",
       plot = elec_map,device = "pdf",width = 7,height = 3.7,units = "in")



#### PRODUCES SI FIGURE 6 ####
load("../data/output/spatial_close80_nointeraction.Rdata")
load("../data/output/spatial_close80_coords_nointeraction.Rdata")


test<-data.frame(coef=unlist(map(outcome2, (function(x) if ("close80" %in% rownames(x)) {x["close80",1]} else {NA}))),
                 SE=unlist(map(outcome2, (function(x) if ("close80" %in% rownames(x)) {x["close80",2]} else {NA}))),
                 x=unlist(map(coordinates2_out, (function(x) if(length(x)>0) {x[1]} else{NA}))),
                 y=unlist(map(coordinates2_out, (function(x) if(length(x)>0) {x[2]} else{NA}))))


close80_map<-test %>% mutate(coef=replace(coef, coef<= -10, -10)) %>%
    mutate(coef=replace(coef, coef>= 10, 10)) %>%
    ggplot() + geom_point(aes(x,y,color=coef),size=0.1) + mytheme + theme_void() +
    scale_color_gradient2(limits=c(-10,10), breaks=c(-10,-5,0,5,10),labels=c("<-10",-5,0,5,">10"),
                          midpoint=0, low="chocolate4", mid="white",
                          high="green4", space ="Lab", name="Coefficient") +
    geom_sf(data = countries1, colour = "black", fill = NA) +
    theme(panel.grid.major = element_line(colour = "transparent"),
          legend.position = c(.7,.11),
          legend.direction = "horizontal")
ggsave(filename = "../figures/GWR_close80.pdf",
       plot = close80_map,device = "pdf",width = 7,height = 3.7,units = "in")

#### PRODUCES SI FIGURE 7 ####
load("../data/output/spatial_close90_nointeraction.Rdata")
load("../data/output/spatial_close90_coords_nointeraction.Rdata")

test<-data.frame(coef=unlist(map(outcome3, (function(x) if ("close90" %in% rownames(x)) {x["close90",1]} else {NA}))),
                 SE=unlist(map(outcome3, (function(x) if ("close90" %in% rownames(x)) {x["close90",2]} else {NA}))),
                 x=unlist(map(coordinates3_out, (function(x) if(length(x)>0) {x[1]} else{NA}))),
                 y=unlist(map(coordinates3_out, (function(x) if(length(x)>0) {x[2]} else{NA}))))

close90_map<-test %>% mutate(coef=replace(coef, coef<= -10, -10)) %>%
    mutate(coef=replace(coef, coef>= 10, 10)) %>%
    ggplot() + geom_point(aes(x,y,color=coef),size=0.1) + mytheme + theme_void() +
    scale_color_gradient2(limits=c(-10,10), breaks=c(-10,-5,0,5,10),labels=c("<-10",-5,0,5,">10"),
                          midpoint=0, low="chocolate4", mid="white",
                          high="green4", space ="Lab", name="Coefficient") +
    geom_sf(data = countries1, colour = "black", fill = NA) +
    theme(panel.grid.major = element_line(colour = "transparent"),
          legend.position = c(.7,.11),
          legend.direction = "horizontal")
ggsave(filename = "../figures/GWR_close90.pdf",
       plot = close90_map,device = "pdf",width = 7,height = 3.7,units = "in")
