#maps of colombian armed group origins
#march 19, 2018

date()


#load data
library(foreign)
library(car)

all_muni <- read.dta("code_muni_cede.dta")
orig_muni <- read.csv("group_origins.csv")

#1. create indicator variables for originating groups####

colnames(orig_muni)[1] <- "Group"

auc_80muni <- subset(orig_muni$muni_code, orig_muni$Group == "AUC" & orig_muni$Five.year.period == 1980)
farc_muni <- subset(orig_muni$muni_code, orig_muni$Group == "FARC")


all_muni$farcauc <- 0
all_muni$farcauc[all_muni$muni_code %in% farc_muni] <- 1
all_muni$farcauc[all_muni$muni_code %in% auc_80muni] <- 2
all_muni$farcauc[all_muni$muni_code %in% farc_muni & all_muni$muni_code %in% auc_80muni] <- 1 #for map purposes

all_muni$farcauc <- as.factor(all_muni$farcauc)


levels(all_muni$farcauc) <- c(" ","FARC, 1965-1995","Paramilitaries, 1980")


#2. maps####

library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(plyr)

#load shape file and fortify
colom.shape <- readOGR(dsn=path.expand("mpio.shp"), layer =  "mpio")
colom.map.data <- fortify(colom.shape, region="MPIOS")

colom.shape2 <- colom.shape@data

#centroids for both groups
centroids <- as.data.frame(coordinates(colom.shape))
colnames(centroids) <- c("cent_long","cent_lat")

colom.shape2 <- cbind(colom.shape2, centroids)

#format for merging
colnames(colom.map.data)[colnames(colom.map.data) == "id"] <- "muni_code"
colom.map.data$muni_code <- as.integer(colom.map.data$muni_code)

colnames(colom.shape2)[colnames(colom.shape2) == "MPIOS"] <- "muni_code"
colom.shape2$muni_code <- as.integer(as.character(colom.shape2$muni_code))
colom.shape2 <- left_join(colom.shape2, all_muni, by="muni_code")

#fill missing data
colom.shape2$farcauc[is.na(colom.shape2$farcauc)] <- " "

#merge with shapefile
map.data <- merge(colom.map.data, colom.shape2, by = "muni_code", all.x = TRUE)

#remove islands
map.data <- subset(map.data, map.data$NOMBRE_DPT != "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA")

#create

map8 <- ggplot() +
  geom_polygon(data=map.data, aes(y=lat, x=long, group=group, fill=farcauc),color = "gray50") +
  #geom_point(data = subset(map.data, bothgroups == "Both Groups"),
  #           aes(x=cent_long, y=cent_lat,shape=bothgroups),size=4) +
  #guides(fill = guide_legend(order = 1),
  #       shape = guide_legend(order = 2))
  guides(fill = guide_legend(nrow = 1))

# Force the scales of each axis to be the same

map8 <- map8 + coord_equal()

# Get a plain white background

map8 <- map8 + theme(axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     axis.line.x = element_blank(),
                     panel.background = element_blank(),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.background = element_blank())


map8 <- map8 + scale_fill_manual(breaks=c("FARC, 1965-1995","Paramilitaries, 1980"," "),
                      values=c("black","gray30","white")) +
               # scale_shape_manual(breaks="Both Groups",values=13) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.5, 0.01)) +
  theme(legend.text=element_text(size=14)) #+
  #theme(legend.margin=margin(-1.5,-1.5,-1.5,-1.5)) +
  #theme(legend.spacing.y = unit(0.1, 'cm'))


# view maps

ggsave("figurea2-b.png", plot = map8, width = 10, height = 9)


sessionInfo()
date()
