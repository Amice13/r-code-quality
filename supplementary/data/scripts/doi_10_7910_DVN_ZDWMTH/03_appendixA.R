#####################################################
############# REPLICATION MATERIALS #################
#####################################################

#This code reproduces the tables and figures in the online appendix.

#Rodon, Toni; Guinjoan, Marc. Beaten ballots: Political participation dynamics amidst police interventions, Political Science & Research Methods.

rm(list=ls())
#UNCOMMENT AND INCLUDE YOUR PATH
#setwd("YOUR PATH HERE")


# Clean and prepare datasets ---------------------------------------------------------

#Import polling stations data file and referendum results file
library("readxl")
polling_data <- read_xlsx("data_analysis.xlsx", sheet="data_unique")

library("readstata13")
mydata <- read.dta13("data_1o.dta", nonint.factors = TRUE) 
data_2017 <- read.dta13("dades_2017.dta", nonint.factors = TRUE) 

#change code to allow merging
library("stringr")
polling_data$codi_unic <- str_pad(polling_data$code, 5, pad = "0")

#merge
library("dplyr")
df <- left_join(mydata, polling_data)
df <- left_join(df, data_2017)

#municipalities that did not witness a police intervention should get a zero
df <- tidyr::replace_na(df, list(attack=0))

#turn it into a factor and label it
df$attack <- as.factor(df$attack)
df$attack <- factor(df$attack,
                    levels = c(0,1),
                    labels = c("No", "Yes"))

#create a factory identifying the mayor's party
df$alcalde2015_net <- 0
df$alcalde2015_net[df$alcalde2015=="CiU"] <- 1
df$alcalde2015_net[df$alcalde2015=="ERC"] <- 2
df$alcalde2015_net[df$alcalde2015=="PSOE"] <- 3
df$alcalde2015_net[df$alcalde2015=="Altres"] <- 0
df$alcalde2015_net[df$alcalde2015=="IU"] <- 0
df$alcalde2015_net[df$alcalde2015=="PP"] <- 0
df$alcalde2015_net <- factor(df$alcalde2015_net,
                             levels = c(0,1,2,3),
                             labels = c("Others", "CiU", "ERC", "PSC"))

#Create violence categorical variable
df$violence_cat <- 0 #no police and no violence
df$violence_cat[df$violence==0] <- 1 #police, but no violence 
df$violence_cat[df$violence==1] <- 2 #police, and violence 
df$violence_cat[df$violence==2] <- 2 #police, and violence 

df$violence_cat <- as.factor(df$violence_cat)
df$violence_cat <- factor(df$violence_cat,
                          levels = c(0,1,2),
                          labels = c("No intervention", "Intervention: no violence",
                                     "Intervention: violence"))

#did the police seize the ballots?
df$ballots[is.na(df$ballots)] <- 99
df$tookballot <- 0 #no police intervention
df$tookballot[df$ballots==99] <- 0 #no police intervention
df$tookballot[df$ballots==0] <- 1 #police intervention, they did not sieze the ballots
df$tookballot[df$ballots==1] <- 2 #police intervention, they sieze the ballots

df$tookballot <- as.factor(df$tookballot)
df$tookballot <- factor(df$tookballot,
                        levels = c(0,1,2),
                        labels = c("No intervention", "Police: no confiscation",
                                   "Police: confiscation"))

#Number of policemen sent to each polling station
df$number_policemen[is.na(df$number_policemen)] <- 0

df$police_pob_qt  <- 0  
df$police_pob_qt[df$number_policemen>0 & df$number_policemen<31 ]  <- 1
df$police_pob_qt[df$number_policemen>30 ]  <- 2

df$police_pob_qt <- factor(df$police_pob_qt,
                           levels = c(0,1,2),
                           labels = c("No intervention", "Between 10-30", "More than 30"))

#log population 2017
df$log_pobtotal_17 <- log(df$total_17)

#Fix the name of one municipality
df <- df[ which(df$nom_mun!="Canonja, la"), ]

library("rgdal")
library("sp")
library("rgeos")

#Open Catalan map
#UNZIP first the "shapefile.zip" file
cat_map <- readOGR("Municipis", "Municipis")

#open latitude and longitude data for all police interventions
attacks_data <- read_xlsx("data_analysis.xlsx", sheet="data")


#create function to clean latitude and longitude
dms2dec <- function(dms, separators = c("º", "°", "\'", "\"")) {
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)
  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    sec[i] <- splits[[i]][3]
    hem[i] <- splits[[i]][4]
  }
  dec <- as.numeric(deg) + (as.numeric(min) / 60) + (as.numeric(sec) / 3600)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  dec <- sign * dec
  return(dec)
}  

attacks_data$lat <- dms2dec(attacks_data$lat)
attacks_data$long <- dms2dec(attacks_data$long)

#generate spatial object
attacks_data_sh <- attacks_data
coordinates(attacks_data_sh) <- cbind(attacks_data_sh$long, attacks_data_sh$lat)

#New projection for better visualization
MyNewProjection <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
cat_map <- spTransform(cat_map, MyNewProjection)


library("maptools")

#Distances between the affected polling station and the closest centroid
#we first create the centroids
centroids_cat <- gCentroid(cat_map, byid = TRUE)

#calculate distances
library("geosphere")

#calculate distance
dist <- distm(centroids_cat, attacks_data_sh)
#take the smallest value
min_Distance<-apply(dist, 1, min)
#data frame it
min_Distance <- as.data.frame(min_Distance)
#Bring the data back in
cat_map@data$Nearest_poll<-min_Distance$min_Distance

#dataframe we are going to use to merge back in
dat_distances <- as.data.frame(cat_map@data)
dat_distances$Codigo <- as.character(dat_distances$Codigo)

df <- left_join(df, dat_distances, by = c("codi_unic" = "Codigo"))

df$distinkm <- df$Nearest_poll/1000
df$log_distancekm <- log(1+df$distinkm)

#We now identify contiguous polygons.
library("spdep")

#calculate the number of attacks per municipality
library("maptools")
proj4string(attacks_data_sh) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
over.list <- over(cat_map, geometry(attacks_data_sh), returnList = TRUE)
num.attacks <- sapply(over.list, length)
num.attacks <- as.data.frame(num.attacks)
cat_map@data <- cbind(cat_map@data, num.attacks)

#dummy 1 attacked; 0 otherwise
cat_map$attacked[num.attacks==0] <- 0
cat_map$attacked[num.attacks>0] <- 1
cat_map$attacked <- as.factor(cat_map$attacked)

#Create a list that identifies adjacent polygons 
list.nb <- gTouches(cat_map, byid = TRUE, returnDense = FALSE)
#Attach the names of municipalities
list.Territories <- lapply(list.nb, function(x) cat_map$Codigo[x])
names(list.Territories)<-cat_map$Texto
#unlist and turn into a df
df_adjacency <- as.data.frame(unlist(list.Territories),ncol=7,byrow=TRUE)
library("data.table")
#add another column with names
df_adjacency <- setDT(df_adjacency, keep.rownames = TRUE)[]
#remove number
df_adjacency$rn <- gsub("^\\d+|\\d+$", "", df_adjacency$rn)    
#reshape from long to wide (we first change column names)
colnames(df_adjacency)=c("municipi", "adjacent")
#create unique identifier per group
df_adjacency <- data.frame(df_adjacency)
library("splitstackshape")
df_adjacency <- getanID(df_adjacency, id= "municipi")
df_adjacency <- dplyr::rename(df_adjacency, id = .id)
library("tidyr")
df_adjacency <- spread(df_adjacency, key = id, value = adjacent)
#change column names
colnames(df_adjacency)[2:18] <- str_c("adj_", colnames(df_adjacency)[2:18] )
#add unique id to merge
cat_map_ids <- data.frame(cat_map@data)
cat_map_ids <- cat_map_ids[c(1,2)] 
df_adjacency <- df_adjacency %>% left_join(cat_map_ids,by = c("municipi" = "Texto"))
df_adjacency$Codigo <- as.character(df_adjacency$Codigo)
#We now need to remove those municipalities that were not affected by a police intervention.
#We first subset and create an attacked dataset
df_attacked <- df[c("codi_unic", "attack")] 
#we merge it in
df_adjacency <- left_join(df_adjacency, df_attacked,by = c("Codigo" = "codi_unic") )
#remove those not attacked. 
df_adjacency_at <- df_adjacency[ which(df_adjacency$attack=="Yes"), ]
#remove Albages.
df_adjacency_at <- df_adjacency_at[!duplicated(df_adjacency_at$municipi),]
df_adjacency_at_long <- reshape(df_adjacency_at, direction = "long", 
                                varying = list(names(df_adjacency_at)[2:18]), 
                                idvar = c("Codigo","municipi"), sep="_")
#keep complete observations
df_adjacency_at_long<-na.omit(df_adjacency_at_long)
#it might be that we are identifying a polygon attacked close to another polygon attacked.
#remove them
df_adjacency_at_long$adj_1 <- as.character(df_adjacency_at_long$adj_1)
df_adjacency_at_long<-df_adjacency_at_long[!(df_adjacency_at_long$Codigo==df_adjacency_at_long$adj_1),]
#there are several duplicates in adjacent polygons. we keep only one
df_adjacency_at_long <- df_adjacency_at_long %>%
  group_by(adj_1) %>% filter (! duplicated(adj_1))
#change name column
names(df_adjacency_at_long)[names(df_adjacency_at_long)=="adj_1"]<-"codi_unic"
#create dummy identifying adjacents
df_adjacency_at_long$adjacent <- 1
#keep only variables of interest
df_adjacency_at_long <- df_adjacency_at_long[c("codi_unic", "adjacent")] 
#we now merge this dataset with the main dataset
df <- left_join(df, df_adjacency_at_long)
#replace attack missings with zero
df$adjacent[is.na(df$adjacent)] <- 0
#some municipalities need to be zeros because they themselves were attacked
df$adjacent[df$adjacent==1 & df$attack=="Yes" ] <- 0
df$adjacent <- as.factor(df$adjacent)

df$adjacent <- factor(df$adjacent,
                      levels = c(0,1),
                      labels = c("No", "Yes"))


###Calculate buffers
#fix projection first
crs.geo <- CRS("+proj=utm +zone=30 +ellps=intl +towgs84=0.0000,0.0000,0.0000,0.000000,0.000000,0.000000,0.00000000 +units=m +no_defs" )  # UTM 33N
cat_map <- spTransform(cat_map, crs.geo)
attacks_data_sh <- spTransform(attacks_data_sh, crs.geo)

#Calculate 5km buffer
buffered5 <- gBuffer(attacks_data_sh, width = 5000, byid = FALSE)
#we don't want buffers to "eat" the sea
intersection_five <- gIntersection(buffered5, cat_map, byid = TRUE)

result5 <- gArea(intersection_five, byid = TRUE)
result5 <- as.data.frame(result5)
colnames(result5) <- c("area_near_attack")
ids <- as.character(rownames(result5))
new.id.columns <- t(as.data.frame(strsplit(ids, " ")))
colnames(new.id.columns) <- c("id1", "id2")
result5 <- cbind(result5, new.id.columns)
cat_map$id2 <- rownames(as.data.frame(cat_map))
cat_map <- merge(cat_map, result5, by = "id2", type = "left")
library("gdata")
cat_map <- rename.vars(cat_map, from = "id1", to = "buffer5")

#now the same but 10 km
buffered10 <- gBuffer(attacks_data_sh, width = 10000, byid = FALSE)
intersection_ten <- gIntersection(buffered10, cat_map, byid = TRUE)

result10 <- gArea(intersection_ten, byid = TRUE)
result10 <- as.data.frame(result10)
colnames(result10) <- c("area_near_attack_10")
ids <- as.character(rownames(result10))
new.id.columns <- t(as.data.frame(strsplit(ids, " ")))
colnames(new.id.columns) <- c("id1", "id2")
result10 <- cbind(result10, new.id.columns)
cat_map$id2 <- rownames(as.data.frame(cat_map))
cat_map <- merge(cat_map, result10, by = "id2", type = "left")
cat_map <- rename.vars(cat_map, from = "id1", to = "buffer10")

#we now merge it with the main dataset
buffered_data <- data.frame(cat_map)
buffered_data <- buffered_data[c("Codigo", "area_near_attack", "buffer5", "area_near_attack_10",
                                 "buffer10")] 
df <- left_join(df, buffered_data, by = c("codi_unic" = "Codigo"))

df$buffer5net[df$buffer5=="buffer"] <- 1
df$buffer5net[is.na(df$buffer5net)] <- 0
df$buffer5 <- factor(df$buffer5net,
                     levels = c(0,1),
                     labels = c("No", "Yes"))
df$buffer5[df$attack=="Yes"] <- "No"

df$buffer10net[df$buffer10=="buffer"] <- 1
df$buffer10net <- as.numeric(df$buffer10net)
df$buffer10net[is.na(df$buffer10net)] <- 0
df$buffer10 <- factor(df$buffer10net,
                      levels = c(0,1),
                      labels = c("No", "Yes"))
df$buffer10[df$attack=="Yes"] <- "No"

df$area_near_attack[is.na(df$area_near_attack)] <- 0

#Import 2012 elections dataset
df_2012 <- read_xlsx("eleccions_2012.xlsx", sheet="neta")
#get the code ready to join
df_2012$codi_unic <- str_pad(df_2012$codi_mun, 5, pad = "0")
#join
df <- left_join(df, df_2012, by="codi_unic")
#Import 
df_2015_2012 <- read_xlsx("dades_2015_2012.xlsx", sheet="dades")
#get the code ready to join
df_2015_2012$codi_unic <- str_pad(df_2015_2012$codi_mun, 5, pad = "0")
#join
df <- left_join(df, df_2015_2012, by="codi_unic")

#Import consulta dataset
df_9n <- read_xlsx("dades_9n.xlsx", sheet="dades")
#get the code ready to join
df_9n$codi_unic <- str_pad(df_9n$codimun, 5, pad = "0")
#join
df <- left_join(df, df_9n)

#calculate turnout 9n
df$ppart9n <- (df$part_9n*100)/df$cens_9n

#import density data
df_superf <- read_xlsx("dades_superficie.xlsx", sheet="dades")
names(df_superf)[names(df_superf) == 'codi_mun'] <- 'codi_unic'
df <- left_join(df, df_superf)

df$log_pobtotal_12 <- log(df$poblacio_2012)
df$densitat12 <- df$total_12/df$superficie_12
df$densitat14 <- df$pob_total_14/df$superficie_14
df$log_pobtotal_14 <- log(df$pob_total_14)

#Create dataset for analysis
df_mini <- df[ which(df$part_1o<330), ]

# Table A.1: Summary statistics -------------------------------------------

#Turnout
summary(df$part_1o)
nobs(df$part_1o)
sd(df$part_1o)
#Turnout (subset)
summary(df_mini$part_1o)
nobs(df_mini$part_1o)
sd(df_mini$part_1o)
#Police interventions
100*prop.table(table(df$attack))
#Type of intervention
100*prop.table(table(df$violence_cat))
#Confiscation electoral material
100*prop.table(table(df$tookballot))
#Adjacent municipalities
100*prop.table(table(df$adjacent))
#Buffer (5km)
100*prop.table(table(df$buffer5))
#Buffer (10km)
100*prop.table(table(df$buffer10))
#Number of police officers per 10,000 inhabitants
summary(df$number_policemen)
sd(df$number_policemen)
#Number of police officers (categorical)
100*prop.table(table(df$police_pob_qt))
#(Log) distance to nearest affected polling station
summary(df$log_distancekm)
sd(df$log_distancekm)
#Mayor 2015
100*prop.table(table(df$alcalde2015_net))
#Percentage not born in Catalonia (2015)
summary(df$per_nascutsaltresccaa15)
sd(df$per_nascutsaltresccaa15)
#Percentage support secessionist parties (2015)
summary(df$pvotsobiranista15)
sd(df$pvotsobiranista15)
#Population density (2017)
summary(df$densitat17)
sd(df$densitat17)
#(Log) Population (2017)
summary(df$log_pobtotal_17)
sd(df$log_pobtotal_17)
#Elevation (in m)
summary(df$elevation)
sd(df$elevation)


# Figure A.1: Turnout in the Catalan referendum (boxplots) --------------------------

pdf("boxplot_turnout.pdf")

par(mfrow=c(1,2))
boxplot(df$part_1o)
title(ylab="Turnout")

boxplot(df_mini$part_1o)
title(ylab="Turnout (Less than 330)")

#Close the graph
dev.off()


# Figure A.2: Municipalities adjacent to affected municipalities -----------------------

library("sf")
library("ggplot2")
map_adj <- st_read("Municipis/Municipis.shp")
map_adj <- left_join(map_adj, df) 

map_p <- st_as_sf(attacks_data, coords = c("long", "lat"), 
         crs = 4326, agr = "constant")

ggplot() +
  geom_sf(data=map_adj, aes(fill = adjacent)) +
  geom_sf(data=map_p, color="red", size=1.2) +
    scale_fill_manual(values=c("#999999", "black")) +
theme(line = element_blank(),  # remove the background, tickmarks, etc
      axis.text=element_blank(),
      axis.title=element_blank(),
      panel.background = element_blank()) +
  guides(fill=guide_legend(title="Adjacent"))


ggsave("map_adjacent.pdf", width = 8, height = 8)

# Figure A.3: 5km buffer around municipalities raided by the police -----------------------

pdf("map_buffers5.pdf")
plot(cat_map)
plot(intersection_five, col = "blue", add = T)

dev.off()

# Figure A.4: 10km buffer around municipalities raided by the police -----------------------

pdf("map_buffers10.pdf")
plot(cat_map)
plot(intersection_ten, col = "blue", add = T)
dev.off()

# Figure A.5: Spatial distribution of interviews and police interventions -------

#Load survey with R's latitude and longitude
library("readxl")
adreces <- read_xlsx("BOP 863 Adreces.xlsx", sheet="data")

#Create an spatial object
adreces_shp <- adreces
#from character to numeric
adreces_shp$long <- as.numeric(adreces_shp$GPS_LONG)
adreces_shp$lat <- as.numeric(adreces_shp$GPS_LAT)
coordinates(adreces_shp) <- cbind(adreces_shp$long, adreces_shp$lat)
#define common projection
MyNewProjection <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
cat_map <- spTransform(cat_map, MyNewProjection)
proj4string(adreces_shp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


#Polling stations coordinates
polling_data <- read_xlsx("data_analysis.xlsx", sheet="data")
#add a zero to code
library("stringr")
polling_data$codi_unic <- str_pad(polling_data$code, 5, pad = "0")

polling_data$lat <- dms2dec(polling_data$lat)
polling_data$long <- dms2dec(polling_data$long)

polling_data <-  tibble::rowid_to_column(polling_data, "ID")
polling_data$atac[polling_data$ID<59] <- "Respondent's residence"
polling_data$atac[polling_data$ID>58] <- "Police intervention"

polling_data$atac <- as.factor(polling_data$atac)
polling_data$atac  <- relevel(polling_data$atac , ref = "Respondent's residence")

polling_data_sh <- polling_data

coordinates(polling_data_sh) <- cbind(polling_data_sh$long, polling_data_sh$lat)

#Project it
proj4string(polling_data_sh) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#create a level for adreces
plotColors <- c("blue","red")

pdf("map_interviews.pdf", width = 10, height = 8)
plot(cat_map)
plot(adreces_shp, add=TRUE, col="blue")
plot(polling_data_sh, add=TRUE, col="red")
legend("bottomright", 
       legend=levels(polling_data_sh$atac),
       fill=plotColors, bty="n", cex=.8) 
dev.off()


# Table B.1: The effect of police violence on turnout ---------------------

model_poli <- lm(part_1o ~ attack +  alcalde2015_net + per_nascutsaltresccaa17 +
                    pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_mini)

model_violence_cat <- lm(part_1o ~ violence_cat +  alcalde2015_net + 
                           per_nascutsaltresccaa17 +
                           pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation  , 
                         data = df_mini)
model_violence_number <- lm(part_1o ~ police_pob_qt  +  alcalde2015_net + 
                              per_nascutsaltresccaa17 +
                              pvotsobiranista15 + densitat17 + 
                              log_pobtotal_17 + elevation , data = df_mini)
model_ballots <- lm(part_1o ~ tookballot +  alcalde2015_net + per_nascutsaltresccaa17 +
                      pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation   ,
                    data = df_mini)
model_distance <- lm(part_1o ~ log_distancekm + I(log_distancekm^2)   +  
                       alcalde2015_net + 
                       per_nascutsaltresccaa17 +
                       pvotsobiranista15 + densitat17 + 
                       log_pobtotal_17 + elevation, data = df_mini)
model_violence_adjacency <- lm(part_1o ~ adjacent +  alcalde2015_net + 
                                 per_nascutsaltresccaa17 +
                                 pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation ,
                               data = df_mini)
model_buff5 <- lm(part_1o ~ buffer5 +  alcalde2015_net + per_nascutsaltresccaa17 +
                    pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_mini)
model_buff10 <- lm(part_1o ~ buffer10 +  alcalde2015_net + per_nascutsaltresccaa17 +
                     pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_mini)


library("stargazer")
stargazer(model_poli, model_violence_cat,model_violence_number,model_ballots,
          model_distance,model_violence_adjacency,model_buff5,model_buff10 ,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          covariate.labels = 
            c("Attacked", "Moderate violence", "Intense violence",
              "Between 10-30", "More than 30", 
              "No confiscation of electoral material", 
              "Confiscation of electoral material",
              "Distance to nearest attacked polling station",
              "Distance to nearest attacked polling station$^2$",
              "Adjacent municipality", "Municipality in 5km (buffer)",
              "Municipality in 10km (buffer)",
              "Mayor 2015: CiU", "Mayor 2015: ERC", "Mayor 2015: PSC",
              "Percentage born in other AC (2017)",
              "Percentage support secessionist parties (2015)",
              "Population density (2017)",
              "(Log) total population (2017)", "Elevation (in m)"),
          dep.var.labels.include = FALSE,
          column.labels = c("Police intervention", "Violence intensity", 
                            "N. policemen", "Distance", "Adjacent",
                            "Buffer 5km","Buffer 10km"),
          title = "The effect of police violence on turnout",
          dep.var.caption = ""
)


# Figure C.1: The determinants of police interventions --------------------

library("jtools")
library("broom")
library("ggstance")

model_exclusion <- glm(attack ~ alcalde_sobiranista2015 + per_nascutsaltresccaa17 +
                         pvotsobiranista15 + turnout_15 + densitat17 + log_pobtotal_17  + elevation, 
                       data = df_mini, family = "binomial")

pdf("figures_exclusion.pdf", width = 8, height = 8)
plot_summs(model_exclusion, scale = TRUE, inner_ci_level = .90,
           coefs=c("Pro-independence mayor"="alcalde_sobiranista2015",
                   "% Turnout (2015)"="turnout_15",
                   "% born in other AC (2017)"="per_nascutsaltresccaa17",
                   "% support secessionist parties (2015)"="pvotsobiranista15",
                   "Population density (2017)"="densitat17",
                   "(Log) total population (2017)"="log_pobtotal_17",
                   "Elevation (in m)"="elevation"), colors="Qual2")
dev.off()



# Table D.1: Placebo test: The effect of police violence on secessionist support before the referendum --------


model_placebo1 <- lm(pvotsobiranista15 ~ attack +  per_nascutsaltresccaa15 +
                       densitat15 + log_pobtotal_15 + elevation , data = df_mini)
model_placebo2 <- lm(difvotsobiranista ~ attack +  per_nascutsaltresccaa15 +
                       pvotsobiranista15 + densitat15 + log_pobtotal_15  + elevation , data = df_mini)
model_placebo3 <- lm(pvotsobiranista12 ~ attack +  per_nascutsaltresccaa12 +
                       densitat12 + log_pobtotal_12  + elevation , data = df_mini)
model_placebo4 <- lm(turnout_15 ~ attack +  per_nascutsaltresccaa15 +
                       densitat15 + log_pobtotal_15  + elevation , data = df_mini)
model_placebo5 <- lm(turnout_12 ~ attack +   per_nascutsaltresccaa12 +
                       densitat12 + log_pobtotal_12  + elevation , data = df_mini)
model_placebo6 <- lm(psi_9n ~ attack +   per_nascutsaltresccaa14 +
                       densitat14 + log_pobtotal_14  + elevation , data = df_mini)
model_placebo7 <- lm(ppart9n ~ attack +  per_nascutsaltresccaa14 +
                       densitat14 + log_pobtotal_14  + elevation , data = df_mini)


stargazer(model_placebo1,model_placebo2,model_placebo3,
          model_placebo4,model_placebo5,model_placebo6,model_placebo7,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          dep.var.labels.include = FALSE,
          column.labels = c("Secessionist support 2015", 
                            "Increase secessionist support 2006-2015",
                            "Secessionist support 2012",
                            "Turnout 2015",
                            "Turnout 2012",
                            "Secessionist support 2014",
                            "Turnout 2014"),
          title = "Placebo test: The effect of police violence on secessionist support before the referendum")

# Table D.2: Placebo test: The effect of violence intensity on secessionist support before the referendum --------

model_placebo8 <- lm(pvotsobiranista15 ~ violence_cat +  per_nascutsaltresccaa15 +
                       densitat15 + log_pobtotal_15 + elevation , data = df_mini)
model_placebo9 <- lm(difvotsobiranista ~ violence_cat +  per_nascutsaltresccaa15 +
                       pvotsobiranista15 + densitat15 + log_pobtotal_15  + elevation , data = df_mini)
model_placebo10 <- lm(pvotsobiranista12 ~ violence_cat +  per_nascutsaltresccaa12 +
                        densitat12 + log_pobtotal_12  + elevation , data = df_mini)
model_placebo11 <- lm(turnout_15 ~ violence_cat +  per_nascutsaltresccaa15 +
                        densitat15 + log_pobtotal_15  + elevation , data = df_mini)
model_placebo12 <- lm(turnout_12 ~ violence_cat +   per_nascutsaltresccaa12 +
                        densitat12 + log_pobtotal_12  + elevation , data = df_mini)
model_placebo13 <- lm(psi_9n ~ violence_cat +   per_nascutsaltresccaa14 +
                        densitat14 + log_pobtotal_14  + elevation , data = df_mini)
model_placebo14 <- lm(ppart9n ~ violence_cat +  per_nascutsaltresccaa14 +
                        densitat14 + log_pobtotal_14  + elevation , data = df_mini)



stargazer(model_placebo8,model_placebo9,model_placebo10,
          model_placebo11,model_placebo12,model_placebo13,model_placebo14,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          dep.var.labels.include = FALSE,
          column.labels = c("Secessionist support 2015", 
                            "Increase secessionist support 2006-2015",
                            "Secessionist support 2012",
                            "Turnout 2015",
                            "Turnout 2012",
                            "Secessionist support 2014",
                            "Turnout 2014"),
          title = "Placebo test: The effect of violence intensity on secessionist support before the referendum")


# Table D.3: Placebo test: The effect of violence intensity (II) on secessionist support before the referendum --------

model_placebo15 <- lm(pvotsobiranista15 ~ police_pob_qt +  per_nascutsaltresccaa15 +
                        densitat15 + log_pobtotal_15 + elevation , data = df_mini)
model_placebo16 <- lm(difvotsobiranista ~ police_pob_qt +  per_nascutsaltresccaa15 +
                        pvotsobiranista15 + densitat15 + log_pobtotal_15  + elevation , data = df_mini)
model_placebo17 <- lm(pvotsobiranista12 ~ police_pob_qt +  per_nascutsaltresccaa12 +
                        densitat12 + log_pobtotal_12  + elevation , data = df_mini)
model_placebo18 <- lm(turnout_15 ~ police_pob_qt +  per_nascutsaltresccaa15 +
                        densitat15 + log_pobtotal_15  + elevation , data = df_mini)
model_placebo19 <- lm(turnout_12 ~ police_pob_qt +   per_nascutsaltresccaa12 +
                        densitat12 + log_pobtotal_12  + elevation , data = df_mini)
model_placebo20 <- lm(psi_9n ~ police_pob_qt +   per_nascutsaltresccaa14 +
                        densitat14 + log_pobtotal_14  + elevation , data = df_mini)
model_placebo21 <- lm(ppart9n ~ police_pob_qt +  per_nascutsaltresccaa14 +
                        densitat14 + log_pobtotal_14  + elevation , data = df_mini)


stargazer(model_placebo15,model_placebo16,model_placebo17,
          model_placebo18,model_placebo19,model_placebo20,model_placebo21,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          dep.var.labels.include = FALSE,
          column.labels = c("Secessionist support 2015", 
                            "Increase secessionist support 2006-2015",
                            "Secessionist support 2012",
                            "Turnout 2015",
                            "Turnout 2012",
                            "Secessionist support 2014",
                            "Turnout 2014"),
          title = "Placebo test: The effect of violence intensity (II) on secessionist support before the referendum")


# Table D.4: Placebo test: The effect of violence intensity (III) on secessionist support before the referendum --------

model_placebo22 <- lm(pvotsobiranista15 ~ tookballot +  per_nascutsaltresccaa15 +
                        densitat15 + log_pobtotal_15 + elevation , data = df_mini)
model_placebo23 <- lm(difvotsobiranista ~ tookballot +  per_nascutsaltresccaa15 +
                        pvotsobiranista15 + densitat15 + log_pobtotal_15  + elevation , data = df_mini)
model_placebo24 <- lm(pvotsobiranista12 ~ tookballot +  per_nascutsaltresccaa12 +
                        densitat12 + log_pobtotal_12  + elevation , data = df_mini)
model_placebo25 <- lm(turnout_15 ~ tookballot +  per_nascutsaltresccaa15 +
                        densitat15 + log_pobtotal_15  + elevation , data = df_mini)
model_placebo26 <- lm(turnout_12 ~ tookballot +   per_nascutsaltresccaa12 +
                        densitat12 + log_pobtotal_12  + elevation , data = df_mini)
model_placebo27 <- lm(psi_9n ~ tookballot +   per_nascutsaltresccaa14 +
                        densitat14 + log_pobtotal_14  + elevation , data = df_mini)
model_placebo28 <- lm(ppart9n ~ tookballot +  per_nascutsaltresccaa14 +
                        densitat14 + log_pobtotal_14  + elevation , data = df_mini)


stargazer(model_placebo22,model_placebo23,model_placebo24,
          model_placebo25,model_placebo26,model_placebo27,model_placebo28,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          dep.var.labels.include = FALSE,
          column.labels = c("Secessionist support 2015", 
                            "Increase secessionist support 2006-2015",
                            "Secessionist support 2012",
                            "Turnout 2015",
                            "Turnout 2012",
                            "Secessionist support 2014",
                            "Turnout 2014"),
          title = "Placebo test: The effect of violence intensity (III) on secessionist support before the referendum")

# Figure D.1: Support for secession in municipalities with and without a police intervention--parallel trends --------

#Get the data ready to reshape and plot
myvars12 <- c("codi_unic", "nom_mun", "turnout_12", "pvotsobiranista12")
df_did_12 <- df_2012[myvars12]
df_did_12 <- dplyr::rename(df_did_12, pvotsobiranista_12 = pvotsobiranista12)

df_9n$pvotsobiranista_14 <- df_9n$psi_9n
df_9n$turnout_14 <- (df_9n$part_9n*100)/df_9n$cens_9n
myvars14 <- c("codi_unic", "turnout_14", "pvotsobiranista_14")
df_did_14 <- df_9n[myvars14]
myvars15 <- c("codi_unic", "nom_mun", "turnout_15", "pvotsobiranista15")
df_did_15 <- df[myvars15]
df_did_15 <- dplyr::rename(df_did_15, pvotsobiranista_15 = pvotsobiranista15)

df$pno <- (df$no1o*100)/df$cens1o
df$psi <- (df$si1o*100)/df$cens1o
myvars17 <- c("codi_unic", "nom_mun", "part_1o", "psi", "attack")
df_did_17 <- df[myvars17]
df_did_17 <- dplyr::rename(df_did_17, pvotsobiranista_17 = psi,
                           turnout_17 = part_1o)

#Merge all of them
df_did <-  left_join(df_did_12, df_did_14) %>%
  left_join(., df_did_15) %>%
  left_join(., df_did_17)

#reshape
l_df <- df_did %>%  pivot_longer(cols = starts_with(c("turnout","pvotsobiranista")), 
                                 names_to = c(".value", "year"), names_sep = "_") 

l_df$attack[is.na(l_df$attack)] <- "No"


#Create dummies for year
l_df$t_2014 <- 0
l_df$t_2014[l_df$year=="14"] <- 1

l_df$t_2015 <- 0
l_df$t_2015[l_df$year=="15"] <- 1

l_df$t_2017 <- 0
l_df$t_2017[l_df$year=="17"] <- 1

#remove outliers
l_df_mini<-l_df[!(l_df$turnout>329),]
l_df_mini<-l_df_mini[!is.na(l_df_mini$turnout),] 

library("Hmisc")
library("Rmisc")
df_trend_vote <-   l_df_mini[!is.na(l_df_mini[["pvotsobiranista"]]), ] %>%
  group_by(attack, year) %>%
  dplyr::summarise(avg_sob = mean(pvotsobiranista,na.rm=T), 
                   uci_sob = CI(pvotsobiranista)[1], 
                   lci_sob = CI(pvotsobiranista)[3]) %>%
  mutate(year = year %>% as.factor())


ggplot(df_trend_vote) +
  geom_pointrange(aes(x = year, y = avg_sob, 
                      ymin = lci_sob, ymax = uci_sob, group=attack,
                      linetype=attack),  
                  lwd = 1/2,shape = 21, fill = "WHITE") +
  geom_line(aes(x = year, y = avg_sob, group=attack,
                linetype=attack )) +
  theme_bw() + 
  theme(legend.position="bottom") +
  ylab("Secessionist support") + 
  xlab("Elections and consultations") +
  scale_linetype_manual(name = "Police intervention?", 
                        values = c("dashed","solid")) +
  scale_x_discrete(breaks=c("12","14","15", "17"),
                   labels=c("2012 Election","2014 consultation",
                            "2015 Election", "2017 referendum*")) +
  annotate("text", x = 3.4, y = 55, label = "Police intervention") +
  annotate("text", x = 4, y = 78, label = "Unaffected") 


ggsave("parallel_trends_secession.pdf", width = 8, height = 8)

# Figure E.1: Municipalities where turnout was higher than 100% --------

map_adj$morehundred[map_adj$part_1o>100] <- 1
map_adj$morehundred[map_adj$part_1o<=100] <- 0

map_adj$morehundred <- factor(map_adj$morehundred,
                                 levels = c(0,1),
                                 labels = c("Less than 100%", "More than 100%"))


ggplot() +
  geom_sf(data=map_adj, aes(fill = morehundred), lwd=0) +
  scale_fill_brewer( palette = rev("Spectral"),  na.value="grey") + # fill with brewer colors
  geom_sf(data=map_p, color="red", size=1.2) +
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) +
  guides(fill=guide_legend(title=""))

ggsave("map_morehundred.pdf", width = 8, height = 8)


# Figure E.2: The effect of distance to a polling station intervened by the police on registering more than 100\% turnout --------

df$morehundred[df$part_1o>100] <- 1
df$morehundred[df$part_1o<=100] <- 0
df_mini <- df[ which(df$part_1o<330), ]

modcent <- glm(morehundred ~ log_distancekm + I(log_distancekm^2) + log_pobtotal_17 +  
                  pvotsobiranista15 + 
                  per_nascutsaltresccaa15 + elevation + densitat15  , 
                data = df_mini, family = "binomial")
summary(modcent)

library("margins")


pdf("morehundredpred.pdf", width = 10, height = 8)
cplot(modcent, x = "log_distancekm",  what = "prediction",
      main = "", 
      ylab="Predicted turnout",
      xlab = "(Log of) Distance to nearest raid polling station",
      xlim=c(0,4))
dev.off()


# Figure E.3: QQ Plot - outlier detection ---------------------------------

#cook distance
mod_outlier <- lm(part_1o ~ attack +  alcalde2015_net + per_nascutsaltresccaa17 +
                    pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation  , data = df)

library("car")


pdf("qq_outlier_plot.pdf")
qqPlot(mod_outlier, simulate=T, confidence = .95)
dev.off()

# Table E.1: The effect of police violence on voting in favour of and against independence - sensitivity to outliers ---------------------------------

model_all <- lm(part_1o ~ attack +  alcalde2015_net + per_nascutsaltresccaa17 +
                  pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation  , data = df)
df_200 <- df[ which(df$part_1o<200), ]
model_200 <- lm(part_1o ~ attack +  alcalde2015_net + per_nascutsaltresccaa17 +
                  pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_200)
df_150 <- df[ which(df$part_1o<150), ]
model_150 <- lm(part_1o ~ attack +  alcalde2015_net + per_nascutsaltresccaa17 +
                  pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_150)
df_125 <- df[ which(df$part_1o<125), ]
model_125 <- lm(part_1o ~ attack +  alcalde2015_net + per_nascutsaltresccaa17 +
                  pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_125)
df_mini <- df[ which(df$part_1o<100), ]
model_100 <- lm(part_1o ~ attack +  alcalde2015_net + per_nascutsaltresccaa17 +
                  pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_mini)

stargazer(model_all,model_200,model_150,model_125,model_100,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          covariate.labels = 
            c("Attacked", 
              "Mayor 2015: CiU", "Mayor 2015: ERC", "Mayor 2015: PSC",
              "Percentage born in other AC (2017)",
              "Percentage support secessionist parties (2015)",
              "Population density (2017)",
              "(Log) total population (2017)",
              "Elevation (in m)"),
          dep.var.labels.include = FALSE,
          column.labels = c("All", "$<$ 200", "$<$ 150", "$<$ 125"),
          title = "The effect of police violence on voting in favour and against independence-sensitivity to outliers",
          dep.var.caption = ""
)

# Figure F1: The effect of police violence on turnout (matching estimation) ---------------------------------

library("MatchIt")

cov <- c( 'per_nascutsaltresccaa17', 
          'pvotsobiranista15', 'densitat17', 
          'log_pobtotal_17', 'elevation', 'alcalde2015_net')

#logit model for matching
m_ps <- glm(attack ~  alcalde2015_net + per_nascutsaltresccaa17 +
              pvotsobiranista15 + densitat17 + 
              log_pobtotal_17 + elevation ,
            family = binomial, data = df)

#calculate municipality's probability of being attacked
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     attack = m_ps$model$attack)

#plot probabilities
labs <- paste("Attack:", c("Yes", "No"))

#execute matching
df_match <- df %>%
  dplyr::group_by(attack) %>%
  dplyr::summarize(n_mun = n(),
                   mean_part = mean(part_1o),
                   std_error = sd(part_1o) / sqrt(n_mun))

df_nomiss <- df %>%  # Remove missing values
  select(part_1o, attack, one_of(cov)) %>%
  na.omit()

#we need to remove the labels for it to work
df_nomiss$attack <- factor(df_nomiss$attack,
                           levels = c("No", "Yes"),
                           labels = c(0,1))
mod_match <- matchit(attack ~  alcalde2015_net + per_nascutsaltresccaa17 +
                       pvotsobiranista15 + densitat17 + elevation,
                     method = "nearest", data = df_nomiss)

dta_m <- match.data(mod_match)


#Examining covariate balance
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$attack <- as.factor(dta$attack)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = attack)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)}


library("gridExtra")


pdf("matching_robustness.pdf", width = 8, height = 8)
grid.arrange(
  fn_bal(dta_m, "per_nascutsaltresccaa17") + theme(legend.position = "none") +
    ylab("Born in other AC (2017)"),
  fn_bal(dta_m, "pvotsobiranista15") + theme(legend.position = "none") +
    ylab("Support secessionist parties (2015)"),
  fn_bal(dta_m, "densitat17") + theme(legend.position = "none") +
    ylab("Population density (2017)"),
  fn_bal(dta_m, "elevation") + theme(legend.position = "none") +
    ylab("Elevation (in m)"),
  fn_bal(dta_m, "log_pobtotal_17") +
    ylab("(Log) total population (2017)")+
    scale_colour_discrete(name="Police intervention?",
                          breaks=c("0" , "1"),
                          labels=c("No", "Yes")),
  nrow = 3, widths = c(1, 0.8)
)
dev.off()

# Table F1: The effect of police violence on turnout (matching estimation) ---------------------------------

#difference-in-means matched sample
dta_m %>%
  group_by(attack) %>%
  select(one_of(cov)) %>%
  summarise_all(funs(mean))

lm_treat <- lm(part_1o ~ attack + alcalde2015_net + per_nascutsaltresccaa17 +
                 pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation, data = dta_m)

stargazer(lm_treat,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          covariate.labels = 
            c("Police intervention", 
              "Mayor 2015: CiU", "Mayor 2015: ERC", "Mayor 2015: PSC",
              "Percentage born in other AC (2017)",
              "Percentage support secessionist parties (2015)",
              "Population density (2017)",
              "(Log) total population (2017)",
              "Elevation (in m)"),
          dep.var.labels.include = FALSE,
          column.labels = c("Match municipalities"),
          title = "The effect of police violence on turnout (matching estimation)",
          dep.var.caption = ""
)


# Table G.1: The effect of police violence on voting in favour and against independence --------------------------------------------------------------

model_si <-lm(psi ~ attack   +  
                    alcalde2015_net + 
                    per_nascutsaltresccaa17 +
                    pvotsobiranista15 + densitat17 + 
                    log_pobtotal_17 + elevation, data = df_mini)

model_no <-lm(pno ~ attack   +  
                alcalde2015_net + 
                per_nascutsaltresccaa17 +
                pvotsobiranista15 + densitat17 + 
                log_pobtotal_17 + elevation, data = df_mini)

stargazer(model_si, model_no,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          covariate.labels = 
            c("Police intervention", 
              "Mayor 2015: CiU", "Mayor 2015: ERC", "Mayor 2015: PSC",
              "Percentage born in other AC (2017)",
              "Percentage support secessionist parties (2015)",
              "Population density (2017)",
              "(Log) total population (2017)",
              "Elevation (in m)"),
          dep.var.labels.include = FALSE,
          column.labels = c("Match municipalities"),
          title = "The effect of police violence on voting in favour or against independence",
          dep.var.caption = "",
          type = "text"
)

# Figure G.1: The effect of police violence on voting in favour and against independence --------------------------------------------------------------


pdf("predicted_yesno.pdf", width = 10, height = 8)
par(mfrow=c(1,2))
cplot(model_no, x = "attack",  what = "prediction",
      ylab="Predicted turnout",
      xlab = "Police intervention?",
      main = "% against independence")
cplot(model_si, x = "attack",  what = "prediction",
      main = "% in favour independence", 
      ylab="Predicted turnout",
      xlab = "Police intervention?")
dev.off()


# Table H.1: The effect of police deployment time on turnout --------------------------------------------------------------

df_mini$time_left_full <- df_mini$time_left
df_mini$time_left_full[is.na(df_mini$time_left_full)] <- 11
df_mini$log_time_left_full <- log(df_mini$time_left_full)


model_time <- lm(part_1o ~ time_left_full  + alcalde2015_net + per_nascutsaltresccaa17 +
                   pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation ,
                 data = subset(df_mini, attack=="Yes"))

stargazer(model_time,
          digit.separate = 3,
          keep.stat = c("n", "rsq"),
          notes.append = FALSE,
          font.size = "scriptsize",
          no.space = TRUE,
          covariate.labels = 
            c("Time left after police deployment arrival", 
              "Mayor 2015: CiU", "Mayor 2015: ERC", "Mayor 2015: PSC",
              "Percentage born in other AC (2017)",
              "Percentage support secessionist parties (2015)",
              "Population density (2017)",
              "(Log) total population (2017)",
              "Elevation (in m)"),
          dep.var.labels.include = FALSE,
          column.labels = c("Match municipalities"),
          title = "The effect of police deployment time on turnout",
          dep.var.caption = ""
)

# Figure I.1: Google search volume of terms related to violence in September and October 2017-------------------------------------------------------------

#Warning: the 'news' option from the the library gtrendsR is often unstable and does not retrieve any number.
#If you encounter this problem, we scraped Google Trends on 6 July 2020 and saved the corresponding dataframe to the file df_gtrends.

#In addition, users need to be aware that their scraped count might not be exactly the same as the one included in the article. 
#This is due to changes in Google's policies and to the fact that, as explained by Google, "Non-realtime data is a separate sample from real-time data 
#and goes as far back as 2004 and up to 36 hours before your search".
#See https://support.google.com/trends/answer/4365533?hl=en
#The scraping code starts at line 1128 

#Load saved Google Trends dataset and plot it

df <- readRDS("gtrends.Rda")


ggplot(data=df, aes(x=date, y=hits,type=type, colour=type))+
  geom_line()+xlab('Time')+ylab('Relative Interest')+ 
  theme_bw()+
  theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+
  ggtitle("Google Search Volume")


ggsave("google_trends.pdf", width = 12, height = 8)


#Scraping of google trends data

library("gtrendsR")

#define keyword
keywords <- c("violencia policial")
#set geographic area
country <- c("ES")
#Set the time window
time<-("2017-09-01 2017-10-31")
#set channels 
channel<-'web'
channel2<- 'news'
channel3<- 'youtube'

#run the query
trends <- gtrends(keywords, gprop =channel,geo=country, time = time, onlyInterest = TRUE)
news <- gtrends(keywords, gprop =channel2,geo=country, time = time, onlyInterest = TRUE)
youtube <- gtrends(keywords, gprop =channel3,geo=country, time = time, onlyInterest = TRUE)

#select only interst over time 
time_trend <- trends$interest_over_time
newsdf <- news$interest_over_time
youtubedf <- youtube$interest_over_time

#Create identifier
time_trend$type <- 1
newsdf$type <- 2
youtubedf$type <- 3

#merge
library("dplyr")
df <-  rbind(time_trend, newsdf, youtubedf)

df <- df %>% mutate(type=recode_factor(type, 
                                       "1"="Web",
                                       "2"="News",
                                       "3"="Youtube"))

saveRDS(df, file="gtrends.Rda")

library("ggplot2")

ggplot(data=df, aes(x=date, y=hits,type=type, colour=type))+
  geom_line()+xlab('Time')+ylab('Relative Interest')+ 
  theme_bw()+
  theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+
  ggtitle("Google Search Volume")


#ggsave("google_trends_updated.pdf", width = 12, height = 8)


