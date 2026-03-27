#####################################################
############# REPLICATION MATERIALS #################
#####################################################

#This code reproduces the Figures 1-4 included in the article.

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

#change code for merge
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


# FIGURE 1: Spatial distribution of polling stations affected by police interventions ------------------------------------------------------------

library("rgdal")
library("sp")
library("rgeos")

#Open Catalan map
#UNZIP first the "Municipis.zip" file
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


library("ggplot2")
cat_map$id = rownames(as.data.frame(cat_map))
cat_map.pts <- fortify(cat_map, region="id") 
cat_map.df <- merge(cat_map.pts, cat_map, by="id", type='left')  

mapdata<-data.frame(attacks_data_sh)
names(mapdata)[names(mapdata)=="long"]<-"x"
names(mapdata)[names(mapdata)=="lat"]<-"y"

ggplot() + # the data
  geom_polygon(data=cat_map.df, aes(long,lat, group=group), 
               fill=NA, color="black",  size = 0.2) + # make polygons
  geom_point(data=mapdata, aes(x=x, y=y), color="red", size=1.2) +
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) +
  coord_equal() 


#TO EXPORT FIGURE, COMMENT OUT THE NEXT LINE
ggsave("Figure_1.png", width = 8, height = 8)


# Figure 2: Turnout in municipalities with and without police intervention (parallel trends) --------

#Import 2012 election data
df_2012 <- read_xlsx("eleccions_2012.xlsx", sheet="neta")
#get the code ready to join
df_2012$codi_unic <- str_pad(df_2012$codi_mun, 5, pad = "0")
#Import data on other covariates to be used later on in the models (2015 and 2012)
df_2015_2012 <- read_xlsx("dades_2015_2012.xlsx", sheet="dades")
#get the code ready to join
df_2015_2012$codi_unic <- str_pad(df_2015_2012$codi_mun, 5, pad = "0")
#Import 9n 2014 referendum dataset
df_9n <- read_xlsx("dades_9n.xlsx", sheet="dades")
#get the code ready to join
df_9n$codi_unic <- str_pad(df_9n$codimun, 5, pad = "0")
#calculate turnout 9n
df_9n$pvotsobiranista_14 <- df_9n$psi_9n
df_9n$turnout_14 <- (df_9n$part_9n*100)/df_9n$cens_9n

#Get the data ready to reshape and plot
myvars12 <- c("codi_unic", "nom_mun", "turnout_12", "pvotsobiranista12")
df_did_12 <- df_2012[myvars12]
df_did_12 <- dplyr::rename(df_did_12, pvotsobiranista_12 = pvotsobiranista12)
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
l_df <- df_did %>%  tidyr::pivot_longer(cols = starts_with(c("turnout","pvotsobiranista")), 
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

#Let's plot it
library("Hmisc")
library("Rmisc")
df_plot_ptrents <-  l_df_mini %>%
  group_by(attack, year) %>%
  dplyr::summarise(avg_turnout = mean(turnout), 
                   uci_turnout = CI(turnout)[1], 
                   lci_turnout = CI(turnout)[3]) %>%
  mutate(year = year %>% as.factor())



ggplot(df_plot_ptrents) +
  geom_pointrange(aes(x = year, y = avg_turnout, 
                      ymin = lci_turnout, ymax = uci_turnout, group=attack,
                      linetype=attack),  
                  lwd = 1/2,shape = 21, fill = "WHITE") +
  geom_line(aes(x = year, y = avg_turnout, group=attack,
                linetype=attack )) +
  theme_bw() + 
  theme(legend.position="none") +
  ylab("Turnout") + 
  xlab("Elections and consultations") +
  scale_linetype_manual(name = "Police intervention?", 
                        values = c("dashed","solid")) +
  scale_x_discrete(breaks=c("12","14","15", "17"),
                   labels=c("2012 Election","2014 consultation",
                            "2015 Election", "2017 referendum")) +
  annotate("text", x = 3.4, y = 55, label = "Police intervention") +
  annotate("text", x = 3.8, y = 74, label = "Unaffected") 

#TO EXPORT FIGURE, COMMENT OUT THE NEXT LINE
ggsave("Figure_2.pdf", width = 8, height = 8)



# Figure 3: The effect of police interventions on turnout -----------------

library("margins")

#Remove outliers for analysis (see appendix)
df_mini <- df[ which(df$part_1o<330), ]

#Police intervention versus not
model_poli <- lm(part_1o ~ attack +  alcalde2015_net + per_nascutsaltresccaa17 +
                   pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_mini)

#Police intervention - categorical variable
model_violence_cat <- lm(part_1o ~ violence_cat +  alcalde2015_net + 
                           per_nascutsaltresccaa17 +
                           pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation  , 
                         data = df_mini)
#Number policemen
model_violence_number <- lm(part_1o ~ police_pob_qt  +  alcalde2015_net + 
                              per_nascutsaltresccaa17 +
                              pvotsobiranista15 + densitat17 + 
                              log_pobtotal_17 + elevation , data = df_mini)
#sieze ballots
model_ballots <- lm(part_1o ~ tookballot +  alcalde2015_net + per_nascutsaltresccaa17 +
                      pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation   ,
                    data = df_mini)

#TO EXPORT FIGURE, COMMENT OUT THE NEXT LINE
pdf("Figure_3.pdf", width = 10, height = 8)
par(mfrow=c(2,2))
cplot(model_poli, "attack", what = "prediction", 
      main = "",
      ylab="Predicted turnout",
      xlab = "Police intervention?")
cplot(model_violence_cat, "violence_cat", what = "prediction", 
      main = "",
      ylab="Predicted turnout",
      xlab = "Intensity of police violence")
cplot(model_violence_number, "police_pob_qt", what = "prediction", 
      main = "",
      ylab="Predicted turnout",
      xlab = "No. of police officers/10,000 inhabitants")
cplot(model_ballots, x = "tookballot",  what = "prediction",
      main = "", 
      ylab="Predicted turnout",
      xlab = "Did the police confiscate electoral material?")
dev.off()


# Figure 4: Predicted turnout around targeted municipalities --------------

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

#dummy 1 intervention; 0 otherwise
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
#We remove those municipalities that were not affected by a police intervention.
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

df_mini <- df[ which(df$part_1o<330), ]

#Models
model_violence_adjacency <- lm(part_1o ~  adjacent +  alcalde2015_net + 
                                 per_nascutsaltresccaa17 +
                                 pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation ,
                               data = df_mini)
model_buff5 <- lm(part_1o ~   buffer5 +  alcalde2015_net + per_nascutsaltresccaa17 +
                    pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_mini)

model_buff10 <- lm(part_1o ~   buffer10 +  alcalde2015_net + per_nascutsaltresccaa17 +
                     pvotsobiranista15 + densitat17 + log_pobtotal_17 + elevation , data = df_mini)


#Plot them
#Adjacent municipalities
plot_adjacent <- cplot(model_violence_adjacency, "adjacent", what = "prediction", 
                       ylab="Predicted turnout",
                       xlab = "Adjacent municipality")

p_adjacentg <-   ggplot(plot_adjacent) +
  geom_pointrange(aes(x = xvals, y = yvals, ymin = lower, ymax = upper), fill = "gray70") +
  xlab("Adjacent municipality") +
  ylab("Predicted turnout") +
  ggtitle("Adjacent municipalities") +
  theme_bw()

#Distance to raided municipalities 5-10km away
buf0 <- cplot(model_poli)
buf0 <- buf0 %>% subset(xvals == "Yes")
buf0$type <- 0

buf5 <- cplot(model_buff5)
buf5 <- buf5 %>% subset(xvals == "Yes")
buf5$type <- 1

buf10 <- cplot(model_buff10)
buf10 <- buf10 %>% subset(xvals == "Yes")
buf10$type <- 2

df_coef <- rbind(buf0, buf5, buf10)

df_coef$type <- factor(df_coef$type,
                       levels = c(0,1,2),
                       labels = c("Police intervention", "5km away", "10km away"))

distance_plot <- ggplot(df_coef) +
  geom_pointrange(aes(x = type, y = yvals, ymin = lower, ymax = upper)) +
  theme_bw() +
  ggtitle("Distance to raid polling stations") +
  ylab("Predicted turnout")  +
  xlab("Distance from a police intervention") 

#Quadratic distance plot
model_distance <- lm(part_1o ~ log_distancekm + I(log_distancekm^2)   +  
                       alcalde2015_net + 
                       per_nascutsaltresccaa17 +
                       pvotsobiranista15 + densitat17 + 
                       log_pobtotal_17 + elevation, data = df_mini)

plot_quadratic <- cplot(model_distance, x = "log_distancekm",  what = "prediction",
                        main = "", 
                        ylab="Predicted turnout",
                        xlab = "(Log of) Distance to nearest raided polling station")

p_quadraticg <- ggplot(plot_quadratic, aes(x = xvals)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray70") +
  geom_line(aes(y = yvals)) +
  geom_rug(data=df_mini, aes(x = log_distancekm), inherit.aes = F) +
  xlab("(Log of) Distance to nearest raided polling station") +
  ylab("Predicted turnout") +
  ggtitle("Distance to raided polling stations (Curvilinear relationship)") +
  theme_bw() +
  xlim(0 , 4)



###Combine
library("gridExtra")
plot_save <- grid.arrange(arrangeGrob(p_adjacentg, distance_plot, ncol = 2),    
                          p_quadraticg, 
                          nrow = 2)     

#TO EXPORT FIGURE, COMMENT OUT THE NEXT LINE
ggsave("Figure_4.pdf",plot_save, width = 8, height = 8)
