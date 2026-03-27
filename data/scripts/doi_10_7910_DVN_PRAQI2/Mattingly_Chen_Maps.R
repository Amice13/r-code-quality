
####################### 
####MAIN TEXT MAPS#####
####################### 


##Load in polygon data
#1911 Prefecture Shapefiles from Harvard China Historical GIS project
#See http://sites.fas.harvard.edu/~chgis/ for more information

prefectures <- readOGR(dsn = "_prefectures_china_polygons_mxl.shp")
prefectures <- spTransform(prefectures, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Subset to just the prefectures in China proper
prefectures <- prefectures[prefectures@data$LEV1_CH %in% dat$prov == TRUE,]
#Note that because of data availability, some 厅-level and other units are not included in this dataset


#Create polygon for prefectures with at least one Tongmenghui Member
prefectures.tmh <- prefectures[prefectures@data$NAME_CH %in% dat$pref[which(dat$tmh.dummy==1)]  == TRUE,]


#Create polygon for prefectures with at least one anti-missionary incident
prefectures.missionary <- prefectures[prefectures@data$NAME_CH %in% dat$pref[which(dat$missionary.cases.dummy==1)]  == TRUE,]

#Note that there are two prefectures named 太平府 but in different provinces
#We need to remove the one in 广西 province which did not experience an anti-missionary case
prefectures.missionary <- prefectures.missionary[ !(prefectures.missionary@data$NAME_CH ==  "太平府"& prefectures.missionary@data$LEV1_CH == "广西") ,]



pdf("Figure_1_A_Revolutionaries.pdf")
#Plot the full set of prefectures
plot(prefectures)
#Plot the revolutionary prefectures  shaded dark blue
plot(prefectures.tmh, lwd=0.2, col="darkblue", add=TRUE)
dev.off()

pdf("Figure_1_B_Missionaries.pdf")
#Plot the full set of prefectures
plot(prefectures)
#Plot the missionary conflict prefectures  shaded dark gray
plot(prefectures.missionary, lwd=0.2, col="darkgray", add=TRUE)
dev.off()


#Clean workspace
rm(list=ls()[which(ls()!="dat")])



