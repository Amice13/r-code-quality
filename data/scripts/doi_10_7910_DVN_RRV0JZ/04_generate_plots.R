####
#Author: M.R. Kenwick
#Date: Dec 28, 2020
#Purpose: Generates visualizations. 
####
#Install packages, as necessary
#install.packages('haven')
#install.packages('dplyr')
#install.packages('countrycode')
#install.packages('foreign')
#install.packages('stringr')
#install.packages('sf')
#install.packages('tmap') #requires sf 0.9.7 or greater
#install.packages('RColorBrewer')
#install.packages('ggplot2')

rm(list=ls())
library(haven)
library(dplyr)
library(countrycode)
library(foreign)
library(stringr)
library(sf)
library(tmap)
library(RColorBrewer)
library(ggplot2)

#Set working directory to top level of replication folder
setwd()

#Load regression data and latent estiamtes
data<-read.csv('regression/regression_data.csv')
data<-arrange(data,state1,state2,year,borderid)
load('model/bo_static_output.RData')
#Summary indicators from latent data
data$theta_up<-data$theta + (1.96*data$theta_sd)
data$theta_lw<-data$theta - (1.96*data$theta_sd)
data<-cbind(as.data.frame(data),t(output$theta[1:500,]))


#####
#Figure 3: Border Orientation at the Level of the Border Crossing and Border Pair
#####
#Residualization
#Directed border crossing year model for residualization (Table B1, Model 1)
fit1<-lm(theta~elevation+lngdp1,data=data,na.action = na.exclude)
residuals_c<-residuals(fit1)
data$resid<-residuals(fit1)

#Averaging scores across both sides of the borderid and border for the map
data$borderid<-str_split_fixed(data$borderid,'_',2)[,1]
data_2017<-subset(data,year==2017)
data_cross <- data_2017 %>% 
  group_by(borderid) %>%
  mutate(theta = mean(theta,na.rm=T)) %>%
  mutate(resid = mean(resid,na.rm=T)) %>% 
  mutate(xi = mean(xi,na.rm=T)) %>%
  dplyr::select(borderid,state1,state2,theta,xi,resid,long,lat) %>%
  unique()
data_ddyad<- as.data.frame(data %>%
                             dplyr::select(ddyad,year,xi,elevation,lngdp1,state1,state2) %>%
                             group_by(ddyad,year) %>%
                             mutate(elevation = mean(elevation,na.rm=T))  %>%
                             mutate(lngdp1 = mean(lngdp1,na.rm=T))  %>%
                             mutate(xi = mean(xi)))  %>%
                             unique()
fit2<-lm(xi~elevation+lngdp1,data=data_ddyad,na.action = na.exclude)
data_ddyad$resid<-residuals(fit2)
data_ddyad$dyad<-ifelse(data_ddyad$state1<data_ddyad$state2,
                        data_ddyad$state1*1000+data_ddyad$state2,
                        data_ddyad$state2*1000+data_ddyad$state1)
data_dyad <- data_ddyad %>% 
  filter(year==2017) %>%
  group_by(dyad) %>%
  mutate(resid = mean(resid,na.rm=T)) %>%
  mutate(xi = mean(xi,na.rm=T)) %>%
  dplyr::select(dyad,xi,resid) %>%
  arrange(dyad,xi) %>%
  unique()
#Loading shapefiles for countries, roads, and landborders
countries<- st_read('model/country_shapefiles/ne_50m_admin_0_countries.shp')
roads<-st_read('model/roads/world_main_roads.shp')
landborders<-st_read('model/landborders/Int_Borders version 2.shp')
landborders$dyad<-ifelse(landborders$LEFT_FID<landborders$RIGHT_FID,
                         landborders$LEFT_FID*1000+landborders$RIGHT_FID,
                         landborders$RIGHT_FID*1000+landborders$LEFT_FID)
landborders<-merge(landborders,data_dyad,by="dyad",all.x=T,all.y=F)
countries = lwgeom::st_transform_proj(countries, crs = "+proj=moll")
data_cross<-st_as_sf(data_cross, coords = c("long", "lat"),crs = 4326)

bbox_new <- st_bbox(countries) 
xrange <- bbox_new$xmax - bbox_new$xmin 
yrange <- bbox_new$ymax - bbox_new$ymin
bbox_new[1] <- bbox_new[1] + (0.15 * xrange)
bbox_new[3] <- bbox_new[3] - (0.075 * xrange) 
bbox_new[2] <- bbox_new[2] + (0.15 * yrange) 
bbox_new <- bbox_new %>%  
  st_as_sfc()

#Version of fig 3 showing raw scores pre-residualization
pdf("plots/fig3_raw_scores.pdf",height=20,width=30)
tm_shape(countries,bbox=bbox_new) +
  tm_fill("gray10") +
  tm_borders("black") +
  tm_shape(roads) + tm_lines("gray40") +
  tm_shape(landborders) + tm_lines(col="xi",palette=rev(brewer.pal(5,"RdYlGn")),lwd=2,title.col="Border Dyad Scores") +
  tm_shape(data_cross,projection="longlat")+tm_dots(col="theta",palette=rev(brewer.pal(5,"RdYlGn")),
                                                    border.col="black",border.lwd=2,size=.75,shape=21,title="Border Crossing Scores") +
  tm_layout(legend.title.size = 1.5,
            legend.text.size = 1.25)
dev.off()

#Fig 3 from main text (residualized)
#for viz, drop out crossings with missing values (often driven by missing elevation and/or GDP)
data_cross<-data_cross[!is.na(data_cross$resid),]
pdf("plots/fig3.pdf",height=20,width=30)
tm_shape(countries,bbox=bbox_new) +
  tm_fill("gray10") +
  tm_borders("black") +
  tm_shape(roads) + tm_lines("gray40") +
  tm_shape(landborders) + tm_lines(col="resid",palette=rev(brewer.pal(5,"RdYlGn")),lwd=2,title.col="Border Dyad Scores") +
  tm_shape(data_cross,projection="longlat")+tm_dots(col="resid",palette=rev(brewer.pal(5,"RdYlGn")),
                                                    border.col="black",border.lwd=2,size=.75,shape=21,title="Border Crossing Scores") +
  tm_layout(legend.title.size = 1.5,
            legend.text.size = 1.25)
dev.off()



#####
#Figure 4: Border Orientation Across Time
#####
#Generate categorical indicators by which crossing scores will be compared
data<-arrange(data,borderid,year)
data$dem<-ifelse(data$polity2_1>=6,1,0)
data$aut<-ifelse(data$polity2_1<=-6,1,0)

data<- data %>% 
  group_by(state1) %>%
  mutate(dem_max = max(dem,na.rm=T)) %>% 
  mutate(dem_min = min(dem,na.rm=T)) %>% 
  mutate(aut_max = max(aut,na.rm=T)) %>% 
  mutate(aut_min = min(aut,na.rm=T)) 
data$always_aut<-ifelse(data$aut_max==1 & data$aut_min==1,1,0)
data$always_dem<-ifelse(data$dem_max==1 & data$dem_min==1,1,0)

data$region<-countrycode(data$state1,origin='cown',destination='region23')
table(data$region)
data$europe<-ifelse(data$region=="Southern Europe" | data$region=="Southern Europe" | 
                      data$region=="Eastern Europe" | data$region=="Northern Europe" ,1,0)
data$mideast<-ifelse(data$country1== 'Algeria' | 
                       data$country1== 'Jordan' | 
                       data$country1=='Qatar' | 
                       data$country1== 'Bahrain' | 
                       data$country1== 'Kuwait' | 
                       data$country1== 'Saudi Arabia' | 
                       data$country1== 'Djibouti' | 
                       data$country1== 'Lebanon' | 
                       data$country1== 'Syrian' | 
                       data$country1=='Egypt' | 
                       data$country1=='Libya' | 
                       data$country1=='Tunisia' | 
                       data$country1== 'Iran'| 
                       data$country1=='Malta' | 
                       data$country1=='United Arab' | 
                       data$country1== 'Iraq' | 
                       data$country1=='Morocco' | 
                       data$country1=='West Bank' | 
                       data$country1=='Gaza Strip' |   
                       data$country1=='Israel' |   
                       data$country1=='Oman' | 
                       data$country1=='Yemen' | 
                       data$country1=='yemen' 
                     ,1,0)
data$northam<-ifelse(data$region=="Northern America" |
                       data$region=="Central America" ,1,0)
data$africa<-ifelse(data$region=="Eastern Africa" | 
                      data$region=="Middle Africa" | 
                      data$region=="Southern Africa" | 
                      data$region=="Western Africa"
                    ,1,0)
data$asia<-ifelse((data$region=="Central Asia" | 
                     data$region=="Eastern Asia" | 
                     data$region=="South-Eastern Asia" | 
                     data$region=="Southern Asia" |
                     data$region=="Western Asia" ) &
                    data$mideast==0
                  ,1,0)
data$southam<-ifelse(data$region=="South America" ,1,0)
data$schengen1<-0
data$schengen1[data$country1=="Austria" | 
                 data$country1=="Belgium" | 
                 data$country1=="Czech Republic" | 
                 data$country1=="Denmark" | 
                 data$country1=="Estonia" | 
                 data$country1=="Finland" | 
                 data$country1=="France" | 
                 data$country1=="Germany" | 
                 data$country1=="Greece" | 
                 data$country1=="Hungary" | 
                 data$country1=="Iceland" | 
                 data$country1=="Italy" | 
                 data$country1=="Latvia" | 
                 data$country1=="Liechtenstein" | 
                 data$country1=="Lithuania" | 
                 data$country1=="Luxembourg" | 
                 data$country1=="Malta" | 
                 data$country1=="Netherlands" | 
                 data$country1=="Norway" | 
                 data$country1=="Poland" | 
                 data$country1=="Portugal" | 
                 data$country1=="Slovakia" | 
                 data$country1=="Slovenia" | 
                 data$country1=="Spain" | 
                 data$country1=="Sweden" | 
                 data$country1=="Switzerland"]<-1
data$schengen2<-0
data$schengen2[data$country2=="Austria" | 
                 data$country2=="Belgium" | 
                 data$country2=="Czech Republic" | 
                 data$country2=="Denmark" | 
                 data$country2=="Estonia" | 
                 data$country2=="Finland" | 
                 data$country2=="France" | 
                 data$country2=="Germany" | 
                 data$country2=="Greece" | 
                 data$country2=="Hungary" | 
                 data$country2=="Iceland" | 
                 data$country2=="Italy" | 
                 data$country2=="Latvia" | 
                 data$country2=="Liechtenstein" | 
                 data$country2=="Lithuania" | 
                 data$country2=="Luxembourg" | 
                 data$country2=="Malta" | 
                 data$country2=="Netherlands" | 
                 data$country2=="Norway" | 
                 data$country2=="Poland" | 
                 data$country2=="Portugal" | 
                 data$country2=="Slovakia" | 
                 data$country2=="Slovenia" | 
                 data$country2=="Spain" | 
                 data$country2=="Sweden" | 
                 data$country2=="Switzerland"]<-1
data$schengen_inner<-0
data$schengen_inner[data$schengen1==1 & data$schengen2==1]<-1
data$schengen_outer<-0
data$schengen_outer[(data$schengen1==1 & data$schengen2==0) | 
                      (data$schengen1==0 & data$schengen2==1) ]<-1
data<-arrange(data,state1,state2,year,borderid)

#Residualize the posterior draws
data_reduced<-as.data.frame(data[!is.na(data$lngdp1) & !is.na(data$elevation),])
data_residualized<-data[!is.na(data$lngdp1) & !is.na(data$elevation),]
for(ii in 57:558){
  fit<-lm(data_reduced[,ii]~data_reduced$elevation+data_reduced$lngdp1,na.action = na.exclude)
  data_residualized[,ii]<-residuals<-residuals(fit)
}
data<-as.data.frame(data_residualized)
europe_means<-matrix(NA,nrow=500,19)
mideast_means<-matrix(NA,nrow=500,19)
northam_means<-matrix(NA,nrow=500,19)
africa_means<-matrix(NA,nrow=500,19)
asia_means<-matrix(NA,nrow=500,19)
southam_means<-matrix(NA,nrow=500,19)
for(ii in 1:500){
  for(jj in 1:19){
    europe_means[ii,jj]<-mean(data[data$europe==1 & data$year==1999+jj,56+ii],na.rm=T)
    mideast_means[ii,jj]<-mean(data[data$mideast==1 & data$year==1999+jj,56+ii],na.rm=T)
    northam_means[ii,jj]<-mean(data[data$northam==1 & data$year==1999+jj,56+ii],na.rm=T)
    africa_means[ii,jj]<-mean(data[data$africa==1 & data$year==1999+jj,56+ii],na.rm=T)
    southam_means[ii,jj]<-mean(data[data$southam==1 & data$year==1999+jj,56+ii],na.rm=T)
    asia_means[ii,jj]<-mean(data[data$asia==1 & data$year==1999+jj,56+ii],na.rm=T)
  }}
sch_inner<-matrix(NA,nrow=500,19)
sch_outer<-matrix(NA,nrow=500,19)
for(ii in 1:500){
  for(jj in 1:19){
    sch_inner[ii,jj]<-mean(data[data$schengen_inner==1 & data$year==1999+jj,56+ii],na.rm=T)
    sch_outer[ii,jj]<-mean(data[data$schengen_outer==1 & data$year==1999+jj,56+ii],na.rm=T)
  }}
us_can<-matrix(NA,nrow=500,19)
us_mex<-matrix(NA,nrow=500,19)
mex_us<-matrix(NA,nrow=500,19)
can_us<-matrix(NA,nrow=500,19)
for(ii in 1:500){
  for(jj in 1:19){
    us_can[ii,jj]<-mean(data[data$state1==2 & data$state2==20 & data$year==1999+jj,56+ii],na.rm=T)
    us_mex[ii,jj]<-mean(data[data$state1==2 & data$state2==70 & data$year==1999+jj,56+ii],na.rm=T)
    mex_us[ii,jj]<-mean(data[data$state1==70 & data$state2==2 & data$year==1999+jj,56+ii],na.rm=T)
    can_us[ii,jj]<-mean(data[data$state1==20 & data$state2==2 & data$year==1999+jj,56+ii],na.rm=T)
  }}
dem_mean<-matrix(NA,nrow=500,18)
aut_mean<-matrix(NA,nrow=500,18)
for(ii in 1:500){
  for(jj in 1:18){
    dem_mean[ii,jj]<-mean(data[data$always_dem==1 & data$year==1999+jj,56+ii],na.rm=T)
    aut_mean[ii,jj]<-mean(data[data$always_aut==1 & data$year==1999+jj,56+ii],na.rm=T)
  }}

####
#Figure 4 
####
pdf("plots/fig4.pdf",height=9.5,width=10)
par(mfrow=c(2,2),mar=c(4,5,3,7),oma=c(0,0,0,0))
plot(NULL,# create empty plot
     xlim = c(2000,2018), 
     ylim = c(-1,2.75), 
     axes = F, xlab = NA, ylab = NA) 
grid()
lines(2000:2018,apply(mideast_means,2,mean),  col=alpha("goldenrod3",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(mideast_means,2,quantile, 0.975), rev(apply(mideast_means,2,quantile, 0.025)))
        ,col=alpha("goldenrod3",.4), border=NA )
lines(2000:2018,apply(northam_means,2,mean),  col=alpha("red",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(northam_means,2,quantile, 0.975), rev(apply(northam_means,2,quantile, 0.025)))
        ,col=alpha("red",.4), border=NA )
lines(2000:2018,apply(africa_means,2,mean),  col=alpha("brown",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(africa_means,2,quantile, 0.975), rev(apply(africa_means,2,quantile, 0.025)))
        ,col=alpha("brown",.4), border=NA )
lines(2000:2018,apply(southam_means,2,mean),  col=alpha("forestgreen",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(southam_means,2,quantile, 0.975), rev(apply(southam_means,2,quantile, 0.025)))
        ,col=alpha("green",.4), border=NA )
lines(2000:2018,apply(asia_means,2,mean),  col=alpha("purple",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(asia_means,2,quantile, 0.975), rev(apply(asia_means,2,quantile, 0.025)))
        ,col=alpha("purple",.3), border=NA )
lines(2000:2018,apply(europe_means,2,mean),  col=alpha("#313695",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(europe_means,2,quantile, 0.975), rev(apply(europe_means,2,quantile, 0.025)))
        ,col=alpha("#abd9e9",.4), border=NA )
mtext("Europe",side=4,at=max(apply(europe_means,2,mean)-.12),las=2,col="#313695")
mtext("Africa",side=4,at=max(apply(africa_means,2,mean)),las=2,col="brown")
mtext("MENA",side=4,at=max(apply(mideast_means,2,mean)),las=2,col="goldenrod3")
mtext("N. America",side=4,at=max(apply(northam_means,2,mean)),las=2,col="red")
mtext("Asia",side=4,at=max(apply(asia_means,2,mean)),las=2,col="purple")
mtext("S. America",side=4,at=max(apply(southam_means+.11,2,mean)),las=2,col="forestgreen")
axis(1,cex.axis=1)
axis(2,cex.axis=1)
mtext("Year",side=1,line=2.25)
mtext("Border Orientation",side=2,line=2.25)
title("Region")
box()
plot(NULL,# create empty plot
     xlim = c(2000,2018), 
     ylim = c(-1,2.75), 
     axes = F, xlab = NA, ylab = NA) 
grid()
lines(2000:2018,apply(mex_us,2,mean),  col=alpha("forestgreen",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(mex_us,2,quantile, 0.975), rev(apply(mex_us,2,quantile, 0.025)))
        ,col=alpha("forestgreen",.2), border=NA )
lines(2000:2018,apply(us_mex,2,mean),  col=alpha("steelblue4",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(us_mex,2,quantile, 0.975), rev(apply(us_mex,2,quantile, 0.025)))
        ,col=alpha("steelblue4",.4), border=NA )
lines(2000:2018,apply(can_us,2,mean),  col=alpha("red",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(can_us,2,quantile, 0.975), rev(apply(can_us,2,quantile, 0.025)))
        ,col=alpha("red",.2), border=NA )
lines(2000:2018,apply(us_can,2,mean),  col=alpha("steelblue1",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(us_can,2,quantile, 0.975), rev(apply(us_can,2,quantile, 0.025)))
        ,col=alpha("steelblue1",.2), border=NA )
mtext(expression(CAN %->% USA),side=4,line=.5,at=max(apply(can_us,2,mean)-.04),las=2,col="red")
mtext(expression(USA %->% CAN),side=4,line=.5,at=max(apply(us_can,2,mean)),las=2,col="steelblue1")
mtext(expression(USA %->% MEX),side=4,line=.5,at=max(apply(us_mex,2,mean)),las=2,col="steelblue4")
mtext(expression(MEX %->% USA),side=4,line=.5,at=max(apply(mex_us,2,mean)),las=2,col="forestgreen")
axis(1,cex.axis=1)
axis(2,cex.axis=1)
mtext("Year",side=1,line=2.25)
mtext("Border Orientation",side=2.25,line=2)
box()
title("North America")

plot(NULL,# create empty plot
     xlim = c(2000,2018), 
     ylim = c(-1,1.5), 
     axes = F, xlab = NA, ylab = NA) 
grid()
lines(2000:2018,apply(sch_inner,2,mean),  col=alpha("steelblue1",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(sch_inner,2,quantile, 0.975), rev(apply(sch_inner,2,quantile, 0.025)))
        ,col=alpha("steelblue1",.2), border=NA )
lines(2000:2018,apply(sch_outer,2,mean),  col=alpha("red",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2018,2018:2000),c(apply(sch_outer,2,quantile, 0.975), rev(apply(sch_outer,2,quantile, 0.025)))
        ,col=alpha("red",.2), border=NA )
mtext("Schengen \nInterior",side=4,line=.5,at=min(apply(sch_inner,2,mean)),las=2,col="steelblue1")
mtext("Schengen \nExterior",side=4,line=.5,at=max(apply(sch_outer,2,mean)),las=2,col="red")
axis(1,cex.axis=1)
axis(2,cex.axis=1)
mtext("Year",side=1,line=2.25)
mtext("Border Orientation",side=2.25,line=2)
box()
title("The Schengen Zone")

plot(NULL,# create empty plot
     xlim = c(2000,2017), 
     ylim = c(-.3,.9), 
     axes = F, xlab = NA, ylab = NA) 
grid()
lines(2000:2017,apply(aut_mean,2,mean),  col=alpha("red",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2017,2017:2000),c(apply(aut_mean,2,quantile, 0.975), rev(apply(aut_mean,2,quantile, 0.025)))
        ,col=alpha("red",.4), border=NA )
lines(2000:2017,apply(dem_mean,2,mean),  col=alpha("#313695",1), cex=1, pch=21,lwd=1.5 )
polygon(c(2000:2017,2017:2000),c(apply(dem_mean,2,quantile, 0.975), rev(apply(dem_mean,2,quantile, 0.025)))
        ,col=alpha("#abd9e9",.4), border=NA )
axis(1,cex.axis=1)
axis(2,cex.axis=1)
mtext("Year",side=1,line=2.25)
mtext("Border Orientation",side=2.25,line=2)
title("Regime Type")
mtext("Democracies",side=4,at=max(apply(dem_mean,2,mean)),las=2,col="#313695")
mtext("Autocracies",side=4,at=max(apply(aut_mean,2,mean)),las=2,col="red")
box()
dev.off()


####
#Figure 5 (Populism)
####
#re-open original regression data
data<-read.csv('regression/regression_data.csv')
data2<- data %>% 
  group_by(country1) %>%
  mutate(pop = mean(pop,na.rm=T)) %>% 
  mutate(bo = mean(xi,na.rm=T)) %>% 
  dplyr::select(country1,pop,bo) %>% 
  unique()
names<-as.character(data2$country1)
names[ names!="Venezuela" &  
         names!="Ecuador" &  
         names!="Bolivia" &  
         names!="Turkey" &  
         names!="Nicaragua" &  
         names!="United States" &  
         names!="Italy" &  
         names!="Sweden" &  
         names!="Germany" &  
         names!="India" &  
         names!="Spain"  ]<-""

pdf("plots/fig5.pdf",height=7.5,width=8)
par(mar=c(4,4,3,3),  oma = c(0,0,0,0),mfrow=c(1,1))
plot(NULL,# create empty plot
     xlim = c(-0.1,1.85), 
     ylim = c(-1.75,1.75), 
     axes = F, xlab = NA, ylab = NA)  
grid()
abline(h=mean(data2$bo[!is.na(data2$pop)]), col="steelblue",lty=2, lwd=1)
abline(v=mean(data2$pop[!is.na(data2$pop)]), col="steelblue",lty=2, lwd=1)
box()
reg<-lm(bo~pop, data=data2)
abline(reg, col="firebrick",lwd=1.25 )
text(data2$pop,
     data2$bo,
     labels=names,pos=3,col="gray70",cex=1)
points(data2$pop,data2$bo,bg=alpha("gray80",.4), col=alpha("black",1),lwd=1,cex=.7, pch=21 )
axis(1)
axis(2)
mtext(side=1,"Populism, Country Mean",line=2)
mtext(side=2,"Border Orientation",line=2.5)
box()
dev.off()

#####
#Figure A1: Evidence of Discriminant Validity
#####
#Correlation coefficients reported in the "Validation" section of the ms. 
#Ease of trade across borders
#Collapse down to state year (suffixing temp data sets for discriminant validity "_discv")
data_discv <- as.data.frame(data %>%
                           group_by(state1) %>%
                           mutate(Trading.Across.Borders.rank = mean(Trading.across.Borders.rank))  %>%
                           mutate(xi = mean(xi)))
data_discv<-unique(dplyr::select(data_discv,state1,country1,
                              Trading.Across.Borders.rank,
                              xi))
names<-as.character(data_discv$country1)
names[ names!="Albania" &  
         names!="Russia" &  
         names!="China" &  
         names!="Brazil" &  
         names!="Belarus" &  
         names!="United States" &  
         names!="Canada" &  
         names!="South Africa" &  
         names!="Norway" &  
         names!="France" &  
         names!="Ireland" &  
         names!="Somalia" &  
         names!="Qatar" &  
         names!="Congo" &  
         names!="Andorra" &  
         names!="Nigeria"]<-""

#Start plot
pdf("plots/figA_1.pdf",height=9.5,width=10)
par(mar=c(4,5,3,3),  oma = c(.5,0,.5,0),mfrow=c(1,1))
layout(matrix(c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,5,5), 4, 4, byrow = TRUE))
plot(NULL,
     ylim = c(-1.5,1.5), 
     xlim = rev(c(-5,190)), 
     axes = F, xlab = NA, ylab = NA)
grid()
reg<-lm(xi~Trading.Across.Borders.rank, data=data_discv)
abline(reg, col="#d73027",lwd=1.5)
points(data_discv$Trading.Across.Borders.rank,data_discv$xi,
       bg=alpha("#74add1",.3),col=alpha("#313695",.3), cex=1, pch=21,lwd=1.5)
axis(side=2, las=1, cex.axis=1.25)
axis(side=1, las=1,at=c(1,25,50,75,100,125,150,175),cex.axis=1)
mtext("Border Orientation ", side = 2,line=3.25, cex=1) 
mtext("Ease of Trade Across Borders (Rank)", side = 1, line=2.5,cex=1) 
arrows( -17.7,1.28,-17.75,1.5, xpd = TRUE, lwd=1.15, length=.1)
mtext("High Filtering",line=0.33,side=4,at=.85,col=,cex=1)
arrows(-17.7,-1.28,-17.75,-1.5, xpd = TRUE, lwd=1.15, length=.1)
mtext("Low Filtering",line=0.33,side=4,at=-.85,col=,cex=1)
title("")
text(data_discv$Trading.Across.Borders.rank,data_discv$xi,labels=names,pos=3,col="#313695",cex=1.2)
box()
text(165, 1.45,  
     bquote(rho==.(round(-1*cor(
       data_discv$Trading.Across.Borders.ran,
       data_discv$xi,method="spearman",
       use="pairwise.complete.obs"),3)
     ))
     ,cex=1.5,col="#d73027")


#GDP
data_discv <- as.data.frame(data %>%
                           group_by(state1) %>%
                           mutate(lngdp1 = mean(lngdp1,na.rm=T))  %>%
                           mutate(xi = mean(xi)))

data_discv<-unique(select(data_discv,state1,country1,
                       lngdp1,
                       xi))

names<-as.character(data_discv$country1)
names[ names!="Albania" &  
         names!="Kazakhstan" &  
         names!="China" &  
         names!="Saudi Arabia" &  
         names!="Russia" &  
         names!="Costa Rica" &  
         names!="Brazil" &  
         names!="United States" &  
         names!="Norway" &  
         names!="Kuwait" &  
         names!="Ireland" &  
         names!="Somalia" &  
         names!="Sweden" &  
         names!="Afghanistan" &
         names!="Malawi" &  
         names!="Romania" &  
         names!="Belgium" &  
         names!="Argentina" &
         names!="Armenia" &
         names!="Malaysia" &
         names!="Myanmar" &
         names!="Thailand" &
         names!="Pakistan" &
         names!="Lesotho" &
         names!="United Kingdom" &
         names!="Nigeria"]<-""
plot(NULL,
     ylim = c(-1.5,1.5), 
     xlim = c(5.28,11.7), 
     axes = F, xlab = NA, ylab = NA)
grid()
reg<-lm(xi~lngdp1, data=data_discv)
abline(reg, col="#d73027",lwd=1.5)
points(data_discv$lngdp1,data_discv$xi,
       bg=alpha("#74add1",.3),col=alpha("#313695",.3), cex=1, pch=21,lwd=1.5)
cor(data_discv$lngdp1,data_discv$xi,use="pairwise.complete.obs")
axis(side=1, las=1, cex.axis=1.25)
axis(side=2, las=1, cex.axis=1.25)
mtext("Border Orientation ", side = 2,line=3.25, cex=1) 
mtext("Logged GDP Per Capita", side = 1, line=2.5,cex=1) 
text(data_discv$lngdp1,data_discv$xi,labels=names,pos=3,col="#313695",cex=1.2)
box()
arrows( 12.14,1.28,12.14,1.5, xpd = TRUE, lwd=1.15, length=.1)
mtext("High Filtering",line=0.33,side=4,at=.85,col=,cex=1)
arrows(12.14,-1.28,12.14,-1.5, xpd = TRUE, lwd=1.15, length=.1)
mtext("Low Filtering",line=0.33,side=4,at=-.85,col=,cex=1)
text(6, 1.4,  
     bquote(rho==.(round(cor(
       data_discv$lngdp1,
       data_discv$xi,method="spearman",
       use="pairwise.complete.obs"),3)
     ))
     ,cex=1.5,col="#d73027")


#CINC
#Note: CINC is not in regression data, so pulling directly
cinc<-read_dta("regression/NMC_5_0/NMC_5_0.dta")
cinc<-subset(cinc,year==2012)
cinc$cinc<-log(cinc$cinc)
data_discv<-merge(data,cinc,by.x='state1',by.y='ccode',all.x=T,all.y=F)
#Collapse down to state year
data_discv <- as.data.frame(data_discv %>%
                           group_by(state1) %>%
                           mutate(cinc2 = mean(cinc,na.rm=T))  %>%
                           mutate(xi = mean(xi)))
data_discv<-unique(select(data_discv,country1,cinc,xi))
names<-as.character(data_discv$country1)
names[ names!="Iran" &  
         names!="Russia" &  
         names!="China" &  
         names!="Brazil" &  
         names!="Belarus" &  
         names!="United States" &  
         names!="South Africa" & 
         names!="Swaziland" &  
         names!="Moldova" &  
         names!="Oman" &  
         names!="Norway" &  
         names!="Rwanda" &  
         names!="Finland" &  
         names!="France" &  
         names!="Ireland" &  
         names!="Somalia" &  
         names!="Qatar" &  
         names!="Congo" &  
         names!="Andorra" ]<-""

plot(NULL,
     ylim = c(-1.5,1.5), 
     xlim = c(-13.5,-.75), 
     axes = F, xlab = NA, ylab = NA)
grid()
reg<-lm(xi~cinc, data=data_discv)
abline(reg, col="#d73027",lwd=1.5)
points(data_discv$cinc,data_discv$xi,
       bg=alpha("#74add1",.3),col=alpha("#313695",.3), cex=1, pch=21,lwd=1.5)
axis(side=2, las=1, cex.axis=1.25)
axis(side=1, las=1,cex.axis=1.25)
mtext("Border Orientation ", side = 2,line=3.25, cex=1) 
mtext("Logged Capabilities (CINC)", side = 1, line=2.5,cex=1) 
arrows( .1,1.27,.1,1.5, xpd = TRUE, lwd=1.15, length=.1)
mtext("High Filtering",line=0.33,side=4,at=.85,col=,cex=1)
arrows(.1,-1.27,.1,-1.5, xpd = TRUE, lwd=1.15, length=.1)
mtext("Low Filtering",line=0.33,side=4,at=-.85,col=,cex=1)
text(data_discv$cinc,data_discv$xi,labels=names,pos=3,col="#313695",cex=1.2)
box()
text(-12, 1.4,  
     bquote(rho==.(round(cor(
       data_discv$cinc,
       data_discv$xi,method="spearman",
       use="pairwise.complete.obs"),3)
     ))
     ,cex=1.5,col="#d73027")

#Visa
data_discv <- as.data.frame(data %>%
                           group_by(state1, state2) %>%
                           mutate(visa_waiver = mean(visa_waiver,na.rm=T))  %>%
                           mutate(xi = mean(xi)))
plot(NULL,
     xlim = (c(-3,3)), 
     ylim = c(0,.6), 
     axes = F, xlab = NA, ylab = NA) 
polygon(density(na.omit(data_discv$xi[data_discv$visa_waiver==0]),bw=.2),col=alpha("#ece7f2",.8),border="black",lwd=1.5)  
axis(side=1, las=1, cex.axis=1.25,mgp=c(3,.8,0))
axis(side=2, las=2, cex.axis=1.25)
title("", cex.main=1)
mtext("Density", side = 2,line=4,cex=.75) 
abline(v=mean(na.omit(data_discv$xi[data_discv$visa_waiver==0])),lty=2,lwd=1.5,col="#d73027")
title("No Visa Waiver",cex=1)
text(-2.3, .55,  
     bquote(rho==.(round(cor(
       data_discv$visa_waiver,
       data_discv$xi,method="spearman",
       use="pairwise.complete.obs"),3)
     ))
     ,cex=1.5,col="#d73027")

plot(NULL,
     xlim = (c(-3,3)), 
     ylim = c(0,.6), 
     axes = F, xlab = NA, ylab = NA) 
polygon(density(na.omit(data_discv$xi[data_discv$visa_waiver==1]),bw=.2),col=alpha("#a6bddb",.8),border="black",lwd=1.5) 
axis(side=1, las=1, cex.axis=1.25,mgp=c(3,.8,0))
axis(side=2, las=2, cex.axis=1.25)
title("", cex.main=1)
mtext("Density", side = 2,line=4,cex=.75) 
title("Visa Waiver",cex=1)
abline(v=mean(na.omit(data_discv$xi[data_discv$visa_waiver==1])),lty=2,lwd=1.5,col="#d73027")
mtext("Border Orientation", side = 1, line=2.25,cex=1) 
dev.off()


####
#Figure A2 (Posterior Predictive Checks)
####
rm(list=ls()) #clearing out excess
load('model/base_data_vectorized.RData')
load("model/bo_static_output.RData")

pdf("plots/FigA_2.pdf", width=8,height=10)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(4,5,3,3),
  mfrow= c(3,2)
)
NAMES<-	c("Official Buildings")
LABELS<- c("No Buildings","1 Building","2-3 Buildings", "4+ Buildings")

  k<-1
  k4 <- output$y_hat_k4[,which(item_k4==1)]
  sims = 1000
  sums<- matrix(NA, ncol=sims, nrow= 4, byrow=T)
  for(ii in 1:sims){
    for(b in 1:4){
      sums[b,ii]<-sum(k4[ii,]==b)
    }
  }
  
  hi <- apply(sums, 1, quantile,0.975)
  lo<-apply(sums, 1, quantile,0.025)
  mean<-apply(sums, 1, mean)
  real<-table(y_k4[item_k4==k])
  
  plot(NULL,
       ylim = c(0,max(hi)+1), 
       xlim = c(0.25,4.75), 
       axes = F, xlab = NA, ylab = NA)     
  for(m in 1:4){
    polygon(x=c(rep(m+.5,2),rep(m-.5,2)),y=c(0,real[m],real[m],0),col=alpha("#74add1",.3),border="#74add1",lwd=1.5)
    lines(c(m,m),c(hi[m], lo[m]),col="orangered", lwd=3)
  }
  axis(side=1, at=c(1,2,3,4),labels=LABELS, las=1,cex.axis=1,mgp=c(3,.6,0))
  axis(side=2, las=1, cex.axis =1)
  mtext(side = 2, "Frequency", line =3.5,cex=1) 
  mtext(NAMES[k], side=1,line=2,cex=1)
  box()

k3 <- output$y_hat_k3
sims = 1000
sums<- matrix(NA, ncol=sims, nrow= 3, byrow=T)
for(ii in 1:sims){
  for(b in 1:3){
    sums[b,ii]<-sum(k3[ii,]==b)
  }
}
hi <- apply(sums, 1, quantile,0.975)
lo<-apply(sums, 1, quantile,0.025)
mean<-apply(sums, 1, mean)
real<-table(y_k3)
plot(NULL,
     ylim = c(0,max(hi)+1), 
     xlim = c(0.25,3.75), 
     axes = F, xlab = NA, ylab = NA)     
for(m in 1:3){
  polygon(x=c(rep(m+.5,2),rep(m-.5,2)),y=c(0,real[m],real[m],0),col=alpha("#74add1",.3),border="#74add1",lwd=1.5)
  lines(c(m,m),c(hi[m], lo[m]),col="orangered", lwd=3)
}
axis(side=1, at=c(1,2,3),labels=c("No Gate","Gate","Covered Gate"), las=1,cex.axis=1,mgp=c(3,.6,0))
axis(side=2, las=1, cex.axis =1)
mtext(side = 2, "Frequency", line =3.5,cex=1) 
mtext("Gates", side=1,line=2,cex=1)
box()

k2 <- output$y_hat_k2
sims = 1000
sums<- matrix(NA, ncol=sims, nrow= 3, byrow=T)
for(ii in 1:sims){
  for(b in 1:3){
    sums[b,ii]<-sum(k2[ii,]==b)
  }
}
hi <- apply(sums, 1, quantile,0.975)
lo<-apply(sums, 1, quantile,0.025)
mean<-apply(sums, 1, mean)
real<-table(y_k2)
plot(NULL,
     ylim = c(0,max(hi)+1), 
     xlim = c(0.25,2.75), 
     axes = F, xlab = NA, ylab = NA)     
for(m in 1:2){
  polygon(x=c(rep(m+.5,2),rep(m-.5,2)),y=c(0,real[m],real[m],0),col=alpha("#74add1",.3),border="#74add1",lwd=1.5)
  lines(c(m,m),c(hi[m], lo[m]),col="orangered", lwd=3)
}
axis(side=1, at=c(1,2),labels=c("No Split-Lanes","Split-Lane"), las=1,cex.axis=1,mgp=c(3,.6,0))
axis(side=2, las=1, cex.axis =1)
mtext(side = 2, "Frequency", line =3.5,cex=1) 
mtext("Split Lanes", side=1,line=2,cex=1)
box()

kdyr <- output$y_hat_kdyr
sims = 1000
sums<- matrix(NA, ncol=sims, nrow= 3, byrow=T)
for(ii in 1:sims){
  for(b in 1:3){
    sums[b,ii]<-sum(kdyr[ii,]==b)
  }
}
hi <- apply(sums, 1, quantile,0.975)
lo<-apply(sums, 1, quantile,0.025)
mean<-apply(sums, 1, mean)
real<-table(y_kdyr)

plot(NULL,
     ylim = c(0,max(hi)+1), 
     xlim = c(0.25,2.75), 
     axes = F, xlab = NA, ylab = NA)     
for(m in 1:2){
  polygon(x=c(rep(m+.5,2),rep(m-.5,2)),y=c(0,real[m],real[m],0),col=alpha("#74add1",.3),border="#74add1",lwd=1.5)
  lines(c(m,m),c(hi[m], lo[m]),col="orangered", lwd=3)
}
axis(side=1, at=c(1,2),labels=c("No Wall","Wall"), las=1,cex.axis=1,mgp=c(3,.6,0))
axis(side=2, las=1, cex.axis =1)
mtext(side = 2, "Frequency", line =3.5,cex=1) 
mtext("Border Walls", side=1,line=2,cex=1)
box()

kdy <- output$y_hat_kdy
sims = 1000
sums<- matrix(NA, ncol=sims, nrow= 5, byrow=T)
for(ii in 1:sims){
  for(b in 1:5){
    sums[b,ii]<-sum(kdy[ii,]==b)
  }
}
hi <- apply(sums, 1, quantile,0.975)
lo<-apply(sums, 1, quantile,0.025)
mean<-apply(sums, 1, mean)
real<-table(y_kdy)

plot(NULL,
     ylim = c(0,max(hi)+1), 
     xlim = c(0.25,5.75), 
     axes = F, xlab = NA, ylab = NA)     
for(m in 1:5){
  polygon(x=c(rep(m+.5,2),rep(m-.5,2)),y=c(0,real[m],real[m],0),col=alpha("#74add1",.3),border="#74add1",lwd=1.5)
  lines(c(m,m),c(hi[m], lo[m]),col="orangered", lwd=3)
}
axis(side=1, at=c(1,2,3,4,5), las=1,cex.axis=1,mgp=c(3,.6,0))
axis(side=2, las=1, cex.axis =1)
mtext(side = 2, "Frequency", line =3.5,cex=1) 
mtext("Border Police Index", side=1,line=2,cex=1)
box()
dev.off()




