#R
#chamber analysis guatavita
#from 2021 to 2024

library(ggplot2)
library(RColorBrewer)
library(reshape)
library(lme4)
library(lmerTest)
library(lubridate)
library(gridExtra)
library(ggpubr)

std <- function(x) sd(x)/sqrt(length(x))


pcdir<-"C:/Users/jubenavides/Dropbox/papers/Guatavita fluxes/stats"
macdir<-"/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Guatavita fluxes/stats"

setwd(macdir)

fluxes<-read.csv("fluxes5.csv",header=TRUE)



# View result

################################################################################################
################################################################################################



fluxes$Date3<-as.POSIXct(fluxes$Date2,format="%m/%d/%y")
fluxes$Date4<-as.Date(fluxes$Date3)

fluxes1<-fluxes
fluxes1<-fluxes1[order(fluxes1$Date4),]

fluxes1$year<-format(as.Date(fluxes1$Date4, format="%Y-%m-%d"),"%Y")

year2<-as.numeric(fluxes1$year)




plot(fluxes1$Date4, fluxes1$WTD)

plot(fluxes1$WTD, fluxes1$CO2.Flux.umolm.2s.1.,)
plot(fluxes1$WTD, (fluxes1$CH4.Flux.nmolm.2s.1.),col=as.factor(fluxes1$Analizer),pch=(year2-2010))


plot(fluxes1$Date4, fluxes1$WTD)
points(fluxes1$Date4, fluxes1$WTD,col="red")

plot(fluxes1$Date4, fluxes1$consecutive_rain_days)


#seasons
season.start.date <- as.POSIXct(c("2021-01-01", "2021-03-01", "2021-9-01","2021-10-01","2021-11-01",
                                  "2022-03-01", "2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02", "2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21", "2024-10-06","2024-12-15",
                                  "2025-03-01"))
 
season.end.date <- as.POSIXct(c( "2021-03-01", "2021-9-01","2021-10-01","2021-11-01",
                                  "2022-03-01", "2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02", "2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21", "2024-10-06","2024-12-15",
                                  "2025-03-01","2025-07-15"))

season <- c("Dry", "Rain", "Dry", "Rain", "Dry" , "Rain", "Dry" , "Rain", "Dry" , "Rain",
      "Dry","Rain", "Dry","Rain", "Dry","Rain", "Dry","Rain")

seasons<-data.frame(season.start.date ,season.end.date ,season)
season_vec <- character(length(fluxes1$dater))

# Loop through each date
for (i in seq_along(fluxes1$Date4)) {
  current_date <- fluxes1$Date4[i]
  
  # Find row in seasons where date falls between start and end
  matched <- which(seasons$season.start.date <= current_date &
                   seasons$season.end.date >= current_date)
  
  # If match found, assign season
  if (length(matched) == 1) {
    fluxes1$season[i] <- seasons$season[matched]
  }
}

########################################################################################################################
########################################################################################################################
########################################################################################################################
#CH4


fluxesCH4<-subset(fluxes1,CH4_flux>0  )

plot(fluxesCH4$Date4, fluxesCH4$consecutive_rain_days)

write.csv(fluxesCH4,"fluxesCH4.csv")


# Add season to the data frame

flux_time_agg<-aggregate(CH4.Flux.nmolm.2s.1.~Site2+season+year,data=fluxesCH4,FUN=mean)
flux_time_sd<-aggregate(CH4.Flux.nmolm.2s.1.~Site2+season+year,data=fluxesCH4,FUN=std)
flux_time_agg$ch4_sd<-flux_time_sd$CH4.Flux.nmolm.2s.1.
#flux_time_agg$co2_sd<-flux_time_sd$CO2.Flux.umolm.2s.1.


flux_time<-ggplot(fluxesCH4,aes(x=year,y=(CH4_flux)))+theme_bw()

 flux_time1<-flux_time +geom_boxplot(aes(fill=season))+
                    scale_fill_grey("Season")+
                    scale_x_discrete("Year")+
                    scale_y_continuous(expression(paste("CH4 flux (nmolCH4 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )
flux_time1


flux_time<-ggplot(fluxesCH4,aes(x=Date4,y=(CH4_flux)))+theme_bw()

 flux_time1<-flux_time +geom_point(aes(colour=season))+
                    scale_colour_grey("Season")+
                    scale_y_continuous(expression(paste("CH4 flux (nmolCH4 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )
flux_time1





flux_raindays<-ggplot(fluxesCH4,aes(x=consecutive_rain_days,y=(CH4_flux)))+theme_bw()

 flux_raindays1<-flux_raindays +geom_point(aes(colour=season))+
                    scale_colour_grey("Season")+
                    scale_x_continuous("Days of continuous Rain (Days)")+
                    scale_y_continuous(expression(paste("CH4 flux (nmolCH4 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )
flux_raindays1


flux_rainmonth<-ggplot(fluxesCH4,aes(x=precip30,y=(CH4_flux)))+theme_bw()

 flux_rainmonth1<-flux_rainmonth +geom_point(aes(colour=season))+
                    scale_colour_grey("Season")+
                    scale_x_continuous("Precipitation last month (mm)")+
                    scale_y_continuous(expression(paste("CH4 flux (nmolCH4 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )
flux_rainmonth1


flux_WT<-ggplot(fluxesCH4,aes(x=WTD,y=(CH4_flux)))+theme_bw()

 flux_WT1<-flux_WT +geom_point(aes(colour=season))+
                    scale_colour_grey("Season")+
                    scale_x_continuous("Water table (cm)")+
                    scale_y_continuous(expression(paste("CH4 flux (nmolCH4 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )
flux_WT1



grid.arrange(flux_raindays1,flux_rainmonth1,flux_WT1)

########################################################################################################################
########################################################################################################################
#CO2 NEE-GPP-ER


#fluxes1$CO2code<-paste(fluxes1$Wcode,fluxes1$Time,sep="_")

fluxes2<-subset(fluxes1,!is.na(Time))

fluxes2$plotcode<-paste(fluxes2$Site2,fluxes2$Plot,fluxes2$Date4,fluxes2$Time,sep="_")


#fluxNEE<-subset(fluxes2,Day.Night=="Day")

fluxResp<-subset(fluxes2,Day.Night=="Night")
fluxResp$ER_flux<-fluxResp$CO2_ori


fluxGPP<-merge(fluxes2,fluxResp[,c(47,48)],by="plotcode",all=TRUE)


fluxGPP$GPP_flux<-fluxGPP$CO2.Flux.umolm.2s.1.-fluxGPP$ER_flux
#fluxGPP<-subset(fluxGPP,!is.na(year))
fluxGPP<-subset(fluxGPP,Day.Night=="Day")
#fluxGPP<-subset(fluxGPP,CO2.Flux.umolm.2s.1.<25)
#fluxGPP<-subset(fluxGPP,ER_flux<25)

fluxGPP<-fluxGPP[order(fluxGPP$Date4),]
par(mfrow=c(3,1))
plot(fluxGPP$Date4,fluxGPP$GPP_flux)
plot(fluxGPP$Date4,fluxGPP$ER_flux)
plot(fluxGPP$Date4,fluxGPP$CO2.Flux.umolm.2s.1.)
#cbind(as.Date(fluxGPP$Date4),fluxGPP$GPP_flux,fluxGPP$ER_flux,fluxGPP$CO2.Flux.umolm.2s.1.)
par(mfrow=c(1,1))


ER_time1<-ggscatter(fluxGPP, x = "Date4", y = "ER_flux", color = "season",
   palette = c("#00AFBB", "#E7B800", "#FC4E07") )
GPP_time1<-ggscatter(fluxGPP, x = "Date4", y = "GPP_flux", color = "season",
   palette = c("#00AFBB", "#E7B800", "#FC4E07") )
NEE_time1<-ggscatter(fluxGPP, x = "Date4", y = "CO2.Flux.umolm.2s.1.", color = "season",
   palette = c("#00AFBB", "#E7B800", "#FC4E07") )


grid.arrange(GPP_time1,ER_time1,NEE_time1)


ER_WT1<-ggscatter(fluxGPP, x = "WaterTable2", y = "ER_flux", color = "season",
   palette = c("#00AFBB", "#E7B800", "#FC4E07"),
   xlab= "Water table (cm)",
   ylab=expression("ER"* (mu*mol~m^{-2}~s^{-1})))



ER_WT<-ggplot(fluxGPP,aes(x=WaterTable2,y=ER_flux))+theme_bw()

 ER_WT1<-ER_WT +geom_point(aes(colour=Site2), position = "dodge2")+
                    scale_fill_grey("Season")+
                    scale_x_continuous("Water table (cm)")+
                    scale_y_continuous(expression(paste("ER (umolCO2 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )

ER_WT1



NEE_PAR<-ggplot(fluxGPP,aes(x=PAR2,y=-GPP_flux))+theme_bw()

 NEE_PAR1<-NEE_PAR +geom_point(aes(colour=Site2), position = "dodge2")+
                    scale_fill_grey("Season")+
                    scale_x_continuous("PAR radiation")+
                    scale_y_continuous(expression(paste("NEE (umolCO2 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )

NEE_PAR1


library(minpack.lm)

library(photosynthesis)


fluxGPP$PAR<-(fluxGPP$PAR1+fluxGPP$PAR2)/2

fluxGPP$GPP_flux2<-fluxGPP$GPP_flux+10

fit_dry1 <- nls(
  GPP_flux2 ~ (alpha * PAR) / (1 + (alpha * PAR / Amax)) - Rd,
  data = subset(fluxGPP,season=="Dry") ,
  start = list(alpha = 0.05, Amax = 12, Rd = 1),
  control = list(maxiter = 500)
)


fit_rain1 <- nlsLM(
  GPP_flux2 ~ (alpha * PAR) / (1 + (alpha * PAR / Amax)) - Rd,
  data = subset(fluxGPP,season=="Rain") ,
  start = list(alpha = 0.05, Amax = 10, Rd = 1),
  control = nls.lm.control(maxiter = 500)
)

plot(fluxGPP$PAR1,fluxGPP$PAR2)
abline(0,1)

fluxGPP$PAR<-(fluxGPP$PAR1+fluxGPP$PAR2)/2

# 1. Create a smooth range of PAR values for prediction
par_seq <- data.frame(PAR = seq(min(fluxGPP$PAR,na.rm=TRUE), max(fluxGPP$PAR,na.rm=TRUE), length.out = 200))

# 2. Predict fitted values
par_seq$A_fittedS1<- predict(fit_dry1, newdata = par_seq)
par_seq$A_fittedS2<- predict(fit_rain1, newdata = par_seq)


fluxGPP2 <- fluxGPP[!is.na(fluxGPP$PAR) & !is.na(fluxGPP$GPP_flux2), ]
fluxGPP2_dry<-subset(fluxGPP2,season=="Dry")
fluxGPP2_rain<-subset(fluxGPP2,season=="Rain")

# Fit one light-response curve
fit_dry <- fit_photosynthesis(
  .data = fluxGPP2_dry,
  .photo_fun = "aq_response",
  .vars = list(.A = GPP_flux2, .Q = PAR)
)


# Fit one light-response curve
fit_rain <- fit_photosynthesis(
  .data = fluxGPP2_rain,
  .photo_fun = "aq_response",
  .vars = list(.A = GPP_flux2, .Q = PAR)
)

# Use of the photosynthesis function
df_predict_st_dry <- data.frame(Qabs = seq(0, 0.84 * 3500, length.out = 100)) |>
  mutate(A = marshall_biscoe_1980(Q_abs = Qabs,k_sat = coef(fit_dry)["k_sat"],coef(fit_dry)["phi_J"],coef(fit_dry)["theta_J"]) - coef(fit_dry)["Rd"])

df_predict_st_rain <- data.frame(Qabs = seq(0, 0.84 * 3500, length.out = 100)) |>
  mutate(A = marshall_biscoe_1980(Q_abs = Qabs,k_sat = coef(fit_rain)["k_sat"],coef(fit_rain)["phi_J"],coef(fit_rain)["theta_J"]) - coef(fit_rain)["Rd"])




GPP_PAR<-ggplot(fluxGPP,aes(x=PAR,y=-GPP_flx|ux))+theme_bw()

 GPP_PAR1<-GPP_PAR +geom_point(aes(colour=season), position = "dodge2")+
                    geom_line(data= par_seq,aes(x=PAR,y=A_fittedS1),colour="#D55E00")+
                    geom_line(data= par_seq,aes(x=PAR,y=A_fittedS2),colour="#009E73")+
                    scale_colour_manual(name = "season", values = c("Dry" = "#D55E00", "Rain" = "#009E73")) +
                    scale_x_continuous("PAR radiation (umol m2 s)")+
                    scale_y_continuous(expression(paste("GPP (umolCO2 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )

GPP_PAR1

fluxGPP$Station<-ifelse(fluxGPP$Site2=="S1","Degraded","Conserved")

NEEPlot <- ggboxplot(fluxGPP, x = "season", y = "CO2.Flux.umolm.2s.1.",
                color = "Station", 
                add = "jitter", shape = "Station",
                xlab ="Season",
                legend="none")+
                scale_y_continuous(expression("NEE "*(mu*mol~m^{-2}~s^{-1})),lim=c(-20,22))+
                scale_color_manual(name = "Station", 
                    values = c("Degraded" = "#D55E00", "Conserved" = "#009E73")) 

ERPlot <- ggboxplot(fluxGPP, x = "season", y = "ER_flux",
                color = "Station", 
                add = "jitter", shape = "Station",
                xlab ="Season",
                legend=c(0.15,0.8))+
                scale_y_continuous(expression("ER "*(mu*mol~m^{-2}~s^{-1})),lim=c(0,30))+
                scale_color_manual(name = "Station", 
                    values = c("Degraded" = "#D55E00", "Conserved" = "#009E73")) 


fluxes2$Station<-ifelse(fluxes2$Site2=="S1","Degraded","Conserved")

fluxes2$CH4micromol<-fluxes2$CH4.Flux.nmolm.2s.1./1000
MetPlot <- ggboxplot(fluxes2, x = "season", y = "CH4micromol",
                color = "Station", 
                add = "jitter", shape = "Station",
                xlab ="Season",
                legend="none")+
                scale_y_continuous(expression("CH4 flux "*(mu*mol~m^{-2}~s^{-1})),lim=c(0,1))+
                scale_color_manual(name = "Station", 
                    values = c("Degraded" = "#D55E00", "Conserved" = "#009E73")) 
MetPlot

grid.arrange(NEEPlot,ERPlot,MetPlot)

fluxGPP2<-subset(fluxGPP,year>2022 & year<2025)
#mean fuxes per site and season
a<-aggregate(cbind(CO2.Flux.umolm.2s.1.,ER_flux) ~ year+season+Station,data=fluxGPP2,FUN=mean)
asd<-aggregate(cbind(CO2.Flux.umolm.2s.1.,ER_flux) ~ year+season+Station,data=fluxGPP2,FUN=std)

#ERk<-

#a$night<-a$ER_flux
#a$dayNEE<-a$CO2.Flux.umolm.2s.1.+a$night
#a$sdNEE<-(asd$CO2.Flux.umolm.2s.1.+asd$ER_flux)*a$night/2
a$sdC<-asd$CO2.Flux.umolm.2s.1.


write.csv(a,"co2flux.csv")
write.csv(fluxGPP,"co2flux_gpp.csv")


fluxes3<-subset(fluxes2,CH4.Flux.nmolm.2s.1.>0)

b<-aggregate( CH4.Flux.nmolm.2s.1.~ year+season+Station,data=fluxes3,FUN=mean)
bsd<-aggregate( CH4.Flux.nmolm.2s.1.~ year+season+Station,data=fluxes3,FUN=std)
b$sdCH4<-bsd$CH4.Flux.nmolm.2s.1.

write.csv(b,"ch4flux.csv")

met.agg<-aggregate(CH4.Flux.nmolm.2s.1. ~ season+Station, data = fluxes3, FUN = function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))

met.agg<-aggregate(CH4.Flux.nmolm.2s.1. ~ season+Station, data = fluxes3, FUN = median)

met.agg<-aggregate(CH4.Flux.nmolm.2s.1. ~ season+Station, data = fluxes3, FUN = mean)


#values for night respiration from GPP responde model
# Step 1: Identify nighttime data (e.g., PPFD < 20 µmol/m²/s)
night_idx <- which(fluxGPP$PAR < 20)

# Step 2: Fit Reco model to nighttime data (simple exponential function)
# Reco = a * exp(b * Tair)
night_T <- fluxGPP$AirTemp[night_idx]
night_NEE <- fluxGPP$ER_flux[night_idx]

fit <- nls(night_NEE ~ a * exp(b * night_T), start = list(a = 1, b = 0.05))

  fluxER<-subset(fluxGPP,ER_flux>0 & ER_flux<15 )
  #fluxER<-subset(fluxER,SoilTemp<20 )

  plot(fluxER$SoilTemp,fluxER$ER_flux, col=as.factor(fluxER$season))

  fluxER<-subset(fluxER,season=="Rain" & Site2=="S2")

  # Step 1: Convert SoilTemp to Kelvin
  fluxER$SoilTemp_K <- fluxER$SoilTemp + 273.15

  # Step 2: Define Lloyd–Taylor model
  lloyd_taylor <- function(T, Rref, E0, Tref = 288.15, T0 = 227.13) {
    Rref * exp(E0 * (1 / (Tref - T0) - 1 / (T - T0)))
  }

  # Step 3: Fit model using only rows with valid ER_flux and SoilTemp
  fit_data <- fluxER[!is.na(fluxER$ER_flux) & !is.na(fluxER$SoilTemp_K), ]

  # Step 4: Fit model using nls
  lt_fit <- nls(ER_flux ~ lloyd_taylor(SoilTemp_K, Rref, E0),
                data = fit_data,
                start = list(Rref = 2, E0 = 100),
                control = nls.control(maxiter = 100, warnOnly = TRUE))

  # Step 5: Predict Reco across all temperatures in fluxER
  fluxER$Reco_LloydTaylor <- lloyd_taylor(fluxER$SoilTemp_K,
                                          Rref = coef(lt_fit)["Rref"],
                                          E0 = coef(lt_fit)["E0"])

  # Step 6: Optional - Plot fit
  plot(fit_data$SoilTemp, fit_data$ER_flux,
       main = "Lloyd–Taylor fit to ER_flux vs SoilTemp",
       xlab = "Soil Temperature (°C)",
       ylab = "ER Flux (µmol CO2 m⁻² s⁻¹)",
       pch = 16, col = "blue")
  lines(sort(fit_data$SoilTemp),
        lloyd_taylor(sort(fit_data$SoilTemp + 273.15),
                     coef(lt_fit)["Rref"],
                     coef(lt_fit)["E0"]),
        col = "red", lwd = 2)
  legend("topleft", legend = "Lloyd–Taylor fit", col = "red", lwd = 2)

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
#Annual model with dry and wet seasons


#read env data
st1<-read.csv("St1NEEcum.csv", header=TRUE,stringsAsFactors = FALSE, check.names = FALSE)
st2<-read.csv("St2NEEcum.csv", header=TRUE,stringsAsFactors = FALSE, check.names = FALSE)
var_names<-read.csv("tower_var.csv", header=TRUE,stringsAsFactors = FALSE, check.names = FALSE)

st1mod<-st1[ ,var_names$ colname]
st2mod<-st2[ ,var_names$ colname]

#rm(st1,st2)
# Parse as Colombia time (UTC-5, no DST)
st1mod$datetime <- as.POSIXct(st1mod$DateTime, tz = "America/Bogota")
st1mod$date2 <- as.Date(st1mod$datetime)


# Time-of-day in minutes since midnight
tod_min <- as.integer(format(st1mod$datetime, "%H")) * 60 + as.integer(format(st1mod$datetime, "%M"))

# Day/Night labeling
st1mod$day_night <- ifelse(tod_min >= 6*60 & tod_min < (18*60 + 30), "day", "night")

