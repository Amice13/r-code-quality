#################################################################################################
#This file replicates the tables and figures in "Do Officer-Involved Shootings Reduce 
#Citizen Contact with Government?" by Cohen, Elisha, Anna Gunderson, Kaylyn Jackson, 
#Paul Zachary, Tom S. Clark, Adam Glynn, and Michael Leo Owens, forthcoming in the Journal of Politics.

#The file first reproduces the figures in the paper, followed by the tables, and appendix materials.
#################################################################################################
library(tidyverse)
library(lfe)
library(lubridate)
library(stargazer)
library(plyr)
library(dplyr)
library(ggmap)

#################################################################################################
#Figures -------
#################################################################################################
# To make the following figures go to the registration page and get an API key form Google
# https://cloud.google.com/maps-platform/#get-started
#Figure 1 -------
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

latable <- readRDS("LA_OIS_311calls.RDS") 

trauma <- read.csv("trauma-clean.csv")

trauma <- trauma[which(trauma$level==1),]

# Replace this with your key
register_google(key="key_goes_here")

la <- get_map(location = "Los Angeles", maptype = "toner-lite")  #Note: This command may take several tries
                                                                #If the API is overloaded

latable$Outcome <- ifelse(latable$civilian_fatality=="Fatal","Fatal",
                          ifelse(latable$civilian_fatality=="Non-Fatal","Non-Fatal",NA))
latable <- latable %>% drop_na(Outcome)

trauma$Outcome <- "Trauma Center"

latable <- select(latable,Lat,Long,Outcome)
trauma <- select(trauma,Lat,Long,Outcome)

LAshootingsandtraumacenters <- as.data.frame(rbind.fill(latable,trauma))
LAshootingsandtraumacenters$Outcome <- as.factor(LAshootingsandtraumacenters$Outcome)

ggmap(la)+
  geom_point(aes(x = Long, y = Lat,colour=Outcome,shape=Outcome,size=Outcome), 
             data = LAshootingsandtraumacenters) +ditch_the_axes+ 
  scale_shape_manual(values=c(19,19,17))+  
  scale_color_manual(values=c('grey41','darkgray', 'black'))+
  scale_size_manual(values=c(2,2,3))+  theme(legend.title=element_blank())+
  ggtitle("Los Angeles Fatal and Non-Fatal Officer-Involved \n Shootings and Level I 
          Trauma Centers, 2010 to 2017")
#ggsave("LAfatalgrayscaleandlevelonetrauma.pdf")

#Figure 2 ---------
#911 and 311 calls
LA.calls.311 <- read.csv('data/la_311_daily_calls_by_RD.csv')
LA.calls.911 <- read.csv('data/la_911_daily_calls_by_RD.csv')

LA.calls.311.collapsed <- LA.calls.311 %>%
  group_by(Date) %>%
  dplyr::summarize(total = sum(daily_311_calls))

LA.calls.911.collapsed <- LA.calls.911 %>%
  group_by(date) %>%
  dplyr::summarize(total = sum(daily_911_calls))

ggplot(LA.calls.911.collapsed, aes(x=as.Date(date), y=total, group=1)) + geom_line() +
  theme_bw() + 
  ggtitle ('Daily 911 calls in Los Angeles, Jan 2011-Dec 2017') + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
  labs(x='Date',y='Total Calls') +
  xlim(as.Date('2011-01-01'),as.Date('2017-12-31')) +
  ylim(0,6000) 
#ggsave('LA911CallTimeSeries.pdf')

ggplot(LA.calls.311.collapsed, aes(x=as.Date(Date), y=total, group=1)) + geom_line() +
  theme_bw() + 
  ggtitle ('Daily 311 calls in Los Angeles, Jan 2016-Dec 2017') + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
  labs(x='Date',y='Total Calls') +
  xlim(as.Date('2011-01-01'),as.Date('2017-12-31')) +
  ylim(0,6000)
#ggsave('LA311CallTimeSeries.pdf')

dev.off()

#################################################################################################
#Tables -------
#################################################################################################
#Table 1 -------
LA.calls <- readRDS('data/LA_RD_OIS_calls.RDS')
OIS <- unique(LA.calls[,c(1,9,10)])

#Racial group proportions in sample of OIS in LA:
length(which(OIS$civilian_race_new=="White"))/nrow(OIS) #0.15
length(which(OIS$civilian_race_new=="Black"))/nrow(OIS) #0.34
length(which(OIS$civilian_race_new=="Hispanic"))/nrow(OIS) #0.48
length(which(OIS$civilian_race_new=="Asian"))/nrow(OIS) #0.02
length(which(OIS$civilian_race_new=="Unknown"))/nrow(OIS) #0.02

#Racial group proportions in LA population according to Census Bureau: 
#https://www.census.gov/quickfacts/losangelescitycalifornia

#Proportion of fatal OIS by racial group in LA sample:
length(which(OIS$civilian_fatality=="Fatal" & OIS$civilian_race_new=="White"))/
  length(which(OIS$civilian_race_new=="White")) #0.50
length(which(OIS$civilian_fatality=="Fatal" & OIS$civilian_race_new=="Black"))/
  length(which(OIS$civilian_race_new=="Black")) #0.33
length(which(OIS$civilian_fatality=="Fatal" & OIS$civilian_race_new=="Hispanic"))/
  length(which(OIS$civilian_race_new=="Hispanic")) #0.50
length(which(OIS$civilian_fatality=="Fatal" & OIS$civilian_race_new=="Asian"))/
  length(which(OIS$civilian_race_new=="Asian")) #0.60
length(which(OIS$civilian_fatality=="Fatal" & OIS$civilian_race_new=="Unknown"))/
  length(which(OIS$civilian_race_new=="Unknown")) #0.33


#Table 2 -------
LA.calls <- readRDS('data/LA_RD_OIS_calls.RDS') 

LA.calls$after.before.911 <- LA.calls$after_911calls-LA.calls$before_911calls
LA.calls$after.before.311 <- LA.calls$after_311calls-LA.calls$before_311calls
LA.calls$same.race <- ifelse(LA.calls$civilian_race_new==LA.calls$officer_race_new,1,0) 

#group by incident and calculate the proportion of fatalities and whether civilian is same race as officer
LA.collapsed <- LA.calls %>%
  group_by(id,civilian_race_new) %>%
  dplyr::summarize(fatal = mean(fatal),
            after.before.911 = mean(after.before.911),
            after.before.311 = mean(after.before.311),
            Date = mean(Date),
            trauma.dist = mean(closesttraumamiles),
            RD = median(as.numeric(Reporting_District)),
            same.race = mean(same.race)
  ) 

#Do some data editing: make Whites the reference category for civilian race
LA.collapsed$civilian_race_new <- as.factor(LA.collapsed$civilian_race_new)
LA.collapsed$civilian_race_new <- factor(LA.collapsed$civilian_race_new , levels = c("White",
                                  "Black","Hispanic","Asian","Unknown")) 

#Run regressions
fatal.one <- glm(fatal~trauma.dist, data=LA.collapsed, family=binomial(link='logit'))
fatal.two <- glm(fatal~trauma.dist+as.factor(civilian_race_new), data=LA.collapsed, family=binomial(link='logit'))
fatal.three <- glm(fatal~trauma.dist+as.factor(civilian_race_new)+(same.race==1), data=LA.collapsed, family=binomial(link='logit'))
fatal.four <- glm(fatal~trauma.dist+as.factor(civilian_race_new)*(same.race==1), data=LA.collapsed, family=binomial(link='logit'))

stargazer(fatal.one,fatal.two,fatal.three,fatal.four,
          digits = 2,
          covariate.labels = c("Distance to Trauma Center", "Black Civilian", "Hispanic Civilian",
                               "Asian Civilian", "Unknown Civilian Race", "Officer Same Race as Civilian",
                               "Black Civilian X Same Race Officer", "Hispanic Civilian X Same Race Officer",
                               "Civilian Race Unknown X Officer Same Race",
                               "Intercept"),
          keep.stat = c("aic"),
          no.space = TRUE) 


#Table 3 ------
full.calls <- readRDS('data/LA_RD_311_911_controls.RDS')
full.calls$after.before.311 <- full.calls$after_311calls-full.calls$before_311calls
full.calls$after.before.911 <- full.calls$after_911calls-full.calls$before_911calls
purecontrol311 <- lm(after.before.311~RDtype1, data = full.calls)
purecontrol311anddate <- felm(after.before.311~RDtype1|as.factor(Date)|0|0,
                              data = full.calls)
purecontrol911 <- lm(after.before.911~RDtype1, data = full.calls)
purecontrols911anddate <- felm(after.before.911~RDtype1|as.factor(Date)|0|0, data = full.calls)

stargazer(purecontrol311,purecontrol311anddate,purecontrol911,purecontrols911anddate,
          title="Effect of Fatal Police Shootings on 911 and 311 Calls Including Pure Controls",
          covariate.labels = c("Treated Reporting District"),
          dep.var.labels = c("Difference in 311 Calls 30 Days Before and After Shooting",
                             "Difference in 911 Calls 30 Days Before and After Shooting" ),
          no.space=T,df=F,label="311and911callswithpurecontrol",notes="All models are OLS.")

rm(full.calls)

#Table 4 -------
nineoneone1 <- lm(after.before.911~fatal, data=LA.collapsed)
nineoneone2 <- lm(after.before.911~fatal+as.factor(civilian_race_new), data=LA.collapsed)
nineoneone3 <- lm(after.before.911~fatal+as.factor(civilian_race_new)+trauma.dist, data=LA.collapsed)
nineoneone4 <- felm(after.before.911~fatal+as.factor(civilian_race_new)+trauma.dist|RD, data=LA.collapsed)
nineoneone5 <- felm(after.before.911~fatal+as.factor(civilian_race_new)+trauma.dist|RD+month(Date), data=LA.collapsed)
nineoneone6 <- felm(after.before.911~fatal+as.factor(civilian_race_new)+trauma.dist|RD+month(Date)+
                      year(Date), data=LA.collapsed)

threeoneone1 <- lm(after.before.311~fatal, data=LA.collapsed)
threeoneone2 <- lm(after.before.311~fatal+as.factor(civilian_race_new), data=LA.collapsed)
threeoneone3 <- lm(after.before.311~fatal+as.factor(civilian_race_new)+trauma.dist, data=LA.collapsed)
threeoneone4 <- felm(after.before.311~fatal+as.factor(civilian_race_new)+trauma.dist|RD, data=LA.collapsed)
threeoneone5 <- felm(after.before.311~fatal+as.factor(civilian_race_new)+trauma.dist|RD+month(Date), 
                     data=LA.collapsed)
threeoneone6 <- felm(after.before.311~fatal+as.factor(civilian_race_new)+trauma.dist|RD+month(Date)+
                       year(Date), data=LA.collapsed)

stargazer(nineoneone1,nineoneone2,nineoneone3,nineoneone4,nineoneone5,nineoneone6,
          threeoneone1,threeoneone2,threeoneone3,threeoneone4,threeoneone5,threeoneone6,
          title="Effect of Fatal Police Shootings on 911 and 311 Calls",
          covariate.labels = c("Fatal Shooting","Black Civilian","Hispanic Civilian","Asian Civilian",
                               "Unknown Race Civilian","Distance to Trauma Center"),
          dep.var.labels = c("Difference in 911 Calls 30 Days Before and After Shooting",
                             "Difference in 311 Calls 30 Days Before and After Shooting" ),
          no.space=T,df=F,label="311and911calls",
          float.env = "sidewaystable",notes="All models are OLS.")

#################################################################################################
#Appendix Materials -------
#################################################################################################
## Robustness check on calls from all contiguous reporting districts
LA.calls.contig <- readRDS('data/LA_RD_contiguous_OIS_calls.RDS')
LA.calls.contig$after.before.911 <- LA.calls.contig$after_911calls-LA.calls.contig$before_911calls
LA.calls.contig$after.before.311 <- LA.calls.contig$after_311calls-LA.calls.contig$before_311calls
LA.calls.contig$same.race <- ifelse(LA.calls.contig$civilian_race_new==LA.calls.contig$officer_race_new,1,0)

LA.collapsed.contig <- LA.calls.contig %>%
  group_by(id,civilian_race_new) %>%
  dplyr::summarize(fatal = mean(fatal),
            after.before.911 = mean(after.before.911),
            after.before.311 = mean(after.before.311),
            Date = mean(Date),
            trauma.dist = mean(closesttraumamiles),
            RD = median(as.numeric(Reporting_District)),
            same.race = mean(same.race)
  )

#Do some data editing: make Whites the reference category for civilian race
LA.collapsed.contig$civilian_race_new <- as.factor(LA.collapsed.contig$civilian_race_new)
LA.collapsed.contig$civilian_race_new <- factor(LA.collapsed.contig$civilian_race_new , levels = c("White",
                                                                                     "Black","Hispanic","Asian","Unknown")) 

nineoneone1con <- lm(after.before.911~fatal, data=LA.collapsed.contig)
nineoneone2con <- lm(after.before.911~fatal+as.factor(civilian_race_new), data=LA.collapsed.contig)
nineoneone3con <- lm(after.before.911~fatal+as.factor(civilian_race_new)+trauma.dist, data=LA.collapsed.contig)
nineoneone4con <- felm(after.before.911~fatal+as.factor(civilian_race_new)+trauma.dist|RD, data=LA.collapsed.contig)
nineoneone5con <- felm(after.before.911~fatal+as.factor(civilian_race_new)+trauma.dist|RD+month(Date), data=LA.collapsed.contig)
nineoneone6con <- felm(after.before.911~fatal+as.factor(civilian_race_new)+trauma.dist|RD+month(Date)+
                      year(Date), data=LA.collapsed.contig)


threeoneone1con <- lm(after.before.311~fatal, data=LA.collapsed.contig)
threeoneone2con <- lm(after.before.311~fatal+as.factor(civilian_race_new), data=LA.collapsed.contig)
threeoneone3con <- lm(after.before.311~fatal+as.factor(civilian_race_new)+trauma.dist, data=LA.collapsed.contig)
threeoneone4con <- felm(after.before.311~fatal+as.factor(civilian_race_new)+trauma.dist|RD, data=LA.collapsed.contig)
threeoneone5con <- felm(after.before.311~fatal+as.factor(civilian_race_new)+trauma.dist|RD+month(Date), 
                     data=LA.collapsed.contig)
threeoneone6con <- felm(after.before.311~fatal+as.factor(civilian_race_new)+trauma.dist|RD+month(Date)+
                       year(Date), data=LA.collapsed.contig)

stargazer(nineoneone1con,nineoneone2con,nineoneone3con,nineoneone4con,nineoneone5con,nineoneone6con,
          threeoneone1con,threeoneone2con,threeoneone3con,threeoneone4con,threeoneone5con,threeoneone6con,
          title="Effect of Fatal Police Shootings on 911 and 311 Calls Including Contiguous Reporting Districts",
          covariate.labels = c("Fatal Shooting","Black Civilian","Hispanic Civilian",
                               "Asian Civilian","Unknown Race Civilian","Distance to Trauma Center"),
          dep.var.labels = c("Difference in 911 Calls 30 Days Before and After Shooting",
                             "Difference in 311 Calls 30 Days Before and After Shooting" ),
          no.space=T,df=F,label="311and911callscontiguous",
          float.env = "sidewaystable",notes="All models are OLS. Calls are from all contiguous reporting districts")


##########