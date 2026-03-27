

# Script "Sustainability Challenges of Artificial Intelligence and Citizens' Regulatory Preferences"

# Version: 5 August 2023



#### Load Packages ### 

library(ggplot2)
library(texreg)
library(corrplot)
library(RColorBrewer)
library(psych)
library(FactoMineR)
library(factoextra)
library(lattice)



#### Load dataset ####

# ds.full <- read.csv("Regulatory Preferences Sustainable AI.csv", encoding="UTF-8")



#### Filter out cases by attention check and control questions ####


# ds.full <- ds.full[is.na(ds.full$REF) == F, ]


ds.full <- ds.full[ds.full$TE05_13 == 1 | ds.full$TE05_13 < 0, ] # attention check

ds.full <- ds.full[ds.full$KO01 == "Ja, ich habe den Fragebogen aufmerksam und nach bestem Wissen und Gewissen beantwortet. Meine Angaben können zur Auswertung verwendet werden." , ]





#### 1. Independent variables ####

#### 1.1 Political trust ####

table(ds.full$VT05_01)

ds.full$insttrust <- apply( ds.full[c( "VT05_01", 
                                     "VT05_02",
                                     "VT05_04",
                                     "VT05_05",
                                     "VT05_06"
                                     )], 1, mean, na.rm=TRUE)


hist(ds.full$insttrust)


#### 1.2 Trust in technology companies ####

# ds.full$VT05_10

hist(ds.full$VT05_10)




#### 1.3 Regulatory competence ####


# Economic development and innovation
ds.full$TE12_01[ds.full$TE12_01 < 0 ] <- mean(ds.full$TE12_01, na.rm = T)
ds.full$TE12_02[ds.full$TE12_02 < 0 ] <- mean(ds.full$TE12_02, na.rm = T)
ds.full$TE12_03[ds.full$TE12_03 < 0  ] <- mean(ds.full$TE12_03, na.rm = T)
ds.full$TE12_04[ds.full$TE12_04 < 0  ] <- mean(ds.full$TE12_04, na.rm = T)
ds.full$TE12_05[ds.full$TE12_05 < 0  ] <- mean(ds.full$TE12_05, na.rm = T)
ds.full$TE12_06[ds.full$TE12_06 < 0  ] <- mean(ds.full$TE12_06, na.rm = T)
ds.full$TE12_07[ds.full$TE12_07 < 0  ] <- mean(ds.full$TE12_07, na.rm = T)



# Transparency of AI 

ds.full$TE14_01[ds.full$TE14_01 < 0  ] <- mean(ds.full$TE14_01, na.rm = T)
ds.full$TE14_02[ds.full$TE14_02 < 0  ] <- mean(ds.full$TE14_02, na.rm = T)
ds.full$TE14_03[ds.full$TE14_03 < 0  ] <- mean(ds.full$TE14_03, na.rm = T)
ds.full$TE14_04[ds.full$TE14_04 < 0  ] <- mean(ds.full$TE14_04, na.rm = T)
ds.full$TE14_05[ds.full$TE14_05 < 0  ] <- mean(ds.full$TE14_05, na.rm = T)
ds.full$TE14_06[ds.full$TE14_06 < 0  ] <- mean(ds.full$TE14_06, na.rm = T)
ds.full$TE14_07[ds.full$TE14_07 < 0  ] <- mean(ds.full$TE14_07, na.rm = T)




# Energy efficiency

ds.full$TE13_01[ds.full$TE13_01 < 0  ] <- mean(ds.full$TE13_01, na.rm = T)
ds.full$TE13_02[ds.full$TE13_02 < 0  ] <- mean(ds.full$TE13_02, na.rm = T)
ds.full$TE13_03[ds.full$TE13_03 < 0  ] <- mean(ds.full$TE13_03, na.rm = T)
ds.full$TE13_04[ds.full$TE13_04 < 0 ] <- mean(ds.full$TE13_04, na.rm = T)
ds.full$TE13_05[ds.full$TE13_05 < 0  ] <- mean(ds.full$TE13_05, na.rm = T)
ds.full$TE13_06[ds.full$TE13_06 < 0  ] <- mean(ds.full$TE13_06, na.rm = T)
ds.full$TE13_07[ds.full$TE13_07 < 0 ] <- mean(ds.full$TE13_07, na.rm = T)





# Generate competence perception variables

ds.full$comp1gov <- apply( ds.full[c( "TE12_01", 
                                      "TE12_02",
                                      "TE12_03"   )], 1, mean, na.rm=TRUE)
ds.full$comp1all <- apply( ds.full[c( "TE12_01", 
                                      "TE12_02",
                                      "TE12_03",
                                      "TE12_04",
                                      "TE12_05",
                                      "TE12_06",
                                      "TE12_07"   )], 1, mean, na.rm=TRUE)

# Transparency
ds.full$comp2gov <- apply( ds.full[c( "TE14_01", 
                                      "TE14_02",
                                      "TE14_03"   )], 1, mean, na.rm=TRUE)
ds.full$comp2all <- apply( ds.full[c( "TE14_01", 
                                      "TE14_02",
                                      "TE14_03",
                                      "TE14_04",
                                      "TE14_05",
                                      "TE14_06",
                                      "TE14_07"   )], 1, mean, na.rm=TRUE)

# Energy efficiency
ds.full$comp3gov <- apply( ds.full[c( "TE13_01", 
                                      "TE13_02",
                                      "TE13_03"   )], 1, mean, na.rm=TRUE)


ds.full$comp3all <- apply( ds.full[c( "TE13_01", 
                                      "TE13_02",
                                      "TE13_03",
                                      "TE13_04",
                                      "TE13_05",
                                      "TE13_06",
                                      "TE13_07"   )], 1, mean, na.rm=TRUE)



#### 1.4 Political position ####

# market vs. state 

table(ds.full$TE08_01)
ds.full$TE08_01 <- 7 - ds.full$TE08_01 # recode to get market-liberal


# environment

table(ds.full$TE09_01)
hist(ds.full$TE09_01)

ds.full$TE09_01 <- 8 - ds.full$TE09_01


# immigration

table(ds.full$TE22_01)



#### 1.5 Environmental concern ####

ds.full$TE04_01[ds.full$TE04_01 == -1] <- NA # negative
ds.full$TE04_02[ds.full$TE04_02 == -1] <- NA # positive
ds.full$TE04_03[ds.full$TE04_03 == -1] <- NA # negative
ds.full$TE04_04[ds.full$TE04_04 == -1] <- NA # positive
ds.full$TE04_05[ds.full$TE04_05 == -1] <- NA # negative
ds.full$TE04_06[ds.full$TE04_06 == -1] <- NA # positive
ds.full$TE04_07[ds.full$TE04_07 == -1] <- NA # positive

ds.full$TE04_01rc <- 6-ds.full$TE04_01 
ds.full$TE04_03rc <- 6-ds.full$TE04_03 
ds.full$TE04_05rc <- 6-ds.full$TE04_05 

psych::alpha(ds.full[c("TE04_01rc", 
                       "TE04_02",
                       "TE04_03rc", 
                       "TE04_04",
                       "TE04_05rc",
                       "TE04_06",
                       "TE04_07") ],
             check.keys=TRUE) # Alpha is 0.81



ds.full$environ <- apply( ds.full[c("TE04_01rc", 
                                    "TE04_02",
                                    "TE04_03rc",
                                    "TE04_04",
                                    "TE04_05rc",
                                    "TE04_06",
                                    "TE04_07")], 1, mean, na.rm=TRUE)


hist(ds.full$environ)


cor(ds.full$TE09_01, ds.full$environ, use = "complete") # correlation with policy position



tmp <- ds.full[ , c("TE04_01rc", 
                    "TE04_02",
                    "TE04_03rc",
                    "TE04_04",
                    "TE04_05rc",
                    "TE04_06",
                    "TE04_07") ]
ds.full.compl <- tmp[complete.cases(tmp), ]


factanal( ds.full.compl[c("TE04_01rc", 
                          "TE04_02",
                          "TE04_03rc",
                          "TE04_04",
                          "TE04_05rc",
                          "TE04_06",
                          "TE04_07")], factors = 3, rotation = "promax"
)




factanal( ds.full.compl[c("TE04_01rc", 
                                    "TE04_03rc",
                                     "TE04_04",
                                     "TE04_05rc",
                                     "TE04_06",
                                     "TE04_07")], factors = 3, rotation = "promax"
)




fa.out1 <- factanal( ds.full.compl[c("TE04_01rc", 
                                     "TE04_03rc",
                                     "TE04_04",
                                     "TE04_05rc",
                                     "TE04_06",
                                     "TE04_07")], factors = 2, rotation = "promax"
)

unclass(loadings(fa.out1) )




ds.full$environ_eco <- apply( ds.full[c("TE04_01rc", 
                                    "TE04_03rc",
                                    "TE04_05rc")], 1, mean, na.rm=TRUE)
ds.full$environ_future <- apply( ds.full[c("TE04_04",
                                        "TE04_06",
                                        "TE04_07")], 1, mean, na.rm=TRUE)



psych::alpha(ds.full[c("TE04_01rc", 
                       "TE04_03rc", 
                       "TE04_05rc") ],
             check.keys=TRUE) # Alpha is 0.81

psych::alpha(ds.full[c("TE04_04",
                       "TE04_06",
                       "TE04_07") ],
             check.keys=TRUE) # Alpha is 0.81




#### 1.6 Desire for control ####

table(ds.full$TE05_16)

ds.full$TE05_01[ds.full$TE05_01 == -1] <- NA # negative
ds.full$TE05_03[ds.full$TE05_03 == -1] <- NA # negative
ds.full$TE05_07[ds.full$TE05_07 == -1] <- NA # negative
ds.full$TE05_08[ds.full$TE05_08 == -1] <- NA # negative
ds.full$TE05_15[ds.full$TE05_15 == -1] <- NA # 
ds.full$TE05_16[ds.full$TE05_16 == -1] <- NA


ds.full$TE05_15rc <- 6 - ds.full$TE05_15

ds.full$TE05_16rc <- 6 - ds.full$TE05_16



psych::alpha(ds.full[c("TE05_01", 
                       "TE05_03",
                       "TE05_07", 
                       "TE05_08") ],
             check.keys=TRUE) # Alpha is 0.75



cor( ds.full[c("TE05_15",
               "TE05_16") ], use = "complete")


ds.full$control <- apply( ds.full[c( "TE05_01", 
                                     "TE05_03",
                                     "TE05_07",
                                     "TE05_08")], 1, mean, na.rm=TRUE)

ds.full$control <- 6 - ds.full$control 

hist(ds.full$control)





#### 1.7 Technophobia ####


ds.full$TE23_01[ds.full$TE23_01 == -1] <- NA
ds.full$TE23_02[ds.full$TE23_02 == -1] <- NA
ds.full$TE23_03[ds.full$TE23_03 == -1] <- NA
ds.full$TE23_04[ds.full$TE23_04 == -1] <- NA
ds.full$TE23_05[ds.full$TE23_05 == -1] <- NA


table(ds.full$TE23_01)

psych::alpha(ds.full[c("TE23_01", 
                       "TE23_02",
                       "TE23_03", 
                       "TE23_04",
                       "TE23_05") ],
             check.keys=TRUE) # Alpha is 0.92




ds.full$technophob <- (ds.full$TE23_01 + ds.full$TE23_02 +ds.full$TE23_03 +ds.full$TE23_04 +ds.full$TE23_05)/5

hist(ds.full$technophob)


#### 1.8 Algorithm literacy ####

table(ds.full$KE03 )
unique( ds.full$KE03  )[1]

ds.full$algolit[ ds.full$KE03 == "Keine Kenntnisse"] <- 1
ds.full$algolit[ ds.full$KE03 == "Sehr geringe Kenntnisse"] <- 2
ds.full$algolit[ ds.full$KE03 == "Geringe Kenntnisse"] <- 3
ds.full$algolit[ ds.full$KE03 == unique( ds.full$KE03  )[1] ] <- 4
ds.full$algolit[ ds.full$KE03 == "Gute Kenntnisse"] <- 5
ds.full$algolit[ ds.full$KE03 == "Sehr gute Kenntnisse"] <- 6

table(ds.full$algolit )






#### 1.9 Political interest ####

str(ds.full$TE10)

table(ds.full$TE10)
unique(ds.full$TE10)

ds.full$polint[ ds.full$TE10 == unique(ds.full$TE10)[5] ] <- 1
ds.full$polint[ ds.full$TE10 == unique(ds.full$TE10)[2] ] <- 2
ds.full$polint[ ds.full$TE10 == unique(ds.full$TE10)[4] ] <- 3
ds.full$polint[ ds.full$TE10 == unique(ds.full$TE10)[1] ] <- 4
ds.full$polint[ ds.full$TE10 == unique(ds.full$TE10)[3] ] <- 5

table(ds.full$polint )



#### 1.10 Sociodemographics ####


# age

ds.full$DG02_01


# gender

table(ds.full$DG15)

ds.full$female <- 0
ds.full$female[ds.full$DG15 == "weiblich"] <- 1
table(ds.full$female)


# education

table(ds.full$DG03)

ds.full$education <- 0
ds.full$education[ds.full$DG03 == "Abitur bzw. erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)"] <- 1
ds.full$education[ds.full$DG03 == "Fachhochschulreife (Abschluss einer Fachoberschule etc.)"] <- 1
ds.full$education[ds.full$DG03 == "Hochschulabschluss"] <- 1
ds.full$education[ds.full$DG03 == "Promotion"] <- 1

table(ds.full$education)




#### 2. Dependent variables ####

#### 2.1 Exploratory factor analysis for transparency regulation ####

tmp <- ds.full[ , c("TE19_05",
               "TE19_01",  
               "TE19_03", 
               "TE19_02", 
               "TE19_04", 
               "TE19_06") ]
ds.full.compl <- tmp[complete.cases(tmp), ]


fa.out1 <- factanal( ds.full.compl[c("TE19_05",
                    "TE19_01",  
                    "TE19_03", 
                    "TE19_02", 
                    "TE19_04", 
                    "TE19_06")], factors = 2, rotation = "promax"
          )

fa.out1$loadings

unclass(loadings(fa.out1) )

# write.csv(unclass(loadings(fa.out1)), file = "efa.loadings_1.csv")


#### 2.2 Exploratory factor analysis for energy efficiency regulation ####

tmp <- ds.full[ , c("TE11_05",
                    "TE11_01",  
                    "TE11_03", 
                    "TE11_02", 
                    "TE11_04", 
                    "TE11_06") ]
ds.full.compl <- tmp[complete.cases(tmp), ]


fa.out2 <- factanal( ds.full.compl[c("TE11_05",
                         "TE11_01",  
                         "TE11_03", 
                         "TE11_02", 
                         "TE11_04", 
                         "TE11_06")], factors = 2, rotation = "promax"
)

fa.out2

unclass(loadings(fa.out2) )




#### 2.3 Create dependent variables ####

# Transparency

ds.full$TE19_01[ds.full$TE19_01 <  0] <- NA
ds.full$TE19_02[ds.full$TE19_02 <  0] <- NA
ds.full$TE19_03[ds.full$TE19_03 <  0] <- NA
ds.full$TE19_04[ds.full$TE19_04 <  0] <- NA
ds.full$TE19_05[ds.full$TE19_05 <  0] <- NA
ds.full$TE19_06[ds.full$TE19_06 <  0] <- NA

# energy efficiency

ds.full$TE11_01[ds.full$TE11_01 <  0] <- NA
ds.full$TE11_02[ds.full$TE11_02 <  0] <- NA
ds.full$TE11_03[ds.full$TE11_03 <  0] <- NA
ds.full$TE11_04[ds.full$TE11_04 <  0] <- NA
ds.full$TE11_05[ds.full$TE11_05 <  0] <- NA
ds.full$TE11_06[ds.full$TE11_06 <  0] <- NA



# Transparency hard regulation
ds.full$regtranshard <- apply( ds.full[c( 
                                  "TE19_02",
                                  "TE19_04",
                                  "TE19_06")], 1, mean, na.rm=TRUE)

# Transparency soft regulation
ds.full$regtranssoft <- apply( ds.full[c( "TE19_05", 
                                 "TE19_01",
                                 "TE19_03")], 1, mean, na.rm=TRUE)

# Energy efficiency hard regulation
ds.full$regenergyhard<- apply( ds.full[c( 
                                "TE11_02",
                                "TE11_04",
                                "TE11_06")], 1, mean, na.rm=TRUE)

# Energy efficiency soft regulation
ds.full$regenergysoft<- apply( ds.full[c( "TE11_05", 
                                "TE11_01",
                                "TE11_03")], 1, mean, na.rm=TRUE)


# Inspect Cronbach's Alphas

psych::alpha( ds.full[c( 
  "TE19_02",
  "TE19_04",
  "TE19_06")], check.keys=TRUE)


psych::alpha( ds.full[c( "TE19_05", 
                          "TE19_01",
                          "TE19_03")], check.keys=TRUE)


psych::alpha( ds.full[c( 
  "TE11_02",
  "TE11_04",
  "TE11_06")], check.keys=TRUE)

psych::alpha( ds.full[c( "TE11_05", 
                          "TE11_01",
                          "TE11_03")], check.keys=TRUE)






# Overall index

ds.full$regtrans <- apply( ds.full[c( "TE19_05", 
                                      "TE19_01",
                                      "TE19_03",
                                      "TE19_02",
                                      "TE19_04",
                                      "TE19_06")], 1, mean, na.rm=TRUE)


psych::alpha( ds.full[c( 
  "TE19_05", 
  "TE19_01",
  "TE19_03",
  "TE19_02",
  "TE19_04",
  "TE19_06")], check.keys=TRUE)

ds.full$regenergy <- apply( ds.full[c( "TE11_05", 
                                       "TE11_01",
                                       "TE11_03",
                                       "TE11_02",
                                       "TE11_04",
                                       "TE11_06")], 1, mean, na.rm=TRUE)

psych::alpha( ds.full[c( 
  "TE11_05", 
  "TE11_01",
  "TE11_03",
  "TE11_02",
  "TE11_04",
  "TE11_06")], check.keys=TRUE)




# Hard vs. soft regulation

ds.full$regtrans2 <- -(ds.full$TE19_05 + ds.full$TE19_01 + ds.full$TE19_03 - ds.full$TE19_02 - ds.full$TE19_04 - ds.full$TE19_06)

ds.full$regenergy2  <- -(ds.full$TE11_05 + ds.full$TE11_01 + ds.full$TE11_03 - ds.full$TE11_02 - ds.full$TE11_04 - ds.full$TE11_06)



#### 3. Inspect Dependent variables ####

#### 3.1 T-Tests for DVs ####

t.test(ds.full$regtranssoft, ds.full$regtranshard, paired = TRUE, alternative = "two.sided")
t.test(ds.full$regenergysoft, ds.full$regenergyhard, paired = TRUE, alternative = "two.sided")




#### 3.2 Histograms for dependent variables ####

desc_2 <- psych::describe(ds.full[c("regenergyhard",
                                    "regtranshard", 
                                    "regenergysoft", 
                                    "regtranssoft"
)] 
) 


p1_1 <- ggplot(ds.full, aes(x=regtranssoft)) + 
  geom_histogram(color="black", fill="white", binwidth=0.5) + 
  geom_vline(aes(xintercept= mean(ds.full$regtranssoft, na.rm = T) ),  color="grey20", linetype="dashed", size=1) + 
  theme_bw() + ylab("Count") + xlab("Soft regulation of transparency") + scale_x_continuous(breaks = seq(1, 7 , by = 1))


p1_2 <-ggplot(ds.full, aes(x=regtranshard)) + 
  geom_histogram(color="black", fill="white", binwidth=0.5) + 
  geom_vline(aes(xintercept= mean(ds.full$regtranshard, na.rm = T) ),  color="grey20", linetype="dashed", size=1) + 
  theme_bw() + ylab("Count") + xlab("Hard regulation of transparency") + scale_x_continuous(breaks = seq(1, 7 , by = 1))


library(gridExtra)

# png(paste0("Figure_regtrans.png"), width=12, height=12,units="cm", res=400)

# grid.arrange(p1_1, p1_2, ncol = 1)

# dev.off()



p2_1 <-ggplot(ds.full, aes(x=regenergysoft)) + 
  geom_histogram(color="black", fill="white", binwidth=0.5) + 
  geom_vline(aes(xintercept= mean(ds.full$regenergysoft, na.rm = T) ),  color="grey20", linetype="dashed", size=1) + 
  theme_bw() + ylab("Count") + xlab("Soft regulation of energy efficiency") + scale_x_continuous(breaks = seq(1, 7 , by = 1))

p2_2 <-ggplot(ds.full, aes(x=regenergyhard)) + 
  geom_histogram(color="black", fill="white", binwidth=0.5) + 
  geom_vline(aes(xintercept= mean(ds.full$regenergyhard, na.rm = T) ),  color="grey20", linetype="dashed", size=1) + 
  theme_bw() + ylab("Count") + xlab("Hard regulation of energy efficiency") + scale_x_continuous(breaks = seq(1, 7 , by = 1))



# png(paste0("Figure_regenergy.png"), width=12, height=12,units="cm", res=400)

# grid.arrange(p2_1, p2_2, ncol = 1)
# dev.off()




#### 3.3 Individual items of dependent variable ####

# transparency

ds.full$tinfo <- as.numeric(as.character(ds.full$TE19_05))
ds.full$tlabel <- as.numeric(as.character(ds.full$TE19_01))
ds.full$tposin <- as.numeric(as.character(ds.full$TE19_03))
ds.full$tnegin <- as.numeric(as.character(ds.full$TE19_02))
ds.full$tlaws <- as.numeric(as.character(ds.full$TE19_04))
ds.full$tbans <- as.numeric(as.character(ds.full$TE19_06))


reg.frame1 <- data.frame(
  Instrument = c("Information", "Labels", "Positive incentives", "Negative incentives", "Hard provisions", "Bans"),
  Mean = c( mean(ds.full$TE19_05, na.rm = T), 
            mean(ds.full$TE19_01, na.rm = T), 
            mean(ds.full$TE19_03, na.rm = T), 
            mean(ds.full$TE19_02, na.rm = T), 
            mean(ds.full$TE19_04, na.rm = T), 
            mean(ds.full$TE19_06, na.rm = T))
)

reg.frame1$Instrument2 <- factor(reg.frame1$Instrument, levels =  c("Information", "Labels", "Positive incentives", "Negative incentives", "Hard provisions", "Bans")  )


# png("Figures/Fig1_regprefs1.png", width=12, height=12,units="cm", res=400)

# ggplot() + geom_bar(data=reg.frame1, aes(x = reorder(Instrument2, -as.numeric(Instrument2)   ), y = Mean), stat = "identity" ) + xlab("") + theme_bw() + coord_flip(ylim=c(1,5 )) 

# dev.off()


# energy efficiency

reg.frame2 <- data.frame(
  Instrument = c("Information", "Labels", "Positive incentives", "Negative incentives", "Hard provisions", "Bans"),
  Mean = c( mean(ds.full$TE11_05, na.rm = T), 
            mean(ds.full$TE11_01, na.rm = T), 
            mean(ds.full$TE11_03, na.rm = T), 
            mean(ds.full$TE11_02, na.rm = T), 
            mean(ds.full$TE11_04, na.rm = T), 
            mean(ds.full$TE11_06, na.rm = T))
)

reg.frame2$Instrument2 <- factor(reg.frame2$Instrument, levels =  c("Information", "Labels", "Positive incentives", "Negative incentives", "Hard provisions", "Bans")  )


# png("Figures/Fig1_regprefs2.png", width=12, height=12,units="cm", res=400)

# ggplot() + geom_bar(data=reg.frame2,aes(x = reorder(Instrument2,-as.numeric(Instrument2)   ), y = Mean), stat = "identity" ) + xlab("") + theme_bw() + coord_flip(ylim=c(1,5 )) 

# dev.off()





# energy efficiency

reg.frame2 <- data.frame(
  Instrument = c("Information", "Labels", "Positive incentives", "Negative incentives", "Hard provisions", "Bans"),
  Mean = c( mean(ds.full$TE21_05, na.rm = T), 
            mean(ds.full$TE21_01, na.rm = T), 
            mean(ds.full$TE21_03, na.rm = T), 
            mean(ds.full$TE21_02, na.rm = T), 
            mean(ds.full$TE21_04, na.rm = T), 
            mean(ds.full$TE21_06, na.rm = T))
)


# png("Figures/Fig2_regprefs2.png", width=12, height=12,units="cm", res=400)

# ggplot() + geom_bar(data=reg.frame2, aes(x = Instrument, y = Mean), stat = "identity" )  + xlab("") + theme_bw() +  coord_flip(ylim=c(1,5 )) 

# dev.off()


table(ds.full$TE21_05)



cor(ds.full$TE19_01, ds.full$TE21_01, use = "complete" )
cor(ds.full$TE19_02, ds.full$TE21_01, use = "complete" )
cor(ds.full$TE19_03, ds.full$TE21_01, use = "complete" )
cor(ds.full$TE19_04, ds.full$TE21_01, use = "complete" )
cor(ds.full$TE19_05, ds.full$TE21_01, use = "complete" )
cor(ds.full$TE19_06, ds.full$TE21_01, use = "complete" )





# energy efficiency

reg.frame2 <- data.frame(
  Instrument = c("Information", "Labels", "Positive incentives", "Negative incentives", "Hard provisions", "Bans"),
  Mean = c( mean(ds.full$TE20_05, na.rm = T), 
            mean(ds.full$TE20_01, na.rm = T), 
            mean(ds.full$TE20_03, na.rm = T), 
            mean(ds.full$TE20_02, na.rm = T), 
            mean(ds.full$TE20_04, na.rm = T), 
            mean(ds.full$TE20_06, na.rm = T))
)


# png("Figures/Fig2_regprefs4.png", width=12, height=12,units="cm", res=400)

# ggplot() +geom_bar(data=reg.frame2, aes(x = Instrument, y = Mean), stat = "identity" )  + xlab("") + theme_bw() +  coord_flip(ylim=c(1,5 )) 

# dev.off()


table(ds.full$TE21_05)





#### 4. Describe Independent variables ####


desc_1 <- psych::describe(ds.full[c("VT05_05",
                               "VT05_01", 
                               "VT05_10", 
                               "comp2gov",
                               "comp3gov",
                               "TE08_01",
                               "TE22_01",
                               "TE09_01",
                               "environ_eco",
                               "environ_future",
                               "control",
                               "technophob",
                               "algolit",
                               "polint",
                               "DG02_01",
                               "female",
                               "education")] 
      ) 

desc_1




#### 5. Norm variables to scale from 0 to 1 ####

ds.full$VT05_01_n <- (ds.full$VT05_01-1)/6
ds.full$VT05_05_n <- (ds.full$VT05_05-1)/6
ds.full$VT05_10_n <- (ds.full$VT05_10-1)/6 
ds.full$comp2all_n <- (ds.full$comp2all-1)/4
ds.full$comp3all_n <- (ds.full$comp3all-1)/4

ds.full$comp2gov_n <- (ds.full$comp2gov-1)/4
ds.full$comp3gov_n <- (ds.full$comp3gov-1)/4

ds.full$TE08_01_n <- (ds.full$TE08_01-1)/6
ds.full$TE22_01_n <- (ds.full$TE22_01-1)/6
ds.full$TE09_01_n <- (ds.full$TE09_01-1)/6

ds.full$control_n <- (ds.full$control-1)/4
ds.full$environ_n <- (ds.full$environ-1)/4
ds.full$environ_eco_n <- (ds.full$environ_eco-1)/4
ds.full$environ_future_n <- (ds.full$environ_future-1)/4

ds.full$technophob_n <- (ds.full$technophob-1)/4
ds.full$algolit_n <- (ds.full$algolit-1)/5
ds.full$polint_n <- (ds.full$polint-1)/4
ds.full$DG02_01_n <- (ds.full$DG02_01)/74




#### 6. Regression models ####

#### 6.1 Main models ####

#### m1a+b: soft regulation ####

ds.full$regcomptemp <- ds.full$comp2all_n 
# ds.full$regcomptemp <- ds.full$comp3gov

m1_1 <- lm(regtranssoft ~ 
            VT05_05_n +        # Political trust
             VT05_01_n +       # Trust Parliament
            VT05_10_n +        # Trust tech companies
            regcomptemp +      # Regulatory competence transparency
            TE08_01_n +        # position economy

            control_n +        # desire for control
            environ_eco_n +    # environmental concern eco
            environ_future_n + # environmental concern future 
            technophob_n +     # technophobia
            algolit_n  +       # algo literacy
            polint_n +         # Political interest
            DG02_01_n +        # age
            factor(female) +   # gender
            education,         # education
          data = ds.full)

summary(m1_1) 


ds.full$regcomptemp <- ds.full$comp3all_n 
m1_2 <- lm(regenergysoft ~ 
            VT05_05_n +        # Political trust
             VT05_01_n +       # Trust Parliament
            VT05_10_n +        # Trust tech companies
            regcomptemp +      # Regulatory competence energy
            TE08_01_n +        # position economy

            control_n +        # desire for control
             environ_eco_n +   # environmental concern eco
             environ_future_n +# environmental concern future 
            technophob_n +     # technophobia
            algolit_n  +       # algo literacy
            polint_n +         # Political interest
            DG02_01_n +        # age
            factor(female) +   # gender
            education,         # education
          data = ds.full)

summary(m1_2) 



# soft reg

modelframe1_1 <- data.frame(term = c(
  'Intercept',
  'Trust government',
  'Trust parliament',
  'Trust tech companies',
  'Reg. competence parties',
  'Market-liberal',
  

  'Desire for control',
  'Environmental concern:\nnature',
  'Environmental concern:\nfuture orientation',
  'Technophobia',
  'AI knowledge (self-assessed)',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m1_1)$coef[ , 1]  ,
std.error = summary(m1_1)$coef[ , 2]
)



modelframe1_2 <- data.frame(term = c(
  'Intercept',
  'Trust government',
  'Trust parliament',
  'Trust tech companies',
  'Reg. competence parties',
  'Market-liberal',

  
  'Desire for control',
  'Environmental concern:\nnature',
  'Environmental concern:\nfuture orientation',
  'Technophobia',
  'AI knowledge (self-assessed)',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m1_2)$coef[ , 1]  ,
std.error = summary(m1_2)$coef[ , 2]
)



modelframe1_1$model <- "Regulate \ntransparency"
modelframe1_2$model <- "Regulate \nenergy efficiency"


modelframe1 <- rbind(modelframe1_1, modelframe1_2)
modelframe1 <- modelframe1[modelframe1$term != "Intercept" , ]



p1 <- dwplot(modelframe1, 
              vline = geom_vline(xintercept = 0,  linetype = 1), 
              dot_args = list(aes(shape = model) , size = 2.5  ),
              vars_order = c(
                'Trust government',
                'Trust parliament',
                'Reg. competence parties',
                
                'Trust tech companies',

                'Market-liberal',

                
                'Desire for control',
                'Environmental concern:\nnature',
                'Environmental concern:\nfuture orientation',
                'Technophobia',
                'AI knowledge (self-assessed)',
                'Political interest',
                'Age',
                'Female',
                'High formal\neducation'
              ),
              model_order = c("Regulate \ntransparency", "Regulate \nenergy efficiency")
) + 
  theme_bw() + scale_colour_grey() + ggtitle("Soft regulation") +
  theme(legend.position = "none", plot.title = element_text(face="bold"), title =  element_text(size=10),
        axis.title.x = element_text(size=7), 
        legend.text = element_text(size=7)) + 
  xlab("Coefficient") + ylab("") + xlim( c(-0.8, 2.2)  ) + 
  scale_color_manual(values = c("grey65",  "black"), name = "Model", breaks=c("Regulate \ntransparency", "Regulate \nenergy efficiency") ) + 
  scale_shape_discrete(name = "Model", breaks = c("Regulate \ntransparency", "Regulate \nenergy efficiency") )  +
  guides(
    shape = guide_legend("Model"), 
    colour = guide_legend("Model"),
    size = guide_legend("Model")
  )
p1




#### m2a+b: hard regulation ####

ds.full$regcomptemp <- ds.full$comp2all_n 

m2_1 <- lm(regtranshard ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +      # Regulatory competence transparency
             TE08_01_n +        # position economy

             control_n +        # desire for control
             environ_eco_n +        # environmental concern eco
             environ_future_n +        # environmental concern future 
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_1) 



ds.full$regcomptemp <- ds.full$comp3all_n 

m2_2 <- lm(regenergyhard ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +       # Regulatory competence energy
             TE08_01_n +        # position economy

             control_n +        # desire for control
             environ_eco_n +        # environmental concern eco
             environ_future_n +        # environmental concern future 
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_2) 



# hard reg

modelframe2_1 <- data.frame(term = c(
  'Intercept',
  'Trust government',
  'Trust parliament',
  'Trust tech companies',
  'Reg. competence parties',
  'Market-liberal',

  
  'Desire for control',
  'Environmental concern:\nnature',
  'Environmental concern:\nfuture orientation',
  'Technophobia',
  'AI knowledge (self-assessed)',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m2_1)$coef[ , 1]  ,
std.error = summary(m2_1)$coef[ , 2]
)



modelframe2_2 <- data.frame(term = c(
  'Intercept',
  'Trust government',
  'Trust parliament',
  'Trust tech companies',
  'Reg. competence parties',
  'Market-liberal',

  
  'Desire for control',
  'Environmental concern:\nnature',
  'Environmental concern:\nfuture orientation',
  'Technophobia',
  'AI knowledge (self-assessed)',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m2_2)$coef[ , 1]  ,
std.error = summary(m2_2)$coef[ , 2]
)



modelframe2_1$model <- "Regulate \ntransparency"
modelframe2_2$model <- "Regulate \nenergy efficiency"


modelframe2 <- rbind(modelframe2_1, modelframe2_2)
modelframe2 <- modelframe2[modelframe2$term != "Intercept" , ]




p2 <- dwplot(modelframe2, 
             vline = geom_vline(xintercept = 0,  linetype = 1), 
             dot_args = list(aes(shape = model) , size = 2.5  ),
             vars_order = c(
               'Trust government',
               'Trust parliament',
               'Reg. competence parties',
               'Trust tech companies',

               'Market-liberal',

               
               'Desire for control',
               'Environmental concern:\nnature',
               'Environmental concern:\nfuture orientation',
               'Technophobia',
               'AI knowledge (self-assessed)',
               'Political interest',
               'Age',
               'Female',
               'High formal\neducation'
             ),
             model_order = c("Regulate \ntransparency", "Regulate \nenergy efficiency")
) + 
  theme_bw() + scale_colour_grey() + ggtitle("Hard regulation") +
  theme(legend.position = "right", plot.title = element_text(face="bold"), title =  element_text(size=10),
        axis.text.y = element_blank(), axis.title.x = element_text(size=7), 
        legend.text = element_text(size=7)) + 
  xlab("Coefficient") + ylab("") + xlim( c(-0.8, 2.2)  ) + 
  scale_color_manual(values = c("grey65",  "black"), name = "Model", breaks=c("Regulate \ntransparency", "Regulate \nenergy efficiency") ) + 
  scale_shape_discrete(name = "Model", breaks = c("Regulate \ntransparency", "Regulate \nenergy efficiency") )  +
  guides(
    shape = guide_legend("Model"), 
    colour = guide_legend("Model"),
    size = guide_legend("Model")
  )
p2 # plot without axis tics, will be joined with plot for soft regulation



library(gridExtra)

# png(paste0("Figure_dotwhisker_2.png"), width=20, height=12, units="cm", res=400)

# grid.arrange(p1, p2, widths = c(1.1, 1.0), ncol = 2)

# dev.off()

# wordreg( list(m1_1, m1_2, m2_1, m2_2),   file = "Main Models.doc")





#### 6.2 Regression without desire for control and environmental concern - testing the importance of political orientation ####

#### m1a+b: soft regulation ####

ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m1_1 <- lm(regtranssoft ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +    # Regulatory competence transparency
             TE08_01_n +        # position economy
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m1_1) 


ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m1_2 <- lm(regenergysoft ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +       # Regulatory competence transparency
             TE08_01_n +        # position economy
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m1_2) 



# soft reg

modelframe1_1 <- data.frame(term = c(
  'Intercept',
  'Trust government',
  'Trust parliament',
  'Trust tech companies',
  'Reg. competence parties',
  'Market-liberal',
  'Technophobia',
  'AI knowledge (self-assessed)',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m1_1)$coef[ , 1]  ,
std.error = summary(m1_1)$coef[ , 2]
)



modelframe1_2 <- data.frame(term = c(
  'Intercept',
  'Trust government',
  'Trust parliament',
  'Trust tech companies',
  'Reg. competence parties',
  'Market-liberal',
  'Technophobia',
  'AI knowledge (self-assessed)',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m1_2)$coef[ , 1]  ,
std.error = summary(m1_2)$coef[ , 2]
)



modelframe1_1$model <- "Regulate \ntransparency"
modelframe1_2$model <- "Regulate \nenergy efficiency"


modelframe1 <- rbind(modelframe1_1, modelframe1_2)
modelframe1 <- modelframe1[modelframe1$term != "Intercept" , ]


library(dotwhisker)
p1 <- dwplot(modelframe1, 
             vline = geom_vline(xintercept = 0,  linetype = 1), 
             dot_args = list(aes(shape = model) , size = 2.5  ),
             vars_order = c(
               'Trust government',
               'Trust parliament',
               'Reg. competence parties',
               
               'Trust tech companies',
               
               'Market-liberal',
               
               
               'Technophobia',
               'AI knowledge (self-assessed)',
               'Political interest',
               'Age',
               'Female',
               'High formal\neducation'
             ),
             model_order = c("Regulate \ntransparency", "Regulate \nenergy efficiency")
) + 
  theme_bw() + scale_colour_grey() + ggtitle("Soft regulation") +
  theme(legend.position = "none", plot.title = element_text(face="bold"), title =  element_text(size=10),
        axis.title.x = element_text(size=7), 
        legend.text = element_text(size=7)) + 
  xlab("Coefficient") + ylab("") + xlim( c(-0.8, 2.2)  ) + 
  scale_color_manual(values = c("grey65",  "black"), name = "Model", breaks=c("Regulate \ntransparency", "Regulate \nenergy efficiency") ) + 
  scale_shape_discrete(name = "Model", breaks = c("Regulate \ntransparency", "Regulate \nenergy efficiency") )  +
  guides(
    shape = guide_legend("Model"), 
    colour = guide_legend("Model"),
    size = guide_legend("Model")
  )
p1




#### m2a+b: hard regulation ####

ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m2_1 <- lm(regtranshard ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +       # Regulatory competence transparency
             TE08_01_n +        # position economy
             
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_1) 



ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m2_2 <- lm(regenergyhard ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +       # Regulatory competence transparency
             TE08_01_n +        # position economy
             
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_2) 



# hard reg

modelframe2_1 <- data.frame(term = c(
  'Intercept',
  'Trust government',
  'Trust parliament',
  'Trust tech companies',
  'Reg. competence parties',
  'Market-liberal',
  
  'Technophobia',
  'AI knowledge (self-assessed)',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m2_1)$coef[ , 1]  ,
std.error = summary(m2_1)$coef[ , 2]
)



modelframe2_2 <- data.frame(term = c(
  'Intercept',
  'Trust government',
  'Trust parliament',
  'Trust tech companies',
  'Reg. competence parties',
  'Market-liberal',
  
  'Technophobia',
  'AI knowledge (self-assessed)',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m2_2)$coef[ , 1]  ,
std.error = summary(m2_2)$coef[ , 2]
)



modelframe2_1$model <- "Regulate \ntransparency"
modelframe2_2$model <- "Regulate \nenergy efficiency"


modelframe2 <- rbind(modelframe2_1, modelframe2_2)
modelframe2 <- modelframe2[modelframe2$term != "Intercept" , ]




p2 <- dwplot(modelframe2, 
             vline = geom_vline(xintercept = 0,  linetype = 1), 
             dot_args = list(aes(shape = model) , size = 2.5  ),
             vars_order = c(
               'Trust government',
               'Trust parliament',
               'Reg. competence parties',
               
               'Trust tech companies',
               
               'Market-liberal',
               
               
               'Technophobia',
               'AI knowledge (self-assessed)',
               'Political interest',
               'Age',
               'Female',
               'High formal\neducation'
             ),
             model_order = c("Regulate \ntransparency", "Regulate \nenergy efficiency")
) + 
  theme_bw() + scale_colour_grey() + ggtitle("Hard regulation") +
  theme(legend.position = "right", plot.title = element_text(face="bold"), title =  element_text(size=10),
        axis.text.y = element_blank(), axis.title.x = element_text(size=7), 
        legend.text = element_text(size=7)) + 
  xlab("Coefficient") + ylab("") + xlim( c(-0.8, 2.2)  ) + 
  scale_color_manual(values = c("grey65",  "black"), name = "Model", breaks=c("Regulate \ntransparency", "Regulate \nenergy efficiency") ) + 
  scale_shape_discrete(name = "Model", breaks = c("Regulate \ntransparency", "Regulate \nenergy efficiency") )  +
  guides(
    shape = guide_legend("Model"), 
    colour = guide_legend("Model"),
    size = guide_legend("Model")
  )
p2



library(gridExtra)

# png(paste0("Figure_dotwhisker_1.png"), width=20, height=12, units="cm", res=400)

# grid.arrange(p1, p2, widths = c(1.1, 1.0), ncol = 2)

# dev.off()





#### 6.3 Regressions for individual instruments ####

# Transparency as DV

# information 

m1_1 <- lm(TE19_05 ~ 
             VT05_05 +        # Political trust
              VT05_10 +        # Trust tech companies
              comp2all +       # Perceived degulatory competence
              TE08_01 +        # position economy
             TE22_01 +
              TE09_01 +        # position environment
              environ +        # environmental concern
              control +        # desire for control
              technophob +     # technophobia
              algolit  +       # algo literacy
              polint +         # Political interest
              DG02_01 +        # alter
              factor(female) + # gender
              education,       # education
              data = ds.full)

summary(m1_1) 


# Labels 

m1_2 <- lm(TE19_01 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp2all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit  +       # algo literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m1_2) 



# Positive incentives 

m1_3 <- lm(TE19_03 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp2all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit  +       # algo literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m1_3) 



# Negative incentives

m1_4 <- lm(TE19_02 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp2all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit  +       # algo literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m1_4) 



# Laws


m1_5 <- lm(TE19_04 ~ 
              VT05_05 +        # Political trust
              VT05_10 +        # Trust tech companies
              comp2all +       # Perceived degulatory competence
              TE08_01 +        # position economy
             TE22_01 +
              TE09_01 +        # position environment
              environ +        # environmental concern
              control +        # desire for control
              technophob +     # technophobia
              algolit  +       # algo literacy
              polint +         # Political interest
              DG02_01 +        # alter
              factor(female) + # gender
              education,       # education
            data = ds.full)

summary(m1_5) 


# Bans

m1_6 <- lm(TE19_06 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp2all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit +        # algorithmic literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m1_6) 




# wordreg( list(m1_1, m1_2, m1_3, m1_4, m1_5, m1_6),   file = "Model1.doc")





# Energy efficiency as DV


# information 

m2_1 <- lm(TE11_05 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp3all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit +        # algorithmic literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_1) 



# Labels 

m2_2 <- lm(TE11_01 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp3all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit +        # algorithmic literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_2) 



# Positive incentives 

m2_3 <- lm(TE11_03 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp3all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit +        # algorithmic literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_3) 



# Negative incentives

m2_4 <- lm(TE11_02 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp3all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit +        # algorithmic literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_4) 



# Laws

m2_5 <- lm(TE11_04 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp3all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit +        # algorithmic literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_5) 



# Bans

m2_6 <- lm(TE11_06 ~ 
             VT05_05 +        # Political trust
             VT05_10 +        # Trust tech companies
             comp3all +       # Perceived degulatory competence
             TE08_01 +        # position economy
             TE22_01 +
             TE09_01 +        # position environment
             environ +        # environmental concern
             control +        # desire for control
             technophob +     # technophobia
             algolit +        # algorithmic literacy
             polint +         # Political interest
             DG02_01 +        # alter
             factor(female) + # gender
             education,       # education
           data = ds.full)

summary(m2_6) 


# wordreg( list(m2_1, m2_2, m2_3, m2_4, m2_5, m2_6),   file = "Model2.doc")








#### 7 Additional analyses ####

#### 7.1 Models with government competence as independent variable (Annex A5) ####

ds.full$regcomptemp <- ds.full$comp2gov_n # competence transparency
# ds.full$regcomptemp <- ds.full$comp3gov

m_govcomp_1 <- lm(regtranssoft ~ 
                 VT05_05_n +        # Political trust
                 VT05_01_n +        # Trust Parliament
                 VT05_10_n +        # Trust tech companies
                 regcomptemp +      # Regulatory competence transparency
                 TE08_01_n +        # position economy
                 
                 control_n +        # desire for control
                 environ_eco_n +    # environmental concern eco
                 environ_future_n + # environmental concern future 
                 technophob_n +     # technophobia
                 algolit_n  +       # algo literacy
                 polint_n +         # Political interest
                 DG02_01_n +        # age
                 factor(female) +   # gender
                 education,         # education
                 data = ds.full)

summary(m_govcomp_1) 


ds.full$regcomptemp <- ds.full$comp3gov_n # competence energy

m_govcomp_2 <- lm(regenergysoft ~ 
                 VT05_05_n +        # Political trust
                 VT05_01_n +        # Trust Parliament
                 VT05_10_n +        # Trust tech companies
                 regcomptemp +      # Regulatory competence transparency
                 TE08_01_n +        # position economy
                 
                 control_n +        # desire for control
                 environ_eco_n +    # environmental concern eco
                 environ_future_n + # environmental concern future 
                 technophob_n +     # technophobia
                 algolit_n  +       # algo literacy
                 polint_n +         # Political interest
                 DG02_01_n +        # age
                 factor(female) +   # gender
                 education,         # education
               data = ds.full)

summary(m_govcomp_2) 



ds.full$regcomptemp <- ds.full$comp2gov_n # competence transparency
# ds.full$regcomptemp <- ds.full$comp3gov

m_govcomp_3 <- lm(regtranshard ~ 
                    VT05_05_n +        # Political trust
                    VT05_01_n +        # Trust Parliament
                    VT05_10_n +        # Trust tech companies
                    regcomptemp +      # Regulatory competence transparency
                    TE08_01_n +        # position economy
                    
                    control_n +        # desire for control
                    environ_eco_n +    # environmental concern eco
                    environ_future_n + # environmental concern future 
                    technophob_n +     # technophobia
                    algolit_n  +       # algo literacy
                    polint_n +         # Political interest
                    DG02_01_n +        # age
                    factor(female) +   # gender
                    education,         # education
                  data = ds.full)

summary(m_govcomp_3) 


ds.full$regcomptemp <- ds.full$comp3gov_n # competence energy

m_govcomp_4 <- lm(regenergyhard ~ 
                    VT05_05_n +        # Political trust
                    VT05_01_n +        # Trust Parliament
                    VT05_10_n +        # Trust tech companies
                    regcomptemp +      # Regulatory competence transparency
                    TE08_01_n +        # position economy
                    
                    control_n +        # desire for control
                    environ_eco_n +    # environmental concern eco
                    environ_future_n + # environmental concern future 
                    technophob_n +     # technophobia
                    algolit_n  +       # algo literacy
                    polint_n +         # Political interest
                    DG02_01_n +        # age
                    factor(female) +   # gender
                    education,         # education
                  data = ds.full)

summary(m_govcomp_4) 


# wordreg( list(m_govcomp_1, m_govcomp_2, m_govcomp_3, m_govcomp_4),   file = "Models with gov competence.doc")



#### 7.2 Perceived effectiveness as DV (Annex A6) ####

# Transparency

ds.full$TE21_01[ds.full$TE19_01 <  0] <- NA
ds.full$TE21_02[ds.full$TE19_02 <  0] <- NA
ds.full$TE21_03[ds.full$TE19_03 <  0] <- NA
ds.full$TE21_04[ds.full$TE19_04 <  0] <- NA
ds.full$TE21_05[ds.full$TE19_05 <  0] <- NA
ds.full$TE21_06[ds.full$TE19_06 <  0] <- NA

# energy efficiency

ds.full$TE20_01[ds.full$TE11_01 <  0] <- NA
ds.full$TE20_02[ds.full$TE11_02 <  0] <- NA
ds.full$TE20_03[ds.full$TE11_03 <  0] <- NA
ds.full$TE20_04[ds.full$TE11_04 <  0] <- NA
ds.full$TE20_05[ds.full$TE11_05 <  0] <- NA
ds.full$TE20_06[ds.full$TE11_06 <  0] <- NA



ds.full$regtranshard_eff <- apply( ds.full[c( 
  "TE21_02",
  "TE21_04",
  "TE21_06")], 1, mean, na.rm=TRUE)


ds.full$regtranssoft_eff <- apply( ds.full[c( "TE21_05", 
                                              "TE21_01",
                                              "TE21_03")], 1, mean, na.rm=TRUE)


ds.full$regenergyhard_eff <- apply( ds.full[c( 
  "TE20_02",
  "TE20_04",
  "TE20_06")], 1, mean, na.rm=TRUE)

ds.full$regenergysoft_eff <- apply( ds.full[c( "TE20_05", 
                                               "TE20_01",
                                               "TE20_03")], 1, mean, na.rm=TRUE)



# Regression models

ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m_eff_1 <- lm(regtranssoft_eff ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                environ_future_n + # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m_eff_1) 


ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m_eff_2 <- lm(regenergysoft_eff ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                environ_future_n + # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m_eff_2) 




ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m_eff_3 <- lm(regtranshard_eff ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                environ_future_n + # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m_eff_3) 



ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m_eff_4 <- lm(regenergyhard_eff ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                environ_future_n + # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m_eff_4) 

# wordreg( list(m_eff_1, m_eff_2, m_eff_3, m_eff_4),   file = "Models with effectiveness DV.doc")



#### 7.3 Models with overall scales (Annex A7) ####


ds.full$regcomptemp <- ds.full$comp2all_n 
# ds.full$regcomptemp <- ds.full$comp3gov

m_overall_1 <- lm(regtrans ~ 
                    VT05_05_n +        # Political trust
                    VT05_01_n +        # Trust Parliament
                    VT05_10_n +        # Trust tech companies
                    regcomptemp +      # Regulatory competence transparency
                    TE08_01_n +        # position economy
                    
                    control_n +        # desire for control
                    environ_eco_n +    # environmental concern eco
                    environ_future_n + # environmental concern future 
                    technophob_n +     # technophobia
                    algolit_n  +       # algo literacy
                    polint_n +         # Political interest
                    DG02_01_n +        # age
                    factor(female) +   # gender
                    education,         # education
                  data = ds.full)

summary(m_overall_1) 


ds.full$regcomptemp <- ds.full$comp3all_n 

m_overall_2 <- lm(regenergy ~ 
                    VT05_05_n +        # Political trust
                    VT05_01_n +        # Trust Parliament
                    VT05_10_n +        # Trust tech companies
                    regcomptemp +      # Regulatory competence transparency
                    TE08_01_n +        # position economy
                    
                    control_n +        # desire for control
                    environ_eco_n +    # environmental concern eco
                    environ_future_n + # environmental concern future 
                    technophob_n +     # technophobia
                    algolit_n  +       # algo literacy
                    polint_n +         # Political interest
                    DG02_01_n +        # age
                    factor(female) +   # gender
                    education,         # education
                  data = ds.full)

summary(m_overall_2) 


# wordreg( list(m_overall_1, m_overall_2),   file = "Models with overall DV scales.doc")




#### 7.4 Models without environmental concern (Annex A8) ####

# soft

ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m1_1 <- lm(regtranssoft ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +      # Regulatory competence transparency
             TE08_01_n +        # position economy
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # age
             factor(female) +   # gender
             education,         # education
           data = ds.full)

summary(m1_1) 


ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m1_2 <- lm(regenergysoft ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +      # Regulatory competence transparency
             TE08_01_n +        # position economy
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # alter
             factor(female) +   # age
             education,         # education
           data = ds.full)

summary(m1_2) 


# hard

ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m2_1 <- lm(regtranshard ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +      # Regulatory competence transparency
             TE08_01_n +        # position economy
             
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # age
             factor(female) +   # gender
             education,         # education
           data = ds.full)

summary(m2_1) 



ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m2_2 <- lm(regenergyhard ~ 
             VT05_05_n +        # Political trust
             VT05_01_n +        # Trust Parliament
             VT05_10_n +        # Trust tech companies
             regcomptemp +      # Regulatory competence transparency
             TE08_01_n +        # position economy
             
             technophob_n +     # technophobia
             algolit_n  +       # algo literacy
             polint_n +         # Political interest
             DG02_01_n +        # age
             factor(female) +   # gender
             education,         # education
           data = ds.full)

summary(m2_2) 


# wordreg( list(m1_1, m1_2, m2_1, m2_2),   file = "Models without env concern.doc")




#### 7.5 Additional models for hard minus soft regulation (Annex A9) ####

# transparency

ds.full$regcomptemp <- ds.full$comp2all_n # code regulatory competence specifically as competenc regarding regulation for transparency
# ds.full$regcomptemp <- ds.full$comp3gov
m_hvss_1 <- lm(regtrans2 ~ 
                 VT05_05_n +        # Political trust
                 VT05_01_n +        # trust Parliament
                 VT05_10_n +        # Trust tech companies
                 regcomptemp +      # Regulatory competence transparency
                 TE08_01_n +        # position economy
                 
                 control_n +        # desire for control
                 environ_eco_n +    # environmental concern eco
                 environ_future_n + # environmental concern future 
                 technophob_n +     # technophobia
                 algolit_n  +       # algo literacy
                 polint_n +         # Political interest
                 DG02_01_n +        # age
                 factor(female) +   # gender
                 education,         # education
               data = ds.full)

summary(m_hvss_1) 


# energy

ds.full$regcomptemp <- ds.full$comp3all_n # code regulatory competence specifically as competenc regarding regulation for energy efficiency
m_hvss_2 <- lm(regenergy2 ~ 
                 VT05_05_n +        # Political trust
                 VT05_01_n +        # Trust Parliament
                 VT05_10_n +        # Trust tech companies
                 regcomptemp +      # Regulatory competence energy
                 TE08_01_n +        # position economy
                 
                 control_n +        # desire for control
                 environ_eco_n +    # environmental concern eco
                 environ_future_n + # environmental concern future 
                 technophob_n +     # technophobia
                 algolit_n  +       # algo literacy
                 polint_n +         # Political interest
                 DG02_01_n +        # age
                 factor(female) +   # gender
                 education,         # education
               data = ds.full)

summary(m_hvss_2) 


# wordreg( list(m_hvss_1, m_hvss_2),   file = "Models with hard vs soft DV.doc")





#### 7.6 Models for individual instruments (Annex A10 and A11) ####


ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m1_instr1 <- lm(TE19_05 ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                environ_future_n + # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m1_instr1) 

m1_instr2 <- lm(TE19_01 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m1_instr2) 


m1_instr3 <- lm(TE19_03 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m1_instr3) 

m1_instr4 <- lm(TE19_02 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m1_instr4) 


m1_instr5 <- lm(TE19_04 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m1_instr5) 


m1_instr6 <- lm(TE19_06 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m1_instr6) 


# wordreg( list(m1_instr1, m1_instr2, m1_instr3, m1_instr4, m1_instr5, m1_instr6),   file = "Models for individual instruments - transparency.doc")






ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m2_instr1 <- lm(TE11_05 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m2_instr1) 

m2_instr2 <- lm(TE11_01 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m2_instr2) 


m2_instr3 <- lm(TE11_03 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m2_instr3) 

m2_instr4 <- lm(TE11_02 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m2_instr4) 


m2_instr5 <- lm(TE11_04 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m2_instr5) 


m2_instr6 <- lm(TE11_06 ~ 
                  VT05_05_n +        # Political trust
                  VT05_01_n +        # Trust Parliament
                  VT05_10_n +        # Trust tech companies
                  regcomptemp +      # Regulatory competence transparency
                  TE08_01_n +        # position economy
                  
                  control_n +        # desire for control
                  environ_eco_n +    # environmental concern eco
                  environ_future_n + # environmental concern future 
                  technophob_n +     # technophobia
                  algolit_n  +       # algo literacy
                  polint_n +         # Political interest
                  DG02_01_n +        # age
                  factor(female) +   # gender
                  education,         # education
                data = ds.full)

summary(m2_instr6) 


# wordreg( list(m2_instr1, m2_instr2, m2_instr3, m2_instr4, m2_instr5, m2_instr6),   file = "Models for individual instruments - energy.doc")




#### 7.7 Analysis with item that has most explicit future orientation in wording (Annex A12) ####

ds.full$fu_gen_n <- (ds.full$TE04_06-1)/4

ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m_fugen_1 <- lm(regtranssoft ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                fu_gen_n +         # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m_fugen_1) 


ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m_fugen_2 <- lm(regenergysoft ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                fu_gen_n +         # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m_fugen_2) 




ds.full$regcomptemp <- ds.full$comp2all_n # competence transparency

m_fugen_3 <- lm(regtranshard ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                fu_gen_n +         # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m_fugen_3) 



ds.full$regcomptemp <- ds.full$comp3all_n # competence energy

m_fugen_4 <- lm(regenergyhard ~ 
                VT05_05_n +        # Political trust
                VT05_01_n +        # Trust Parliament
                VT05_10_n +        # Trust tech companies
                regcomptemp +      # Regulatory competence transparency
                TE08_01_n +        # position economy
                
                control_n +        # desire for control
                environ_eco_n +    # environmental concern eco
                fu_gen_n +         # environmental concern future 
                technophob_n +     # technophobia
                algolit_n  +       # algo literacy
                polint_n +         # Political interest
                DG02_01_n +        # age
                factor(female) +   # gender
                education,         # education
              data = ds.full)

summary(m_fugen_4) 

# wordreg( list(m_fugen_1, m_fugen_2, m_fugen_3, m_fugen_4),   file = "Models with only future gen.doc")





