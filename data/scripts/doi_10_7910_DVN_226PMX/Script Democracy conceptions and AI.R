

## Script for Citizen Conceptions of Democracy and Support for Artificial Intelligence in Government and Politics

## Version: June 13, 2022



#### Load packages ####

library(RColorBrewer)
library(psych)
library(FactoMineR)
library(factoextra)
library(lattice)
library(MASS)
library(smacof)
library(tidyr)
library(texreg)
library(dotwhisker)
library(gridExtra)
library(lavaan)
library(mokken)
library(Hmisc)
library(konfound)




#### Apply filters ####

# Attention check
df <- df[df$TE06_05 == 1, ]

# Control question (at the end of the survey)
df <- df[df$KO01 == "Ja, ich habe den Fragebogen aufmerksam und nach bestem Wissen und Gewissen beantwortet. Meine Angaben können zur Auswertung verwendet werden.", ]

# Speeding (at least 5 min. = 300 Sec.)
sum(df$TIME_SUM < 300, na.rm = T)
df <- df[df$TIME_SUM >= 300, ]




#### 1. Description and scaling of the dependent variable: support for AI in government and politics ####

#### 1.1 description ####

aisuppframe1 <- data.frame(
  Task = c( "Perform routine admin. tasks", 
            "Perform complex admin. tasks",
            "Assist in preparation of pol. decisions", 
            "Assist parliamentarians with decisions", 
            "Take decisions for politicians", 
            "Compete in elections"),
  Mean = c( mean(df$PL04_01, na.rm = T), 
            mean(df$PL04_02, na.rm = T), 
            mean(df$PL04_03, na.rm = T), 
            mean(df$PL04_04, na.rm = T), 
            mean(df$PL04_05, na.rm = T), 
            mean(df$PL04_06, na.rm = T)
            ),
  SD   = c( sd(df$PL04_01, na.rm = T)/sqrt( sum( is.na(df$PL04_01)==F ) ), 
            sd(df$PL04_02, na.rm = T)/sqrt( sum( is.na(df$PL04_02)==F ) ), 
            sd(df$PL04_03, na.rm = T)/sqrt( sum( is.na(df$PL04_03)==F ) ), 
            sd(df$PL04_04, na.rm = T)/sqrt( sum( is.na(df$PL04_04)==F ) ), 
            sd(df$PL04_05, na.rm = T)/sqrt( sum( is.na(df$PL04_05)==F ) ), 
            sd(df$PL04_06, na.rm = T)/sqrt( sum( is.na(df$PL04_06)==F ) )
  )
)

aisuppframe1$Task <- factor(aisuppframe1$Task , 
                            levels =  c("Perform routine admin. tasks", 
                                        "Perform complex admin. tasks",
                                        "Assist in preparation of pol. decisions", 
                                        "Assist parliamentarians with decisions", 
                                        "Take decisions for politicians", 
                                        "Compete in elections") )




# png(paste0("Figure_1_DV1.png"), width=14, height=6,units="cm", res=400)

ggplot(data=aisuppframe1, aes(x = reorder(Task, -as.numeric(Task)), y = Mean)) + 
  geom_bar(stat = "identity", fill = "grey60") + xlab("") + 
  geom_errorbar( aes(ymin=Mean-1.96*SD, ymax=Mean + 1.96*SD), size = 0.4,  width = 0.2, colour = "grey20") + theme_bw() + coord_flip(ylim=c(1,5 ))


# dev.off()


# pdf(paste0("Figure_1_DV1.pdf"), width=6, height=5)

ggplot(data=aisuppframe1, aes(x = reorder(Task, -as.numeric(Task)), y = Mean)) + 
  geom_bar(stat = "identity", fill = "grey60") + xlab("") + 
  geom_errorbar( aes(ymin=Mean-1.96*SD, ymax=Mean + 1.96*SD), size = 0.4,  width = 0.2, colour = "grey20") + theme_bw() + coord_flip(ylim=c(1,5 ))

# dev.off()



#### 1.2 Mokken scaling (Annex A16) ####

tmp <- df[ , c("PL04_01", 
                    "PL04_02",
                    "PL04_03", 
                    "PL04_04", 
                    "PL04_05", 
                    "PL04_06") ]
tmp <- tmp[complete.cases(tmp), ]

coefH( tmp) # calculate scalability coefficients H

check.reliability(tmp) # calculate reliability

coefZ(tmp) # calculate Z-values for item pairs and individual items 

summary(check.iio( tmp  ) ) # Check invariance of item ordering

# plot( check.monotonicity(tmp ) )




#### 1.3 Principal component analysis (Annex A15) ####

pca.out <- principal(df[c("PL04_01", 
                          "PL04_02",
                          "PL04_03", 
                          "PL04_04", 
                          "PL04_05", 
                          "PL04_06")], 
                     nfactors = 3, rotate="promax")
pca.out$loadings


# Calculate alphas for subscales and the entire scale

# subscale Perform administrative tasks
psych::alpha(df[c("PL04_01", 
                  "PL04_02") ],
             check.keys=TRUE) 

# subscale: Assist political decision making
psych::alpha(df[c("PL04_03", 
                  "PL04_04") ],
             check.keys=TRUE) 

# subscale: Replace political decision-making
psych::alpha(df[c("PL04_05", 
                  "PL04_06") ],
             check.keys=TRUE) 

# entire scale
psych::alpha(df[c("PL04_01", 
                  "PL04_02",
                  "PL04_03", 
                  "PL04_04", 
                  "PL04_05", 
                  "PL04_06") ],
             check.keys=TRUE) 



# Generate subscales

df$aisupport_admin <- (df$PL04_01 + df$PL04_02)/2
df$aisupport_assist <- (df$PL04_03 + df$PL04_04)/2
df$aisupport_replace <- (df$PL04_05 + df$PL04_06)/2


# Generate overall scale

df$aisupport_total <- apply( df[c( "PL04_01", 
                              "PL04_02",
                              "PL04_03", 
                              "PL04_04", 
                              "PL04_05", 
                              "PL04_06")], 1, mean, na.rm=TRUE)

desc_1 <- psych::describe(df[c("aisupport_admin", "aisupport_assist", "aisupport_replace", "aisupport_total")] ) 



# Plot distribution of overall scale (Annex A17)

# png(paste0("Figure_1_DV1_index.png"), width=12, height=8,units="cm", res=400)

ggplot(df, aes( x = aisupport_total)) + geom_histogram(color="black", fill="white", bins = 30) + 
  geom_vline(aes(xintercept=mean(aisupport_total, na.rm =T)), color="grey50", linetype="dotted", size=1) +
  xlab("Value") + ylab("Frequency") + theme_bw()

# dev.off()








#### 2. Prepare independent variables ####


#### 2.1 Conceptions of democracy (Annex A5 to A8) ####

# Only deformations of democracy
pca.out <- principal(df[c("DE05_01", # populist
                          "DE05_02", # populist
                          "DE05_03", # populist
                          "DE05_04", # post-democratic
                          "DE05_05", # post-democratic
                          "DE05_06", # post-democratic
                          "DE05_07", # technocratic
                          "DE05_08", # technocratic
                          "DE05_09", # technocratic
                          "DE05_10", # majrel
                          "DE05_11", # majrel
                          "DE05_12")], 
                     nfactors = 4, rotate="promax")
pca.out$loadings


# All items
pca.out <- principal(df[c("DE05_01", # populist
                          "DE05_02", # populist
                          "DE05_03", # populist
                          "DE05_04", # post-democratic
                          "DE05_05", # post-democratic
                          "DE05_06", # post-democratic
                          "DE05_07", # technocratic
                          "DE05_08", # technocratic
                          "DE05_09", # technocratic
                          "DE05_10", # majrel
                          "DE05_11", # majrel
                          "DE05_12", # majrel
                          "DE08_01",   
                          "DE08_02",
                          "DE08_03", 
                          "DE08_04",
                          "DE08_05",   
                          "DE08_06",
                          "DE08_07", 
                          "DE08_08")], 
                     nfactors = 6, rotate="promax")
pca.out$loadings

KMO(tmp)



# Confirmatory factor analysis (CFA)

tmp <- df[ , c("DE05_01", # populist
               "DE05_02", # populist
               "DE05_03", # populist
               "DE05_04", # post-democratic
               "DE05_05", # post-democratic
               "DE05_06", # post-democratic
               "DE05_07", # technocratic
               "DE05_08", # technocratic
               "DE05_09", # technocratic
               "DE05_10", # majrel
               "DE05_11", # majrel
               "DE05_12",
               "DE08_01", ## lib-dem: dynamic
               "DE08_02",   
               "DE08_03",   
               "DE08_04", ## lib-dem: pluralist
               "DE08_05",   
               "DE08_06",
               "DE08_07",
               "DE08_08") ]
tmp <- tmp[complete.cases(tmp), ]


# CFA - only disfigurations

model <- '
  # measurement model

    populist  =~ DE05_01 + DE05_02 + DE05_03 
    
    postdemocratic  =~ DE05_04 + DE05_05 + DE05_06
    
    technocratic  =~  DE05_07 + DE05_08 + DE05_09
    
    majrel  =~ DE05_10 + DE05_11 + DE05_12
    

'


fit <- cfa(model, data = tmp, )

summary(fit, fit.measures=TRUE)

summary(fit, standardized = TRUE, rsq = T)



# CFA - all items

model <- '
  # measurement model

    populist  =~ DE05_01 + DE05_02 + DE05_03 
    
    postdemocratic  =~ DE05_04 + DE05_05 + DE05_06
    
    technocratic  =~  DE05_07 + DE05_08 +  DE05_09
    
    majrel  =~ DE05_10 + DE05_11 + DE05_12
    
    libdem1 =~ DE08_01 + DE08_02 + DE08_03 + DE08_04
    
    libdem2 =~ DE08_05 + DE08_06 + DE08_07 + DE08_08
'


fit <- cfa(model, data = tmp, )

summary(fit, fit.measures=TRUE)

summary(fit, standardized = TRUE, rsq = T)

model_performance(fit, metrics = "all")


# CFA - removing libdem1-1 and technocratic2

model <- '
  # measurement model

    populist  =~ DE05_01 + DE05_02 + DE05_03 
    
    postdemocratic  =~ DE05_04 + DE05_05 + DE05_06
    
    technocratic  =~  DE05_07 + DE05_09
    
    majrel  =~ DE05_10 + DE05_11 + DE05_12
    
    libdem1 =~ DE08_02 + DE08_03 + DE08_04
    
    libdem2 =~ DE08_05 + DE08_06 + DE08_07 + DE08_08
'


fit <- cfa(model, data = tmp, )

summary(fit, fit.measures=TRUE)

summary(fit, standardized = TRUE, rsq = T)




# Generate variables for conceptions of democracy

psych::alpha(df[c("DE05_01", 
                  "DE05_02",
                  "DE05_03") ],
             check.keys=TRUE) 

df$populist <- apply( df[c( "DE05_01", 
                            "DE05_02",
                            "DE05_03")], 1, mean, na.rm=TRUE)

psych::alpha(df[c("DE05_04", 
                  "DE05_05",
                  "DE05_06") ],
             check.keys=TRUE) 

df$postdem <- apply( df[c( "DE05_04", 
                           "DE05_05",
                           "DE05_06")], 1, mean, na.rm=TRUE)

psych::alpha(df[c("DE05_07", 
                  "DE05_08",
                  "DE05_09") ],
             check.keys=TRUE) 


df$technocratic <- apply( df[c( "DE05_07", 
                                "DE05_08", 
                                "DE05_09")], 1, mean, na.rm=TRUE)


psych::alpha(df[c("DE05_10", 
                  "DE05_11",
                  "DE05_12") ],
             check.keys=TRUE) 

df$majrel <- apply( df[c( "DE05_10", 
                            "DE05_11",
                            "DE05_12")], 1, mean, na.rm=TRUE)

# libdem

psych::alpha(df[c("DE08_01",   
                  "DE08_02",
                  "DE08_03", 
                  "DE08_04",
                  "DE08_05",   
                  "DE08_06",
                  "DE08_07", 
                  "DE08_08") ],
             check.keys=TRUE) # Alpha is 0.87


# dynamic contestation

psych::alpha(df[c("DE08_01",   
                  "DE08_02",
                  "DE08_03", 
                  "DE08_04") ],
             check.keys=TRUE) # Alpha is 0.81



# pluralist

psych::alpha(df[c("DE08_05",   
                  "DE08_06",
                  "DE08_07", 
                  "DE08_08") ],
             check.keys=TRUE) # Alpha is 0.86



df$dynamic <- apply( df[c( "DE08_01", 
                           "DE08_02",
                           "DE08_03",
                           "DE08_04")], 1, mean, na.rm=TRUE)

df$pluralist <- apply( df[c( "DE08_05", 
                             "DE08_06",
                             "DE08_07",
                             "DE08_08")], 1, mean, na.rm=TRUE)


df$libdem <- (df$dynamic + df$pluralist)/2







#### 2.2 Political disaffection ####

pca.out <- principal(df[c("PL06_01", # inverted
                          "PL06_02",
                          "PL06_03",
                          "PL06_04", 
                          "PL06_05", # pol efficacy
                          "PL06_06", # pol efficacy
                          "PL06_07", # inverted
                          "PL06_08")], 
                     nfactors = 3, rotate="promax")
pca.out$loadings # item 8 shows a bad fit, leads to a third component ; but no fourth component with Eigenvalue above 1


pca.out <- principal(df[c("PL06_01", # inverted
                          "PL06_02",
                          "PL06_03",
                          "PL06_04", 
                          "PL06_05", # pol efficacy
                          "PL06_06", # pol efficacy
                          "PL06_07"  # inverted
                          )], 
                     nfactors = 2, rotate="promax")
pca.out$loadings # political disaffection items can be seperated into two subdimensions



df$PL06_03r <- 6-df$PL06_03 # invert item
df$PL06_07r <- 6-df$PL06_07 # invert item


psych::alpha(df[c("PL06_01", 
                  "PL06_02",
                  "PL06_03r",
                  "PL06_04",
                  "PL06_07r") ],
             check.keys=TRUE) # Alpha is 0.80


df$disaffec <- apply( df[c( "PL06_01", 
                            "PL06_02",
                            "PL06_03r",
                            "PL06_04",
                            "PL06_07r")], 1, mean, na.rm=TRUE)



#### 2.3 Satisfaction with democracy, institutional trust, self-efficacy ####

# satisfaction with democracy

df$VT06_01




# efficacy
df$PL06_06r <- 6-df$PL06_06

psych::alpha(df[c("PL06_05", 
                  "PL06_06r") ],
             check.keys=TRUE) 


df$efficacy <- apply( df[c( "PL06_05", 
                            "PL06_06r")], 1, mean, na.rm=TRUE)

df$efficacy <- 6 - df$efficacy


# institutional trust
psych::alpha(df[c("VT04_01",   
                  "VT04_02",
                  "VT04_03", 
                  "VT04_04",
                  "VT04_05") ],
             check.keys=TRUE) 

df$poltrust <- apply( df[c( "VT04_01",   
                            "VT04_02",
                            "VT04_03", 
                            "VT04_04",
                            "VT04_05")], 1, mean, na.rm=TRUE)




#### 2.4 ambiguity intolerance ####

psych::alpha(df[c("PS04_01", 
                  "PS04_02",
                  "PS04_03",
                  "PS04_04", 
                  "PS04_06",
                  "PS04_08") ],
             check.keys=TRUE) # Alpha is 0.75

df$PS04_01r <- 6 - df$PS04_01 
df$PS04_03r <- 6 - df$PS04_03


df$ambitol <- apply( df[c( "PS04_01r", 
                           "PS04_02",
                           "PS04_03r",
                           "PS04_04", 
                           "PS04_06",
                           "PS04_08")], 1, mean, na.rm=TRUE)


#### 2.5 Left-right ideolog and policy preferences ####

df$PL01_01 # left right

df$PL02_01  # economic

df$PL03_01  # socio-cultural / immigration



#### 2.6 General AI evaluation ####

df$TE04_01


#### 2.7 AI optimism ####

psych::alpha(df[c("TE06_01",   
                  "TE06_02",
                  "TE06_03", 
                  "TE06_04",
                  "TE06_06",
                  "TE06_07", 
                  "TE06_08") ],
             check.keys=TRUE) 


df$aioptimism <- apply( df[c( "TE06_01",   
                              "TE06_02",
                              "TE06_03", 
                              "TE06_04",   
                              "TE06_06",
                              "TE06_07", 
                              "TE06_08")], 1, mean, na.rm=TRUE)



#### 2.8 Self-assessed AI knowledge ####

df$KE03
table(as.numeric(df$KE03) )

unique(df$KE03)

df$knowledge <- NA
df$knowledge[df$KE03 == unique(df$KE03)[5]  ]  <- 1
df$knowledge[df$KE03 == unique(df$KE03)[2]  ]  <- 2
df$knowledge[df$KE03 == unique(df$KE03)[3]  ]  <- 3
df$knowledge[df$KE03 == unique(df$KE03)[1]  ]  <- 4
df$knowledge[df$KE03 == unique(df$KE03)[4]  ]  <- 5
df$knowledge[df$KE03 == unique(df$KE03)[7]  ]  <- 6

table(df$knowledge )



#### 2.9 Political interest ####

df$VT05
table(df$VT05)

df$polint <- as.numeric(df$VT05)


#### 2.10 Demographics ####


# gender

df$female <- 0
df$female[df$DG15 == "weiblich"] <- 1

# age

df$age


# education

df$education <- 0 
df$education[df$DG03 == "Fachhochschulreife (Abschluss einer Fachoberschule etc.)"] <- 1
df$education[df$DG03 == "Abitur bzw. erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)"] <- 1






#### 3. Description of IVs (Annex A14) ####

desc_1 <- psych::describe(df[c("populist",
                             "postdem", 
                             "technocratic",
                             "majrel",
                             "libdem",
                             "dynamic",
                             "pluralist",
                             "disaffec",
                             "poltrust",
                             "efficacy",
                             "VT06_01",
                             "ambitol",
                             "TE04_01",
                             "aioptimism",
                             "PL01_01",
                             "PL02_01",
                             "PL03_01",
                             "knowledge",
                             "polint",
                             "age",
                             "female",
                             "education")] 
) 




# Check distribution of AI optimism (Annex A11) 

hist(df$aioptimism)
summary(df$aioptimism)
sd(df$aioptimism, na.rm = T)


png(paste0("Figure_A1_aioptimism.png"), width=12, height=12,units="cm", res=400)

ggplot(df, aes( x = aioptimism)) + geom_histogram(color="black", fill="white") + 
  geom_vline(aes(xintercept=mean(aioptimism, na.rm =T)), color="grey20", linetype="dashed", size=1) +
  xlab("Value") + ylab("Frequency") + theme_bw()

dev.off()



# Check distribution of AI knowlege (Annex A13)

hist(df$knowledge)
summary(df$knowledge)
sd(df$knowledge, na.rm = T)


png(paste0("Figure_AIknowledge.png"), width=12, height=12,units="cm", res=400)

ggplot(df, aes( x = knowledge)) + geom_bar(color="black", fill="white") + 
  geom_vline(aes(xintercept=mean(aioptimism, na.rm =T)), color="grey20", linetype="dashed", size=1) +
  xlab("Value") + ylab("Frequency") + theme_bw()

dev.off()







#### 4. Correlations of conceptions of democracy and political support with DVs (Annex A18)  #####

tmp <- df[ c("libdem", "technocratic", "populist",  "postdem", "majrel", 
             "disaffec", "poltrust", "VT06_01",
             "aisupport_admin", "aisupport_assist" , "aisupport_replace", "aisupport_total" ) ]


cor.out <- rcorr(as.matrix(tmp))

cor.out$r
cor.out$P

cor.out$P2 <- cor.out$P
cor.out$P2[cor.out$P >= 0.05] <- ""
cor.out$P2[cor.out$P < 0.05] <- "*"
cor.out$P2[cor.out$P < 0.01] <- "**"
cor.out$P2 


#### 5. Correlations between main independent variables  #####

tmp <- df[ c("libdem", "technocratic", "populist",  "postdem", "majrel", 
             "VT06_01", "disaffec", "poltrust") ]



cor.out <- rcorr(as.matrix(tmp))

cor.out$r
cor.out$P

cor.out$P2 <- cor.out$P
cor.out$P2[cor.out$P >= 0.05] <- ""
cor.out$P2[cor.out$P < 0.05] <- "*"
cor.out$P2[cor.out$P < 0.01] <- "**"
cor.out$P2 






#### 6. Regression models ####

#### 6.1 DV: Subscale 1: administrative tasks ####

m1_1a <- lm(aisupport_admin ~ 
              libdem + 
              populist +        
              technocratic +   
              postdem +         
              majrel +       
              VT06_01 + 
              disaffec +       
              poltrust +
              efficacy +
              ambitol +        
              knowledge +    
              PL01_01 +        
              PL02_01 +        
              PL03_01 +   
              polint +        
              age +        
              female + 
              education,       
         data = df)

summary(m1_1a) 


m1_1b <- lm(aisupport_admin ~ 
              libdem +       # liberal-democratic
              populist +     # populist    
              technocratic + # technocratic   
              postdem +      # postdemocratic    
              majrel +       # majoritarian-relativist
              VT06_01 +      # satisfaction with democracy
              disaffec +     # political disaffection  
              poltrust +     # institutional trust
              efficacy +     # self-efficacy
              ambitol +      # ambiguity intolerance   
              TE04_01 +      # general AI evaluation 
              aioptimism +   # AI optimism  
              knowledge  +   # AI knowledge (self-assessed) 
              PL01_01 +      # left-right ideology   
              PL02_01 +      # economic policy preference   
              PL03_01 +      # immigratino policy preference   
              polint +       # political interest   
              age +          # age
              female +       # gender
              education,     # education   
            data = df)

summary(m1_1b) 





#### 6.2 DV: Subscale 2: assist decision-making ####

m1_2a <- lm(aisupport_assist ~ 
              libdem + 
              populist +        
              technocratic +   
              postdem +         
              majrel +       
              VT06_01 + 
              disaffec +       
              poltrust +
              efficacy +
              ambitol +        
              knowledge  +     
              PL01_01 +        
              PL02_01 +       
              PL03_01 +        
              polint +         
              age +        
              female +
              education,       
           data = df)

summary(m1_2a) 



m1_2b <- lm(aisupport_assist ~ 
              libdem + 
              populist +        
              technocratic +   
              postdem +        
              majrel +       
              VT06_01 + 
              disaffec +       
              poltrust +
              efficacy +
              ambitol +        
              TE04_01 +        
              aioptimism +     
              knowledge  +     
              PL01_01 +        
              PL02_01 +        
              PL03_01 +       
              polint +        
              age +        
              female + 
              education,       
           data = df)

summary(m1_2b) 








#### 6.3 DV: Subscale 3: replace decision-making ####

m1_3a <- lm(aisupport_replace ~ 
              libdem + 
              populist +        
              technocratic +   
              postdem +         
              majrel +       
              VT06_01 + 
              disaffec +       
              poltrust +
              efficacy +
              ambitol +        
              knowledge  +     
              PL01_01 +        
              PL02_01 +       
              PL03_01 +        
              polint +         
              age +        
              female +
              education,       
           data = df)

summary(m1_3a) 



m1_3b <- lm(aisupport_replace ~ 
              libdem + 
              populist +        
              technocratic +   
              postdem +        
              majrel +       
              VT06_01 + 
              disaffec +       
              poltrust +
              efficacy +
              ambitol +        
              TE04_01 +        
              aioptimism +     
              knowledge  +     
              PL01_01 +        
              PL02_01 +        
              PL03_01 +       
              polint +        
              age +        
              female + 
              education,       
            data = df)

summary(m1_3b) 




# wordreg( list(m4_1a, m4_1b,  m4_2a, m4_2b,  m4_3a, m4_3b),  file = "Main_models_2_subscales.doc")


data.frame(car::vif(m1_1b)) 


#### 6.4 Generate coefficient plot with all three subscales as DVs ####

# Build dataframe for coefplot

modelframe1 <- data.frame(term = c(
  'Intercept',
  'Liberal-democratic',
  'Populist',
  'Technocratic',
  'Postdemocratic',
  'Majoritarian-relativist',
  'Satisfaction w. democracy',
  'Political disaffection',
  'Institutional trust',
  'Self-efficacy',
  'Ambiguity intolerance',
  'General AI evaluation',
  'AI optimism',
  'AI knowledge\n(self-assessed)',
  'Left-Right',
  'Market-liberal',
  'Anti- vs.pro-\nimmigration',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m1_1b)$coef[ , 1]  ,
std.error = summary(m1_1b)$coef[ , 2]
)



modelframe2 <- data.frame(term = c(
  'Intercept',
  'Liberal-democratic',
  'Populist',
  'Technocratic',
  'Postdemocratic',
  'Majoritarian-relativist',
  'Satisfaction w. democracy',
  'Political disaffection',
  'Institutional trust',
  'Self-efficacy',
  'Ambiguity intolerance',
  'General AI evaluation',
  'AI optimism',
  'AI knowledge\n(self-assessed)',
  'Left-Right',
  'Market-liberal',
  'Anti- vs.pro-\nimmigration',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m1_2b)$coef[ , 1]  ,
std.error = summary(m1_2b)$coef[ , 2]
)





modelframe3 <- data.frame(term = c(
  'Intercept',
  'Liberal-democratic',
  'Populist',
  'Technocratic',
  'Postdemocratic',
  'Majoritarian-relativist',
  'Satisfaction w. democracy',
  'Political disaffection',
  'Institutional trust',
  'Self-efficacy',
  'Ambiguity intolerance',
  'General AI evaluation',
  'AI optimism',
  'AI knowledge\n(self-assessed)',
  'Left-Right',
  'Market-liberal',
  'Anti- vs.pro-\nimmigration',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m1_3b)$coef[ , 1]  ,
std.error = summary(m1_3b)$coef[ , 2]
)




modelframe1$model <- "Perform\nadministrative tasks"
modelframe2$model <- "Assist political\ndecision-making"
modelframe3$model <- "Replace political\ndecision-making"

modelframe <- rbind(modelframe1, modelframe2, modelframe3)
modelframe <- modelframe[modelframe$term != "Intercept" , ]




p1a <- dwplot(modelframe, 
              vline = geom_vline(xintercept = 0,  linetype = 1), 
              dot_args = list(aes(shape = model) , size = 2.5  ),
              model_order = c("Perform\nadministrative tasks", "Assist political\ndecision-making", "Replace political\ndecision-making")
              ) + 
  theme_bw() + scale_colour_grey() + theme(legend.position = "bottom") + xlab("Coefficient") + ylab("") +    
  scale_color_manual(values = c("grey65", "grey50", "black") , name = "Dependent\nvariable", breaks=c("Perform\nadministrative tasks", "Assist political\ndecision-making", "Replace political\ndecision-making") ) + 
  scale_shape_discrete(name = "Dependent\nvariable", breaks = c("Perform\nadministrative tasks", "Assist political\ndecision-making", "Replace political\ndecision-making") )  +
  guides(
    shape = guide_legend("Dependent\nvariable"), 
    colour = guide_legend("Dependent\nvariable"),
    size = guide_legend("Dependent\nvariable")
  )




# png(paste0("coefficient_plot_1.png"), width=18, height=22, units="cm", res=400)

p1a

# dev.off()



# pdf(paste0("coefficient_plot_1.pdf"), width=7, height=9)

p1a

# dev.off()



#### 6.5 DV: overall scale (Annex A23) - including coefficient plot ####

# Without conceptions of democracy
m2 <- lm(aisupport_total ~ 
           VT06_01 +        # satisfaction with democracy
           disaffec +       # political disaffection
           poltrust  +      # institutional trust
           efficacy +       # self-efficacy
           ambitol +        # ambiguity tolerance
           PL01_01 +        # left right
           PL02_01 +        # economic position
           PL03_01 +        # immigration position
           knowledge  +     # AI knowledge
           polint +         # political interest
           age +            # age
           female +         # gender
           education,       # education
         data = df)

summary(m2) 



# With conceptions of democracy
m3 <- lm(aisupport_total ~ 
           libdem + 
           technocratic +   
           populist +        
           postdem +         
           majrel +         
           VT06_01 + 
           disaffec +       
           poltrust +
           efficacy +
           ambitol +       
           TE04_01 +        
           aioptimism +     
           knowledge  +     
           PL01_01 +        
           PL02_01 +        
           PL03_01 +       
           polint +         
           age +        
           female + 
           education,       
         data = df)

summary(m3) 


# check variance inflation coefficients (Annex A22)

car::vif(m3)



# Build dataframe for coefplot

modelframe_index <- data.frame(term = c(
  'Intercept',
  'Liberal-democratic',
  'Technocratic',
  'Populist',
  'Postdemocratic',
  'Majoritarian-relativist',
  'Satisfaction w. democracy',
  'Political disaffection',
  'Political trust',
  'Self-efficacy',
  'Ambiguity intolerance',
  'General AI evaluation',
  'AI optimism',
  'AI knowledge (self-assessed)',
  'Left-Right',
  'Market-liberal',
  'Anti- vs.pro-\nimmigration',
  'Political interest',
  'Age',
  'Female',
  'High formal\neducation'
  
),
estimate = summary(m3)$coef[ , 1]  ,
std.error = summary(m3)$coef[ , 2]
)


modelframe_index <- modelframe_index[modelframe_index$term != "Intercept" , ]


p2a <- dwplot(modelframe_index, 
              vline = geom_vline(xintercept = 0,  linetype = 1), size = 2.5  ) + 
  theme_bw() + scale_colour_grey() + theme(legend.position = "") + xlab("Coefficient") + ylab("") 



# png(paste0("coefficient_plot_2.png"), width=16, height=16, units="cm", res=400)

p2a

# dev.off()





#### 7. Additional regression models ####

#### 7.1 Models with individual variables for conceptions of democracy (Annex A24 to A26) ####

# subscale 1

#### DV: Subscale 1: administrative tasks ####

m4_1b_1 <- lm(aisupport_admin ~ 
                libdem + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education,      
              data = df)


m4_1b_2 <- lm(aisupport_admin ~ 
                technocratic + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_1b_3 <- lm(aisupport_admin ~ 
                populist +       # 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_1b_4 <- lm(aisupport_admin ~ 
                postdem + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)



m4_1b_5 <- lm(aisupport_admin ~ 
                majrel + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


# wordreg( list(m4_1b_1, m4_1b_2, m4_1b_3, m4_1b_4, m4_1b_5),   file = "models_individ_var_s1.doc")



#### DV: Subscale 2: assist decision-making ####


m4_2b_1 <- lm(aisupport_assist ~ 
                libdem + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_2b_2 <- lm(aisupport_assist ~ 
                technocratic + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_2b_3 <- lm(aisupport_assist ~ 
                populist +       # 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_2b_4 <- lm(aisupport_assist ~ 
                postdem + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_2b_5 <- lm(aisupport_assist ~ 
                majrel + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)



# wordreg( list(m4_2b_1, m4_2b_2, m4_2b_3, m4_2b_4, m4_2b_5),   file = "models_individ_var_s2.doc")




#### DV: Subscale 3: replace decision-making ####


m4_3b_1 <- lm(aisupport_replace ~ 
                libdem + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)



m4_3b_2 <- lm(aisupport_replace ~ 
                technocratic + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_3b_3 <- lm(aisupport_replace ~ 
                populist +       # 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_3b_4 <- lm(aisupport_replace ~ 
                postdem + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)


m4_3b_5 <- lm(aisupport_replace ~ 
                majrel + 
                VT06_01 + 
                disaffec +       
                poltrust +
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +    
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +        
                age +        
                female + 
                education, 
              data = df)



# wordreg( list(m4_3b_1, m4_3b_2, m4_3b_3, m4_3b_4, m4_3b_5),   file = "models_individ_var_s3.doc")






#### 7.2 Models with individual items for political support (Annex A23) ####

#### DV: Subscale 1: administrative tasks ####

m4_1b_1 <- lm(aisupport_admin ~ 
              libdem + 
              technocratic +   
              populist +       
              postdem +        
              majrel +       
              VT06_01 + 

              efficacy +
              ambitol +        
              TE04_01 +        
              aioptimism +     
              knowledge  +     
              PL01_01 +        
              PL02_01 +        
              PL03_01 +        
              polint +         
              age +       
              female + 
              education,       
            data = df)

summary(m4_1b_1) 

m4_1b_2 <- lm(aisupport_admin ~ 
                libdem + 
                technocratic +   
                populist +       
                postdem +        
                majrel +       
                disaffec +      

                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +     
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +         
                age +       
                female + 
                education,     
              data = df)

summary(m4_1b_2) 

m4_1b_3 <- lm(aisupport_admin ~ 
                libdem + 
                technocratic +   
                populist +        
                postdem +         
                majrel +       
                poltrust +
                
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +     
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +         
                age +       
                female + 
                education,  
              data = df)

summary(m4_1b_3) 




#### DV: Subscale 2: assist decision-making ####

m4_2b_1 <- lm(aisupport_assist ~ 
                libdem + 
                technocratic +   
                populist +       
                postdem +        
                majrel +       
                VT06_01 + 

                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +     
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +         
                age +       
                female + 
                education,  
            data = df)

summary(m4_2b_1) 


m4_2b_2 <- lm(aisupport_assist ~ 
                libdem + 
                technocratic +   
                populist +       
                postdem +        
                majrel +       
                disaffec +       

                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +     
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +         
                age +       
                female + 
                education,  
              data = df)

summary(m4_2b_2) 


m4_2b_3 <- lm(aisupport_assist ~ 
                libdem + 
                technocratic +   
                populist +        
                postdem +        
                majrel +       
                poltrust +
                
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +     
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +         
                age +       
                female + 
                education,  
              data = df)

summary(m4_2b_3) 





#### DV: Subscale 3: replace decision-making ####

m4_3b_1 <- lm(aisupport_replace ~ 
                libdem + 
                technocratic +   
                populist +       
                postdem +         
                majrel +       
                VT06_01 + 
                
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +     
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +         
                age +       
                female + 
                education,  
            data = df)

summary(m4_3b_1) 


m4_3b_2 <- lm(aisupport_replace ~ 
                libdem + 
                technocratic +   
                populist +       
                postdem +        
                majrel +       
                disaffec +      

                
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +     
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +         
                age +       
                female + 
                education,  
              data = df)

summary(m4_3b_2) 


m4_3b_3 <- lm(aisupport_replace ~ 
                libdem + 
                technocratic +   
                populist +       
                postdem +        
                majrel +       
                poltrust +
                
                efficacy +
                ambitol +        
                TE04_01 +        
                aioptimism +     
                knowledge  +     
                PL01_01 +        
                PL02_01 +        
                PL03_01 +        
                polint +         
                age +       
                female + 
                education,  
              data = df)

summary(m4_3b_3) 


# wordreg( list(m4_1b_1, m4_1b_2, m4_1b_3,  m4_2b_1, m4_2b_2, m4_2b_3,  m4_3b_1, m4_3b_2, m4_3b_3),   file = "models_polsupp_ind.doc")






#### 7.3 Robustness checks with alternative versions of technocracy (Annex A27 to A30) ####

# Item 1

mt1a <- lm(aisupport_total ~ 
             libdem + 
             DE05_07 +   
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,       
           data = df)

summary(mt1a) 




mt1b <- lm(aisupport_admin ~ 
             libdem + 
             DE05_07 +   
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt1b) 



mt1c <- lm(aisupport_assist ~ 
             libdem + 
             DE05_07 +   
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt1c) 



mt1d <- lm(aisupport_replace ~ 
             libdem + 
             DE05_07 +   
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt1d) 


# wordreg( list(mt1b, mt1c, mt1d, mt1a), file = "technocratic_1.doc")




# Item 2

mt2a <- lm(aisupport_total ~ 
             libdem + 
             DE05_08 +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt2a) 




mt2b <- lm(aisupport_admin ~ 
             libdem + 
             DE05_08 +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)


summary(mt2b) 



mt2c <- lm(aisupport_assist ~ 
             libdem + 
             DE05_08 +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt2c) 



mt2d <- lm(aisupport_replace ~ 
             libdem + 
             DE05_08 +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt2d) 


# wordreg( list(mt2b, mt2c,mt2d, mt2a), file = "technocratic_2.doc")






# Item 3

mt3a <- lm(aisupport_total ~ 
             libdem + 
             DE05_09 +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt3a) 




mt3b <- lm(aisupport_admin ~ 
             libdem + 
             DE05_09 +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt3b) 



mt3c <- lm(aisupport_assist ~ 
             libdem + 
             DE05_09 +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt3c) 



mt3d <- lm(aisupport_replace ~ 
             libdem + 
             DE05_09 +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,  
           data = df)

summary(mt3d) 


# wordreg( list(mt3b, mt3c, mt3d, mt3a), file = "technocratic_4.doc")


# 2-item version

df$technocratic_2item <- apply( df[c( "DE05_07", "DE05_09")], 1, mean, na.rm=TRUE)

mt4a <- lm(aisupport_total ~ 
             libdem + 
             technocratic_2item +   
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,
           data = df)

summary(mt4a) 




mt4b <- lm(aisupport_admin ~ 
             libdem + 
             technocratic_2item +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,
           data = df)

summary(mt4b) 



mt4c <- lm(aisupport_assist ~ 
             libdem + 
             technocratic_2item +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,
           data = df)

summary(mt4c) 



mt4d <- lm(aisupport_replace ~ 
             libdem + 
             technocratic_2item +   #
             populist +       
             postdem +        
             majrel + 
             VT06_01 + 
             disaffec +       
             poltrust +
             efficacy +
             ambitol +        
             TE04_01 +        
             aioptimism +     
             knowledge  +     
             PL01_01 +        
             PL02_01 +        
             PL03_01 +       
             polint +        
             age +       
             female + 
             education,
           data = df)

summary(mt4d) 


# wordreg( list(mt4b, mt4c, mt4d, mt4a), file = "technocratic_4.doc")






#### Sensitivity analysis (Annex A31) ####

# https://github.com/jrosen48/konfound/

summary(m1_1b)

konfound(m1_1b, TE04_01, index = "RIR")
konfound(m1_1b, TE04_01, index = "IT")

konfound(m1_1b, aioptimism, index = "RIR")
konfound(m1_1b, aioptimism, index = "IT")

konfound(m1_1b, PL03_01, index = "RIR")
konfound(m1_1b, PL03_01, index = "IT")

konfound(m1_1b, age, index = "RIR")
konfound(m1_1b, age, index = "IT")

konfound(m1_1b, female, index = "RIR")
konfound(m1_1b, female, index = "IT")

konfound(m1_1b, education, index = "RIR")
konfound(m1_1b, education, index = "IT")



summary(m1_2b)

konfound(m1_2b, postdem, index = "RIR")
konfound(m1_2b, postdem, index = "IT")

konfound(m1_2b, TE04_01, index = "RIR")
konfound(m1_2b, TE04_01, index = "IT")

konfound(m1_2b, aioptimism, index = "RIR")
konfound(m1_2b, aioptimism, index = "IT")

konfound(m1_2b, polint, index = "RIR")
konfound(m1_2b, polint, index = "IT")

konfound(m1_2b, age, index = "RIR")
konfound(m1_2b, age, index = "IT")



summary(m1_3b)

konfound(m1_3b, populist, index = "RIR")
konfound(m1_3b, populist, index = "IT")

konfound(m1_3b, postdem, index = "RIR")
konfound(m1_3b, postdem, index = "IT")

konfound(m4_3b, majrel, index = "RIR")
konfound(m4_3b, majrel, index = "IT")

konfound(m1_3b, libdem, index = "RIR")
konfound(m1_3b, libdem, index = "IT")

konfound(m1_3b, VT06_01, index = "RIR")
konfound(m1_3b, VT06_01, index = "IT")

konfound(m1_3b, aioptimism, index = "RIR")
konfound(m1_3b, aioptimism, index = "IT")





  