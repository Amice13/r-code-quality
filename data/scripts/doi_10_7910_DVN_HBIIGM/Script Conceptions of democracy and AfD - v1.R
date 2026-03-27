


# Script for: Support for a populist form of democratic politics or political discontent? How conceptions of democracy relate to support for the AfD
#
# Version: June 28, 2022
#
# Author: Pascal D. K�nig




#### Load packages ####

library(mediation)
library(RColorBrewer)
library(psych)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(corrplot)
library(texreg)
library(Hmisc)
library(lavaan)
library(semPlot)
library(car)
library(margins)
library(dotwhisker)
library(lavaan)
library(plotrix)
library(rcompanion)



#### Apply filters ####

# Attention check
df <- df[df$TE06_05 == 1, ]

# Control question (at the end of the survey)
df <- df[df$KO01 == "Ja, ich habe den Fragebogen aufmerksam und nach bestem Wissen und Gewissen beantwortet. Meine Angaben können zur Auswertung verwendet werden.", ]

# Speeding (at least 5 min. = 300 Sec.)
sum(df$TIME_SUM < 300, na.rm = T)
df <- df[df$TIME_SUM >= 300, ]






#### 1. Prepare variables ####

#### 1.1 Political trust and satisfaction with democracy ####

df$PL06_03r <- 6-df$PL06_03
df$PL06_07r <- 6-df$PL06_07

tmp <- df[ , c("VT04_01",   
               "VT04_02",
               "VT04_03", 
               "VT04_04",
               "VT04_05",
               "PL06_01", 
               "PL06_02",
               "PL06_03r",
               "PL06_04",
               "PL06_07r") ]
df.compl <- tmp[complete.cases(tmp), ]


fa.out <- factanal( df.compl[c( "VT04_01",   
                      "VT04_02",
                      "VT04_03", 
                      "VT04_04",
                      "VT04_05",
                      "PL06_01", 
                      "PL06_02",
                      "PL06_03r",
                      "PL06_04",
                      "PL06_07r")], factors = 3, rotation = "varimax"
)

unclass(loadings(fa.out))


fa.out <- factanal( df.compl[c( "VT04_01",   
                      "VT04_02",
                      "VT04_03", 
                      "VT04_04",
                      "PL06_01", 
                      "PL06_02",
                      "PL06_04",
                      "PL06_07r")], factors = 2, rotation = "varimax"
)

unclass(loadings(fa.out))



psych::alpha(df[c("VT04_01",   
                  "VT04_02",
                  "VT04_03", 
                  "VT04_04") ],
             check.keys=TRUE) 

df$poltrust <- apply( df[c( "VT04_01",   
                            "VT04_02",
                            "VT04_03", 
                            "VT04_04")], 1, mean, na.rm=TRUE)


# write.csv(unclass(loadings(fa.out)), file = "pca_loadings_polsupport.csv")



psych::alpha(df[c("PL06_01", 
                  "PL06_02",
                  "PL06_04",
                  "PL06_07r") ],
             check.keys=TRUE) 


df$disaffec <- apply( df[c( "PL06_01", 
                            "PL06_02",
                            "PL06_04",
                            "PL06_07r")], 1, mean, na.rm=TRUE)


#### 1.2 Self- efficacy ####

df$PL06_06r <- 6-df$PL06_06

df$efficacy <- apply( df[c( "PL06_05", 
                            "PL06_06r")], 1, mean, na.rm=TRUE)

df$efficacy <- 6 - df$efficacy



#### 1.3 Satisfaction with democracy ####

df$VT06_01 # demsat


#### 1.4 Ambiguity tolerance ####


df$PS04_01r <- 6 - df$PS04_01 
df$PS04_03r <- 6 - df$PS04_03


tmp <- df[ , c("PS04_01r", 
               "PS04_02",
               "PS04_03r",
               "PS04_04", 
               "PS04_06",
               "PS04_08") ]
df.compl <- tmp[complete.cases(tmp), ]


fa.out <- factanal( df.compl[c( "PS04_01r", 
                                "PS04_02",
                                "PS04_03r",
                                "PS04_04", 
                                "PS04_06",
                                "PS04_08")], factors = 2, rotation = "varimax"
)

fa.out


psych::alpha(df[c("PS04_01r", 
                  "PS04_02",
                  "PS04_03r",
                  "PS04_04", 
                  "PS04_06",
                  "PS04_08") ],
             check.keys=TRUE) 


df$ambitol <- apply( df[c( "PS04_01r", 
                           "PS04_02",
                           "PS04_03r",
                           "PS04_04", 
                           "PS04_06",
                           "PS04_08")], 1, mean, na.rm=TRUE)




#### 1.5 Policy preferences ####

df$PL02_01_num <- as.numeric(df$PL02_01) # Economic policy

df$PL03_01_num <- as.numeric(df$PL03_01) # Immigration policy




#### 1.6 Political interest and sociodemographics ####

# political interest

df$polint <- as.numeric(df$VT05)

# education

df$education <- 0 
df$education[df$DG03 == "Fachhochschulreife (Abschluss einer Fachoberschule etc.)"] <- 1
df$education[df$DG03 == "Abitur bzw. erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)"] <- 1
table(df$education)

# gender

df$female <- 0
df$female[df$DG15 == "weiblich"] <- 1

# age

df$age


#### 1.7 Vote intention ####

# AfD 
df$voterrp <- 0
df$voterrp[ is.na(df$PL07) == T] <- NA
df$voterrp[ df$PL07 == "AfD"] <- 1
table(df$voterrp)

# AfD with missings as no vote intention
df$voterrp_nomiss <- 0
df$voterrp_nomiss[ is.na(df$PL07) == T] <- 0
df$voterrp_nomiss[ df$PL07 == "AfD"] <- 1
table(df$voterrp_nomiss)

# AfD and Left Party
df$voterrp2 <- 0
df$voterrp2[ is.na(df$PL07) == T] <- NA
df$voterrp2[ df$PL07 == "AfD"] <- 1
df$voterrp2[ df$PL07 == "Die Linke"] <- 1
table(df$voterrp2)

# Would not vote as missing
df$voterrp3 <- 0
df$voterrp3[ is.na(df$PL07) == T] <- NA
df$voterrp3[ df$PL07 == "Würde nicht wählen"] <- NA
df$voterrp3[ df$PL07 == "AfD"] <- 1
table(df$voterrp3)

# Vote opposition
df$voteopp <- 0
df$voteopp[ is.na(df$PL07) == T] <- NA
df$voteopp[ df$PL07 == "CDU/CSU"] <- 1
df$voteopp[ df$PL07 == "SPD"] <- 1
table(df$voteopp)





#### 1.8 Populist, majoritarian-relativist, pluralist ####

# Exploratory factor analysis

tmp <- df[ , c("DE05_01", # populist
               "DE05_02", # populist
               "DE05_03", # populist
               "DE05_10", # majoritarian-relativist
               "DE05_11", # majoritarian-relativist
               "DE05_12", # majoritarian-relativist
               "DE08_05", # pluralist
               "DE08_06", # pluralist
               "DE08_07", # pluralist
               "DE08_08"  # pluralist
) ]
tmp <- tmp[complete.cases(tmp), ]


fa.out <- factanal(tmp[c("DE05_01", # populist
                         "DE05_02", # populist
                         "DE05_03", # populist
                         "DE05_10", # majoritarian-relativist
                         "DE05_11", # majoritarian-relativist
                         "DE05_12", # majoritarian-relativist
                         "DE08_05", # pluralist
                         "DE08_06", # pluralist
                         "DE08_07" # pluralist
                         
)], 
factors = 3, rotation = "promax")

fa.out$loadings
fa.out

KMO(tmp)

# write.csv(unclass(loadings(fa.out)), file = "pca.loadings.csv")





# COnfirmatory factor analysis 

model <- '
  # measurement model

    populist  =~ DE05_01 + DE05_02 + DE05_03 
    
    majrel  =~ DE05_10 + DE05_11 + DE05_12
      
    pluralist  =~  DE08_05 + DE08_06 + DE08_07
    

'


fit <- cfa(model, data = df )

summary(fit, fit.measures=TRUE)

summary(fit, standardized = TRUE, rsq = T)




# Generate variables for conceptions of democracy

# Populist
psych::alpha(df[c("DE05_01", 
                  "DE05_02",
                  "DE05_03") ],
             check.keys=TRUE) 

df$populist <- apply( df[c( "DE05_01", 
                            "DE05_02",
                            "DE05_03")], 1, mean, na.rm=TRUE)


# Majoritarian-relativist
psych::alpha(df[c("DE05_10", 
                  "DE05_11",
                  "DE05_12") ],
             check.keys=TRUE) 

df$majrel <- apply( df[c( "DE05_10", 
                            "DE05_11",
                            "DE05_12")], 1, mean, na.rm=TRUE)


# Pluralist
psych::alpha(df[c("DE08_05",   
                  "DE08_06",
                  "DE08_07") ],
             check.keys=TRUE) 


df$pluralist <- apply( df[c( "DE08_05",   
                             "DE08_06",
                             "DE08_07")], 1, mean, na.rm=TRUE)


# Generate relative variables (Score on one conception minus mean on the other conceptions# 

df$populist.diff <- df$populist - (  df$majrel + df$pluralist)/2
df$majrel.diff <- df$majrel - ( df$populist + df$pluralist)/2
df$pluralist.diff <- df$pluralist - ( df$populist + df$majrel)/2




#### 2. Norm variables ####

df$populist_n       <- (df$populist-1)/6
df$majrel_n         <- (df$majrel-1)/6
df$pluralist_n      <- (df$pluralist-1)/6

df$VT06_01_n        <- (df$VT06_01-1)/6  # satisfaction with democracy
df$disaffec_n       <- (df$disaffec-1)/4
df$poltrust_n       <- (df$poltrust-1)/6

df$efficacy_n       <- (df$efficacy-1)/4
df$PL01_01_n        <- (df$PL01_01-1)/6
df$PL02_01_n        <- (df$PL02_01-1)/6
df$PL03_01_n        <- (df$PL03_01-1)/6

df$ambitol_n        <- (df$ambitol-1)/4
df$polint_n         <- (df$polint-1)/4
df$age_n            <- df$age/74




#### 3. Description ####

# Variable overview

desc_1 <- psych::describe(df[c('populist',
                        'majrel',
                        'pluralist',
                        'VT06_01',
                        'disaffec',
                        'poltrust',
                        'PL01_01',
                        'PL02_01',
                        'PL03_01',
                        'ambitol',
                        'polint', 
                        'age',
                        'female',
                        'education')] 
) 


# write.csv(desc_1, "var_desc.csv")



# description for reduced dataset

tmp <- df[ , c("voterrp",
               "populist_n",
               "majrel_n",
               "pluralist_n",
               "VT06_01_n",
               "disaffec_n",
               "efficacy_n",
               "poltrust_n", 
               "PL01_01_n", 
               "PL02_01_n", 
               "PL03_01_n", 
               "ambitol_n",
               "polint_n", 
               "age_n", 
               "female", 
               "education"
) ]

df_reduced <- df[complete.cases(tmp), ]



desc_2 <- psych::describe(df_reduced[c('populist',
                               'majrel',
                               'pluralist',
                               'VT06_01',
                               'disaffec',
                               'poltrust',
                               'PL01_01',
                               'PL02_01',
                               'PL03_01',
                               'ambitol',
                               'polint', 
                               'age',
                               'female',
                               'education')] 
) 


# write.csv(desc_2, "var_desc2.csv")




# description for reduced dataset with DV that includes missings as 0 - voterrp_nomiss

tmp <- df[ , c("voterrp_nomiss",
               "populist_n",
               "majrel_n",
               "pluralist_n",
               "VT06_01_n",
               "disaffec_n",
               "poltrust_n", 
               "PL01_01_n", 
               "PL02_01_n", 
               "PL03_01_n", 
               "ambitol_n",
               "polint_n", 
               "age_n", 
               "female", 
               "education",
               "DG03",
               "DG15",
               "age"
) ]

df_reduced2 <- df[complete.cases(tmp), ]



desc_3 <- psych::describe(df_reduced2[c('populist',
                                       'majrel',
                                       'pluralist',
                                       'VT06_01',
                                       'disaffec',
                                       'poltrust',
                                       'PL01_01',
                                       'PL02_01',
                                       'PL03_01',
                                       'ambitol',
                                       'polint', 
                                       'age',
                                       'female',
                                       'education')] 
) 

# write.csv(desc_3, "var_desc3.csv")


# Visualization of populism, majoritarian relativism, and pluralism

tmp.frame <- data.frame(var = c('Populist',
                                'Majoritarian-relativist',
                                'Pluralist'),
                        
                        mean = c( mean(df$populist, na.rm = T), 
                                  mean(df$majrel, na.rm = T),
                                  mean(df$pluralist, na.rm = T)  ),
                        sd = c( std.error(df$populist, na.rm = T), 
                                std.error(df$majrel, na.rm = T),
                                std.error(df$pluralist, na.rm = T)   )
) 


colnames(tmp.frame) <- c("Variable", "Mean", "SD")
tmp.frame$Variable <- factor(tmp.frame$Variable, levels =  c('Populist',
                                                             'Majoritarian-relativist',
                                                             'Pluralist') )


# png(paste0("variable_description.png"), width=9, height=6, units="cm", res=400)

ggplot( data=tmp.frame, aes(x = reorder(Variable, as.numeric(Variable)), y = Mean) ) + 
  geom_bar(stat = "identity", fill = "grey60") + xlab("") + 
  geom_errorbar( aes(ymin=Mean-1.96*SD, ymax=Mean + 1.96*SD), size = 0.2,  width = 0.1) + theme_bw() + ylim( c(0, 7) )

# dev.off()







#### 4. Correlations ####

cor.tmp <- df[c("populist",
                "majrel",
                "pluralist",
                "VT06_01",
                "disaffec",
                "poltrust")]

cor.out <- rcorr(as.matrix(cor.tmp))

cor.out$r
cor.out$r2 <- round(cor.out$r, 2)
cor.out$P
cor.out$P2 <- cor.out$P
cor.out$P2[cor.out$P >= 0.05] <- ""
cor.out$P2[cor.out$P < 0.05] <- "*"
cor.out$P2[cor.out$P < 0.01] <- "**"
cor.out$P2 



# Inspect individual items for the main independent variables
cor.tmp <- df[c("DE05_01", # populist
                "DE05_02", # populist
                "DE05_03", # populist
                "DE05_10", # majrel
                "DE05_11", # majrel
                "DE05_12", # majrel
                "DE08_05", # pluralist
                "DE08_06", # pluralist
                "DE08_07" # pluralist
                )]

cor.out <- rcorr(as.matrix(cor.tmp))

cor.out$r
cor.out$r2 <- round(cor.out$r, 2)
cor.out$P
cor.out$P2 <- cor.out$P
cor.out$P2[cor.out$P >= 0.05] <- ""
cor.out$P2[cor.out$P < 0.05] <- "*"
cor.out$P2[cor.out$P < 0.01] <- "**"
cor.out$P2 






#### 5. Logistic regression models ####

tmp <- df[ , c("populist_n",
               "majrel_n",
               "pluralist_n",
               "VT06_01_n",
               "disaffec_n",
               "poltrust_n", 
               "PL01_01_n", 
               "PL02_01_n", 
               "PL03_01_n", 
               "ambitol_n",
               "polint_n", 
               "age_n", 
               "female", 
               "education"
) ]

df2 <- df[complete.cases(tmp), ]


# Normed variables: Without political support
m1_n <- glm(voterrp ~ populist_n + majrel_n + pluralist_n + 
             PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
           data = df2, family = "binomial")
summary(m1_n)

# Normed variables: With political support
m2_n <- glm(voterrp ~ populist_n + majrel_n + pluralist_n + 
                VT06_01_n + disaffec_n + poltrust_n + 
              PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
            data = df2, family = "binomial")

summary(m2_n) 



# htmlreg( list(m1_n, m2_n),   file = "Regressions_mc.htm", stars = c(0.01, 0.05, 0.1))


nagelkerke(m1_n)




# Check variance inflation 

m2_n_vif <- lm(voterrp ~  populist + majrel + pluralist + 
                   VT06_01 + disaffec + poltrust + 
                   PL01_01 + PL02_01 + PL03_01 + ambitol + polint + age + female + education,
                 data = df2)
vif(m2_n_vif)




#### 5. Average Marginal Effects and visualization ####

#### 5.1 Model without political support variables ####

margins.out1 <- margins(m1_n)

summary(margins.out1)$AME


modelframe1 <- data.frame(term = c(
  'Age',
  'Uncertainty avoidance',
  'High formal\neducation',
  'Female',
  'Majoritarian-relativist',
  
  'Left-Right',
  'Market-liberal',
  'Anti- vs.pro-\nimmigration',
  
  'Pluralist',
  
  'Political interest',
  
  'Populist'
  
),
estimate = summary(margins.out1)$AME  ,
std.error = summary(margins.out1)$SE
)


p1a <- dwplot(modelframe1, 
              vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1),  
              vars_order = c(
                'Populist',
                'Majoritarian-relativist',
                'Pluralist',
                
                'Left-Right',
                'Market-liberal',
                'Anti- vs.pro-\nimmigration',
                'Uncertainty avoidance',
                'Political interest',
                'High formal\neducation',
                'Female',
                'Age'
              )
) + 
  theme_bw() + scale_colour_grey() + theme(legend.position = "none") + xlab("Coefficient") + ylab("")  




# png(paste0("coefficient_plot.png"), width=16, height=16, units="cm", res=400)

p1a

# dev.off()




#### 5.2 Model with political support variables ####

margins.out2 <- margins(m2_n)

summary(margins.out2)$AME


modelframe2 <- data.frame(term = c(
  'Age',
  'Uncertainty avoidance',
  'Political disaffection',
  'High formal\neducation',
  'Female',
  'Majoritarian-relativist',
  
  'Left-Right',
  'Market-liberal',
  'Anti- vs.pro-\nimmigration',
  
  'Pluralist',
  
  'Political interest',
  
  'Political trust',
  
  'Populist',
  
  "Satisf. w. democracy"
  
),
estimate = summary(margins.out2)$AME  ,
std.error = summary(margins.out2)$SE
)


p1b <- dwplot(modelframe2, 
              vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1),  
              vars_order = c(
                'Populist',
                'Technocratic',
                'Post-democratic',
                'Majoritarian-relativist',
                'Pluralist',
                "Satisf. w. democracy",
                'Political disaffection',
                'Political trust',
                'Efficacy',
                'Left-Right',
                'Market-liberal',
                'Anti- vs.pro-\nimmigration',
                'Uncertainty avoidance',
                'Political interest',
                'High formal\neducation',
                'Female',
                'Age'
              )
) + 
  theme_bw() + scale_colour_grey() + theme(legend.position = "none") + xlab("Coefficient") + ylab("")  




# png(paste0("coefficient_plot.png"), width=16, height=16, units="cm", res=400)

p1b

# dev.off()


modelframe1$model <- "Without variables for\npolitical support"
modelframe2$model <- "With variables for\npolitical support"

modelframe <- rbind(modelframe1, modelframe2)


p2 <- 
  dwplot( modelframe , 
          vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1), 
          vars_order = c(
            'Populist',
            'Technocratic',
            'Post-democratic',
            'Majoritarian-relativist',
            'Pluralist',
            'Satisf. w. democracy',
            'Political disaffection',
            'Political trust',
            'Efficacy',
            'Left-Right',
            'Market-liberal',
            'Anti- vs.pro-\nimmigration',
            'Uncertainty avoidance',
            'Political interest',
            'High formal\neducation',
            'Female',
            'Age'
          ), 
          dot_args = list(aes(shape = model) , size = 2.5  ),
          model_order = c("Without variables for\npolitical support", 
                          "With variables for\npolitical support")  ) + 
  theme_bw( ) + xlab("Average marginal effect") + ylab("") +  
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.title = element_blank() ) + 
  scale_color_manual(values = c("grey65",  "black") , name = "Model", breaks=c("Without variables for\npolitical support", "With variables for\npolitical support") ) + 
  scale_shape_discrete(name = "Model", breaks = c("Without variables for\npolitical support", "With variables for\npolitical support") )  +
  guides(
    shape = guide_legend("Model"), 
    colour = guide_legend("Model"),
    size = guide_legend("Model")
  )


# png(paste0("coefficient_plot_2models.png"), width=14, height=16, units="cm", res=400)

p2

# dev.off()





#### 6. Additional logistic regression tables for the annex ####

#### 6.1 Annex A10: Multinomial regression ####

table(df$PL07)
str(df$PL07)

df$voteall <- as.character(df$PL07)
table(df$voteall)
df$voteall[df$voteall == "Würde nicht wählen"] <- NA
df$voteall <- factor(df$voteall, levels = c("CDU/CSU", 
                                            "SPD", 
                                            "FDP", 
                                            "B90/Die Grünen", 
                                            "Die Linke", 
                                            "AfD",
                                            "Andere:"))

tmp <- df[ , c("populist_n",
               "majrel_n",
               "pluralist_n",
               "VT06_01_n",
               "disaffec_n",
               "poltrust_n", 
               "PL01_01_n", 
               "PL02_01_n", 
               "PL03_01_n", 
               "ambitol_n",
               "polint_n", 
               "age_n", 
               "female", 
               "education"
) ]

df2 <- df[complete.cases(tmp), ]

library(nnet)
m_multinom <- multinom(voteall ~ populist_n + majrel_n + pluralist_n + 
                   VT06_01_n + disaffec_n + poltrust_n + 
                   PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
                 data = df2)


summary(m_multinom)

z <- summary(m_multinom)$coefficients/summary(m_multinom)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p



library(VGAM)
vglmFitMN <- vglm(voteall ~ populist_n + majrel_n + pluralist_n + 
                    VT06_01_n + disaffec_n + poltrust_n + 
                    PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
                  family=multinomial(refLevel=1),
                  model=TRUE, data=df2)
vglmFitMN

library(DescTools)
PseudoR2(vglmFitMN, which = "Nagelkerke")


extract(
  m_multinom,
  include.pvalues = TRUE,
  include.aic = TRUE,
  include.bic = TRUE,
  include.nobs = TRUE)

# htmlreg( m_multinom,   file = "Regressions_multinom.htm", stars = c(0.01, 0.05, 0.1))




#### 6.2 Annex A11 and 12: Models with individual predictors / items ####

# Only populism
m_a1_1 <- glm(voterrp ~ populist_n +
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_a1_1) 

m_a1_2 <- glm(voterrp ~ populist_n +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_a1_2) 


# Only majoritarian relativisn
m_a2_1 <- glm(voterrp ~ majrel_n +
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_a2_1) 

m_a2_2 <- glm(voterrp ~ majrel_n +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_a2_2) 



# Only pluralism
m_a3_1 <- glm(voterrp ~ pluralist_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_a3_1) 

m_a3_2 <- glm(voterrp ~ pluralist_n + 
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_a3_2) 


# htmlreg( list(m_a1_1, m_a1_2, m_a2_1, m_a2_2, m_a3_1, m_a3_2),   file = "Regressions_annex_a10.htm", stars = c(0.01, 0.05, 0.1))


nagelkerke(m_a1_1)
nagelkerke(m_a1_2)
nagelkerke(m_a2_1)
nagelkerke(m_a2_2)
nagelkerke(m_a3_1)
nagelkerke(m_a3_2)



# Individual items

m_ind1 <- glm(voterrp ~ 
                DE05_01 + 
                #DE05_02 + 
                #DE05_03 + 
                #DE05_10 + 
                #DE05_11 + 
                #DE05_12 + 
                #DE08_05 + 
                #DE08_06 + 
                #DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

m_ind2 <- glm(voterrp ~ 
                #DE05_01 + 
                DE05_02 + 
                #DE05_03 + 
                #DE05_10 + 
                #DE05_11 + 
                #DE05_12 + 
                #DE08_05 + 
                #DE08_06 + 
                #DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

m_ind3 <- glm(voterrp ~ 
                #DE05_01 + 
                #DE05_02 + 
                DE05_03 + 
                #DE05_10 + 
                #DE05_11 + 
                #DE05_12 + 
                #DE08_05 + 
                #DE08_06 + 
                #DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

m_ind4 <- glm(voterrp ~ 
                #DE05_01 + 
                #DE05_02 + 
                #DE05_03 + 
                DE05_10 + 
                #DE05_11 + 
                #DE05_12 + 
                #DE08_05 + 
                #DE08_06 + 
                #DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

m_ind5 <- glm(voterrp ~ 
                #DE05_01 + 
                #DE05_02 + 
                #DE05_03 + 
                #DE05_10 + 
                DE05_11 + 
                #DE05_12 + 
                #DE08_05 + 
                #DE08_06 + 
                #DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

m_ind6 <- glm(voterrp ~ 
                #DE05_01 + 
                #DE05_02 + 
                #DE05_03 + 
                #DE05_10 + 
                #DE05_11 + 
                DE05_12 + 
                #DE08_05 + 
                #DE08_06 + 
                #DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

m_ind7 <- glm(voterrp ~ 
                #DE05_01 + 
                #DE05_02 + 
                #DE05_03 + 
                #DE05_10 + 
                #DE05_11 + 
                #DE05_12 + 
                DE08_05 + 
                #DE08_06 + 
                #DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

m_ind8 <- glm(voterrp ~ 
                #DE05_01 + 
                #DE05_02 + 
                #DE05_03 + 
                #DE05_10 + 
                #DE05_11 + 
                #DE05_12 + 
                #DE08_05 + 
                DE08_06 + 
                #DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

m_ind9 <- glm(voterrp ~ 
                #DE05_01 + 
                #DE05_02 + 
                #DE05_03 + 
                #DE05_10 + 
                #DE05_11 + 
                #DE05_12 + 
                #DE08_05 + 
                #DE08_06 + 
                DE08_07 +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")


# htmlreg( list(m_ind1, m_ind2, m_ind3, m_ind4, m_ind5, m_ind6, m_ind7, m_ind8, m_ind9),  file = "Regressions_individ_items_annex_a11.htm", stars = c(0.01, 0.05, 0.1))

c(nagelkerke( m_ind1)$Pseudo.R.squared.for.model.vs.null[3],
    nagelkerke( m_ind2)$Pseudo.R.squared.for.model.vs.null[3],
    nagelkerke( m_ind3)$Pseudo.R.squared.for.model.vs.null[3],
    nagelkerke( m_ind4)$Pseudo.R.squared.for.model.vs.null[3],
    nagelkerke( m_ind5)$Pseudo.R.squared.for.model.vs.null[3],
    nagelkerke( m_ind6)$Pseudo.R.squared.for.model.vs.null[3],
    nagelkerke( m_ind7)$Pseudo.R.squared.for.model.vs.null[3],
    nagelkerke( m_ind8)$Pseudo.R.squared.for.model.vs.null[3],
    nagelkerke( m_ind9)$Pseudo.R.squared.for.model.vs.null[3] 
)




#### 6.3 Annex A13: Subtractive main predictors (relative variables) ####

m_b1_1 <- glm(voterrp ~ populist.diff +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_b1_1) 


m_b2_1 <- glm(voterrp ~ majrel.diff +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_b2_1) 

m_b3_1 <- glm(voterrp ~ pluralist.diff +
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_b3_1) 



# htmlreg( list(m_b1_1, m_b2_1, m_b3_1),   file = "Regressions_annex_12.htm", stars = c(0.01, 0.05, 0.1))

nagelkerke(m_b1_1)
nagelkerke(m_b2_1)
nagelkerke(m_b3_1)





#### 6.4 Annex A14: Opposition party support ####

mc2_opp_n <- glm(voteopp ~ populist_n + majrel_n + pluralist_n + 
                   PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
                 data = df2, family = "binomial")
summary(mc2_opp_n)

mc3c_opp_n <- glm(voteopp ~ populist_n + majrel_n + pluralist_n + 
                    VT06_01_n + disaffec_n + poltrust_n + 
                    PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
                  data = df2, family = "binomial")

summary(mc3c_opp_n) 

# htmlreg( list(mc2_opp_n, mc3c_opp_n),   file = "Regressions_mc_opp_annex_13.htm", stars = c(0.01, 0.05, 0.1))

nagelkerke(mc2_opp_n)
nagelkerke(mc3c_opp_n)





#### 6.5 Annex A15: without would not vote and with missings ####

tmp <- df[ , c("populist_n",
               "majrel_n",
               "pluralist_n",
               "VT06_01_n",
               "disaffec_n",
               "poltrust_n", 
               "PL01_01_n", 
               "PL02_01_n", 
               "PL03_01_n", 
               "ambitol_n",
               "polint_n", 
               "age_n", 
               "female", 
               "education"
) ]

df2 <- df[complete.cases(tmp), ]



m_a14_1 <- glm(voterrp3 ~ populist_n + majrel_n + pluralist_n + 
               PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
             data = df2, family = "binomial")
summary(m_a14_1)


m_a14_2 <- glm(voterrp3 ~ populist_n + majrel_n + pluralist_n + 
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_a14_2) 

m_a14_3 <- glm(voterrp_nomiss ~ populist_n + majrel_n + pluralist_n + 
               PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
             data = df2, family = "binomial")
summary(m_a14_3)


m_a14_4 <- glm(voterrp_nomiss ~ populist_n + majrel_n + pluralist_n + 
                VT06_01_n + disaffec_n + poltrust_n + 
                PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
              data = df2, family = "binomial")

summary(m_a14_4) 



# htmlreg( list(m_a14_1, m_a14_2, m_a14_3, m_a14_4),   file = "Regressions_annex_14__nomiss.htm", stars = c(0.01, 0.05, 0.1))

nagelkerke(m_a14_1)
nagelkerke(m_a14_2)

nagelkerke(m_a14_3)
nagelkerke(m_a14_4)






#### 6.6 Annex A15: With left party in category 1 ####

mc2_left <- glm(voterrp2 ~ populist + majrel + pluralist + 
                  PL01_01 + I(PL01_01_n^2)+ PL02_01 + PL03_01 + ambitol + polint + age + female + education,
                data = df2, family = "binomial")
summary(mc2_left)

mc2_left_n <- glm(voterrp2 ~ populist_n + majrel_n + pluralist_n + 
                    PL01_01_n + I(PL01_01_n^2)+ PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
                  data = df2, family = "binomial")
summary(mc2_left_n)


mc3c_left <- glm(voterrp2 ~ populist + majrel + pluralist + 
                   VT06_01 + disaffec + poltrust +
                   PL01_01 + I(PL01_01_n^2)+ PL02_01 + PL03_01 + ambitol + polint + age + female + education,
                 data = df2, family = "binomial")

summary(mc3c_left) 


mc3c_left_n <- glm(voterrp2 ~ populist_n + majrel_n + pluralist_n + 
                     VT06_01_n + disaffec_n + poltrust_n + 
                     PL01_01_n+ I(PL01_01_n^2) + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
                   data = df2, family = "binomial")

summary(mc3c_left_n) 



# htmlreg( list(mc2_left_n, mc3c_left_n),   file = "Regressions_mc_left1.htm", stars = c(0.01, 0.05, 0.1))

nagelkerke(mc2_left_n)
nagelkerke(mc3c_left_n)




#### 6.7 Annex A15: Without left party in category 0 ####


mc2_left <- glm(voterrp2 ~ populist + majrel + pluralist + 
                  PL01_01 + PL02_01 + PL03_01 + ambitol + polint + age + female + education,
                data = df2, family = "binomial")
summary(mc2_left)

mc2_left_n <- glm(voterrp2 ~ populist_n + majrel_n + pluralist_n + 
                    PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
                  data = df2, family = "binomial")
summary(mc2_left_n)


mc3c_left <- glm(voterrp2 ~ populist + majrel + pluralist + 
                   VT06_01 + disaffec + poltrust +
                   PL01_01 + PL02_01 + PL03_01 + ambitol + polint + age + female + education,
                 data = df2, family = "binomial")

summary(mc3c_left) 


mc3c_left_n <- glm(voterrp2 ~ populist_n + majrel_n + pluralist_n + 
                     VT06_01_n + disaffec_n + poltrust_n + 
                     PL01_01_n + PL02_01_n + PL03_01_n + ambitol_n + polint_n + age_n + female + education,
                   data = df2, family = "binomial")

summary(mc3c_left_n) 



# htmlreg( list(mc2_left_n, mc3c_left_n),   file = "Regressions_mc_left2.htm", stars = c(0.01, 0.05, 0.1))

nagelkerke(mc2_left_n)
nagelkerke(mc3c_left_n)






