
# Script for: Cause for celebration or concern
#
# Version: February 8, 2024
#


#### 1 Load packages ####

library(RColorBrewer)
library(psych)
library(FactoMineR)
library(factoextra)
library(lattice)
library(MASS)
library(tidyr)
library(corrplot)
library(texreg)
library(Hmisc)
library(fmsb)
library(dotwhisker)
library(gridExtra)
library(mediation)
library(ggeffects)
library(plotrix)




#### 2 Prepare  variables ####

#### :: 2.1 Prepare dependent variable preferences for housing policy ####

# Inspect scree plot
scree(df[c("TE11_01", 
           "TE11_02",
           "TE11_03",
           "TE11_04", 
           "TE11_05", 
           "TE11_06", 
           "TE11_07", 
           "TE11_08",
           "TE11_09"
)], pc=T, fa = T) 



# 3-factor solution (Appendix Table 11)
fa.out <- factanal( df[c( "TE11_01", 
                          "TE11_02",
                          "TE11_03",
                          "TE11_04", 
                          "TE11_05", 
                          "TE11_06", 
                          "TE11_07", 
                          "TE11_08",
                          "TE11_09")], factors = 3, rotation = "promax"
)
fa.out


# 2-factor solution
fa.out <- factanal( df[c( "TE11_01", 
                          "TE11_02",
                          "TE11_03",
                        # "TE11_04", 
                          "TE11_05", 
                          "TE11_06", 
                          "TE11_07", 
                          "TE11_08",
                          "TE11_09")], factors = 2, rotation = "promax"
)

fa.out


# write.csv(unclass(loadings(fa.out)), file = "pca_loadings_housing_pol.csv")

# Cronbachs Alpha for owner-oriented policy preferences
psych::alpha(df[c("TE11_01", 
                  "TE11_02",
                  "TE11_03"
) ],
check.keys=TRUE) # 


df$hou_pol1 <- apply( df[c( "TE11_01", 
                           "TE11_02",
                           "TE11_03"
)], 1, mean, na.rm=TRUE) # 


# Cronbachs Alpha for renter-oriented policy preferences
psych::alpha(df[c("TE11_05", 
                  "TE11_06",
                  "TE11_07",
                  "TE11_09"
) ],
check.keys=TRUE) # 


df$hou_pol2 <- apply( df[c( "TE11_05", 
                            "TE11_06",
                            "TE11_07", 
                            "TE11_09"
)], 1, mean, na.rm=TRUE) # 



### ::  2.2 Financialization attitudes (Table in Annex A15) ####

# Exploratory Factor Analysis

# Inspect scree plot
scree(df[c("TE29_01", 
           "TE29_02",
           "TE29_03",
           "TE29_04", 
           "TE29_05", 
           "TE29_06", 
           "TE29_07", 
           "TE29_08",
           "TE29_09",
           "TE29_10",
           "TE29_11",
           "TE29_12"
)], pc=T, fa = T) 



fa.out <- factanal( df[c("TE29_01", 
                         "TE29_02",
                         "TE29_03",
                         "TE29_04", 
                         "TE29_05", 
                         "TE29_06", 
                         "TE29_07", 
                         "TE29_08",
                         "TE29_09",
                         "TE29_10",
                         "TE29_11",
                         "TE29_12"
)], factors = 2, rotation = "promax"
)

fa.out


# write.csv(unclass(loadings(fa.out)), file = "pca_loadings_fin_att.csv")


# Cronbachs Alpha for the items loading high on factor 1 (item 10 only slightly below 0.5, all others above 0.5)
psych::alpha(df[c( "TE29_05", 
                   "TE29_07", 
                   "TE29_08",
                   "TE29_10", 
                   "TE29_11"
) ],
check.keys=TRUE) # 


# Cronbachs Alpha when removing item 10 (cut-off of 0.5)
psych::alpha(df[c( "TE29_05", 
                   "TE29_07", 
                   "TE29_08",
                   "TE29_11"
) ],
check.keys=TRUE) # 


# Generate variable (additive index)

df$fin_att <- apply( df[c( "TE29_05", 
                           "TE29_07", 
                           "TE29_08",
                           "TE29_10", 
                           "TE29_11")], 1, mean, na.rm=TRUE) # 




#### :: 2.3 Homeowner ####

unique(df$DG25)
df$homeowner <- "Non-\nowner"
df$homeowner[df$DG25 == "Ja"] <- "Home-\nowner"
df$homeowner[df$DG25 == "Yes"] <-"Home-\nowner"
table(df$homeowner)

# Generate a numeric variable
df$homeowner_num <- as.numeric(factor(df$homeowner))
table(df$homeowner_num ) 
df$homeowner_num <- 2 - df$homeowner_num 







#### :: 2.4 Treatment ####

df$treatment <- df$CS10
df$treatment <- as.factor(df$treatment)
df$treatment <- factor(df$treatment, labels = c("House prices\nfall", "House prices\nincrease", "No information") )
df$treatment <- factor(df$treatment, levels = c("No information", "House prices\nincrease", "House prices\nfall"  ) )

# df$treatment <- factor(df$treatment, levels = c("House prices\nincrease", "House prices\nfall", "No information" ) )



#### :: 2.5 Social policy orientation ####

summary(df$TE08_01)



#### :: 2.6 Left right ideology ####

summary(df$TE08)



#### :: 2.7 Political interest and sociodemographics ####

# political interest

df$polint <- NA

df$polint[df$TE10 == "1 überhaupt nicht interessiert"] <- 1
df$polint[df$TE10 == "2 wenig interessiert"] <- 2
df$polint[df$TE10 == "3 ziemlich interessiert"] <- 3
df$polint[df$TE10 == "4 sehr interessiert"] <- 4
df$polint[df$TE10 == "1 Not at all interested"] <- 1
df$polint[df$TE10 == "2 Not very interested"] <- 2
df$polint[df$TE10 == "3 Somewhat interested"] <- 3
df$polint[df$TE10 == "4 Very interested"] <- 4

table(df$polint)


# gender

df$female <- 0
df$female[df$DG15 == "Woman"] <- 1
df$female[df$DG15 == "Frau"] <- 1
table(df$female)


# age

df$age



#### :: 2.8 Norm variables ####

df$TE30_01 <- (df$TE30_01 - 1)/4       # House price eval personal
df$TE30_02 <- (df$TE30_02 - 1)/4       # House price eval country

df$TE25_01 <- (df$TE25_01 - 1)/6       # Post-treatment: House price eval personal
df$TE35_01 <- (df$TE35_01 - 1)/6       # Post-treatment: House price eval country

df$TE34_03 <- 1-( (df$TE34_03 - 1)/10) # Slow versus stimulate housing markets - higher scores = slow housing markets
df$hou_pol_slow <- df$TE34_03

df$hou_pol1 <- (df$hou_pol1 - 1)/4     # Owner market-oriented housing policy
df$hou_pol2 <- (df$hou_pol2 - 1)/4     # Renter market-oriented housing policy

df$fin_att <- (df$fin_att - 1)/4       # Financialization attitudes 1 to 5
df$TE29_12 <- (df$TE29_12 -1)/4        # financialization item 12

df$TE31_01 <- (df$TE31_01 - 1)/10      # Housing as investment versus place to live 1 (place to live) to 11 (investment)

df$TE08_01 <- (df$TE08_01 - 1)/10      # Pro social policy

df$DG02_01 <- (df$DG02_01)/max(df$DG02_01)        # Age
df$polint <- (df$polint - 1)/3                    # Political interest
df$subjclass <- 1- ((as.numeric(df$DG24)  - 1)/9) # Inverted such that higher values represent higher subj. class (visualization in the survey was via a ladder, with highest values scored for the rungs furthest down)






#### 3 Direct evaluations of house price increases (Figure 1) ####

df$cmean <- "Country\nmean"


#### :: 3.1 House prices good for household ####

country.frame1 <- cbind(
  aggregate(TE30_01 ~ country + homeowner, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(TE30_01 ~ country + homeowner, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame1) <- c("Country", "homeowner", "Mean", "SE")

country.frame1c <- cbind(
  aggregate(TE30_01 ~ country + cmean, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(TE30_01 ~ country + cmean, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame1c) <- c("Country", "homeowner", "Mean", "SE")

country.frame1 <- rbind(country.frame1, country.frame1c)

country.frame1$Country <- factor(country.frame1$Country, levels =  c("DE", "UK") )


country.frame1$group2 <- c("DE Groups", "UK Groups", "DE Groups", "UK Groups", "DE", "UK"   )

p1.1 <- ggplot(data=country.frame1, aes(x = group2  , y = Mean, 
                                        shape = homeowner,
                                        color  = homeowner )) + 
  geom_point( size = 2 ) + xlab("") + ylab("House price increase positive household") +
  geom_errorbar( aes(ymin=Mean-1.96*SE, ymax=Mean + 1.96*SE), size = 0.4,  width = 0.2, colour = "grey20")  +
  scale_y_continuous(limits = c(0.1, 0.7), breaks = seq(0, 1, by = 0.1)) + 
  theme_bw() + theme(legend.position="bottom") +  
  scale_color_manual(name = "Group", values = c("black", "grey60", "grey60", "black", "grey60", "grey60")) +
  scale_shape_manual(name = "Group", values = c(15,19,17 ) )   +
  guides(
    colour = guide_legend("Group"),
    shape = guide_legend("Group")  )
p1.1 



#### :: 3.2 House prices good for economy ####

country.frame2 <- cbind(
  aggregate(TE30_02 ~ country + homeowner, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(TE30_02 ~ country + homeowner, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame2) <- c("Country", "homeowner", "Mean", "SE")

country.frame2c <- cbind(
  aggregate(TE30_02 ~ country + cmean, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(TE30_02 ~ country + cmean, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame2c) <- c("Country", "homeowner", "Mean", "SE")

country.frame2 <- rbind(country.frame2, country.frame2c)

country.frame2$Country <- factor(country.frame2$Country, levels =  c("DE", "UK") )

country.frame2$group2 <- c("DE Groups", "UK Groups", "DE Groups", "UK Groups", "DE", "UK"   )

p1.2 <- ggplot(data=country.frame2, aes(x = group2, y = Mean, 
                                        shape = homeowner,
                                        color  = homeowner )) + 
  geom_point( size = 2 ) + xlab("") + ylab("House price increase positive country") +
  geom_errorbar( aes(ymin=Mean-1.96*SE, ymax=Mean + 1.96*SE), size = 0.4,  width = 0.2, colour = "grey20")  +
  scale_y_continuous(limits = c(0.1, 0.7), breaks = seq(0, 1, by = 0.1)) + 
  theme_bw() + theme(legend.position="bottom") +  
  scale_color_manual(name = "Group", values = c("black", "grey60", "grey60")) +
  scale_shape_manual(name = "Group", values = c(15,19,17 ) )   +
  guides(
    colour = guide_legend("Group"),
    shape = guide_legend("Group")  )
p1.2



# png(paste0("./Figures/Figure_1.png"), width=18, height=10,units="cm", res=400)

# grid.arrange(p1.1, p1.2,  ncol = 2)

# dev.off()


pdf(paste0("Figure_1.pdf"), width=8, height=4)

grid.arrange(p1.1, p1.2,  ncol = 2)

dev.off()




# Check demand for rent control

country.frame2b <- cbind(
  aggregate(TE29_12 ~ country + homeowner, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(TE29_12 ~ country + homeowner, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame2b) <- c("Country", "homeowner", "Mean", "SE")

country.frame2bc <- cbind(
  aggregate(TE29_12 ~ country + cmean, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(TE29_12 ~ country + cmean, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame2bc) <- c("Country", "homeowner", "Mean", "SE")

country.frame2b <- rbind(country.frame2b, country.frame2bc)

country.frame2b$Country <- factor(country.frame2b$Country, levels =  c("DE", "UK") )


country.frame2b$group2 <- c("DE Groups", "UK Groups", "DE Groups", "UK Groups", "DE", "UK"   )

p1.3 <- ggplot(data=country.frame2b, aes(x = group2  , y = Mean, 
                                         shape = homeowner,
                                         color  = homeowner )) + 
  geom_point( size = 2 ) + xlab("") + ylab("Support for rent control") +
  geom_errorbar( aes(ymin=Mean-1.96*SE, ymax=Mean + 1.96*SE), size = 0.4,  width = 0.2, colour = "grey20")  +
  scale_y_continuous(limits = c(0.1, 0.7), breaks = seq(0, 1, by = 0.1)) + 
  theme_bw() + theme(legend.position="bottom") +  
  scale_color_manual(name = "Group", values = c("black", "grey60", "grey60", "black", "grey60", "grey60")) +
  scale_shape_manual(name = "Group", values = c(15,19,17 ) )   +
  guides(
    colour = guide_legend("Group"),
    shape = guide_legend("Group")  )
p1.3 





#### :: 3.3 T-test: Comparison German and British homeowners ####

df$f_country <- factor(df$country)

t.test( df$TE30_01[df$homeowner_num == 1] ~ df$f_country[df$homeowner_num == 1], alternative = "two.sided", var.equal = FALSE) # evaluation house price increase personal
t.test( df$TE30_02[df$homeowner_num == 1] ~ df$f_country[df$homeowner_num == 1], alternative = "two.sided", var.equal = FALSE) # evaluation house price increase personal




#### 4 Reactions to house price changes (Figure 2 and Figure 3) ####
 
#### :: 4.1 Evaluation economy personal #### 

# Germany 
m1a <- lm(TE25_01 ~ treatment*homeowner_num ,
          data = df[df$country == "DE", ])
summary(m1a)

p4.1 <- plot( ggpredict(m1a, terms = c("treatment", "homeowner_num"))  ) + 
  theme_bw() + theme(legend.position="bottom") +  scale_color_grey(name = "Homeowner") +
  ylab("Ego-tropic evaluation \n of econ. outlook") + xlab ("") +
  labs(title = "Germany") +
  scale_y_continuous(limits = c(0.3, 0.8), breaks = seq(0, 1, by = 0.1))

p4.1



# UK
m1b <- lm(TE25_01 ~ treatment*homeowner_num ,
          data = df[df$country == "UK", ])
summary(m1b)

plot( ggpredict(m1b, terms = c("treatment", "homeowner_num"))  )  + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1))


p4.2  <- plot( ggpredict(m1b, terms = c("treatment", "homeowner_num"))  ) + 
  theme_bw() + theme(legend.position="bottom") +  scale_color_grey(name = "Homeowner") +
  ylab("Ego-tropic evaluation \n of econ. outlook") + xlab ("") +
  labs(title = "United Kingdom") +
  scale_y_continuous(limits = c(0.3, 0.8), breaks = seq(0, 1, by = 0.1))

p4.2


# png(paste0("Figure_2.png"), width=18, height=10,units="cm", res=400)

# grid.arrange(p4.1, p4.2, ncol = 2)

# dev.off()



pdf(paste0("Figure_2.pdf"), width=8, height=4)

grid.arrange(p4.1, p4.2, ncol = 2)

dev.off()





#### :: 4.2 Evaluation economy country ####

# Germany
m1c <- lm(TE35_01 ~ treatment*homeowner_num ,
          data = df[df$country == "DE", ])
summary(m1c)


p5.1 <- plot( ggpredict(m1c, terms = c("treatment", "homeowner_num"))  ) + 
  theme_bw() + theme(legend.position="bottom") +  scale_color_grey(name = "Homeowner") +
  ylab("Socio-tropic evaluation \n of econ. outlook") + xlab ("") +
  labs(title = "Germany") + 
  scale_y_continuous(limits = c(0.3, 0.8), breaks = seq(0, 1, by = 0.1))
p5.1


# UK
m1d <- lm(TE35_01 ~ treatment*homeowner_num ,
          data = df[df$country == "UK", ])
summary(m1d)

p5.2 <- plot( ggpredict(m1d, terms = c("treatment", "homeowner_num"))  ) + 
  theme_bw() + theme(legend.position="bottom") +  scale_color_grey(name = "Homeowner") +
  ylab("Socio-tropic evaluation \n of econ. outlook") + xlab ("") +
  labs(title = "UK") + 
  scale_y_continuous(limits = c(0.3, 0.8), breaks = seq(0, 1, by = 0.1))
p5.2



# png(paste0("Figure_3.png"), width=18, height=10,units="cm", res=400)

# grid.arrange(p5.1, p5.2, ncol = 2)

# dev.off()


pdf(paste0("Figure_3.pdf"), width=8, height=4)

grid.arrange(p5.1, p5.2, ncol = 2)

dev.off()


# Additional analysis: Support for rent control (policy preference) as dependent variable (Annex A6)

df$s_TE11_07 <- (df$TE11_07 - 1)/4 # norm variable to range 0 to 1.

m_rent_control_1 <- lm(s_TE11_07 ~ treatment*homeowner_num ,
          data = df[df$country == "DE", ])
summary(m_rent_control_1)
nobs(m_rent_control_1)


p5.1 <- plot( ggpredict(m_rent_control_1, terms = c("treatment", "homeowner_num"))  ) + 
  theme_bw() + theme(legend.position="bottom") +  scale_color_discrete(name = "homeowner_num") +
  ylab("Socio-tropic evaluation of the economy") + xlab ("") +
  labs(title = "Germany") + 
  scale_y_continuous(limits = c(0.3, 1.0), breaks = seq(0, 1, by = 0.1))
p5.1


m_rent_control_2 <- lm(s_TE11_07 ~ treatment*homeowner_num ,
          data = df[df$country == "UK", ])
summary(m_rent_control_2)
nobs(m_rent_control_2)

p5.1 <- plot( ggpredict(m_rent_control_2, terms = c("treatment", "homeowner_num"))  ) + 
  theme_bw() + theme(legend.position="bottom") +  scale_color_discrete(name = "homeowner_num") +
  ylab("Socio-tropic evaluation of the economy") + xlab ("") +
  labs(title = "Germany") + 
  scale_y_continuous(limits = c(0.3, 1.0), breaks = seq(0, 1, by = 0.1))
p5.1





#### 5 Description of housing policy preferences (Figure 4) ####

#### :: 5.1 Correlations ####

cor(df[c("TE30_01", "TE30_02", "hou_pol_slow", "hou_pol1", "hou_pol2")])

cor.test( df$hou_pol_slow, df$hou_pol1) # slow down housing markets x owner-oriented policy
cor.test( df$hou_pol_slow, df$hou_pol2) # slow down housing markets x renter-oriented policy
cor.test( df$hou_pol1, df$hou_pol2)     # owner-oriented policy     x renter-oriented policy




df$cmean <- "Country\nmean"

#### :: 5.2 Slow down versus stimulate markets ####

country.frame0 <- cbind(
  aggregate(hou_pol_slow ~ country + homeowner, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(hou_pol_slow ~ country + homeowner, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame0) <- c("Country", "homeowner", "Mean", "SE")

country.frame1c <- cbind(
  aggregate(hou_pol_slow ~ country + cmean, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(hou_pol_slow ~ country + cmean, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame1c) <- c("Country", "homeowner", "Mean", "SE")

country.frame0 <- rbind(country.frame0, country.frame1c)

country.frame0$Country <- factor(country.frame0$Country, levels =  c("DE", "UK") )


country.frame0$group2 <- c("DE\nGroups", "UK\nGroups", "DE\nGroups", "UK\nGroups", "DE", "UK"   )


p1.0 <- ggplot(data=country.frame0, aes(x = group2  , y = Mean, 
                                        shape = homeowner,
                                        color  = homeowner )) + 
  geom_point( size = 2 ) + xlab("") + ylab("Stimulate vs. slow down markets") +
  geom_errorbar( aes(ymin=Mean-1.96*SE, ymax=Mean + 1.96*SE), size = 0.4,  width = 0.2, colour = "grey20")  +
  scale_y_continuous(limits = c(0.4, 0.9), breaks = seq(0, 1, by = 0.1)) + 
  theme_bw() + theme(legend.position="bottom") +  
  scale_color_manual(name = "Group", values = c("black", "grey60", "grey60", "black", "grey60", "grey60")) +
  scale_shape_manual(name = "Group", values = c(15,19,17 ) )   +
  guides(
    colour = guide_legend("Group"),
    shape = guide_legend("Group")  )
p1.0 



#### :: 5.3 owner-oriented policies ####

country.frame1 <- cbind(
  aggregate(hou_pol1 ~ country + homeowner, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(hou_pol1 ~ country + homeowner, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame1) <- c("Country", "homeowner", "Mean", "SE")

country.frame1c <- cbind(
  aggregate(hou_pol1 ~ country + cmean, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(hou_pol1 ~ country + cmean, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame1c) <- c("Country", "homeowner", "Mean", "SE")

country.frame1 <- rbind(country.frame1, country.frame1c)

country.frame1$Country <- factor(country.frame1$Country, levels =  c("DE", "UK") )


country.frame1$group2 <- c("DE\nGroups", "UK\nGroups", "DE\nGroups", "UK\nGroups", "DE", "UK"   )

p1.1 <- ggplot(data=country.frame1, aes(x = group2  , y = Mean, 
                                        shape = homeowner,
                                        color  = homeowner )) + 
  geom_point( size = 2 ) + xlab("") + ylab("Support owner-oriented policies") +
  geom_errorbar( aes(ymin=Mean-1.96*SE, ymax=Mean + 1.96*SE), size = 0.4,  width = 0.2, colour = "grey20")  +
  scale_y_continuous(limits = c(0.4, 0.9), breaks = seq(0, 1, by = 0.1)) + 
  theme_bw() + theme(legend.position="bottom") +  
  scale_color_manual(name = "Group", values = c("black", "grey60", "grey60", "black", "grey60", "grey60")) +
  scale_shape_manual(name = "Group", values = c(15,19,17 ) )   +
  guides(
    colour = guide_legend("Group"),
    shape = guide_legend("Group")  )
p1.1 



#### :: 5.3 renter-oriented policies ####

country.frame2 <- cbind(
  aggregate(hou_pol2 ~ country + homeowner, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(hou_pol2 ~ country + homeowner, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame2) <- c("Country", "homeowner", "Mean", "SE")

country.frame2c <- cbind(
  aggregate(hou_pol2 ~ country + cmean, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(hou_pol2 ~ country + cmean, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame2c) <- c("Country", "homeowner", "Mean", "SE")

country.frame2 <- rbind(country.frame2, country.frame2c)

country.frame2$Country <- factor(country.frame2$Country, levels =  c("DE", "UK") )

country.frame2$group2 <- c("DE\nGroups", "UK\nGroups", "DE\nGroups", "UK\nGroups", "DE", "UK"   )

p1.2 <- ggplot(data=country.frame2, aes(x = group2, y = Mean, 
                                        shape = homeowner,
                                        color  = homeowner )) + 
  geom_point( size = 2 ) + xlab("") + ylab("Support renter-oriented policies") +
  geom_errorbar( aes(ymin=Mean-1.96*SE, ymax=Mean + 1.96*SE), size = 0.4,  width = 0.2, colour = "grey20")  +
  scale_y_continuous(limits = c(0.4, 0.9), breaks = seq(0, 1, by = 0.1)) + 
  theme_bw() + theme(legend.position="bottom") +  
  scale_color_manual(name = "Group", values = c("black", "grey60", "grey60")) +
  scale_shape_manual(name = "Group", values = c(15,19,17 ) )   +
  guides(
    colour = guide_legend("Group"),
    shape = guide_legend("Group")  )
p1.2



# png(paste0("Figure_4.png"), width=24, height=9,units="cm", res=400)

# grid.arrange(p1.0, p1.1, p1.2, ncol = 3)

# dev.off()



pdf(paste0("Figure_4.pdf"), width=12, height=4)

grid.arrange(p1.0, p1.1, p1.2, ncol = 3)

dev.off()




#### 6 Regressions for housing policy preferences ####

df$treatment <- factor(df$treatment, levels = c("House prices\nincrease", "House prices\nfall", "No information" ) )

#### :: 6.1 Regressions Germany #### 

m_ger_1a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
               data = df[df$country == "DE", ] )

summary(m_ger_1a) 


m_ger_1b <- lm(hou_pol1 ~ TE30_01 + TE30_02 + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
               data = df[df$country == "DE", ] )

summary(m_ger_1b) 


m_ger_1c <- lm(hou_pol2 ~ TE30_01 + TE30_02 + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female ,
               data = df[df$country == "DE", ] )

summary(m_ger_1c) 




#### :: 6.2 Regressions UK #### 


m_uk_2a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female ,
              data = df[df$country == "UK", ] )

summary(m_uk_2a) 


m_uk_2b <- lm(hou_pol1 ~ TE30_01 + TE30_02  + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2b) 


m_uk_2c <- lm(hou_pol2 ~ TE30_01 + TE30_02 + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2c) 


# htmlreg( list(m_ger_1a, m_ger_1b, m_ger_1c, m_uk_2a, m_uk_2b, m_uk_2c), file = "Main_models_countries.htm")









#### Annex A5: Main effects  ####

m_c_1 <- lm(TE25_01 ~ treatment ,
            data = df[df$country == "DE", ])
summary(m_c_1)

m_c_2 <- lm(TE25_01 ~ treatment ,
            data = df[df$country == "UK", ])
summary(m_c_2)



m_c_3 <- lm(TE35_01 ~ treatment ,
            data = df[df$country == "DE", ])
summary(m_c_3)

m_c_4 <- lm(TE35_01 ~ treatment ,
            data = df[df$country == "UK", ])
summary(m_c_4)



# htmlreg(list(m_c_1, m_c_2, m_c_3, m_c_4 ), "Annex_main_effects.htm")






#### Annex A8: Young and old median split ####

median(df$DG02_01[df$country == "DE"])
median(df$DG02_01[df$country == "UK"])


m_a1_1 <- lm(TE25_01 ~ treatment*homeowner_num,
             data = df[df$country == "DE" & df$DG02_01 < 0.5050505, ])
summary(m_a1_1)

m_a1_2 <- lm(TE25_01 ~ treatment*homeowner_num ,
             data = df[df$country == "DE" & df$DG02_01 >= 0.5050505, ])
summary(m_a1_2)

m_a2_1 <- lm(TE25_01 ~ treatment*homeowner_num ,
             data = df[df$country == "UK" & df$DG02_01 < 0.5050505, ])
summary(m_a2_1)

m_a2_2 <- lm(TE25_01 ~ treatment*homeowner_num ,
             data = df[df$country == "UK" & df$DG02_01 >= 0.5050505, ])
summary(m_a2_2)




m_a3_1 <- lm(TE35_01 ~ treatment*homeowner_num ,
             data = df[df$country == "DE" & df$DG02_01 < 0.4848485, ])
summary(m_a3_1)

m_a3_2 <- lm(TE35_01 ~ treatment*homeowner_num ,
             data = df[df$country == "DE" & df$DG02_01 >= 0.4848485, ])
summary(m_a3_2)

m_a4_1 <- lm(TE35_01 ~ treatment*homeowner_num ,
             data = df[df$country == "UK" & df$DG02_01 < 0.4848485, ])
summary(m_a4_1)

m_a4_2 <- lm(TE35_01 ~ treatment*homeowner_num ,
             data = df[df$country == "UK" & df$DG02_01 >= 0.4848485, ])
summary(m_a4_2)


# htmlreg(list(m_a1_1, m_a1_2, m_a2_1, m_a2_2, m_a3_1, m_a3_2, m_a4_1, m_a4_2 ), "Annex_main_split_age.htm")






#### Annex A9: Low and high subjective class median split ####

table(df$DG24)
table(df$subjclass)


median(df$subjclass[df$country == "DE"])
median(df$subjclass[df$country == "UK"])

m_b1_1 <- lm(TE25_01 ~ treatment*homeowner_num,
             data = df[df$country == "DE" & df$subjclass < 0.54, ])
summary(m_b1_1)

m_b1_2 <- lm(TE25_01 ~ treatment*homeowner_num ,
             data = df[df$country == "DE" & df$subjclass >= 0.54, ])
summary(m_b1_2)

m_b2_1 <- lm(TE25_01 ~ treatment*homeowner_num ,
             data = df[df$country == "UK" & df$subjclass < 0.54, ])
summary(m_b2_1)

m_b2_2 <- lm(TE25_01 ~ treatment*homeowner_num ,
             data = df[df$country == "UK" & df$subjclass >= 0.54, ])
summary(m_b2_2)




m_b3_1 <- lm(TE35_01 ~ treatment*homeowner_num ,
             data = df[df$country == "DE" & df$subjclass < 0.54, ])
summary(m_b3_1)

m_b3_2 <- lm(TE35_01 ~ treatment*homeowner_num ,
             data = df[df$country == "DE" & df$subjclass >= 0.54, ])
summary(m_b3_2)

m_b4_1 <- lm(TE35_01 ~ treatment*homeowner_num ,
             data = df[df$country == "UK" & df$subjclass < 0.54, ])
summary(m_b4_1)

m_b4_2 <- lm(TE35_01 ~ treatment*homeowner_num ,
             data = df[df$country == "UK" & df$subjclass >= 0.54, ])
summary(m_b4_2)


# htmlreg(list(m_b1_1, m_b1_2, m_b2_1, m_b2_2, m_b3_1, m_b3_2, m_b4_1, m_b4_2 ), "Annex_main_split_sclass.htm")






#### Annex A10 - Factor analysis over housing policies with control group only ####

table(df$CS10, df$treatment)

df_control <- df[df$CS10 == 3, ]

fa.out <- factanal( df_control[c( "TE11_01", 
                          "TE11_02",
                          "TE11_03",
                       #  "TE11_04", 
                          "TE11_05", 
                          "TE11_06", 
                          "TE11_07", 
                          "TE11_08",
                          "TE11_09")], factors = 2, rotation = "promax"
)

fa.out


# write.csv(unclass(loadings(fa.out)), file = "pca_loadings_housing_pol_control.csv")



#### Annex A11 - Factor analysis over housing policies with all items ####

fa.out <- factanal( df[c( "TE11_01", 
                          "TE11_02",
                          "TE11_03",
                          "TE11_04", 
                          "TE11_05", 
                          "TE11_06", 
                          "TE11_07", 
                          "TE11_08",
                          "TE11_09")], factors = 3, rotation = "promax"
)


fa.out


# write.csv(unclass(loadings(fa.out)), file = "pca_loadings_housing_pol_all_items.csv")




#### Annex A12 - Factor analysis over housing policies by country ####

table(df$country )

df_tmp <- df[df$country == "DE", ]

fa.out <- factanal( df_tmp[c( "TE11_01", 
                          "TE11_02",
                          "TE11_03",
                        # "TE11_04", 
                          "TE11_05", 
                          "TE11_06", 
                          "TE11_07", 
                          "TE11_08",
                          "TE11_09")], factors = 2, rotation = "promax"
)


print(fa.out, digits = 2, cutoff = 0)


# write.csv(unclass(loadings(fa.out)), file = "pca_loadings_housing_pol_DE.csv")



df_tmp <- df[df$country == "UK", ]

fa.out <- factanal( df_tmp[c( "TE11_01", 
                          "TE11_02",
                          "TE11_03",
                        # "TE11_04", 
                          "TE11_05", 
                          "TE11_06", 
                          "TE11_07", 
                          "TE11_08",
                          "TE11_09")], factors = 2, rotation = "promax"
)


print(fa.out, digits = 2, cutoff = 0)

# write.csv(unclass(loadings(fa.out)), file = "pca_loadings_housing_pol_UK.csv")



#### Annex A13 - Description of dependent variables (housing policy preferences) ####

h1 <- ggplot() + geom_histogram(data = df, aes( x = hou_pol_slow), bins = 10, color="black", fill="white" ) + xlab("Slow down versus stimulate markets") + theme_bw()
h2 <- ggplot() + geom_histogram(data = df, aes( x = hou_pol1),     bins = 10, color="black", fill="white" ) + xlab("Support owner-oriented policies") + theme_bw()
h3 <- ggplot() + geom_histogram(data = df, aes( x = hou_pol2),     bins = 10, color="black", fill="white" ) + xlab("Support renter-oriented policies")+ theme_bw()


# png(paste0("Figure_DV_dist.png"), width=20, height=20,units="cm", res=400)

grid.arrange(h1, h2, h3, ncol = 1)

# dev.off()




#### Annex A14 - Interaction of treatment with direct price evaluations ####

df$treatment <- factor(df$treatment, levels = c("House prices\nincrease", "House prices\nfall", "No information" ) )

# Regressions Germany 

m_ger_1a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment*TE30_01 + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
               data = df[df$country == "DE", ] )

summary(m_ger_1a) 


m_ger_1b <- lm(hou_pol1 ~ TE30_01 + TE30_02 + treatment*TE30_01 + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
               data = df[df$country == "DE", ] )

summary(m_ger_1b) 


m_ger_1c <- lm(hou_pol2 ~ TE30_01 + TE30_02 + treatment*TE30_01 + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female ,
               data = df[df$country == "DE", ] )

summary(m_ger_1c) 




# Regressions UK 


m_uk_2a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment*TE30_02 + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female ,
              data = df[df$country == "UK", ] )

summary(m_uk_2a) 


m_uk_2b <- lm(hou_pol1 ~ TE30_01 + TE30_02  + treatment*TE30_02 + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2b) 




m_uk_2c <- lm(hou_pol2 ~ TE30_01 + TE30_02 + treatment*TE30_02 + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2c) 




# htmlreg( list(m_ger_1a, m_ger_1b, m_ger_1c, m_uk_2a, m_uk_2b, m_uk_2c), file = "Main_models_int_price.htm")





#### Annex A16 - Models with individual housing policy items as DV - Germany ####

df$treatment <- factor(df$treatment, levels = c("House prices\nincrease", "House prices\nfall", "No information" ) )


m_a_1_1 <- lm(TE11_01 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

m_a_1_2 <- lm(TE11_02 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

m_a_1_3 <- lm(TE11_03 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

m_a_1_4 <- lm(TE11_04 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

m_a_1_5 <- lm(TE11_05 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

m_a_1_6 <- lm(TE11_06 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

m_a_1_7 <- lm(TE11_07 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

m_a_1_8 <- lm(TE11_08 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

m_a_1_9 <- lm(TE11_09 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )



# htmlreg( list(m_a_1_1, m_a_1_2, m_a_1_3, m_a_1_4, m_a_1_5, m_a_1_6, m_a_1_7, m_a_1_8, m_a_1_9  ), file = "Annex_all_items_DE.htm")



#### Annex A17 - Models with individual housing policy items as DV - UK ####

m_a_2_1 <- lm(TE11_01 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

m_a_2_2 <- lm(TE11_02 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

m_a_2_3 <- lm(TE11_03 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

m_a_2_4 <- lm(TE11_04 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

m_a_2_5 <- lm(TE11_05 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

m_a_2_6 <- lm(TE11_06 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

m_a_2_7 <- lm(TE11_07 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

m_a_2_8 <- lm(TE11_08 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

m_a_2_9 <- lm(TE11_09 ~ TE30_01 + TE30_02+ treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )




# htmlreg( list(m_a_2_1, m_a_2_2, m_a_2_3, m_a_2_4, m_a_2_5, m_a_2_6, m_a_2_7, m_a_2_8, m_a_2_9  ), file = "Annex_all_items_UK.htm")





#### Annex A18 - Pooled Regressions ####

m_pooled_a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female + factor(country) ,
                 data = df )

summary(m_pooled_a) 


m_pooled_b <- lm(hou_pol1 ~ TE30_01 + TE30_02 + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female + factor(country) ,
                 data = df )

summary(m_pooled_b) 



m_pooled_c <- lm(hou_pol2 ~ TE30_01 + TE30_02 + treatment + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female + factor(country),
                 data = df )

summary(m_pooled_c) 



# htmlreg( list(m_pooled_a, m_pooled_b, m_pooled_c), file = "Main_models_pooled.htm")




#### Annex A19 - Interaction financialization attitude with homeownership ####

df$treatment <- factor(df$treatment, levels = c("House prices\nincrease", "House prices\nfall", "No information" ) )

# Regressions Germany

m_ger_1a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment+ fin_att*homeowner_num + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female ,
               data = df[df$country == "DE", ] )

summary(m_ger_1a) 


m_ger_1b <- lm(hou_pol1 ~ TE30_01 + TE30_02+ treatment + fin_att*homeowner_num + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female ,
               data = df[df$country == "DE", ] )

summary(m_ger_1b) 


m_ger_1c <- lm(hou_pol2 ~ TE30_01 + TE30_02+ treatment + fin_att*homeowner_num + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female ,
               data = df[df$country == "DE", ] )

summary(m_ger_1c) 




# Regressions UK

m_uk_2a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment+ fin_att*homeowner_num + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2a) 

m_uk_2b <- lm(hou_pol1 ~ TE30_01 + TE30_02 + treatment+ fin_att*homeowner_num + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2b) 


m_uk_2c <- lm(hou_pol2 ~ TE30_01 + TE30_02+ treatment + fin_att*homeowner_num + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female ,
              data = df[df$country == "UK", ] )

summary(m_uk_2c) 



# htmlreg( list(m_ger_1a, m_ger_1b, m_ger_1c, m_uk_2a,m_uk_2b, m_uk_2c  ), file = "Main_models_finatt_x_owner.htm")




#### Annex A20 - Models without financialization attitude - testing only housing as investment ####

df$treatment <- factor(df$treatment, levels = c("House prices\nincrease", "House prices\nfall", "No information" ) )

# Regressions Germany 
m_a_3_0<- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment+  TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
             data = df[df$country == "DE", ] )

summary(m_a_3_0) 

m_a_3_1<- lm(hou_pol1 ~ TE30_01 + TE30_02 + treatment +  TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
             data = df[df$country == "DE", ] )

summary(m_a_3_1) 


m_a_3_2 <- lm(hou_pol2 ~ TE30_01 + TE30_02 + treatment +  TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "DE", ] )

summary(m_a_3_2) 



# Regressions UK
m_a_3_3 <- lm(hou_pol_slow ~ TE30_01 + TE30_02+ treatment +  TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_a_3_3) 


m_a_3_4 <- lm(hou_pol1 ~ TE30_01 + TE30_02+ treatment +  TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_a_3_4) 


m_a_3_5 <- lm(hou_pol2 ~ TE30_01 + TE30_02+ treatment +  TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_a_3_5) 



# htmlreg( list(m_a_3_0, m_a_3_1, m_a_3_2, m_a_3_3, m_a_3_4, m_a_3_5), file = "Annex_model_place_to_live.htm")





#### Annex A21 - Interaction treatment with homeownership ####

df$treatment <- factor(df$treatment, levels = c("House prices\nincrease", "House prices\nfall", "No information" ) )

# Regressions Germany

m_ger_1a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment + treatment*homeowner_num + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
               data = df[df$country == "DE", ] )

summary(m_ger_1a) 


m_ger_1b <- lm(hou_pol1 ~ TE30_01 + TE30_02 + treatment + treatment*homeowner_num + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
               data = df[df$country == "DE", ] )

summary(m_ger_1b) 


m_ger_1c <- lm(hou_pol2 ~ TE30_01 + TE30_02 + treatment + treatment*homeowner_num + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
               data = df[df$country == "DE", ] )

summary(m_ger_1c) 




# Regressions UK

m_uk_2a <- lm(hou_pol_slow ~ TE30_01 + TE30_02 + treatment + treatment*homeowner_num + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2a) 

m_uk_2b <- lm(hou_pol1 ~ TE30_01 + TE30_02 + treatment + treatment*homeowner_num + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2b) 


m_uk_2c <- lm(hou_pol2 ~ TE30_01 + TE30_02 + treatment + treatment*homeowner_num + fin_att + TE31_01 + homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
              data = df[df$country == "UK", ] )

summary(m_uk_2c) 



# htmlreg( list(m_ger_1a, m_ger_1b, m_ger_1c, m_uk_2a, m_uk_2b,m_uk_2c  ), file = "Main_models_treat_x_owner.htm")




#### Annex A22 - Inspect who has stronger financialization attitudes ####

fin_1a <- lm(fin_att ~ TE30_01 + TE30_02 +  homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
             data = df[df$country == "DE", ] )

summary(fin_1a) 

fin_2a <- lm(fin_att ~ TE30_01 + TE30_02 +  homeowner_num + TE08_01 + subjclass + polint + DG02_01 + female,
             data = df[df$country == "UK", ] )

summary(fin_2a) 



# htmlreg( list(fin_1a, fin_2a ), file = "Fin_att_models.htm")




# Correlation finance attitude and house price evaluation


# Correlations Germany 

tmp <- df[df$country == "DE", ]

cor.tmp <- tmp[c("homeowner_num",
                 "fin_att",
                 "TE31_01", # place to live versus investment
                 "TE08_01", # social policy
                 "TE30_01", # eval house prices personal
                 "TE30_02"  # eval house prices country
                 
)]

cor.out <- rcorr(as.matrix(cor.tmp))
cor.out


# Correlations UK 

tmp <- df[df$country == "UK", ]
cor.tmp <- tmp[c("homeowner_num",
                 "fin_att",
                 "TE31_01", # place to live versus investment
                 "TE08_01", # social policy
                 "TE30_01", # eval house prices personal
                 "TE30_02"  # eval house prices country
)]

cor.out <- rcorr(as.matrix(cor.tmp))
cor.out




#### Annex A23 - Description of housing as investment and financialization by country ####


df$cmean <- "Country\nmean"

country.frame3 <- cbind(
  aggregate(TE31_01 ~ country + homeowner, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(TE31_01 ~ country + homeowner, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame3) <- c("Country", "homeowner", "Mean", "SE")

country.frame3c <- cbind(
  aggregate(TE31_01 ~ country + cmean, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(TE31_01 ~ country + cmean, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame3c) <- c("Country", "homeowner", "Mean", "SE")

country.frame3 <- rbind(country.frame3, country.frame3c)


country.frame3$Country <- factor(country.frame3$Country, levels =  c("DE", "UK") )

country.frame3$group2 <- c("DE Groups", "UK Groups", "DE Groups", "UK Groups", "DE", "UK"   )

p2.1 <- ggplot(data=country.frame3, aes(x = group2, y = Mean, 
                                        shape = homeowner,
                                        color  = homeowner )) + 
  geom_point( size = 2 ) + xlab("") + ylab("Housing as investment vs. place to live") +
  geom_errorbar( aes(ymin=Mean-1.96*SE, ymax=Mean + 1.96*SE), size = 0.4,  width = 0.2, colour = "grey20")  +
  scale_y_continuous(limits = c(0.1, 0.7), breaks = seq(0, 1, by = 0.1)) + 
  theme_bw() + theme(legend.position="bottom") +  
  scale_color_manual(name = "Group", values = c("black", "grey60", "grey60")) +
  scale_shape_manual(name = "Group", values = c(15,19,17 ) )   +
  guides(
    colour = guide_legend("Group"),
    shape = guide_legend("Group")  )


country.frame4 <- cbind(
  aggregate(fin_att ~ country + homeowner, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(fin_att ~ country + homeowner, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame4) <- c("Country", "homeowner", "Mean", "SE")


country.frame4c <- cbind(
  aggregate(fin_att ~ country + cmean, df, FUN = function(x) c(Mean = mean(x))  ),
  aggregate(fin_att ~ country + cmean, df, FUN = function(x) c(SE = std.error(x))  )[ , 3] )
colnames(country.frame4c) <- c("Country", "homeowner", "Mean", "SE")

country.frame4 <- rbind(country.frame4, country.frame4c)


country.frame4$Country <- factor(country.frame4$Country, levels =  c("DE", "UK") )

country.frame4$group2 <- c("DE Groups", "UK Groups", "DE Groups", "UK Groups", "DE", "UK"   )

p2.2 <- ggplot(data=country.frame4, aes(x = group2, y = Mean, 
                                        shape = homeowner,
                                        color  = homeowner )) + 
  geom_point( size = 2 ) + xlab("") + ylab("Attitude toward financialization") +
  geom_errorbar( aes(ymin=Mean-1.96*SE, ymax=Mean + 1.96*SE), size = 0.4,  width = 0.2, colour = "grey20")  +
  scale_y_continuous(limits = c(0.1, 0.7), breaks = seq(0, 1, by = 0.1)) + 
  theme_bw() + theme(legend.position="bottom") +  
  scale_color_manual(name = "Group", values = c("black", "grey60", "grey60")) +
  scale_shape_manual(name = "Group", values = c(15,19,17 ) )   +
  guides(
    colour = guide_legend("Group"),
    shape = guide_legend("Group")  )
p2.2




# png(paste0("Figure_desc_invest_financ.png"), width=18, height=10,units="cm", res=400)

grid.arrange(p2.1, p2.2,
             ncol = 2)

# dev.off()





#### Time  filter (used for robustness check) ####

# Speeding (at least 5 min. = 300 Sec.)
sum(df$TIME_SUM < 300, na.rm = T)
# df <- df[df$TIME_SUM >= 300, ]
sum(df$TIME_SUM > median(df$TIME_SUM, na.rm = T)/2)

# df <- df[df$TIME_SUM > median(df$TIME_SUM, na.rm = T)/2,  ]
