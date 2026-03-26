

# Script "Consumers Are Willing to Pay a Price for Explainable, But Not for Green AI. Evidence from a Choice-Based Conjoint Analysis"

# Version: 9 November 2021



#### Load packages ####

library(conjoint)
library(skpr)
library(AlgDesign)
library(mlogit)
library(texreg)
library(dotwhisker)
library(ggplot2)
library(jtools)
library(grid)
library(gridExtra)



#### Step 1: Create orthogonal design and full choice set matrix #####

experiment<-expand.grid(Nutzen =  c("A",
                                    "B",
                                    "C"),
                        Transparenz =  c("A",
                                         "B",
                                         "C"),
                        Kosten =  c("A",
                                    "B",
                                    "C"),
                        Energie =  c("A",
                                     "B",
                                     "C"))

# create orthogonal design
design1=caFactorialDesign(data=experiment, 
                          type = "orthogonal")
print(design1)
print(cor(caEncodedDesign(design1)))


# produce alternatives via shifting

design1 <- data.frame(lapply(design1, 
                             function(x) 
                               as.numeric(x)))

design2 <- design1
design2[design1 == 1] <- 2
design2[design1 == 2] <- 3
design2[design1 == 3] <- 1

design3 <- design2
design3[design2 == 1] <- 2
design3[design2 == 2] <- 3
design3[design2 == 3] <- 1



# combine the designs to get choice sets

fulldesign <- cbind(design1, design2, design3)


fulldesign_long <- data.frame()
for (i in 1: nrow(design1)){
  tmp <- data.frame(choice = i, 
                    stimulus = 1:4, 
                    rbind(design1[i, ], design2[i,], design3[i ,], rep(0, 5) ) # repeat 0 for no choice and 5 times for 5 variables
  )
  
  fulldesign_long <- rbind(fulldesign_long, tmp)
  
}







#### Step 2: Prepare data and merge with conjoint choice design ####


#### Read survey data ####

df.AI <- read.csv("Dataset Explainable and Green AI.csv")


#### Filter out cases by attention check and control questions ####


# Attention check

df.AI <- df.AI[df.AI$TE05_13 < 2, ]


# Filter question at the end of the survey (asking whether people have provided honest responses to be used in the analysis)

df.AI <- df.AI[df.AI$KO01 == "Ja, ich habe den Fragebogen aufmerksam und nach bestem Wissen und Gewissen beantwortet. Meine Angaben können zur Auswertung verwendet werden." , ]





#### time filter ####

df.AI$time_filt <- apply(df.AI[c("TIME004", "TIME005", "TIME006", "TIME007", "TIME008", "TIME009", "TIME010", "TIME011", "TIME012")], 1, mean)
summary(df.AI$time_filt)

df.AI <- df.AI[df.AI$time_filt >= 3, ]







#### Prepare variables for analysis ####

#### Variables for generating subgroups: environmental concern and desire for control ####

# Environmental concern

df.AI$TE04_01[df.AI$TE04_01 == -1] <- NA # negative --> recoded below
df.AI$TE04_02[df.AI$TE04_02 == -1] <- NA # positive
df.AI$TE04_03[df.AI$TE04_03 == -1] <- NA # negative --> recoded below
df.AI$TE04_04[df.AI$TE04_04 == -1] <- NA # positive
df.AI$TE04_05[df.AI$TE04_05 == -1] <- NA # negative --> recoded below
df.AI$TE04_06[df.AI$TE04_06 == -1] <- NA # positive
df.AI$TE04_07[df.AI$TE04_07 == -1] <- NA # positive


# invert negative items
df.AI$TE04_01rc <- 6-df.AI$TE04_01 
df.AI$TE04_03rc <- 6-df.AI$TE04_03 
df.AI$TE04_05rc <- 6-df.AI$TE04_05 


# inspect reliability of the 6 items
psych::alpha(df.AI[c("TE04_01rc", 
                       "TE04_02",
                       "TE04_03rc", 
                       "TE04_04",
                       "TE04_05rc",
                       "TE04_06",
                       "TE04_07") ],
             check.keys=TRUE) # Alpha is 0.81


# generate mean of all items 
df.AI$environ <- apply( df.AI[c( "TE04_01rc", 
                                    "TE04_02",
                                    "TE04_03rc",
                                    "TE04_04",
                                    "TE04_05rc",
                                    "TE04_06",
                                    "TE04_07")], 1, mean, na.rm=TRUE)

hist(df.AI$environ)




# Desire for control - subscale self-control

table(df.AI$TE05_16)

df.AI$TE05_01[df.AI$TE05_01 == -1] <- NA
df.AI$TE05_03[df.AI$TE05_03 == -1] <- NA
df.AI$TE05_07[df.AI$TE05_07 == -1] <- NA
df.AI$TE05_08[df.AI$TE05_08 == -1] <- NA


# inspect reliability
psych::alpha(df.AI[c("TE05_01", 
                       "TE05_03",
                       "TE05_07", 
                       "TE05_08") ],
             check.keys=TRUE) # Alpha is 0.74


# generate mean of all items 
df.AI$control <- apply( df.AI[c( "TE05_01", 
                                     "TE05_03",
                                     "TE05_07",
                                     "TE05_08")], 1, mean, na.rm=TRUE)

# invert score such that higher values represent a stronger desire for control
df.AI$control <- 6 - df.AI$control 

hist(df.AI$control)





#### Political interest and sociodemographics ####

# political interest

table(df.AI$TE10)
unique(df.AI$TE10)

df.AI$polint <- NA
df.AI$polint[df.AI$TE10 == unique(df.AI$TE10)[5] ] <- 1
df.AI$polint[df.AI$TE10 == unique(df.AI$TE10)[2] ] <- 2
df.AI$polint[df.AI$TE10 == unique(df.AI$TE10)[4] ] <- 3
df.AI$polint[df.AI$TE10 == unique(df.AI$TE10)[1] ] <- 4
df.AI$polint[df.AI$TE10 == unique(df.AI$TE10)[3] ] <- 5



# age

df.AI$DG02_01


# gender

df.AI$female <- 0 # male and diverse
df.AI$female[df.AI$DG15 == "weiblich"] <- 1 # female
table(df.AI$female)


# education

df.AI$DG03
table(df.AI$DG03)

df.AI$education <- 0
df.AI$education[df.AI$DG03 == "Abitur bzw. erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)"] <- 1
df.AI$education[df.AI$DG03 == "Fachhochschulreife (Abschluss einer Fachoberschule etc.)"] <- 1
df.AI$education[df.AI$DG03 == "Hochschulabschluss"] <- 1
df.AI$education[df.AI$DG03 == "Promotion"] <- 1

table(df.AI$education)


# algorithm literacy

unique(df.AI$KE03)

df.AI$algolit <- NA
df.AI$algolit[df.AI$KE03 == unique(df.AI$KE03)[2] ] <- 1
df.AI$algolit[df.AI$KE03 == unique(df.AI$KE03)[5] ] <- 2
df.AI$algolit[df.AI$KE03 == unique(df.AI$KE03)[3] ] <- 3
df.AI$algolit[df.AI$KE03 == unique(df.AI$KE03)[1] ] <- 4
df.AI$algolit[df.AI$KE03 == unique(df.AI$KE03)[6] ] <- 5
df.AI$algolit[df.AI$KE03 == unique(df.AI$KE03)[4] ] <- 6










#### Generate clusters based on environmental concern and desire for control ####

# impute values - take means

df.AI$environ2 <- df.AI$environ
df.AI$control2 <- df.AI$control

df.AI$environpos <- df.AI$TE09_01

df.AI$environ2[is.na(df.AI$environ) == T ] <- mean(df.AI$environ, na.rm = T)
df.AI$control2[is.na(df.AI$control) == T ] <- mean(df.AI$control, na.rm = T)
df.AI$environpos[is.na(df.AI$TE09_01) == T ] <- mean(df.AI$TE09_01, na.rm = T)


# identify optimal number of clusters
library(NbClust)
# criteria <- NbClust(data = df.AI[  , c("environ2", "control2") ], diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)


# K-Means Cluster Analysis
kclust <- kmeans( df.AI[  ,  c("environ2", "control2") ] , 
                 3, 
                 nstart=10 ) # 3 cluster solution

df.AI$kgroup <- kclust$cluster


# get cluster means (shown in Annex A7)
aggregate(df.AI[, c("environ2", "control2")], 
          by=list(df.AI$kgroup),
          FUN=mean)




df.AI$kgroup2 <- df.AI$kgroup
df.AI$kgroup[df.AI$kgroup2 == 1] <- 2 # high environmental concern
df.AI$kgroup[df.AI$kgroup2 == 2] <- 3 # high desire for control
df.AI$kgroup[df.AI$kgroup2 == 3] <- 1 # high both

aggregate(df.AI[, c("environ2", "control2")], 
          by=list(df.AI$kgroup),
          FUN=mean)




#### Prepare choice variables for reshaping the data to be used in multinomial logit ####

# transform choice variables (no-choice is the first out of four and should instead be the fourth choice, i.e. 4 out of 4)


df.AI$set1 <- 4
df.AI$set1[df.AI$CS01 == "Option A"] <- 1
df.AI$set1[df.AI$CS01 == "Option B"] <- 2
df.AI$set1[df.AI$CS01 == "Option C"] <- 3

df.AI$set2 <- 4
df.AI$set2[df.AI$CS02 == "Option A"] <- 1
df.AI$set2[df.AI$CS02 == "Option B"] <- 2
df.AI$set2[df.AI$CS02 == "Option C"] <- 3

df.AI$set3 <- 4
df.AI$set3[df.AI$CS03 == "Option A"] <- 1
df.AI$set3[df.AI$CS03 == "Option B"] <- 2
df.AI$set3[df.AI$CS03 == "Option C"] <- 3

df.AI$set4 <- 4
df.AI$set4[df.AI$CS04 == "Option A"] <- 1
df.AI$set4[df.AI$CS04 == "Option B"] <- 2
df.AI$set4[df.AI$CS04 == "Option C"] <- 3

df.AI$set5 <- 4
df.AI$set5[df.AI$CS05 == "Option A"] <- 1
df.AI$set5[df.AI$CS05 == "Option B"] <- 2
df.AI$set5[df.AI$CS05 == "Option C"] <- 3

df.AI$set6 <- 4
df.AI$set6[df.AI$CS06 == "Option A"] <- 1
df.AI$set6[df.AI$CS06 == "Option B"] <- 2
df.AI$set6[df.AI$CS06 == "Option C"] <- 3

df.AI$set7 <- 4
df.AI$set7[df.AI$CS07 == "Option A"] <- 1
df.AI$set7[df.AI$CS07 == "Option B"] <- 2
df.AI$set7[df.AI$CS07 == "Option C"] <- 3

df.AI$set8 <- 4
df.AI$set8[df.AI$CS08 == "Option A"] <- 1
df.AI$set8[df.AI$CS08 == "Option B"] <- 2
df.AI$set8[df.AI$CS08 == "Option C"] <- 3

df.AI$set9 <- 4
df.AI$set9[df.AI$CS09 == "Option A"] <- 1
df.AI$set9[df.AI$CS09 == "Option B"] <- 2
df.AI$set9[df.AI$CS09 == "Option C"] <- 3




#### Transform response data into required format ####

df.AI$respondentID <- 1:nrow(df.AI)

tmpc_frame <- df.AI

r <- 9 # set number of choice sets
k <- 4 # set number of stimuli per choice set


mnl_frame <- data.frame() # define empty dataframe

for (i in 1:nrow(tmpc_frame)){ # build choice data frames for every respondent and append them.
  
  tmp <- data.frame(respondentID = tmpc_frame$respondentID[i],
                     choice = rep(1:r, each=k), 
                     stimulus = rep(1:k, r)
                     )
  
  tmp_choice <- rep(0, r*k)
  # get indices: which 
  get_indices <- c(tmpc_frame$set1[i],
    tmpc_frame$set2[i],
    tmpc_frame$set3[i],
    tmpc_frame$set4[i],
    tmpc_frame$set5[i],
    tmpc_frame$set6[i],
    tmpc_frame$set7[i],
    tmpc_frame$set8[i],
    tmpc_frame$set9[i]
  ) + rep(k, r)*(0:(r-1)) # take into account that indices add up in steps of 4
  
  tmp_choice[get_indices] <- 1
  
  tmp <- data.frame(cbind(tmp, tmp_choice))
  mnl_frame <- rbind(mnl_frame, tmp)
  
}



#### generate dummy variables ####

# Nutzen = usefulness (user satisfaction)
fulldesign_long$Nutzen1 <- 0
fulldesign_long$Nutzen1[fulldesign_long$Nutzen == 1] <- 1

fulldesign_long$Nutzen2 <- 0
fulldesign_long$Nutzen2[fulldesign_long$Nutzen == 2] <- 1

fulldesign_long$Nutzen3 <- 0
fulldesign_long$Nutzen3[fulldesign_long$Nutzen == 3] <- 1


# Transparenz = transparency

fulldesign_long$Transparenz1 <- 0
fulldesign_long$Transparenz1[fulldesign_long$Transparenz == 1] <- 1

fulldesign_long$Transparenz2 <- 0
fulldesign_long$Transparenz2[fulldesign_long$Transparenz == 2] <- 1

fulldesign_long$Transparenz3 <- 0
fulldesign_long$Transparenz3[fulldesign_long$Transparenz == 3] <- 1


# Kosten = costs

fulldesign_long$Kosten1 <- 0
fulldesign_long$Kosten1[fulldesign_long$Kosten == 1] <- 1

fulldesign_long$Kosten2 <- 0
fulldesign_long$Kosten2[fulldesign_long$Kosten == 2] <- 1

fulldesign_long$Kosten3 <- 0
fulldesign_long$Kosten3[fulldesign_long$Kosten == 3] <- 1


# Energie = energy efficiency

fulldesign_long$Energie1 <- 0
fulldesign_long$Energie1[fulldesign_long$Energie == 1] <- 1

fulldesign_long$Energie2 <- 0
fulldesign_long$Energie2[fulldesign_long$Energie == 2] <- 1

fulldesign_long$Energie3 <- 0
fulldesign_long$Energie3[fulldesign_long$Energie == 3] <- 1


# none
fulldesign_long$none <- 0
fulldesign_long$none[fulldesign_long$stimulus == 4] <- 1






#### generate effect-coded variables ####

# Nutzen = usefulness (user satisfaction)

fulldesign_long$NutzenE2 <- 0
fulldesign_long$NutzenE2[fulldesign_long$Nutzen == 1] <- -1
fulldesign_long$NutzenE2[fulldesign_long$Nutzen == 2] <- 1

fulldesign_long$NutzenE3 <- 0
fulldesign_long$NutzenE3[fulldesign_long$Nutzen == 1] <- -1
fulldesign_long$NutzenE3[fulldesign_long$Nutzen == 3] <- 1



# Transparenz = transparency

fulldesign_long$TransparenzE2 <- 0
fulldesign_long$TransparenzE2[fulldesign_long$Transparenz == 1] <- -1
fulldesign_long$TransparenzE2[fulldesign_long$Transparenz == 2] <- 1

fulldesign_long$TransparenzE3 <- 0
fulldesign_long$TransparenzE3[fulldesign_long$Transparenz == 1] <- -1
fulldesign_long$TransparenzE3[fulldesign_long$Transparenz == 3] <- 1



# Kosten = costs

fulldesign_long$KostenE2 <- 0
fulldesign_long$KostenE2[fulldesign_long$Kosten == 1] <- -1
fulldesign_long$KostenE2[fulldesign_long$Kosten == 2] <- 1

fulldesign_long$KostenE3 <- 0
fulldesign_long$KostenE3[fulldesign_long$Kosten == 1] <- -1
fulldesign_long$KostenE3[fulldesign_long$Kosten == 3] <- 1



# Energie = energy efficiency

fulldesign_long$EnergieE2 <- 0
fulldesign_long$EnergieE2[fulldesign_long$Energie == 1] <- -1
fulldesign_long$EnergieE2[fulldesign_long$Energie == 2] <- 1

fulldesign_long$EnergieE3 <- 0
fulldesign_long$EnergieE3[fulldesign_long$Energie == 1] <- -1
fulldesign_long$EnergieE3[fulldesign_long$Energie == 3] <- 1


# none
fulldesign_long$noneE <- -1
fulldesign_long$noneE[fulldesign_long$stimulus == 4] <- 1




# match the reshaped data.frame with the choice designs

merged_data <- merge(mnl_frame, fulldesign_long, by = c("choice", "stimulus"))
merged_data <- merged_data[order(merged_data$respondentID), ]


# prepare individual level variables to be merged with the choice data frame

tmp <- df.AI[c("respondentID", 
              "environ", 
              "control",
              "TE08_01",
              "TE09_01",
              "algolit",
              "polint",
              "female",
              "DG02_01",
              "education",
              "kgroup")] # generaged clustervariable 

merged_data <- merge(merged_data, tmp, by = "respondentID") # merge individual-level variables with the choice data frame


# convert data for mlogit

cbc <- mlogit.data(merged_data, choice="tmp_choice", shape="long", alt.var="stimulus", id.var = "respondentID")




#### Step 3: Analysis using multinomial logit models ####

#### partworth model with dummy coding ####

ml1 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen3 +
                Transparenz2 + Transparenz3 +
                Kosten2 + Kosten3 +
                Energie2 + Energie3 + 
                none | 0, cbc)
summary(ml1)

# wordreg(  ml1,   file = "model 1.doc")



# prepare results for plotting

modelframe <- data.frame(term = c(
  '94% satisfied',
  '99% satisfied',
  'Transparency low',
  'Transparency high',
  '1.99 EUR',
  '3.99 EUR',
  'Energy 3h',
  'Energy 5h',
  'No-choice option'),
  estimate = c(  ml1$coefficients[1], ml1$coefficients[2],
                ml1$coefficients[3], ml1$coefficients[4],
                ml1$coefficients[5], ml1$coefficients[6],
                ml1$coefficients[7], ml1$coefficients[8],
                ml1$coefficients[9]),
  std.error = c( coef(summary(ml1))[1, "Std. Error"], coef(summary(ml1))[2, "Std. Error"], 
                  coef(summary(ml1))[3, "Std. Error"], coef(summary(ml1))[4, "Std. Error"], 
                  coef(summary(ml1))[5, "Std. Error"], coef(summary(ml1))[6, "Std. Error"], 
                  coef(summary(ml1))[7, "Std. Error"], coef(summary(ml1))[8, "Std. Error"],
                 coef(summary(ml1))[9, "Std. Error"]
  )
)



# https://cran.r-project.org/web/packages/dotwhisker/vignettes/dotwhisker-vignette.html

dwplot(modelframe)

# define additional labels in plot (bracketing the feature levels)
four_brackets <- list(c("User satisfaction\nReference: 89%", 
                        "94% satisfied", 
                        "99% satisfied"), 
                      c("Transparency\nReference: none",
                        'Transparency low',
                        'Transparency high'),
                      c("Costs\nReference: for free", 
                        '1.99 EUR',
                        '3.99 EUR'),
                      c("Energy efficiency\nReference: 1h",
                        'Energy 3h',
                        'Energy 5h')
) 



p1 <- {dwplot(modelframe, 
        vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) + 
    theme_bw() + scale_colour_grey() + theme(legend.position = "none") + xlab("Estimated partworth utilities") + ylab("")  } %>%   add_brackets(four_brackets) 


png("Fig_1.png", width=18, height=18,units="cm", res=400)

p1

dev.off()


pdf(paste0("Fig_1.pdf"), width=7, height=7)

p1

dev.off()




# alternative comand for plotting the part worth utilities

p1 <- plot_summs(ml1, scale = TRUE, colors = "grey20",
                 ci_level = 0.95) + 
  theme( axis.text.y=element_text(hjust=0.95,vjust=0.2)) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, by = 0.5)) +
  scale_y_discrete(labels = c('No-choice option',
                              'Energy 5h',
                              'Energy 3h',
                              '3.99 EUR',
                              '1.99 EUR',
                              'Transparency high',
                              'Transparency low',
                              '99% satisfied',
                              '94% satisfied'  )) +  theme(legend.position="bottom")

p1





#### Model with effects coding (shown in Annex A5 and A6) ####

ml1e <- mlogit(tmp_choice ~ NutzenE2 + NutzenE3 +
                TransparenzE2 + TransparenzE3 +
                KostenE2 + KostenE3 +
                EnergieE2 + EnergieE3 + 
                noneE | 0, cbc)
summary(ml1e)


# wordreg(  ml1e,   file = "model 1E.doc")


covMatrix <- vcov(ml1e)
sqrt(diag(covMatrix))


modelframe <- data.frame(term = c(
  '89% satisfied',
  '94% satisfied',
  '99% satisfied',
  'No transparency',
  'Transparency low',
  'Transparency high',
  'for free',
  '1.99 EUR',
  '3.99 EUR',
  'Energy 1h',
  'Energy 3h',
  'Energy 5h',
  'No-choice option'),
  estimate = c( -(ml1e$coefficients[1] + ml1e$coefficients[2]), ml1e$coefficients[1], ml1e$coefficients[2],
                -(ml1e$coefficients[3] + ml1e$coefficients[4]), ml1e$coefficients[3], ml1e$coefficients[4],
                -(ml1e$coefficients[5] + ml1e$coefficients[6]), ml1e$coefficients[5], ml1e$coefficients[6],
                -(ml1e$coefficients[7] + ml1e$coefficients[8]), ml1e$coefficients[7], ml1e$coefficients[8],
                ml1e$coefficients[9]),
  std.error = c( sqrt(sum(covMatrix[1:2, 1:2])), coef(summary(ml1e))[1, "Std. Error"], coef(summary(ml1e))[2, "Std. Error"], 
                 sqrt(sum(covMatrix[3:4, 3:4])), coef(summary(ml1e))[3, "Std. Error"], coef(summary(ml1e))[4, "Std. Error"], 
                 sqrt(sum(covMatrix[5:6, 5:6])), coef(summary(ml1e))[5, "Std. Error"], coef(summary(ml1e))[6, "Std. Error"], 
                 sqrt(sum(covMatrix[7:8, 7:8])), coef(summary(ml1e))[7, "Std. Error"], coef(summary(ml1e))[8, "Std. Error"],
                 coef(summary(ml1e))[9, "Std. Error"]
                 )
                        )

modelframe # get estimates (for Annex A5)

dwplot(modelframe)


# define additional labels in plot (bracketing the feature levels)
four_brackets <- list(c("User satisfaction", 
                        "89% satisfied", 
                        "99% satisfied"), 
                       c("Transparency",
                         'No transparency',
                         'Transparency high'),
                        c("Costs", 
                          'for free',
                          '3.99 EUR'),
                       c("Energy efficiency",
                         'Energy 1h',
                         'Energy 5h')
                         ) 



p1e <- {dwplot(modelframe, 
              vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) + 
    theme_bw() + scale_colour_grey() + theme(legend.position = "none") + xlab("Estimated partworth utilities") + ylab("") } %>%   add_brackets(four_brackets)


png("Fig_effects_coding.png", width=18, height=18,units="cm", res=400)

p1e

dev.off()







#### Point predictions for main model ####

#### Choose explainable AI with 1.99 Euros monthly  versus no costs and without these features ; user satisfaction is set to the reference category of 89% ####

tmp_ml <- ml1
p_pred_a <- exp(  tmp_ml$coefficients[4] + # explainable AI
                      tmp_ml$coefficients[5] )  /  # costs2
                                                        ( exp( tmp_ml$coefficients[4] + # explainable AI
                                                            tmp_ml$coefficients[5] ) + # costs 2
                                                           exp(  tmp_ml$coefficients[4]*0 + # NO explainable AI
                                                                   tmp_ml$coefficients[5]*0 ) # NO costs
                                                        )

p_pred_a # get predicted probability of choosing option A versus option B


### calculate confidence intervals

library(MASS)

m <- ml1
est_betas <- m$coefficients

sim_betas <- mvrnorm(1000, m$coefficients, vcov(m))

sim_ci <- data.frame()

  sim_preds <- apply(sim_betas, 1, function(x) {
    m$coefficients <- x
    exp(  m$coefficients[4] + # explainable AI
            m$coefficients[5] )  /  # costs2
      ( exp( m$coefficients[4] + # explainable AI
               m$coefficients[5] ) + # costs 2
          exp(  m$coefficients[4]*0 + # NO explainable AI
                  m$coefficients[5]*0 ) # NO costs
      )
    
  })
  
  sim_ci_a <- rbind(sim_ci, quantile(sim_preds, c(.025, .975)) )


colnames(sim_ci_a) <- c("lo", "hi")

sim_ci_a




#### Same hypothetical choice but with more user satisfaction (94%) ####

tmp_ml <- ml1
p_pred_b <- exp(    tmp_ml$coefficients[1] + # Nutzen2 - 94% user satisfaction
                  tmp_ml$coefficients[4] + # explainable AI
                  tmp_ml$coefficients[5] )  /  # costs2
  ( exp( tmp_ml$coefficients[1] + # # Nutzen2 - 94% user satisfaction
           tmp_ml$coefficients[4] + # explainable AI
           tmp_ml$coefficients[5] ) + # costs 2
      exp(  tmp_ml$coefficients[4]*0 + # NO explainable AI
              tmp_ml$coefficients[5]*0 ) # NO costs
  )

p_pred_b # get predicted probability of choosing option A versus option B



### calculate confidence intervals

m <- ml1
est_betas <- m$coefficients

sim_betas <- mvrnorm(1000, m$coefficients, vcov(m))

sim_ci <- data.frame()

sim_preds <- apply(sim_betas, 1, function(x) {
  m$coefficients <- x
  exp(    m$coefficients[1] + # Nutzen3
            m$coefficients[4] + # explainable AI
            m$coefficients[5] )  /  # costs2
    ( exp( m$coefficients[1] + # # Nutzen3
             m$coefficients[4] + # explainable AI
             m$coefficients[5] ) + # costs 2
        exp(  m$coefficients[4]*0 + # NO explainable AI
                m$coefficients[5]*0 ) # NO costs
    )
  
  
})

sim_ci_b <- rbind(sim_ci, quantile(sim_preds, c(.025, .975)) )


colnames(sim_ci_b) <- c("lo", "hi")

sim_ci_b







#### Estimated partworths by clusters ####

ml1g1 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen3 +
                  Transparenz2 + Transparenz3 +
                  Kosten2 + Kosten3 +
                  Energie2 + Energie3 + 
                  none | 0, cbc[cbc$kgroup == 1, ])
summary(ml1g1)

ml1g2 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen3 +
                  Transparenz2 + Transparenz3 +
                  Kosten2 + Kosten3 +
                  Energie2 + Energie3 + 
                  none | 0, cbc[cbc$kgroup == 2, ])
summary(ml1g2)

ml1g3 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen3 +
                  Transparenz2 + Transparenz3 +
                  Kosten2 + Kosten3 +
                  Energie2 + Energie3 + 
                  none | 0, cbc[cbc$kgroup == 3, ])
summary(ml1g3)



# Prepare results for plotting group 1
tmp <- ml1g1

covMatrix <- vcov(tmp)
sqrt(diag(covMatrix))

modelframe2a <- data.frame(term = c(
  '94% satisfied',
  '99% satisfied',
  'Transparency low',
  'Transparency high',
  '1.99 EUR',
  '3.99 EUR',
  'Energy 3h',
  'Energy 5h',
  'No-choice option'),
  estimate = c(  tmp$coefficients[1], tmp$coefficients[2],
                 tmp$coefficients[3], tmp$coefficients[4],
                 tmp$coefficients[5], tmp$coefficients[6],
                 tmp$coefficients[7], tmp$coefficients[8],
                 tmp$coefficients[9]),
  std.error = c( coef(summary(tmp))[1, "Std. Error"], coef(summary(tmp))[2, "Std. Error"], 
                 coef(summary(tmp))[3, "Std. Error"], coef(summary(tmp))[4, "Std. Error"], 
                 coef(summary(tmp))[5, "Std. Error"], coef(summary(tmp))[6, "Std. Error"], 
                 coef(summary(tmp))[7, "Std. Error"], coef(summary(tmp))[8, "Std. Error"],
                 coef(summary(tmp))[9, "Std. Error"]
  )
)



modelframe2a$term <- as.character(modelframe2a$term)
dwplot(modelframe2a)


# Prepare results for plotting group 2
tmp <- ml1g2

covMatrix <- vcov(tmp)
sqrt(diag(covMatrix))

modelframe2b <- data.frame(term = c(
  '94% satisfied',
  '99% satisfied',
  'Transparency low',
  'Transparency high',
  '1.99 EUR',
  '3.99 EUR',
  'Energy 3h',
  'Energy 5h',
  'No-choice option'),
  estimate = c(  tmp$coefficients[1], tmp$coefficients[2],
                 tmp$coefficients[3], tmp$coefficients[4],
                 tmp$coefficients[5], tmp$coefficients[6],
                 tmp$coefficients[7], tmp$coefficients[8],
                 tmp$coefficients[9]),
  std.error = c( coef(summary(tmp))[1, "Std. Error"], coef(summary(tmp))[2, "Std. Error"], 
                 coef(summary(tmp))[3, "Std. Error"], coef(summary(tmp))[4, "Std. Error"], 
                 coef(summary(tmp))[5, "Std. Error"], coef(summary(tmp))[6, "Std. Error"], 
                 coef(summary(tmp))[7, "Std. Error"], coef(summary(tmp))[8, "Std. Error"],
                 coef(summary(tmp))[9, "Std. Error"]
  )
)



modelframe2b$term <- as.character(modelframe2b$term)
dwplot(modelframe2b)


# Prepare results for plotting group 3
tmp <- ml1g3

covMatrix <- vcov(tmp)
sqrt(diag(covMatrix))

modelframe2c <- data.frame(term = c(
  '94% satisfied',
  '99% satisfied',
  'Transparency low',
  'Transparency high',
  '1.99 EUR',
  '3.99 EUR',
  'Energy 3h',
  'Energy 5h',
  'No-choice option'),
  estimate = c(  tmp$coefficients[1], tmp$coefficients[2],
                 tmp$coefficients[3], tmp$coefficients[4],
                 tmp$coefficients[5], tmp$coefficients[6],
                 tmp$coefficients[7], tmp$coefficients[8],
                 tmp$coefficients[9]),
  std.error = c( coef(summary(tmp))[1, "Std. Error"], coef(summary(tmp))[2, "Std. Error"], 
                 coef(summary(tmp))[3, "Std. Error"], coef(summary(tmp))[4, "Std. Error"], 
                 coef(summary(tmp))[5, "Std. Error"], coef(summary(tmp))[6, "Std. Error"], 
                 coef(summary(tmp))[7, "Std. Error"], coef(summary(tmp))[8, "Std. Error"],
                 coef(summary(tmp))[9, "Std. Error"]
  )
)


modelframe2c$term <- as.character(modelframe2c$term)
dwplot(modelframe2c)



# combine results for all three groups into one data frame for plotting

modelframe_allg <- rbind(modelframe2a, modelframe2b, modelframe2c) # dataframe with outputs for all groups

modelframe_allg$model <- rep(c("Group 1", "Group 2", "Group 3"), each = 9)

modelframe_allg <- modelframe_allg[order(modelframe_allg$model, decreasing = T), ]


modelframe_allg$model <- factor (modelframe_allg$model, 
                              levels = c("Group 1","Group 2","Group 3"), 
                              labels = c("Group 1: values\ncontrol", "Group 2:environmentally\nconcerned","Group 3: environmentally\nconcerned + values control"))
  

four_brackets <- list(c("User satisfaction\nReference: 89%", 
                        "94% satisfied", 
                        "99% satisfied"), 
                      c("Transparency\nReference: none",
                        'Transparency low',
                        'Transparency high'),
                      c("Costs\nReference: for free", 
                        '1.99 EUR',
                        '3.99 EUR'),
                      c("Energy efficiency\nReference: 1h",
                        'Energy 3h',
                        'Energy 5h')
) 


p2 <- {dwplot( modelframe_allg , 
              vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1),  
              model_order = c(
                "Group 1: values\ncontrol", "Group 2:environmentally\nconcerned","Group 3: environmentally\nconcerned + values control"
              ),
              dot_args = list(aes(shape = model) , size = 2.5  )  ) + 
    theme_bw( ) + xlab("Estimated partworth utilities") + ylab("") +  
        theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.title = element_blank()) + scale_color_grey(name = "Group", breaks=c("Group 1: values\ncontrol", "Group 2:environmentally\nconcerned","Group 3: environmentally\nconcerned + values control") ) + 
        scale_shape_discrete(name = "Group", breaks = c("Group 1: values\ncontrol", "Group 2:environmentally\nconcerned","Group 3: environmentally\nconcerned + values control") )  +
    guides(
      shape = guide_legend("Group"), 
      colour = guide_legend("Group"),
      size = guide_legend("Group")
    ) }   %>% 
  add_brackets(four_brackets)

p2



png("Fig_2.png", width=18, height=18,units="cm", res=400)

p2

dev.off()


pdf(paste0("Fig_2.pdf"), width=7, height=7)

p2

dev.off()





# alternative command for plotting the results for the three groups

p2 <- plot_summs(ml1g1, ml1g2, ml1g3, scale = TRUE, 
                 ci_level = 0.95, 
                 legend.title = "Group",
                 colors = c("grey20", "grey40", "grey70"),
                 model.names = c("g1", "g2", "g3")) + 
  theme( axis.text.y=element_text(hjust=0.95,vjust=0.2)) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, by = 0.5)) +
  scale_y_discrete(labels = c('No-choice option',
                              'Energy 5h',
                              'Energy 3h',
                              '3.99 EUR',
                              '1.99 EUR',
                              'Transparency high',
                              'Transparency low',
                              '99% satisfied',
                              '94% satisfied'  )) +  theme(legend.position="bottom")
p2





#### Inspect clusters further ####

# get cluster means
aggregate(df.AI[, c("control2", "environ2")], 
          by=list(df.AI$kgroup),
          FUN=mean)


aggregate(df.AI[, c("control2", 
                      "environ2",
                      "TE08_01",
                      "TE09_01",
                      "polint",
                      "female",
                      "DG02_01",
                      "education"
                      )], 
          by=list(df.AI$kgroup),
          FUN=mean, na.rm = T)


# ANOVAs over groups for select variables (shown in Annex A7)

res.aov <- aov(environ2 ~ kgroup, data = df.AI) # environmental concern
# Summary of the analysis
summary(res.aov)

res.aov <- aov(control2 ~ kgroup, data = df.AI) # desire for control
# Summary of the analysis
summary(res.aov)

res.aov <- aov(TE08_01 ~ kgroup, data = df.AI) # position socio-economic dimension
# Summary of the analysis
summary(res.aov)


res.aov <- aov(TE09_01 ~ kgroup, data = df.AI) # position environmental dimension
# Summary of the analysis
summary(res.aov)


res.aov <- aov(polint ~ kgroup, data = df.AI) # political interest
# Summary of the analysis
summary(res.aov)


res.aov <- aov(female ~ kgroup, data = df.AI) # female
# Summary of the analysis
summary(res.aov)


res.aov <- aov(DG02_01 ~ kgroup, data = df.AI) # age
# Summary of the analysis
summary(res.aov)


res.aov <- aov(education ~ kgroup, data = df.AI) # education
# Summary of the analysis
summary(res.aov)







#### Median split of respondents with environmental policy variable (shown in Annex A8) ####

cbc$TE09_01_bin[ cbc$TE09_01 < median(cbc$TE09_01, na.rm = T) ] <- 1 # prioritizing environmental protection over economic growth
cbc$TE09_01_bin[ cbc$TE09_01 >= median(cbc$TE09_01, na.rm = T) ] <- 0

table(cbc$TE09_01_bin)


ml_interact1 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen2:TE09_01_bin + Nutzen3 + Nutzen3:TE09_01_bin +
                        Transparenz2 + Transparenz2:TE09_01_bin + Transparenz3 + Transparenz3:TE09_01_bin +
                        Kosten2 + Kosten2:TE09_01_bin + Kosten3 + Kosten3:TE09_01_bin +
                        Energie2+ Energie2:TE09_01_bin + Energie3 + Energie3:TE09_01_bin + 
                        none + none:TE09_01_bin | 0, cbc)                  
summary(ml_interact1)



# wordreg(  ml_interact1,   file = "A model interact.doc")


ml_interact2 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen2:education + Nutzen3 + Nutzen3:education +
                        Transparenz2 + Transparenz2:education + Transparenz3 + Transparenz3:education +
                        Kosten2 + Kosten2:education + Kosten3 + Kosten3:education +
                        Energie2+ Energie2:education + Energie3 + Energie3:education + 
                        none + none:education | 0, cbc)                  
summary(ml_interact2)





# alternative command for plotting

p4 <- plot_summs(ml1g1, ml1g2, scale = TRUE, 
                 ci_level = 0.95, 
                 legend.title = "Group",
                 colors = c("grey20", "grey40"),
                 model.names = c("g1", "g2")) + 
  theme( axis.text.y=element_text(hjust=0.95,vjust=0.2)) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, by = 0.5)) +
  scale_y_discrete(labels = c('No-choice option',
                              'Energy 5h',
                              'Energy 3h',
                              '3.99 EUR',
                              '1.99 EUR',
                              'Transparency high',
                              'Transparency low',
                              '99% satisfied',
                              '94% satisfied'  )) +  theme(legend.position="bottom")
p4








#### Subgroup analysis: young, highly educated, environmentally conscious (shown in Annex A9) ####

summary(cbc$environ)

cbc$mlgroup <- 0
cbc$mlgroup[cbc$education == 1 & cbc$DG02_01 < 26 & cbc$environ > 4.571  ] <- 1
table(cbc$mlgroup)


ml3 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen3 +
                  Transparenz2 + Transparenz3 +
                  Kosten2 + Kosten3 +
                  Energie2 + Energie3 + 
                  none | 0, cbc[cbc$mlgroup == 1, ])
summary(ml3)


# wordreg(  ml3,   file = "Model interact mlgroup.doc")





#### Subgroup analysis for educated and market-liberal versus opposite group (shown in Annex A10) ####

summary(cbc$TE08_01)

cbc$group_price[cbc$education == 0 & cbc$TE08_01 >= 5  ] <- 0
cbc$group_price[cbc$education == 1 & cbc$TE08_01 <= 3  ] <- 1
table(cbc$group_price)


ml_interact4 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen2:group_price + Nutzen3 + Nutzen3:group_price + 
                Transparenz2 + Transparenz2:group_price + Transparenz3 + Transparenz3:group_price +
                Kosten2 + Kosten2:group_price + Kosten3 + Kosten3:group_price +
                Energie2 + Energie2:group_price + Energie3 + Energie3:group_price + 
                none + none:group_price | 0, cbc)
summary(ml_interact4)


# wordreg(  ml_interact4,   file = "Model interact pricesens.doc")

