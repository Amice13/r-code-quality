#Regression analysis
library(tidyverse)
library(broom)

#load data and MatchIt object
df <- read.csv("Data4.csv")
load("GeneticMatchesJanuary4final.RData")

balanceStats <- summary(m1)
p<-balanceStats$sum.all
p2<-balanceStats$sum.matched
p$model<-"All"
p2$model<-"Matched"

Bal <- bind_rows(p,p2 )



#Add panel stats to dataframe
panel<-read.csv("Panelvotes.csv")
df<-left_join(df, panel, by = "newid")
df$voteprop <- (df$Njudgesproresp-df$activist)/df$Njudges





df$weight  <- m1$weights
genetic.matches <- df[which(df$weight != 0),]

write.csv(genetic.matches,"MatchedDataJan4final.csv")

#Minimal Regression Models

mod1 <- lm(activist ~ 
             female + voteprop, 
           data = genetic.matches,
           weights = weight)
summary(mod1)

mod1.tidy <- tidy(mod1)
mod1.tidy$description <- "All Judgments"
mod1.tidy$applicants<- "All applicants"

mod1b <- lm(activist ~ 
              female + voteprop, 
            data = filter(genetic.matches, femaleapplicant==1),
            weights = weight)
mod1b.tidy <- tidy(mod1b)
mod1b.tidy$applicants<- "Female"
mod1b.tidy$description <- "All Judgments"

mod2 <- lm(activist ~ 
             female + voteprop, 
           data = filter(genetic.matches, physical==1),
           weights = weight)
mod2.tidy <- tidy(mod2)
mod2.tidy$description <- "Physical integrity rights"
mod2.tidy$applicants<- "All applicants"

mod2b <- lm(activist ~ 
              female + voteprop, 
            data = filter(genetic.matches, physical==1 & femaleapplicant==1),
            weights = weight)
mod2b.tidy <- tidy(mod2b)
mod2b.tidy$applicants <- "Female"
mod2b.tidy$description <- "Physical integrity rights"

mod3 <- lm(activist ~ 
             female + voteprop, 
           data = filter(genetic.matches, liberty==1),
           weights = weight)
mod3.tidy <- tidy(mod3)
mod3.tidy$description <- "Liberty"
mod3.tidy$applicants<- "All applicants"

mod3b <- lm(activist ~ 
              female + voteprop, 
            data = filter(genetic.matches, liberty==1 & femaleapplicant==1),
            weights = weight)
mod3b.tidy <- tidy(mod3b)
mod3b.tidy$description <- "Liberty"
mod3b.tidy$applicants <- "Female"

mod4 <- lm(activist ~ 
             female + voteprop, 
           data = filter(genetic.matches, prisoner==1),
           weights = weight)
mod4.tidy <- tidy(mod4)
mod4.tidy$description <- "Imprisoned applicant"
mod4.tidy$applicants<- "All applicants"

mod4b <- lm(activist ~ 
              female + voteprop, 
            data = filter(genetic.matches, prisoner==1  & femaleapplicant==1),
            weights = weight)
mod4b.tidy <- tidy(mod4b)
mod4b.tidy$applicants <- "Female"
mod4b.tidy$description <- "Imprisoned applicant"

mod5 <- lm(activist ~ 
             voteprop + female, 
           data = filter(genetic.matches,property==1),
           weights = weight)
mod5.tidy <- tidy(mod5)
mod5.tidy$description <- "Property"
mod5.tidy$applicants<- "All applicants"

mod5b <- lm(activist ~ 
              voteprop + female, 
            data = filter(genetic.matches,property==1  & femaleapplicant==1),
            weights = weight)
mod5b.tidy <- tidy(mod5b)
mod5b.tidy$description <- "Property"
mod5b.tidy$applicants <- "Female"

mod6 <- lm(activist ~ 
             female + voteprop, 
           data = filter(genetic.matches, speech==1),
           weights = weight)
mod6.tidy <- tidy(mod6)
mod6.tidy$description <- "Free Speech"
mod6.tidy$applicants<- "All applicants"


mod6b <- lm(activist ~ 
              female + voteprop, 
            data = filter(genetic.matches, speech==1 & femaleapplicant==1),
            weights = weight)
mod6b.tidy <- tidy(mod6b)
mod6b.tidy$description <- "Free Speech"
mod6b.tidy$applicants <- "Female"

mod7 <- lm(activist ~ 
             female + voteprop, 
           data = filter(genetic.matches, fairtrial==1),
           weights = weight)
mod7.tidy <- tidy(mod7)
mod7.tidy$description <- "Fair Trial"
mod7.tidy$applicants<- "All applicants"

mod7b <- lm(activist ~ 
              female + voteprop, 
            data = filter(genetic.matches, fairtrial==1 & femaleapplicant==1),
            weights = weight)
mod7b.tidy <- tidy(mod7b)
mod7b.tidy$description <- "Fair Trial"
mod7b.tidy$applicants <- "Female"

mod8 <- lm(activist ~ 
             female + voteprop, 
           data = filter(genetic.matches, discrimination==1),
           weights = weight)
mod8.tidy <- tidy(mod8)
mod8.tidy$description <- "Discrimination"
mod8.tidy$applicants<- "All applicants"

mod8b <- lm(activist ~ 
              female + voteprop, 
            data = filter(genetic.matches, discrimination==1 & femaleapplicant==1),
            weights = weight)
mod8b.tidy <- tidy(mod8b)
mod8b.tidy$description <- "Discrimination"
mod8b.tidy$applicants <- "Female"


mod9 <- lm(activist ~ 
             female + voteprop, 
           data = filter(genetic.matches, privacy==1),
           weights = weight)
mod9.tidy <- tidy(mod9)
mod9.tidy$description <- "Privacy"
mod9.tidy$applicants<- "All applicants"

mod9b <- lm(activist ~ 
              female + voteprop, 
            data = filter(genetic.matches, privacy==1 & femaleapplicant==1),
            weights = weight)
mod9b.tidy <- tidy(mod9b)
mod9b.tidy$description <- "Privacy"
mod9b.tidy$applicants <- "Female"


Coef <- bind_rows(mod1.tidy,mod2.tidy,mod3.tidy,mod4.tidy,mod5.tidy,mod6.tidy,mod7.tidy,mod8.tidy,mod9.tidy,
                  mod1b.tidy,mod2b.tidy,mod3b.tidy,mod4b.tidy,mod5b.tidy,mod6b.tidy,mod7b.tidy,mod8b.tidy,mod9b.tidy )

Coef$conf.low <- Coef$estimate - 1.96*abs(Coef$std.error)
Coef$conf.high <-Coef$estimate + 1.96*abs(Coef$std.error)

FemaleCoef <- filter(Coef,term=="female")

FemaleCoef$term<-FemaleCoef$description
FemaleCoef$model<-FemaleCoef$applicants
library(dotwhisker)
dwplot(FemaleCoef)


coefplot <- dwplot(FemaleCoef, 
                   # here are our regular aesthetics
                   dot_args = list(aes(colour = model, 
                                       shape = model)), 
                   size = 3) + 
  theme_bw() + 
  labs(title = "Effect of Female Judge on Probability of Violation", 
       x = "Marginal Effect with 95% CIs", 
       y = "Issue") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_shape_discrete(name  ="Models", breaks = c(0, 1)) + # breaks assign shapes
  scale_colour_grey(start = .3, end = .7, name = "Models") # start/end for light/dark greys

png("Coeffigure1Jan4.png",  width = 6, height = 8, units = 'in', res = 300)
coefplot
dev.off()

#Based on full regression models


mod1 <- lm(activist ~ 
             female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear  , 
           data = genetic.matches,
           weights = weight)
summary(mod1)

mod1.tidy <- tidy(mod1)
mod1.tidy$description <- "All Judgments"
mod1.tidy$applicants<- "All applicants"

mod1b <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, femaleapplicant==1),
            weights = weight)
mod1b.tidy <- tidy(mod1b)
mod1b.tidy$applicants<- "Female"
mod1b.tidy$description <- "All Judgments"

mod1c <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, maleapplicant==1),
            weights = weight)
mod1c.tidy <- tidy(mod1c)
mod1c.tidy$applicants<- "Male"
mod1c.tidy$description <- "All Judgments"

mod2 <- lm(activist ~ 
             female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear, 
           data = filter(genetic.matches, physical==1),
           weights = weight)
mod2.tidy <- tidy(mod2)
mod2.tidy$description <- "Physical integrity rights"
mod2.tidy$applicants<- "All applicants"

mod2b <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, physical==1 & femaleapplicant==1),
            weights = weight)
mod2b.tidy <- tidy(mod2b)
mod2b.tidy$applicants <- "Female"
mod2b.tidy$description <- "Physical integrity rights"

mod2c <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, physical==1 & maleapplicant==1),
            weights = weight)
mod2c.tidy <- tidy(mod2c)
mod2c.tidy$applicants <- "Male"
mod2c.tidy$description <- "Physical integrity rights"

mod3 <- lm(activist ~ 
             female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear, 
           data = filter(genetic.matches, liberty==1),
           weights = weight)
mod3.tidy <- tidy(mod3)
mod3.tidy$description <- "Liberty"
mod3.tidy$applicants<- "All applicants"

mod3b <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, liberty==1 & femaleapplicant==1),
            weights = weight)
mod3b.tidy <- tidy(mod3b)
mod3b.tidy$description <- "Liberty"
mod3b.tidy$applicants <- "Female"

mod3c <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, liberty==1 & maleapplicant==1),
            weights = weight)
mod3c.tidy <- tidy(mod3c)
mod3c.tidy$description <- "Liberty"
mod3c.tidy$applicants <- "Male"

mod4 <- lm(activist ~ 
             female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear, 
           data = filter(genetic.matches, prisoner==1),
           weights = weight)
mod4.tidy <- tidy(mod4)
mod4.tidy$description <- "Imprisoned applicant"
mod4.tidy$applicants<- "All applicants"

mod4b <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, prisoner==1  & femaleapplicant==1),
            weights = weight)
mod4b.tidy <- tidy(mod4b)
mod4b.tidy$applicants <- "Female"
mod4b.tidy$description <- "Imprisoned applicant"

mod4c <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, prisoner==1  & maleapplicant==1),
            weights = weight)
mod4c.tidy <- tidy(mod4c)
mod4c.tidy$applicants <- "Male"
mod4c.tidy$description <- "Imprisoned applicant"

mod5 <- lm(activist ~ 
             voteprop + female + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear, 
           data = filter(genetic.matches,property==1),
           weights = weight)
mod5.tidy <- tidy(mod5)
mod5.tidy$description <- "Property"
mod5.tidy$applicants<- "All applicants"

mod5b <- lm(activist ~ 
              voteprop + female + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches,property==1  & femaleapplicant==1),
            weights = weight)
mod5b.tidy <- tidy(mod5b)
mod5b.tidy$description <- "Property"
mod5b.tidy$applicants <- "Female"

mod5c <- lm(activist ~ 
              voteprop + female + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches,property==1  & maleapplicant==1),
            weights = weight)
mod5c.tidy <- tidy(mod5c)
mod5c.tidy$description <- "Property"
mod5c.tidy$applicants <- "Male"

mod6 <- lm(activist ~ 
             female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear, 
           data = filter(genetic.matches, speech==1),
           weights = weight)
mod6.tidy <- tidy(mod6)
mod6.tidy$description <- "Free Speech"
mod6.tidy$applicants<- "All applicants"


mod6b <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, speech==1 & femaleapplicant==1),
            weights = weight)
mod6b.tidy <- tidy(mod6b)
mod6b.tidy$description <- "Free Speech"
mod6b.tidy$applicants <- "Female"

mod6c <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, speech==1 & maleapplicant==1),
            weights = weight)
mod6c.tidy <- tidy(mod6c)
mod6c.tidy$description <- "Free Speech"
mod6c.tidy$applicants <- "Male"

mod7 <- lm(activist ~ 
             female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear, 
           data = filter(genetic.matches, fairtrial==1),
           weights = weight)
mod7.tidy <- tidy(mod7)
mod7.tidy$description <- "Fair Trial"
mod7.tidy$applicants<- "All applicants"

mod7b <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, fairtrial==1 & femaleapplicant==1),
            weights = weight)
mod7b.tidy <- tidy(mod7b)
mod7b.tidy$description <- "Fair Trial"
mod7b.tidy$applicants <- "Female"

mod7c <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, fairtrial==1 & maleapplicant==1),
            weights = weight)
mod7c.tidy <- tidy(mod7c)
mod7c.tidy$description <- "Fair Trial"
mod7c.tidy$applicants <- "Male"


mod8 <- lm(activist ~ 
             female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear, 
           data = filter(genetic.matches, discrimination==1),
           weights = weight)
mod8.tidy <- tidy(mod8)
mod8.tidy$description <- "Discrimination"
mod8.tidy$applicants<- "All applicants"

mod8b <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, discrimination==1 & femaleapplicant==1),
            weights = weight)
mod8b.tidy <- tidy(mod8b)
mod8b.tidy$description <- "Discrimination"
mod8b.tidy$applicants <- "Female"

mod8c <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, discrimination==1 & maleapplicant==1),
            weights = weight)
mod8c.tidy <- tidy(mod8c)
mod8c.tidy$description <- "Discrimination"
mod8c.tidy$applicants <- "Male"


mod9 <- lm(activist ~ 
             female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
             right + religious + nationalist + legor_uk + 
             legor_fr +appointmentyear, 
           data = filter(genetic.matches, privacy==1),
           weights = weight)
mod9.tidy <- tidy(mod9)
mod9.tidy$description <- "Privacy"
mod9.tidy$applicants<- "All applicants"

mod9b <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, privacy==1 & femaleapplicant==1),
            weights = weight)
mod9b.tidy <- tidy(mod9b)
mod9b.tidy$description <- "Privacy"
mod9b.tidy$applicants <- "Female"

mod9c <- lm(activist ~ 
              female + voteprop + v2x_gender + v2x_liberal + diplomat + left + 
              right + religious + nationalist + legor_uk + 
              legor_fr +appointmentyear, 
            data = filter(genetic.matches, privacy==1 & maleapplicant==1),
            weights = weight)
mod9c.tidy <- tidy(mod9c)
mod9c.tidy$description <- "Privacy"
mod9c.tidy$applicants <- "Male"


Coef <- bind_rows(mod1.tidy,mod2.tidy,mod3.tidy,mod4.tidy,mod5.tidy,mod6.tidy,mod7.tidy,mod8.tidy,mod9.tidy,
                  mod1b.tidy,mod2b.tidy,mod3b.tidy,mod4b.tidy,mod5b.tidy,mod6b.tidy,mod7b.tidy,mod8b.tidy,mod9b.tidy,
                  mod1c.tidy,mod2c.tidy,mod3c.tidy,mod4c.tidy,mod5c.tidy,mod6c.tidy,mod7c.tidy,mod8c.tidy,mod9c.tidy)

Coef$conf.low <- Coef$estimate - 1.96*abs(Coef$std.error)
Coef$conf.high <-Coef$estimate + 1.96*abs(Coef$std.error)

write.csv(Coef,"CoefficientsGeneticMatchingJan4MB.csv")

FemaleCoef <- filter(Coef,term=="female")

FemaleCoef$term<-FemaleCoef$description
FemaleCoef$model<-FemaleCoef$applicants
library(dotwhisker)
dwplot(FemaleCoef)

Coef$model<-Coef$description


coefplot <- dwplot(FemaleCoef, 
                   # here are our regular aesthetics
                   dot_args = list(aes(colour = model, 
                                       shape = model)), 
                   size = 3) + 
  theme_light() + scale_colour_manual(values=c("black", "darkgray","gray"))+
  scale_size_manual(values=c(8,8,8)) +
  labs(title = "Effect of Female Judge on Probability of Violation", 
       x = "Marginal Effect with 95% CIs", 
       y = "Issue") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) + geom_vline(xintercept = 0)
  

coefplot

png("Coeffigure2Jan4final.png",  width = 6, height = 8, units = 'in', res = 300)
coefplot
dev.off()


