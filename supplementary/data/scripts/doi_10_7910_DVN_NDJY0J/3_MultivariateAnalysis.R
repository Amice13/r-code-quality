###################################################################
# Project:  Replication of Rauh/Bes/Schoonvelde 2019 EJPR
#
# Task:     Multivariate regression models and their visualization
#           Section four in the main text
#
# Author:   Christian Rauh / Martijn Schoonvelde
# Date:     22.02.2019
##################################################################

# R version 3.4.4 (2018-03-15)

# Packages ####
library(tidyverse) # 1.2.1
library(stargazer) # 5.2
library(interplot) # 0.1.5
library(cowplot) # 0.8.0
library(sandwich) # 2.4-0


# Set YOUR working directory here ####
# Root folder of replication archive, relative paths used from hereon
setwd("Your/Working/Directory") 

# remove scientific notation
options(scipen = 999)


# Load the data ###
corpus <- readRDS("./data/RauhBesSchoonvelde_ReplData.Rds") 



# Data selection and preparation ####

# NOTE: if you want to replicate the results for the log-scaled sentiment scores (FN 9 in main text), 
# you can replace the 'sent.eusentences' variable with 'sent.log.eusentences' in the function below

# Establish a custom  function
cut <- function(actor) { # Specify EUspeech actor type as character
  
  # Cut out data of interest
  help <- corpus[corpus$institution == actor, ]
  
  # Keep variables of interest
  df <- help[ , c("date", "country", "speaker",
                  "sent.eusentences", 
                  "complscore.eusentences",
                  "eb.image",
                  "challenger.share",
                  "EUpos", 
                  "eu.trade.dependence", "net.contribution", "ez.bond.spread")]

  # Invert reading ease score to interpret it as complexity
  df$complscore.eusentences <- -df$complscore.eusentences + 100
  
  # Invert EB image vars to interpret them as Euroscepticism
  df$eb.image <- -df$eb.image
  
  # Exponentiated version of Euroscepticism measures
  df$eb.image.exp <- exp(df$eb.image)
  df$challenger.share.exp <- exp(df$challenger.share)
  
  # Name vars meaningfully
  names(df) <- c("Date", "Country", "Speaker",
                 "Sentiment", 
                 "Complexity",
                 "Euroscepticism", 
                 "PartyChallengers",
                 "PartyEUpos", 
                 "TradeDependence", "NetContr", "EZBondSpread",
                 "EuroscepticismExp", "PartyChallengersExp")
  
  # Output
  return(df)
  
}

# EU Commission data
com <- cut("EU Commission")
com <- subset(com, Complexity > 0 & Complexity < 100) # Subset complexity within theoretically valid range (others are scraping errors), also excludes speeches w/out integration reference
sum(complete.cases(com))

# National leaders data
nat <- cut("Nat. leader")
nat <- subset(nat, Complexity > 0 & Complexity < 100) # Subset complexity within theoretically valid range (others are scraping errors), also excludes speeches w/out integration reference
sum(complete.cases(nat))


# Export descriptive statistics ####
# Appendix B to the main text

output <- stargazer(nat, median = T,
                    type = "html", title = "")
write(output, file = "./tables/TabB1_DescriptiveStats_NatLeaders.html")

output <- stargazer(com, median = T,
                    type = "html", title = "")
write(output, file = "./tables/TabB2_DescriptiveStats_Commissioners.html")



# Set up the the different regression models ####

# Base model

regress <- function(df, DV){ # Supply dataframe and DV
  
  # Regresion model
  fit <- lm(scale(df[,DV]) ~ 
              scale(Euroscepticism) + scale(PartyChallengers) + scale(PartyEUpos) +
              scale(TradeDependence) + scale(NetContr) + scale(EZBondSpread), na.action = na.exclude,
            data = df)
  # Output
  return(fit)
  
}

# Model with exponentiated Euroscepticism

regress.exp <- function(df, DV){ # Supply dataframe and DV
  
  # Regresion model
  fit <- lm(scale(df[,DV]) ~ 
              scale(Euroscepticism) + scale(PartyChallengers) + scale(EuroscepticismExp) + scale(PartyChallengersExp) +
              scale(PartyEUpos) +
              scale(TradeDependence) + scale(NetContr) + scale(EZBondSpread), na.action = na.exclude,
            data = df)
  # Output
  return(fit)
  
}


# Model with exponentiated Euroscepticism and interaction term

regress.iac <- function(df, DV){ # Supply dataframe and DV
  
  # Regresion model
  fit <- lm(scale(df[,DV]) ~ 
              scale(Euroscepticism) + scale(PartyChallengers) + scale(EuroscepticismExp) + scale(PartyChallengersExp) + 
              scale(Euroscepticism)*scale(PartyChallengers) + 
              scale(PartyEUpos) +
              scale(TradeDependence) + scale(NetContr) + scale(EZBondSpread), na.action = na.exclude,
            data = df)
  # Output
  return(fit)
  
}




# Estimate and plot the models for national leaders' speeches ####

# Fit OLS models

compl.fit <- regress(nat, "Complexity")
compl.fit2 <- regress.exp(nat, "Complexity")
compl.fit3 <- regress.iac(nat, "Complexity")
senti.fit <- regress(nat, "Sentiment")
senti.fit2 <- regress.exp(nat, "Sentiment")
senti.fit3 <- regress.iac(nat, "Sentiment")

# Calculate robust SEs
cov <- vcovHC(compl.fit, type = "HC")
compl.robust.se <- sqrt(diag(cov))
cov <- vcovHC(compl.fit2, type = "HC")
compl.robust.se2 <- sqrt(diag(cov))
cov <- vcovHC(compl.fit3, type = "HC")
compl.robust.se3 <- sqrt(diag(cov))
cov <- vcovHC(senti.fit, type = "HC")
senti.robust.se <- sqrt(diag(cov))
cov <- vcovHC(senti.fit2, type = "HC")
senti.robust.se2 <- sqrt(diag(cov))
cov <- vcovHC(senti.fit3, type = "HC")
senti.robust.se3 <- sqrt(diag(cov))

# Export regression tables with robust errors (Table 2 in main text)
var.labels <- c("Public Euroscepticism", "Eurosceptic party strength", "Public Euroscepticism (exp)", "Eurosceptic party strength (exp)", "Public x Partisan Euroscept.", "Party EU position", "EU trade dependence", "Net Contribution", "Eurozone Bond Spread")

output <- stargazer(senti.fit, senti.fit2, senti.fit3, compl.fit, compl.fit2, compl.fit3, align = TRUE,
                    type = "html", title = "National leaders", no.space=TRUE, single.row = F,
                    column.labels = c("Sentiment of integration references", "Complexity of integration references"),
                    column.separate = c(3, 3),
                    se=list(compl.robust.se, compl.robust.se2, compl.robust.se3, senti.robust.se, senti.robust.se2, senti.robust.se3),
                    order = c(1, 2, 3, 4, 9, 5, 6, 7, 8),
                    covariate.labels = var.labels,
                    model.names = FALSE, dep.var.labels.include = FALSE, dep.var.caption = "XXX",
                    notes = "Standardized coefficients, robust standard errors in parentheses. *** p<0.001, ** p<0.01, * p<0.05",
                    style = "io",
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    star.char = c("*", "**", "***"),
                    notes.append = FALSE)

# Manually edit output
output <- append(output, "<tr><td colspan=\"7\" style=\"border-bottom: 1px solid black\">", after=24)
output <- append(output, "<tr><td colspan=\"7\" style=\"border-bottom: 1px dashed black\">", after=14)

# Export
write(output, file = "./tables/Tab2_RegressionModels_NatLeaders.html")


# Interaction plots for robust OLS
names(compl.fit3$coefficients) <- gsub("scale", "", names(compl.fit3$coefficients))
names(compl.fit3$coefficients) <- gsub("\\(|\\)", "", names(compl.fit3$coefficients))
names(compl.fit3$model) <- gsub("scale", "", names(compl.fit3$model))
names(compl.fit3$model) <- gsub("\\(|\\)", "", names(compl.fit3$model))

compl.iac <- interplot(m=compl.fit3, var1 = "Euroscepticism", var2 = "PartyChallengers", hist = F) +
  xlab("\nStrength of Eurosceptic challenger parties (standardized)")+
  ylab("\nStandardized effect of Public Euroscepticism\non complexity of  European integration references\n")+
  ggtitle("\nComplexity\n")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  coord_cartesian(ylim = c(-0.3, 0.6), xlim = c(-1.2, 3.5))+ 
  theme_bw()


names(senti.fit3$coefficients) <- gsub("scale", "", names(senti.fit3$coefficients))
names(senti.fit3$coefficients) <- gsub("\\(|\\)", "", names(senti.fit3$coefficients))
names(senti.fit3$model) <- gsub("scale", "", names(senti.fit3$model))
names(senti.fit3$model) <- gsub("\\(|\\)", "", names(senti.fit3$model))

senti.iac <- interplot(m=senti.fit3, var1 = "Euroscepticism", var2 = "PartyChallengers", hist = F) +
  xlab("\nStrength of Eurosceptic challenger parties (standardized)")+
  ylab("\nStandardized effect of Public Euroscepticism\non sentiment of  European integration references")+
  ggtitle("\nSentiment\n")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  coord_cartesian(ylim = c(-0.3, 0.6), xlim = c(-1.2, 3.5))+ 
  theme_bw()

challenger.hist <- ggplot()+
  geom_histogram(data=nat, aes(x=scale(PartyChallengers)), colour = NA, fill = "grey73")+
  coord_cartesian(xlim = c(-1.2, 3.5))+
  xlab("")+
  ylab("Freq.")+
  theme_bw()

combined <- plot_grid(senti.iac, compl.iac, challenger.hist, challenger.hist, align = "v", rel_heights=c(0.85, 0.15))
ggsave("./figures/Fig3_InteractionEffects_NatLeaders.jpg", combined, width = 32, height = 18, units = "cm")




# Estimate and plot the models for EU Commmisoiners' speeches ####


# Fit OLS models
compl.fit <- regress(com, "Complexity")
compl.fit2 <- regress.exp(com, "Complexity")
compl.fit3 <- regress.iac(com, "Complexity")
senti.fit <- regress(com, "Sentiment")
senti.fit2 <- regress.exp(com, "Sentiment")
senti.fit3 <- regress.iac(com, "Sentiment")


# Calculate robust SEs
cov <- vcovHC(compl.fit, type = "HC")
compl.robust.se <- sqrt(diag(cov))
cov <- vcovHC(compl.fit2, type = "HC")
compl.robust.se2 <- sqrt(diag(cov))
cov <- vcovHC(compl.fit3, type = "HC")
compl.robust.se3 <- sqrt(diag(cov))
cov <- vcovHC(senti.fit, type = "HC")
senti.robust.se <- sqrt(diag(cov))
cov <- vcovHC(senti.fit2, type = "HC")
senti.robust.se2 <- sqrt(diag(cov))
cov <- vcovHC(senti.fit3, type = "HC")
senti.robust.se3 <- sqrt(diag(cov))


# Export regression tables with robust errors
var.labels <- c("Public Euroscepticism", "Eurosceptic party strength", "Public Euroscepticism (exp)", "Eurosceptic party strength (exp)", "Public x Partisan Euroscept.", "Party EU position", "EU trade dependence", "Net Contribution", "Eurozone Bond Spread")
output <- stargazer(senti.fit, senti.fit2, senti.fit3, compl.fit, compl.fit2, compl.fit3, align = TRUE,
                    type = "html", title = "EU Commissioners", no.space=TRUE, single.row = F,
                    column.labels = c("Sentiment of integration references", "Complexity of integration references"),
                    column.separate = c(3, 3),
                    se=list(compl.robust.se, compl.robust.se2, compl.robust.se3, senti.robust.se, senti.robust.se2, senti.robust.se3),
                    order = c(1, 2, 3, 4, 9, 5, 6, 7, 8),
                    covariate.labels = var.labels,
                    model.names = FALSE, dep.var.labels.include = FALSE, dep.var.caption = "XXX",
                    notes = "Standardized coefficients, robust standard errors in parentheses. *** p<0.001, ** p<0.01, * p<0.05",
                    style = "io",
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    star.char = c("*", "**", "***"),
                    notes.append = FALSE)

# Manually edit output
output <- append(output, "<tr><td colspan=\"7\" style=\"border-bottom: 1px solid black\">", after=24)
output <- append(output, "<tr><td colspan=\"7\" style=\"border-bottom: 1px dashed black\">", after=14)

# Export
write(output, file = "./tables/Tab3_RegressionModels_EUcommissioners.html")


# Interaction plots for robust OLS
names(compl.fit3$coefficients) <- gsub("scale", "", names(compl.fit3$coefficients))
names(compl.fit3$coefficients) <- gsub("\\(|\\)", "", names(compl.fit3$coefficients))
names(compl.fit3$model) <- gsub("scale", "", names(compl.fit3$model))
names(compl.fit3$model) <- gsub("\\(|\\)", "", names(compl.fit3$model))
compl.iac <- interplot(m=compl.fit3, var1 = "Euroscepticism", var2 = "PartyChallengers", hist = F) +
  xlab("\nStrength of Eurosceptic challenger parties (standardized)")+
  ylab("\nStandardized effect of Public Euroscepticism\non complexity of  European integration references\n")+
  ggtitle("\nComplexity\n")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  coord_cartesian(ylim = c(-0.2, 0.4), xlim = c(-1.2, 3.5))+ 
  theme_bw()

names(senti.fit3$coefficients) <- gsub("scale", "", names(senti.fit3$coefficients))
names(senti.fit3$coefficients) <- gsub("\\(|\\)", "", names(senti.fit3$coefficients))
names(senti.fit3$model) <- gsub("scale", "", names(senti.fit3$model))
names(senti.fit3$model) <- gsub("\\(|\\)", "", names(senti.fit3$model))
senti.iac <- interplot(m=senti.fit3, var1 = "Euroscepticism", var2 = "PartyChallengers", hist = F) +
  xlab("\nStrength of Eurosceptic challenger parties (standardized)")+
  ylab("\nStandardized effect of Public Euroscepticism\non sentiment of  European integration references")+
  ggtitle("\nSentiment\n")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  coord_cartesian(ylim = c(-0.2, 0.4), xlim = c(-1.2, 3.5))+ 
  theme_bw()

challenger.hist <- ggplot()+
  geom_histogram(data=com, aes(x=scale(PartyChallengers)), colour = NA, fill = "grey73")+
  coord_cartesian(xlim = c(-1.2, 3.5))+
  xlab("")+
  ylab("Freq.")+
  theme_bw()


combined <- plot_grid(senti.iac, compl.iac, challenger.hist, challenger.hist, align = "v", rel_heights=c(0.85, 0.15))
ggsave("./figures/Fig4_InteractionEffects_EUcommissioners.jpg", combined, width = 32, height = 18, units = "cm")
