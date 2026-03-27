###################################################################
# Project:  Replication of Rauh/Bes/Schoonvelde 2019 EJPR
#
# Task:     Re-estimates models for nat. leaders with polling data
#           Appendix B of the main text
#
# Author:   Martijn Schoonvelde / Christian Rauh
# Date:     22.02.2019
##################################################################

# R version 3.4.4 (2018-03-15)

# Packages ####
library(tidyverse) # 1.2.1
library(stargazer) # 5.2
library(interplot) # 0.1.5
library(cowplot) # 0.8.0
library(sandwich) # 2.4-0
library(zoo) # 1.8-0


# Set YOUR working directory here ####
# Root folder of replication archive, relative paths used from hereon
setwd("Your/Working/Directory") 


# remove scientific notation
options(scipen = 999)


# Load the data ###
corpus <- readRDS("./data/RauhBesSchoonvelde_ReplData.Rds") 

# Add zero polling for Spain (absent challenger parties)
corpus$poll[corpus$country == "ES"] <- 0



# Data selection and cleaning ####


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
                  "EUpos", "poll",
                  "eu.trade.dependence", "net.contribution", "ez.bond.spread")]

  # Invert reading ease score to interpret it as complexity
  df$complscore.eusentences <- -df$complscore.eusentences + 100
  
  # Invert EB image vars to interpret them as Euroscepticism
  df$eb.image <- -df$eb.image


  df$poll <- df$poll*100

  # Exponentiated version of Euroscepticism measures
  df$eb.image.exp <- exp(df$eb.image)
  df$poll.exp <- exp(df$poll)
  
  # Name vars meaningfully
  names(df) <- c("Date", "Country", "Speaker",
                 "Sentiment", 
                 "Complexity",
                 "Euroscepticism", 
                 "PartyChallengers",
                 "PartyEUpos", "Poll",
                 "TradeDependence", "NetContr", "EZBondSpread",
                 "EuroscepticismExp", "PollExp")
  
  # Output
  return(df)
  
}


# National leaders
nat <- cut("Nat. leader")
nat <- subset(nat, Complexity > 0 & Complexity < 100)
sum(complete.cases(nat))


# Set up Linear regression models ####

# Base model

regress <- function(df, DV){ # Supply dataframe and DV
  
  # Regresion model
  fit <- lm(scale(df[,DV]) ~ 
              scale(Euroscepticism) + scale(Poll) + scale(PartyEUpos) +
              scale(TradeDependence) + scale(NetContr) + scale(EZBondSpread), na.action = na.exclude,
            data = df)
  # Output
  return(fit)
  
}


# Model with exponentiated Euroscepticism

regress.exp <- function(df, DV){ # Supply dataframe and DV
  
  # Regresion model
  fit <- lm(scale(df[,DV]) ~ 
              scale(Euroscepticism) + scale(Poll) + scale(EuroscepticismExp) + scale(PollExp) +
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
              scale(Euroscepticism) + scale(Poll) + scale(EuroscepticismExp) + scale(PollExp) + 
              scale(Euroscepticism)*scale(Poll) + 
              scale(PartyEUpos) +
              scale(TradeDependence) + scale(NetContr) + scale(EZBondSpread), na.action = na.exclude,
            data = df)
  # Output
  return(fit)
  
}


# Models national leaders ####


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


# Export regression tables with robust errors

var.labels <- c("Public Euroscepticism", "Eurosceptic party poll rating", "Public Euroscepticism (exp)", "Eurosceptic party poll rating (exp)", "Public x Partisan Euroscept.", "Party EU position", "EU trade dependence", "Net Contribution", "Eurozone Bond Spread")

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

#manually edit output
output <- append(output, "<tr><td colspan=\"7\" style=\"border-bottom: 1px solid black\">", after=24)
output <- append(output, "<tr><td colspan=\"7\" style=\"border-bottom: 1px dashed black\">", after=14)

# Export
write(output, file = "./tables/TabC1_RegressionModels_NatLeaders_Polling.html")


# Interaction plots for robust OLS
#---------------------------------

names(compl.fit3$coefficients) <- gsub("scale", "", names(compl.fit3$coefficients))
names(compl.fit3$coefficients) <- gsub("\\(|\\)", "", names(compl.fit3$coefficients))
names(compl.fit3$model) <- gsub("scale", "", names(compl.fit3$model))
names(compl.fit3$model) <- gsub("\\(|\\)", "", names(compl.fit3$model))

compl.iac <- interplot(m=compl.fit3, var1 = "Euroscepticism", var2 = "Poll", hist = F) +
  xlab("\nStrength of Eurosceptic challenger parties in the polls (standardized)")+
  ylab("\nStandardized effect of Public Euroscepticism\non complexity of  European integration references\n")+
  ggtitle("\nComplexity\n")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  #coord_cartesian(ylim = c(-0.3, 0.6), xlim = c(-1.2, 3.5))+ 
  theme_bw()


names(senti.fit3$coefficients) <- gsub("scale", "", names(senti.fit3$coefficients))
names(senti.fit3$coefficients) <- gsub("\\(|\\)", "", names(senti.fit3$coefficients))
names(senti.fit3$model) <- gsub("scale", "", names(senti.fit3$model))
names(senti.fit3$model) <- gsub("\\(|\\)", "", names(senti.fit3$model))

senti.iac <- interplot(m=senti.fit3, var1 = "Euroscepticism", var2 = "Poll", hist = F) +
  xlab("\nStrength of Eurosceptic challenger parties in the polls (standardized)")+
  ylab("\nStandardized effect of Public Euroscepticism\non sentiment of  European integration references")+
  ggtitle("\nSentiment\n")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  #coord_cartesian(ylim = c(-0.3, 0.6), xlim = c(-1.2, 3.5))+ 
  theme_bw()

challenger.hist <- ggplot()+
  geom_histogram(data=nat, aes(x=scale(Poll)), colour = NA, fill = "grey73")+
  #coord_cartesian(xlim = c(-1.2, 3.5))+
  xlab("")+
  ylab("Freq.")+
  theme_bw()


combined <- plot_grid(senti.iac, compl.iac, challenger.hist, challenger.hist, align = "v", rel_heights=c(0.85, 0.15))
# title <- ggdraw() + draw_label("European Commissioners", fontface='bold')
# combined <- plot_grid(title, combined, ncol = 1, rel_heights=c(0.1, 1))
ggsave("./figures/FigC2_InteractionEffects_NatLeaders_Polling.jpg", combined, width = 32, height = 18, units = "cm")



# Plot available polling data ####

load(file = "./data/AggregatedPollingData_JenningsWlezien_Wikipedia.Rdata")

ggplot(data = aggregated.polling.data, aes(x = quarter, y = poll*100, group = country))+
  geom_line(size = 1) +
  xlab("Quarter")+ scale_x_yearqtr(format = "%Y") +
  #xlab("Quarter") +
  ylab("Combined poll shares of Eurosceptic parties")+
  facet_wrap( ~ country ) +
  theme_bw()

ggsave(file ="./figures/FigC1_PollsEuroscpeticPartiesByQuarter.jpg", height = 20, width = 30, units = "cm")  
