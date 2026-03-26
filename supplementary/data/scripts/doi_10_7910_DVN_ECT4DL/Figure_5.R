######################## Code Summary ##################
#Replication for Polarization In COVID-19 Vaccine Discussion Networks
#American Politics Research
#Amlani, Butters, and Kiesel (2022)
#R Version: 4.1.1
#December 14 2022

#This R script makes figure 5
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################
#Turns a Regression into a data frame
Model.DF <- function(Model, Robust.SE = NULL) {
  
  #Extract Coefficients
  Model.Output <- as.data.frame(coef(summary(Model)))
  Model.Output$Label <- rownames(Model.Output)
  rownames(Model.Output) <- NULL
  
  #Generate Confidence Intervals
  CI <- as.data.frame(confint(Model, variable.names(Model), level=0.95))
  CI$Label <- rownames(CI)
  rownames(CI) <- NULL
  
  #Merge Model and CIs together 
  Model.Output.Final <- merge(x = Model.Output, y = CI, by =c("Label"))
  
  #Name the columns numeric
  colnames(Model.Output.Final) <- c("Label", "Coeff", "SE", "t.value", "P.Value", "lower", "upper")
  
  Model.Output.Final$Sig.05 <- ifelse(Model.Output.Final$P.Value <= .05, 1,0)
  Model.Output.Final$Sig.10 <- ifelse(Model.Output.Final$P.Value <= .10, 1,0)
  
  #Adjusted R Squared
  Model.Output.Final$AdJ.R2 <- summary(Model)$adj.r.squared
  
  #Dependent Variable
  Model.Output.Final$DV <- all.vars(formula(Model))[1]
  
  #Check for NA's in Model
  for(n in names(coef(Model))){
    if(is.na(Model$coefficients[[n]]) == T){
      newRow <- data.frame(Label=n, 
                           Coeff = NA, 
                           SE = NA, 
                           t.value = NA,
                           P.Value = NA,
                           lower = NA,
                           upper = NA,
                           AdJ.R2 = NA, 
                           Sig.05 = NA,
                           Sig.10 = NA,
                           DV=all.vars(formula(Model))[1])
      
      Model.Output.Final <- rbind(Model.Output.Final, newRow)
      
    }
  }
  
  #Option for Robust Standard Errors
  if(is.null(Robust.SE) == F){
    library(sandwich)
    x<- coeftest(Model, vcov = sandwich::vcovHC(Model, type=Robust.SE))
    xr<- setNames(data.frame(x[1:dim(x)[1], 2]), c("Robust Standard Errors"))
    xr$Label<- rownames(xr); rownames(xr) <- NULL
    
    Model.Output.Final <- merge(Model.Output.Final, xr, by = "Label")
    
  }
  
  return(Model.Output.Final)
  
}

#Example

######################### Library #####################
library(ggplot2)
library(sjPlot)

######################## Upload Data ##################

#Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load(file = "Replication Data - Amlani, Butters, and Kiesel 2022.rda"); SN.2 <- SN.1

##################### Examine Data #########################
head(SN.2)
####################Data Management############################
SN.2$vaccine_question_update[SN.2$vaccine_question == "Yes"] <- "Vaccinated"
SN.2$vaccine_question_update[SN.2$vaccine_question == "No"] <- "Unvaccinated"

##################### Main Models: Raw Number -  Vaccinated Social Network##################### 

Wave1_2_Control <- lm(vaccine_question_n ~ as.factor(SN_Vaccine_Master)*PID_3 + as.numeric(ideo) + education_numeric + hhi_numeric +  age + gender + ethnicity + Waves_Detail, data = SN.2, weights = weights)
summary(Wave1_2_Control)

##################### Figures ##################### 
Plot.1 <- data.frame(get_model_data(Wave1_2_Control, type = "pred", terms = c("SN_Vaccine_Master", "PID_3")))

#Recode Labels
Plot.1$x_Label <- Plot.1$x
Plot.1$x_Label[Plot.1$x == 0] <- "0 People Vaccinated"
Plot.1$x_Label[Plot.1$x == 1] <- "1 Person Vaccinated"
Plot.1$x_Label[Plot.1$x == 2] <- "2 People Vaccinated"
Plot.1$x_Label[Plot.1$x == 3] <- "3 People Vaccinated"

#Reorder Levels
Plot.1$x_Label <- factor(Plot.1$x_Label, levels = c("0 People Vaccinated", "1 Person Vaccinated","2 People Vaccinated", "3 People Vaccinated" ))

library(ggplot2)
P1 <- ggplot(Plot.1, aes(x = x_Label, y = predicted, ymin = conf.low, ymax = conf.high, color = group, label = round(predicted,2))) +
  geom_pointrange(position = position_dodge(width= 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  guides(text="none") +
  theme_bw() +
  scale_color_manual("Party", values = rev(c("#03A9F4", "#673AB7", "#F44336")), labels = rev(c("Democrat", "Independent", "Republican"))) +
  coord_flip() +
  labs(title = "Partisanship and Network Vaccination on the Likelihood of Being Vaccinated", 
       subtitle = "Predicted Probabilities",
       y = "Predicted Probabilities",
       caption= "Note: Point estimates are dots with lines indicating 95 percent confidence intervals.\nModel: Linear regression with controls.\nFull model specification available in the appendix (Table 5A).\nModel controls for ideology, respondent's education, household income, age, gender, ethnicity, and wave fixed effects.") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.y=element_blank(),
        legend.position = "bottom");P1

######################### Save Plot ######################### 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ggsave(P1,
       file = "Figure 5.png",
       width=8, height=6,  dpi = 300)

