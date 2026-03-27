######################## Code Summary ##################
#Replication for Polarization In COVID-19 Vaccine Discussion Networks
#American Politics Research
#Amlani, Butters, and Kiesel (2022)
#R Version: 4.1.1
#December 14 2022

#This R script makes figure 4
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

######################### Library #####################
library(ggplot2)

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

##################### Analysis #####################
################# Fixed Effects Model ##################### 
Wave1_2_Fixed_Effect <- lm(vaccine_question_n ~ as.factor(SN_Vaccine_Master) + SN_Attitude_Master + SN_Knowledge_Master + SN_Size +  PID_3 + ideo + education_numeric + hhi_numeric +  age + gender + ethnicity + Waves_Detail + as.factor(county_name), data = SN.2, weights = weights)
summary(Wave1_2_Fixed_Effect)

##################### Figure ##################### 
Wave1_2_Fixed_Effect.DF <- Model.DF(Wave1_2_Fixed_Effect)

#Subset Key Term
Wave1_2_Fixed_Effect.DF.2 <- subset(Wave1_2_Fixed_Effect.DF, Wave1_2_Fixed_Effect.DF$Label %in% grep("SN_Vaccine_Master", Wave1_2_Fixed_Effect.DF$Label, value=TRUE))

#Recode Labels
Wave1_2_Fixed_Effect.DF.2$Label[Wave1_2_Fixed_Effect.DF.2$Label == "as.factor(SN_Vaccine_Master)1"] <- "1 Discussant Vaccinated"
Wave1_2_Fixed_Effect.DF.2$Label[Wave1_2_Fixed_Effect.DF.2$Label == "as.factor(SN_Vaccine_Master)2"] <- "2 Discussants Vaccinated"
Wave1_2_Fixed_Effect.DF.2$Label[Wave1_2_Fixed_Effect.DF.2$Label == "as.factor(SN_Vaccine_Master)3"] <- "3 Discussants Vaccinated"

#Reorder Levels
Wave1_2_Fixed_Effect.DF.2$Label <- factor(Wave1_2_Fixed_Effect.DF.2$Label, levels = c("1 Discussant Vaccinated","2 Discussants Vaccinated", "3 Discussants Vaccinated" ))

#Plot
library(ggplot2)
Plot_Final <- ggplot(Wave1_2_Fixed_Effect.DF.2, aes(x = Label, y = Coeff, ymin = lower, ymax = upper, label = round(Coeff, 2), color = as.factor(Sig.05))) +
  geom_pointrange() +
  scale_colour_manual(values = c("black", "black")) +
  geom_text(hjust = -.3,
            vjust = .35) +
#  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  guides(color="none") +
  theme_bw() +
  labs(title = "The Effect of Network Vaccination on the Likelihood of Being Vaccinated", 
       y = "Change in P(Respondent is Vaccinated)",
       caption= "Note: Point estimates are dots with lines indicating 95 percent confidence intervals.\nThe reference category is 0 Discussants Vaccinated.\nModel: Wave 1 & 2 Fixed Effects") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.x=element_blank()); Plot_Final


#Save Plot
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ggsave(Plot_Final, 
       file = "Figure 4.png",
       width=8, height=6,  dpi = 300) 

