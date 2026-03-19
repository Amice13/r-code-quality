#Sharif Amlani
#R 4.1.1
#Winter 2021

######################## Code Summary ##################
# This code creates Figure 8 in the manuscript.

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

Interaction_Logit_Model_DF <- function(Model, term){
  DF <- data.frame(summary(margins(Model, type="response", change="sd", variables  = term,  at = list(PID_3 = c("Democrat", "Republican")), vcov = sandwich::vcovHC(Model, type="HC1") )))
  DF$N <- nrow(model.frame(Model))
  DF$pr2 <- round(DescTools::PseudoR2(Model)[[1]], 3)
  DF$AIC <- round(AIC(Model), 2)
  DF$SE <- as.numeric(DF$SE)
  
  return(DF)
}


Additive_Logit_Model_DF <- function(Model, term){
  DF <- data.frame(summary(margins(Model, type="response", change="sd", variables  = term)), vcov = sandwich::vcovHC(Model, type="HC1"))
  DF$N <- nrow(model.frame(Model))
  DF$pr2 <- round(DescTools::PseudoR2(Model)[[1]], 3)
  DF$AIC <- round(AIC(Model), 2)
  DF$SE <- as.numeric(DF$SE)
  
  return(DF)
}


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
  Model.Output.Final$AdJ.R2 <- round(summary(Model)$adj.r.squared,3)
  
  #Number of Observations
  Model.Output.Final$N <- formatC(nrow(Model$model), format="d", big.mark=",")
  
  #RMSE
  Model.Output.Final$RMSE <- round(sqrt(mean(Model$residuals^2)),3)
  
  
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
library(margins)
library(sandwich)
library(lmtest)
library(ggrepel)
library(stargazer)
library(performance)
######################## Upload Data ##################

#Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load("Replication Data - Amlani and Kiesel 2024.rda"); Demographics.1 <- Demographics.Pure

######################## Subset Data ##################

Demographics.2 <- subset(Demographics.1, Check_Master_NoCon == "Check" & Policy_or_Not_OutParty != "Nonsense")

######################## Data Management ##################
#Ideology
Demographics.2$ideo_extreme <- abs(Demographics.2$ideo_numeric - 4)

#PID Extreme
Demographics.2$PID_7_extreme <- abs(Demographics.2$PID_7_numeric - 4)

#Valence and Policy Dimention
Demographics.2$Out_Valence <- ifelse(Demographics.2$Policy_or_Not_OutParty == "Valence", 1, 0)
Demographics.2$Out_Policy <- ifelse(Demographics.2$Policy_or_Not_OutParty == "Policy", 1, 0)

Demographics.2$Out_Word_Eval <- NA
Demographics.2$Out_Word_Eval[Demographics.2$PID_3 == "Democrat"  & !is.na(Demographics.2$PID_3)] <- Demographics.2$r_word_code_numeric[Demographics.2$PID_3 == "Democrat" & !is.na(Demographics.2$PID_3)]
Demographics.2$Out_Word_Eval[Demographics.2$PID_3 == "Republican"  & !is.na(Demographics.2$PID_3)] <- Demographics.2$d_word_code_numeric[Demographics.2$PID_3 == "Republican" & !is.na(Demographics.2$PID_3)] 

#Reverse Code
Demographics.2$Out_Word_Eval <- (Demographics.2$Out_Word_Eval* -1)

##################### Analysis: Characterizing the Dimensions of Affective Polarization ##################
#Set Levels
Demographics.2$Policy_or_Not_OutParty <- factor(Demographics.2$Policy_or_Not_OutParty, levels = c("No Policy or Valence", "Policy", "Valence"))

#******************** Word - One ******************
Base <- lm(scale(Affective_Polarization_OneWord_OwnCode) ~ Out_Valence + Out_Policy + Out_Word_Eval, data = Demographics.2, weights = weights)
summary(Base)
Base_DF <- Model.DF(Base)
Base_DF$Model <- "Base"

Control <- lm(scale(Affective_Polarization_OneWord_OwnCode) ~ Out_Valence + Out_Policy + Out_Word_Eval + age + hhi_numeric + gender + education_numeric + ethnicity + ideo_extreme + PID_7_extreme + donor + Turnout, data = Demographics.2, weights = weights)
summary(Control)
Control_DF <- Model.DF(Control)
Control_DF$Model <- "Control"

FE <- lm(scale(Affective_Polarization_OneWord_OwnCode) ~ Out_Valence + Out_Policy + Out_Word_Eval + age + hhi_numeric + gender + education_numeric + ethnicity + ideo_extreme + PID_7_extreme + donor + Turnout + state, data = Demographics.2, weights = weights)
summary(FE)
FE_DF <- Model.DF(FE)
FE_DF$Model <- "State Fixed Effects"

Dim_Model_AF_Word <- rbind(Base_DF, Control_DF, FE_DF)
Dim_Model_AF_Word$Group <- "Word"

#******************** Therm ******************
Base <- lm(scale(Affective_Polarization_Therm) ~ Out_Valence + Out_Policy + Out_Word_Eval, data = Demographics.2)
summary(Base)
Base_DF <- Model.DF(Base)
Base_DF$Model <- "Base"

Control <- lm(scale(Affective_Polarization_Therm) ~  Out_Valence + Out_Policy + Out_Word_Eval + age + hhi_numeric + gender + education_numeric + ethnicity + ideo_extreme + PID_7_extreme + donor + Turnout, data = Demographics.2, weights = weights)
summary(Control)
Control_DF <- Model.DF(Control)
Control_DF$Model <- "Control"

FE <- lm(scale(Affective_Polarization_Therm) ~ Out_Valence + Out_Policy + Out_Word_Eval + age + hhi_numeric + gender + education_numeric + ethnicity + ideo_extreme + PID_7_extreme + donor + Turnout + state, data = Demographics.2, weights = weights)
summary(FE)
FE_DF <- Model.DF(FE)
FE_DF$Model <- "State Fixed Effects"

Dim_Model_AF_Therm <- rbind(Base_DF, Control_DF, FE_DF)
Dim_Model_AF_Therm$Group <- "Thermometer"

#******************** Candidate ******************
Base <- lm(scale(Affective_Polarization_Candidate) ~ Out_Valence + Out_Policy + Out_Word_Eval , data = Demographics.2)
summary(Base)
Base_DF <- Model.DF(Base)
Base_DF$Model <- "Base"

Control <- lm(scale(Affective_Polarization_Candidate) ~  Out_Valence + Out_Policy + Out_Word_Eval + age + hhi_numeric + gender + education_numeric + ethnicity + ideo_extreme + PID_7_extreme + donor + Turnout, data = Demographics.2, weights = weights)
summary(Control)
Control_DF <- Model.DF(Control)
Control_DF$Model <- "Control"

FE <- lm(scale(Affective_Polarization_Candidate) ~ Out_Valence + Out_Policy + Out_Word_Eval + age + hhi_numeric + gender + education_numeric + ethnicity + ideo_extreme + PID_7_extreme + donor + Turnout + state, data = Demographics.2, weights = weights)
summary(FE)
FE_DF <- Model.DF(FE)
FE_DF$Model <- "State Fixed Effects"

Dim_Model_AF_Candidate <- rbind(Base_DF, Control_DF, FE_DF)
Dim_Model_AF_Candidate$Group <- "Candidate"

#******************** Social Distance ******************
Base <- lm(scale(Affective_Polarization_Lifestyle) ~ Out_Valence + Out_Policy + Out_Word_Eval, data = Demographics.2, weights = weights)
summary(Base)
Base_DF <- Model.DF(Base)
Base_DF$Model <- "Base"

Control <- lm(scale(Affective_Polarization_Lifestyle) ~  Out_Valence + Out_Policy + Out_Word_Eval + age + hhi_numeric + gender + education_numeric + ethnicity + ideo_extreme + PID_7_extreme + donor + Turnout, data = Demographics.2, weights = weights)
summary(Control)
Control_DF <- Model.DF(Control)
Control_DF$Model <- "Control"

FE <- lm(scale(Affective_Polarization_Lifestyle) ~ Out_Valence + Out_Policy + Out_Word_Eval + age + hhi_numeric + gender + education_numeric + ethnicity + ideo_extreme + PID_7_extreme + donor + Turnout + state, data = Demographics.2, weights = weights)
summary(FE)
FE_DF <- Model.DF(FE)
FE_DF$Model <- "State Fixed Effects"

Dim_Model_AF_Life <- rbind(Base_DF, Control_DF, FE_DF)
Dim_Model_AF_Life$Group <- "Social-Distance"

######################### Plot ###########################
Dim_Model_AF_Plot <- rbind(Dim_Model_AF_Therm, Dim_Model_AF_Candidate, Dim_Model_AF_Life, Dim_Model_AF_Word)

Dim_Model_AF_Plot.2 <- subset(Dim_Model_AF_Plot, Label %in% unique(grep("Out", Dim_Model_AF_Plot$Label, value = T)) & Label != "Out_Word_Eval")

Dim_Model_AF_Plot.2$Label <- gsub("Out_", "", Dim_Model_AF_Plot.2$Label)

Dim_Model_AF_Plot.2$Group <- factor(Dim_Model_AF_Plot.2$Group, levels = rev(c("Word",  "Thermometer", "Candidate", "Social-Distance")))

#************** All Model  ****************
Dim_Model_AF_Plot.3 <- subset(Dim_Model_AF_Plot.2)

P1 <- ggplot(Dim_Model_AF_Plot.3, aes(x = Group , y = Coeff, color = Label, shape = Label, ymin = lower, ymax = upper, label = round(Coeff,2))) +
  facet_wrap(Model  ~ ., scale = "free_x" ) +
  #geom_text(position = position_dodge(width = 1),show.legend = FALSE) +
  geom_text(aes(label=ifelse( P.Value < 0.05, round(Coeff,2),NA)),  position = position_dodge(width = 1), show.legend = FALSE) +
  geom_pointrange(position = position_dodge(width= 0.4)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  scale_color_manual("One-Word Dimensions", labels = c("Policy", "Valence"), values = c("#512DA8", "#0097A7")) +
  scale_shape_manual("One-Word Dimensions", labels = c("Policy", "Valence"), values = c(16, 17)) +
  theme_bw() +
  labs(title = "Effect of Out-Group Policy and Valence One-Word Dimension on the Predicting Affective Polarization",
       subtitle = "Across Alternative Measures of Affective Polarization",
       y = "Estimated Effect Size",
       caption= "Note: Point estimates are dots with lines indicating 95 percent confidence intervals.\nModel: Linear Regression.\nAffective Polarization measures are standardized.\nWe omit the coefficient representing word evaluations because the scales are not comparable,\nbut the coefficient is significant, positive, and outperforms both the valence and policy dimensions in each model.") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.y=element_blank(),
        legend.position = "bottom"); P1


ggsave(P1, 
       file = "Fig_8.png",
       width=10, height=6,  dpi = 300)
