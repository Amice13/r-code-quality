################################################################################
# #################### SET UP  #################################################
################################################################################

packs <- c( 'tidyverse', 'ggeffects',  'sjPlot', 'patchwork',
            'ggpubr', 'stargazer', 'vtable', 'sjlabelled' , 'summarytools')
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)


getwd()

source("ReplicationCode_01_Preparation.R")
source("ReplicationCode_02_Functions.R")


getwd()


################################################################################
# #################### MORE TREATMENT EFFECTS  #################################
################################################################################


### Figure S5 + Table S19 Start ####

H1.1 <- glm(FirstChoiceHighPol ~ treat  - 1 , data = data, family = "binomial")

summary(H1.1)


H1.2 <- glm(FirstChoiceHighMed ~ treat  - 1 , data = data, family = "binomial")

summary(H1.2)

H1.3 <- glm(FirstChoiceNoPol ~ treat  - 1 , data = data, family = "binomial")

summary(H1.3)



stargazer(H1.1, H1.2, H1.3,
          title="Treatment Effects",
          align=TRUE, label = "tab:App1Results",
          out =  "Out/Tab/S19_App1Results.tex",
          out.header = FALSE,
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c("No Goal Treatment",
                             "Policy+Goal Treatment",
                             "Goal Assigned Treatment") ,
          no.space=TRUE,
          notes = "Intercepts omitted.",
          notes.align = "l") 

### Table S19 END ####



### plot with ggeffects to get the CIs non-dashed
H1.1plot <- 
  predict_response(H1.1 , "treat") %>%
  plot() +
  labs(x = "", y= "", 
       title = paste0("initial policy choice = max")) +
  policyplustheme_no_legend()

H1.1plot


H1.2plot <-
  predict_response(H1.2 , "treat") %>%
  plot() +
  labs(x = "", y= "", 
       title = paste0("initial policy choice = max or medium")) +
  policyplustheme_no_legend()

H1.2plot

combined_H1_plot <- combine_and_make_same_y_axis(H1.1plot, H1.2plot)
combined_H1_plot_n <- annotate_figure(combined_H1_plot,
                                      top = text_grob(paste0( "Predicted probabilities for choosing ambitious policy proposals (n = ", nobs(H1.1),")"  )  , size = 16) ) 



combined_H1_plot_n

combofig2tex(combined_H1_plot_n, "Out/FigS5_FirstDecision.tex")


### Figure S5 END ####


### Table S13 Start ####
fchigh_logit <- glm(FirstChoiceHighPol ~ treat*goal  , data = data, family = "binomial")
summary(fchigh_logit)

fchighmed_logit <- glm(FirstChoiceHighMed ~ treat*goal  , data = data, family = "binomial")
summary(fchigh_logit)

FirstChoiceNoPol_logit <- glm(FirstChoiceNoPol ~ treat*goal  , data = data, family = "binomial")
summary(fchigh_logit)

stargazer(fchigh_logit, fchighmed_logit, FirstChoiceNoPol_logit, title="Treatment Effects",
          align=TRUE, label = "tab:MainResults",
          out =  "Out/Tab/S13_MainResults.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment", "
                              Goal Assigned Treatment",
                              "Medium Goal",
                              "High Goal",
                              "Policy+Goal X Medium Goal",
                              "Goal Assigned X Medium Goal",
                              "Policy+Goal X High Goal", 
                              "Goal Assigned X High Goal"
          ),
          no.space=TRUE ,      
          notes = "Excl.: No Goal Treat., Low Goal.",
          notes.align = "l") 
          # add.lines = list(
          #   c("Excluded categories:", "No Goal Treatment, Low Goal") )
          # )


### Table for the S13 END ####


################################################################################
# #################### CONDITIONAL TREATMENT EFFECTS  ##########################
################################################################################
### Figure S4 & Table S10 Start ####

### create the variable FirstChoiceAnyPol that is the opposite of FirstChoiceNoPol
data <- data |> mutate(FirstChoiceAnyPol = case_when(FirstChoiceNoPol==0 ~1, 
                                                     .default = 0) )

# Conditional logistic / logit / probit: regress Y on Treatment, ConditionVar, Treatment×ConditionVar, plus controls. 
# Report marginal effects (ATE within ConditionVar=1).


conditionalmodel_high <- glm(FirstChoiceHighPol ~ treat*goal + FirstChoiceAnyPol + (treat*goal):FirstChoiceAnyPol   , data = data, family = "binomial")

summary(conditionalmodel_high)

conditionalmodel_highmed <- glm(FirstChoiceHighMed ~ treat*goal + FirstChoiceAnyPol + (treat*goal):FirstChoiceAnyPol   , data = data, family = "binomial")
summary(conditionalmodel_highmed)


stargazer(conditionalmodel_high, conditionalmodel_highmed, 
          title="Predicted probabilities for initial policy choice given any policy is chosen",
          align=TRUE, label = "tab:CondModel",
          out =  "Out/Tab/S10_CondModel.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment", "
                              Goal Assigned Treatment",
                              "Medium Goal",
                              "High Goal",
                              "Selected: Any Policy",
                              "Policy+Goal X Medium Goal",
                              "Goal Assigned X Medium Goal",
                              "Policy+Goal X High Goal",
                              "Goal Assigned X High Goal",
                              "Policy+Goal X Selected: Any",
                              "Goal Assigned X Selected: Any",
                              "Medium Goal X Selected: Any",
                              "High Goal X Selected: Any",
                              "Policy+Goal X Medium X Selected: Any",
                              "Goal Assigned X Medium X Selected: Any",
                              "Policy+Goal X High X Selected: Any",
                              "Goal Assigned X High X Selected: Any"
          ),
          no.space=TRUE ,       
          notes = "Excl.: No Goal Treat., Low Goal.",
          notes.align = "l") 
          # add.lines = list(
          #   c("Excluded categories:", "No Goal Treatment, Low Goal") ))

### Table S10 END ####

#for plotting Fig S4
selected_any_df <- data |>  filter(!(FirstChoiceNoPol==1))
# is same as : selected_any_df <- data |>  filter(FirstChoiceAnyPol==1)

cond_model <- glm(FirstChoiceHighPol ~ goal*treat     ,
                  data = selected_any_df, family = "binomial")

summary(cond_model)

cond_model_high <- plot_model(cond_model, type = "int", 
                              show.legend = TRUE,
                              terms = "FirstChoiceAnyPol [1]",
                              rm.terms = "FirstChoiceAnyPol [0]",
                              transform = "plogis", show.values = T, dot.size = 5, line.size = 1.5, value.offset = .3, 
                              colors = c("#b3b3b3",  "black", "#666666"), 
                              dodge = 0.5, 
                              axis.labels = "",
                              legend.title = c("") )  +  labs(y= "", 
                              title = paste0( "Predicted probabilities for initial policy choice = max\namong those who selected any policy (n = ", nobs(cond_model),")")) +
  policyplustheme_w_legend() 

cond_model_high
fig2tex(cond_model_high, "Out/FigS4_cond_model_high.tex")

### Figure S4 END ####





################################################################################
# #################### HETEROGENOUS TREATMENT EFFECTS  #########################
################################################################################

### Regressions for TRUST ####
### Figure S9 and Figure S10 and Table S20 Start ####
fchigh_logit_scitrust <- glm(formula = FirstChoiceHighPol ~ 
                               treat * TrustInScience * goal , family = "binomial", 
                             data = data)

fchighmed_logit_scitrust <- glm(formula = FirstChoiceHighMed ~ 
                                  treat * TrustInScience * goal , family = "binomial", 
                                data = data)


fcnone_logit_scitrust  <- glm(FirstChoiceNoPol ~ 
                                treat * TrustInScience * goal , family = "binomial", 
                              data = data)


stargazer(fchigh_logit_scitrust, fchighmed_logit_scitrust, fcnone_logit_scitrust , 
          title="Heterogeneous Treatment Effects w.r.t. Trust in Science",
          align=TRUE, label = "tab:DetResultsTrust",
          out =  "Out/Tab/S20_DetResultsTrust.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment", 
                              "Goal Assigned Treatment",
                              "HTS (High Trust in Science)",
                              "Medium (Med) Goal",
                              "High Goal",
                              "Policy+Goal X HTS", 
                              "Goal Assigned X HTS",
                              "Policy+Goal X Med Goal", 
                              "Goal Assigned X Med Goal", 
                              "Policy+Goal X High Goal", 
                              "Goal Assigned X High Goal",
                              "Med Goal X HTS",
                              "High Goal X HTS",
                              "Policy+Goal X Med X HTS", 
                              "Goal Assigned X Med X HTS",
                              "Policy+Goal X High X HTS", 
                              "Goal Assigned X High X HTS"
          ),
          no.space=TRUE ,  
          notes = "Excl.: No Goal Treat., Low Goal, Low or NA Trust in Science.",
          notes.align = "l") 
          # add.lines = list(c("Excluded category:", "Low or NA Trust in Science"), 
          #                  c("Excluded categories:", "No Goal Treatment, Low Goal") ))

### Table S20 END ####


### Associated PLOTS for Trust 
data %>% 
  summarise(total_non_na = sum(!is.na(TrustInScience)))

data %>% filter (TrustInScience == "High" ) %>% 
  summarise(total_non_na = sum(!is.na(TrustInScience)))

data %>% filter (TrustInScience == "Low or NA" ) %>% 
  summarise(total_non_na = sum(!is.na(TrustInScience)))

trust_labeller <- as_labeller(c("High" =  paste0("Trust In Science: High (n = ", data %>% filter (TrustInScience == "High" ) %>% 
                                                   summarise(total_non_na = sum(!is.na(TrustInScience))) , ")"),
                                "Low or NA" = paste0("Trust In Science: Low or NA (n = ", data %>% filter (TrustInScience == "Low or NA" ) %>% 
                                                       summarise(total_non_na = sum(!is.na(TrustInScience))), ")") ) )


plot_fchigh_logit_scitrust <-
  predict_response(fchigh_logit_scitrust, terms = c( "goal", "treat",  "TrustInScience")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
       colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = max (n = ", nobs(fcnone_logit_scitrust),")")) +
  facet_grid(cols = vars(facet),
             labeller = labeller(facet = trust_labeller)
             #   , axes = "all"
  )+
  policyplustheme_heterogeneous()


plot_fchigh_logit_scitrust
combofig2tex(plot_fchigh_logit_scitrust, "Out/FigS9_plot_fchigh_logit_scitrust.tex")

### Figure S9 END ####


plot_fcnone_logit_scitrust <-
  predict_response(fcnone_logit_scitrust, terms = c( "goal", "treat",  "TrustInScience")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
       colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_grid(cols = vars(facet),
             labeller = labeller(facet = trust_labeller), axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = none (n = ", nobs(fcnone_logit_scitrust),")")) +
  policyplustheme_heterogeneous()


plot_fcnone_logit_scitrust
combofig2tex(plot_fcnone_logit_scitrust, "Out/FigS10_plot_fcnone_logit_scitrust.tex")

### Figure S10 END ####



### Regressions for POLITICAL SELF-PLACEMENT ####
### Figure S11 and Figure S12 and Table S21 Start ####

fchigh_logit_leri <- glm(formula = FirstChoiceHighPol ~ 
                           treat * C(leri, base = 2)  * goal , family = "binomial", 
                         data = data)

fchighmed_logit_leri <- glm(formula = FirstChoiceHighMed ~ 
                              treat * C(leri, base = 2)  * goal , family = "binomial", 
                            data = data)

fcnone_logit_leri  <- glm(FirstChoiceNoPol ~ 
                            treat * C(leri, base = 2) * goal , family = "binomial", 
                          data = data)



stargazer(fchigh_logit_leri, fchighmed_logit_leri, fcnone_logit_leri, 
          title="Heterogeneous Treatment Effects w.r.t. Political self-placement",
          align=TRUE, label = "tab:DetResultsPol",
          out =  "Out/Tab/S21_DetResultsPol.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment",
                              "Goal Assigned Treatment",
                              "Political self-placement: Left", 
                              "Political self-placement: Right", 
                              "Political self-placement: None", 
                              "Medium (Med) Goal",
                              "High Goal",
                              "Policy+Goal X Left",
                              "Goal Assigned X Left",
                              "Policy+Goal X Right",
                              "Goal Assigned X Right",
                              "Policy+Goal X None",
                              "Goal Assigned X None",
                              "Policy+Goal X Med Goal",
                              "Goal Assigned X Med Goal",
                              "Policy+Goal X High Goal",
                              "Goal Assigned X High Goal",
                              "Med Goal X Left",
                              "Med Goal X Right",
                              "Med Goal X None",
                              "High Goal X Left",
                              "High Goal X Right",
                              "High Goal X None",                          
                              "Policy+Goal X Med X Left",
                              "Goal Assigned X Med X Left",
                              "Policy+Goal X Med X Right",
                              "Goal Assigned X Med X Right",
                              "Policy+Goal X Med X None",
                              "Goal Assigned X Med X None",
                              "Policy+Goal X High X Left",
                              "Goal Assigned X High X Left",
                              "Policy+Goal X High X Right",
                              "Goal Assigned X High X Right",  
                              "Policy+Goal X High X None",
                              "Goal Assigned X High X None" 
          ),
          no.space=TRUE,       
          notes = "Excl.: No Goal Treat., Low Goal, Pol: Center.",
          notes.align = "l") 
          # add.lines = list(c("Excluded category:", "Political self-placement: Center"), 
          #                  c("Excluded categories:", "No Goal Treatment, Low Goal") ))


### Table S21 END ####

data %>% 
  summarise(total_non_na = sum(!is.na(leri)))



leri_labeller <- as_labeller(c("Left" =  paste0("Political self-placement: Left (n = ",
                                                data %>% filter (leri == "Left" ) %>% 
                                                  summarise(total_non_na = sum(!is.na(leri)))
                                                , ")"),
                               "Center" =  paste0("Political self-placement: Center (n = ",
                                                  data %>% filter (leri == "Center" ) %>% 
                                                    summarise(total_non_na = sum(!is.na(leri)))
                                                  , ")"),
                               "Right" =  paste0("Political self-placement: Right (n = ",
                                                 data %>% filter (leri == "Right" ) %>% 
                                                   summarise(total_non_na = sum(!is.na(leri)))
                                                 , ")"),
                               "None" =  paste0("Political self-placement: None (n = ",
                                                data %>% filter (leri == "None" ) %>% 
                                                  summarise(total_non_na = sum(!is.na(leri))), 
                                                ")") ) )





plot_fchigh_logit_leri <-
  predict_response(fchigh_logit_leri, terms = c( "goal", "treat",  "leri")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
       colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_wrap( ~ facet,
              labeller = labeller(facet = leri_labeller), nrow = 2, axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = max (n = ", nobs(fchigh_logit_leri),")")) +
  policyplustheme_heterogeneous()

plot_fchigh_logit_leri

combofig2tex(plot_fchigh_logit_leri, "Out/FigS11_plot_fchigh_logit_leri.tex")

### Figure S11 END ####

plot_fcnone_logit_leri <-
  predict_response(fcnone_logit_leri, terms = c( "goal", "treat",  "leri")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
       colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_wrap( ~ facet,
              labeller = labeller(facet = leri_labeller), nrow = 2, axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = none (n = ", nobs(fcnone_logit_leri),")")) +
  policyplustheme_heterogeneous()


plot_fcnone_logit_leri

combofig2tex(plot_fcnone_logit_leri, "Out/FigS12_plot_fcnone_logit_leri.tex")

### Figure S12 END ####



### Regressions for EDUCATION ####
### Figure S13 and Figure S14 and Table S22 Start ####
fchigh_logit_educ <- glm(formula = FirstChoiceHighPol ~ 
                           treat * C(educ, base = 3)  * goal , family = "binomial", 
                         data = data)

fchighmed_logit_educ <- glm(formula = FirstChoiceHighMed ~ 
                              treat * C(educ, base = 3)  * goal , family = "binomial", 
                            data = data)


fcnone_logit_educ  <- glm(FirstChoiceNoPol ~ 
                            treat * C(educ, base = 3) * goal , family = "binomial", 
                          data = data)




stargazer(fchigh_logit_educ, fchighmed_logit_educ, fcnone_logit_educ, 
          title="Heterogeneous Treatment Effects w.r.t. Educational Attainment",
          align=TRUE, label = "tab:DetResultsEduc",
          out =  "Out/Tab/S22_DetResultsEduc.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment",
                              "Goal Assigned Treatment",
                              "Secondary 1 Education", "Secondary 2 Education",
                              "Medium (Med) Goal",
                              "High Goal",
                              "Policy+Goal X Sec 1",
                              "Goal Assigned X Sec 1",
                              "Policy+Goal X Sec 2",
                              "Goal Assigned X Sec 2",
                              "Policy+Goal X Med Goal",
                              "Goal Assigned X Med Goal",
                              "Policy+Goal X High Goal",
                              "Goal Assigned X High Goal",
                              "Med Goal X Sec 1",
                              "Med Goal X Sec 2",
                              "High Goal X Sec 1",
                              "High Goal X Sec 2",                          
                              "Policy+Goal X Med X Sec 1",
                              "Goal Assigned X Med X Sec 1",
                              "Policy+Goal X Med X Sec 2",
                              "Goal Assigned X Med X Sec 2",
                              "Policy+Goal X High X Sec 1",
                              "Goal Assigned X High X Sec 1",
                              "Policy+Goal X High X Sec 2",
                              "Goal Assigned X High X Sec 2" ),
          no.space=TRUE, 
          notes = "Excl.: No Goal Treat., Low Goal, Educ: Tertiary.",
          notes.align = "l") 
          # add.lines = list(c("Excluded category:", "Tertiary education"), 
          #                  c("Excluded categories:", "No Goal Treatment, Low Goal") ))

### Table S22 END ####


data %>% 
  summarise(total_non_na = sum(!is.na(educ)))


# Heterogeneous Treatment Effects w.r.t. Educational Attainment
# Secondary I Secondary II     Tertiary 

educ_labeller <- as_labeller(c("Secondary I" =  paste0("Educational attainment: Secondary I (n = ",
                                                       data %>% filter (educ == "Secondary I" ) %>% 
                                                         summarise(total_non_na = sum(!is.na(educ)))
                                                       , ")"),
                               "Secondary II" =  paste0("Educational attainment: Secondary II (n = ",
                                                        data %>% filter (educ == "Secondary II" ) %>% 
                                                          summarise(total_non_na = sum(!is.na(educ)))
                                                        , ")"),
                               "Tertiary" =  paste0("Educational attainment: Tertiary (n = ",
                                                    data %>% filter (educ == "Tertiary" ) %>% 
                                                      summarise(total_non_na = sum(!is.na(educ))), 
                                                    ")") ) )





plot_fchigh_logit_educ <-
  predict_response(fchigh_logit_educ, terms = c( "goal", "treat",  "educ")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
       colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_wrap( ~ facet,
              labeller = labeller(facet = educ_labeller), nrow = 2, axes = "all") +
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = max (n = ", nobs(fchigh_logit_educ),")")) +
  policyplustheme_heterogeneous()

plot_fchigh_logit_educ


combofig2tex(plot_fchigh_logit_educ, "Out/FigS13_plot_fchigh_logit_educ.tex")

### Figure S13 END ####


plot_fcnone_logit_educ <-
  predict_response(fcnone_logit_educ, terms = c( "goal", "treat",  "educ")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
       colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_wrap( ~ facet,
              labeller = labeller(facet = educ_labeller), nrow = 2, axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = none (n = ", nobs(fcnone_logit_educ),")")) +
  policyplustheme_heterogeneous()


plot_fcnone_logit_educ

combofig2tex(plot_fcnone_logit_educ, "Out/FigS14_plot_fcnone_logit_educ.tex")

### Figure S14 END ####



### Regressions for CLIMATE PRIORITIES ####
### Figure S15 and Figure S16 and Table S23 Start ####

fchigh_logit_ccprio1 <- glm(formula = FirstChoiceHighPol ~ 
                              treat * ccprio1 * goal , family = "binomial", 
                            data = data)

fchighmed_logit_ccprio1 <- glm(formula = FirstChoiceHighMed ~ 
                                 treat * ccprio1 * goal , family = "binomial", 
                               data = data)


fcnone_logit_ccprio1  <- glm(FirstChoiceNoPol ~ 
                               treat * ccprio1 * goal , family = "binomial", 
                             data = data)

stargazer(fchigh_logit_ccprio1, fchighmed_logit_ccprio1, fcnone_logit_ccprio1 , 
          title="Heterogeneous Treatment Effects w.r.t. Climate change as first priority",
          align=TRUE, label = "tab:DetResultsCCprio",
          out =  "Out/Tab/S23_DetResultsCCprio.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment", 
                              "Goal Assigned Treatment",
                              "Yes (C.C. as first priority)",
                              "Medium (Med) Goal",
                              "High Goal",
                              "Policy+Goal X Yes", 
                              "Goal Assigned X Yes",
                              "Policy+Goal X Med Goal", 
                              "Goal Assigned X Med Goal", 
                              "Policy+Goal X High Goal", 
                              "Goal Assigned X High Goal",
                              "Med Goal X Yes",
                              "High Goal X Yes",
                              "Policy+Goal X Med X Yes", 
                              "Goal Assigned X Med X Yes",
                              "Policy+Goal X High X Yes", 
                              "Goal Assigned X High X Yes"
          ),
          no.space=TRUE ,      
          notes = "Excl.: No Goal Treat., Low Goal, First prio not CC.",
          notes.align = "l") 
          # add.lines = list(c("Excluded category:", "No (C.C. is not first priority)"), 
          #                  c("Excluded categories:", "No Goal Treatment, Low Goal") ))


### Table S23 END ####


data %>% 
  summarise(total_non_na = sum(!is.na(ccprio1)))

data %>% filter (ccprio1 == "Yes" ) %>% 
  summarise(total_non_na = sum(!is.na(ccprio1)))

data %>% filter (ccprio1 == "No" ) %>% 
  summarise(total_non_na = sum(!is.na(ccprio1)))

ccprio1_labeller <- as_labeller(c("Yes" =  paste0("Climate Change: first priority (n = ",
                                                  data %>% filter (ccprio1 == "Yes" ) %>% 
                                                    summarise(total_non_na = sum(!is.na(ccprio1)))
                                                  , ")"),
                                  "No" = paste0("Climate Change: not first priority (n = ", 
                                                data %>% filter (ccprio1 == "No" ) %>% 
                                                  summarise(total_non_na = sum(!is.na(ccprio1))), 
                                                ")") ) )


# Order for the picture
data$ccprio1_ord <- ordered(data$ccprio1, levels=c("Yes", "No"))
fchigh_logit_ccprio1_ord <- glm(formula = FirstChoiceHighPol ~ 
                                  treat * ccprio1_ord * goal , family = "binomial", 
                                data = data)



fcnone_logit_ccprio1_ord  <- glm(FirstChoiceNoPol ~ 
                                   treat * ccprio1_ord * goal , family = "binomial", 
                                 data = data)


plot_fchigh_logit_ccprio1 <-
  predict_response(fchigh_logit_ccprio1_ord, terms = c( "goal", "treat",  "ccprio1_ord")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
       colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_grid(cols = vars(facet),
             labeller = labeller(facet = ccprio1_labeller), axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = max (n = ", nobs(fcnone_logit_ccprio1),")")) +
  policyplustheme_heterogeneous()

plot_fchigh_logit_ccprio1

combofig2tex(plot_fchigh_logit_ccprio1, "Out/FigS15_plot_fchigh_logit_ccprio1.tex")

### Figure S15 END ####

plot_fcnone_logit_ccprio1 <-
  predict_response(fcnone_logit_ccprio1_ord, terms = c( "goal", "treat",  "ccprio1_ord")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
       colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_grid(cols = vars(facet),
             labeller = labeller(facet = ccprio1_labeller), axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = none (n = ", nobs(fcnone_logit_ccprio1),")")) +
  policyplustheme_heterogeneous()


plot_fcnone_logit_ccprio1

combofig2tex(plot_fcnone_logit_ccprio1, "Out/FigS16_plot_fcnone_logit_ccprio1.tex")

### Figure S16 END ####



### Regressions for GENDER ####
### Figure S17 and Figure S18 and Table S24 Start ####
data$gender_binary <- ifelse(data$gender== "female", "Women", "any other")
data$gender_binary <- forcats::fct_recode(data$gender_binary,  "Women" = "Women", 
                                          "any other" ="any other")
data$gender_binary <- forcats::fct_relevel(data$gender_binary, c("any other", "Women"))

### Associated Regressions for gender (gender_binary) 
fchigh_logit_gender <- glm(formula = FirstChoiceHighPol ~ 
                             treat * gender_binary * goal , family = "binomial", 
                           data = data)

fchighmed_logit_gender <- glm(formula = FirstChoiceHighMed ~ 
                                treat * gender_binary * goal , family = "binomial", 
                              data = data)


fcnone_logit_gender  <- glm(FirstChoiceNoPol ~ 
                              treat * gender_binary * goal , family = "binomial", 
                            data = data)

stargazer(fchigh_logit_gender , fchighmed_logit_gender, fcnone_logit_gender, 
          title="Heterogeneous Treatment Effects w.r.t. gender",
          align=TRUE, label = "tab:DetResultsGender",
          out =  "Out/Tab/S24_DetResultsGender.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment",
                              "Goal Assigned Treatment",
                              "Gender: Women",
                              "Medium (Med) Goal",
                              "High Goal",
                              "Policy+Goal X Women",
                              "Goal Assigned X Women",
                              "Policy+Goal X Med Goal",
                              "Goal Assigned X Med Goal",
                              "Policy+Goal X High Goal",
                              "Goal Assigned X High Goal",
                              "Med Goal X Women",
                              "High Goal X Women",
                              "Policy+Goal X Med X Women",
                              "Goal Assigned X Med X Women",
                              "Policy+Goal X High X Women",
                              "Goal Assigned X High X Women"
          ),
          no.space=TRUE ,       
          notes = "Excl.: No Goal Treat., Low Goal, Other gender.",
          notes.align = "l") 
          # add.lines = list(c("Excluded category:", "Any other gender."), 
          #                  c("Excluded categories:", "No Goal Treatment, Low Goal") ))

### Table S24 END ####


data %>% 
  summarise(total_non_na = sum(!is.na(gender_binary)))

data %>% filter (gender_binary == "any other" ) %>% 
  summarise(total_non_na = sum(!is.na(gender_binary)))

data %>% filter (gender_binary == "Women" ) %>% 
  summarise(total_non_na = sum(!is.na(gender_binary)))

gender_labeller <- as_labeller(c("any other" =  paste0("Any other gender (n = ", data %>% filter (gender_binary == "any other" ) %>% 
                                                         summarise(total_non_na = sum(!is.na(gender_binary))) , ")"),
                                 "Women" = paste0("Women (n = ", data %>% filter (gender_binary == "Women" ) %>% 
                                                    summarise(total_non_na = sum(!is.na(gender_binary))), ")") ) )


plot_fchigh_logit_gender <-
  predict_response(fchigh_logit_gender, terms = c( "goal", "treat",  "gender_binary")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_grid(cols = vars(facet),
             labeller = labeller(facet = gender_labeller), axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = max (n = ", nobs(fcnone_logit_gender),")")) +
  policyplustheme_heterogeneous()

plot_fchigh_logit_gender
combofig2tex(plot_fchigh_logit_gender, "Out/FigS17_plot_fchigh_logit_gender.tex")

### Figure S17 END ####


plot_fcnone_logit_gender <-
  predict_response(fcnone_logit_gender, terms = c( "goal", "treat",  "gender_binary")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_grid(cols = vars(facet),
             labeller = labeller(facet = gender_labeller), axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = none (n = ", nobs(fcnone_logit_gender),")")) +
  policyplustheme_heterogeneous()


plot_fcnone_logit_gender
combofig2tex(plot_fcnone_logit_gender, "Out/FigS18_plot_fcnone_logit_gender.tex")

### Figure S18 END ####


### Regressions for AGE ####
### Figure S19 and Figure S20 and Table S25 Start ####

data <- data |> 
  group_by(age_groups = cut_number(Age, 4))




fchigh_logit_age <- glm(formula = FirstChoiceHighPol ~ 
                          treat * C(age_groups, base = 2)  * goal , family = "binomial", 
                        data = data)

fchighmed_logit_age <- glm(formula = FirstChoiceHighMed ~ 
                             treat * C(age_groups, base = 2)  * goal , family = "binomial", 
                           data = data)

fcnone_logit_age  <- glm(FirstChoiceNoPol ~ 
                           treat * C(age_groups, base = 2) * goal , family = "binomial", 
                         data = data)




stargazer(fchigh_logit_age, fchighmed_logit_age, fcnone_logit_age, 
          title="Heterogeneous Treatment Effects w.r.t. Age",
          align=TRUE, label = "tab:DetResultsAge",
          out =  "Out/Tab/S25_DetResultsAge.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment",
                              "Goal Assigned Treatment",
                              "Age: 18-36",
                              "Age: 51-60",
                              "Age: 61 and up (61+)",
                              "Medium (Med) Goal",
                              "High Goal",
                              "Policy+Goal X 18-36",
                              "Goal Assigned X 18-36",
                              "Policy+Goal X 51-60",
                              "Goal Assigned X 51-60",
                              "Policy+Goal X 61+",
                              "Goal Assigned X 61+",
                              "Policy+Goal X Med Goal",
                              "Goal Assigned X Med Goal",
                              "Policy+Goal X High Goal",
                              "Goal Assigned X High Goal",
                              "Med Goal X 18-36",
                              "Med Goal X 51-60",
                              "Med Goal X 61+",
                              "High Goal X 18-36",
                              "High Goal X 51-60",
                              "High Goal X 61+",
                              "Policy+Goal X Med X 18-36",
                              "Goal Assigned X Med X 18-36",
                              "Policy+Goal X Med X 51-60",
                              "Goal Assigned X Med X 51-60",
                              "Policy+Goal X Med X 61+",
                              "Goal Assigned X Med X 61+",
                              "Policy+Goal X High X 18-36",
                              "Goal Assigned X High X 18-36",
                              "Policy+Goal X High X 51-60",
                              "Goal Assigned X High X 51-60",
                              "Policy+Goal X High X 61+",
                              "Goal Assigned X High X 61+"
          ),
          no.space=TRUE, 
          notes = "Excl.: No Goal Treat., Low Goal, Age: 37-50.",
          notes.align = "l") 
          # add.lines = list(c("Excluded category:", "Age: 37-50"), 
          #                  c("Excluded categories:", "No Goal Treatment, Low Goal") ))



### Table S25 END ####



data %>% 
  summarise(total_non_na = sum(!is.na(age_groups)))



age_labeller <- as_labeller(c("[18,36]" =  paste0("Age: 18-36 (n = ",
                                                  data %>% filter (age_groups == "[18,36]" ) %>% 
                                                    summarise(total_non_na = sum(!is.na(age_groups)))
                                                  , ")"),
                              "(36,50]" =  paste0("Age: 37-50 (n = ",
                                                  data %>% filter (age_groups == "(36,50]" ) %>% 
                                                    summarise(total_non_na = sum(!is.na(age_groups)))
                                                  , ")"),
                              "51-60" =  paste0("Age: 51-60 (n = ",
                                                data %>% filter (age_groups == "(50,60]" ) %>% 
                                                  summarise(total_non_na = sum(!is.na(age_groups)))
                                                , ")"),
                              "61 +" =  paste0("Age: 61 and up (n = ",
                                               data %>% filter (age_groups == "(60,97]" ) %>% 
                                                 summarise(total_non_na = sum(!is.na(age_groups))), 
                                               ")") ) )





plot_fchigh_logit_age <-
  predict_response(fchigh_logit_age, terms = c( "goal", "treat",  "age_groups")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_wrap( ~ facet,
              labeller = labeller(facet = age_labeller), nrow = 2, axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = max (n = ", nobs(fchigh_logit_age),")")) +
  policyplustheme_heterogeneous()

plot_fchigh_logit_age

combofig2tex(plot_fchigh_logit_age, "Out/FigS19_plot_fchigh_logit_age.tex")
### Figure S19 END ####


plot_fcnone_logit_age <-
  predict_response(fcnone_logit_age, terms = c( "goal", "treat",  "age_groups")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_wrap( ~ facet,
              labeller = labeller(facet = age_labeller), nrow = 2, axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = none (n = ", nobs(fcnone_logit_age),")")) +
  policyplustheme_heterogeneous()


plot_fcnone_logit_age

combofig2tex(plot_fcnone_logit_age, "Out/FigS20_plot_fcnone_logit_age.tex")

### Figure S20 END ####



### Regressions for Monthly Household INCOME (hhincind) ####
### Figure S21 and Figure S22 and Table S26 Start ####
data <- data |> mutate(hhincind = case_when(incomehighdummy== "Lower" ~"below CHF 7000",
                                            incomehighdummy== "Higher" ~"over CHF 7000", .default = NA_character_ ) ) 

fchigh_logit_hhincind <- glm(formula = FirstChoiceHighPol ~ 
                               treat * hhincind * goal , family = "binomial", 
                             data = data)

fchighmed_logit_hhincind <- glm(formula = FirstChoiceHighMed ~ 
                                  treat * hhincind * goal , family = "binomial", 
                                data = data)


fcnone_logit_hhincind  <- glm(FirstChoiceNoPol ~ 
                                treat * hhincind * goal , family = "binomial", 
                              data = data)

stargazer(fchigh_logit_hhincind, fchighmed_logit_hhincind, fcnone_logit_hhincind , 
          title="Heterogeneous Treatment Effects w.r.t. monthly household income",
          align=TRUE, label = "tab:DetResultsIncome",
          out =  "Out/Tab/S26_DetResultsIncome.tex", 
          out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( "Policy+Goal Treatment",
                              "Goal Assigned Treatment",
                              "Higher Income",
                              "Medium (Med) Goal",
                              "High Goal",
                              "Policy+Goal X Higher Income",
                              "Goal Assigned X Higher Income",
                              "Policy+Goal X Med Goal",
                              "Goal Assigned X Med Goal",
                              "Policy+Goal X High Goal",
                              "Goal Assigned X High Goal",
                              "Med Goal X Higher Income",
                              "High Goal X Higher Income",
                              "Policy+Goal X Med X Higher Income",
                              "Goal Assigned X Med X Higher Income",
                              "Policy+Goal X High X Higher Income",
                              "Goal Assigned X High X Higher Income"
          ),
          no.space=TRUE ,   
          notes = "Excl.: No Goal Treat., Low Goal, Inc: below CHF 7000.",
          notes.align = "l") 
          # add.lines = list(c("Excluded category:", "Monthly Household Income Below CHF 7000"), 
          #                  c("Excluded categories:", "No Goal Treatment, Low Goal") ))

### Table S26 END ####

data %>% 
  summarise(total_non_na = sum(!is.na(hhincind)))

data %>% filter (hhincind == "over CHF 7000" ) %>% 
  summarise(total_non_na = sum(!is.na(hhincind)))

data %>% filter (hhincind == "below CHF 7000" ) %>% 
  summarise(total_non_na = sum(!is.na(hhincind)))

hhinc_labeller <- as_labeller(c("over CHF 7000" =  paste0("Higher Income (n = ", data %>% filter (hhincind == "over CHF 7000" ) %>% 
                                                            summarise(total_non_na = sum(!is.na(hhincind))) , ")"),
                                "below CHF 7000" = paste0("Lower Income (n = ", data %>% filter (hhincind == "below CHF 7000" ) %>% 
                                                            summarise(total_non_na = sum(!is.na(hhincind))), ")") ) )


plot_fchigh_logit_hhincind <-
  predict_response(fchigh_logit_hhincind, terms = c( "goal", "treat",  "hhincind")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_grid(cols = vars(facet),
             labeller = labeller(facet = hhinc_labeller), axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = max (n = ", nobs(fcnone_logit_hhincind),")")) +
  policyplustheme_heterogeneous()

plot_fchigh_logit_hhincind
combofig2tex(plot_fchigh_logit_hhincind, "Out/FigS21_plot_fchigh_logit_hhincind.tex")

### Figure S21 END ####


plot_fcnone_logit_hhincind <-
  predict_response(fcnone_logit_hhincind, terms = c( "goal", "treat",  "hhincind")) %>%
  plot(,   facets = FALSE,  show_x_title = FALSE, show_y_title = FALSE, 
colors = c("#b3b3b3", "black" , "#666666"), 
       alpha = 0.05,
       dodge = 0.5) +
  facet_grid(cols = vars(facet),
             labeller = labeller(facet = hhinc_labeller), axes = "all")+
  labs(x = "", y= "", colour = "",
       title = paste0( "Predicted probabilities for initial policy choice = none (n = ", nobs(fcnone_logit_hhincind),")")) +
  policyplustheme_heterogeneous()


plot_fcnone_logit_hhincind
combofig2tex(plot_fcnone_logit_hhincind, "Out/FigS22_plot_fcnone_logit_hhincind.tex")


### Figure S22 END ####











################################################################################
# #################### GOAL SELECTED TREATMENT GROUP ###########################
################################################################################


#### Figure S3 Start ####
### For goal selected treatment group: use data gs_data 

gs_data <- gs_data %>%  mutate( goal_s  = case_when(
  goal == "High Goal" ~ "High Goal\nSelected",
  goal == "Medium Goal" ~ "Medium Goal\nSelected", 
  goal == "Low Goal" ~ "Low Goal\nSelected"
), 
goal_s  =  factor(goal_s, levels= c("Low Goal\nSelected",  "Medium Goal\nSelected", "High Goal\nSelected")) )

gs_fchigh_logit <- glm(FirstChoiceHighPol ~ goal_s  , data = gs_data, family = "binomial")
summary(gs_fchigh_logit)


gs_fchighmed_logit <- glm(FirstChoiceHighMed ~ goal_s  , data = gs_data, family = "binomial")
summary(gs_fchighmed_logit)





labs(x = "", y= "", fill = "Final Goal: ",
     title = paste0( "Frequency of final goals by treatment (n = ", data %>%   summarise(total_non_na = sum(!is.na(lgoal))),")")) +
  policyplustheme_w_legend()



FirsthighPol_gs <-
  predict_response(gs_fchigh_logit, "goal_s") %>%
  plot(  ) +
  labs(x = "", y= "", fill = "Chosen Goal: ",
       title = paste0("initial policy choice = max")) +
  policyplustheme_no_legend()

FirsthighmedPol_gs <-
  predict_response(gs_fchighmed_logit, "goal_s") %>%
  plot() +
  labs(x = "", y= "", 
       title = paste0("initial policy choice = max or medium")) +
  policyplustheme_no_legend()



combined_gs_plot <- combine_and_make_same_y_axis(FirsthighPol_gs, FirsthighmedPol_gs)
combined_gs_plot_n <- annotate_figure(combined_gs_plot,
                                      top = text_grob(paste0( "Predicted probabilities for ambitious policy choice among Goal Selected group (n = ", nobs(gs_fchigh_logit),")"  )  , size = 16) ) 



combined_gs_plot_n

combofig2tex(combined_gs_plot_n, "Out/FigS3_FirstPol_gs.tex")

####Figure S3 END ####





#####Table S18 Start ####


gs_fchigh_logit <- glm(FirstChoiceHighPol ~ goal  , data = gs_data, family = "binomial")
summary(gs_fchigh_logit)

gs_fchighmed_logit <- glm(FirstChoiceHighMed ~ goal  , data = gs_data, family = "binomial")
summary(gs_fchighmed_logit)

gs_nopol_logit <- glm(FirstChoiceNoPol ~ goal  , data = gs_data, family = "binomial")
summary(gs_nopol_logit)




stargazer(gs_fchigh_logit, gs_fchighmed_logit, gs_nopol_logit, 
          title="Initial policy choices among Goal Selected treatment group", 
          align=TRUE, label = "tab:PoliciesGoalSelctedResults",
          out =  "Out/Tab/S18_PoliciesGoalSelctedResults.tex", 
         out.header = FALSE, 
          dep.var.labels=c("Max", "Max or medium", "None"),
          covariate.labels=c( 
            "Medium Goal Selected",
            "High Goal Selected"  ),
          no.space=TRUE ,     
         notes = "Excl.: Low Goal Selected.",
         notes.align = "l") 
          # add.lines = list(
          #   c("Excluded category:", "Low Goal Selected") ))

#####Table S18 END ####



#### Table S11 Start####
# Who wants policies? 
# Fit the model who wants these high policies?
gs_data$highgoalselected <- ifelse(gs_data$goal== "High Goal",1, 0)
gs_data$lowgoalselected <- ifelse(gs_data$goal== "Low Goal",1, 0)

modelhigh <- glm(highgoalselected ~ C(leri, base = 2) +  Age + ownhouseflat  + nocarhousehold +  C(ccprio1, base = 2) + C(energyprio1, base = 2) +
                   incomehighdummy +    C(educ, base = 3)  + urban + TrustInScience, data = gs_data, family = "binomial") 
summary(modelhigh)


modellow <- glm(lowgoalselected ~ C(leri, base = 2) +  Age + ownhouseflat  + nocarhousehold +  C(ccprio1, base = 2) + C(energyprio1, base = 2) +
                  incomehighdummy +    C(educ, base = 3)  + urban + TrustInScience , data = gs_data, family = "binomial") 
summary(modellow)



stargazer(modelhigh, modellow, title="Predicting initial policy goal selection", align=TRUE, label = "tab:goalselection",
          dep.var.labels=c("Highest Goal Selected","Lowest Goal Selected"), 
         out =  "Out/Tab/S11_goalselection.tex", 
         out.header = FALSE, 
          covariate.labels=c( "Political self-placement: Left", 
                              "Political self-placement: Right", 
                              "Political self-placement: None",
                              "Age (in years)",
                              "House or flat ownership", "Car free household", " Climate change as first priority", "Energy as first priority",
                              "Income below CHF 7000","Secondary 1 Education","Secondary 2 Education",  "Rural dweller", "Urban dweller", 
                              "High Trust in Science" ),
         no.space=TRUE,     
         notes = "Excl.: Pol: Center, other first prios, Educ: Tert., Agglomeration.",
         notes.align = "l") 

###Table S11 END ####



################################################################################
# #################### FEEDBACK   ##############################################
################################################################################



# Make the feedback dataset
# use gs_data for the goal selected group
ga_data <-  data %>% filter(treat== 'Goal Assigned')

# combine them 
potentialfeedbackers <-   bind_rows(ga_data, gs_data)

# remove low goal who never can get feedback. 
potentialfeedbackers <-  potentialfeedbackers %>% filter(goal!= 'Low Goal')

###Table S12 Start ####

table(potentialfeedbackers$treat, potentialfeedbackers$goal)

table(potentialfeedbackers$treat, potentialfeedbackers$Feedback_decison, exclude = NULL)
table(potentialfeedbackers$goal, potentialfeedbackers$Feedback_decison, exclude = NULL)

actualfeedbackers <- potentialfeedbackers %>% filter(  !(is.na(Feedback_decison)))
table(actualfeedbackers$Feedback_decison, exclude = NULL)
table(actualfeedbackers$treat, actualfeedbackers$goal)
table(actualfeedbackers$treat)


votednon <-  potentialfeedbackers[potentialfeedbackers$firstpolchoice %in% c('None'),  ]
table(votednon$treat,votednon$goal)
table(votednon$treat)


###Table S12 END ####



actualfeedbackers <- potentialfeedbackers %>% filter(  !(is.na(Feedback_decison)))

# remove empty labels in treatment for the actualfeedbackers. 
actualfeedbackers$treat <- factor(actualfeedbackers$treat)


## Figure S8 Start ####

# What is done by those who wanted to increase their policy? ----


WannaIncreasePol_all <-  actualfeedbackers %>% filter(Feedback_decison == "Policy")

table(WannaIncreasePol_all$SecChoiceIncreasePol, exclude = NULL)

WannaIncreasePol_NAs <-  WannaIncreasePol_all %>% filter(is.na(SecChoiceIncreasePol))
# 3 people just dropped out at this point. 

WannaIncreasePol <-  WannaIncreasePol_all %>%  filter(! is.na(SecChoiceIncreasePol))

table(WannaIncreasePol$SecChoiceIncreasePol, exclude = NULL)
prop.table(table(WannaIncreasePol$SecChoiceIncreasePol))


# remove empty labels in PolicyFirst for the actualfeedbackers who WannaIncreasePol
WannaIncreasePol$FC <- factor(WannaIncreasePol$FC)

WannaIncreasePol <- WannaIncreasePol %>% mutate(igoal =  case_when(goal == "Medium Goal"~"Medium", 
                                                                   goal == "High Goal" ~"High") )



WannaIncreasePol$igoal <- as.factor(WannaIncreasePol$igoal) 

WannaIncreasePol$igoal <- forcats::fct_relevel(WannaIncreasePol$igoal, c( "Medium", "High"))



SecChoiceIncreasePol_logit <- glm(SecChoiceIncreasePol ~ igoal*treat  , data = WannaIncreasePol, family = "binomial")
summary(SecChoiceIncreasePol_logit )



table(WannaIncreasePol$SecChoiceIncreasePol, WannaIncreasePol$treat, exclude = NULL)


polincreaseafterfeedback <-   plot_model(SecChoiceIncreasePol_logit,  
                                         show.legend = TRUE, dot.size = 5, line.size = 1.5, 
                                         type = "int", show.values = T, value.offset = .3,     
                                         colors = c( "#666666", "black"))  +
  aes(linetype=group, color=group) + #  use different line-types
  scale_linetype_manual(values = c("solid", "longdash")) +  #dotted line definition
  labs(y = "",  color = "", linetype  = "", x="Initial Goal",
       title = paste0("After Feedback: Policy increased (n = " , nobs(SecChoiceIncreasePol_logit) , ")") )  +
  policyplustheme_w_legend() +
  theme(axis.title.x = element_text(  size=14,  colour = "black"))



polincreaseafterfeedback
fig2tex(polincreaseafterfeedback, "Out/FigS8_polincreaseafterfeedback.tex")
### Figure S8 END ####




###  Figure S6 Start ####
### Last Goal by treatment # 

data$goal <- factor(data$goal,  levels= c("High Goal", "Medium Goal", "Low Goal"))
data$lgoal <- factor(data$lgoal,  levels= c("High Goal", "Medium Goal", "Low Goal"))
WannaAlterGoal <-  data %>%  filter(! is.na(GoalDecreasedAfterFeedback))




table(WannaAlterGoal$lgoal, exclude = NULL)
table(WannaAlterGoal$GoalDecreasedAfterFeedback, exclude = NULL)
prop.table(table(WannaAlterGoal$GoalDecreasedAfterFeedback))




data3<- data %>%
  group_by(lgoal, treat) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))

data3<- cbind(id = 1:nrow(data3), data3)


data3<- cbind(freq2 = 100* round(data3$freq, digits = 2), data3)


library(plyr)
data3 <- plyr::ddply(data3, .(treat), transform, freq2 = n / sum(n) )
#positions the data
data3 <- plyr::ddply(data3, .(treat), transform, pos = cumsum(freq2) - (0.5 * freq2))

#rounds the data for printing
data3$freq2 <- paste (round(100 * data3$freq2, digits = 2), "%", sep = "") 

#Reorder for the picture 
data3$lgoal <- factor(data3$lgoal,  levels= c("High Goal", "Medium Goal", "Low Goal"))


lastgoalsbytreatment <- ggplot(data3, aes(x = treat, y = freq, fill = lgoal)) + 
  geom_bar(position = "fill", stat = "identity")+ 
  scale_y_continuous(labels = scales::percent) +
  geom_text(data=data3, aes(x = treat, y = pos, label = freq2),
            size=5) +
  scale_fill_manual(values=c("#b2df8a", "#1f78b4", "#a6cee3"), c('Final goal:', 'Low',  'Medium', 'High' ))+
  labs(x = "", y= "", fill = "Final Goal: ",
       title = paste0( "Frequency of final goals by treatment (n = ", data %>%   summarise(total_non_na = sum(!is.na(lgoal))),")")) +
  policyplustheme_w_legend()

lastgoalsbytreatment
fig2tex(lastgoalsbytreatment, "Out/FigS6_lastgoalsbytreatment.tex")
###  Figure S6 END ####


### Table S17 Start  ####

WannaAlterGoal_all <-  actualfeedbackers %>% filter(Feedback_decison == "Goal")

table(WannaAlterGoal_all$GoalDecreasedAfterFeedback, exclude = NULL)


# 1 obs just dropped out at this point. 

WannaAlterGoal <-  WannaAlterGoal_all %>%  filter(! is.na(GoalDecreasedAfterFeedback))

table(WannaAlterGoal$lgoal, exclude = NULL)
table(WannaAlterGoal$GoalDecreasedAfterFeedback, exclude = NULL)
prop.table(table(WannaAlterGoal$GoalDecreasedAfterFeedback))


WannaAlterGoal <- WannaAlterGoal %>% mutate(igoal =  case_when(goal == "Medium Goal"~"Medium", 
                                                               goal == "High Goal" ~"High") )


WannaAlterGoal$igoal <- as.factor(WannaAlterGoal$igoal) 

WannaAlterGoal$igoal <- forcats::fct_relevel(WannaAlterGoal$igoal, c( "Medium", "High"))


GoalDecreasedAfterFeedback_logit <- glm(GoalDecreasedAfterFeedback ~ igoal*treat  , data = WannaAlterGoal, family = "binomial")
summary(GoalDecreasedAfterFeedback_logit )

nobs(GoalDecreasedAfterFeedback_logit)


stargazer(SecChoiceIncreasePol_logit, GoalDecreasedAfterFeedback_logit,  
          title="Second Decision Treatment Effects",
          align=TRUE, label = "tab:FeedbackRegs",
          out =  "Out/Tab/S17_FeedbackRegs.tex",
          out.header = FALSE, 
          dep.var.labels=c("Increased policy", "Decreased goal"),
          covariate.labels=c("Goal Selected",
                             "High Goal",
                             "Goal Selected X High Goal") ,
          no.space=TRUE,
          notes = "Excl.: Goal Assigned Treat., Medium Goal.",
          notes.align = "l") 
### Table S17 END  ####



################################################################################
# #################### DESCRIPTIVE FIGURES #####################################
################################################################################

### Figure S1 Start ####
########### Initial Goal: 

data2<- data %>%
  group_by(goal, treat) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))

data2<- cbind(id = 1:nrow(data2), data2)


data2<- cbind(freq2 = 100* round(data2$freq, digits = 2), data2)

packs <- c( 'plyr' )
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)
library(plyr)
data2 <- plyr::ddply(data2, .(treat), transform, freq2 = n / sum(n) )
#positions the data
data2 <- plyr::ddply(data2, .(treat), transform, pos = cumsum(freq2) - (0.5 * freq2))

#rounds the data for printing
data2$freq2 <- paste (round(100 * data2$freq2, digits = 2), "%", sep = "") 

#Reorder for the picture 
data2$goal <- factor(data2$goal,  levels= c("High Goal", "Medium Goal", "Low Goal"))


firstgoalsbytreatment <- ggplot(data2, aes(x = treat, y = freq, fill = goal)) + 
  geom_bar(position = "fill", stat = "identity")+ 
  scale_y_continuous(labels = scales::percent) +
  geom_text(data=data2, aes(x = treat, y = pos, label = freq2),
            size=5) +
  scale_fill_manual(values=c("#b2df8a", "#1f78b4", "#a6cee3"), c('Initial Goal: ', 'Low',  'Medium', 'High' ))+
  labs(x = "", y= "", fill = "Initial Goal: ",
       title = paste0( "Frequency of goals by treatment (n = ", data %>%   summarise(total_non_na = sum(!is.na(treat))),")")) +
  policyplustheme_w_legend()
  
firstgoalsbytreatment
fig2tex(firstgoalsbytreatment, "Out/FigS1_firstgoalsbytreatment.tex")

### Figure S1  END ####

### Figure S2 Start ####
level_order <- c("None", "Min", "Med", "Max")
data$firstpolchoice <- factor(data$firstpolchoice, levels = level_order)

df<- data %>%
  group_by(goal, treat, firstpolchoice) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))

df<- cbind(id = 1:nrow(df), df)
df<- cbind(freq2 = 100* round(df$freq, digits = 2), df)


library(plyr)
df <- plyr::ddply(df, .(goal, treat ), transform, freq2 = n / sum(n) )
#positions the data

df <- plyr::ddply(df, .(treat, goal), transform, pos = cumsum(freq2) - (0.5 * freq2))

#rounds the data for printing
df$freq2 <- paste (round(100 * df$freq2, digits = 0), "%", sep = "") 




df$firstpolchoice <- factor(df$firstpolchoice,  levels= c("Max","Med", "Min" , "None"))



initialpolicieschosen <- ggplot(df, aes(x = treat, y = freq, fill = firstpolchoice)) + 
  facet_grid(~goal)+
  geom_bar(position = "fill", stat = "identity")+ 
  scale_y_continuous(labels = scales::percent) +
  geom_text(data=df, aes(x = treat, y = pos, label = freq2),
            size=5) +
  scale_fill_manual(values=c("#a6611a", "#dfc27d", "#80cdc1", "#018571" ),
                    breaks=c('None', 'Min', 'Med', 'Max'))  +
  labs(x = "", y= "", fill = "First Policy Choice: ",
       title = paste0( "Frequency of initially chosen policies by treatment group (n = ", data %>%   summarise(total_non_na = sum(!is.na(treat))),")")) +
  policyplustheme_w_legend() +
  theme(        axis.text.x = element_text( angle = 45, hjust = 1))
  
initialpolicieschosen

combofig2tex(initialpolicieschosen, "Out/FigS2_initialpolicieschosen.tex")

### Figure S2 END ####


###  Figure S7 Start ####
#### Last Policies Chosen: 

df2<- data %>%
  group_by(goal, treat, lastpolchoice) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))

df2 <- plyr::ddply(df2, .(goal, treat), transform, freq = n / sum(n) )

df2<- cbind(id = 1:nrow(df2), df2)
df2<- cbind(freq2 = 100* round(df2$freq, digits = 2), df2)

df2$freq2 <- paste (df2$freq2, "%", sep = "") 

df2$lastpolchoice <- factor(df2$lastpolchoice,  levels= c("Max","Med", "Min" , "None"))


finalpolicieschosen <- ggplot(df2, aes(x = treat, y = freq, fill = lastpolchoice , label = freq2)) + 
  facet_grid(~goal)+
  geom_bar(position = "fill", stat = "identity")+ 
  scale_y_continuous(labels = scales::percent) +
  geom_text(
    size = 5, position = position_stack(vjust = 0.5))  +
  scale_fill_manual(values=c("#a6611a", "#dfc27d", "#80cdc1", "#018571" ),
                    breaks=c('None', 'Min', 'Med', 'Max'))  +
  labs(x = "", y= "", fill = "Policy Choice: ",
       title = paste0( "Frequency of final chosen policies by treatment group (n = ", data %>%   summarise(total_non_na = sum(!is.na(lastpolchoice))),")")) +
  policyplustheme_w_legend() +
  theme(        axis.text.x = element_text( angle = 45, hjust = 1))



finalpolicieschosen
combofig2tex(finalpolicieschosen, "Out/FigS7_finalpolicieschosen.tex")
###  Figure S7 END ####





### comparing initial choices with last choices 
### Table S14 Start #### 

df_first <- data %>%
  group_by(goal, treat, firstpolchoice) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  dplyr::rename(polchoice = firstpolchoice)

df_last <- data %>%
  group_by(goal, treat, lastpolchoice) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  dplyr::rename(polchoice = lastpolchoice)


combined <- left_join(df_first, df_last, by = c("goal", "treat", "polchoice"))

# combined_f <- combined |> filter(treat == "Goal Assigned" | treat == "Goal Selected") |> 
#   filter(goal != "Low Goal")
# 
# combined_f <- combined_f |> mutate(achieved = case_when(goal=="Medium Goal" & polchoice == "Med"  ~ 1 ,
#                                                         goal=="Medium Goal" & polchoice == "Max"  ~ 1 ,
#                                                         goal=="High Goal" & polchoice == "Max"  ~ 1 , .default = 0)
# )
# 
# 
# combined_f <- combined_f %>%
#   group_by(achieved) %>%
#   dplyr::summarise(prior = sum(n.x), 
#                    post = sum(n.y))
# 
# # From 1262 respondents (see combined_f) before the post-feedback adjustment 601 achieved their goal (0.476), while after adjustement 668 (52.9%) achieved their goal.


combined <- combined |> mutate(achieved = case_when( goal=="Low Goal" & polchoice == "Min"  ~ 1 ,
                                                     goal=="Low Goal" & polchoice == "Med"  ~ 1 ,
                                                     goal=="Low Goal" & polchoice == "Max"  ~ 1 ,
                                                     goal=="Medium Goal" & polchoice == "Med"  ~ 1 ,
                                                     goal=="Medium Goal" & polchoice == "Max"  ~ 1 ,
                                                     goal=="High Goal" & polchoice == "Max"  ~ 1 , .default = 0)
)


combined_achieved <- combined %>%
  group_by(achieved) %>%
  dplyr::summarise(prior = sum(n.x), 
                   post = sum(n.y))

# BEFORE: 2194/3808 = 57,6%, AFTER: 2261/3808 59.3%  0.59375

stargazer(combined_achieved,
          out =  "Out/Tab/S14.tex", 
          header=FALSE, type='latex', 
          summary=FALSE, 
          title="Goal achievement (before and after feedback)",
          label="tab:Goalsachieved",
          digits=3)


### Table S14 END #### 

### Table S15 Start #### 
## If we also take into account goal changes:


df_last_2 <- data %>%
  group_by(lgoal, treat, lastpolchoice) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  dplyr::rename(polchoice = lastpolchoice,
                goal = lgoal)

combination <- left_join(df_first, df_last_2, by = c("goal", "treat", "polchoice"))


combination <- combination |> mutate(achieved = case_when( goal=="Low Goal" & polchoice == "Min"  ~ 1 ,
                                                            goal=="Low Goal" & polchoice == "Med"  ~ 1 ,
                                                           goal=="Low Goal" & polchoice == "Max"  ~ 1 ,
                                                        goal=="Medium Goal" & polchoice == "Med"  ~ 1 ,
                                                        goal=="Medium Goal" & polchoice == "Max"  ~ 1 ,
                                                        goal=="High Goal" & polchoice == "Max"  ~ 1 , .default = 0)
)

combination_achieved <- combination %>%
  group_by(achieved) %>%
  dplyr::summarise(prior = sum(n.x), 
                   post = sum(n.y))

### Before 2194 /3808 = 57%, after 2304/3808 =  0.605042 nearly 61%.

stargazer(combination_achieved,
          header=FALSE, type='latex', 
          summary=FALSE, 
          title="Achievement",
          digits=3)
  
  


### Shares by policies accepted #### 


df_pol_first <- df_first %>%
  group_by(polchoice) %>%
  dplyr::summarise(sum_n = sum(n)) %>%
  mutate(freq = sum_n / sum(sum_n)) 

df_pol_first_l  <- df_pol_first %>%
  mutate(ambitionlevel = case_when(polchoice=="Max" ~"Higher",
                                   polchoice=="Med" ~"Higher",
                                   .default ="Lower")) %>%
  group_by(ambitionlevel) %>%
  dplyr::summarise(sum_h = sum(sum_n)) %>%
  mutate(freq = sum_h / sum(sum_h))
  
 
df_pol_last <- df_last %>%
   group_by(polchoice) %>%
   dplyr::summarise(sum_n = sum(n)) %>%
   mutate(freq = sum_n / sum(sum_n))

 

df_pol_last_l  <- df_pol_last %>%
  mutate(ambitionlevel = case_when(polchoice=="Max" ~"Higher",
                                   polchoice=="Med" ~"Higher",
                                   .default ="Lower")) %>%
  group_by(ambitionlevel) %>%
  dplyr::summarise(sum_h = sum(sum_n)) %>%
  mutate(freq = sum_h / sum(sum_h))


# Note: This is identical to df_pol_last 
 # df_last_2 %>%
 #   group_by(polchoice) %>%
 #   dplyr::summarise(sum_n = sum(n)) %>%
 #   mutate(freq = sum_n / sum(sum_n)) 
 
 
 
 polchoicecomb <- left_join(df_pol_first, df_pol_last, by = "polchoice")
 
 polchoicecomb
 
 stargazer(polchoicecomb,
           out =  "Out/Tab/S15.tex", 
           header=FALSE, type='latex', 
           summary=FALSE, 
           title="Policy choice (before and after feedback)",
           label= "tab:descriptivespolchoice_detail",
           digits=3)
 
 ### Table S15 END #### 
 
 ### Table S16 Start #### 
 polchoicecomb_amb <- left_join(df_pol_first_l, df_pol_last_l, by = "ambitionlevel")
 
 polchoicecomb_amb
 
 
 stargazer(polchoicecomb_amb,
           out =  "Out/Tab/S16.tex", 
           header=FALSE, type='latex', 
           summary=FALSE, 
           title="Policy choice (before and after feedback)",
           label= "tab:descriptivespolchoice",
           digits=3)
 ### Table S16 END #### 
 ### END: comparing initial choices with last choices 
 
 
 


 ################################################################################
 # #################### Market-based vs. Regulatory  #########################
 ################################################################################
 
 # only when a policy was chosen
 selected_any_df <- data |>  filter(!(FirstChoiceNoPol==1))
 
 # create relevant variables
 # for the first choice
 
 selected_any_df$FRegMark <- ifelse(selected_any_df$FC == "none", NA, ifelse(grepl("Fin$", selected_any_df$FC),"Fin", "Reg"))
 
 ### preview the prevalence of Reg and Fin
 table(selected_any_df$FRegMark)
 
 selected_any_df |>  group_by(FRegMark) |> dplyr::summarise(n = n())  |> 
   mutate(freq = n / sum(n))
 
 ### Differences by treatment ####
 
 selected_any_df$treat <- fct_drop(selected_any_df$treat)
 
 # Create a contingency table of the counts of the two categories by treat group
 cont_table <- table( selected_any_df$FRegMark, selected_any_df$treat)
 
 # Perform chi-squared test of independence
 chi_sq <- chisq.test(cont_table, correct = TRUE)
 
 # Print the results
 print(cont_table)
 print(chi_sq)
 
 
 ### Differences by goal ####
 
 # Create a contingency table of the counts of the two categories by treat group
 cont_table <- table( selected_any_df$goal,  selected_any_df$FRegMark)
 
 # Perform chi-squared test of independence
 chi_sq <- chisq.test(cont_table, correct = TRUE)
 
 # Print the results
 print(cont_table)
 print(chi_sq)
 
 
 ### For all respondents including goal selected ####  
 
 comb <- bind_rows(data, gs_data)
 selected_any_data <- comb |>  filter(!(FirstChoiceNoPol==1))
 
 selected_any_data$FRegMark <- ifelse(selected_any_data$FC == "none", NA, ifelse(grepl("Fin$", selected_any_data$FC),"Fin", "Reg"))
 
 ### preview the prevalence of Reg and Fin
 table(selected_any_data$FRegMark)
 
 selected_any_data |>  group_by(FRegMark) |> dplyr::summarise(n = n())  |> 
   mutate(freq = n / sum(n))
 
 ### Differences by treatment ####
 
 selected_any_data$treat <- fct_drop(selected_any_data$treat)
 
 # Create a contingency table of the counts of the two categories by treat group
 cont_table <- table( selected_any_data$FRegMark, selected_any_data$treat)
 
 # Perform chi-squared test of independence
 chi_sq <- chisq.test(cont_table, correct = TRUE)
 
 # Print the results
 print(cont_table)
 print(chi_sq)
 
 
 ### Differences by goal ####
 
 # Create a contingency table of the counts of the two categories by treat group
 cont_table <- table( selected_any_data$goal,  selected_any_data$FRegMark)
 
 # Perform chi-squared test of independence
 chi_sq <- chisq.test(cont_table, correct = TRUE)
 
 # Print the results
 print(cont_table)
 print(chi_sq)
 
 # Market-based vs. Regulatory  END #########################
 
 
 
 
 ################################################################################
 # #################### DESCRIPTIVES & BALANCE FOR ALL ##########################
 ################################################################################
 
 # combine datasets
 
 comb <- bind_rows(data, gs_data)
 
 ### share of people who do not select any policy first - by treatment
 comb %>%
   group_by(treat)  %>%
   count(FirstChoiceNoPol) %>% # shorthand for group_by + summarise(n = n())
   mutate(freq = n / sum(n))
 
 ### share of people who do not select any policy first 
 comb %>%
   count(FirstChoiceNoPol) %>% # shorthand for group_by + summarise(n = n())
   mutate(freq = n / sum(n))
 
 ### share of people who do not select any policy first - by treatment among random sample
 data %>%
   group_by(treat)  %>%
   count(FirstChoiceNoPol) %>% # shorthand for group_by + summarise(n = n())
   mutate(freq = n / sum(n))
 
 ### share of people who do not select any policy first - among random sample
 data %>%
   count(FirstChoiceNoPol) %>% # shorthand for group_by + summarise(n = n())
   mutate(freq = n / sum(n))
 
 
 
 
 ### Table S2 Start #### 
 
 comb %>% filter(!(is.na(agecat)))  %>% 
   summarytools::freq(agecat)
 
 comb %>% filter(!(is.na(gender)))  %>% 
   summarytools::freq(gender)
 
 comb %>% filter(!(is.na(educ)))  %>% 
   summarytools::freq(educ)
 
 comb %>% filter(!(is.na(income)))  %>% 
   summarytools::freq(income)
 
 ### Table S2 END #### 
 
 
 ################################################################################
 # #################### BALANCING TABLES ########################################
 ################################################################################
 
 ### Preparation for Balancing Tables (Table S3 to Table S9) Start ####
 
 comb$goal_assigned <- ifelse(comb$treat== "Goal Assigned", "Goal Assigned", "All other")
 comb$no_goal <- ifelse(comb$treat== "No Goal", "No Goal","All other")
 comb$policy_goal <- ifelse(comb$treat== "Policy+Goal", "Policy+Goal","All other")
 comb$goal_selected <- ifelse(comb$treat== "Goal Selected", "Goal Selected", "All other")
 
 
 comb$gender_binary <- ifelse(comb$gender== "female", "Women", "any other")
 comb$gender_binary <- forcats::fct_recode(comb$gender_binary,  "Women" = "Women", 
                                           "any other" ="any other")
 comb$gender_binary <- forcats::fct_relevel(comb$gender_binary, c("Women", "any other"))
 
 
 comb$ownhouseflat <- forcats::fct_recode(comb$ownhouseflat,  "Yes" = "Owning", 
                                          "No (e.g., renting)" ="Other")
 comb$ownhouseflat <- forcats::fct_relevel(comb$ownhouseflat, c("Yes", "No (e.g., renting)"))
 
 
 table(comb$ccprio1, comb$energyprio1)
 
 comb$priorities <- ifelse(comb$ccprio1== "Yes", "Climate Change", 
                           ifelse(comb$energyprio1== "Yes", "Energy",      
                                  "Other"))
 
 comb$incomehighdummy <- forcats::fct_recode(comb$incomehighdummy,  "Above CHF 7000" = "Higher", 
                                             "Below  CHF 7000" ="Lower")
 comb$incomehighdummy <- forcats::fct_relevel(comb$incomehighdummy, c("Above CHF 7000", "Below  CHF 7000"))
 
 comb$nocarhousehold <- forcats::fct_recode(comb$nocarhousehold,  "No (i.e., with car(s))" = "Has car", 
                                            "Yes" ="No car")
 comb$nocarhousehold <- forcats::fct_relevel(comb$nocarhousehold, c("Yes", "No (i.e., with car(s))"))
 
 comb <- comb %>%
   sjlabelled::var_labels(gender_binary = "Gender",
                          leftright =  "Political self-placement (1-10)",
                          Age =  "Age (at survey year)",
                          ownhouseflat = "House or flat ownership",
                          nocarhousehold = "Car free household" ,
                          incomehighdummy = "Monthly Household Income",
                          educ = "Educational Attainment",
                          priorities = "First Priority",
                          urban = "Dwelling in (area)",
                          TrustInScience = "Trust in Science")
 
 
 ### Do balancing 
 
 
 balance <- c("gender_binary", "Age", "educ", "ownhouseflat", "nocarhousehold", "priorities", "incomehighdummy", "urban",  "leftright", "TrustInScience")
 
 ### Export Tables
 vtable::sumtable(comb, group = 'no_goal', file='Out/Tab/S3_balancing_ng.tex', 
                  vars = balance , group.test = TRUE, labels = TRUE, title = "No Goal Treatment vs. remaining experimental participants", 
                  out = "latex", anchor = "tab:balancing_ng")
 
 ### Table S3 END #### 
 
 
 vtable::sumtable(comb, group = 'policy_goal',  file='Out/Tab/S4_balancing_pg.tex',                  
                  vars = balance , group.test = TRUE, 
                  labels = TRUE, title = "Policy+Goal Treatment vs. remaining experimental participants", 
                  out = "latex", anchor = "tab:balancing_pg")
 
 ### Table S4 END #### 
 
 vtable::sumtable(comb, group = 'goal_assigned', file='Out/Tab/S5_balancing_ga.tex', 
                  vars = balance , group.test = TRUE, labels = TRUE, title = "Goal Assigned Treatment vs. remaining experimental participants", 
                  out = "latex", anchor = "tab:balancing_ga")
 
 ### Table S5 END #### 
 
 vtable::sumtable(comb, group = 'goal_selected', file='Out/Tab/S6_balancing_gs.tex', 
                  vars = balance,  group.test = TRUE, labels = TRUE, title = "Goal Selected Treatment vs. remaining experimental participants", 
                  out = "latex", anchor = "tab:balancing_gs")
 
 ### Table S6 END #### 
 
 
 comb_random <- comb %>% filter(treat !=  "Goal Selected")
 
 comb_random$low <- ifelse(comb_random$goal== "Low Goal", "Low Goal", "All other")
 comb_random$medium <- ifelse(comb_random$goal== "Medium Goal", "Medium Goal","All other")
 comb_random$high <- ifelse(comb_random$goal== "High Goal", "High Goal","All other")
 
 balance2 <- c("gender_binary", "Age", "educ", "ownhouseflat", "nocarhousehold", "priorities", "incomehighdummy", "urban",  "leftright", "TrustInScience", "treat")
 
 
 
 
 vtable::sumtable(comb_random, group = 'low', file='Out/Tab/S7_balancing_lowgoal.tex',
                  vars = balance2 , group.test = TRUE, labels = TRUE, title = "Randomized into Low Goal vs. remaining experimental participants", 
                  out = "latex", anchor = "tab:balancing_lowgoal")
 
 ### Table S7 END #### 
 
 
 vtable::sumtable(comb_random, group = 'medium', file='Out/Tab/S8_balancing_medgoal.tex',
                  vars = balance2 , group.test = TRUE, labels = TRUE, title = "Randomized into Medium Goal vs. remaining experimental participants", 
                  out = "latex", anchor = "tab:balancing_medgoal")
 
 ### Table S8 END #### 
 
 vtable::sumtable(comb_random, group = 'high', file='Out/Tab/S9_balancing_highgoal.tex',
                  vars = balance2 , group.test = TRUE, labels = TRUE, title = "Randomized into High Goal vs. remaining experimental participants", 
                  out = "latex", anchor = "tab:balancing_highgoal")
 
 ### Table S9 END #### 
 