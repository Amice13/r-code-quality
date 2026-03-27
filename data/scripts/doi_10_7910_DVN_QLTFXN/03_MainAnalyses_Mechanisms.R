## This file conducts statistical analyses associated with
## mechanisms PTG, Stigma, and Self-blame
## for Figure 4: Psychosocial Correlates of Wartime Sexual Violence
## This file also conducts statistical analyses associated with
## Displacement for Figure 5: Other Correlates of Wartime Sexual Violence
## The code produces Figures 4 and 5 of the Main Paper
## and provides code to export both figures
## This file also produces Appendix Tables A7-16 (Appendix Section 6)

# this file is designed to run after the following files
# uncomment to run previous coding files
# source("00_Packages.R")
# source("01_DataVariables.R")
# source("02_MainAnalyses_Outcomes.R")
# source("03_MainAnalyses_Estimation.R")


############################################
# General functions and list relevant to multiple analyses in multiple files
############################################


# function for statistical significance levels
p.stars.fct <- function(PS){
  PS$p.stars <- "   "
  PS$p.stars[PS$p.value<.1] <- "+  "
  PS$p.stars[PS$p.value<.05] <- "*  "
  PS$p.stars[PS$p.value<.01] <- "** "
  PS$p.stars[PS$p.value<.001] <- "***"
  return(PS$p.stars)
}


#basic controls
controls <- "vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + exchange_prev"
controls.wfes <- "vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + exchange_prev + as.factor(territoire)"

############################################
# Loop for analysis of mediator variables
############################################

med.fct <- function(DF, DF.s, OUT, CTR, FL1, FL2){
  
  F1 <- as.formula(paste0(OUT,"~ rape_yes + treat_list_CRSV +", CTR, sep=" "))
  F3 <- as.formula(paste0("list1_CRSV ~ ", CTR, sep=" "))
  
  direct <- glm(F1, 
                data=DF, family = FL1)
  indirect.bi <- ictreg.joint(F3,
                              treat="treat_list_CRSV", 
                              outcome = OUT,
                              data = DF.s, J=3, 
                              outcome.reg = FL2,
                              constrained = TRUE)
  
  
  # M3 (indirect with List package)
  indirect.bi$z <- cbind(unlist((indirect.bi)["par.outcome"]))/(cbind(unlist((indirect.bi)["se.outcome"])))
  indirect.bi$p <- 2*pnorm(-abs(indirect.bi$z))
  
  indirect.mat <- data.frame(cbind(unlist(indirect.bi$par.outcome)), cbind(unlist(indirect.bi$se.outcome)), indirect.bi$z, indirect.bi$p)
  rownames(indirect.mat) <- row.names(cbind(unlist(indirect.bi$se.outcome)))
  rownames(indirect.mat)[nrow(indirect.mat)] <- "Sensitive"
  names(indirect.mat) <- c("Coefficient", "Std. Error", "Z Score", "p.value")
  
  
  indirect.mat$p.stars <- p.stars.fct(PS=indirect.mat)
  
  indirect.mat$SE <- paste("(", round(indirect.mat[,"Std. Error"], 3), ")" , sep="")
  
  indirect.mat$Coef <- NA
  indirect.mat$Coef[indirect.mat$Coefficient>=0] <- 
    paste(" ", round(indirect.mat$Coefficient[indirect.mat$Coefficient>=0],3), indirect.mat$p.stars[indirect.mat$Coefficient>=0], sep = "")
  indirect.mat$Coef[indirect.mat$Coefficient<0] <- 
    paste(round(indirect.mat$Coefficient[indirect.mat$Coefficient<0],3), indirect.mat$p.stars[indirect.mat$Coefficient<0], sep = "")
  indirect.mat <- data.frame(indirect.mat)
  rownames(indirect.mat)[nrow(indirect.mat)] <- "rape_yes"
  
  
  # M1 (direct)
  direct.mat <- data.frame(direct$coefficients, summary(direct)$coefficients[,"Std. Error"], 
                           summary(direct)$coefficients[,4], rep(NA, times=length(direct$coefficients)))
  names(direct.mat) <- c("Coefficient", "Std. Error", "p.value", "p.stars")
  
  direct.mat$p.stars <- p.stars.fct(PS=direct.mat)
  
  direct.mat$Coef <- NA
  direct.mat$Coef[direct.mat$Coefficient>=0] <- 
    paste(" ", round(direct.mat$Coefficient[direct.mat$Coefficient>=0],3), direct.mat$p.stars[direct.mat$Coefficient>=0], sep = "")
  direct.mat$Coef[direct.mat$Coefficient<0] <- 
    paste(round(direct.mat$Coefficient[direct.mat$Coefficient<0],3), direct.mat$p.stars[direct.mat$Coefficient<0], sep = "")
  
  direct.mat$SE <- NA
  direct.mat$SE <- paste("(", round(direct.mat[,"Std. Error"], 3), ")" , sep="")
  
  # Merge
  
  direct.mat$rownum <- rep(1:nrow(direct.mat))
  direct.mat <- data.frame(direct.mat)
  MAT.merge1 <- merge(direct.mat[, c("rownum","Coef", "SE")], indirect.mat[, c("Coef", "SE")], by="row.names", all=TRUE, sort=FALSE)
  MAT <- MAT.merge1[order(MAT.merge1$rownum),]
  rownames(MAT) <- MAT$Row.names
  MAT <- data.frame(MAT[,3:6])
  MAT <- rbind(MAT, c(nrow(DF.s), "", nrow(DF.s), ""))
  names(MAT) <- rep(c("Coef", "SE"), times=2)
  rownames(MAT)[nrow(MAT)] <- "N"
  
  OUT <- list()
  OUT$MAT <- MAT
  OUT$M1 <- direct.mat[,1:2]
  OUT$M3 <- indirect.bi
  return(OUT)
}


#################################################
#linear regressions for on normalized variables or dichotomous variables for paper figure
#################################################

Med.sti.testexp1 <-med.fct(DF=D, DF.s=D.sti, OUT="stigma_exp_m_norm", CTR=controls.wfes, FL1="gaussian", FL2="linear")

# Mediator 1: Anticipated Stigma
Med.sti1.norm <- med.fct(DF=D, DF.s=D.sti, OUT="stigma_anticip_m_norm", CTR=controls.wfes, FL1="gaussian" , FL2="linear")

# Mediator 2: Victim Blaming
Med.vic1.norm <- med.fct(DF=D, DF.s=D.sti, OUT="stigma_internal_r1_norm", CTR=controls.wfes, FL1="gaussian", FL2="linear")

# Mediator 3: PTG
Med.ptg1.norm <- med.fct(DF=D, DF.s=D.sti, OUT="ptg_all_norm", CTR=controls.wfes, FL1="gaussian" , FL2="linear")

# Mediator 4: leave home
Med.leave1.lin <- med.fct(DF=D, DF.s=D.sti, OUT="leavehome_yes", CTR=controls.wfes, FL1="gaussian" , FL2="linear")

#################################################
## regressions specifications for appendix tables (logistic or non normalized)
#################################################

Med.sti.testexp2 <-med.fct(DF=D, DF.s=D.sti, OUT="stigma_exp_m", CTR=controls.wfes, FL1="gaussian", FL2="linear")

# Mediator 1: Anticipated Stigma
Med.sti1 <- med.fct(DF=D, DF.s=D.sti, OUT="stigma_anticip_m", CTR=controls.wfes, FL1="gaussian", FL2="linear") 

# Mediator 2: Victim Blaming
Med.vic1 <- med.fct(DF=D, DF.s=D.sti, OUT="stigma_internal_r1", CTR=controls.wfes, FL1="gaussian", FL2="linear")

# Mediator 3: PTG
Med.ptg1 <- med.fct(DF=D, DF.s=D.sti, OUT="ptg_all", CTR=controls.wfes, FL1="gaussian" , FL2="linear")

# Mediator 4: leave home
Med.leave1 <- med.fct(DF=D, DF.s=D.sti, OUT="leavehome_yes", CTR=controls.wfes, FL1="binomial" , FL2="logistic")

########## naming  rows (control vars) in data #########################

names.med <- c("(Intercept)", "CRSV Exposure", "List Treatment", "Witnessed CRSV", "Homicide", "Female", "Age", "Education", "HH Size", "Assets", "Prev Exchange", "Kabare", "Kalehe", "Mwenga", "Uvira", "Walungu","N")

rownames(Med.sti1.norm$MAT) <- names.med
rownames(Med.vic1.norm$MAT) <- names.med
rownames(Med.ptg1.norm$MAT) <- names.med
rownames(Med.leave1.lin$MAT) <- names.med
rownames(Med.sti1$MAT) <- names.med
rownames(Med.vic1$MAT) <- names.med
rownames(Med.ptg1$MAT) <- names.med
rownames(Med.leave1$MAT) <- names.med
rownames(Med.sti.testexp1$MAT) <- names.med
rownames(Med.sti.testexp2$MAT) <- names.med

#####################
## Appendix 6 Table Outputs, Tables A7-16 (Section 6)
#####################


k.fct <- function(M, Title, Notes){kable(M, "html", booktabs = T, caption=Title, row.names = TRUE, align = c("l")) %>%
    kable_styling() %>% 
    kable_styling(latex_options = c("hold_position"), font_size=8) %>%
    add_header_above(c(" " = 1, "Direct \n (Model 1)" = 2, "Indirect Pred \n (Model 2)" = 2)) %>%
    footnote(general = Notes)
}

# Table A7: Anticipated Stigma 
k.fct(M=Med.sti1.norm$MAT, Title="Anticipated Stigma, Figure Equivalent", Notes=c("Linear (Normalized)","significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

# Table A8: Anticipated Stigma
k.fct(M=Med.sti1$MAT, Title="Anticipated Stigma, Not Normalized", Notes=c("Linear (not normalized)","significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))


# Table A9: Self-blame
k.fct(M=Med.vic1.norm$MAT, Title="Self-blame, Figure Equivalent", Notes=c("Linear (Normalized)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

# Table A10: Self-blame
k.fct(M=Med.vic1$MAT, Title="Self-blame, Not Normalized", Notes=c("Linear (not normalized)","significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

# Table A11: PTG
k.fct(M=Med.ptg1.norm$MAT, Title="Post Traumatic Growth, Figure Equivalent", Notes=c("Linear (Normalized)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

# Table A12: PTG
k.fct(M=Med.ptg1$MAT, Title="Post Traumatic Growth, Not Normalized", Notes=c("Linear (not normalized)","significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

# Table A13: Displacement
k.fct(M=Med.leave1.lin$MAT, Title="Displacement, Figure Equivalent", Notes=c("Linear (Dichotomous)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))


# Table A14: Displacement (Logistic)
k.fct(M=Med.leave1$MAT, Title="Displacement, Logistic", Notes=c("Logistic (Dichotomous)","significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

# Table A15: Alternative Stigma Variable (Experienced Stigma)
kable(Med.sti.testexp1$MAT, "html", booktabs=T, caption="Correlate, Experienced Stigma Normalized", row.names = TRUE, align = c("l")) %>% 
  kable_styling() %>% 
  kable_styling(latex_options = c("hold_position"), font_size=8) %>%
  add_header_above(c(" " = 1, "Direct" = 2, "Indirect Pred" = 2)) %>%
  footnote(general = c("Linear (Normalized)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

# Table A16: Alternative Stigma Variable (Experienced Stigma)
kable(Med.sti.testexp2$MAT, "html", booktabs=T, caption="Correlate, Experienced Stigma Not Normalized", row.names = TRUE, align = c("l")) %>% 
  kable_styling() %>% 
  kable_styling(latex_options = c("hold_position"), font_size=8) %>%
  add_header_above(c(" " = 1, "Direct" = 2, "Indirect Pred" = 2)) %>%
  footnote(general = c("Linear (not normalized)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

#########################################################################

#########################################################################
# COEFFICIENT PLOT FOR MECHANISMS (STIGMA, SELF BLAME, PTG) IN FIGURE 4


medm1.norm <- data.frame(Correlate = c("Anticipated Stigma", 
                                       "Self Blame",
                                       "Post Traumatic Growth"),
                         Coefficient = c(Med.sti1.norm$M1["rape_yes", 1],
                                         Med.vic1.norm$M1["rape_yes", 1],
                                         Med.ptg1.norm$M1["rape_yes", 1]),
                         SE = c(Med.sti1.norm$M1["rape_yes", 2], 
                                Med.vic1.norm$M1["rape_yes", 2],
                                Med.ptg1.norm$M1["rape_yes", 2]),
                         modelName = "Direct")

medm2.norm <- data.frame(Correlate =  c("Anticipated Stigma", 
                                        "Self Blame",
                                        "Post Traumatic Growth"),
                         Coefficient = c(unlist(Med.sti1.norm$M3["par.outcome"])["par.outcome.zrep"],
                                         unlist(Med.vic1.norm$M3["par.outcome"])["par.outcome.zrep"],
                                         unlist(Med.ptg1.norm$M3["par.outcome"])["par.outcome.zrep"]),
                         SE = c(unlist(Med.sti1.norm$M3["se.outcome"])["se.outcome15"],
                                unlist(Med.vic1.norm$M3["se.outcome"])["se.outcome15"],
                                unlist(Med.ptg1.norm$M3["se.outcome"])["se.outcome15"]),
                         modelName = "List")


med.models.norm <- data.frame(rbind(medm1.norm, medm2.norm))

med.models.norm$Correlate <- factor(med.models.norm$Correlate,levels = c("Post Traumatic Growth", "Self Blame", "Anticipated Stigma"))

medplot.norm <- ggplot(med.models.norm, aes(colour = modelName)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = Correlate, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = Correlate, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  geom_text(aes(x=Correlate,y=Coefficient, label=as.character(round(Coefficient,2))), colour="gray20", alpha=2, size=2.2, nudge_x = 0, nudge_y = 0, check_overlap = T) +
  coord_flip() + theme_bw() + theme(legend.position="top")+ scale_color_manual(values=c('#fdbb84','#2c7fb8')) +
  theme(legend.title=element_blank())

# VIEW Figures in Main Paper

medplot.norm   # fig 4

## EXPORT Figure 4 in Main Paper
jpeg("Figure4_Correlates.jpeg", units="in", width=5, height=3, res=300)
medplot.norm
dev.off()

#########################################################################
# COEFFICIENT PLOT FOR ALTERNATIVE MEDIATOR VARIABLE (DISPLACEMENT) IN FIGURE 5


medm1.norm.alt <- data.frame(Correlate = c("Displacement"),
                             Coefficient = c(Med.leave1.lin$M1["rape_yes", 1]),
                             SE = c(Med.leave1.lin$M1["rape_yes", 2]),
                             modelName = "Direct")

medm2.norm.alt <- data.frame(Correlate =  c("Displacement"),
                             Coefficient = c(unlist(Med.leave1.lin$M3["par.outcome"])["par.outcome.zrep"]),
                             SE = c(unlist(Med.leave1.lin$M3["se.outcome"])["se.outcome15"]),
                             modelName = "List")


med.models.norm.alt <- data.frame(rbind(medm1.norm.alt, medm2.norm.alt))

med.models.norm.alt$Correlate <- factor(med.models.norm.alt$Correlate,levels = c("Displacement"))

medplot.norm.alt <- ggplot(med.models.norm.alt, aes(colour = modelName)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = Correlate, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = Correlate, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  geom_text(aes(x=Correlate,y=Coefficient, label=as.character(round(Coefficient,2))), colour="gray20", alpha=2, size=2.2, nudge_x = 0, nudge_y = 0, check_overlap = T) +
  coord_flip() + theme_bw() + theme(legend.position="top")+ scale_color_manual(values=c('#fdbb84','#2c7fb8')) +
  theme(legend.title=element_blank())

# VIEW Figures in Main Paper

medplot.norm.alt   # fig 5

## EXPORT Figure 5 in Main Paper

jpeg("Figure5_Correlates.jpeg", units="in", width=5, height=2.5, res=300)
medplot.norm.alt
dev.off()