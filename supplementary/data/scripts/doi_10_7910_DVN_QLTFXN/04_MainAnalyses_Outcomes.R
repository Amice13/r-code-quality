## This file conducts statistical analyses associated with
## our main socio-political outcome variables as in
## Figure 3: Effects of Wartime Sexual Violence on Sociopolitical Mobilization
## This file produces and exports Figure 3 of the Main Paper.
## and produces the related Appendix Tables A17-21 (Appendix Section 7)

# this file is designed to run after the following files
# uncomment to run previous coding files
# source("00_Packages.R")
# source("01_DataVariables.R")
# source("02_MainAnalyses_Estimation.R")


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
# Loop for analysis of outcome variables
############################################

mat.fct <- function(DF, DF.s, OUT, CTR, FL1, FL2){

  F1 <- as.formula(paste0(OUT, "~ rape_yes + treat_list_CRSV +", CTR, sep=" "))
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
  OUT$direct <- direct
  return(OUT)
}

#################################################
#linear regressions for on normalized variables for paper figure
#################################################

test1.noloop <- lm(D$ingroup_visit_mean_norm ~ D$rape_yes + D$treat_list_CRSV + D$vio_witness1 + D$murder_yes + D$female + D$age + D$edu_level + D$hh_size + D$assets_sum + D$exchange_prev + as.factor(D$territoire))
test2.noloop <- ictreg.joint(list1_CRSV ~ vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + exchange_prev + as.factor(territoire),
                            treat="treat_list_CRSV",
                            outcome = "ingroup_visit_mean_norm",
                            data = D.vis, J=3,
                            outcome.reg = "linear",
                            constrained = TRUE)
summary(test2.noloop)
predict(test2.noloop, sensitive.value = "both", se.fit = TRUE, avg = TRUE, predict.sensitive = TRUE)

# without mediator/reporting variables
Out.vis2.norm <- mat.fct(DF=D, DF.s=D.vis, OUT="ingroup_visit_mean_norm", CTR=controls.wfes, FL1="gaussian", FL2="linear")
Out.org2.lin <- mat.fct(DF=D, DF.s=D.org, OUT="org_leader_d", CTR=controls.wfes, FL1="gaussian" , FL2="linear")
Out.mem2.norm <- mat.fct(DF=D, DF.s=D.mem, OUT="org_member_r1_norm", CTR=controls.wfes, FL1="gaussian" , FL2="linear")
Out.eve2.norm <- mat.fct(DF=D, DF.s=D.eve, OUT="event_com_mean_norm", CTR=controls.wfes, FL1="gaussian" , FL2="linear")
Out.don2.norm <- mat.fct(DF=D, DF.s=D.don, OUT="donate_amount_norm", CTR=controls.wfes, FL1="gaussian" , FL2="linear")


#################################################
## regressions specifications for appendix tables - logistic for dichotomous; not normalized linear for others
#################################################

# Outcome 1: Personal exchanges / visit
Out.vis2 <- mat.fct(DF=D, DF.s=D.vis, OUT="ingroup_visit_mean", CTR=controls.wfes, FL1="gaussian", FL2="linear")

# Outcome 2: Civic Leadership
Out.org2 <- mat.fct(DF=D, DF.s=D.org, OUT="org_leader_d", CTR=controls.wfes, FL1="binomial" , FL2="logistic") 

# Outcome 3: Civic Engagement org membership
Out.mem2 <- mat.fct(DF=D, DF.s=D.mem, OUT="org_member_r1", CTR=controls.wfes, FL1="gaussian" , FL2="linear")

# Outcome 4: Cooperative Behavior - Engagement in events
Out.eve2 <- mat.fct(DF=D, DF.s=D.eve, OUT="event_com_mean", CTR=controls.wfes, FL1="gaussian" , FL2="linear")

# Outcome 5: Public Goods Contribution
Out.don2 <- mat.fct(DF=D, DF.s=D.don, OUT="donate_amount", CTR=controls.wfes, FL1="gaussian" , FL2="linear")


combine.mats <- function(DM1, DM2){
  R1 <- data.frame(DM1$MAT[1:11,], DM2$MAT[1:11,])
  R3 <- data.frame(DM1$MAT[12:17,], DM2$MAT[12:17,])
  
  M <- rbind.data.frame(R1, R3)
  colnames(M) <- rep(c("Coef", "SE"), times=4)
  return(M)
  }


# normalized versions for appendix
Out.vis.norm <- combine.mats(DM1=Out.vis2.norm , DM2=Out.vis2)

Out.org.lin <- combine.mats(DM1=Out.org2.lin, DM2=Out.org2)

Out.mem.norm <- combine.mats(DM1=Out.mem2.norm, DM2=Out.mem2)

Out.eve.norm <- combine.mats(DM1=Out.eve2.norm, DM2=Out.eve2)

Out.don.norm <- combine.mats(DM1=Out.don2.norm, DM2=Out.don2)


# labeling for Appendix table outputs

names.out <- c("(Intercept)", "CRSV Exposure", "List Treatment", "Witnessed CRSV", "Homicide", "Female", "Age", "Education", "HH Size", "Assets", "Prev Exchange","Kabare", "Kalehe", "Mwenga", "Uvira", "Walungu", "N")

rownames(Out.vis.norm) <- names.out
rownames(Out.org.lin) <- names.out
rownames(Out.mem.norm) <- names.out
rownames(Out.eve.norm) <- names.out
rownames(Out.don.norm) <- names.out

#####################
### Appendix 7 Table outputs  A17-21---------------------------------------------------
######################

k.fct2 <- function(M, Title, Notes){kable(M, "html", booktabs = T, caption=Title, row.names = TRUE, align = c("l")) %>%
    kable_styling() %>% 
    kable_styling(latex_options = c("scale_down","hold_position")) %>%
    add_header_above(c(" " = 1, "Direct \n (Model 1)" = 2, "Indirect Pred \n (Model 2)" = 2, "Direct \n (Model 3)" = 2, "Indirect Pred \n (Model 4)" = 2)) %>%
    footnote(general = Notes)
}

## Table A17
k.fct2(M=Out.vis.norm, Title="Freq. Personal Exchanges", Notes=c("M1 & M2 Linear (normalized)", "M3 & M4 Linear (not normalized)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

## Table A18
k.fct2(M=Out.org.lin, Title="Leader of Organization", Notes=c("M1 & M2 Linear (dichotomous)", "M3 & M4 Logistic (dichotomous)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

## Table A19
k.fct2(M=Out.mem.norm, Title="Membership in Organization", Notes=c("M1 & M2 Linear (normalized)", "M3 & M4 Linear (not normalized)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

## Table A20
k.fct2(M=Out.eve.norm, Title="Freq. Engagement in Events", Notes=c("M1 & M2 Linear (normalized)", "M3 & M4 Linear (not normalized)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))

## Table A21
k.fct2(M=Out.don.norm, Title="Public Goods Contribution", Notes=c("M1 & M2 Linear (normalized)", "M3 & M4 Linear (not normalized)", "significance levels indicated as +p<.1 *p<=.05, **p<.01, ***p<.001"))



########################################################################
# COEFFICIENT PLOT FOR SOCIOPOLITICAL OUTCOMES IN FIGURE 3
########################################################################

outm1.norm <- data.frame(Outcome = c("Personal Exchange",
                                     "Event Engagements",
                                     "Donate Amount",
                                     "Org Leadership",
                                     "Org Membership"),
                         Coefficient = c(Out.vis2.norm$M1["rape_yes", 1],
                                         Out.eve2.norm$M1["rape_yes", 1],
                                         Out.don2.norm$M1["rape_yes", 1],
                                         Out.org2.lin$M1["rape_yes", 1],
                                         Out.mem2.norm$M1["rape_yes", 1]),
                         SE = c(Out.vis2.norm$M1["rape_yes", 2],
                                Out.eve2.norm$M1["rape_yes", 2],
                                Out.don2.norm$M1["rape_yes", 2],
                                Out.org2.lin$M1["rape_yes", 2],
                                Out.mem2.norm$M1["rape_yes", 2]),
                         modelName = "Direct")

outm2.norm <- data.frame(Outcome =  c("Personal Exchange",
                                      "Event Engagements",
                                      "Donate Amount",
                                      "Org Leadership",
                                      "Org Membership"),
                         Coefficient = c(unlist(Out.vis2.norm$M3["par.outcome"])["par.outcome.zrep"],
                                         unlist(Out.eve2.norm$M3["par.outcome"])["par.outcome.zrep"],
                                         unlist(Out.don2.norm$M3["par.outcome"])["par.outcome.zrep"],
                                         unlist(Out.org2.lin$M3["par.outcome"])["par.outcome.zrep"],
                                         unlist(Out.mem2.norm$M3["par.outcome"])["par.outcome.zrep"]),
                         SE = c(unlist(Out.vis2.norm$M3["se.outcome"])["se.outcome15"],
                                unlist(Out.eve2.norm$M3["se.outcome"])["se.outcome15"],
                                unlist(Out.don2.norm$M3["se.outcome"])["se.outcome15"],
                                unlist(Out.org2.lin$M3["se.outcome"])["se.outcome15"],
                                unlist(Out.mem2.norm$M3["se.outcome"])["se.outcome15"]),
                         modelName = "List")


models.norm <- data.frame(rbind(outm1.norm, outm2.norm))

models.norm$Outcome <- factor(models.norm$Outcome,levels = c("Org Membership","Org Leadership","Donate Amount", "Event Engagements", "Personal Exchange"))

outplot.norm <- ggplot(models.norm, aes(colour = modelName, fill = Outcome)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = Outcome, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = Outcome, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  geom_text(aes(x=Outcome,y=Coefficient, label=as.character(round(Coefficient,2))), colour="gray20", alpha=2, size=2, nudge_x = 0, nudge_y = 0, check_overlap = T) +
  coord_flip() + theme_bw() + theme(legend.position="top") + scale_color_manual(values=c('#fdbb84','#2c7fb8')) +
  theme(legend.title=element_blank())


# VIEW Figures in Main Paper
outplot.norm   # fig 3

## EXPORT Figure 3 in Main Paper
jpeg("Figure3_Coefficients.jpeg", units="in", width=5, height=4, res=300)
outplot.norm
dev.off()

