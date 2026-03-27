######################################################################
###### Rabia Malik
###### Replication Code for "Lesser of Two Evils: Allocating Resources to Opposition Districts in Pakistan
###### (Forthcoming at Legislative Studies Quarterly)
######################################################################

rm(list=ls())
library(foreign)
library(MASS)
library(stargazer)
library(ggplot2)
library(gridExtra)

# load dataset from wherever it has been saved
democ2 <- read.csv("file path here/Malik_LSQ2022_dataB.csv")


#################
# Table 1: Descriptive Statistics
#################
cdf_desc <- data.frame(cbind(as.numeric(paste(democ2$fund_perc)),
                             as.numeric(paste(democ2$opp_dum)),
                             as.numeric(paste(democ2$MofV_abs)),
                             as.numeric(paste(democ2$Previous_MNA)),
                             as.numeric(paste(democ2$Previous_MNA_Terms)),
                             as.numeric(paste(democ2$Min_CurrentOrPast)),
                             as.numeric(paste(democ2$Election_Year)),
                             as.numeric(paste(democ2$Turnout_perc)),
                             as.numeric(paste(democ2$Reg_Voters)),
                             as.numeric(paste(democ2$Num_Cand)),
                             as.numeric(paste(democ2$Urban))))

stargazer(cdf_desc,
          median=TRUE,
          covariate.labels=c("Fund Access Perc", 
                             "Opposition Legislator",
                             "Margin of Victory",
                             "Previous MNA", 
                             "Previous MNA Terms",
                             "Federal Minister", 
                             "Election Year",
                             "Turnout", 
                             "Num Registered Voters",
                             "Num Candidates",
                             "Urban"))

#################
# Table 2: Core-Swing Districts and Development Funds
#################
### Model 1
distFE1 <- (lm(fund_perc ~ opp_dum +
                 MofV_abs +
                 MofV_abs*opp_dum +
                 Urban +
                 Previous_MNA +
                 Min_CurrentOrPast +
                 Start_Year +
                 Turnout_perc +
                 as.numeric(paste(Num_Cand)) +
                 as.factor(District)-1,
               data=democ2))
#### Model 2
adminFE1 <- (lm(fund_perc ~ opp_dum +
                  MofV_abs +
                  opp_dum*MofV_abs +
                  Urban +
                  Previous_MNA +
                  Min_CurrentOrPast +
                  Start_Year +
                  Turnout_perc +
                  as.numeric(paste(Num_Cand)) +
                  as.factor(Election_Year)-1,
                data=democ2))

#### Model 3
Dist_adminFE1 <- (lm(fund_perc ~ opp_dum +
                       MofV_abs +
                       opp_dum*MofV_abs +
                       Urban +
                       Previous_MNA +
                       Min_CurrentOrPast +
                       Start_Year +
                       Turnout_perc +
                       as.numeric(paste(Num_Cand)) +
                       as.factor(District) +
                       as.factor(Election_Year)-1,
                     data=democ2))

# generate table
stargazer(distFE1, adminFE1, Dist_adminFE1,
          type="latex",
          title="Core-Swing Districts and Development Funds ",
          style="ajps",
          summary=TRUE,
          column.labels=c("Fund Access Perc"),
          column.separate=c(3),
          covariate.labels=c("Opposition Legislator",
                             "Margin of Victory",
                             "Urban",
                             "Previous MNA",
                             "Federal Minister",
                             "Year",
                             "Turnout",
                             "Num. Cand.",
                             "Opp*MofV"),
          dep.var.labels.include=FALSE,
          digits=1,
          align=TRUE,
          keep.stat=c("n", "adj.rsq"),
          omit=c(9:50)
)
# Note that, due to omitting the FEs output, the stargazer output needs a bit of manual fixing.
# Specifically, the correct interaction coefficient is in the last row of the output, and
# there are two extra rows being output that have been manually deleted when inserting the
# table into the tex file for the paper.


##################
### Figure 1: Effect of Victory Margin on Development Fund Access
##################

# defining range for competitiveness variable based on actual data
comp.range <- seq(from=0, to=0.6, length=1099)

# create RP data set based on means for other variables
OLS_DFE_pred_RP <- data.frame(opp_dum=0,
                              MofV_abs=comp.range,
                              Urban=1,
                              Previous_MNA=1,
                              Min_CurrentOrPast=0,
                              Start_Year=2000,
                              Turnout_perc=47.05,
                              Num_Cand=6.2,
                              District=(democ2$District)[747])

# create Opp data set based on means for other variables
OLS_DFE_pred_Opp <- data.frame(opp_dum=1,
                               MofV_abs=comp.range,
                               Urban=1,
                               Previous_MNA=1,
                               Min_CurrentOrPast=0,
                               Start_Year=2000,
                               Turnout_perc=47.05,
                               Num_Cand=6.2,
                               District=(democ2$District)[747])

# Use Model 1 coefficients from above to create predicted y-hats
predicted_y_CI_winners <- predict(distFE1, newdata=OLS_DFE_pred_RP, interval="confidence")
predicted_y_CI_opp <- predict(distFE1, newdata=OLS_DFE_pred_Opp, interval="confidence")

# save predicted dataframes to plot
predicted_y_CI_rulingparty <- cbind(comp.range, predicted_y_CI_winners)
predicted_y_CI_opposition <- cbind(comp.range, predicted_y_CI_opp)

opposition <- as.data.frame(predicted_y_CI_opposition)
ruling <- as.data.frame(predicted_y_CI_rulingparty)

# rename data frame variables to plot
names(opposition)[1] <- "margin"
names(ruling) <- c("margin2", "fit2", "lwr2", "upr2")

# Opposition figure
OppPlot <- ggplot(data = opposition, aes(x= margin, y = fit)) +
  geom_line(aes(x=margin,y=fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), color="light gray", alpha=0.1) +
  scale_x_continuous("\n Margin of Victory", breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6, 0.7)) +
  scale_y_continuous("Fund Access % \n", breaks = c(-50, -25, 0, 25, 50, 75, 100, 125, 150),
                     lim=c(-30,150)) +
  scale_linetype_manual("Party", values = c("solid", "dashed")) +
  ggtitle("Opposition Funds") +
  theme_bw() 
OppPlot

# RP figure
RP_Plot <- ggplot(data = ruling, aes(x= margin2, y = fit2)) +
  geom_line(aes(x=margin2,y=fit2)) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), color="light gray", alpha=0.1) +
  scale_x_continuous("\n Margin of Victory", breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6, 0.7)) +
  scale_y_continuous("Fund Access % \n", breaks = c(-50, -25, 0, 25, 50, 75, 100, 125, 150),
                     lim=c(-30,150)) +
  ggtitle("Ruling Party Funds") +
  scale_linetype_manual("Party", values = c("solid", "dashed")) +
  theme_bw() 
RP_Plot

# putting the two figures together
grid.arrange(OppPlot,RP_Plot,ncol=2)
dev.off()

########################
#### Table 3 (Appendix B): Dichotomized Core Districts and Development Funds
########################

# Model 1
coreDum_distFE <- (lm(fund_perc ~ opp_dum +
                        core +
                        opp_dum*core +
                        Urban +
                        Previous_MNA +
                        Min_CurrentOrPast +
                        Start_Year +
                        Turnout_perc +
                        as.numeric(paste(Num_Cand)) +
                        as.factor(District)-1,
                      data=democ2))

# Model 2
coreDum_adminFE <- (lm(fund_perc ~ opp_dum +
                         core +
                         opp_dum*core +
                         Urban +
                         Previous_MNA +
                         Min_CurrentOrPast +
                         Start_Year +
                         Turnout_perc +
                         as.numeric(paste(Num_Cand)) +
                         as.factor(Election_Year)-1,
                       data=democ2))

# Model 3
coreDum_bothFE <- (lm(fund_perc ~ opp_dum +
                        core +
                        opp_dum*core +
                        Urban +
                        Previous_MNA +
                        Min_CurrentOrPast +
                        Start_Year +
                        Turnout_perc +
                        as.numeric(paste(Num_Cand)) +
                        as.factor(District) +
                        as.factor(Election_Year)-1,
                      data=democ2))

# stargazer output
stargazer(coreDum_distFE, coreDum_adminFE, coreDum_bothFE, type="latex",
          title="Dichotomized Core Districts and Development Funds",
          style="ajps",
          summary=TRUE,
          column.labels=c("Perc. fund accessed"),
          column.separate=c(3),
          covariate.labels=c("Opposition Legislator",
                             "Core District",
                             "Urban",
                             "Previous MNA",
                             "Federal Minister",
                             "Year",
                             "Turnout",
                             "Num. Cand.",
                             "Opp.*Core"),
          dep.var.labels.include=FALSE,
          digits=1,
          align=TRUE,
          keep.stat=c("n", "adj.rsq"),
          omit=c(10:51)
)
# Note that, due to omitting the FEs output, the stargazer output needs a bit of manual fixing.
# Specifically, the correct interaction coefficient is in the last row of the output, and
# there are two extra rows being output that have been manually deleted when inserting the
# table into the tex file for the paper.

#########################
#### Table 4 (Appendix B): Quadratic Victory Margin and Development Funds
#########################

# Model 1
olsQuad_DistFE <- (lm(fund_perc ~ opp_dum +
                        MofV_abs +
                        opp_dum*MofV_abs +
                        opp_dum*MofV_sq +
                        MofV_sq +
                        Urban +
                        Previous_MNA +
                        Min_CurrentOrPast +
                        Start_Year +
                        Turnout_perc +
                        as.numeric(paste(Num_Cand)) +
                        as.factor(District)-1,
                      data=democ2))

# Model 2
olsQuad_AdminFE <- (lm(fund_perc ~ opp_dum +
                         MofV_abs +
                         opp_dum*MofV_abs +
                         opp_dum*MofV_sq +
                         MofV_sq +
                         Urban +
                         Previous_MNA +
                         Min_CurrentOrPast +
                         Start_Year +
                         Turnout_perc +
                         as.numeric(paste(Num_Cand)) +
                         as.factor(Election_Year)-1,
                       data=democ2))

# Model 3
olsQuad_BothFE <- (lm(fund_perc ~ opp_dum +
                        MofV_abs +
                        opp_dum*MofV_abs +
                        opp_dum*MofV_sq +
                        MofV_sq +
                        Urban +
                        Previous_MNA +
                        Min_CurrentOrPast +
                        Start_Year +
                        Turnout_perc +
                        as.numeric(paste(Num_Cand)) +
                        as.factor(District) +
                        as.factor(Election_Year)-1,
                      data=democ2))

# stargazer output
stargazer(olsQuad_DistFE, olsQuad_AdminFE, olsQuad_BothFE, type="latex",
          title="Quadratic Victory Margin and Development Funds",
          style="ajps",
          summary=TRUE,
          column.labels=c("Perc. fund accessed"),
          column.separate=c(3),
          covariate.labels=c("Opposition Legislator",
                             "Margin of Victory",
                             "Margin of Victory sq",
                             "Urban",
                             "Previous MNA",
                             "Federal Minister",
                             "Year",
                             "Turnout",
                             "Num. Cand.",
                             "Opp.*Margin of Vic.",
                             "Opp.*Margin of Vic. sq"),
          dep.var.labels.include=FALSE,
          digits=1,
          align=TRUE,
          keep.stat=c("n", "adj.rsq"),
          omit=c(10:47)
)
# Note that, due to omitting the FEs output, the stargazer output needs a bit of manual fixing.
# Specifically, the correct interaction coefficient is in the last row of the output, and
# there are two extra rows being output that have been manually deleted when inserting the
# table into the tex file for the paper.

#####################
#### Figure 2 (Appendix B): Quadratic Relationship between Ruling Party Victory Margin
# and Development Fund Access
#####################

# competitiveness range variable
comp.range <- seq(from=0, to=0.6, length=1099)

# Ruling Party data frame
OLS_DFE_pred_RP <- data.frame(opp_dum=0,
                              MofV_abs = comp.range,
                              MofV_sq = (comp.range)^2,
                              Urban=1,
                              Previous_MNA=1,
                              Min_CurrentOrPast=0,
                              Start_Year=2000,
                              Turnout_perc=47.05,
                              Num_Cand=6.2,
                              District=(democ2$District)[747])

# Opp Party data frame
OLS_DFE_pred_Opp <- data.frame(opp_dum=1,
                               MofV_abs = comp.range,
                               MofV_sq = (comp.range)^2,
                               Urban=1,
                               Previous_MNA=1,
                               Min_CurrentOrPast=0,
                               Start_Year=2000,
                               Turnout_perc=47.05,
                               Num_Cand=6.2,
                               District=(democ2$District)[747])

# predicted y-hats for both data frames
predicted_y_CI_winners <- predict(olsQuad_DistFE, newdata=OLS_DFE_pred_RP, interval="confidence")
predicted_y_CI_opp <- predict(olsQuad_DistFE, newdata=OLS_DFE_pred_Opp, interval="confidence")

predicted_y_CI_rulingparty <- cbind(comp.range, predicted_y_CI_winners)
predicted_y_CI_opposition <- cbind(comp.range, predicted_y_CI_opp)

# make data frames
opposition <- as.data.frame(predicted_y_CI_opposition)
ruling <- as.data.frame(predicted_y_CI_rulingparty)

# rename to plot
names(opposition)[1] <- "margin"
names(ruling) <- c("margin2", "fit2", "lwr2", "upr2")

data_fig <- as.data.frame(cbind(opposition, ruling))

data.opposition <- data.frame(margin = data_fig$margin,
                              fit = data_fig$fit,
                              lwr = data_fig$lwr,
                              upr = data_fig$upr,
                              party = "Opposition")

data.ruling <- data.frame(margin = data_fig$margin2,
                          fit = data_fig$fit2,
                          lwr = data_fig$lwr2,
                          upr = data_fig$upr2,
                          party = "Ruling")

data_ggplot <- as.data.frame(rbind(data.opposition, data.ruling))

# making figure
plot1 <- ggplot(data = data_ggplot, aes(x= margin, y = fit)) +
  geom_line(aes(lty = party)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, lty=party), color="light gray", alpha=0.2) +
  scale_x_continuous("\n Margin of Victory", breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6, 0.7)) +
  scale_y_continuous("Fund Access % \n", breaks = c(-50, -25, 0, 25, 50, 75, 100, 125, 150,
                                                    175,200,225,250)) +
  scale_linetype_manual("Party", values = c("solid", "dashed")) +
  theme_bw() 
plot1
dev.off()

####################
### Table 5 (Appendix B): Dichotomized Core Districts and Development Funds (Robustness)
####################

# Model 1
coreDum0.3_distFE <- (lm(fund_perc ~ opp_dum +
                           core0.3 +
                           opp_dum*core0.3 +
                           Urban +
                           Previous_MNA +
                           Min_CurrentOrPast +
                           Start_Year +
                           Turnout_perc +
                           as.numeric(paste(Num_Cand)) +
                           as.factor(District)-1,
                         data=democ2))

# Model 2
coreDum0.3_adminFE <- (lm(fund_perc ~ opp_dum +
                            core0.3 +
                            opp_dum*core0.3 +
                            Urban +
                            Previous_MNA +
                            Min_CurrentOrPast +
                            Start_Year +
                            Turnout_perc +
                            as.numeric(paste(Num_Cand)) +
                            as.factor(Election_Year)-1,
                          data=democ2))

# Model 3
coreDum0.3_bothFE <- (lm(fund_perc ~ opp_dum +
                           core0.3 +
                           opp_dum*core0.3 +
                           Urban +
                           Previous_MNA +
                           Min_CurrentOrPast +
                           Start_Year +
                           Turnout_perc +
                           as.numeric(paste(Num_Cand)) +
                           as.factor(District) +
                           as.factor(Election_Year)-1,
                         data=democ2))

# Model 4
coreDum0.4_distFE <- (lm(fund_perc ~ opp_dum +
                           core0.4 +
                           opp_dum*core0.4 +
                           Urban +
                           Previous_MNA +
                           Min_CurrentOrPast +
                           Start_Year +
                           Turnout_perc +
                           as.numeric(paste(Num_Cand)) +
                           as.factor(District)-1,
                         data=democ2))

# Model 5
coreDum0.4_adminFE <- (lm(fund_perc ~ opp_dum +
                            core0.4 +
                            opp_dum*core0.4 +
                            Urban +
                            Previous_MNA +
                            Min_CurrentOrPast +
                            Start_Year +
                            Turnout_perc +
                            as.numeric(paste(Num_Cand)) +
                            as.factor(Election_Year)-1,
                          data=democ2))

# Model 6
coreDum0.4_bothFE <- (lm(fund_perc ~ opp_dum +
                           core0.4 +
                           opp_dum*core0.4 +
                           Urban +
                           Previous_MNA +
                           Min_CurrentOrPast +
                           Start_Year +
                           Turnout_perc +
                           as.numeric(paste(Num_Cand)) +
                           as.factor(District) +
                           as.factor(Election_Year)-1,
                         data=democ2))

# stargarzer output
stargazer(coreDum0.3_distFE, coreDum0.3_adminFE, coreDum0.3_bothFE,
          coreDum0.4_distFE, coreDum0.4_adminFE, coreDum0.4_bothFE,
          type="latex",
          title="Dichotomized Core Districts and Development Funds (Robustness)",
          style="ajps",
          summary=TRUE,
          column.labels=c("Perc. fund accessed"),
          column.separate=c(6),
          covariate.labels=c("Opposition Legislator",
                             "Core District 0.3",
                             "Core District 0.4",
                             "Urban",
                             "Previous MNA",
                             "Federal Minister",
                             "Year",
                             "Turnout",
                             "Num. Cand.",
                             "Opp.*Core"),
          dep.var.labels.include=FALSE,
          digits=1,
          align=TRUE,
          keep.stat=c("n", "adj.rsq"),
          omit=c(10:51)
)
# Note that, due to omitting the FEs output, the stargazer output needs a bit of manual fixing.
# Specifically, the correct interaction coefficient is in the last row of the output, and
# there are two extra rows being output that have been manually deleted when inserting the
# table into the tex file for the paper.

#####################
#### Table 6 (Appendix B): Development Funds: Alternative Measures
#####################

# Model 1
distPKR_FE1 <- (lm(Total_Rs ~ opp_dum +
                     MofV_abs +
                     opp_dum*MofV_abs +
                     Previous_MNA +
                     Min_CurrentOrPast +
                     Start_Year +
                     Turnout_perc +
                     Urban +
                     as.numeric(paste(Num_Cand)) +
                     as.factor(District)-1,
                   data=democ2))

# Model 2
adminPKR_FE1 <- (lm(Total_Rs ~ opp_dum +
                      MofV_abs +
                      opp_dum*MofV_abs +
                      Previous_MNA +
                      Min_CurrentOrPast +
                      Start_Year +
                      Turnout_perc +
                      Urban +
                      as.numeric(paste(Num_Cand)) +
                      as.factor(Election_Year)-1,
                    data=democ2))

# Model 3
Dist_adminPKR_FE1 <- (lm(Total_Rs ~ opp_dum +
                           MofV_abs +
                           opp_dum*MofV_abs +
                           Previous_MNA +
                           Min_CurrentOrPast +
                           Start_Year +
                           Turnout_perc +
                           Urban +
                           as.numeric(paste(Num_Cand)) +
                           as.factor(District) +
                           as.factor(Election_Year)-1,
                         data=democ2))


# Model 4
distAlt_FE1 <- (lm(fund_perc_alt ~ opp_dum +
                     MofV_abs +
                     opp_dum*MofV_abs +
                     Previous_MNA +
                     Min_CurrentOrPast +
                     Start_Year +
                     Turnout_perc +
                     Urban +
                     as.numeric(paste(Num_Cand)) +
                     as.factor(District)-1,
                   data=democ2))

# Model 5
adminAlt_FE1 <- (lm(fund_perc_alt ~ opp_dum +
                      MofV_abs +
                      opp_dum*MofV_abs +
                      Previous_MNA +
                      Min_CurrentOrPast +
                      Start_Year +
                      Turnout_perc +
                      Urban +
                      as.numeric(paste(Num_Cand)) +
                      as.factor(Election_Year)-1,
                    data=democ2))

# Model 6
Dist_adminAlt_FE1 <- (lm(fund_perc_alt ~ opp_dum +
                           MofV_abs +
                           opp_dum*MofV_abs +
                           Previous_MNA +
                           Min_CurrentOrPast +
                           Start_Year +
                           Turnout_perc +
                           Urban +
                           as.numeric(paste(Num_Cand)) +
                           as.factor(District) +
                           as.factor(Election_Year)-1,
                         data=democ2))

# stargazer output
stargazer(distPKR_FE1, adminPKR_FE1, Dist_adminPKR_FE1,
          distAlt_FE1, adminAlt_FE1, Dist_adminAlt_FE1,
          type="latex",
          title="Development Funds: Alternative Measures",
          style="ajps",
          summary=TRUE,
          column.labels=c("Raw Fund Access", "Fund Access Perc. Recoded"),
          column.separate=c(3,3),
          covariate.labels=c("Opposition Legislator",
                             "Margin of Victory",
                             "Previous MNA",
                             "Federal Minister",
                             "Year",
                             "Turnout",
                             "Urban",
                             "Num. Cand.",
                             "Opp.*Margin of Vic."),
          dep.var.labels.include=FALSE,
          digits=1,
          align=TRUE,
          keep.stat=c("n", "adj.rsq"),
          omit=c(9:50)
)
# Note that, due to omitting the FEs output, the stargazer output needs a bit of manual fixing.
# Specifically, the correct interaction coefficient is in the last row of the output, and
# there are two extra rows being output that have been manually deleted when inserting the
# table into the tex file for the paper.

#####################
#### Table 7 (Appendix B): Core-Swing Districts and Election Cycles
#####################

# Model 1
distFE1_elec <- (lm(fund_perc ~ opp_dum +
                      MofV_abs +
                      MofV_abs*opp_dum +
                      Urban +
                      elec_year + 
                      Previous_MNA +
                      Min_CurrentOrPast +
                      Start_Year +
                      Turnout_perc +
                      MofV_abs*opp_dum*elec_year +
                      as.numeric(paste(Num_Cand)) +
                      as.factor(District)-1,
                    data=democ2))
# Model 2
adminFE1_elec <- (lm(fund_perc ~ opp_dum +
                       MofV_abs +
                       opp_dum*MofV_abs +
                       MofV_abs*opp_dum*elec_year +
                       Urban +
                       Previous_MNA +
                       Min_CurrentOrPast +
                       Start_Year +
                       Turnout_perc +
                       as.numeric(paste(Num_Cand)) +
                       as.factor(Election_Year)-1,
                     data=democ2))

# Model 3
Dist_adminFE1_elec <- (lm(fund_perc ~ opp_dum +
                            MofV_abs +
                            opp_dum*MofV_abs +
                            MofV_abs*opp_dum*elec_year +
                            Urban +
                            Previous_MNA +
                            Min_CurrentOrPast +
                            Start_Year +
                            Turnout_perc +
                            as.numeric(paste(Num_Cand)) +
                            as.factor(District) +
                            as.factor(Election_Year)-1,
                          data=democ2))

# stargazer output
stargazer(distFE1_elec, adminFE1_elec, Dist_adminFE1_elec,
          type="latex",
          title="Core-Swing Districts and Election Cycles",
          style="ajps",
          summary=TRUE,
          column.labels=c("Fund Access Perc"),
          column.separate=c(3),
          covariate.labels=c("Opposition Legislator",
                             "Margin of Victory",
                             "Urban",
                             "Election Year",
                             "Previous MNA",
                             "Federal Minister",
                             "Year",
                             "Turnout",
                             "Num. Cand.",
                             "Opp*MofV",
                             "ElecYr*MofV",
                             "ElecYr*Opp",
                             "ElecYr*Opp*MofV"),
          dep.var.labels.include=FALSE,
          digits=1,
          align=TRUE,
          keep.stat=c("n", "adj.rsq"),
          omit=c(10:50)
)
# Note that, due to omitting the FEs output, the stargazer output needs a bit of manual fixing.
# Specifically, the correct interaction coefficient is in the last row of the output, and
# there are two extra rows being output that have been manually deleted when inserting the
# table into the tex file for the paper.

#################
#### Figure 3 (Appendix B): Margin of Victory & Fund Access: Raw Data
#################

# prep opposition data
opp_data <- subset(democ2, opp_dum==1)
count <- 1
bin.size <- .02
binx.opp <- vector(length=length(seq(0,0.6, bin.size)))
biny.opp <- vector(length=length(binx.opp))
last <- 0
for(j in seq(0,0.6, bin.size)) {
  biny.opp[count] <- mean(opp_data$fund_perc[opp_data$MofV_abs >= j-bin.size & opp_data$MofV_abs < j],na.rm=T)
  binx.opp[count] <- (j+last)/2
  last <- j
  count <- count + 1
  print(j)
}

# prep ruling partyy (RP) data
rp_data <- subset(democ2, opp_dum==0)
count <- 1
bin.size <- .02
binx.RP <- vector(length=length(seq(0,0.6, bin.size)))
biny.RP <- vector(length=length(binx.RP))
last <- 0
for(j in seq(0, 0.6, bin.size)) {
  biny.RP[count] <- mean(rp_data$fund_perc[rp_data$MofV_abs >= j-bin.size & rp_data$MofV_abs < j],na.rm=T)
  binx.RP[count] <- (j+last)/2
  last <- j
  count <- count + 1
  print(j)
}

# linear regressions for fit 
reg1 <- lm(opp_data$fund_perc ~ opp_data$MofV_abs)
reg2 <- lm(rp_data$fund_perc ~ rp_data$MofV_abs)

fits1 <- reg1$coefficients[1] + reg1$coefficients[2] *opp_data$MofV_abs
fits2 <- reg2$coefficients[1] + reg2$coefficients[2] * rp_data$MofV_abs

# to get plots in same screen
par(mfrow=c(1,2))

# opp plot
plot(x=opp_data$MofV_abs, y=opp_data$fund_perc, col="white", xlab="Margin of Victory", yaxt="n", ylab="Fund Percentage", 
     cex.lab=1.2, cex.axis=.9, ylim=c(0,140), main="Opposition Funds", cex.main=1.2, cex.axis=1.2, xaxt="n", 
     xlim=c(0,0.5))
points(x=opp_data$MofV_abs, y=opp_data$fund_perc, pch=16, col="gray50", cex=.6)
points(x=binx.opp, y=biny.opp, cex=1, col="black", pch=16)
lines(x=opp_data$MofV_abs, y=fits1, lwd=2, col="black")
axis(side=2, las=1, cex.axis=.7, at=seq(0,140,20), labels=seq(0, 140, 20), cex.axis=1)
axis(side=1, at=seq(0, 0.5, 0.1), labels=seq(0, 0.5, 0.1), cex.axis=1)

# RP plot
plot(x=rp_data$MofV_abs, y=rp_data$fund_perc, col="white", xlab="Margin of Victory", yaxt="n", 
     ylab="Fund Percentage", cex.lab=1.2, cex.axis=.9, ylim=c(0,140), 
     main="Ruling Party Funds", cex.main=1.2, cex.axis=1.2, xaxt="n", 
     xlim=c(0,0.5), title="RP Funds")
points(x=rp_data$MofV_abs, y=rp_data$fund_perc, pch=16, col="gray50", cex=.6)
points(x=binx.RP, y=biny.RP, cex=1, col="black", pch=16)
lines(x=rp_data$MofV_abs, y=fits2, lwd=2, col="black")
axis(side=2, las=1, cex.axis=.7, at=seq(0,140,20), labels=seq(0, 140, 20), cex.axis=1)
axis(side=1, at=seq(0, 0.5, 0.1), labels=seq(0, 0.5, 0.1), cex.axis=1)

dev.off()

######################
#### Table 8 (Appendix B): Previous MNA experience and Fund Access
######################

# Model 1
distFE1_oppV<- (lm(fund_perc ~ opp_dum +
                     MofV_abs +
                     MofV_abs*opp_dum*Previous_MNA_Terms +
                     Urban +
                     Previous_MNA_Terms +
                     Min_CurrentOrPast +
                     Start_Year +
                     Turnout_perc +
                     as.numeric(paste(Num_Cand)) +
                     as.factor(District)-1,
                   data=democ2))

# Model 2
adminFE1_oppV<- (lm(fund_perc ~ opp_dum +
                      MofV_abs +
                      MofV_abs*opp_dum*Previous_MNA_Terms +
                      Urban +
                      Previous_MNA_Terms +
                      Min_CurrentOrPast +
                      Start_Year +
                      Turnout_perc +
                      as.numeric(paste(Num_Cand)) +
                      as.factor(Election_Year)-1,
                    data=democ2))

# Model 3
distAdminFE1_oppV<- (lm(fund_perc ~ opp_dum +
                          MofV_abs +
                          MofV_abs*opp_dum*Previous_MNA_Terms +
                          Urban +
                          Previous_MNA_Terms +
                          Min_CurrentOrPast +
                          Start_Year +
                          Turnout_perc +
                          as.numeric(paste(Num_Cand)) +
                          as.factor(District) +
                          as.factor(Election_Year)-1,
                        data=democ2))

# stargazer output
stargazer(distFE1_oppV, adminFE1_oppV, distAdminFE1_oppV,
          type="latex",
          title="Previous MNA experience and Fund Access",
          style="ajps",
          summary=TRUE,
          column.labels=c("Fund Access Perc"),
          column.separate=c(3),
          covariate.labels=c("Opposition Legislator",
                             "Margin of Victory",
                             "Previous MNA",
                             "Urban",
                             "Federal Minister",
                             "Year",
                             "Turnout",
                             "Num. Cand.",
                             "Opp*MofV",
                             "Opp*PrevMNA*MofV"),
          dep.var.labels.include=FALSE,
          digits=1,
          align=TRUE,
          keep.stat=c("n", "adj.rsq"),
          omit=c(10:50)
)
# Note that, due to omitting the FEs output, the stargazer output needs a bit of manual fixing.
# Specifically, the correct interaction coefficient is in the last row of the output, and
# there are two extra rows being output that have been manually deleted when inserting the
# table into the tex file for the paper.




