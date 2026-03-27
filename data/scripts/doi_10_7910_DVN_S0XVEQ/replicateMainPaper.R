library(data.table)
library(mediation)
library(readstata13)
library(texreg)

set.seed(1989)

setwd() # set to location where files are stored

TAPSdata <- fread("TAPSdata.csv", header = TRUE, stringsAsFactors = FALSE)
MTurk_stylized_data <- fread("MTurk_stylized_data.csv", header = TRUE, stringsAsFactors = FALSE)
MTurk_stylized_data_ordering <- fread("MTurk_stylized_data_ordering.csv", header = TRUE, stringsAsFactors = FALSE)
MTurk_factorial_data <- fread("MTurk_factorial_data.csv", header = TRUE, stringsAsFactors = FALSE)
MTurk_flint_data <- fread("MTurk_flint_data.csv", header = TRUE, stringsAsFactors = FALSE)

# set baseline levels of factor variables

TAPSdata$Floodresponse <- relevel(as.factor(TAPSdata$Floodresponse), ref = "control")
MTurk_stylized_data$BCresponse <- relevel(as.factor(MTurk_stylized_data$BCresponse), ref = "control")
MTurk_stylized_data$BSresponse <- relevel(as.factor(MTurk_stylized_data$BSresponse), ref = "control")
MTurk_stylized_data$HWresponse <- relevel(as.factor(MTurk_stylized_data$HWresponse), ref = "control")
MTurk_flint_data$treatment <- relevel(as.factor(MTurk_flint_data$treatment), ref = "control")

###############################################################################
### MAIN PAPER FIGURES
###############################################################################
# Figure 1, approval (left panel) and vote choice (right panel)

# linear model for handling DV for all governmental crises

flood_handling_bi_model <- lm(handling_bin ~ Floodresponse, 
                              data = TAPSdata, weights = jan2018wt1)
summary(flood_handling_bi_model)

BC_handling_bi_model <- lm(BCgovapprove_bin ~ BCresponse, data=MTurk_stylized_data)
summary(BC_handling_bi_model)

BS_handling_bi_model <- lm(BSgovapprove_bin ~ BSresponse, data=MTurk_stylized_data)
summary(BS_handling_bi_model)

HW_handling_bi_model <- lm(HWgovapprove_bin ~ HWresponse, data=MTurk_stylized_data)
summary(HW_handling_bi_model)

# create an empty DF to store the elements from the linear model objects to
# create the figure

handling_results <- data.frame("context"=character(0), "treatment"=character(0),
                               "point_est"=numeric(0), "lb"=numeric(0), "ub"=numeric(0))

handling_results <- rbind(handling_results, data.frame("context"="flood", 
                                                       "treatment"="blameclaim", 
                                                       "point_est"=flood_handling_bi_model$coefficients[2],
                                                       "lb"=confint(flood_handling_bi_model)[2,1],
                                                       "ub"=confint(flood_handling_bi_model)[2,2]))

handling_results <- rbind(handling_results, data.frame("context"="flood", 
                                                       "treatment"="blamedeflect", 
                                                       "point_est"=flood_handling_bi_model$coefficients[3],
                                                       "lb"=confint(flood_handling_bi_model)[3,1],
                                                       "ub"=confint(flood_handling_bi_model)[3,2]))

handling_results <- rbind(handling_results, data.frame("context"="bridge", 
                                                       "treatment"="blameclaim", 
                                                       "point_est"=BC_handling_bi_model$coefficients[2],
                                                       "lb"=confint(BC_handling_bi_model)[2,1],
                                                       "ub"=confint(BC_handling_bi_model)[2,2]))

handling_results <- rbind(handling_results, data.frame("context"="bridge", 
                                                       "treatment"="blamedeflect", 
                                                       "point_est"=BC_handling_bi_model$coefficients[3],
                                                       "lb"=confint(BC_handling_bi_model)[3,1],
                                                       "ub"=confint(BC_handling_bi_model)[3,2]))

handling_results <- rbind(handling_results, data.frame("context"="budget", 
                                                       "treatment"="blameclaim", 
                                                       "point_est"=BS_handling_bi_model$coefficients[2],
                                                       "lb"=confint(BS_handling_bi_model)[2,1],
                                                       "ub"=confint(BS_handling_bi_model)[2,2]))

handling_results <- rbind(handling_results, data.frame("context"="budget", 
                                                       "treatment"="blamedeflect", 
                                                       "point_est"=BS_handling_bi_model$coefficients[3],
                                                       "lb"=confint(BS_handling_bi_model)[3,1],
                                                       "ub"=confint(BS_handling_bi_model)[3,2]))

handling_results <- rbind(handling_results, data.frame("context"="heat", 
                                                       "treatment"="blameclaim", 
                                                       "point_est"=HW_handling_bi_model$coefficients[2],
                                                       "lb"=confint(HW_handling_bi_model)[2,1],
                                                       "ub"=confint(HW_handling_bi_model)[2,2]))

handling_results <- rbind(handling_results, data.frame("context"="heat", 
                                                       "treatment"="blamedeflect", 
                                                       "point_est"=HW_handling_bi_model$coefficients[3],
                                                       "lb"=confint(HW_handling_bi_model)[3,1],
                                                       "ub"=confint(HW_handling_bi_model)[3,2]))

# linear model for vote choice DV for all four governmental crises

flood_vote_next_bi_model <- lm(vote_bin ~ Floodresponse,
                               data = TAPSdata, weights = jan2018wt1)
summary(flood_vote_next_bi_model)

BC_vote_next_bi_model <- lm(BCgovvote_bin ~ BCresponse, data=MTurk_stylized_data)
summary(BC_vote_next_bi_model)

BS_vote_next_bi_model <- lm(BSgovvote_bin ~ BSresponse, data=MTurk_stylized_data)
summary(BS_vote_next_bi_model)

HW_vote_next_bi_model <- lm(HWgovvote_bin ~ HWresponse, data=MTurk_stylized_data)
summary(HW_vote_next_bi_model)

# create an empty DF to store the elements from the linear model objects to
# create the figure

vote_results <- data.frame("context"=character(0), "treatment"=character(0),
                           "point_est"=numeric(0), "lb"=numeric(0), "ub"=numeric(0))

vote_results <- rbind(vote_results, data.frame("context"="flood", 
                                               "treatment"="blameclaim", 
                                               "point_est"=flood_vote_next_bi_model$coefficients[2],
                                               "lb"=confint(flood_vote_next_bi_model)[2,1],
                                               "ub"=confint(flood_vote_next_bi_model)[2,2]))

vote_results <- rbind(vote_results, data.frame("context"="flood", 
                                               "treatment"="blamedeflect", 
                                               "point_est"=flood_vote_next_bi_model$coefficients[3],
                                               "lb"=confint(flood_vote_next_bi_model)[3,1],
                                               "ub"=confint(flood_vote_next_bi_model)[3,2]))

vote_results <- rbind(vote_results, data.frame("context"="bridge", 
                                               "treatment"="blameclaim", 
                                               "point_est"=BC_vote_next_bi_model$coefficients[2],
                                               "lb"=confint(BC_vote_next_bi_model)[2,1],
                                               "ub"=confint(BC_vote_next_bi_model)[2,2]))

vote_results <- rbind(vote_results, data.frame("context"="bridge", 
                                               "treatment"="blamedeflect", 
                                               "point_est"=BC_vote_next_bi_model$coefficients[3],
                                               "lb"=confint(BC_vote_next_bi_model)[3,1],
                                               "ub"=confint(BC_vote_next_bi_model)[3,2]))

vote_results <- rbind(vote_results, data.frame("context"="budget", 
                                               "treatment"="blameclaim", 
                                               "point_est"=BS_vote_next_bi_model$coefficients[2],
                                               "lb"=confint(BS_vote_next_bi_model)[2,1],
                                               "ub"=confint(BS_vote_next_bi_model)[2,2]))

vote_results <- rbind(vote_results, data.frame("context"="budget", 
                                               "treatment"="blamedeflect", 
                                               "point_est"=BS_vote_next_bi_model$coefficients[3],
                                               "lb"=confint(BS_vote_next_bi_model)[3,1],
                                               "ub"=confint(BS_vote_next_bi_model)[3,2]))

vote_results <- rbind(vote_results, data.frame("context"="heat", 
                                               "treatment"="blameclaim", 
                                               "point_est"=HW_vote_next_bi_model$coefficients[2],
                                               "lb"=confint(HW_vote_next_bi_model)[2,1],
                                               "ub"=confint(HW_vote_next_bi_model)[2,2]))

vote_results <- rbind(vote_results, data.frame("context"="heat", 
                                               "treatment"="blamedeflect", 
                                               "point_est"=HW_vote_next_bi_model$coefficients[3],
                                               "lb"=confint(HW_vote_next_bi_model)[3,1],
                                               "ub"=confint(HW_vote_next_bi_model)[3,2]))

# create Figure 1

pdf(file = , # enter your preferred location to save the figure here
    family = "Times", height = 7, width=13)
par(mar=c(5.1,11,2.75,.5))
# plotting the point estimates for both panels
plot(x=c(handling_results$point_est[which(handling_results$context=="heat" & handling_results$treatment=="blamedeflect")],
         handling_results$point_est[which(handling_results$context=="heat" & handling_results$treatment=="blameclaim")],
         handling_results$point_est[which(handling_results$context=="budget" & handling_results$treatment=="blamedeflect")],
         handling_results$point_est[which(handling_results$context=="budget" & handling_results$treatment=="blameclaim")],
         handling_results$point_est[which(handling_results$context=="bridge" & handling_results$treatment=="blamedeflect")],
         handling_results$point_est[which(handling_results$context=="bridge" & handling_results$treatment=="blameclaim")],
         handling_results$point_est[which(handling_results$context=="flood" & handling_results$treatment=="blamedeflect")],
         handling_results$point_est[which(handling_results$context=="flood" & handling_results$treatment=="blameclaim")],
         vote_results$point_est[which(vote_results$context=="heat" & vote_results$treatment=="blamedeflect")]+1,
         vote_results$point_est[which(vote_results$context=="heat" & vote_results$treatment=="blameclaim")]+1,
         vote_results$point_est[which(vote_results$context=="budget" & vote_results$treatment=="blamedeflect")]+1,
         vote_results$point_est[which(vote_results$context=="budget" & vote_results$treatment=="blameclaim")]+1,
         vote_results$point_est[which(vote_results$context=="bridge" & vote_results$treatment=="blamedeflect")]+1,
         vote_results$point_est[which(vote_results$context=="bridge" & vote_results$treatment=="blameclaim")]+1,
         vote_results$point_est[which(vote_results$context=="flood" & vote_results$treatment=="blamedeflect")]+1,
         vote_results$point_est[which(vote_results$context=="flood" & vote_results$treatment=="blameclaim")]+1), 
     y=rep(c(1,3,9,11,17,19,25,27),2),
     xlim=c(-.45,1.45),
     xaxt="n",
     ylim=c(0.5, 29.5),
     pch=19,
     tck=-.02,
     cex.axis=0.9,
     cex=1.5,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
abline(v=1,lwd=2, col="gray",lty=2)
# plotting the CIs for both panels
segments(x0=c(handling_results$lb[which(handling_results$context=="heat" & handling_results$treatment=="blamedeflect")],
              handling_results$lb[which(handling_results$context=="heat" & handling_results$treatment=="blameclaim")],
              handling_results$lb[which(handling_results$context=="budget" & handling_results$treatment=="blamedeflect")],
              handling_results$lb[which(handling_results$context=="budget" & handling_results$treatment=="blameclaim")],
              handling_results$lb[which(handling_results$context=="bridge" & handling_results$treatment=="blamedeflect")],
              handling_results$lb[which(handling_results$context=="bridge" & handling_results$treatment=="blameclaim")],
              handling_results$lb[which(handling_results$context=="flood" & handling_results$treatment=="blamedeflect")],
              handling_results$lb[which(handling_results$context=="flood" & handling_results$treatment=="blameclaim")],
              vote_results$lb[which(vote_results$context=="heat" & vote_results$treatment=="blamedeflect")]+1,
              vote_results$lb[which(vote_results$context=="heat" & vote_results$treatment=="blameclaim")]+1,
              vote_results$lb[which(vote_results$context=="budget" & vote_results$treatment=="blamedeflect")]+1,
              vote_results$lb[which(vote_results$context=="budget" & vote_results$treatment=="blameclaim")]+1,
              vote_results$lb[which(vote_results$context=="bridge" & vote_results$treatment=="blamedeflect")]+1,
              vote_results$lb[which(vote_results$context=="bridge" & vote_results$treatment=="blameclaim")]+1,
              vote_results$lb[which(vote_results$context=="flood" & vote_results$treatment=="blamedeflect")]+1,
              vote_results$lb[which(vote_results$context=="flood" & vote_results$treatment=="blameclaim")]+1),
         x1=c(handling_results$ub[which(handling_results$context=="heat" & handling_results$treatment=="blamedeflect")],
              handling_results$ub[which(handling_results$context=="heat" & handling_results$treatment=="blameclaim")],
              handling_results$ub[which(handling_results$context=="budget" & handling_results$treatment=="blamedeflect")],
              handling_results$ub[which(handling_results$context=="budget" & handling_results$treatment=="blameclaim")],
              handling_results$ub[which(handling_results$context=="bridge" & handling_results$treatment=="blamedeflect")],
              handling_results$ub[which(handling_results$context=="bridge" & handling_results$treatment=="blameclaim")],
              handling_results$ub[which(handling_results$context=="flood" & handling_results$treatment=="blamedeflect")],
              handling_results$ub[which(handling_results$context=="flood" & handling_results$treatment=="blameclaim")],
              vote_results$ub[which(vote_results$context=="heat" & vote_results$treatment=="blamedeflect")]+1,
              vote_results$ub[which(vote_results$context=="heat" & vote_results$treatment=="blameclaim")]+1,
              vote_results$ub[which(vote_results$context=="budget" & vote_results$treatment=="blamedeflect")]+1,
              vote_results$ub[which(vote_results$context=="budget" & vote_results$treatment=="blameclaim")]+1,
              vote_results$ub[which(vote_results$context=="bridge" & vote_results$treatment=="blamedeflect")]+1,
              vote_results$ub[which(vote_results$context=="bridge" & vote_results$treatment=="blameclaim")]+1,
              vote_results$ub[which(vote_results$context=="flood" & vote_results$treatment=="blamedeflect")]+1,
              vote_results$ub[which(vote_results$context=="flood" & vote_results$treatment=="blameclaim")]+1),
         y0=rep(c(1,3,9,11,17,19,25,27),2))
# axis labels for bottom of figure
axis(1,at=c(-.4, -.3, -.2, -.1, 0, .1, .2, .3, .4), 
     labels = c("-40%", "-30%", "-20%", "-10%", "0%", "10%",
                "20%", "30%", "40%"), cex.axis = 1.5)
axis(1,at=c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4), 
     labels = c("-40%", "-30%", "-20%", "-10%", "0%", "10%",
                "20%", "30%", "40%"), cex.axis = 1.5)
par(mgp=c(0,8,0))
mtext("Difference from Control", side = 1, at=0, line = 3, cex = 2)
mtext("Difference from Control", side = 1, at=1, line = 3, cex = 2)
# axis labels for left of figure
axis(2,at=c(1,3,9,11,17,19,25,27),
     las=2,
     labels=rep(c("Blame Deflect", "Blame Claim"),4),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
par(mgp=c(0,11,0))
axis(2,at=c(5.25, 13.25, 21.25, 29.25),
     las=2,
     labels=c(expression(~bold(~underline("Heat Wave"))), 
              expression(~bold(~underline("Budget Shortfall"))), 
              expression(~bold(~underline("Bridge Collapse"))), 
              expression(~bold(~underline("Flood")))),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
# panel titles
mtext("Approval", side = 3, at=0, line=0.5, cex = 2.5)
mtext("Vote Choice", side = 3, at=1, line=0.5, cex = 2.5)
# plotting numerical effect estimates in figure
text(c(handling_results$point_est[which(handling_results$context=="heat" & handling_results$treatment=="blamedeflect")],
       handling_results$point_est[which(handling_results$context=="heat" & handling_results$treatment=="blameclaim")],
       handling_results$point_est[which(handling_results$context=="budget" & handling_results$treatment=="blamedeflect")],
       handling_results$point_est[which(handling_results$context=="budget" & handling_results$treatment=="blameclaim")],
       handling_results$point_est[which(handling_results$context=="bridge" & handling_results$treatment=="blamedeflect")],
       handling_results$point_est[which(handling_results$context=="bridge" & handling_results$treatment=="blameclaim")],
       handling_results$point_est[which(handling_results$context=="flood" & handling_results$treatment=="blamedeflect")],
       handling_results$point_est[which(handling_results$context=="flood" & handling_results$treatment=="blameclaim")],
       vote_results$point_est[which(vote_results$context=="heat" & vote_results$treatment=="blamedeflect")]+1,
       vote_results$point_est[which(vote_results$context=="heat" & vote_results$treatment=="blameclaim")]+1,
       vote_results$point_est[which(vote_results$context=="budget" & vote_results$treatment=="blamedeflect")]+1,
       vote_results$point_est[which(vote_results$context=="budget" & vote_results$treatment=="blameclaim")]+1,
       vote_results$point_est[which(vote_results$context=="bridge" & vote_results$treatment=="blamedeflect")]+1,
       vote_results$point_est[which(vote_results$context=="bridge" & vote_results$treatment=="blameclaim")]+1,
       vote_results$point_est[which(vote_results$context=="flood" & vote_results$treatment=="blamedeflect")]+1,
       vote_results$point_est[which(vote_results$context=="flood" & vote_results$treatment=="blameclaim")]+1), 
     c(1,3,9,11,17,19,25,27) + 0.9,
     paste0(sprintf("%.0f",round(c(handling_results$point_est[which(handling_results$context=="heat" & handling_results$treatment=="blamedeflect")],
                                   handling_results$point_est[which(handling_results$context=="heat" & handling_results$treatment=="blameclaim")],
                                   handling_results$point_est[which(handling_results$context=="budget" & handling_results$treatment=="blamedeflect")],
                                   handling_results$point_est[which(handling_results$context=="budget" & handling_results$treatment=="blameclaim")],
                                   handling_results$point_est[which(handling_results$context=="bridge" & handling_results$treatment=="blamedeflect")],
                                   handling_results$point_est[which(handling_results$context=="bridge" & handling_results$treatment=="blameclaim")],
                                   handling_results$point_est[which(handling_results$context=="flood" & handling_results$treatment=="blamedeflect")],
                                   handling_results$point_est[which(handling_results$context=="flood" & handling_results$treatment=="blameclaim")],
                                   vote_results$point_est[which(vote_results$context=="heat" & vote_results$treatment=="blamedeflect")],
                                   vote_results$point_est[which(vote_results$context=="heat" & vote_results$treatment=="blameclaim")],
                                   vote_results$point_est[which(vote_results$context=="budget" & vote_results$treatment=="blamedeflect")],
                                   vote_results$point_est[which(vote_results$context=="budget" & vote_results$treatment=="blameclaim")],
                                   vote_results$point_est[which(vote_results$context=="bridge" & vote_results$treatment=="blamedeflect")],
                                   vote_results$point_est[which(vote_results$context=="bridge" & vote_results$treatment=="blameclaim")],
                                   vote_results$point_est[which(vote_results$context=="flood" & vote_results$treatment=="blamedeflect")],
                                   vote_results$point_est[which(vote_results$context=="flood" & vote_results$treatment=="blameclaim")])*100, 0)),"%"), cex=1.75)

dev.off()

################################################################################
# Figure 2, Flint approval (left panel) and remain in office (right panel)

# linear models for both DVs

flint_handling_bi_model <- summary(lm(handling_bin ~ treatment, data = MTurk_flint_data))
flint_handling_conf_ints <- confint(lm(handling_bin ~ treatment, data = MTurk_flint_data))

flint_remain_model <- summary(lm(stay ~ treatment, data = MTurk_flint_data))
flint_remain_conf_ints <- confint(lm(stay ~ treatment, data = MTurk_flint_data))

pdf(file = , # enter your preferred location to save the figure here 
    family = "Times", height = 7, width=13)
par(mar=c(5.1,6,2.75,.5))
# plotting point estimates
plot(x=rev(c(flint_handling_bi_model$coefficients[4,1],
             flint_handling_bi_model$coefficients[2,1],
             flint_handling_bi_model$coefficients[3,1],
             flint_remain_model$coefficients[4,1]+0.8,
             flint_remain_model$coefficients[2,1]+0.8,
             flint_remain_model$coefficients[3,1]+0.8)), 
     y=rep(c(0.5, 1.5, 2.5),2),
     xlim=c(-.35,1.15),
     xaxt="n",
     ylim=c(0, 3),
     pch=19,
     tck=-.02,
     cex.axis=0.9,
     cex=1.5,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
abline(v=0.8,lwd=2, col="gray",lty=2)
# plotting CIs
segments(x0=rev(c(flint_handling_conf_ints[4,1],
                  flint_handling_conf_ints[2,1],
                  flint_handling_conf_ints[3,1],
                  flint_remain_conf_ints[4,1]+0.8,
                  flint_remain_conf_ints[2,1]+0.8,
                  flint_remain_conf_ints[3,1]+0.8)),
         x1=rev(c(flint_handling_conf_ints[4,2],
                  flint_handling_conf_ints[2,2],
                  flint_handling_conf_ints[3,2],
                  flint_remain_conf_ints[4,2]+0.8,
                  flint_remain_conf_ints[2,2]+0.8,
                  flint_remain_conf_ints[3,2]+0.8)),
         y0=rep(c(0.5, 1.5, 2.5),2))
# plotting bottom axis labels
axis(1,at=c(-.3, -.2, -.1, 0, .1, .2, .3), 
     labels = c("-30%", "-20%", "-10%", "0%", "10%",
                "20%", "30%"), cex.axis = 1.5)
axis(1,at=c(-.2), 
     labels = c("-20%"), cex.axis = 1.5)
axis(1,at=c(seq(0.5, 1.1, by=0.1)), 
     labels = c("-30%", "-20%", "-10%", "0%", "10%",
                "20%", "30%"), cex.axis = 1.5)
axis(1,at=c(0.6), 
     labels = c("-20%"), cex.axis = 1.5)
par(mgp=c(0,5,0))
mtext("Difference from Control", side = 1, at=0, line = 3, cex = 2)
mtext("Difference from Control", side = 1, at=0.8, line = 3, cex = 2)
# plotting left axis labels
axis(2,at=c(0.5, 1.5, 2.5),
     las=2,
     labels=rev(c("Blame\nClaim", "Blame\nAppointee", "Blame\nExpert")),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
# plotting panel titles
mtext("Approval", side = 3, at=0, line=0.5, cex = 2.5)
mtext("Remain in Office", side = 3, at=0.8, line=0.5, cex = 2.5)
# plotting numerical effect estimates in figure
text(rev(c(flint_handling_bi_model$coefficients[4,1],
           flint_handling_bi_model$coefficients[2,1],
           flint_handling_bi_model$coefficients[3,1],
           flint_remain_model$coefficients[4,1]+0.8,
           flint_remain_model$coefficients[2,1]+0.8,
           flint_remain_model$coefficients[3,1]+0.8)), 
     c(0.5, 1.5, 2.5) + 0.15,
     paste0(sprintf("%.0f",round(rev(c(flint_handling_bi_model$coefficients[4,1],
                                       flint_handling_bi_model$coefficients[2,1],
                                       flint_handling_bi_model$coefficients[3,1],
                                       flint_remain_model$coefficients[4,1],
                                       flint_remain_model$coefficients[2,1],
                                       flint_remain_model$coefficients[3,1]))*100, 0)),"%"), cex=1.75)
dev.off()

################################################################################
# Figure 3, effect of treatments on character valence 

# estimating linear models for each of the four governmental crises

flood_valence_bi_model <- lm(traits_scale ~ Floodresponse, 
                             data = TAPSdata, weights = jan2018wt1)
summary(flood_valence_bi_model)

BC_valence_bi_model <- lm(BCtraits ~ BCresponse, data=MTurk_stylized_data)
summary(BC_valence_bi_model)

BS_valence_bi_model <- lm(BStraits ~ BSresponse, data=MTurk_stylized_data)
summary(BS_valence_bi_model)

HW_valence_bi_model <- lm(HWtraits ~ HWresponse, data=MTurk_stylized_data)
summary(HW_valence_bi_model)

# creating an empty DF to store all necessary information for plotting

valence_results <- data.frame("context"=character(0), "treatment"=character(0),
                              "point_est"=numeric(0), "lb"=numeric(0), "ub"=numeric(0))

valence_results <- rbind(valence_results, data.frame("context"="flood", 
                                                     "treatment"="blameclaim", 
                                                     "point_est"=flood_valence_bi_model$coefficients[2],
                                                     "lb"=confint(flood_valence_bi_model)[2,1],
                                                     "ub"=confint(flood_valence_bi_model)[2,2]))

valence_results <- rbind(valence_results, data.frame("context"="flood", 
                                                     "treatment"="blamedeflect", 
                                                     "point_est"=flood_valence_bi_model$coefficients[3],
                                                     "lb"=confint(flood_valence_bi_model)[3,1],
                                                     "ub"=confint(flood_valence_bi_model)[3,2]))

valence_results <- rbind(valence_results, data.frame("context"="bridge", 
                                                     "treatment"="blameclaim", 
                                                     "point_est"=BC_valence_bi_model$coefficients[2],
                                                     "lb"=confint(BC_valence_bi_model)[2,1],
                                                     "ub"=confint(BC_valence_bi_model)[2,2]))

valence_results <- rbind(valence_results, data.frame("context"="bridge", 
                                                     "treatment"="blamedeflect", 
                                                     "point_est"=BC_valence_bi_model$coefficients[3],
                                                     "lb"=confint(BC_valence_bi_model)[3,1],
                                                     "ub"=confint(BC_valence_bi_model)[3,2]))

valence_results <- rbind(valence_results, data.frame("context"="budget", 
                                                     "treatment"="blameclaim", 
                                                     "point_est"=BS_valence_bi_model$coefficients[2],
                                                     "lb"=confint(BS_valence_bi_model)[2,1],
                                                     "ub"=confint(BS_valence_bi_model)[2,2]))

valence_results <- rbind(valence_results, data.frame("context"="budget", 
                                                     "treatment"="blamedeflect", 
                                                     "point_est"=BS_valence_bi_model$coefficients[3],
                                                     "lb"=confint(BS_valence_bi_model)[3,1],
                                                     "ub"=confint(BS_valence_bi_model)[3,2]))

valence_results <- rbind(valence_results, data.frame("context"="heat", 
                                                     "treatment"="blameclaim", 
                                                     "point_est"=HW_valence_bi_model$coefficients[2],
                                                     "lb"=confint(HW_valence_bi_model)[2,1],
                                                     "ub"=confint(HW_valence_bi_model)[2,2]))

valence_results <- rbind(valence_results, data.frame("context"="heat", 
                                                     "treatment"="blamedeflect", 
                                                     "point_est"=HW_valence_bi_model$coefficients[3],
                                                     "lb"=confint(HW_valence_bi_model)[3,1],
                                                     "ub"=confint(HW_valence_bi_model)[3,2]))

# Plotting Figure 3

pdf(file = , # enter your preferred location to save the figure here
    family = "Times", height = 7, width=8)
par(mar=c(5.1,12,2.75,0))
plot(x=c(valence_results$point_est[which(valence_results$context=="heat" & valence_results$treatment=="blamedeflect")],
         valence_results$point_est[which(valence_results$context=="heat" & valence_results$treatment=="blameclaim")],
         valence_results$point_est[which(valence_results$context=="budget" & valence_results$treatment=="blamedeflect")],
         valence_results$point_est[which(valence_results$context=="budget" & valence_results$treatment=="blameclaim")],
         valence_results$point_est[which(valence_results$context=="bridge" & valence_results$treatment=="blamedeflect")],
         valence_results$point_est[which(valence_results$context=="bridge" & valence_results$treatment=="blameclaim")],
         valence_results$point_est[which(valence_results$context=="flood" & valence_results$treatment=="blamedeflect")],
         valence_results$point_est[which(valence_results$context=="flood" & valence_results$treatment=="blameclaim")]), 
     y=c(1,3,9,11,17,19,25,27),
     xlim=c(-1.05,2.05),
     xaxt="n",
     ylim=c(0.5, 29.5),
     pch=19,
     tck=-.02,
     cex.axis=0.9,
     cex=1.5,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=c(valence_results$lb[which(valence_results$context=="heat" & valence_results$treatment=="blamedeflect")],
              valence_results$lb[which(valence_results$context=="heat" & valence_results$treatment=="blameclaim")],
              valence_results$lb[which(valence_results$context=="budget" & valence_results$treatment=="blamedeflect")],
              valence_results$lb[which(valence_results$context=="budget" & valence_results$treatment=="blameclaim")],
              valence_results$lb[which(valence_results$context=="bridge" & valence_results$treatment=="blamedeflect")],
              valence_results$lb[which(valence_results$context=="bridge" & valence_results$treatment=="blameclaim")],
              valence_results$lb[which(valence_results$context=="flood" & valence_results$treatment=="blamedeflect")],
              valence_results$lb[which(valence_results$context=="flood" & valence_results$treatment=="blameclaim")]),
         x1=c(valence_results$ub[which(valence_results$context=="heat" & valence_results$treatment=="blamedeflect")],
              valence_results$ub[which(valence_results$context=="heat" & valence_results$treatment=="blameclaim")],
              valence_results$ub[which(valence_results$context=="budget" & valence_results$treatment=="blamedeflect")],
              valence_results$ub[which(valence_results$context=="budget" & valence_results$treatment=="blameclaim")],
              valence_results$ub[which(valence_results$context=="bridge" & valence_results$treatment=="blamedeflect")],
              valence_results$ub[which(valence_results$context=="bridge" & valence_results$treatment=="blameclaim")],
              valence_results$ub[which(valence_results$context=="flood" & valence_results$treatment=="blamedeflect")],
              valence_results$ub[which(valence_results$context=="flood" & valence_results$treatment=="blameclaim")]),
         y0=c(1,3,9,11,17,19,25,27))
axis(1,at=c(seq(-1,2,by=0.5)), 
     labels = sprintf("%.2f",c(seq(-1,2,by=0.5))), cex.axis = 1.5)
par(mgp=c(0,9,0))
axis(2,at=c(1,3,9,11,17,19,25,27),
     las=2,
     labels=rep(c("Blame Deflect", "Blame Claim"),4),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
par(mgp=c(0,11,0))
axis(2,at=c(5.25, 13.25, 21.25, 29.25),
     las=2,
     labels=c(expression(~bold(~underline("Heat Wave"))), 
              expression(~bold(~underline("Budget Shortfall"))), 
              expression(~bold(~underline("Bridge Collapse"))), 
              expression(~bold(~underline("Flood")))),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Character Valence", side = 3, line=0.5, cex = 2.5)
text(c(valence_results$point_est[which(valence_results$context=="heat" & valence_results$treatment=="blamedeflect")],
       valence_results$point_est[which(valence_results$context=="heat" & valence_results$treatment=="blameclaim")],
       valence_results$point_est[which(valence_results$context=="budget" & valence_results$treatment=="blamedeflect")],
       valence_results$point_est[which(valence_results$context=="budget" & valence_results$treatment=="blameclaim")],
       valence_results$point_est[which(valence_results$context=="bridge" & valence_results$treatment=="blamedeflect")],
       valence_results$point_est[which(valence_results$context=="bridge" & valence_results$treatment=="blameclaim")],
       valence_results$point_est[which(valence_results$context=="flood" & valence_results$treatment=="blamedeflect")],
       valence_results$point_est[which(valence_results$context=="flood" & valence_results$treatment=="blameclaim")]), 
     c(1,3,9,11,17,19,25,27) + 1,
     sprintf("%.2f",round(c(valence_results$point_est[which(valence_results$context=="heat" & valence_results$treatment=="blamedeflect")],
                            valence_results$point_est[which(valence_results$context=="heat" & valence_results$treatment=="blameclaim")],
                            valence_results$point_est[which(valence_results$context=="budget" & valence_results$treatment=="blamedeflect")],
                            valence_results$point_est[which(valence_results$context=="budget" & valence_results$treatment=="blameclaim")],
                            valence_results$point_est[which(valence_results$context=="bridge" & valence_results$treatment=="blamedeflect")],
                            valence_results$point_est[which(valence_results$context=="bridge" & valence_results$treatment=="blameclaim")],
                            valence_results$point_est[which(valence_results$context=="flood" & valence_results$treatment=="blamedeflect")],
                            valence_results$point_est[which(valence_results$context=="flood" & valence_results$treatment=="blameclaim")]), 2)), cex=1.75)

dev.off()

################################################################################
# Figure 4, mediation analysis  for blame claim --> traits --> effects on outcomes

# mediation analysis for binary approval (TAPS flood),
# because of DK option, traits measure is binarized (see paper for details)

# are the traits outcomes internally consistent?
psych::alpha(cbind(TAPSdata$intelligent_bin, TAPSdata$honest_bin,
                   TAPSdata$trustworthy_bin, TAPSdata$leadership_bin,
                   TAPSdata$competent_bin))
# yes; standardized alpha at 0.93

TAPSdata$traits_scale <- TAPSdata$intelligent_bin + TAPSdata$honest_bin +
                        TAPSdata$trustworthy_bin + TAPSdata$leadership_bin +
                        TAPSdata$competent_bin

med.fit <- lm(traits_scale ~ Floodresponse, data = TAPSdata,
              weights = jan2018wt1)
summary(med.fit)
out.fit <- lm(handling_bin ~ Floodresponse + traits_scale, 
              data = TAPSdata, weights = jan2018wt1)
summary(out.fit)

med.out <- mediate(med.fit, out.fit, treat="Floodresponse", mediator="traits_scale", 
                   robustSE = FALSE, sims=1000,  control.value = "control", 
                   treat.value = "blame claim", boot=TRUE, dropobs = TRUE)

summary(med.out)

# average ACME
avg_ACME_handling_flood <- med.out$d.avg
avg_ACME_ci_handling_flood <- med.out$d.avg.ci
avg_ACME_p_handling_flood <- med.out$d.avg.p

avg_ADE_handling_flood <- med.out$z.avg
avg_ADE_ci_handling_flood <- med.out$z.avg.ci
avg_ADE_p_handling_flood <- med.out$z.avg.p

totaleff_handling_flood <- med.out$tau.coef
totaleff_ci_handling_flood <- med.out$tau.ci
totaleff_p_handling_flood <- med.out$tau.p

avg_propmed_handling_flood <- med.out$n.avg
avg_propmed_ci_handling_flood <- med.out$n.avg.ci
avg_propmed_p_handling_flood <- med.out$n.avg.p

#########################################
# mediation analysis for binary approval (MTurk BC)

# are the traits outcomes internally consistent?
psych::alpha(cbind(MTurk_stylized_data$BCintelligent, MTurk_stylized_data$BCcompetent,
                   MTurk_stylized_data$BCtrustworthy, MTurk_stylized_data$BChonest,
                   MTurk_stylized_data$BCleadership))
# yes; standardized alpha at 0.94

### mediation analysis--effect of blame claim mediated by traits
# mediation analysis for binary approval

MTurk_stylized_data$BCtraits <- (MTurk_stylized_data$BCintelligent + MTurk_stylized_data$BCcompetent + 
                             MTurk_stylized_data$BCtrustworthy + MTurk_stylized_data$BChonest + 
                             MTurk_stylized_data$BCleadership)/5

med.fit <- lm(BCtraits ~ BCresponse, data = MTurk_stylized_data)
out.fit <- lm(BCgovapprove_bin ~ BCresponse + BCtraits, 
              data = MTurk_stylized_data)

med.out <- mediate(med.fit, out.fit, treat="BCresponse", mediator="BCtraits", 
                   robustSE = FALSE, sims=1000,  control.value = "control", 
                   treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out)

# average ACME
avg_ACME_handling_BC <- med.out$d.avg
avg_ACME_ci_handling_BC <- med.out$d.avg.ci
avg_ACME_p_handling_BC <- med.out$d.avg.p

avg_ADE_handling_BC <- med.out$z.avg
avg_ADE_ci_handling_BC <- med.out$z.avg.ci
avg_ADE_p_handling_BC <- med.out$z.avg.p

totaleff_handling_BC <- med.out$tau.coef
totaleff_ci_handling_BC <- med.out$tau.ci
totaleff_p_handling_BC <- med.out$tau.p

avg_propmed_handling_BC <- med.out$n.avg
avg_propmed_ci_handling_BC <- med.out$n.avg.ci
avg_propmed_p_handling_BC <- med.out$n.avg.p

#########################################
# mediation analysis for binary approval (MTurk BS)

# are the traits outcomes internally consistent?
psych::alpha(cbind(MTurk_stylized_data$BSintelligent, MTurk_stylized_data$BScompetent,
                   MTurk_stylized_data$BStrustworthy, MTurk_stylized_data$BShonest,
                   MTurk_stylized_data$BSleadership))
# yes; standardized alpha at 0.94

### mediation analysis--effect of blame claim mediated by traits
# mediation analysis for binary approval

MTurk_stylized_data$BStraits <- (MTurk_stylized_data$BSintelligent + MTurk_stylized_data$BScompetent + 
                             MTurk_stylized_data$BStrustworthy + MTurk_stylized_data$BShonest + 
                             MTurk_stylized_data$BSleadership)/5

med.fit <- lm(BStraits ~ BSresponse, data = MTurk_stylized_data)
out.fit <- lm(BSgovapprove_bin ~ BSresponse + BStraits, 
              data = MTurk_stylized_data)

med.out <- mediate(med.fit, out.fit, treat="BSresponse", mediator="BStraits", 
                   robustSE = FALSE, sims=1000,  control.value = "control", 
                   treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out)

# average ACME
avg_ACME_handling_BS <- med.out$d.avg
avg_ACME_ci_handling_BS <- med.out$d.avg.ci
avg_ACME_p_handling_BS <- med.out$d.avg.p

avg_ADE_handling_BS <- med.out$z.avg
avg_ADE_ci_handling_BS <- med.out$z.avg.ci
avg_ADE_p_handling_BS <- med.out$z.avg.p

totaleff_handling_BS <- med.out$tau.coef
totaleff_ci_handling_BS <- med.out$tau.ci
totaleff_p_handling_BS <- med.out$tau.p

avg_propmed_handling_BS <- med.out$n.avg
avg_propmed_ci_handling_BS <- med.out$n.avg.ci
avg_propmed_p_handling_BS <- med.out$n.avg.p

#########################################
# mediation analysis for binary approval (MTurk HW)

# are the traits outcomes internally consistent?
psych::alpha(cbind(MTurk_stylized_data$HWintelligent, MTurk_stylized_data$HWcompetent,
                   MTurk_stylized_data$HWtrustworthy, MTurk_stylized_data$HWhonest,
                   MTurk_stylized_data$HWleadership))
# yes; standardized alpha at 0.94

### mediation analysis--effect of blame claim mediated by traits
# mediation analysis for binary approval

MTurk_stylized_data$HWtraits <- (MTurk_stylized_data$HWintelligent + MTurk_stylized_data$HWcompetent + 
                             MTurk_stylized_data$HWtrustworthy + MTurk_stylized_data$HWhonest + 
                             MTurk_stylized_data$HWleadership)/5

med.fit <- lm(HWtraits ~ HWresponse, data = MTurk_stylized_data)
out.fit <- lm(HWgovapprove_bin ~ HWresponse + HWtraits, 
              data = MTurk_stylized_data)

med.out <- mediate(med.fit, out.fit, treat="HWresponse", mediator="HWtraits", 
                   robustSE = FALSE, sims=1000,  control.value = "control", 
                   treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out)

# average ACME
avg_ACME_handling_HW <- med.out$d.avg
avg_ACME_ci_handling_HW <- med.out$d.avg.ci
avg_ACME_p_handling_HW <- med.out$d.avg.p

avg_ADE_handling_HW <- med.out$z.avg
avg_ADE_ci_handling_HW <- med.out$z.avg.ci
avg_ADE_p_handling_HW <- med.out$z.avg.p

totaleff_handling_HW <- med.out$tau.coef
totaleff_ci_handling_HW <- med.out$tau.ci
totaleff_p_handling_HW <- med.out$tau.p

avg_propmed_handling_HW <- med.out$n.avg
avg_propmed_ci_handling_HW <- med.out$n.avg.ci
avg_propmed_p_handling_HW <- med.out$n.avg.p

############################################################

# mediation analysis for binary vote (TAPS flood),
# collapse dont know and disapprove

med.fit <- lm(traits_scale ~ Floodresponse, data = TAPSdata,
              weights = jan2018wt1)
summary(med.fit)
out.fit <- lm(vote_bin ~ Floodresponse + traits_scale, 
              data = TAPSdata, weights = jan2018wt1)
summary(out.fit)

### mediation analysis--effect of blame claim mediated by traits
# mediation analysis for vote choice

med.out <- mediate(med.fit, out.fit, treat="Floodresponse", mediator="traits_scale", 
                   robustSE = FALSE, sims=1000,  control.value = "control", 
                   treat.value = "blame claim", boot=TRUE, dropobs = TRUE)

summary(med.out)

# average ACME
avg_ACME_vote_next_flood <- med.out$d.avg
avg_ACME_ci_vote_next_flood <- med.out$d.avg.ci
avg_ACME_p_vote_next_flood <- med.out$d.avg.p

avg_ADE_vote_next_flood <- med.out$z.avg
avg_ADE_ci_vote_next_flood <- med.out$z.avg.ci
avg_ADE_p_vote_next_flood <- med.out$z.avg.p

totaleff_vote_next_flood <- med.out$tau.coef
totaleff_ci_vote_next_flood <- med.out$tau.ci
totaleff_p_vote_next_flood <- med.out$tau.p

avg_propmed_vote_next_flood <- med.out$n.avg
avg_propmed_ci_vote_next_flood <- med.out$n.avg.ci
avg_propmed_p_vote_next_flood <- med.out$n.avg.p

#########################################
# mediation analysis for vote choice (MTurk BC)

# are the traits outcomes internally consistent?
psych::alpha(cbind(MTurk_stylized_data$BCintelligent, MTurk_stylized_data$BCcompetent,
                   MTurk_stylized_data$BCtrustworthy, MTurk_stylized_data$BChonest,
                   MTurk_stylized_data$BCleadership))
# yes; standardized alpha at 0.94

### mediation analysis--effect of blame claim mediated by traits
# mediation analysis for vote choice

MTurk_stylized_data$BCtraits <- (MTurk_stylized_data$BCintelligent + MTurk_stylized_data$BCcompetent + 
                             MTurk_stylized_data$BCtrustworthy + MTurk_stylized_data$BChonest + 
                             MTurk_stylized_data$BCleadership)/5

med.fit <- lm(BCtraits ~ BCresponse, data = MTurk_stylized_data)
out.fit <- lm(BCgovvote_bin ~ BCresponse + BCtraits, 
              data = MTurk_stylized_data)

med.out <- mediate(med.fit, out.fit, treat="BCresponse", mediator="BCtraits", 
                   robustSE = FALSE, sims=1000,  control.value = "control", 
                   treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out)

# average ACME
avg_ACME_vote_next_BC <- med.out$d.avg
avg_ACME_ci_vote_next_BC <- med.out$d.avg.ci
avg_ACME_p_vote_next_BC <- med.out$d.avg.p

avg_ADE_vote_next_BC <- med.out$z.avg
avg_ADE_ci_vote_next_BC <- med.out$z.avg.ci
avg_ADE_p_vote_next_BC <- med.out$z.avg.p

totaleff_vote_next_BC <- med.out$tau.coef
totaleff_ci_vote_next_BC <- med.out$tau.ci
totaleff_p_vote_next_BC <- med.out$tau.p

avg_propmed_vote_next_BC <- med.out$n.avg
avg_propmed_ci_vote_next_BC <- med.out$n.avg.ci
avg_propmed_p_vote_next_BC <- med.out$n.avg.p

#########################################
# mediation analysis for vote choice (MTurk BS)

# are the traits outcomes internally consistent?
psych::alpha(cbind(MTurk_stylized_data$BSintelligent, MTurk_stylized_data$BScompetent,
                   MTurk_stylized_data$BStrustworthy, MTurk_stylized_data$BShonest,
                   MTurk_stylized_data$BSleadership))
# yes; standardized alpha at 0.94

### mediation analysis--effect of blame claim mediated by traits
# mediation analysis for vote choice

MTurk_stylized_data$BStraits <- (MTurk_stylized_data$BSintelligent + MTurk_stylized_data$BScompetent + 
                             MTurk_stylized_data$BStrustworthy + MTurk_stylized_data$BShonest + 
                             MTurk_stylized_data$BSleadership)/5

med.fit <- lm(BStraits ~ BSresponse, data = MTurk_stylized_data)
out.fit <- lm(BSgovvote_bin ~ BSresponse + BStraits, 
              data = MTurk_stylized_data)

med.out <- mediate(med.fit, out.fit, treat="BSresponse", mediator="BStraits", 
                   robustSE = FALSE, sims=1000,  control.value = "control", 
                   treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out)

# average ACME
avg_ACME_vote_next_BS <- med.out$d.avg
avg_ACME_ci_vote_next_BS <- med.out$d.avg.ci
avg_ACME_p_vote_next_BS <- med.out$d.avg.p

avg_ADE_vote_next_BS <- med.out$z.avg
avg_ADE_ci_vote_next_BS <- med.out$z.avg.ci
avg_ADE_p_vote_next_BS <- med.out$z.avg.p

totaleff_vote_next_BS <- med.out$tau.coef
totaleff_ci_vote_next_BS <- med.out$tau.ci
totaleff_p_vote_next_BS <- med.out$tau.p

avg_propmed_vote_next_BS <- med.out$n.avg
avg_propmed_ci_vote_next_BS <- med.out$n.avg.ci
avg_propmed_p_vote_next_BS <- med.out$n.avg.p

#########################################
# mediation analysis for vote choice (MTurk HW)

# are the traits outcomes internally consistent?
psych::alpha(cbind(MTurk_stylized_data$HWintelligent, MTurk_stylized_data$HWcompetent,
                   MTurk_stylized_data$HWtrustworthy, MTurk_stylized_data$HWhonest,
                   MTurk_stylized_data$HWleadership))
# yes; standardized alpha at 0.94

### mediation analysis--effect of blame claim mediated by traits
# mediation analysis for vote choice

MTurk_stylized_data$HWtraits <- (MTurk_stylized_data$HWintelligent + MTurk_stylized_data$HWcompetent + 
                             MTurk_stylized_data$HWtrustworthy + MTurk_stylized_data$HWhonest + 
                             MTurk_stylized_data$HWleadership)/5

med.fit <- lm(HWtraits ~ HWresponse, data = MTurk_stylized_data)
out.fit <- lm(HWgovvote_bin ~ HWresponse + HWtraits, 
              data = MTurk_stylized_data)

med.out <- mediate(med.fit, out.fit, treat="HWresponse", mediator="HWtraits", 
                   robustSE = FALSE, sims=1000,  control.value = "control", 
                   treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out)

# average ACME
avg_ACME_vote_next_HW <- med.out$d.avg
avg_ACME_ci_vote_next_HW <- med.out$d.avg.ci
avg_ACME_p_vote_next_HW <- med.out$d.avg.p

avg_ADE_vote_next_HW <- med.out$z.avg
avg_ADE_ci_vote_next_HW <- med.out$z.avg.ci
avg_ADE_p_vote_next_HW <- med.out$z.avg.p

totaleff_vote_next_HW <- med.out$tau.coef
totaleff_ci_vote_next_HW <- med.out$tau.ci
totaleff_p_vote_next_HW <- med.out$tau.p

avg_propmed_vote_next_HW <- med.out$n.avg
avg_propmed_ci_vote_next_HW <- med.out$n.avg.ci
avg_propmed_p_vote_next_HW <- med.out$n.avg.p

# Figure 4

pdf(file = , # enter your preferred location to save the figure here
    family = "Times", height = 9.5, width=13)
par(mar=c(5.1,10.25,2.75,.5))
plot(x=c(totaleff_handling_HW, avg_ADE_handling_HW, avg_ACME_handling_HW,
         totaleff_handling_BS, avg_ADE_handling_BS, avg_ACME_handling_BS,
         totaleff_handling_BC, avg_ADE_handling_BC, avg_ACME_handling_BC,
         totaleff_handling_flood, avg_ADE_handling_flood, avg_ACME_handling_flood,
         totaleff_vote_next_HW+0.7, avg_ADE_vote_next_HW+0.7, avg_ACME_vote_next_HW+0.7,
         totaleff_vote_next_BS+0.7, avg_ADE_vote_next_BS+0.7, avg_ACME_vote_next_BS+0.7,
         totaleff_vote_next_BC+0.7, avg_ADE_vote_next_BC+0.7, avg_ACME_vote_next_BC+0.7,
         totaleff_vote_next_flood+0.7, avg_ADE_vote_next_flood+0.7, avg_ACME_vote_next_flood+0.7), 
     y=rep(c(seq(1,5, by=2), seq(11,15, by=2), seq(21,25, by=2), seq(31,35, by=2)),2),
     xlim=c(-.15,1.15),
     xaxt="n",
     ylim=c(0.5,38.5),
     pch=19,
     tck=-.02,
     cex.axis=0.9,
     cex=1.5,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
abline(v=0.7,lwd=2, col="gray",lty=2)
segments(x0=c(totaleff_ci_handling_HW[1], avg_ADE_ci_handling_HW[1], avg_ACME_ci_handling_HW[1],
              totaleff_ci_handling_BS[1], avg_ADE_ci_handling_BS[1], avg_ACME_ci_handling_BS[1],
              totaleff_ci_handling_BC[1], avg_ADE_ci_handling_BC[1], avg_ACME_ci_handling_BC[1],
              totaleff_ci_handling_flood[1], avg_ADE_ci_handling_flood[1], avg_ACME_ci_handling_flood[1],
              totaleff_ci_vote_next_HW[1]+0.7, avg_ADE_ci_vote_next_HW[1]+0.7, avg_ACME_ci_vote_next_HW[1]+0.7,
              totaleff_ci_vote_next_BS[1]+0.7, avg_ADE_ci_vote_next_BS[1]+0.7, avg_ACME_ci_vote_next_BS[1]+0.7,
              totaleff_ci_vote_next_BC[1]+0.7, avg_ADE_ci_vote_next_BC[1]+0.7, avg_ACME_ci_vote_next_BC[1]+0.7,
              totaleff_ci_vote_next_flood[1]+0.7, avg_ADE_ci_vote_next_flood[1]+0.7, avg_ACME_ci_vote_next_flood[1]+0.7),
         x1=c(totaleff_ci_handling_HW[2], avg_ADE_ci_handling_HW[2], avg_ACME_ci_handling_HW[2],
              totaleff_ci_handling_BS[2], avg_ADE_ci_handling_BS[2], avg_ACME_ci_handling_BS[2],
              totaleff_ci_handling_BC[2], avg_ADE_ci_handling_BC[2], avg_ACME_ci_handling_BC[2],
              totaleff_ci_handling_flood[2], avg_ADE_ci_handling_flood[2], avg_ACME_ci_handling_flood[2],
              totaleff_ci_vote_next_HW[2]+0.7, avg_ADE_ci_vote_next_HW[2]+0.7, avg_ACME_ci_vote_next_HW[2]+0.7,
              totaleff_ci_vote_next_BS[2]+0.7, avg_ADE_ci_vote_next_BS[2]+0.7, avg_ACME_ci_vote_next_BS[2]+0.7,
              totaleff_ci_vote_next_BC[2]+0.7, avg_ADE_ci_vote_next_BC[2]+0.7, avg_ACME_ci_vote_next_BC[2]+0.7,
              totaleff_ci_vote_next_flood[2]+0.7, avg_ADE_ci_vote_next_flood[2]+0.7, avg_ACME_ci_vote_next_flood[2]+0.7),
         y0=rep(c(seq(1,5, by=2), seq(11,15, by=2), seq(21,25, by=2), seq(31,35, by=2)),2))
axis(1,at=c(-.1, 0, .1, .2, .3, .4), 
     labels = c("-10%", "0%", "10%",
                "20%", "30%", "40%"), cex.axis = 1.75)
axis(1,at=c(.6, .7, .8, .9, 1, 1.1), 
     labels = c("-10%", "0%", "10%",
                "20%", "30%", "40%"), cex.axis = 1.75)
par(mgp=c(0,6,0))
axis(2,at=c(seq(1,5, by=2), seq(11,15, by=2), seq(21,25, by=2), seq(31,35, by=2)),
     las=2,
     labels=rep(c("Total Effect", "ADE", "ACME"),4),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
par(mgp=c(0,10,0))
axis(2,at=c(7.25, 17.25, 27.25, 37.25),
     las=2,
     labels=c(expression(~bold(~underline("Heat Wave"))), 
              expression(~bold(~underline("Budget Shortfall"))), 
              expression(~bold(~underline("Bridge Collapse"))), 
              expression(~bold(~underline("Flood")))),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
mtext("Difference from Control", side = 1, at=0.15, line = 3, cex = 2)
mtext("Difference from Control", side = 1, at=0.85, line = 3, cex = 2)
mtext("Approval", side = 3, cex = 2.5, at=0.15, line=0.5)
mtext("Vote Choice", side = 3, cex = 2.5, at=0.85, line=0.5)
text(c(totaleff_handling_HW, avg_ADE_handling_HW, avg_ACME_handling_HW,
       totaleff_handling_BS, avg_ADE_handling_BS, avg_ACME_handling_BS,
       totaleff_handling_BC, avg_ADE_handling_BC, avg_ACME_handling_BC,
       totaleff_handling_flood, avg_ADE_handling_flood, avg_ACME_handling_flood,
       totaleff_vote_next_HW+0.7, avg_ADE_vote_next_HW+0.7, avg_ACME_vote_next_HW+0.7,
       totaleff_vote_next_BS+0.7, avg_ADE_vote_next_BS+0.7, avg_ACME_vote_next_BS+0.7,
       totaleff_vote_next_BC+0.7, avg_ADE_vote_next_BC+0.7, avg_ACME_vote_next_BC+0.7,
       totaleff_vote_next_flood+0.7, avg_ADE_vote_next_flood+0.7, avg_ACME_vote_next_flood+0.7), 
     c(seq(1,5, by=2), seq(11,15, by=2), seq(21,25, by=2), seq(31,35, by=2))+0.8,
     paste0(sprintf("%.0f",round(c(totaleff_handling_HW, avg_ADE_handling_HW, avg_ACME_handling_HW,
                                   totaleff_handling_BS, avg_ADE_handling_BS, avg_ACME_handling_BS,
                                   totaleff_handling_BC, avg_ADE_handling_BC, avg_ACME_handling_BC,
                                   totaleff_handling_flood, avg_ADE_handling_flood, avg_ACME_handling_flood,
                                   totaleff_vote_next_HW, avg_ADE_vote_next_HW, avg_ACME_vote_next_HW,
                                   totaleff_vote_next_BS, avg_ADE_vote_next_BS, avg_ACME_vote_next_BS,
                                   totaleff_vote_next_BC, avg_ADE_vote_next_BC, avg_ACME_vote_next_BC,
                                   totaleff_vote_next_flood, avg_ADE_vote_next_flood, avg_ACME_vote_next_flood)*100, 0)),"%"), cex=1.75)
dev.off()