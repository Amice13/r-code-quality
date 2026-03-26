
## This command file estimates the prevalence of 
## sexual violence using direct survey estimates
## and list experiment estimates. This file creates
## Figure 2 in the Main Paper (which reports those estimates)
## and includes code to export Figure 2 

# this file is designed to run after the following files
# uncomment to run previous coding files
# source("00_Packages.R")
# source("01_DataVariables.R")

set.seed(20200722)

# Part 1. Estimation  -------------------------------------------------------

## 1. Direct Estimate using regression approach (used in Figure 2 of Main Paper)
est_d1 <- lm(rape_yes~1, data=D) #basic

## 2. List Estimate using regression approach (run as a check)
est_i1 <- lm(list1_CRSV~treat_list_CRSV, data=D)

## 3. Estimate (linear) using list package (Blair and Imai) (used in Figure 2 of Main Paper)
diff.in.means.results <- ictreg(list1_CRSV ~ 1, data = D.org,
                                treat = "treat_list_CRSV", J=3, method = "lm")
summary(diff.in.means.results)

#########################################################################
# COEFFICIENT PLOT FOR ESTIMATES (Figure 2)

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

estm1 <- data.frame(Estimate = c("Rape (Direct)"),
                    Coefficient = c(summary(est_d1)$coefficients[,1]),
                    SE = c(summary(est_d1)$coefficients[,2]),
                    modelName = "Direct")

estm2 <- data.frame(Estimate =  c("Rape (List)"),
                    Coefficient = c(unlist(diff.in.means.results$par.treat)),
                    SE = c(unlist(diff.in.means.results$se.treat)),
                    modelName = "List")


est.models <- data.frame(rbind(estm1, estm2))

estplot <- ggplot(est.models, aes(colour = modelName)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = Estimate, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = Estimate, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  geom_text(aes(x=Estimate,y=Coefficient, label=as.character(round(Coefficient,2))), colour="gray20", alpha=1.5, size=3, nudge_x = .25, nudge_y = 0, check_overlap = T) + 
  theme(legend.position="top")+
  coord_flip() + theme_bw() + theme(legend.position = "top")+ scale_color_manual(values=c('#fdbb84','#2c7fb8')) +
  theme(legend.title=element_blank())


# VIEW Figure 2 in Main Paper

estplot    # fig 2

## EXPORT Figure 2 in Main Paper

jpeg("Figure2_Estimates.jpeg", units="in", width=5, height=2, res=300)
estplot
dev.off()