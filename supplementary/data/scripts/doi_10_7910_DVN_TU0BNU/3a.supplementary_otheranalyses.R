#This file does extra analyses. These are not robustness checks. 
rm(list=ls())
library(tidyverse)
library(rstanarm)

options(mc.cores = 4)
setwd("~/Documents/Projects/Non-RAND Projects/Police Shootings")
load("Data/Bayesian_models_main.RData")

#1. Marginal predictive checks (calibration)----
plot.dat <- data.frame(y = rep(df$n_shootings,5), 
                       y_hat = c(M1$fitted.values, M2$fitted.values, M3$fitted.values, M4$fitted.values, M5$fitted.values),
                       Model = rep(c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), each = nrow(df))) 

g <- ggplot(aes(x=y_hat,y=y), data=plot.dat) + geom_point(alpha=.25, position = "jitter") + 
  theme_bw() + ylab("Y (Observed)") +xlab("Y (Fitted)") + scale_x_sqrt() + scale_y_sqrt() +
  geom_abline(intercept = 0, slope=1, linetype="dashed", size=1.5) + facet_wrap(~Model, nrow=1)
ggsave("Output/Model Checks/Other Supplementary/calibration_plots_sqrt.jpeg", plot= g, device="jpeg", width = 12, height = 3)

#2. Regression coefficients (for Model 5)----
M5sims <- as.matrix(M5)
dat <- data.frame(M5sims) %>% select(!starts_with("b..") & ! starts_with("Sigma")  & !starts_with("year"))
m <- apply(dat,2,median)
q <- apply(dat,2,function(f){quantile(f, probs = c(0.025,.975))})
q2 <- apply(dat,2,function(f){quantile(f, probs = c(0.25,.75))})
var_name <- names(dat)
var_name2 <- c("Intercept", "Black Composition", "Hispanic Composition",
               "Youth Composition", "Foreign Born Composition", "SSI Usage",
               "Female-headed Households", "Income", "Low Education",
               "High Education", "Unemployment", "Family Poverty",
               "Residential Mobility", "Household Size", "Vacancy Rate",
               "Owner Occupied", "Murder Q2", "Murder Q3", "Murder Q4", "Murder Q5",
               "Violent Q2", "Violent Q3", "Violent Q4", "Violent Q5", "Violent Q6", "Violent Q7", "Violent Q8", "Violent Q9", "Violent Q10",
               "Gun Prevalence", "Gun Assault Q2", "Gun Assault Q3","Gun Assault Q4","Gun Assault Q5","Gun Assault Q6","Gun Assault Q7",
               "Property Q2", "Property Q3", "Property Q4", "Property Q5", "Property Q6", "Property Q7", "Property Q8", "Property Q9", "Property Q10",
               "Calls to Service Q2", "Calls to Service Q3", "Calls to Service Q4", "Calls to Service Q5", "Calls to Service Q6", "Calls to Service Q7", "Calls to Service Q8", "Calls to Service Q9", "Calls to Service Q10",
               "Officers Rate Q2", "Officers Rate Q3", "Officers Rate Q4", "Officers Rate Q5", "Officers Rate Q6", "Officers Rate Q7", "Officers Rate Q8", "Officers Rate Q9", "Officers Rate Q10",
               "Investigate Use of Force Injury", "Investigate Use of Force Deaths", "Investigate Other Deaths",
               "Investigate Use of Firearm", "CCRB","Computer System",
               "Blunt Projectile Permitted-All", "Blunt Projectile Permitted-Some", "Taser Permitted-All",
               "Taser Permitted-Some", "Union Authorized-Some", "Union Authorized-None",
               "Policy of Vehicle Pursuit", "Document Displaying Gun-Yes", "Document Displaying Gun-No",
               "Document Discharging Gun-Yes", "Document Discharging Gun-No", "Percent Male Officers",
               "White Overrepresentation", "Educational Requirement-Medium", "Educational Requirement-Low",
               "Body-worn Cameras", "Training Time Academy Q2", "Training Time Academy Q3",
               "Training Time Field Recruits Q2", "Training Time Field Recruits Q3", "Training Time In-service Officers Q2",
               "Training Time In-service Officers Q3", "Background Checks", "Community Policing", "Police Intensity"
               )



#Tabulate:
tab.dat <- data.frame(var_name, B = round(m,3), lb = round(q[1,],3), ub = round(q[2,],3))
rownames(tab.dat) <- NULL

#plot:
plot.dat <- data.frame(m, lb = q[1,], ub = q[2,], lb2 = q2[1,], ub2 = q2[2,], var = var_name2) 
plot.dat$var <- factor(plot.dat$var, levels = rev(var_name2))
M5plot <- ggplot(aes(x=m, y=var), data=plot.dat) +  geom_vline(xintercept = 0, size=1, linetype = "dashed") + 
  geom_linerange(aes(xmin=lb, xmax=ub), size=.3, color="red") +
  geom_linerange(aes(xmin=lb2, xmax=ub2), size=.5, color="darkred") +
  geom_point(color="darkred") + 
  theme_bw() + xlab("Beta") + ylab("") 
ggsave("Output/Model Checks/Other Supplementary/M5_coefficients.jpeg", plot= M5plot, device="jpeg", width = 8, height = 12)

#3. How many fewer African Americans killed if shootings were lower overall?----
shoots <- read_dta("Data/shootings_clean_long.dta")
table(shoots$race)

table(shoots$race)
b <- 1503/(nrow(shoots)-510) #current fraction of shootings. minus the 510 with no race info
w <- .121 #the NH black fraction of population (w doesn't mean white here)

x <- seq(0,1,length.out=101)
y <- seq(w,b,length.out=101)
y2 <- seq(0,1,length.out=101)
y_link <- data.frame(y,y2)

gr <- expand_grid(x, y)  
gr$z <- gr$x*((nrow(shoots)-510)/6)*gr$y
gr <- left_join(gr,y_link, by="y")  

p <- ggplot(gr, aes(x=x, y=y2, fill= z)) + 
  geom_tile()  + theme_minimal() + xlab("Fraction of Current Shootings") +
  ylab("Fraction of Current Disparaties") +
  scale_fill_viridis_c(option = "magma") +
  guides(fill=guide_legend(title="# Black \nFatalities"))

ggsave("Output/Model Checks/Other Supplementary/black_fatalities.jpeg", plot= p, device="jpeg", width = 6, height = 5.5)


