## This file conducts a sensitivity analysis
## examining whether our findings of
## the effects of CRSV on socio-politial
## outcomes are robust to the potential 
## of ceiling and floor effects. 
## The file generates figure A3
## of the supplementary appendix
## and the approach is described in 
## Appendix section 9.3.4
## It also calculates the share of
## Potential potential non disclosers
## As reported in Appendix section 9.3.3


rm(list=ls())

source("00_Packages.R")
source("01_DataVariables.R")

#install.packages("arm")
library(arm)

####### risk factors
# risk_factors <- ictreg(list1_CRSV ~ vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + 
#                          exchange_prev + as.factor(territoire),  
#                        treat="treat_list_CRSV", 
#                        constrained=TRUE,
#                        J=3, data=D)

D$territoire.f <- as.factor(D$territoire)
risk_factors <- ictreg(list1_CRSV ~ female + age + edu_level + hh_size + assets_sum + territoire.f, 
                       treat = "treat_list_CRSV",
                       J=3,
                       data=D,
                       constrained = TRUE)
summary(risk_factors)

############

# Our main model control variables for your reference
# controls.wfes <- "vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + exchange_prev + as.factor(territoire)"

# other important note on datasets.
# main dataset is D, but for all ictreg ictregjoint analyses, 
# we use datasets with no NAs. These vary by variable 
# so we don't have to lose data if we don't need to

# Outcome 1: Personal exchanges / visit
# D.vis, OUT="ingroup_visit_mean"

# Outcome 2: Civic Leadership
# D.org, OUT="org_leader_d"

# Outcome 3: Civic Engagement org membership
# D.mem, OUT="org_member_r1"

# Outcome 4: Cooperative Behavior - Engagement in events
# D.eve, OUT="event_com_mean"

# Outcome 5: Public Goods Contribution
# D.don, OUT="donate_amount"


###############################################################################################
# Assess share of potential liars
###############################################################################################

# Run baseline model
m.liar <- ictreg(list1_CRSV ~ 1, treat="treat_list_CRSV", J=3, data=D) # Baseline Model
summary(m.liar)


# Run floor model
m.liar.2 <- ictreg(list1_CRSV ~ 1, treat="treat_list_CRSV", J=3, data=D, floor=TRUE)
summary(m.liar.2)

# Run ceiling model
m.liar.3 <- ictreg(list1_CRSV ~ 1, treat="treat_list_CRSV", J=3, data=D, ceiling=TRUE, ceiling.fit = "bayesglm")
summary(m.liar.3)

# Share of Potential Liars in floor model based on predicted probabilities (invlogit)
share.floor <- invlogit(m.liar.2$par.treat)-invlogit(m.liar$par.treat) 
share.floor

# Share of Potential Liars in ceiling model
share.ceiling <- invlogit(m.liar.3$par.treat)-invlogit(m.liar$par.treat) 
share.ceiling

# Take whatever share (ceiling or floor) is higher and use that for the simulation (in this case ca. 3.5%)
share.sim <- ifelse(share.ceiling > share.floor, share.ceiling, share.floor) 
share.sim

###############################################################################################
# Simulation of liar effects
###############################################################################################

# Outer Loop starts here to go through all the outcome variables
for(var in c("ingroup_visit_mean_norm", "event_com_mean_norm", "org_member_r1_norm", "donate_amount_norm", "org_leader_d")) {
  
  # Set up empty container to store sim results in
  sim.coefs.congo <- matrix(NA, 1000, 15)
  
  # Inner loop performs 1000 estimations of the specification and randomly assigns the share of liars in each iteration
  for(i in 1:1000){
    
    D$liar <-  rbinom(1000, 1, share.sim) # Randomly simulate potential liars
    D$wsv.sim <- ifelse(D$liar==1 & D$treat_list_CRSV==1 & D$list1_CRSV!=4, D$list1_CRSV+1, D$list1_CRSV)
    
    try(m.out.1.a <- ictreg.joint(wsv.sim ~ vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + 
                                    exchange_prev + as.factor(territoire),  
                                  treat="treat_list_CRSV", 
                                  outcome=var,
                                  outcome.reg="linear",
                                  constrained=TRUE,
                                  J=3, data=D), silent=TRUE) # Run model with simulated liars
    
    sim.coefs.congo[i,]  <- m.out.1.a$par.outcome # Store model results
    print(i)
    
  } # end of inner loop
  
  # Save the simulation results
  saveRDS(sim.coefs.congo, paste("sim.", var, ".rds"))
  #saveRDS(sim.ses.congo, "sim.ses.congo.rds")
  
} # end of outer loop through outcome vars

###############################################################################################
# Produce figures that (a) display the distribution of the simulated effects, (b) the averaged
# simulated effect size, and (c) the original estimate to compare it to
###############################################################################################

# Start loop
for(var in c("ingroup_visit_mean_norm", "event_com_mean_norm", "org_member_r1_norm", "donate_amount_norm", "org_leader_d")) {
  
  # read saved results  
  simulated_result <- readRDS(paste("sim.", var, ".rds"))
  simulated_result <- data.frame(simulated_result)
  simulated_result$X15
  simulated_result.avg <- mean(simulated_result$X15)
  #simulated_result.avg <- apply(simulated_result,2,mean)
  #simulated_result.avg <- simulated_result$X15
  #class(simulated_result.avg)
  #simulated_result.avg <- as.numeric(unlist(simulated_result.avg))
  
  # Run baseline model ictreg without set share of liars (will be used to display original estimate)
  set.seed(20210518)
  list.result <- ictreg.joint(list1_CRSV ~ vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + 
                                exchange_prev + as.factor(territoire),  
                              treat="treat_list_CRSV", 
                              outcome=var,
                              outcome.reg="linear",
                              constrained=TRUE,
                              J=3, data=D)  
  
  original_result <- list.result$par.outcome[15]
  
  # Save figure
  jpeg(paste("sim_", var, ".jpeg"))
  hist(simulated_result$X15, main="", ylab="Frequency", xlab="", border=F)
  box(col="darkgrey")
  abline(v=c(original_result), lty=2, col="red")
  abline(v=c(simulated_result.avg), lty=2)
  text(original_result, 90, paste0("Orig. Est.: ", round(original_result,2)), col="red", cex=.8, pos=2)
  text(simulated_result.avg, 180, paste0("Avg. of Sim. Est.: ", round(simulated_result.avg,2)), cex=.8, pos=2)
  dev.off()
  
} # end of loop

#########################################################################################
