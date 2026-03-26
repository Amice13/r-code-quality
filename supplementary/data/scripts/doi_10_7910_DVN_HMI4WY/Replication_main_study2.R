# Libraries
install.packages(c("MASS","texreg","arm"))
library(MASS)
library(texreg)
library(arm)

# Read in the data
load("Study2.RData")

#########################
##### Manipulation Check
#########################

sum(round(prop.table(table(cons$manipulation_neg)),2)[4:5])

sum(round(prop.table(table(lab$manipulation_neg)),2)[4:5])

##########################
###### Table 3
##########################

vote_loyal_logit_cons <- glm(vote_loyal ~ treatment, data=cons, family="binomial")
vote_loyal_logit_lab <- glm(vote_loyal ~ treatment, data=lab, family="binomial")
vote_loyal_logit_inter <- glm(vote_loyal ~ treatment * lab, data=combined, family="binomial")

##########################
###### Predicted probabilitis
##########################

predict.glm(vote_loyal_logit_lab, newdata=data.frame(treatment=0), type="response")
predict.glm(vote_loyal_logit_lab, newdata=data.frame(treatment=1), type="response")

##########################
###### First Differences
###########################

# Leftist voter:

set.seed(314)

# Simulate from the model.
sims <- sim(vote_loyal_logit_lab,n.sims=100000)

# Save the simulations of the fixed effects.
fe.sims <- sims@coef

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,
             f.treat)
}

# set values of variable of interest
f.treat <- c(0, 1)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
p.hi.stat.gen <- p.hi
p.lo.stat.gen <- p.lo
fd.stat.gen.low.educ <- p.hi - p.lo

fd <- round(quantile(fd.stat.gen.low.educ, 0.5), 2)
lwr <- round(quantile(fd.stat.gen.low.educ, 0.025), 2)
upr <- round(quantile(fd.stat.gen.low.educ, 0.975), 2)
c(lwr, fd, upr)
# 2.5%   50% 97.5% 
# -0.08 -0.04 -0.01 

##########################
###### Conditional coefficient
##########################

covMat = vcov(vote_loyal_logit_inter)

# Extract the data frame of the model
mod_frame = model.frame(vote_loyal_logit_inter)

# Get coefficients of variables
beta_1 = vote_loyal_logit_inter$coefficients[["treatment"]]
beta_3 = vote_loyal_logit_inter$coefficients[["treatment:lab"]]

# Create list of moderator values at which marginal effect is evaluated
x_2 <- c(0,1)

# Compute marginal effects
delta_1 = beta_1 + beta_3*x_2

# Compute variances
var_1 = covMat["treatment","treatment"] + (x_2^2)*covMat["treatment:lab", "treatment:lab"] + 2*x_2*covMat["treatment", "treatment:lab"]

# Standard errors
se_1 = sqrt(var_1)

# Upper and lower confidence bounds
z_score = qnorm(1 - ((1 - 0.95)/2))
upper_bound = delta_1 + z_score*se_1
lower_bound = delta_1 - z_score*se_1

# Conditional coefficient for leftist
delta_1[2]

# Standard error
se_1[2]

# p-value
pnorm(-0.9359294/0.4044575)*2

##########################
###### FD for interaction model
###########################

# Leftist voter:

# Simulate from the model. 
sims <- sim(vote_loyal_logit_inter,n.sims=100000)

# Save the simulations of the fixed effects.
fe.sims <- sims@coef

# choose values for the individual-level variables
f.lab <- 1

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,
             f.treat,
             f.lab,
             f.treat*f.lab)
}

# set values of variable of interest
f.treat <- c(0, 1)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
p.hi.stat.gen <- p.hi
p.lo.stat.gen <- p.lo
fd.stat.gen.low.educ <- p.hi - p.lo

fd <- round(quantile(fd.stat.gen.low.educ, 0.5), 2)
lwr <- round(quantile(fd.stat.gen.low.educ, 0.025), 2)
upr <- round(quantile(fd.stat.gen.low.educ, 0.975), 2)
c(lwr, fd, upr)
# 2.5%   50% 97.5% 
# -0.08 -0.04 -0.01 

