# Libraries
install.packages(c("lme4","plm","lmtest","rms","texreg","arm"))
library(lme4)
library(plm)
library(lmtest)
library(rms)
library(texreg)
library(arm)

# Load the data
load("Study1b.Rdata")

######################
## Observations
#####################

length(unique(study1b$country_election)) # 14 elections

length(unique(study1b$country_name)) # 8 countries

temp <- study1b
unique(temp$country_year_party_two_char)
temp$country_party <- paste(substr(temp$prev_vote_choice_cdd,1,3),substr(temp$prev_vote_choice_cdd,10,nchar(temp$prev_vote_choice_cdd)),sep="_")
t <- temp[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","country_party")]
t <- t[complete.cases(t),]
unique(t$country_party)
length(unique(t$country_party)) # 51 parties

#####################
###### Table 2
######################

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled")]
t <- t[complete.cases(t),]
control_inter_0to1 <- lrm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, x=T, y=T)
control_inter_0to1 <- robcov(control_inter_0to1, t$prev_vote_choice_cdd, method="huber")

texreg(list(control_inter_0to1),stars=c(0.01,0.05,0.1))

#####################
###### Figure 3
######################

summary(study1b$ideo_scaled_inverted_0to1) # 1st quantile is 0.3. 3rd quantile is 0.7.

pframe0_low <- expand.grid(neg_share_all_other_only =seq(0.00, 1.00,by=0.005), ideo_scaled_inverted_0to1 =0.3, age =round(mean(control_inter_0to1$x[,"age"]),2), age_sq =(round(mean(control_inter_0to1$x[,"age"]),2))^2,male=0, income_scaled =round(mean(control_inter_0to1$x[,"income_scaled"]),2),educ_scaled=round(mean(control_inter_0to1$x[,"educ_scaled"]),2),total_counts=round(mean(control_inter_0to1$x[,"total_counts"]),2))
pframe0_low$"neg_share_all_other_only * ideo_scaled_inverted_0to1" <- unname(pframe0_low$neg_share_all_other_only*pframe0_low$ideo_scaled_inverted_0to1)
mm <- model.matrix(~ neg_share_all_other_only  + ideo_scaled_inverted_0to1 + age + age_sq + male + income_scaled + educ_scaled + total_counts + neg_share_all_other_only * ideo_scaled_inverted_0to1, data=pframe0_low)
pframe1_low <- data.frame(pframe0_low, eta=mm %*% control_inter_0to1$coefficients)
pframe1_low <- with(pframe1_low, data.frame(pframe1_low, CaseMarking=invlogit(eta)))
pframe1_low$pse <- diag(mm %*% tcrossprod(vcov(control_inter_0to1),mm))
pframe1_low$hi <- with(pframe1_low, eta+1.96*pse) 
pframe1_low$low <- with(pframe1_low, eta-1.96*pse)
pframe1_low$hi.prob <- invlogit(pframe1_low$hi) 
pframe1_low$low.prob <- invlogit(pframe1_low$low)

pframe0_high <- expand.grid(neg_share_all_other_only =seq(0.00, 1.00,by=0.005), ideo_scaled_inverted_0to1 =0.7, age =round(mean(control_inter_0to1$x[,"age"]),2), age_sq =(round(mean(control_inter_0to1$x[,"age"]),2))^2,male=0, income_scaled =round(mean(control_inter_0to1$x[,"income_scaled"]),2),educ_scaled=round(mean(control_inter_0to1$x[,"educ_scaled"]),2),total_counts=round(mean(control_inter_0to1$x[,"total_counts"]),2))
pframe0_high$"eg_share_all_other_only * ideo_scaled_inverted_0to1" <- unname(pframe0_high$neg_share_all_other_only*pframe0_high$ideo_scaled_inverted_0to1)
mm <- model.matrix(~ neg_share_all_other_only  + ideo_scaled_inverted_0to1 + age + age_sq + male + income_scaled + educ_scaled + total_counts + neg_share_all_other_only * ideo_scaled_inverted_0to1, data=pframe0_high)
pframe1_high <- data.frame(pframe0_high, eta=mm %*% control_inter_0to1$coefficients)
pframe1_high <- with(pframe1_high, data.frame(pframe1_high, CaseMarking=invlogit(eta)))
pframe1_high$pse <- diag(mm %*% tcrossprod(vcov(control_inter_0to1),mm))
pframe1_high$hi <- with(pframe1_high, eta+1.96*pse)
pframe1_high$low <- with(pframe1_high, eta-1.96*pse)
pframe1_high$hi.prob <- invlogit(pframe1_high$hi)
pframe1_high$low.prob <- invlogit(pframe1_high$low)

pdf(file="plot_predicted_rightist.pdf")
plot(seq(0.00, 1.00,by=0.005), pframe1_low$CaseMarking, type="l",ylim=c(0,0.9),xlab="Valence attack",ylab="Predicted probability (rightist voter)",main="",cex.lab=1.3)
lines(seq(0.00, 1.00,by=0.005), pframe1_low$low.prob, lty="dashed")
lines(seq(0.00, 1.00,by=0.005), pframe1_low$hi.prob, lty="dashed")
par(new=T)
hist(control_inter_0to1$x[,"neg_share_all_other_only"],axes=F,xlab="",ylab="",border="gray",main="")
dev.off()

pdf(file="plot_predicted_leftist.pdf")
plot(seq(0.00, 1.00,by=0.005), pframe1_high$CaseMarking, type="l",ylim=c(0,0.9),xlab="Valence attack",ylab="Predicted probability (leftist voter)",main="",cex.lab=1.3)
lines(seq(0.00, 1.00,by=0.005), pframe1_high$low.prob, lty="dashed")
lines(seq(0.00, 1.00,by=0.005), pframe1_high$hi.prob, lty="dashed")
par(new=T)
hist(control_inter_0to1$x[,"neg_share_all_other_only"],axes=F,xlab="",ylab="",border="gray",main="")
dev.off()

#####################
###### Substantive effect sizes 
######################

t <- study1b[,c("neg_share_all_other_only", "loyal_vote","prev_vote_choice_cdd","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled")]
t <- t[which(complete.cases(t)),]
rownames(t) <- NULL
control_inter_0to1_glm <- glm(loyal_vote ~ neg_share_all_other_only*ideo_scaled_inverted_0to1  + age+ age_sq + male  +income_scaled+educ_scaled +total_counts, data=t, family="binomial")

set.seed(314)

# Simulate from the model.
sims <- sim(control_inter_0to1_glm,n.sims=100000)

# Save the simulations of the fixed effects.
fe.sims <- sims@coef

# Rightist voter:

# choose values for the individual-level variables
f.ideo <- 0.3
f.age <- mean(t$age)
f.agesq <- mean(t$age_sq)
f.male <- 0
f.income_scaled <- mean(t$income_scaled)
f.educ_scaled <- mean(t$educ_scaled)
f.total_counts <- mean(t$total_counts)

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,                                    
             f.va,                                         
             f.ideo,                                 
             f.age,
             f.agesq,
             f.male,                                            
             f.income_scaled,                                       
             f.educ_scaled,
             f.total_counts,
             f.va*f.ideo)
}

# set values of variable of interest
f.va <- c(0.8, 0.99)

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

## For left voter:

# choose values for the individual-level variables
f.ideo <- 0.7
f.age <- mean(t$age)
f.agesq <- mean(t$age_sq)
f.male <- 0
f.income_scaled <- mean(t$income_scaled)
f.educ_scaled <- mean(t$educ_scaled)
f.total_counts <- mean(t$total_counts)

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,                                    
             f.va,                                         
             f.ideo,                                 
             f.age,
             f.agesq,
             f.male,                                            
             f.income_scaled,                                       
             f.educ_scaled,
             f.total_counts,
             f.va*f.ideo)
}

# set values of variable of interest
f.mr <- c(0.8, 0.99)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
p.hi.stat.gen <- p.hi
p.lo.stat.gen <- p.lo
fd.stat.gen.high.educ <- p.hi - p.lo

fd <- round(quantile(fd.stat.gen.high.educ, 0.5), 2)
lwr <- round(quantile(fd.stat.gen.high.educ, 0.025), 2)
upr <- round(quantile(fd.stat.gen.high.educ, 0.975), 2)
c(lwr, fd, upr)

#####################
###### Substantive effect sizes
######################

# Rightist voter:

# choose values for the individual-level variables
f.ideo <- 0.3
f.age <- mean(t$age)
f.agesq <- mean(t$age_sq)
f.male <- 0
f.income_scaled <- mean(t$income_scaled)
f.educ_scaled <- mean(t$educ_scaled)
f.total_counts <- mean(t$total_counts)

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,                                    
             f.va,                                         
             f.ideo,                                 
             f.age,
             f.agesq,
             f.male,                                            
             f.income_scaled,                                       
             f.educ_scaled,
             f.total_counts,
             f.va*f.ideo)
}

# set values of variable of interest
f.va <- c(0.00, 1.00)

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

## For left voter:

# choose values for the individual-level variables
f.ideo <- 0.7
f.age <- mean(t$age)
f.agesq <- mean(t$age_sq)
f.male <- 0
f.income_scaled <- mean(t$income_scaled)
f.educ_scaled <- mean(t$educ_scaled)
f.total_counts <- mean(t$total_counts)

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,                                    
             f.va,                                         
             f.ideo,                                 
             f.age,
             f.agesq,
             f.male,                                            
             f.income_scaled,                                       
             f.educ_scaled,
             f.total_counts,
             f.va*f.ideo)
}

# set values of variable of interest
f.mr <- c(0.00, 1.00)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
p.hi.stat.gen <- p.hi
p.lo.stat.gen <- p.lo
fd.stat.gen.high.educ <- p.hi - p.lo

fd <- round(quantile(fd.stat.gen.high.educ, 0.5), 2)
lwr <- round(quantile(fd.stat.gen.high.educ, 0.025), 2)
upr <- round(quantile(fd.stat.gen.high.educ, 0.975), 2)
c(lwr, fd, upr)
