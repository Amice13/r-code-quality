# Libraries
install.packages(c("rms","lme4","texreg","interplot"))
library(rms)
library(lme4)
library(texreg)
library(interplot)

##########################
###### Read in the data
##########################

load("Study1.RData")

######################
## Distribution of mr
#####################

t <- study1[,c("mr", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","country_name")]
t <- t[complete.cases(t),]

# Number of party-elections
unique(t$which_id_party_election)
unique(t$mr)

# Distribution
summary(unique(t$mr))
sd(unique(t$mr))

# One SD below and above mean
mean(unique(t$mr)) - sd(unique(t$mr))
mean(unique(t$mr)) + sd(unique(t$mr))

######################
## Table 1
#####################

t <- study1[,c("mr", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat")]
t <- t[complete.cases(t),]
table1_mod1 <- lrm(voted ~ mr*educ_scaled + age_decades + male + income_scaled + niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
table1_mod1  <- robcov(table1_mod1, t$which_id_party_election, method="huber")

table1_mod2 <- glmer(voted ~ mr*educ_scaled + age_decades + male + income_scaled+ (1| country_name) + (1|country_election) + (1|which_id_party), data=study1, family="binomial")

# Table
texreg(list(table1_mod1, table1_mod2), stars=c(0.05))

######################
## Figure 1
#####################

coefficients <- fixef(table1_mod2)
vcov.matrix <- vcov(table1_mod2)

educ.expanded <- seq(0,1,length.out=1000)
mr.marginal.effects <- coefficients["mr"]+coefficients["mr:educ_scaled"]*educ.expanded
se.mr.marginal.effects <- sqrt(vcov.matrix["mr","mr"]+educ.expanded^2*vcov.matrix["mr:educ_scaled","mr:educ_scaled"]+2*educ.expanded*vcov.matrix["mr","mr:educ_scaled"])
mr.upper.bound <- mr.marginal.effects + 1.96*se.mr.marginal.effects
mr.lower.bound <- mr.marginal.effects - 1.96*se.mr.marginal.effects

pdf(file="plot_mlm_me.pdf")
plot(NULL, type="l", xlim=c(0,1), ylim=c(-10,10), xlab="Education", ylab="Marginal effect",cex.lab=1.3)
lines(educ.expanded,mr.marginal.effects,lwd=2)
polygon.x <- c(educ.expanded, rev(educ.expanded))
polygon.y <- c(mr.lower.bound, rev(mr.upper.bound))
polygon(polygon.x, polygon.y, col=adjustcolor("black", alpha.f=0.1), border=NA)
abline(h=0, lty="dashed")
par(new=T)
hist(table1_mod2@frame$educ_scaled, axes=F,xlab="",ylab="",border="gray",main="")
dev.off()

which(mr.lower.bound>0)
educ.expanded[954] # 0.953954
sum(table1_mod2@frame$educ_scaled >= 0.95, na.rm=T)/length(table1_mod2@frame$educ_scaled) # 0.1515335

######################
##### Figure 2
########################

pframe0_high <- expand.grid(mr =seq(0.06, 0.51,by=0.005), educ_scaled =1, age_decades =round(mean(table1_mod2@frame[,"age_decades"]),2),male=0, income_scaled =round(mean(table1_mod2@frame[,"income_scaled"]),2))
pframe0_high$"mr:educ_scaled" <- unname(pframe0_high$mr*pframe0_high$educ_scaled)
mm <- model.matrix(~ mr  + educ_scaled + age_decades + male + income_scaled + mr:educ_scaled, data=pframe0_high)
pframe1_high <- data.frame(pframe0_high, eta=mm %*% fixef(table1_mod2))
pframe1_high <- with(pframe1_high, data.frame(pframe1_high, CaseMarking=invlogit(eta)))
pframe1_high$pse <- diag(mm %*% tcrossprod(vcov(table1_mod2),mm))
pframe1_high$hi <- with(pframe1_high, eta+1.96*pse) 
pframe1_high$low <- with(pframe1_high, eta-1.96*pse) 
pframe1_high$hi.prob <- invlogit(pframe1_high$hi)
pframe1_high$low.prob <- invlogit(pframe1_high$low) 

pdf(file="plot_mlm_pred_high_educ.pdf")
plot(seq(0.06, 0.51,by=0.005), pframe1_high$CaseMarking, type="l",ylim=c(0.85,1),xlab="Moral rhetoric",ylab="Probability of turnout",main="",cex.lab=1.3,lwd=2)
polygon.x <- c(seq(0.06, 0.51,by=0.005), rev(seq(0.06, 0.51,by=0.005)))
polygon.y <- c(pframe1_high$low.prob, rev(pframe1_high$hi.prob))
polygon(polygon.x, polygon.y, col=adjustcolor("black", alpha.f=0.1),border=NA)
par(new=T)
hist(unique(table1_mod2@frame[,"mr"]), axes=F,xlab="",ylab="",border="gray",main="")
dev.off()

############################
### FD calculations for MR value one sd below and above mean. Using highest educated voter.
#############################

set.seed(314)
sims <- sim(table1_mod2,n.sims=100000)
fe.sims <- fixef(sims)

# choose values for the individual-level variables
f.educ <- 1
f.age <- mean(table1_mod2@frame[,"age_decades"])
f.male <- 0
f.income <- mean(table1_mod2@frame[,"income_scaled"])

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,                                    
             f.mr,                                         
             f.educ,                                 
             f.age,                                   
             f.male,                                            
             f.income,                                       
             f.mr*f.educ)
}

# set values of variable of interest
f.mr <- c(0.20, 0.39)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
fd.stat.gen.high.educ <- p.hi - p.lo

fd <- quantile(fd.stat.gen.high.educ, 0.5)
lwr <- quantile(fd.stat.gen.high.educ, 0.025)
upr <- quantile(fd.stat.gen.high.educ, 0.975)
c(lwr, fd, upr)

############################
####### FD calculations for min MR and max MR, using highest educated voter
###############################

# choose values for the individual-level variables
f.educ <- 1
f.age <- mean(table1_mod2@frame[,"age_decades"])
f.male <- 0
f.income <- mean(table1_mod2@frame[,"income_scaled"])

# set values of variable of interest
f.mr <- c(0.06, 0.51)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
fd.stat.gen.high.educ <- p.hi - p.lo

fd <- quantile(fd.stat.gen.high.educ, 0.5)
lwr <- quantile(fd.stat.gen.high.educ, 0.025)
upr <- quantile(fd.stat.gen.high.educ, 0.975)
c(lwr, fd, upr)

######################
## Correlation calculation following Clark and Linzer (2015), footnote 6 on page 404.
#####################

fixed.mod <- glm(voted ~ mr*educ_scaled + age_decades + male + income_scaled + country_election, data=study1, family="binomial")

# Get unit effects
unit_effects <- coef(fixed.mod)
unit_effects <- unit_effects[-(2:6)]
unit_effects <- unit_effects[-length(unit_effects)]
names(unit_effects)[1] <- "country_electionAustralia 2004"
length(unit_effects) 
unit_effects[2:length(unit_effects)] <- (unit_effects[1] + unit_effects[2:length(unit_effects)])
names(unit_effects) <- substring(names(unit_effects),17)

# Get within-unit means of moral rhetoric
t <- study1[,c("which_id_party_election","mr","country_election")]
t <- t[complete.cases(t),]
t <- t[!duplicated(t),]
unit_mr_means <- as.vector(by(t$mr,t$country_election,mean))
names(unit_mr_means) <- names(by(t$mr,t$country_election,mean))
all.equal(names(unit_mr_means), names(unit_effects))

# Correlation
cor.test(unit_effects,unit_mr_means)

######################
## Hausman test 
#####################

random.mod <- glmer(voted ~ mr*educ_scaled + age_decades + male + income_scaled + (1| country_election), data=study1, family="binomial")

hausman <- function(fixed,random) {
  rNames <- names(fixef(random))
  fNames <- names(coef(fixed))
  timevarNames <- intersect(rNames,fNames)
  k <- length(timevarNames)
  rV <- vcov(random)
  rownames(rV)=rNames
  colnames(rV)=rNames
  bDiff <- (fixef(random))[timevarNames] - coef(fixed)[timevarNames]
  vDiff <- vcov(fixed)[timevarNames,timevarNames] - rV[timevarNames,timevarNames]
  H <- as.numeric(t(bDiff) %*% solve(vDiff) %*% bDiff)
  c(H=H,p.value=pchisq(H,k,lower.tail=FALSE))
}
hausman(fixed.mod,random.mod)

