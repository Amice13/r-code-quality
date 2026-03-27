###########################################################################
# Class Isolation and Affluent Americans' Perception of Social Conditions #
###########################################################################

# Set working directory
setwd("")

# Load necessary R packages
# If not installed, use install.packages("") to install before loading
# Make sure all package dependencies are installed
library(foreign)
library(ggplot2)
library(grid)
library(lme4)
library(texreg)

# Load data for white affluent respondents
data.whiteaff <- read.dta("thalpbwhiteaff.dta", convert.underscore = TRUE)

###############################
# Main Paper Replication Code #
###############################

#################################################################################
# Figure 1: Neighborhood Experience as the Basis for the Affluent's Perceptions #
#################################################################################

# Run random effects model
crimeper.re <- lmer(crime ~
  							 scale(feelsafe) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.whiteaff)

# Generate Figure 1
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
df <- as.data.frame(as.numeric(fixef(crimeper.re)[2:10]))
colnames(df) <- "estimate"
ci <- confint(crimeper.re, method = "boot")
ci <- ci[4:12,]
df$lwr <- as.numeric(ci[,1])
df$upr <- as.numeric(ci[,2])
df$var <-c(1,2,3,5,6,7,8,9,11)
name <- c("Neighborhood Experience", 
          "Suburban Resident",          
          "Homeowner",
          "Female",
          "Age",
          "Education",
          "Income Inequality",
          "Percent Black",
          "Percent Hispanic")

p <- ggplot(df, aes(x = rev(var), y = estimate, ymin = lwr, ymax = upr)) + 
  geom_errorbar(width=.5) + 
  geom_point() +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_y_continuous(name="Coefficients", limits=c(-10,10)) +
  scale_x_continuous(breaks=c(1,2,3,5,6,7,8,9,11),
                     labels=rev(c(name)),
                     name="")

p <- p + annotation_custom(grob = textGrob(label = "Individual Controls", gp = gpar(cex = .95,fontface = "bold")),
                       xmin = 10, xmax = 10, ymin = -14.2, ymax = -14.2) 

p <- p + annotation_custom(grob = textGrob(label = "MSA Controls", gp = gpar(cex = .95,fontface = "bold")),
                       xmin = 4, xmax = 4, ymin = -13.35, ymax = -13.35) 

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

################################################################################
# Table 2: Class Isolation and the Affluent's Perceptions of Social Conditions #
################################################################################

# Run random effects model for "Crime Level"
crime.re <- lmer(crime.diff ~
                 scale(whiteaffiso) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
crime.re.coefs <- data.frame(coef(summary(crime.re)))
crime.re.coefs$p.z <- 2 * (1 - pnorm(abs(crime.re.coefs$t.value)))
crime.re.coefs  # Results for coefficients, standard error, and p-values

# Rurn random effects model for "Healthcare"
health.re <- lmer(health.diff ~
                  scale(whiteaffiso) +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
health.re.coefs <- data.frame(coef(summary(health.re)))
health.re.coefs$p.z <- 2 * (1 - pnorm(abs(health.re.coefs$t.value)))
health.re.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Job Opportunities"
jobs.re <- lmer(jobs.diff ~
                scale(whiteaffiso) +
                suburb +
                homeown +
                female +
                scale(age) +
                scale(edu) +
                scale(gini) +
                scale(blackper) +
                scale(latper) +
                factor(year) +
                (1 | metname),
                data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
jobs.re.coefs <- data.frame(coef(summary(jobs.re)))
jobs.re.coefs$p.z <- 2 * (1 - pnorm(abs(jobs.re.coefs$t.value)))
jobs.re.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Public Schools"
schools.re <- lmer(schools.diff ~
                   scale(whiteaffiso) +
                   suburb +
                   homeown +
                   female +
                   scale(age) +
                   scale(edu) +
                   scale(gini) +
                   scale(blackper) +
                   scale(latper) +
                   factor(year) +
                   (1 | metname),
                   data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
schools.re.coefs <- data.frame(coef(summary(schools.re)))
schools.re.coefs$p.z <- 2 * (1 - pnorm(abs(schools.re.coefs$t.value)))
schools.re.coefs # Results for coefficients, standard error, and p-values

# Generate Table 2
texreg(list(crime.re, health.re, jobs.re, schools.re),
       caption = "Class Isolation and the Affluent's Perception of Social Conditions", 
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Crime Level", "Healthcare",
       											 "Job Opportunities","Public Schools"),
       override.pval = list(crime.re.coefs$p.z, health.re.coefs$p.z,
       										 jobs.re.coefs$p.z, schools.re.coefs$p.z), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
                            "Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

#################################################################################
# Figure 2: Class Isolation and the Affluent's Perceptions of Social Conditions #
#################################################################################

# Create binary versions of year variables
data.whiteaff$yr09 <- ifelse(data.whiteaff$year == 2, 1, 0)
data.whiteaff$yr10 <- ifelse(data.whiteaff$year == 3, 1, 0)

# Create temporary dataframe with class isolation set at .207
newdat207 <-  data.frame(whiteaffiso=.207, 
									suburb=mean(data.whiteaff$suburb),
                  homeown=mean(data.whiteaff$homeown),
									female=mean(data.whiteaff$female),
									age=mean(data.whiteaff$age),
									edu=mean(data.whiteaff$edu),
									gini=mean(data.whiteaff$gini),
  								blackper=mean(data.whiteaff$blackper),
									latper=mean(data.whiteaff$latper),
                  yr09=mean(data.whiteaff$yr09),
									yr10=mean(data.whiteaff$yr10))

# Create temporary dataframe with class isolation set at .578
newdat578 <-  data.frame(whiteaffiso=.578, 
  								suburb=mean(data.whiteaff$suburb),
                  homeown=mean(data.whiteaff$homeown),
									female=mean(data.whiteaff$female),
									age=mean(data.whiteaff$age),
									edu=mean(data.whiteaff$edu),
									gini=mean(data.whiteaff$gini),
  								blackper=mean(data.whiteaff$blackper),
									latper=mean(data.whiteaff$latper),
                  yr09=mean(data.whiteaff$yr09),
									yr10=mean(data.whiteaff$yr10))

# Run random effects model for "Crime Level"
crime.re2 <- lmer(crime.diff ~
                  whiteaffiso +
    					 	  suburb +
                  homeown +
                  female +
                  age +
                  edu +
                  gini +
                  blackper +
                  latper +
                  yr09 +
							    yr10 +
                  (1 | metname),
                  data = data.whiteaff)

# Generate predictions and confidence intervals
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
crime.pred207 <- mean(predict(crime.re2, newdat207, re.form=NA)) 
bootfit <- bootMer(crime.re2, FUN=function(x)predict(x, newdat207, re.form=NA), nsim=1000)
crime.lci207 <- apply(bootfit$t, 2, quantile, 0.025) 
crime.uci207 <- apply(bootfit$t, 2, quantile, 0.975)  

crime.pred578 <- mean(predict(crime.re2, newdat578, re.form=NA)) 
bootfit <- bootMer(crime.re2, FUN=function(x)predict(x, newdat578, re.form=NA), nsim=1000)
crime.lci578 <- apply(bootfit$t, 2, quantile, 0.025)
crime.uci578 <- apply(bootfit$t, 2, quantile, 0.975) 

# Run random effects model for "Healthcare"
health.re2 <- lmer(health.diff ~
                   whiteaffiso +
    						   suburb +
                   homeown +
                   female +
                   age +
                   edu +
                   gini +
                   blackper +
                   latper +
                   yr09 +
							     yr10 +
                   (1 | metname),
                   data = data.whiteaff)

# Generate predictions and confidence intervals
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
health.pred207 <- mean(predict(health.re2, newdat207, re.form=NA)) 
bootfit <- bootMer(health.re2, FUN=function(x)predict(x, newdat207, re.form=NA), nsim=1000)
health.lci207 <- apply(bootfit$t, 2, quantile, 0.025) 
health.uci207 <- apply(bootfit$t, 2, quantile, 0.975)  

health.pred578 <- mean(predict(health.re2, newdat578, re.form=NA)) 
bootfit <- bootMer(health.re2, FUN=function(x)predict(x, newdat578, re.form=NA), nsim=1000)
health.lci578 <- apply(bootfit$t, 2, quantile, 0.025)
health.uci578 <- apply(bootfit$t, 2, quantile, 0.975) 

# Run random effects model for "Job Opportunities"
jobs.re2 <- lmer(jobs.diff ~
                whiteaffiso +
								suburb +
                homeown +
                female +
                age +
                edu +
                gini +
                blackper +
                latper +
                yr09 +
							  yr10 +
                (1 | metname),
                data = data.whiteaff)


# Generate predictions and confidence intervals
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
jobs.pred207 <- mean(predict(jobs.re2, newdat207, re.form=NA)) 
bootfit <- bootMer(jobs.re2, FUN=function(x)predict(x, newdat207, re.form=NA), nsim=1000)
jobs.lci207 <- apply(bootfit$t, 2, quantile, 0.025) 
jobs.uci207 <- apply(bootfit$t, 2, quantile, 0.975)  

jobs.pred578 <- mean(predict(jobs.re2, newdat578, re.form=NA))
bootfit <- bootMer(jobs.re2, FUN=function(x)predict(x, newdat578, re.form=NA), nsim=1000)
jobs.lci578 <- apply(bootfit$t, 2, quantile, 0.025)
jobs.uci578 <- apply(bootfit$t, 2, quantile, 0.975) 

# Run random effects model for "Public Schools"
schools.re2 <- lmer(schools.diffw ~
                    whiteaffiso +
  							    suburb +
                    homeown +
                    female +
                    age +
                    edu +
                    gini +
                    blackper +
                    latper +
                    yr09 +
							      yr10 +
                    (1 | metname),
                    data = data.whiteaff)

# Generate predictions and confidence intervals
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
schools.pred207 <- mean(predict(schools.re2, newdat207, re.form=NA)) 
bootfit <- bootMer(schools.re2, FUN=function(x)predict(x, newdat207, re.form=NA), nsim=1000)
schools.lci207 <- apply(bootfit$t, 2, quantile, 0.025) 
schools.uci207 <- apply(bootfit$t, 2, quantile, 0.975)  

schools.pred578 <- mean(predict(schools.re2, newdat578, re.form=NA)) 
bootfit <- bootMer(schools.re2, FUN=function(x)predict(x, newdat578, re.form=NA), nsim=1000)
schools.lci578 <- apply(bootfit$t, 2, quantile, 0.025)
schools.uci578 <- apply(bootfit$t, 2, quantile, 0.975)


# Generate Figure 2
preds <- as.data.frame(c("Job Opportunities", "Job Opportunities", 
												 "Crime Level", "Crime Level", 
												 "Public Schools", "Public Schools", 
												 "Healthcare", "Healthcare"))
colnames(preds) <- "var"
preds$whiteaffiso <- c(.207, .578, .207, .578, .207, .578, .207, .578) 
preds$pred <- c(jobs.pred207, jobs.pred578, crime.pred207, crime.pred578, 
								schools.pred207, schools.pred578, health.pred207, health.pred578)
preds$lci <- c(jobs.lci207, jobs.lci578, crime.lci207, crime.lci578, 
							 schools.lci207, schools.lci578, health.lci207, health.lci578)
preds$uci <- c(jobs.uci207, jobs.uci578, crime.uci207, crime.uci578, 
							 schools.uci207, schools.uci578, health.uci207, health.uci578)

preds$index <- ifelse(preds$whiteaffiso == .207, "Low",
							 ifelse(preds$whiteaffiso == .578, "High", NA))

preds$index <- factor(preds$index, levels = c("Low", 
																							"High"))

preds$var <- factor(preds$var, levels = c( "Crime Level",
                                           "Healthcare",
                                           "Job Opportunities",    
																					 "Public Schools"))

ggplot(preds, aes(x=index, y=pred, fill=index)) +
	facet_grid(.~var) +
	geom_bar(stat="identity") +
	geom_errorbar(aes(ymin=lci, ymax=uci), width=.1) +
	ylab("Distance from Average Non-Affluent Rating") +
	xlab("Affluent Isolation Index") +
  guides(fill=FALSE) +
  scale_fill_manual(values=c("grey63", "grey63")) +
  theme_bw() +
  theme(text = element_text(size=22)) +
	geom_hline(yintercept=0, linetype = "dashed") +
	scale_y_continuous(breaks=c(-10, -5, 0, 5, 10, 15, 20), limits=c(-10,15))

####################################################################################################
# Figure 3: Class Isolation, Suburban Residence, and Affluent Perceptions of Public School Quality #
####################################################################################################

# Create binary versions of year variables
data.whiteaff$yr09 <- ifelse(data.whiteaff$year == 2, 1, 0)
data.whiteaff$yr10 <- ifelse(data.whiteaff$year == 3, 1, 0)

# Create temporary dataframe with class isolation set at .207 and suburb set at 1
newdat207s <-  data.frame(whiteaffiso=.207, 
									suburb=1,
                  homeown=mean(data.whiteaff$homeown),
									female=mean(data.whiteaff$female),
									age=mean(data.whiteaff$age),
									edu=mean(data.whiteaff$edu),
									gini=mean(data.whiteaff$gini),
									blackper=mean(data.whiteaff$blackper),
									latper=mean(data.whiteaff$latper),
                  yr09=mean(data.whiteaff$yr09),
									yr10=mean(data.whiteaff$yr10))

# Create temporary dataframe with class isolation set at .207 and suburb set at 0
newdat207c <-  data.frame(whiteaffiso=.207, 
									suburb=0,
                  homeown=mean(data.whiteaff$homeown),
									female=mean(data.whiteaff$female),
									age=mean(data.whiteaff$age),
									edu=mean(data.whiteaff$edu),
                  gini=mean(data.whiteaff$gini),
									blackper=mean(data.whiteaff$blackper),
									latper=mean(data.whiteaff$latper),
									yr09=mean(data.whiteaff$yr09),
									yr10=mean(data.whiteaff$yr10))

# Create temporary dataframe with class isolation set at .578 and suburb set at 1
newdat578s <-  data.frame(whiteaffiso=.578, 
									suburb=1,
                  homeown=mean(data.whiteaff$homeown),
									female=mean(data.whiteaff$female),
									age=mean(data.whiteaff$age),
									edu=mean(data.whiteaff$edu),
                  gini=mean(data.whiteaff$gini),
									blackper=mean(data.whiteaff$blackper),
									latper=mean(data.whiteaff$latper),
									yr09=mean(data.whiteaff$yr09),
									yr10=mean(data.whiteaff$yr10))

# Create temporary dataframe with class isolation set at .578 and suburb set at 0
newdat578c <-  data.frame(whiteaffiso=.578, 
									suburb=0,
                  homeown=mean(data.whiteaff$homeown),
									female=mean(data.whiteaff$female),
									age=mean(data.whiteaff$age),
									edu=mean(data.whiteaff$edu),
                  gini=mean(data.whiteaff$gini),
									blackper=mean(data.whiteaff$blackper),
									latper=mean(data.whiteaff$latper),
									yr09=mean(data.whiteaff$yr09),
									yr10=mean(data.whiteaff$yr10))

# Run random effects model for "Public Schools" with Class Isolation X Suburb interaction
schools.re3 <- lmer(schools.diff ~
                whiteaffiso*suburb +
                homeown +
                female +
                age +
                edu +
                gini +
                blackper +
                latper +
                yr09 +
							  yr10 +
                (1 | metname),
                data = data.whiteaff)

# Generate predictions and confidence intervals
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
schools.pred207s <- mean(predict(schools.re3, newdat207s, re.form=NA)) 
bootfit <- bootMer(schools.re3, FUN=function(x)predict(x, newdat207s, re.form=NA), nsim=1000)
schools.lci207s <- apply(bootfit$t, 2, quantile, 0.025) 
schools.uci207s <- apply(bootfit$t, 2, quantile, 0.975) 

schools.pred207c <- mean(predict(schools.re3, newdat207c, re.form=NA)) 
bootfit <- bootMer(schools.re3, FUN=function(x)predict(x, newdat207c, re.form=NA), nsim=1000)
schools.lci207c <- apply(bootfit$t, 2, quantile, 0.025) 
schools.uci207c <- apply(bootfit$t, 2, quantile, 0.975)  

schools.pred578s <- mean(predict(schools.re3, newdat578s, re.form=NA)) 
bootfit <- bootMer(schools.re3, FUN=function(x)predict(x, newdat578s, re.form=NA), nsim=1000)
schools.lci578s <- apply(bootfit$t, 2, quantile, 0.025) 
schools.uci578s <- apply(bootfit$t, 2, quantile, 0.975) 

schools.pred578c <- mean(predict(schools.re3, newdat578c, re.form=NA)) 
bootfit <- bootMer(schools.re3, FUN=function(x)predict(x, newdat578c, re.form=NA), nsim=1000)
schools.lci578c <- apply(bootfit$t, 2, quantile, 0.025) 
schools.uci578c <- apply(bootfit$t, 2, quantile, 0.975)  

# Generate Figure 3
sub <- as.data.frame(c(".207", ".207", ".578", ".578"))
colnames(sub) <- "whiteaffiso"
sub$suburban <- c(0,1,0,1)
sub$pred <- c(schools.pred207c, schools.pred207s, schools.pred578c, schools.pred578s)
sub$lci <- c(schools.lci207c, schools.lci207s, schools.lci578c, schools.lci578s)
sub$uci <- c(schools.uci207c, schools.uci207s, schools.uci578c, schools.uci578s)

sub$index <- ifelse(as.numeric(as.character(sub$whiteaffiso)) == .207, "Low",
						 ifelse(as.numeric(as.character(sub$whiteaffiso)) == .578, "High", NA))

sub$index <- factor(sub$index, levels = c("Low", 
																					"High"))

sub$suburban <- ifelse(sub$suburban == 1, "Suburb", "Central City")

sub$suburban <- factor(sub$suburban, levels = c("Central City", 
																			 "Suburb"))


dodge <- position_dodge(width=.9)

ggplot(sub, aes(x=index, y=pred, fill=suburban)) +
	geom_bar(stat="identity", position = "dodge") +
	geom_errorbar(aes(ymin=lci, ymax=uci), width=.1, position = dodge) +
	scale_fill_manual(values=c("grey42", "grey63")) +
	ylab("Distance from Average Non-Affluent Rating") +
	xlab("Affluent Isolation Index") +
	ggtitle("") +
	guides(fill=guide_legend(title=NULL)) +
  theme_bw() +
  theme(text = element_text(size=22)) +
	geom_hline(yintercept=0, linetype = "dashed") +
	scale_y_continuous(breaks=c(-20, -15, -10, -5, 0, 5, 10, 15, 20), limits=c(-15,15))

####################################################################################################
# Table 3: Class Isolation and the Affluent's Perceptions of Social Conditions - White Sample Only #
####################################################################################################

# Run random effects model for "Crime Level"
crime.rew <- lmer(crime.diffw ~
                 scale(whiteaffiso) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
crime.rew.coefs <- data.frame(coef(summary(crime.rew)))
crime.rew.coefs$p.z <- 2 * (1 - pnorm(abs(crime.rew.coefs$t.value)))
crime.rew.coefs  # Results for coefficients, standard error, and p-values

# Rurn random effects model for "Healthcare"
health.rew <- lmer(health.diffw ~
                  scale(whiteaffiso) +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
health.rew.coefs <- data.frame(coef(summary(health.rew)))
health.rew.coefs$p.z <- 2 * (1 - pnorm(abs(health.rew.coefs$t.value)))
health.rew.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Job Opportunities"
jobs.rew <- lmer(jobs.diffw ~
                scale(whiteaffiso) +
                suburb +
                homeown +
                female +
                scale(age) +
                scale(edu) +
                scale(gini) +
                scale(blackper) +
                scale(latper) +
                factor(year) +
                (1 | metname),
                data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
jobs.rew.coefs <- data.frame(coef(summary(jobs.rew)))
jobs.rew.coefs$p.z <- 2 * (1 - pnorm(abs(jobs.rew.coefs$t.value)))
jobs.rew.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Public Schools"
schools.rew <- lmer(schools.diffw ~
                   scale(whiteaffiso) +
                   suburb +
                   homeown +
                   female +
                   scale(age) +
                   scale(edu) +
                   scale(gini) +
                   scale(blackper) +
                   scale(latper) +
                   factor(year) +
                   (1 | metname),
                   data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
schools.rew.coefs <- data.frame(coef(summary(schools.rew)))
schools.rew.coefs$p.z <- 2 * (1 - pnorm(abs(schools.rew.coefs$t.value)))
schools.rew.coefs # Results for coefficients, standard error, and p-values

# Generate Table 2
texreg(list(crime.rew, health.rew, jobs.rew, schools.rew),
       caption = "Class Isolation and the Affluent's Perception of Social Conditions - White Sample Only", 
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Crime Level", "Healthcare",
         										 "Job Opportunities","Public Schools"),
       override.pval = list(crime.rew.coefs$p.z, health.rew.coefs$p.z,
       										 jobs.rew.coefs$p.z, schools.rew.coefs$p.z), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
                            "Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

#######################################################
# Table 4: Class Isolation and Affluent Participation #
#######################################################

# Run random effects model for "Work to Make Change"
work.re <- glmer(work ~
              scale(whiteaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff)

# Run random effects model for "Attend Issue Meetings"
meet.re <- glmer(meet ~
              scale(whiteaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff)

# Run random effects model for "Group Membership"
org.re <- glmer(orgbin ~
             scale(whiteaffiso) +
             suburb +
             homeown +
             female +
             scale(age) +
             scale(edu) +
             scale(gini) +
             scale(blackper) +
             scale(latper) +
             factor(year) +
             (1 | metname),
             family = binomial("logit"),
             data = data.whiteaff)

# Run random effects model for "Group Volunteering"
vol.re <- glmer(vol ~
             scale(whiteaffiso) +
             suburb +
             homeown +
             female +
             scale(age) +
             scale(edu) +
             scale(gini) +
             scale(blackper) +
             scale(latper) +
             factor(year) +
             (1 | metname),
             family = binomial("logit"),
             data = data.whiteaff)

# Run random effects model for "Registered to Vote"
register.re <- glmer(register ~
                  scale(whiteaffiso) +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  family = binomial("logit"),
                  data = data.whiteaff)

# Run random effects model for "Vote in Local Election"
vote.re <- glmer(vote ~
              scale(whiteaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff)

# Generate Table 4
texreg(list(work.re, meet.re, org.re, vol.re, register.re, vote.re),
       caption = "Class Isolation and Affluent Participation", 
       caption.above = TRUE, digits = 2, stars = c(0.001, 0.01, 0.05),
       custom.model.names = c("Work to Make Change",
                             "Attend Issue Meetings", 
       											 "Group Membership",
                              "Group Volunteering",
       											 	"Register to Vote", 
                              "Vote in Local Election"), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
      											"Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

#############################
# Appendix Replication Code #
#############################

###########################################################################
# Figure A1: Income Inequality and Class Isolation within SOTC Communties #
###########################################################################

# Create dataframe of MSA level variables
plot <- unique(cbind(data.whiteaff[c("gini", "whiteaffiso", "metname")]))

# Generate Figure A1
# (Note: Exact positions may vary due to jittering)
ggplot(plot, aes(x=gini, y=whiteaffiso, label = metname)) +
    geom_text(position = position_jitter(h=.01)) + 
    geom_smooth(method=lm, se=FALSE) +
    xlim(.41,.5) +
	  theme_bw() +
		xlab("Gini Coefficient") +
	  ylab("Affluent Isolation Index") 

#######################
# Imputation Analysis #
#######################

# Load data for white affluent respondents based on imputation analysis
data.whiteaff.imp <- read.dta("thalpbwhiteaffimp.dta", convert.underscore = TRUE)

########################################################################################################
# Figure A2: Neighborhood Experience as the Basis for the Affluent's Perceptions - Imputation Analysis #
########################################################################################################

# Run random effects model
crimeper.re.i <- lmer(crime ~
      					 scale(feelsafe) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.whiteaff.imp)

# Generate Figure A2
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
df <- as.data.frame(as.numeric(fixef(crimeper.re.i)[2:10]))
colnames(df) <- "estimate"
ci <- confint(crimeper.re.i, method = "boot")
ci <- ci[4:12,]
df$lwr <- as.numeric(ci[,1])
df$upr <- as.numeric(ci[,2])
df$var <-c(1,2,3,5,6,7,8,9,11)
name <- c("Neighborhood Experience", 
          "Suburban Resident",          
          "Homeowner",
          "Female",
          "Age",
          "Education",
          "Income Inequality",
          "Percent Black",
          "Percent Hispanic")

p <- ggplot(df, aes(x = rev(var), y = estimate, ymin = lwr, ymax = upr)) + 
  geom_errorbar(width=.5) + 
  geom_point() +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_y_continuous(name="Coefficients", limits=c(-10,10)) +
  scale_x_continuous(breaks=c(1,2,3,5,6,7,8,9,11),
                     labels=rev(c(name)),
                     name="")

p <- p + annotation_custom(grob = textGrob(label = "Individual Controls", gp = gpar(cex = .95,fontface = "bold")),
                       xmin = 10, xmax = 10, ymin = -14.2, ymax = -14.2) 

p <- p + annotation_custom(grob = textGrob(label = "MSA Controls", gp = gpar(cex = .95,fontface = "bold")),
                       xmin = 4, xmax = 4, ymin = -13.35, ymax = -13.35) 

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

#######################################################################################################
# Table A2: Class Isolation and the Affluent's Perceptions of Social Conditions - Imputation Analysis #
#######################################################################################################

# Run random effects model for "Crime Level"
crime.re.i <- lmer(crime.diff ~
                 scale(whiteaffiso) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.whiteaff.imp)

# Generate p-values for coefficients using the normal approximation
crime.re.i.coefs <- data.frame(coef(summary(crime.re.i)))
crime.re.i.coefs$p.z <- 2 * (1 - pnorm(abs(crime.re.i.coefs$t.value)))
crime.re.i.coefs  # Results for coefficients, standard error, and p-values

# Rurn random effects model for "Healthcare"
health.re.i <- lmer(health.diff ~
                  scale(whiteaffiso) +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  data = data.whiteaff.imp)

# Generate p-values for coefficients using the normal approximation
health.re.i.coefs <- data.frame(coef(summary(health.re.i)))
health.re.i.coefs$p.z <- 2 * (1 - pnorm(abs(health.re.i.coefs$t.value)))
health.re.i.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Job Opportunities"
jobs.re.i <- lmer(jobs.diff ~
                scale(whiteaffiso) +
                suburb +
                homeown +
                female +
                scale(age) +
                scale(edu) +
                scale(gini) +
                scale(blackper) +
                scale(latper) +
                factor(year) +
                (1 | metname),
                data = data.whiteaff.imp)

# Generate p-values for coefficients using the normal approximation
jobs.re.i.coefs <- data.frame(coef(summary(jobs.re.i)))
jobs.re.i.coefs$p.z <- 2 * (1 - pnorm(abs(jobs.re.i.coefs$t.value)))
jobs.re.i.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Public Schools"
schools.re.i <- lmer(schools.diff ~
                   scale(whiteaffiso) +
                   suburb +
                   homeown +
                   female +
                   scale(age) +
                   scale(edu) +
                   scale(gini) +
                   scale(blackper) +
                   scale(latper) +
                   factor(year) +
                   (1 | metname),
                   data = data.whiteaff.imp)

# Generate p-values for coefficients using the normal approximation
schools.re.i.coefs <- data.frame(coef(summary(schools.re.i)))
schools.re.i.coefs$p.z <- 2 * (1 - pnorm(abs(schools.re.i.coefs$t.value)))
schools.re.i.coefs # Results for coefficients, standard error, and p-values

# Generate Table A2
texreg(list(crime.re.i, health.re.i, jobs.re.i, schools.re.i),
       caption = "Class Isolation and the Affluent's Perception of Social Conditions - Imputation Analysis", 
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Crime Level", "Healthcare",
         										 "Job Opportunities","Public Schools"),
       override.pval = list(crime.re.i.coefs$p.z, health.re.i.coefs$p.z,
       										 jobs.re.i.coefs$p.z, schools.re.i.coefs$p.z), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
                            "Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

##############################################################################
# Table A3: Class Isolation and Affluent Participation - Imputation Analysis #
##############################################################################

# Run random effects model for "Work to Make Change"
work.re.i <- glmer(work ~
              scale(whiteaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff.imp)

# Run random effects model for "Attend Issue Meetings"
meet.re.i <- glmer(meet ~
              scale(whiteaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff.imp)

# Run random effects model for "Group Membership"
org.re.i <- glmer(orgbin ~
             scale(whiteaffiso) +
             suburb +
             homeown +
             female +
             scale(age) +
             scale(edu) +
             scale(gini) +
             scale(blackper) +
             scale(latper) +
             factor(year) +
             (1 | metname),
             family = binomial("logit"),
             data = data.whiteaff.imp)

# Run random effects model for "Group Volunteering"
vol.re.i <- glmer(vol ~
             scale(whiteaffiso) +
             suburb +
             homeown +
             female +
             scale(age) +
             scale(edu) +
             scale(gini) +
             scale(blackper) +
             scale(latper) +
             factor(year) +
             (1 | metname),
             family = binomial("logit"),
             data = data.whiteaff.imp)

# Run random effects model for "Registered to Vote"
register.re.i <- glmer(register ~
                  scale(whiteaffiso) +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  family = binomial("logit"),
                  data = data.whiteaff.imp)

# Run random effects model for "Vote in Local Election"
vote.re.i <- glmer(vote ~
              scale(whiteaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff.imp)

# Generate Table A3
texreg(list(work.re.i, meet.re.i, org.re.i, vol.re.i, register.re.i, vote.re.i),
       caption = "Class Isolation and Affluent Participation - Imputation Analysis", 
       caption.above = TRUE, digits = 2, stars = c(0.001, 0.01, 0.05),
       custom.model.names = c("Work to Make Change",
                             "Attend Issue Meetings", 
         										 "Group Membership",
                              "Group Volunteering",
       											 	"Register to Vote", 
                              "Vote in Local Election"), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
      											"Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

############################
# Affluent Blacks Analysis #
############################

# Load data for black affluent respondents
data.blackaff <- read.dta("thalpbblackaff.dta", convert.underscore = TRUE)

####################################################################################################
# Figure A3: Neighborhood Experience as the Basis for the Affluent's Perceptions - Affluent Blacks #
####################################################################################################

# Run random effects model
crimeper.re.b <- lmer(crime ~
        				 scale(feelsafe) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.blackaff)

# Generate Figure A3
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
df <- as.data.frame(as.numeric(fixef(crimeper.re.b)[2:10]))
colnames(df) <- "estimate"
ci <- confint(crimeper.re.b, method = "boot")
ci <- ci[4:12,]
df$lwr <- as.numeric(ci[,1])
df$upr <- as.numeric(ci[,2])
df$var <-c(1,2,3,5,6,7,8,9,11)
name <- c("Neighborhood Experience", 
          "Suburban Resident",          
          "Homeowner",
          "Female",
          "Age",
          "Education",
          "Income Inequality",
          "Percent Black",
          "Percent Hispanic")

p <- ggplot(df, aes(x = rev(var), y = estimate, ymin = lwr, ymax = upr)) + 
  geom_errorbar(width=.5) + 
  geom_point() +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_y_continuous(name="Coefficients", limits=c(-20,20)) +
  scale_x_continuous(breaks=c(1,2,3,5,6,7,8,9,11),
                     labels=rev(c(name)),
                     name="")

p <- p + annotation_custom(grob = textGrob(label = "Individual Controls", gp = gpar(cex = .95,fontface = "bold")),
                       xmin = 10, xmax = 10, ymin = -27.8, ymax = -27.8) 

p <- p + annotation_custom(grob = textGrob(label = "MSA Controls", gp = gpar(cex = .95,fontface = "bold")),
                       xmin = 4, xmax = 4, ymin = -26.35, ymax = -26.35) 

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

###################################################################################################
# Table A4: Class Isolation and the Affluent's Perceptions of Social Conditions - Affluent Blacks #
###################################################################################################

# Run random effects model for "Crime Level"
crime.re.b <- lmer(crime.diff ~
                 scale(blackaffiso) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.blackaff)

# Generate p-values for coefficients using the normal approximation
crime.re.b.coefs <- data.frame(coef(summary(crime.re.b)))
crime.re.b.coefs$p.z <- 2 * (1 - pnorm(abs(crime.re.b.coefs$t.value)))
crime.re.b.coefs  # Results for coefficients, standard error, and p-values

# Rurn random effects model for "Healthcare"
health.re.b <- lmer(health.diff ~
                  scale(blackaffiso) +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  data = data.blackaff)

# Generate p-values for coefficients using the normal approximation
health.re.b.coefs <- data.frame(coef(summary(health.re.b)))
health.re.b.coefs$p.z <- 2 * (1 - pnorm(abs(health.re.b.coefs$t.value)))
health.re.b.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Job Opportunities"
jobs.re.b <- lmer(jobs.diff ~
                scale(blackaffiso) +
                suburb +
                homeown +
                female +
                scale(age) +
                scale(edu) +
                scale(gini) +
                scale(blackper) +
                scale(latper) +
                factor(year) +
                (1 | metname),
                data = data.blackaff)

# Generate p-values for coefficients using the normal approximation
jobs.re.b.coefs <- data.frame(coef(summary(jobs.re.b)))
jobs.re.b.coefs$p.z <- 2 * (1 - pnorm(abs(jobs.re.b.coefs$t.value)))
jobs.re.b.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Public Schools"
schools.re.b <- lmer(schools.diff ~
                   scale(blackaffiso) +
                   suburb +
                   homeown +
                   female +
                   scale(age) +
                   scale(edu) +
                   scale(gini) +
                   scale(blackper) +
                   scale(latper) +
                   factor(year) +
                   (1 | metname),
                   data = data.blackaff)

# Generate p-values for coefficients using the normal approximation
schools.re.b.coefs <- data.frame(coef(summary(schools.re.b)))
schools.re.b.coefs$p.z <- 2 * (1 - pnorm(abs(schools.re.b.coefs$t.value)))
schools.re.b.coefs # Results for coefficients, standard error, and p-values

# Generate Table A4
texreg(list(crime.re.b, health.re.b, jobs.re.b, schools.re.b),
       caption = "Class Isolation and the Affluent's Perception of Social Conditions - Affluent Blacks", 
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Crime Level", "Healthcare",
         										 "Job Opportunities","Public Schools"),
       override.pval = list(crime.re.b.coefs$p.z, health.re.b.coefs$p.z,
       										 jobs.re.b.coefs$p.z, schools.re.b.coefs$p.z), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
                            "Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

##########################################################################
# Table A5: Class Isolation and Affluent Participation - Affluent Blacks #
##########################################################################

# Run random effects model for "Work to Make Change"
work.re.b <- glmer(work ~
              scale(blackaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.blackaff)

# Run random effects model for "Attend Issue Meetings"
meet.re.b <- glmer(meet ~
              scale(blackaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.blackaff)

# Run random effects model for "Group Membership"
org.re.b <- glmer(orgbin ~
             scale(blackaffiso) +
             suburb +
             homeown +
             female +
             scale(age) +
             scale(edu) +
             scale(gini) +
             scale(blackper) +
             scale(latper) +
             factor(year) +
             (1 | metname),
             family = binomial("logit"),
             data = data.blackaff)

# Run random effects model for "Group Volunteering"
vol.re.b <- glmer(vol ~
             scale(blackaffiso) +
             suburb +
             homeown +
             female +
             scale(age) +
             scale(edu) +
             scale(gini) +
             scale(blackper) +
             scale(latper) +
             factor(year) +
             (1 | metname),
             family = binomial("logit"),
             data = data.blackaff)

# Run random effects model for "Registered to Vote"
register.re.b <- glmer(register ~
                  scale(blackaffiso) +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  family = binomial("logit"),
                  data = data.blackaff)

# Run random effects model for "Vote in Local Election"
vote.re.b <- glmer(vote ~
              scale(blackaffiso) +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.blackaff)

# Generate Table A5
texreg(list(work.re.b, meet.re.b, org.re.b, vol.re.b, register.re.b, vote.re.b),
       caption = "Class Isolation and Affluent Participation - Affluent Blacks", 
       caption.above = TRUE, digits = 2, stars = c(0.001, 0.01, 0.05),
       custom.model.names = c("Work to Make Change",
                             "Attend Issue Meetings", 
         										 "Group Membership",
                              "Group Volunteering",
       											 	"Register to Vote", 
                              "Vote in Local Election"), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
      											"Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

#############################################
# Red State vs. Blue State Robustness Check #
#############################################

##############################################################################################################################
# Figure A4: Neighborhood Experience as the Basis for the Affluent's Perceptions - Red State vs. Blue State Robustness Check #
##############################################################################################################################

# Run random effects model
crimeper.re.rs <- lmer(crime ~
        				 scale(feelsafe) +
                 redstate +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.whiteaff)

# Generate Figure A4
# (Note: Exact confidence intervals may vary due to simulations in bootstrapping process)
df <- as.data.frame(as.numeric(fixef(crimeper.re.rs)[2:11]))
colnames(df) <- "estimate"
ci <- confint(crimeper.re.rs, method = "boot")
ci <- ci[4:13,]
df$lwr <- as.numeric(ci[,1])
df$upr <- as.numeric(ci[,2])
df$var <-c(1,2,3,5,6,7,8,9,11,12)
name <- c("Neighborhood Experience", 
          "Red State",
          "Suburban Resident",          
          "Homeowner",
          "Female",
          "Age",
          "Education",
          "Income Inequality",
          "Percent Black",
          "Percent Hispanic")

p <- ggplot(df, aes(x = rev(var), y = estimate, ymin = lwr, ymax = upr)) + 
  geom_errorbar(width=.5) + 
  geom_point() +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_y_continuous(name="Coefficients", limits=c(-10,10)) +
  scale_x_continuous(breaks=c(1,2,3,5,6,7,8,9,11,12),
                     labels=rev(c(name)),
                     name="")

p <- p + annotation_custom(grob = textGrob(label = "Individual Controls", gp = gpar(cex = .95,fontface = "bold")),
                       xmin = 10, xmax = 10, ymin = -14.2, ymax = -14.2) 

p <- p + annotation_custom(grob = textGrob(label = "MSA Controls", gp = gpar(cex = .95,fontface = "bold")),
                       xmin = 4, xmax = 4, ymin = -13.35, ymax = -13.35) 

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

#############################################################################################################################
# Table A6: Class Isolation and the Affluent's Perceptions of Social Conditions - Red State vs. Blue State Robustness Check #
#############################################################################################################################

# Run random effects model for "Crime Level"
crime.re.rs <- lmer(crime.diff ~
                 scale(whiteaffiso) +
                 redstate +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
crime.re.rs.coefs <- data.frame(coef(summary(crime.re.rs)))
crime.re.rs.coefs$p.z <- 2 * (1 - pnorm(abs(crime.re.rs.coefs$t.value)))
crime.re.rs.coefs  # Results for coefficients, standard error, and p-values

# Rurn random effects model for "Healthcare"
health.re.rs <- lmer(health.diff ~
                  scale(whiteaffiso) +
                  redstate +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
health.re.rs.coefs <- data.frame(coef(summary(health.re.rs)))
health.re.rs.coefs$p.z <- 2 * (1 - pnorm(abs(health.re.rs.coefs$t.value)))
health.re.rs.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Job Opportunities"
jobs.re.rs <- lmer(jobs.diff ~
                scale(whiteaffiso) +
                redstate +
                suburb +
                homeown +
                female +
                scale(age) +
                scale(edu) +
                scale(gini) +
                scale(blackper) +
                scale(latper) +
                factor(year) +
                (1 | metname),
                data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
jobs.re.rs.coefs <- data.frame(coef(summary(jobs.re.rs)))
jobs.re.rs.coefs$p.z <- 2 * (1 - pnorm(abs(jobs.re.rs.coefs$t.value)))
jobs.re.rs.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Public Schools"
schools.re.rs <- lmer(schools.diff ~
                   scale(whiteaffiso) +
                   redstate +
                   suburb +
                   homeown +
                   female +
                   scale(age) +
                   scale(edu) +
                   scale(gini) +
                   scale(blackper) +
                   scale(latper) +
                   factor(year) +
                   (1 | metname),
                   data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
schools.re.rs.coefs <- data.frame(coef(summary(schools.re.rs)))
schools.re.rs.coefs$p.z <- 2 * (1 - pnorm(abs(schools.re.rs.coefs$t.value)))
schools.re.rs.coefs # Results for coefficients, standard error, and p-values

# Generate Table A6
texreg(list(crime.re.rs, health.re.rs, jobs.re.rs, schools.re.rs),
       caption = "Class Isolation and the Affluent's Perception of Social Conditions - Red State vs. Blue State Robustness Check", 
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Crime Level", "Healthcare",
         										 "Job Opportunities","Public Schools"),
       override.pval = list(crime.re.rs.coefs$p.z, health.re.rs.coefs$p.z,
       										 jobs.re.rs.coefs$p.z, schools.re.rs.coefs$p.z), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
                            "Red State",
                            "Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

####################################################################################################
# Table A7: Class Isolation and Affluent Participation - Red State vs. Blue State Robustness Check #
####################################################################################################

# Run random effects model for "Work to Make Change"
work.re.rs <- glmer(work ~
              scale(whiteaffiso) +
              redstate +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff)

# Run random effects model for "Attend Issue Meetings"
meet.re.rs <- glmer(meet ~
              scale(whiteaffiso) +
              redstate +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff)

# Run random effects model for "Group Membership"
org.re.rs <- glmer(orgbin ~
             scale(whiteaffiso) +
             redstate +
             suburb +
             homeown +
             female +
             scale(age) +
             scale(edu) +
             scale(gini) +
             scale(blackper) +
             scale(latper) +
             factor(year) +
             (1 | metname),
             family = binomial("logit"),
             data = data.whiteaff)

# Run random effects model for "Group Volunteering"
vol.re.rs <- glmer(vol ~
             scale(whiteaffiso) +
             redstate +
             suburb +
             homeown +
             female +
             scale(age) +
             scale(edu) +
             scale(gini) +
             scale(blackper) +
             scale(latper) +
             factor(year) +
             (1 | metname),
             family = binomial("logit"),
             data = data.whiteaff)

# Run random effects model for "Registered to Vote"
register.re.rs <- glmer(register ~
                  scale(whiteaffiso) +
                  redstate +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  family = binomial("logit"),
                  data = data.whiteaff)

# Run random effects model for "Vote in Local Election"
vote.re.rs <- glmer(vote ~
              scale(whiteaffiso) +
              redstate +
              suburb +
              homeown +
              female +
              scale(age) +
              scale(edu) +
              scale(gini) +
              scale(blackper) +
              scale(latper) +
              factor(year) +
              (1 | metname),
              family = binomial("logit"),
              data = data.whiteaff)

# Generate Table A7
texreg(list(work.re.rs, meet.re.rs, org.re.rs, vol.re.rs, register.re.rs, vote.re.rs),
       caption = "Class Isolation and Affluent Participation - Red State vs. Blue State Robustness Check", 
       caption.above = TRUE, digits = 2, stars = c(0.001, 0.01, 0.05),
       custom.model.names = c("Work to Make Change",
                             "Attend Issue Meetings", 
         										 "Group Membership",
                              "Group Volunteering",
       											 	"Register to Vote", 
                              "Vote in Local Election"), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
                            "Red State",
      											"Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))

#################################################################################
# Table A8: Neighborhood Experience as the Basis for the Affluent's Perceptions #
#################################################################################

# Run OLS model
crimeper.fe <- lm(crime ~
  							 scale(feelsafe) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 factor(year) +
								 factor(metname),
                 data = data.whiteaff)

texreg(list(crimeper.fe),
       caption = "Neighborhood Experience as the Basis for the Affluent's Perceptions", 
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Perceptions of Crime Level"),
       naive=T)

#################################################################################
# Table A9: Neighborhood Experience as the Basis for the Affluent's Perceptions #
#################################################################################

# Run random effects model for "Public Schools" with Class Isolation X Suburb interaction
schools.re4 <- lmer(schools.diff ~
                scale(whiteaffiso)*suburb +
                homeown +
                female +
                scale(age) +
                scale(edu) +
                scale(gini) +
                scale(blackper) +
                scale(latper) +
                factor(year) +
                (1 | metname),
                data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
schools.re4.coefs <- data.frame(coef(summary(schools.re4)))
schools.re4.coefs$p.z <- 2 * (1 - pnorm(abs(schools.re4.coefs$t.value)))
schools.re4.coefs # Results for coefficients, standard error, and p-values

texreg(list(schools.re4),
       caption = "Class Isolation, Suburban Residence, and Perceptions of Public School Quality", 
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Public Schools"),
       override.pval = list(schools.re4.coefs$p.z), naive=T,
       custom.coef.names = c("Intercept",
                             "Class Isolation",
                             "Suburban Resident",
                             "Homeowner",
                             "Female",
                             "Age",
                             "Education",
                             "Income Inequality",
                             "Percent Black",
                             "Percent Hispanic",
                             "2009",
                             "2010",
                             "Class Isolation X Suburban Resident"))

######################################################################################################
# Table A10: Class Isolation and the Affluent's Perceptions of Social Conditions - White Sample Only #
######################################################################################################

# Run random effects model for "Crime Level"
crime.rew <- lmer(crime.diffw ~
                 scale(whiteaffiso) +
                 suburb +
                 homeown +
                 female +
                 scale(age) +
                 scale(edu) +
                 scale(gini) +
                 scale(blackper) +
                 scale(latper) +
                 factor(year) +
                 (1 | metname),
                 data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
crime.rew.coefs <- data.frame(coef(summary(crime.rew)))
crime.rew.coefs$p.z <- 2 * (1 - pnorm(abs(crime.rew.coefs$t.value)))
crime.rew.coefs  # Results for coefficients, standard error, and p-values

# Rurn random effects model for "Healthcare"
health.rew <- lmer(health.diffw ~
                  scale(whiteaffiso) +
                  suburb +
                  homeown +
                  female +
                  scale(age) +
                  scale(edu) +
                  scale(gini) +
                  scale(blackper) +
                  scale(latper) +
                  factor(year) +
                  (1 | metname),
                  data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
health.rew.coefs <- data.frame(coef(summary(health.rew)))
health.rew.coefs$p.z <- 2 * (1 - pnorm(abs(health.rew.coefs$t.value)))
health.rew.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Job Opportunities"
jobs.rew <- lmer(jobs.diffw ~
                scale(whiteaffiso) +
                suburb +
                homeown +
                female +
                scale(age) +
                scale(edu) +
                scale(gini) +
                scale(blackper) +
                scale(latper) +
                factor(year) +
                (1 | metname),
                data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
jobs.rew.coefs <- data.frame(coef(summary(jobs.rew)))
jobs.rew.coefs$p.z <- 2 * (1 - pnorm(abs(jobs.rew.coefs$t.value)))
jobs.rew.coefs # Results for coefficients, standard error, and p-values

# Run random effects model for "Public Schools"
schools.rew <- lmer(schools.diffw ~
                   scale(whiteaffiso) +
                   suburb +
                   homeown +
                   female +
                   scale(age) +
                   scale(edu) +
                   scale(gini) +
                   scale(blackper) +
                   scale(latper) +
                   factor(year) +
                   (1 | metname),
                   data = data.whiteaff)

# Generate p-values for coefficients using the normal approximation
schools.rew.coefs <- data.frame(coef(summary(schools.rew)))
schools.rew.coefs$p.z <- 2 * (1 - pnorm(abs(schools.rew.coefs$t.value)))
schools.rew.coefs # Results for coefficients, standard error, and p-values

# Generate Table A10
texreg(list(crime.rew, health.rew, jobs.rew, schools.rew),
       caption = "Class Isolation and the Affluent's Perception of Social Conditions - White Sample Only", 
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Crime Level", "Healthcare",
           									 "Job Opportunities","Public Schools"),
       override.pval = list(crime.rew.coefs$p.z, health.rew.coefs$p.z,
       										 jobs.rew.coefs$p.z, schools.rew.coefs$p.z), naive=T,
      custom.coef.names = c("Intercept",
                            "Class Isolation",
                            "Suburban Resident",
                            "Homeowner",
                            "Female",
                            "Age",
                            "Education",
                            "Income Inequality",
                            "Percent Black",
                            "Percent Hispanic",
                            "2009",
                            "2010"))
