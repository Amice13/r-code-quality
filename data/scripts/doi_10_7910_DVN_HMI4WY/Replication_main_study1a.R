# Libraries
install.packages(c("texreg","plyr"))
library(texreg)
library(plyr)

# Load the data
load("Study1a.RData")

######################
### Numbers of observations
#####################

temp <- subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional")

length(na.omit(unique(temp$country))) # 10 countries

length(na.omit(unique(temp$country_year))) # 21 elections

temp$country_party <- paste(substr(temp$country_year_party,1,2),substr(temp$country_year_party,9,9),sep="_")
length(unique(temp$country_party)) # 65 parties

######################
### Figure 1
#####################

summary(study1a$neg_share_all_other_only)

pdf(file="plot_dist.pdf")
hist(study1a$neg_share_all_other_only,xlab="Share of valence attack",ylim=c(0,80),main="",cex.lab=1.3)
dev.off()

#######################
### Table 1
#######################

lm_binary_inter <- lm(pervote_change ~ neg_share_all_other_only*lr  + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

lm_cont_inter <- lm(pervote_change ~ neg_share_all_other_only*originalCMP_rile_inverted + total_counts, data=subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional"))

texreg(list(lm_binary_inter, lm_cont_inter), stars=c(0.01,0.05,0.1))

#######################
### Conditional coefficient for first model
#######################

# Create variance-covariance matrix object
covMat <- vcov(lm_binary_inter)

# Extract the data frame of the model
mod_frame <- model.frame(lm_binary_inter)

# Get coefficients of variables
beta_1 <- lm_binary_inter$coefficients[["neg_share_all_other_only"]]
beta_3 <- lm_binary_inter$coefficients[["neg_share_all_other_only:lrl"]]

# Create list of moderator values at which marginal effect is evaluated
x_2 <- c(0,1)

# Compute marginal effects
delta_1 <- beta_1 + beta_3*x_2

# Compute variances
var_1 <- covMat["neg_share_all_other_only","neg_share_all_other_only"] + (x_2^2)*covMat["neg_share_all_other_only:lrl", "neg_share_all_other_only:lrl"] + 2*x_2*covMat["neg_share_all_other_only", "neg_share_all_other_only:lrl"]

# Standard errors
se_1 <- sqrt(var_1)

# Upper and lower confidence bounds
z_score <- qnorm(1 - ((1 - 0.95)/2))
upper_bound <- delta_1 + z_score*se_1
lower_bound <- delta_1 - z_score*se_1

# Conditional coefficient for leftist party
delta_1[2]

# Standard error
se_1[2]

# p-value
-8.90/4.57
pnorm(-8.90/4.57)*2

#######################
### Substantive effect size discussion for first model
#######################

# Figure out mean and sd of valence attack
mean_va <- mean(subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional")$neg_share_all_other_only,na.rm=T)
sd_va <- sd(subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional")$neg_share_all_other_only,na.rm=T)

# Predicted values
predict(lm_binary_inter, data.frame(neg_share_all_other_only=mean_va-sd_va, lr="l",total_counts=mean(lm_binary_inter$model$total_counts)),se.fit=T,type="response")
predict(lm_binary_inter, data.frame(neg_share_all_other_only=1, lr="l",total_counts=mean(lm_binary_inter$model$total_counts)),se.fit=T,type="response")

# Average change in change in vote share
temp <- subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional")[,c("country","country_year","year","CMPcode","pervote_change")]
temp <- temp[which(complete.cases(temp)),]
temp <- arrange(temp,CMPcode,year)
temp_list <- split(temp,temp$CMPcode)
mean(unlist(lapply(temp_list, function(x){diff(x$pervote_change)})))

######################
#### Figure 2
#######################

interaction_plot_continuous <- function(model, effect, moderator, interaction, varcov="default", minimum="min", maximum="max", ymin="min", ymax="max", incr="default", num_points = 50, conf=.95, mean=FALSE, median=FALSE, alph=80, rugplot=T, histogram=T, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient", ...){
  
  # Define a function to make colors transparent
  makeTransparent<-function(someColor, alpha=alph){
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Set range of the moderator variable
  # Minimum
  if (minimum == "min"){
    min_val = min(mod_frame[[moderator]])
  }else{
    min_val = minimum
  }
  # Maximum
  if (maximum == "max"){
    max_val = max(mod_frame[[moderator]])
  }else{
    max_val = maximum
  }
  
  # Check if minimum smaller than maximum
  if (min_val > max_val){
    stop("Error: Minimum moderator value greater than maximum value.")
  }
  
  # Determine intervals between values of the moderator
  if (incr == "default"){
    increment = (max_val - min_val)/(num_points - 1)
  }else{
    increment = incr
  }
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- seq(from=min_val, to=max_val, by=increment)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  if (ymax == "max"){
    max_y = max(upper_bound)
  }else{
    max_y = ymax
  }
  if (ymin == "min"){
    min_y = min(lower_bound)
  }else{
    min_y = ymin
  }
  
  # Make the histogram color
  hist_col = makeTransparent("grey")
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(min_val, max_val), xlab=xlabel, ylab=ylabel, main=title, cex.lab=1.3)
  
  # Plot estimated effects
  lines(y=delta_1, x=x_2)
  lines(y=upper_bound, x=x_2, lty=2)
  lines(y=lower_bound, x=x_2, lty=2)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3)
  
  # Add a vertical line at the mean
  if (mean){
    abline(v = mean(mod_frame[[moderator]]), lty=2, col="red")
  }
  
  # Add a vertical line at the median
  if (median){
    abline(v = median(mod_frame[[moderator]]), lty=3, col="blue")
  }
  
  # Add Histogram (Histogram only plots when minimum and maximum are the min/max of the moderator)
  if (histogram & minimum=="min" & maximum=="max"){
    par(new=T)
    hist(mod_frame[[moderator]], axes=F, xlab="", ylab="",main="", border=hist_col, col=hist_col)
  }
}

pdf(file="plot_me_continuous.pdf")
interaction_plot_continuous(lm_cont_inter, "neg_share_all_other_only", "originalCMP_rile_inverted", "neg_share_all_other_only:originalCMP_rile_inverted", xlabel="Leftist ideology", ylabel="Marginal effect of valence attack",title="")
dev.off()

#######################
### Substantive effect size discussion for second model
#######################

summary(subset(study1a, parfam_subject!="sip special issue"& parfam_subject!="eth ethnic-regional")$originalCMP_rile_inverted) # 3rd quartile is 16.63

# Predicted values
predict(lm_cont_inter, data.frame(neg_share_all_other_only=mean_va-sd_va, originalCMP_rile_inverted=16.63,total_counts=mean(lm_cont_inter$model$total_counts)),se.fit=T,type="response")
predict(lm_cont_inter, data.frame(neg_share_all_other_only=1, originalCMP_rile_inverted=16.63,total_counts=mean(lm_cont_inter$model$total_counts)),se.fit=T,type="response")
