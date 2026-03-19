#because the data is in stata 2013 format, we need a special package called readstata13
library(readstata13)
library(gdata)
leader = read.dta13("Cross-national leaders.dta")
congress = read.dta13("US Congress.dta")
education = read.xls("us_education.xls")
control = read.xls("control.xls")

#subset data to only include rows where first congress = 1
first = which(congress$first_congress == 1)
model.one = congress[first,]

#subset data to only include rows of those elected from 1901 on
v.46 = which(model.one$V46 >= 901)
model.one = model.one[v.46,]

#subset data so year left congress is >=0
v.47 = which(model.one$V47 >= 0)
model.one = model.one[v.47,]

#get rid of NA values in close
close = which(model.one$close >= 0)
model.one = model.one[close,]

#eliminate entries where there is no state information
no_state = which(is.na(model.one$state_1)==FALSE)
model.one = model.one[no_state,]

model.one[2417,5] = 932

#calculate slope between average education level datapoints
slope = NULL
for(i in 1:nrow(education)-1){
  slope[i] = (education[i+1,2] - education[i,2])/(education[i+1,1] - education[i,1])
}

slope[21] = 0
education$slope = slope

#calculate the average ed level for each congressman
output = NULL
for(i in 1:nrow(model.one)){
  #year of interest we want to calculate slope for
  value = (model.one[i,]$V46 + model.one[i,]$V47)/2
  if(value < 910){
    output[i] = education$College[1]
  }else{
    #match year of interest to nearest year in education control data
    sub_year = which(education$Year <= value)
    sub_year_value = sub_year[length(sub_year)]
    output[i] = education$College[sub_year_value] + (value - education$Year[sub_year_value])*slope[sub_year_value]
  }
}

#append output onto dataset
model.one$ed = output

#subset data to only include close elections
close.data = model.one[which(model.one$close==1),]

#create manual years in office
close.data['years_manual'] = close.data[,5] - close.data[,4]

#create our categorical variables
v46.f = factor(close.data$V46)
v47.f = factor(close.data$V47)

v46.f_full = factor(model.one$V46)
v47.f_full = factor(model.one$V47)

chamber.f = factor(close.data$chamber)

### YEARS MODEL ###

#interact with education
years_interact = lm(close.data$V48 ~ I(close.data$college*close.data$ed) + close.data$college + close.data$ed + chamber.f + close.data$state_1 + close.data$state_2 +close.data$state_3+close.data$state_4+close.data$state_5+close.data$state_6+close.data$state_7+close.data$state_8+close.data$state_9+close.data$state_10+close.data$state_11+close.data$state_12+close.data$state_13+close.data$state_14+close.data$state_15+close.data$state_16+close.data$state_17+close.data$state_18+close.data$state_19+close.data$state_20+close.data$state_21+close.data$state_22+close.data$state_23+close.data$state_24+close.data$state_25+close.data$state_26+close.data$state_27+close.data$state_28+close.data$state_29+close.data$state_30+close.data$state_31+close.data$state_32+close.data$state_33+close.data$state_34+close.data$state_35+close.data$state_36+close.data$state_37+close.data$state_38+close.data$state_39+close.data$state_40+close.data$state_41+close.data$state_42+close.data$state_43+close.data$state_44+close.data$state_45+close.data$state_46+close.data$state_47+close.data$state_48+close.data$state_49+close.data$state_50)
summary(years_interact)

#bootstrap it
set.seed(5432)
years_coef = NULL
years_rep_interact = NULL
years_rep_college = NULL
years_rep_ed = NULL
for(i in 1:10000){
  #create a random sample of data set that is the size of the original dataset
  my.samp.rows <- sample(1:nrow(close.data),nrow(close.data),replace=T)
  my.samp <- close.data[my.samp.rows,]
  
  #run the regression and store coefficients of interest (those that contain college)
  samp_chamber_fact = factor(my.samp$chamber)
  years_replace = lm(my.samp$V48 ~ I(my.samp$college*my.samp$ed) + my.samp$college + my.samp$ed + samp_chamber_fact + my.samp$state_1 + my.samp$state_2 +my.samp$state_3+my.samp$state_4+my.samp$state_5+my.samp$state_6+my.samp$state_7+my.samp$state_8+my.samp$state_9+my.samp$state_10+my.samp$state_11+my.samp$state_12+my.samp$state_13+my.samp$state_14+my.samp$state_15+my.samp$state_16+my.samp$state_17+my.samp$state_18+my.samp$state_19+my.samp$state_20+my.samp$state_21+my.samp$state_22+my.samp$state_23+my.samp$state_24+my.samp$state_25+my.samp$state_26+my.samp$state_27+my.samp$state_28+my.samp$state_29+my.samp$state_30+my.samp$state_31+my.samp$state_32+my.samp$state_33+my.samp$state_34+my.samp$state_35+my.samp$state_36+my.samp$state_37+my.samp$state_38+my.samp$state_39+my.samp$state_40+my.samp$state_41+my.samp$state_42+my.samp$state_43+my.samp$state_44+my.samp$state_45+my.samp$state_46+my.samp$state_47+my.samp$state_48+my.samp$state_49+my.samp$state_50)
  years_rep_interact[i] =  years_replace$coefficients[2]
  years_rep_college[i] =  years_replace$coefficients[3]
  #years_rep_ed[i] = years_replace$coefficients[4]
}

#write a function to calculate marginal effect
marginal_effect = function(x){
  m = mean(years_rep_college) + mean(years_rep_interact)*x
  return(m)
}
#write functions to calculate upper and lower bounds
lower_bound = function(x){
  l = mean(years_rep_college) + mean(years_rep_interact)*x - 1.96*sd(years_rep_college+years_rep_interact)
}
upper_bound = function(x){
  l = mean(years_rep_college) + mean(years_rep_interact)*x + 1.96*sd(years_rep_college+years_rep_interact)
}

#plot marginal effect
curve(marginal_effect,from = min(close.data$ed), to = max(close.data$ed), xlab = "Percent of Americans with College Degree", ylab = "Marginal Effect",ylim = c(-17,5), lwd=3)
#plot confidence intervals
curve(lower_bound,from = min(close.data$ed), to = max(close.data$ed),add=T,lty=2,col=240, lwd=2)
curve(upper_bound,from = min(close.data$ed), to = max(close.data$ed),add=T,lty=2,col=240, lwd=2)
#plot the 0 lines
abline(h=0, lty=3, lwd=2)

#plot the vertical line for average education level across the time period
#abline(v=mean(close.data$ed))
#plot(density(years_coef))
plot(density(years_rep_interact))
plot(density(years_rep_college))

plot(density(close.data$ed))
#create box plots for education trends of non-college and college educated
yescol <-close.data[which(close.data$college==1),]$ed
nocol <- close.data[which(close.data$college==0),]$ed
boxplot(yescol,nocol, horizontal=TRUE, names=c("Yes","No"), ylab="College Degree",xlab="% of US with College Degree")

### LOST MODEL ###
#model with interaction terms
lost_interact = glm(close.data$lostelection ~ I(close.data$college*close.data$ed) + close.data$chamber + close.data$state_1 + close.data$state_2 +close.data$state_3+close.data$state_4+close.data$state_5+close.data$state_6+close.data$state_7+close.data$state_8+close.data$state_9+close.data$state_10+close.data$state_11+close.data$state_12+close.data$state_13+close.data$state_14+close.data$state_15+close.data$state_16+close.data$state_17+close.data$state_18+close.data$state_19+close.data$state_20+close.data$state_21+close.data$state_22+close.data$state_23+close.data$state_24+close.data$state_25+close.data$state_26+close.data$state_27+close.data$state_28+close.data$state_29+close.data$state_30+close.data$state_31+close.data$state_32+close.data$state_33+close.data$state_34+close.data$state_35+close.data$state_36+close.data$state_37+close.data$state_38+close.data$state_39+close.data$state_40+close.data$state_41+close.data$state_42+close.data$state_43+close.data$state_44+close.data$state_45+close.data$state_46+close.data$state_47+close.data$state_48+close.data$state_49+close.data$state_50,family=binomial(link = "logit"))
summary(lost_interact)
#model just by adding education term
lost_add = glm(close.data$lostelection ~ close.data$college + close.data$ed + close.data$chamber + close.data$state_1 + close.data$state_2 +close.data$state_3+close.data$state_4+close.data$state_5+close.data$state_6+close.data$state_7+close.data$state_8+close.data$state_9+close.data$state_10+close.data$state_11+close.data$state_12+close.data$state_13+close.data$state_14+close.data$state_15+close.data$state_16+close.data$state_17+close.data$state_18+close.data$state_19+close.data$state_20+close.data$state_21+close.data$state_22+close.data$state_23+close.data$state_24+close.data$state_25+close.data$state_26+close.data$state_27+close.data$state_28+close.data$state_29+close.data$state_30+close.data$state_31+close.data$state_32+close.data$state_33+close.data$state_34+close.data$state_35+close.data$state_36+close.data$state_37+close.data$state_38+close.data$state_39+close.data$state_40+close.data$state_41+close.data$state_42+close.data$state_43+close.data$state_44+close.data$state_45+close.data$state_46+close.data$state_47+close.data$state_48+close.data$state_49+close.data$state_50,family=binomial(link = "logit"))
summary(lost_add)

#model with interaction terms and a logit model
chamber.f = factor(close.data$chamber)
lost_interact_logit = glm( lostelection ~ I(college*ed) +  college +  ed + chamber.f +  state_2 + state_3 + state_5+ state_6+ state_7+ state_8+ state_9+ state_10+ state_11+ state_12+ state_13+ state_14+ state_15+ state_16+ state_17+ state_18+ state_19+ state_20+ state_21+ state_22+ state_23+ state_24+ state_25+ state_26+ state_27+ state_28+ state_29+ state_30+ state_31+ state_32+ state_33+ state_34+ state_35+ state_36+ state_37+ state_38+ state_39+ state_41+ state_42+ state_43+ state_44+ state_45+ state_46+ state_47+ state_48+ state_49, data=close.data, family=binomial(link = "logit"))
summary(lost_interact_logit)

#rearrange dataframe to allow calculation of difference in means graphic
yes_col = close.data
yes_col = yes_col[,-c(1:6,8,11,47,57:58,61:68)]
yes_col = yes_col[,c(48,50,1:47)]
no_col = yes_col
yes_col[,1] = 1
no_col[,1] = 0
yes_col = cbind(yes_col[,1]*yes_col[,2],yes_col)
no_col = cbind(no_col[,1]*no_col[,1],no_col)
yes_col = cbind(1, yes_col)
no_col = cbind(1, no_col)


#logit predictions
yes_logit = predict(lost_interact_logit, yes_col, type="response")
no_logit = predict(lost_interact_logit, no_col, type="response")

#ols predictions
yes_ols = predict(lost_interact_logit, yes_col, type="response")
no_ols = predict(lost_interact_logit, no_col, type="response")

plot(density(yes_logit))
lines(density(no_logit), add=T)

plot(density(yes_ols))
lines(density(no_ols), add=T)

point_est <- mean(yes_logit) - mean(no_logit)

vcov_matrix = vcov(lost_interact_logit)

#run simulations for predicted means
library(mvtnorm)
set.seed(1234)
sim.betas <- rmvnorm(n = 1000,
                     mean = lost_interact_logit$coefficients,
                     sigma = vcov_matrix)
yes_col = as.matrix(yes_col)
no_col = as.matrix(no_col)
yesones_matrix <- NULL
yeszeros_matrix <- NULL
diff <- NULL

# for loop to determine which rows need to be changed, calculate means
for(i in 1:nrow(sim.betas)){
  yes_exp <- exp(yes_col %*% (as.matrix(sim.betas[i,])))
  yes_ones =  which(yes_exp==Inf)
  yes_zeros = which(yes_exp == -Inf)
  yes <- exp(yes_col %*% (as.matrix(sim.betas[i,])))/(1+exp(yes_col %*% (as.matrix(sim.betas[i,]))))
  yes[yes_ones,] = 1
  yes[yes_zeros,] = 0
  
  no_exp <- exp(no_col %*% (as.matrix(sim.betas[i,])))
  no_ones =  which(no_exp==Inf)
  no_zeros = which(no_exp == -Inf)
  no <- exp(no_col %*% (as.matrix(sim.betas[i,])))/(1+exp(no_col %*% (as.matrix(sim.betas[i,]))))
  no[no_ones,] = 1
  no[no_zeros,] = 0
  
  diff[i] = mean(yes) - mean(no)
}
hist(diff, col=NULL, breaks=20)

plot(density(diff))
mean(diff)
sd(diff)

### BILLS MODEL ###

###Bills > 0 data table 

#College, close
cc = dim(model.one[model.one$bills_enacted_AVG>0 & model.one$college==1 & model.one$close==1,])[1]
cc
#College, no close
cn = dim(model.one[model.one$bills_enacted_AVG>0 & model.one$college==1 & model.one$close==0,])[1]
cn
#No College, close
nc = dim(model.one[model.one$bills_enacted_AVG>0 & model.one$college==0 & model.one$close==1,])[1]
nc
#No College, no close
nn = dim(model.one[model.one$bills_enacted_AVG>0 & model.one$college==0 & model.one$close==0,])[1]
nn

bills_interact = lm(close.data$bills_enacted_AVG ~ I(close.data$college*close.data$ed) + close.data$college + close.data$ed + chamber.f + close.data$state_1 + close.data$state_2 +close.data$state_3+close.data$state_4+close.data$state_5+close.data$state_6+close.data$state_7+close.data$state_8+close.data$state_9+close.data$state_10+close.data$state_11+close.data$state_12+close.data$state_13+close.data$state_14+close.data$state_15+close.data$state_16+close.data$state_17+close.data$state_18+close.data$state_19+close.data$state_20+close.data$state_21+close.data$state_22+close.data$state_23+close.data$state_24+close.data$state_25+close.data$state_26+close.data$state_27+close.data$state_28+close.data$state_29+close.data$state_30+close.data$state_31+close.data$state_32+close.data$state_33+close.data$state_34+close.data$state_35+close.data$state_36+close.data$state_37+close.data$state_38+close.data$state_39+close.data$state_40+close.data$state_41+close.data$state_42+close.data$state_43+close.data$state_44+close.data$state_45+close.data$state_46+close.data$state_47+close.data$state_48+close.data$state_49+close.data$state_50)
summary(bills_interact)


