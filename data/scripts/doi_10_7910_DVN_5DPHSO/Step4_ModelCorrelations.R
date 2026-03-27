#Code to model observed correlations and use model to impute missing correlations

#Change the working directory as needed
setwd("/Users/djd78/Box/Belief Networks/Replication Materials/Results")

rm(list=ls())

require(doBy)
require(lme4)
require(plyr)

load(file="all_correlations.saved")
r = subset(r, !(is.na(r$abs_c))) #reduce to those for which correlations could be computed
min = 5 #minimum number of appearances in editions of GSS for each correlation-pair 

####################################################################
#Mixed-effects model of observed correlations

r$j=as.factor(r$j)
r$year72_dec = r$year72/10 #year variable starting at 1972 and scaled to decades

#reduce to correlation-pairs that appear enough times to meet threshold
r$count = 1
s = summaryBy(year72_dec+count ~ j, data=r, FUN=c(mean, sum))
r$num_years=s$count.sum[match(r$j, s$j)]
r=subset(r, r$num_years>=min)

#mean-center year variable 
r$yearcen = scale(r$year72_dec, center = T, scale = F)

save(r, file="modelinput.saved")

#save a list of years for convenient use later 
yearlist = unique(r$year)
save(yearlist, file="yearlist.saved")

#run models
m=lmer(abs_c~yearcen+(1+yearcen|j), data=r, control = lmerControl(optimizer ="Nelder_Mead"))
save(m, file="lmermodel1.saved")
rm(m)

m2 = lmer(abs_cpart~yearcen+(1+yearcen|j), data=r, control = lmerControl(optimizer ="Nelder_Mead"))
save(m2, file="lmermodel2.saved")
rm(m2)
###########################################
#Use model to impute missing correlations and create yearly data sets
load(file="lmermodel1.saved")
load(file="lmermodel2.saved")
load(file="modelinput.saved")

master = summaryBy(count ~ j, data=r, FUN="sum") # master set of unique item-pairs

for(ye in 1972:2016) {
  print(ye)
  sub=subset(r, year==ye) #subset of observed correlations for that year
  e = master #set up for predictions
  if(!(empty(sub))) {
    e$year=ye
    e$yearcen = sub$yearcen[1]
    e$c_est = predict(m, newdata=e) #model-estimated correlation
    e$c_obs = sub$abs_c[match(e$j, sub$j)] #observed correlation (where available)
    
    e$cpart_est = predict(m2, newdata=e, allow.new.levels=T) #model-estimated partial correlation
    e$cpart_EXISTS = as.character(e$j) %in% rownames(ranef(m2)$j) 
    e$cpart_est[e$cpart_EXISTS==F]=NA #only record estimate if correlation-pair appeared in partial-correlation model 
    e$cpart_obs = sub$abs_cpart[match(e$j, sub$j)] #observed partial correlation (where available)
    
    e = e[, c("j", "year", "c_est", "c_obs", "cpart_est", "cpart_obs")]
    output = paste("corr_predictions_", ye, ".saved", sep="")
    save(e, file = output)
  }
}
