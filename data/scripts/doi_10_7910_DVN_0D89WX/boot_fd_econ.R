pp_white_rep <- vector()
pp_black_rep <- vector()

repdata <- subset(crimeData, president %in% c("Bush","Trump"))
repdata$yr_factor <- droplevels(repdata$yr_factor)
rep_retro_full <- glm(presapp ~ copartisan + independent + econ_retro_rev * black + crime_victimization
           + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
             income50t75 + senior + under30 + someCollege + baDegree + 
             postGrad + rural + suburban + yr_factor,
           data = repdata,
           family = "binomial")

synthwhite_rep <- data.frame(copartisan = mean(repdata$copartisan, na.rm=T),
                             independent = mean(repdata$independent, na.rm=T),
                             econ_retro_rev = c(-1, 1),
                             black = 0,
                             crime_victimization = mean(repdata$crime_victimization, na.rm=T),
                             chgCrimeRate = mean(repdata$chgCrimeRate, na.rm=T),
                             male = mean(repdata$male, na.rm=T),
                             incomeLt20 = mean(repdata$incomeLt20, na.rm=T),
                             income20t30 = mean(repdata$income20t30, na.rm=T),
                             income30t50 = mean(repdata$income30t50, na.rm=T),
                             income50t75 = mean(repdata$income50t75, na.rm=T),
                             senior = mean(repdata$senior, na.rm=T),
                             under30 = mean(repdata$under30, na.rm=T),
                             someCollege = mean(repdata$someCollege, na.rm=T),
                             baDegree = mean(repdata$baDegree, na.rm=T),
                             postGrad = mean(repdata$postGrad, na.rm=T),
                             rural = mean(repdata$rural, na.rm=T),
                             suburban = mean(repdata$suburban, na.rm=T),
                             yr_factor = "2019")

synthblack_rep <- data.frame(copartisan = mean(repdata$copartisan, na.rm=T),
                             independent = mean(repdata$independent, na.rm=T),
                             econ_retro_rev = c(-1, 1),
                             black = 1,
                             crime_victimization = mean(repdata$crime_victimization, na.rm=T),
                             chgCrimeRate = mean(repdata$chgCrimeRate, na.rm=T),
                             male = mean(repdata$male, na.rm=T),
                             incomeLt20 = mean(repdata$incomeLt20, na.rm=T),
                             income20t30 = mean(repdata$income20t30, na.rm=T),
                             income30t50 = mean(repdata$income30t50, na.rm=T),
                             income50t75 = mean(repdata$income50t75, na.rm=T),
                             senior = mean(repdata$senior, na.rm=T),
                             under30 = mean(repdata$under30, na.rm=T),
                             someCollege = mean(repdata$someCollege, na.rm=T),
                             baDegree = mean(repdata$baDegree, na.rm=T),
                             postGrad = mean(repdata$postGrad, na.rm=T),
                             rural = mean(repdata$rural, na.rm=T),
                             suburban = mean(repdata$suburban, na.rm=T),
                             yr_factor = "2019")

pp_white_rep_full <- predict(rep_retro_full, synthwhite_rep, type = "response", se = F)[2] - predict(rep_retro_full, synthwhite_rep, type = "response", se = F)[1]
pp_black_rep_full <- predict(rep_retro_full, synthblack_rep, type = "response", se = F)[2] - predict(rep_retro_full, synthblack_rep, type = "response", se = F)[1]



set.seed(220)
for(i in 1:1000){
  print(i)
  
  repdata <- subset(crimeData, president %in% c("Bush","Trump"))
  repdata$yr_factor <- droplevels(repdata$yr_factor)
  repdata_samp <- slice_sample(repdata, n = nrow(repdata), replace = T)
  
  rep <- glm(presapp ~ copartisan + independent + econ_retro_rev * black + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = repdata_samp,
             family = "binomial")
  
  synthwhite_rep <- data.frame(copartisan = mean(repdata_samp$copartisan, na.rm=T),
                               independent = mean(repdata_samp$independent, na.rm=T),
                               econ_retro_rev = c(-1, 1),
                               black = 0,
                               crime_victimization = mean(repdata_samp$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(repdata_samp$chgCrimeRate, na.rm=T),
                               male = mean(repdata_samp$male, na.rm=T),
                               incomeLt20 = mean(repdata_samp$incomeLt20, na.rm=T),
                               income20t30 = mean(repdata_samp$income20t30, na.rm=T),
                               income30t50 = mean(repdata_samp$income30t50, na.rm=T),
                               income50t75 = mean(repdata_samp$income50t75, na.rm=T),
                               senior = mean(repdata_samp$senior, na.rm=T),
                               under30 = mean(repdata_samp$under30, na.rm=T),
                               someCollege = mean(repdata_samp$someCollege, na.rm=T),
                               baDegree = mean(repdata_samp$baDegree, na.rm=T),
                               postGrad = mean(repdata_samp$postGrad, na.rm=T),
                               rural = mean(repdata_samp$rural, na.rm=T),
                               suburban = mean(repdata_samp$suburban, na.rm=T),
                               yr_factor = "2019")
  
  synthblack_rep <- data.frame(copartisan = mean(repdata_samp$copartisan, na.rm=T),
                               independent = mean(repdata_samp$independent, na.rm=T),
                               econ_retro_rev = c(-1, 1),
                               black = 1,
                               crime_victimization = mean(repdata_samp$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(repdata_samp$chgCrimeRate, na.rm=T),
                               male = mean(repdata_samp$male, na.rm=T),
                               incomeLt20 = mean(repdata_samp$incomeLt20, na.rm=T),
                               income20t30 = mean(repdata_samp$income20t30, na.rm=T),
                               income30t50 = mean(repdata_samp$income30t50, na.rm=T),
                               income50t75 = mean(repdata_samp$income50t75, na.rm=T),
                               senior = mean(repdata_samp$senior, na.rm=T),
                               under30 = mean(repdata_samp$under30, na.rm=T),
                               someCollege = mean(repdata_samp$someCollege, na.rm=T),
                               baDegree = mean(repdata_samp$baDegree, na.rm=T),
                               postGrad = mean(repdata_samp$postGrad, na.rm=T),
                               rural = mean(repdata_samp$rural, na.rm=T),
                               suburban = mean(repdata_samp$suburban, na.rm=T),
                               yr_factor = "2019")
  
  pp_white_rep[i] <- predict(rep, synthwhite_rep, type = "response", se = F)[2] - predict(rep, synthwhite_rep, type = "response", se = F)[1]
  pp_black_rep[i] <- predict(rep, synthblack_rep, type = "response", se = F)[2] - predict(rep, synthblack_rep, type = "response", se = F)[1]
}

pp_white_rep_q <- quantile(pp_white_rep, c(.025, .5, .975))
pp_white_rep_q[2] <- pp_white_rep_full
pp_black_rep_q <- quantile(pp_black_rep, c(.025, .5, .975))
pp_black_rep_q[2] <- pp_black_rep_full

preds_rep <- data.frame(pp = c(pp_white_rep_q[2], pp_black_rep_q[2]),
                        high = c(pp_white_rep_q[3], pp_black_rep_q[3]),
                        low = c(pp_white_rep_q[1], pp_black_rep_q[1]),
                        president = "Bush/Trump",
                        race = rep(c("White","Black"), each = 1))
