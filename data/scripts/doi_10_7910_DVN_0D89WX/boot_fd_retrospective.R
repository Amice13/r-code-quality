pp_white_all <- vector()
pp_black_all <- vector()
pp_white_dem <- vector()
pp_black_dem <- vector()
pp_white_rep <- vector()
pp_black_rep <- vector()

all_retro_full <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
           + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
             income50t75 + senior + under30 + someCollege + baDegree + 
             postGrad + rural + suburban + yr_factor,
           data = crimeData, family = "binomial")

demdata <- subset(crimeData, president %in% c("Clinton","Obama"))
demdata$yr_factor <- droplevels(demdata$yr_factor)
dem_retro_full <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
           + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
             income50t75 + senior + under30 + someCollege + baDegree + 
             postGrad + rural + suburban + yr_factor,
           data = demdata, 
           family = "binomial")

repdata <- subset(crimeData, president %in% c("Bush","Trump"))
repdata$yr_factor <- droplevels(repdata$yr_factor)
rep_retro_full <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
           + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
             income50t75 + senior + under30 + someCollege + baDegree + 
             postGrad + rural + suburban + yr_factor,
           data = repdata,
           family = "binomial")

synthwhite_all <- data.frame(copartisan = mean(crimeData$copartisan, na.rm=T),
                             independent = mean(crimeData$independent, na.rm=T),
                             crime_us_yr_ago = c(-1, 1),
                             black = 0,
                             crime_victimization = mean(crimeData$crime_victimization, na.rm=T),
                             chgCrimeRate = mean(crimeData$chgCrimeRate, na.rm=T),
                             male = mean(crimeData$male, na.rm=T),
                             incomeLt20 = mean(crimeData$incomeLt20, na.rm=T),
                             income20t30 = mean(crimeData$income20t30, na.rm=T),
                             income30t50 = mean(crimeData$income30t50, na.rm=T),
                             income50t75 = mean(crimeData$income50t75, na.rm=T),
                             senior = mean(crimeData$senior, na.rm=T),
                             under30 = mean(crimeData$under30, na.rm=T),
                             someCollege = mean(crimeData$someCollege, na.rm=T),
                             baDegree = mean(crimeData$baDegree, na.rm=T),
                             postGrad = mean(crimeData$postGrad, na.rm=T),
                             rural = mean(crimeData$rural, na.rm=T),
                             suburban = mean(crimeData$suburban, na.rm=T),
                             yr_factor = "2000")

synthblack_all <- data.frame(copartisan = mean(crimeData$copartisan, na.rm=T),
                             independent = mean(crimeData$independent, na.rm=T),
                             crime_us_yr_ago = c(-1, 1),
                             black = 1,
                             crime_victimization = mean(crimeData$crime_victimization, na.rm=T),
                             chgCrimeRate = mean(crimeData$chgCrimeRate, na.rm=T),
                             male = mean(crimeData$male, na.rm=T),
                             incomeLt20 = mean(crimeData$incomeLt20, na.rm=T),
                             income20t30 = mean(crimeData$income20t30, na.rm=T),
                             income30t50 = mean(crimeData$income30t50, na.rm=T),
                             income50t75 = mean(crimeData$income50t75, na.rm=T),
                             senior = mean(crimeData$senior, na.rm=T),
                             under30 = mean(crimeData$under30, na.rm=T),
                             someCollege = mean(crimeData$someCollege, na.rm=T),
                             baDegree = mean(crimeData$baDegree, na.rm=T),
                             postGrad = mean(crimeData$postGrad, na.rm=T),
                             rural = mean(crimeData$rural, na.rm=T),
                             suburban = mean(crimeData$suburban, na.rm=T),
                             yr_factor = "2000")

synthwhite_dem <- data.frame(copartisan = mean(demdata$copartisan, na.rm=T),
                             independent = mean(demdata$independent, na.rm=T),
                             crime_us_yr_ago = c(-1, 1),
                             black = 0,
                             crime_victimization = mean(demdata$crime_victimization, na.rm=T),
                             chgCrimeRate = mean(demdata$chgCrimeRate, na.rm=T),
                             male = mean(demdata$male, na.rm=T),
                             incomeLt20 = mean(demdata$incomeLt20, na.rm=T),
                             income20t30 = mean(demdata$income20t30, na.rm=T),
                             income30t50 = mean(demdata$income30t50, na.rm=T),
                             income50t75 = mean(demdata$income50t75, na.rm=T),
                             senior = mean(demdata$senior, na.rm=T),
                             under30 = mean(demdata$under30, na.rm=T),
                             someCollege = mean(demdata$someCollege, na.rm=T),
                             baDegree = mean(demdata$baDegree, na.rm=T),
                             postGrad = mean(demdata$postGrad, na.rm=T),
                             rural = mean(demdata$rural, na.rm=T),
                             suburban = mean(demdata$suburban, na.rm=T),
                             yr_factor = "2000")

synthblack_dem <- data.frame(copartisan = mean(demdata$copartisan, na.rm=T),
                             independent = mean(demdata$independent, na.rm=T),
                             crime_us_yr_ago = c(-1, 1),
                             black = 1,
                             crime_victimization = mean(demdata$crime_victimization, na.rm=T),
                             chgCrimeRate = mean(demdata$chgCrimeRate, na.rm=T),
                             male = mean(demdata$male, na.rm=T),
                             incomeLt20 = mean(demdata$incomeLt20, na.rm=T),
                             income20t30 = mean(demdata$income20t30, na.rm=T),
                             income30t50 = mean(demdata$income30t50, na.rm=T),
                             income50t75 = mean(demdata$income50t75, na.rm=T),
                             senior = mean(demdata$senior, na.rm=T),
                             under30 = mean(demdata$under30, na.rm=T),
                             someCollege = mean(demdata$someCollege, na.rm=T),
                             baDegree = mean(demdata$baDegree, na.rm=T),
                             postGrad = mean(demdata$postGrad, na.rm=T),
                             rural = mean(demdata$rural, na.rm=T),
                             suburban = mean(demdata$suburban, na.rm=T),
                             yr_factor = "2000")

synthwhite_rep <- data.frame(copartisan = mean(repdata$copartisan, na.rm=T),
                             independent = mean(repdata$independent, na.rm=T),
                             crime_us_yr_ago = c(-1, 1),
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
                             crime_us_yr_ago = c(-1, 1),
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


pp_white_all_full <- predict(all_retro_full, synthwhite_all, type = "response", se = F)[2] - predict(all_retro_full, synthwhite_all, type = "response", se = F)[1]
pp_black_all_full <- predict(all_retro_full, synthblack_all, type = "response", se = F)[2] - predict(all_retro_full, synthblack_all, type = "response", se = F)[1]

pp_white_dem_full <- predict(dem_retro_full, synthwhite_dem, type = "response", se = F)[2] - predict(dem_retro_full, synthwhite_dem, type = "response", se = F)[1]
pp_black_dem_full <- predict(dem_retro_full, synthblack_dem, type = "response", se = F)[2] - predict(dem_retro_full, synthblack_dem, type = "response", se = F)[1]

pp_white_rep_full <- predict(rep_retro_full, synthwhite_rep, type = "response", se = F)[2] - predict(rep_retro_full, synthwhite_rep, type = "response", se = F)[1]
pp_black_rep_full <- predict(rep_retro_full, synthblack_rep, type = "response", se = F)[2] - predict(rep_retro_full, synthblack_rep, type = "response", se = F)[1]



set.seed(220)
for(i in 1:1000){
  print(i)
  alldata_samp <- slice_sample(crimeData, n = nrow(crimeData), replace = T)
  all <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = alldata_samp, family = "binomial")
  
  demdata <- subset(crimeData, president %in% c("Clinton","Obama"))
  demdata$yr_factor <- droplevels(demdata$yr_factor)
  demdata_samp <- slice_sample(demdata, n = nrow(demdata), replace = T)
  
  dem <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = demdata_samp, 
             family = "binomial")
  
  repdata <- subset(crimeData, president %in% c("Bush","Trump"))
  repdata$yr_factor <- droplevels(repdata$yr_factor)
  repdata_samp <- slice_sample(repdata, n = nrow(repdata), replace = T)
  
  rep <- glm(presapp ~ copartisan + independent + crime_us_yr_ago * black + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = repdata_samp,
             family = "binomial")
  
  synthwhite_all <- data.frame(copartisan = mean(alldata_samp$copartisan, na.rm=T),
                               independent = mean(alldata_samp$independent, na.rm=T),
                               crime_us_yr_ago = c(-1, 1),
                               black = 0,
                               crime_victimization = mean(alldata_samp$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(alldata_samp$chgCrimeRate, na.rm=T),
                               male = mean(alldata_samp$male, na.rm=T),
                               incomeLt20 = mean(alldata_samp$incomeLt20, na.rm=T),
                               income20t30 = mean(alldata_samp$income20t30, na.rm=T),
                               income30t50 = mean(alldata_samp$income30t50, na.rm=T),
                               income50t75 = mean(alldata_samp$income50t75, na.rm=T),
                               senior = mean(alldata_samp$senior, na.rm=T),
                               under30 = mean(alldata_samp$under30, na.rm=T),
                               someCollege = mean(alldata_samp$someCollege, na.rm=T),
                               baDegree = mean(alldata_samp$baDegree, na.rm=T),
                               postGrad = mean(alldata_samp$postGrad, na.rm=T),
                               rural = mean(alldata_samp$rural, na.rm=T),
                               suburban = mean(alldata_samp$suburban, na.rm=T),
                               yr_factor = "2000")
  
  synthblack_all <- data.frame(copartisan = mean(alldata_samp$copartisan, na.rm=T),
                               independent = mean(alldata_samp$independent, na.rm=T),
                               crime_us_yr_ago = c(-1, 1),
                               black = 1,
                               crime_victimization = mean(alldata_samp$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(alldata_samp$chgCrimeRate, na.rm=T),
                               male = mean(alldata_samp$male, na.rm=T),
                               incomeLt20 = mean(alldata_samp$incomeLt20, na.rm=T),
                               income20t30 = mean(alldata_samp$income20t30, na.rm=T),
                               income30t50 = mean(alldata_samp$income30t50, na.rm=T),
                               income50t75 = mean(alldata_samp$income50t75, na.rm=T),
                               senior = mean(alldata_samp$senior, na.rm=T),
                               under30 = mean(alldata_samp$under30, na.rm=T),
                               someCollege = mean(alldata_samp$someCollege, na.rm=T),
                               baDegree = mean(alldata_samp$baDegree, na.rm=T),
                               postGrad = mean(alldata_samp$postGrad, na.rm=T),
                               rural = mean(alldata_samp$rural, na.rm=T),
                               suburban = mean(alldata_samp$suburban, na.rm=T),
                               yr_factor = "2000")
  
  synthwhite_dem <- data.frame(copartisan = mean(demdata_samp$copartisan, na.rm=T),
                               independent = mean(demdata_samp$independent, na.rm=T),
                               crime_us_yr_ago = c(-1, 1),
                               black = 0,
                               crime_victimization = mean(demdata_samp$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(demdata_samp$chgCrimeRate, na.rm=T),
                               male = mean(demdata_samp$male, na.rm=T),
                               incomeLt20 = mean(demdata_samp$incomeLt20, na.rm=T),
                               income20t30 = mean(demdata_samp$income20t30, na.rm=T),
                               income30t50 = mean(demdata_samp$income30t50, na.rm=T),
                               income50t75 = mean(demdata_samp$income50t75, na.rm=T),
                               senior = mean(demdata_samp$senior, na.rm=T),
                               under30 = mean(demdata_samp$under30, na.rm=T),
                               someCollege = mean(demdata_samp$someCollege, na.rm=T),
                               baDegree = mean(demdata_samp$baDegree, na.rm=T),
                               postGrad = mean(demdata_samp$postGrad, na.rm=T),
                               rural = mean(demdata_samp$rural, na.rm=T),
                               suburban = mean(demdata_samp$suburban, na.rm=T),
                               yr_factor = "2000")
  
  synthblack_dem <- data.frame(copartisan = mean(demdata_samp$copartisan, na.rm=T),
                               independent = mean(demdata_samp$independent, na.rm=T),
                               crime_us_yr_ago = c(-1, 1),
                               black = 1,
                               crime_victimization = mean(demdata_samp$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(demdata_samp$chgCrimeRate, na.rm=T),
                               male = mean(demdata_samp$male, na.rm=T),
                               incomeLt20 = mean(demdata_samp$incomeLt20, na.rm=T),
                               income20t30 = mean(demdata_samp$income20t30, na.rm=T),
                               income30t50 = mean(demdata_samp$income30t50, na.rm=T),
                               income50t75 = mean(demdata_samp$income50t75, na.rm=T),
                               senior = mean(demdata_samp$senior, na.rm=T),
                               under30 = mean(demdata_samp$under30, na.rm=T),
                               someCollege = mean(demdata_samp$someCollege, na.rm=T),
                               baDegree = mean(demdata_samp$baDegree, na.rm=T),
                               postGrad = mean(demdata_samp$postGrad, na.rm=T),
                               rural = mean(demdata_samp$rural, na.rm=T),
                               suburban = mean(demdata_samp$suburban, na.rm=T),
                               yr_factor = "2000")
  
  synthwhite_rep <- data.frame(copartisan = mean(repdata_samp$copartisan, na.rm=T),
                               independent = mean(repdata_samp$independent, na.rm=T),
                               crime_us_yr_ago = c(-1, 1),
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
                               crime_us_yr_ago = c(-1, 1),
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
  
  pp_white_all[i] <- predict(all, synthwhite_all, type = "response", se = F)[2] - predict(all, synthwhite_all, type = "response", se = F)[1]
  pp_black_all[i] <- predict(all, synthblack_all, type = "response", se = F)[2] -  predict(all, synthblack_all, type = "response", se = F)[1]
  
  pp_white_dem[i] <- predict(dem, synthwhite_dem, type = "response", se = F)[2] - predict(dem, synthwhite_dem, type = "response", se = F)[1]
  pp_black_dem[i] <- predict(dem, synthblack_dem, type = "response", se = F)[2] - predict(dem, synthblack_dem, type = "response", se = F)[1]
  
  pp_white_rep[i] <- predict(rep, synthwhite_rep, type = "response", se = F)[2] - predict(rep, synthwhite_rep, type = "response", se = F)[1]
  pp_black_rep[i] <- predict(rep, synthblack_rep, type = "response", se = F)[2] - predict(rep, synthblack_rep, type = "response", se = F)[1]
}

pp_white_all_q <- quantile(pp_white_all, c(.025, .5, .975))
pp_white_all_q[2] <- pp_white_all_full
pp_black_all_q <- quantile(pp_black_all, c(.025, .5, .975))
pp_black_all_q[2] <- pp_black_all_full

preds_all <- data.frame(pp = c(pp_white_all_q[2], pp_black_all_q[2]),
                        high = c(pp_white_all_q[3], pp_black_all_q[3]),
                        low = c(pp_white_all_q[1], pp_black_all_q[1]),
                        president = "All",
                        race = rep(c("White","Black"), each = 1))

pp_white_dem_q <- quantile(pp_white_dem, c(.025, .5, .975))
pp_white_dem_q[2] <- pp_white_dem_full
pp_black_dem_q <- quantile(pp_black_dem, c(.025, .5, .975))
pp_black_dem_q[2] <- pp_black_dem_full

preds_dem <- data.frame(pp = c(pp_white_dem_q[2], pp_black_dem_q[2]),
                        high = c(pp_white_dem_q[3], pp_black_dem_q[3]),
                        low = c(pp_white_dem_q[1], pp_black_dem_q[1]),
                        president = "Clinton/Obama",
                        race = rep(c("White","Black"), each = 1))

pp_white_rep_q <- quantile(pp_white_rep, c(.025, .5, .975))
pp_white_rep_q[2] <- pp_white_rep_full
pp_black_rep_q <- quantile(pp_black_rep, c(.025, .5, .975))
pp_black_rep_q[2] <- pp_black_rep_full

preds_rep <- data.frame(pp = c(pp_white_rep_q[2], pp_black_rep_q[2]),
                        high = c(pp_white_rep_q[3], pp_black_rep_q[3]),
                        low = c(pp_white_rep_q[1], pp_black_rep_q[1]),
                        president = "Bush/Trump",
                        race = rep(c("White","Black"), each = 1))

preds_stack <- bind_rows(preds_all, preds_dem, preds_rep)

order <- c("All","Clinton/Obama","Bush/Trump")
retro_fd <- arrange(transform(preds_stack, president = factor(president, levels = order)))
