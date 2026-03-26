pp_white_all <- vector()
pp_black_all <- vector()
pp_white_dem <- vector()
pp_black_dem <- vector()
pp_white_rep <- vector()
pp_black_rep <- vector()

demdata <- subset(crimeData, president %in% c("Clinton","Obama"))
demdata$yr_factor <- droplevels(demdata$yr_factor)

repdata <- subset(crimeData, president %in% c("Bush","Trump"))
repdata$yr_factor <- droplevels(repdata$yr_factor)

all_pres_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + chgCrimeRate + 
                           male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                           someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                         data = crimeData, 
                         family = 'binomial')

dem_pres_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + chgCrimeRate + 
                           male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                           someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                         data = demdata, 
                         family = 'binomial')

rep_pres_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization + chgCrimeRate + 
                           male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                           someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                         data = repdata, 
                         family = 'binomial')

synthwhite_all <- data.frame(copartisan = mean(crimeData$copartisan, na.rm=T),
                             independent = mean(crimeData$independent, na.rm=T),
                             worryScale = seq(0,5,5),
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
                             worryScale = seq(0,5,5),
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
                             worryScale = seq(0,5,5),
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
                             worryScale = seq(0,5,5),
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
                             worryScale = seq(0,5,5),
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
                             worryScale = seq(0,5,5),
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


pp_white_all_full <- predict(all_pres_mod_full, synthwhite_all, type = "response", se = F)[2] - predict(all_pres_mod_full, synthwhite_all, type = "response", se = F)[1]
pp_black_all_full <- predict(all_pres_mod_full, synthblack_all, type = "response", se = F)[2] - predict(all_pres_mod_full, synthblack_all, type = "response", se = F)[1]

pp_white_dem_full <- predict(dem_pres_mod_full, synthwhite_dem, type = "response", se = F)[2] - predict(dem_pres_mod_full, synthwhite_dem, type = "response", se = F)[1]
pp_black_dem_full <- predict(dem_pres_mod_full, synthblack_dem, type = "response", se = F)[2] - predict(dem_pres_mod_full, synthblack_dem, type = "response", se = F)[1]

pp_white_rep_full <- predict(rep_pres_mod_full, synthwhite_rep, type = "response", se = F)[2] - predict(rep_pres_mod_full, synthwhite_rep, type = "response", se = F)[1]
pp_black_rep_full <- predict(rep_pres_mod_full, synthblack_rep, type = "response", se = F)[2] - predict(rep_pres_mod_full, synthblack_rep, type = "response", se = F)[1]


set.seed(220)
for(i in 1:1000){
  print(i)
  
  all_dat_i <- slice_sample(crimeData, n = nrow(crimeData), replace = T)
  all <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = all_dat_i, 
             family = "binomial")
  
  dem_dat_i <- slice_sample(demdata, n = nrow(demdata), replace = T)
  dem <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = dem_dat_i, 
             family = "binomial")
  
  rep_dat_i <- slice_sample(repdata, n = nrow(repdata), replace = T)
  rep <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = rep_dat_i,
             family = "binomial")
  
  
  synthwhite_all <- data.frame(copartisan = mean(all_dat_i$copartisan, na.rm=T),
                               independent = mean(all_dat_i$independent, na.rm=T),
                               worryScale = seq(0,5,5),
                               black = 0,
                               crime_victimization = mean(all_dat_i$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(all_dat_i$chgCrimeRate, na.rm=T),
                               male = mean(all_dat_i$male, na.rm=T),
                               incomeLt20 = mean(all_dat_i$incomeLt20, na.rm=T),
                               income20t30 = mean(all_dat_i$income20t30, na.rm=T),
                               income30t50 = mean(all_dat_i$income30t50, na.rm=T),
                               income50t75 = mean(all_dat_i$income50t75, na.rm=T),
                               senior = mean(all_dat_i$senior, na.rm=T),
                               under30 = mean(all_dat_i$under30, na.rm=T),
                               someCollege = mean(all_dat_i$someCollege, na.rm=T),
                               baDegree = mean(all_dat_i$baDegree, na.rm=T),
                               postGrad = mean(all_dat_i$postGrad, na.rm=T),
                               rural = mean(all_dat_i$rural, na.rm=T),
                               suburban = mean(all_dat_i$suburban, na.rm=T),
                               yr_factor = "2000")
  
  synthblack_all <- data.frame(copartisan = mean(all_dat_i$copartisan, na.rm=T),
                               independent = mean(all_dat_i$independent, na.rm=T),
                               worryScale = seq(0,5,5),
                               black = 1,
                               crime_victimization = mean(all_dat_i$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(all_dat_i$chgCrimeRate, na.rm=T),
                               male = mean(all_dat_i$male, na.rm=T),
                               incomeLt20 = mean(all_dat_i$incomeLt20, na.rm=T),
                               income20t30 = mean(all_dat_i$income20t30, na.rm=T),
                               income30t50 = mean(all_dat_i$income30t50, na.rm=T),
                               income50t75 = mean(all_dat_i$income50t75, na.rm=T),
                               senior = mean(all_dat_i$senior, na.rm=T),
                               under30 = mean(all_dat_i$under30, na.rm=T),
                               someCollege = mean(all_dat_i$someCollege, na.rm=T),
                               baDegree = mean(all_dat_i$baDegree, na.rm=T),
                               postGrad = mean(all_dat_i$postGrad, na.rm=T),
                               rural = mean(all_dat_i$rural, na.rm=T),
                               suburban = mean(all_dat_i$suburban, na.rm=T),
                               yr_factor = "2000")
  
  synthwhite_dem <- data.frame(copartisan = mean(dem_dat_i$copartisan, na.rm=T),
                               independent = mean(dem_dat_i$independent, na.rm=T),
                               worryScale = seq(0,5,5),
                               black = 0,
                               crime_victimization = mean(dem_dat_i$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(dem_dat_i$chgCrimeRate, na.rm=T),
                               male = mean(dem_dat_i$male, na.rm=T),
                               incomeLt20 = mean(dem_dat_i$incomeLt20, na.rm=T),
                               income20t30 = mean(dem_dat_i$income20t30, na.rm=T),
                               income30t50 = mean(dem_dat_i$income30t50, na.rm=T),
                               income50t75 = mean(dem_dat_i$income50t75, na.rm=T),
                               senior = mean(dem_dat_i$senior, na.rm=T),
                               under30 = mean(dem_dat_i$under30, na.rm=T),
                               someCollege = mean(dem_dat_i$someCollege, na.rm=T),
                               baDegree = mean(dem_dat_i$baDegree, na.rm=T),
                               postGrad = mean(dem_dat_i$postGrad, na.rm=T),
                               rural = mean(dem_dat_i$rural, na.rm=T),
                               suburban = mean(dem_dat_i$suburban, na.rm=T),
                               yr_factor = "2000")
  
  synthblack_dem <- data.frame(copartisan = mean(dem_dat_i$copartisan, na.rm=T),
                               independent = mean(dem_dat_i$independent, na.rm=T),
                               worryScale = seq(0,5,5),
                               black = 1,
                               crime_victimization = mean(dem_dat_i$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(dem_dat_i$chgCrimeRate, na.rm=T),
                               male = mean(dem_dat_i$male, na.rm=T),
                               incomeLt20 = mean(dem_dat_i$incomeLt20, na.rm=T),
                               income20t30 = mean(dem_dat_i$income20t30, na.rm=T),
                               income30t50 = mean(dem_dat_i$income30t50, na.rm=T),
                               income50t75 = mean(dem_dat_i$income50t75, na.rm=T),
                               senior = mean(dem_dat_i$senior, na.rm=T),
                               under30 = mean(dem_dat_i$under30, na.rm=T),
                               someCollege = mean(dem_dat_i$someCollege, na.rm=T),
                               baDegree = mean(dem_dat_i$baDegree, na.rm=T),
                               postGrad = mean(dem_dat_i$postGrad, na.rm=T),
                               rural = mean(dem_dat_i$rural, na.rm=T),
                               suburban = mean(dem_dat_i$suburban, na.rm=T),
                               yr_factor = "2000")
  
  synthwhite_rep <- data.frame(copartisan = mean(rep_dat_i$copartisan, na.rm=T),
                               independent = mean(rep_dat_i$independent, na.rm=T),
                               worryScale = seq(0,5,5),
                               black = 0,
                               crime_victimization = mean(rep_dat_i$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(rep_dat_i$chgCrimeRate, na.rm=T),
                               male = mean(rep_dat_i$male, na.rm=T),
                               incomeLt20 = mean(rep_dat_i$incomeLt20, na.rm=T),
                               income20t30 = mean(rep_dat_i$income20t30, na.rm=T),
                               income30t50 = mean(rep_dat_i$income30t50, na.rm=T),
                               income50t75 = mean(rep_dat_i$income50t75, na.rm=T),
                               senior = mean(rep_dat_i$senior, na.rm=T),
                               under30 = mean(rep_dat_i$under30, na.rm=T),
                               someCollege = mean(rep_dat_i$someCollege, na.rm=T),
                               baDegree = mean(rep_dat_i$baDegree, na.rm=T),
                               postGrad = mean(rep_dat_i$postGrad, na.rm=T),
                               rural = mean(rep_dat_i$rural, na.rm=T),
                               suburban = mean(rep_dat_i$suburban, na.rm=T),
                               yr_factor = "2019")
  
  synthblack_rep <- data.frame(copartisan = mean(rep_dat_i$copartisan, na.rm=T),
                               independent = mean(rep_dat_i$independent, na.rm=T),
                               worryScale = seq(0,5,5),
                               black = 1,
                               crime_victimization = mean(rep_dat_i$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(rep_dat_i$chgCrimeRate, na.rm=T),
                               male = mean(rep_dat_i$male, na.rm=T),
                               incomeLt20 = mean(rep_dat_i$incomeLt20, na.rm=T),
                               income20t30 = mean(rep_dat_i$income20t30, na.rm=T),
                               income30t50 = mean(rep_dat_i$income30t50, na.rm=T),
                               income50t75 = mean(rep_dat_i$income50t75, na.rm=T),
                               senior = mean(rep_dat_i$senior, na.rm=T),
                               under30 = mean(rep_dat_i$under30, na.rm=T),
                               someCollege = mean(rep_dat_i$someCollege, na.rm=T),
                               baDegree = mean(rep_dat_i$baDegree, na.rm=T),
                               postGrad = mean(rep_dat_i$postGrad, na.rm=T),
                               rural = mean(rep_dat_i$rural, na.rm=T),
                               suburban = mean(rep_dat_i$suburban, na.rm=T),
                               yr_factor = "2019")
  
  pp_white_all[i] <- predict(all, synthwhite_all, type = "response", se = F)[2] - predict(all, synthwhite_all, type = "response", se = F)[1]
  pp_black_all[i] <- predict(all, synthblack_all, type = "response", se = F)[2] - predict(all, synthblack_all, type = "response", se = F)[1]
  
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
race_fd <- arrange(transform(preds_stack, president = factor(president, levels = order)))