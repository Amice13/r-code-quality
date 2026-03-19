pp_white_all_r <- vector()
pp_white_all_d <- vector()

pp_black_all_r <- vector()
pp_black_all_d <- vector()

pp_white_dem_r <- vector()
pp_white_dem_d <- vector()

pp_black_dem_r <- vector()
pp_black_dem_d <- vector()

pp_white_rep_r <- vector()
pp_white_rep_d <- vector()

pp_black_rep_r <- vector()
pp_black_rep_d <- vector()



all_triple_full <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
                       + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                         income50t75 + senior + under30 + someCollege + baDegree + 
                         postGrad + rural + suburban + yr_factor,
                       data = crimeData, 
                       family = "binomial")

demdata_full<- crimeData %>% filter(president %in% c("Clinton", "Obama"))
dem_triple_full <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
                   + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                     income50t75 + senior + under30 + someCollege + baDegree + 
                     postGrad + rural + suburban + yr_factor,
                   data = demdata_full, 
                   family = "binomial")

repdata_full <- crimeData %>% filter(president %in% c("Bush", "Trump"))
rep_triple_full <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
                   + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
                     income50t75 + senior + under30 + someCollege + baDegree + 
                     postGrad + rural + suburban + yr_factor,
                   data = repdata_full, 
                   family = "binomial")

synthwhite_all <- data.frame(
  independent = mean(crimeData$independent, na.rm=T),
  worryScale = c(0,5,0,5),
  black = 0,
  democrat = c(0,0,1,1),
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

synthblack_all <- data.frame(
  independent = mean(crimeData$independent, na.rm=T),
  # independent = 0,
  worryScale = c(0,5,0,5),
  black = 1,
  democrat = c(0,0,1,1),
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

synthwhite_dem <- data.frame(
  independent = mean(demdata_full$independent, na.rm=T),
  # independent = 0,
  worryScale = c(0,5,0,5),
  black = 0,
  democrat = c(0,0,1,1),
  crime_victimization = mean(demdata_full$crime_victimization, na.rm=T),
  chgCrimeRate = mean(demdata_full$chgCrimeRate, na.rm=T),
  male = mean(demdata_full$male, na.rm=T),
  incomeLt20 = mean(demdata_full$incomeLt20, na.rm=T),
  income20t30 = mean(demdata_full$income20t30, na.rm=T),
  income30t50 = mean(demdata_full$income30t50, na.rm=T),
  income50t75 = mean(demdata_full$income50t75, na.rm=T),
  senior = mean(demdata_full$senior, na.rm=T),
  under30 = mean(demdata_full$under30, na.rm=T),
  someCollege = mean(demdata_full$someCollege, na.rm=T),
  baDegree = mean(demdata_full$baDegree, na.rm=T),
  postGrad = mean(demdata_full$postGrad, na.rm=T),
  rural = mean(demdata_full$rural, na.rm=T),
  suburban = mean(demdata_full$suburban, na.rm=T),
  yr_factor = "2000")

synthblack_dem <- data.frame(
  independent = mean(demdata_full$independent, na.rm=T),
  # independent = 0,
  worryScale = c(0,5,0,5),
  black = 1,
  democrat = c(0,0,1,1),
  crime_victimization = mean(demdata_full$crime_victimization, na.rm=T),
  chgCrimeRate = mean(demdata_full$chgCrimeRate, na.rm=T),
  male = mean(demdata_full$male, na.rm=T),
  incomeLt20 = mean(demdata_full$incomeLt20, na.rm=T),
  income20t30 = mean(demdata_full$income20t30, na.rm=T),
  income30t50 = mean(demdata_full$income30t50, na.rm=T),
  income50t75 = mean(demdata_full$income50t75, na.rm=T),
  senior = mean(demdata_full$senior, na.rm=T),
  under30 = mean(demdata_full$under30, na.rm=T),
  someCollege = mean(demdata_full$someCollege, na.rm=T),
  baDegree = mean(demdata_full$baDegree, na.rm=T),
  postGrad = mean(demdata_full$postGrad, na.rm=T),
  rural = mean(demdata_full$rural, na.rm=T),
  suburban = mean(demdata_full$suburban, na.rm=T),
  yr_factor = "2000")

synthwhite_rep <- data.frame(
  independent = mean(repdata_full$independent, na.rm=T),
  # independent = 0,
  worryScale = c(0,5,0,5),
  black = 0,
  democrat = c(0,0,1,1),
  crime_victimization = mean(repdata_full$crime_victimization, na.rm=T),
  chgCrimeRate = mean(repdata_full$chgCrimeRate, na.rm=T),
  male = mean(repdata_full$male, na.rm=T),
  incomeLt20 = mean(repdata_full$incomeLt20, na.rm=T),
  income20t30 = mean(repdata_full$income20t30, na.rm=T),
  income30t50 = mean(repdata_full$income30t50, na.rm=T),
  income50t75 = mean(repdata_full$income50t75, na.rm=T),
  senior = mean(repdata_full$senior, na.rm=T),
  under30 = mean(repdata_full$under30, na.rm=T),
  someCollege = mean(repdata_full$someCollege, na.rm=T),
  baDegree = mean(repdata_full$baDegree, na.rm=T),
  postGrad = mean(repdata_full$postGrad, na.rm=T),
  rural = mean(repdata_full$rural, na.rm=T),
  suburban = mean(repdata_full$suburban, na.rm=T),
  yr_factor = "2019")

synthblack_rep <- data.frame(
  independent = mean(repdata_full$independent, na.rm=T),
  # independent = 0,
  worryScale = c(0,5,0,5),
  black = 1,
  democrat = c(0,0,1,1),
  crime_victimization = mean(repdata_full$crime_victimization, na.rm=T),
  chgCrimeRate = mean(repdata_full$chgCrimeRate, na.rm=T),
  male = mean(repdata_full$male, na.rm=T),
  incomeLt20 = mean(repdata_full$incomeLt20, na.rm=T),
  income20t30 = mean(repdata_full$income20t30, na.rm=T),
  income30t50 = mean(repdata_full$income30t50, na.rm=T),
  income50t75 = mean(repdata_full$income50t75, na.rm=T),
  senior = mean(repdata_full$senior, na.rm=T),
  under30 = mean(repdata_full$under30, na.rm=T),
  someCollege = mean(repdata_full$someCollege, na.rm=T),
  baDegree = mean(repdata_full$baDegree, na.rm=T),
  postGrad = mean(repdata_full$postGrad, na.rm=T),
  rural = mean(repdata_full$rural, na.rm=T),
  suburban = mean(repdata_full$suburban, na.rm=T),
  yr_factor = "2019")

pp_white_all_rfull <- predict(all_triple_full, synthwhite_all, type = "response", se = F)[2] - predict(all_triple_full, synthwhite_all, type = "response", se = F)[1]
pp_white_all_dfull <- predict(all_triple_full, synthwhite_all, type = "response", se = F)[4] - predict(all_triple_full, synthwhite_all, type = "response", se = F)[3]

pp_black_all_rfull <- predict(all_triple_full, synthblack_all, type = "response", se = F)[2] - predict(all_triple_full, synthblack_all, type = "response", se = F)[1]
pp_black_all_dfull <- predict(all_triple_full, synthblack_all, type = "response", se = F)[4] - predict(all_triple_full, synthblack_all, type = "response", se = F)[3]

pp_white_dem_rfull <- predict(dem_triple_full, synthwhite_dem, type = "response", se = F)[2] - predict(dem_triple_full, synthwhite_dem, type = "response", se = F)[1]
pp_white_dem_dfull <- predict(dem_triple_full, synthwhite_dem, type = "response", se = F)[4] - predict(dem_triple_full, synthwhite_dem, type = "response", se = F)[3]

pp_black_dem_rfull <- predict(dem_triple_full, synthblack_dem, type = "response", se = F)[2] - predict(dem_triple_full, synthblack_dem, type = "response", se = F)[1]
pp_black_dem_dfull <- predict(dem_triple_full, synthblack_dem, type = "response", se = F)[4] - predict(dem_triple_full, synthblack_dem, type = "response", se = F)[3]

pp_white_rep_rfull <- predict(rep_triple_full, synthwhite_rep, type = "response", se = F)[2] - predict(rep_triple_full, synthwhite_rep, type = "response", se = F)[1]
pp_white_rep_dfull <- predict(rep_triple_full, synthwhite_rep, type = "response", se = F)[4] - predict(rep_triple_full, synthwhite_rep, type = "response", se = F)[3]

pp_black_rep_rfull <- predict(rep_triple_full, synthblack_rep, type = "response", se = F)[2] - predict(rep_triple_full, synthblack_rep, type = "response", se = F)[1]
pp_black_rep_dfull <- predict(rep_triple_full, synthblack_rep, type = "response", se = F)[4] - predict(rep_triple_full, synthblack_rep, type = "response", se = F)[3]


set.seed(220)
for(i in 1:1000){
  print(i)
  
  alldata_samp <- slice_sample(crimeData, n = nrow(crimeData), replace = T)
  all <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = alldata_samp, 
             family = "binomial")
  
  demdata <- subset(crimeData, president %in% c("Clinton","Obama"))
  demdata$yr_factor <- droplevels(demdata$yr_factor)
  
  demdata_samp <- slice_sample(demdata, n = nrow(demdata), replace = T)
  dem <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = demdata_samp, 
             family = "binomial")
  
  repdata <- subset(crimeData, president %in% c("Bush","Trump"))
  repdata$yr_factor <- droplevels(repdata$yr_factor)
  
  repdata_samp <- slice_sample(repdata, n = nrow(repdata), replace = T)
  rep <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = repdata_samp,
             family = "binomial")
  
  
  synthwhite_all <- data.frame(
    independent = mean(alldata_samp$independent, na.rm=T),
    worryScale = c(0,5,0,5),
    black = 0,
    democrat = c(0,0,1,1),
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
  
  synthblack_all <- data.frame(
    independent = mean(alldata_samp$independent, na.rm=T),
    # independent = 0,
    worryScale = c(0,5,0,5),
    black = 1,
    democrat = c(0,0,1,1),
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
  
  synthwhite_dem <- data.frame(
    independent = mean(demdata_samp$independent, na.rm=T),
    # independent = 0,
    worryScale = c(0,5,0,5),
    black = 0,
    democrat = c(0,0,1,1),
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
  
  synthblack_dem <- data.frame(
    independent = mean(demdata_samp$independent, na.rm=T),
    # independent = 0,
    worryScale = c(0,5,0,5),
    black = 1,
    democrat = c(0,0,1,1),
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
  
  synthwhite_rep <- data.frame(
    independent = mean(repdata_samp$independent, na.rm=T),
    # independent = 0,
    worryScale = c(0,5,0,5),
    black = 0,
    democrat = c(0,0,1,1),
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
  
  synthblack_rep <- data.frame(
    independent = mean(repdata_samp$independent, na.rm=T),
    # independent = 0,
    worryScale = c(0,5,0,5),
    black = 1,
    democrat = c(0,0,1,1),
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
  
  pp_white_all_r[i] <- predict(all, synthwhite_all, type = "response", se = F)[2] - predict(all, synthwhite_all, type = "response", se = F)[1]
  pp_white_all_d[i] <- predict(all, synthwhite_all, type = "response", se = F)[4] - predict(all, synthwhite_all, type = "response", se = F)[3]
  
  pp_black_all_r[i] <- predict(all, synthblack_all, type = "response", se = F)[2] - predict(all, synthblack_all, type = "response", se = F)[1]
  pp_black_all_d[i] <- predict(all, synthblack_all, type = "response", se = F)[4] - predict(all, synthblack_all, type = "response", se = F)[3]
  
  pp_white_dem_r[i] <- predict(dem, synthwhite_dem, type = "response", se = F)[2] - predict(dem, synthwhite_dem, type = "response", se = F)[1]
  pp_white_dem_d[i] <- predict(dem, synthwhite_dem, type = "response", se = F)[4] - predict(dem, synthwhite_dem, type = "response", se = F)[3]
  
  pp_black_dem_r[i] <- predict(dem, synthblack_dem, type = "response", se = F)[2] - predict(dem, synthblack_dem, type = "response", se = F)[1]
  pp_black_dem_d[i] <- predict(dem, synthblack_dem, type = "response", se = F)[4] - predict(dem, synthblack_dem, type = "response", se = F)[3]
  
  pp_white_rep_r[i] <- predict(rep, synthwhite_rep, type = "response", se = F)[2] - predict(rep, synthwhite_rep, type = "response", se = F)[1]
  pp_white_rep_d[i] <- predict(rep, synthwhite_rep, type = "response", se = F)[4] - predict(rep, synthwhite_rep, type = "response", se = F)[3]
  
  pp_black_rep_r[i] <- predict(rep, synthblack_rep, type = "response", se = F)[2] - predict(rep, synthblack_rep, type = "response", se = F)[1]
  pp_black_rep_d[i] <- predict(rep, synthblack_rep, type = "response", se = F)[4] - predict(rep, synthblack_rep, type = "response", se = F)[3]
}



pp_white_all_qr <- quantile(pp_white_all_r, c(.025, .5, .975))
pp_white_all_qr[2] <- pp_white_all_rfull
pp_white_all_qd <- quantile(pp_white_all_d, c(.025, .5, .975))
pp_white_all_qd[2] <- pp_white_all_dfull

pp_black_all_qr <- quantile(pp_black_all_r, c(.025, .5, .975))
pp_black_all_qr[2] <- pp_black_all_rfull
pp_black_all_qd <- quantile(pp_black_all_d, c(.025, .5, .975))
pp_black_all_qd[2] <- pp_black_all_dfull

preds_all <- data.frame(pp = c(pp_white_all_qr[2], pp_white_all_qd[2], pp_black_all_qr[2], pp_black_all_qd[2]),
                        high = c(pp_white_all_qr[3], pp_white_all_qd[3], pp_black_all_qr[3], pp_black_all_qd[3]),
                        low = c(pp_white_all_qr[1], pp_white_all_qd[1], pp_black_all_qr[1], pp_black_all_qd[1]),
                        president = "All",
                        race = rep(c("White","Black"), each = 2),
                        party = rep(c("Rep","Dem"), times = 2))

pp_white_dem_qr <- quantile(pp_white_dem_r, c(.025, .5, .975))
pp_white_dem_qr[2] <- pp_white_dem_rfull
pp_white_dem_qd <- quantile(pp_white_dem_d, c(.025, .5, .975))
pp_white_dem_qd[2] <- pp_white_dem_dfull

pp_black_dem_qr <- quantile(pp_black_dem_r, c(.025, .5, .975))
pp_black_dem_qr[2] <- pp_black_dem_rfull
pp_black_dem_qd <- quantile(pp_black_dem_d, c(.025, .5, .975))
pp_black_dem_qd[2] <- pp_black_dem_dfull

preds_dem <- data.frame(pp = c(pp_white_dem_qr[2], pp_white_dem_qd[2], pp_black_dem_qr[2], pp_black_dem_qd[2]),
                        high = c(pp_white_dem_qr[3], pp_white_dem_qd[3], pp_black_dem_qr[3], pp_black_dem_qd[3]),
                        low = c(pp_white_dem_qr[1], pp_white_dem_qd[1], pp_black_dem_qr[1], pp_black_dem_qd[1]),
                        president = "Clinton/Obama",
                        race = rep(c("White","Black"), each = 2),
                        party = rep(c("Rep","Dem"), times = 2))

pp_white_rep_qr <- quantile(pp_white_rep_r, c(.025, .5, .975))
pp_white_rep_qr[2] <- pp_white_rep_rfull
pp_white_rep_qd <- quantile(pp_white_rep_d, c(.025, .5, .975))
pp_white_rep_qd[2] <- pp_white_rep_dfull

pp_black_rep_qr <- quantile(pp_black_rep_r, c(.025, .5, .975))
pp_black_rep_qr[2] <- pp_black_rep_rfull
pp_black_rep_qd <- quantile(pp_black_rep_d, c(.025, .5, .975))
pp_black_rep_qd[2] <- pp_black_rep_dfull

preds_rep <- data.frame(pp = c(pp_white_rep_qr[2], pp_white_rep_qd[2], pp_black_rep_qr[2], pp_black_rep_qd[2]),
                        high = c(pp_white_rep_qr[3], pp_white_rep_qd[3], pp_black_rep_qr[3], pp_black_rep_qd[3]),
                        low = c(pp_white_rep_qr[1], pp_white_rep_qd[1], pp_black_rep_qr[1], pp_black_rep_qd[1]),
                        president = "Bush/Trump",
                        race = rep(c("White","Black"), each = 2),
                        party = rep(c("Rep","Dem"), times = 2))

preds_stack <- bind_rows(preds_all, preds_dem, preds_rep)

order <- c("All","Clinton/Obama","Bush/Trump")
triple_int_diffs <- arrange(transform(preds_stack, president = factor(president, levels = order)))
