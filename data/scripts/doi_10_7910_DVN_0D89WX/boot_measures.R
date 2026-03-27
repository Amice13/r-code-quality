pp_worry <- vector() # empty vector for predicted probabilities, worry
pp_retro <- vector() # empty vector for predicted probabilities, retrospective crime

# worry model
worry_mod_full <- glm(presapp ~ copartisan + independent + worryScale + crime_victimization + chgCrimeRate + white + male +  
                        incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                        postGrad + rural + suburban + yr_factor, data = crimeData, family = "binomial")

# retrospective model
retro_mod_full <- glm(presapp ~ copartisan + independent + crime_us_yr_ago + crime_victimization + chgCrimeRate + white + male + 
                        incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                        postGrad + rural + suburban + yr_factor, data = crimeData, family = "binomial")



## true predictions
synth_worry <- data.frame(copartisan = mean(crimeData$copartisan, na.rm=T),
                          independent = mean(crimeData$independent, na.rm=T),
                          worryScale = c(0,5),
                          white = mean(crimeData$white, na.rm = T),
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

pp_worry_mod_full <- predict(worry_mod_full, synth_worry, type = 'response')[2] - predict(worry_mod_full, synth_worry, type = 'response')[1] # first difference, low to high worry

# data means for full retrospective dataset
synth_retro <- data.frame(copartisan = mean(crimeData$copartisan, na.rm=T),
                          independent = mean(crimeData$independent, na.rm=T),
                          crime_us_yr_ago = c(-1, 1),
                          white = mean(crimeData$white, na.rm = T),
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

pp_retro_mod_full <- predict(retro_mod_full, synth_retro, type = 'response')[2] - predict(retro_mod_full, synth_retro, type = 'response')[1] # first difference, low to high retrospective





## boostrap confidence intervals
set.seed(220)
for(i in 1:1000){ # 1000 bootstraps
  print(i) # print progress counter
  # worry model, sample full data with replacement
  dat_i <- slice_sample(crimeData, n = nrow(crimeData), replace = T)
  worry_mod <- glm(presapp ~ copartisan + independent + worryScale + crime_victimization + chgCrimeRate + white + male + 
                 incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                 postGrad + rural + suburban + yr_factor,
               data = dat_i, 
               family = "binomial")
  
  # retrospective model, sample full data with replacement
  retro_mod <- glm(presapp ~ copartisan + independent + crime_us_yr_ago + crime_victimization + chgCrimeRate + white + male + 
                     incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + someCollege + baDegree + 
                     postGrad + rural + suburban + yr_factor,
                   data = dat_i,
                   family = "binomial")
  
  # data means for full worry dataset
  synth_worry <- data.frame(copartisan = mean(dat_i$copartisan, na.rm=T),
                               independent = mean(dat_i$independent, na.rm=T),
                               worryScale = c(0,5),
                               white = mean(dat_i$white, na.rm = T),
                               crime_victimization = mean(dat_i$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(dat_i$chgCrimeRate, na.rm=T),
                               male = mean(dat_i$male, na.rm=T),
                               incomeLt20 = mean(dat_i$incomeLt20, na.rm=T),
                               income20t30 = mean(dat_i$income20t30, na.rm=T),
                               income30t50 = mean(dat_i$income30t50, na.rm=T),
                               income50t75 = mean(dat_i$income50t75, na.rm=T),
                               senior = mean(dat_i$senior, na.rm=T),
                               under30 = mean(dat_i$under30, na.rm=T),
                               someCollege = mean(dat_i$someCollege, na.rm=T),
                               baDegree = mean(dat_i$baDegree, na.rm=T),
                               postGrad = mean(dat_i$postGrad, na.rm=T),
                               rural = mean(dat_i$rural, na.rm=T),
                               suburban = mean(dat_i$suburban, na.rm=T),
                               yr_factor = "2000")
  
  # data means for full retrospective dataset
  synth_retro <- data.frame(copartisan = mean(dat_i$copartisan, na.rm=T),
                               independent = mean(dat_i$independent, na.rm=T),
                               crime_us_yr_ago = c(-1, 1),
                               white = mean(dat_i$white, na.rm = T),
                               crime_victimization = mean(dat_i$crime_victimization, na.rm=T),
                               chgCrimeRate = mean(dat_i$chgCrimeRate, na.rm=T),
                               male = mean(dat_i$male, na.rm=T),
                               incomeLt20 = mean(dat_i$incomeLt20, na.rm=T),
                               income20t30 = mean(dat_i$income20t30, na.rm=T),
                               income30t50 = mean(dat_i$income30t50, na.rm=T),
                               income50t75 = mean(dat_i$income50t75, na.rm=T),
                               senior = mean(dat_i$senior, na.rm=T),
                               under30 = mean(dat_i$under30, na.rm=T),
                               someCollege = mean(dat_i$someCollege, na.rm=T),
                               baDegree = mean(dat_i$baDegree, na.rm=T),
                               postGrad = mean(dat_i$postGrad, na.rm=T),
                               rural = mean(dat_i$rural, na.rm=T),
                               suburban = mean(dat_i$suburban, na.rm=T),
                               yr_factor = "2000")
  
  pp_worry[i] <- predict(worry_mod, synth_worry, type = "response", se = F)[2] - predict(worry_mod, synth_worry, type = "response", se = F)[1] # first difference, worry
  pp_retro[i] <- predict(retro_mod, synth_retro, type = "response", se = F)[2] - predict(retro_mod, synth_retro, type = "response", se = F)[1] # first difference, retrospective
  
}

pp_worry_q <- quantile(pp_worry, c(.025, .5, .975)) # 95% confidence intervals and median value of worry model
pp_worry_q[2] <- pp_worry_mod_full # replace median with true predicted value from the full worry model
pp_retro_q <- quantile(pp_retro, c(.025, .5, .975)) # 95% confidence intervals and median value of retrospective model
pp_retro_q[2] <- pp_retro_mod_full # replace median with true predicted value from the full retrospective model

# combine results into organized df
fd_measures <- data.frame(pp = c(pp_worry_q[2], pp_retro_q[2]), 
                        high = c(pp_worry_q[3], pp_retro_q[3]),
                        low = c(pp_worry_q[1], pp_retro_q[1]),
                        measure = c('Anxiety', 'Retrospective'))
