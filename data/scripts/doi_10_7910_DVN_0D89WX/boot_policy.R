pp_approach_all_w <- vector()
pp_approach_all_b <- vector()
pp_issue_all_w <- vector()
pp_issue_all_b <- vector()
pp_approach_dem_w <- vector()
pp_approach_dem_b <- vector()
pp_issue_dem_w <- vector()
pp_issue_dem_b <- vector()
pp_approach_rep_w <- vector()
pp_approach_rep_b <- vector()
pp_issue_rep_w <- vector()
pp_issue_rep_b <- vector()

demdata <- subset(crimeData, president %in% c("Clinton","Obama"))
demdata$yr_factor <- droplevels(demdata$yr_factor)
repdata <- subset(crimeData, president %in% c("Bush","Trump"))
repdata$yr_factor <- droplevels(repdata$yr_factor)

approach_all_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + crimeApproach + crime_victimization + 
                               chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                               someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                             data = crimeData, 
                             family = "binomial")

issue_all_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + gunLaws + deathPenalty + crime_victimization + 
                            chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                            someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                          data = crimeData, 
                          family = "binomial")



approach_dem_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + crimeApproach + crime_victimization + 
                               chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                               someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                             data = demdata, 
                             family = "binomial")

issue_dem_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + gunLaws + deathPenalty + crime_victimization + 
                            chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                            someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                          data = demdata, 
                          family = "binomial")


approach_rep_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + crimeApproach + crime_victimization + 
                               chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                               someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                             data = repdata, 
                             family = "binomial")

issue_rep_mod_full <- glm(presapp ~ copartisan + independent + worryScale * black + gunLaws + deathPenalty + crime_victimization + 
                            chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                            someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                          data = repdata, 
                          family = "binomial")

synth_all_approach <- data.frame(copartisan = mean(crimeData$copartisan, na.rm = T),
                                 independent = mean(crimeData$independent, na.rm=T),
                                 worryScale = c(0,5,0,5),
                                 black = c(0,0,1,1),
                                 crimeApproach = mean(crimeData$crimeApproach, na.rm = T),
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
                                 yr_factor = "2003")

synth_all_issue <- data.frame(copartisan = mean(crimeData$copartisan, na.rm = T),
                              independent = mean(crimeData$independent, na.rm=T),
                              worryScale = c(0,5,0,5),
                              black = c(0,0,1,1),
                              gunLaws = mean(crimeData$gunLaws, na.rm = T),
                              deathPenalty = mean(crimeData$deathPenalty, na.rm = T),
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
                              yr_factor = "2019")

synth_dem_approach <- data.frame(
  copartisan = mean(demdata$copartisan, na.rm = T),
  independent = mean(demdata$independent, na.rm=T),
  worryScale = c(0,5,0,5),
  black = c(0,0,1,1),
  crimeApproach = mean(demdata$crimeApproach, na.rm = T),
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

synth_dem_issue <- data.frame(
  copartisan = mean(demdata$copartisan, na.rm = T),
  independent = mean(demdata$independent, na.rm=T),
  worryScale = c(0,5,0,5),
  black = c(0,0,1,1),
  gunLaws = mean(demdata$gunLaws, na.rm = T),
  deathPenalty = mean(demdata$deathPenalty, na.rm = T),
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
  yr_factor = "2009")

synth_rep_approach <- data.frame(copartisan = mean(repdata$copartisan, na.rm = T),
                                 independent = mean(repdata$independent, na.rm=T),
                                 worryScale = c(0,5,0,5),
                                 black = c(0,0,1,1),
                                 crimeApproach = mean(repdata$crimeApproach, na.rm = T),
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
                                 yr_factor = "2003")

synth_rep_issue <- data.frame(copartisan = mean(repdata$copartisan, na.rm = T),
                              independent = mean(repdata$independent, na.rm=T),
                              worryScale = c(0,5,0,5),
                              black = c(0,0,1,1),
                              gunLaws = mean(repdata$gunLaws, na.rm = T),
                              deathPenalty = mean(repdata$deathPenalty, na.rm = T),
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


pp_approach_all_wfull <- predict(approach_all_mod_full, synth_all_approach, type = "response", se = F)[2] - predict(approach_all_mod_full, synth_all_approach, type = "response", se = F)[1]
pp_approach_all_bfull <- predict(approach_all_mod_full, synth_all_approach, type = "response", se = F)[4] - predict(approach_all_mod_full, synth_all_approach, type = "response", se = F)[3]

pp_issue_all_wfull <- predict(issue_all_mod_full, synth_all_issue, type = "response", se = F)[2] - predict(issue_all_mod_full, synth_all_issue, type = "response", se = F)[1]
pp_issue_all_bfull <- predict(issue_all_mod_full, synth_all_issue, type = "response", se = F)[4] - predict(issue_all_mod_full, synth_all_issue, type = "response", se = F)[3]

pp_approach_dem_wfull <- predict(approach_dem_mod_full, synth_dem_approach, type = "response", se = F)[2] - predict(approach_dem_mod_full, synth_dem_approach, type = "response", se = F)[1]
pp_approach_dem_bfull <- predict(approach_dem_mod_full, synth_dem_approach, type = "response", se = F)[4] - predict(approach_dem_mod_full, synth_dem_approach, type = "response", se = F)[3]

pp_issue_dem_wfull <- predict(issue_dem_mod_full, synth_dem_issue, type = "response", se = F)[2] - predict(issue_dem_mod_full, synth_dem_issue, type = "response", se = F)[1]
pp_issue_dem_bfull <- predict(issue_dem_mod_full, synth_dem_issue, type = "response", se = F)[4] - predict(issue_dem_mod_full, synth_dem_issue, type = "response", se = F)[3]

pp_approach_rep_wfull <- predict(approach_rep_mod_full, synth_rep_approach, type = "response", se = F)[2] - predict(approach_rep_mod_full, synth_rep_approach, type = "response", se = F)[1]
pp_approach_rep_bfull <- predict(approach_rep_mod_full, synth_rep_approach, type = "response", se = F)[4] - predict(approach_rep_mod_full, synth_rep_approach, type = "response", se = F)[3]

pp_issue_rep_wfull <- predict(issue_rep_mod_full, synth_rep_issue, type = "response", se = F)[2] - predict(issue_rep_mod_full, synth_rep_issue, type = "response", se = F)[1]
pp_issue_rep_bfull <- predict(issue_rep_mod_full, synth_rep_issue, type = "response", se = F)[4] - predict(issue_rep_mod_full, synth_rep_issue, type = "response", se = F)[3]

set.seed(220)
for(i in 1:1000){
  print(i)
  
  all_dat_i <- slice_sample(crimeData, n = nrow(crimeData), replace = T)
  
  approach_all <- glm(presapp ~ copartisan + independent + worryScale * black + crimeApproach + crime_victimization + 
                        chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                        someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                      data = all_dat_i, 
                      family = "binomial")
  
  issue_all <- glm(presapp ~ copartisan + independent + worryScale * black + gunLaws + deathPenalty + crime_victimization + 
                        chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                        someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                   data = all_dat_i, 
                   family = "binomial")
  
  dem_dat_i <- slice_sample(demdata, n = nrow(demdata), replace = T)
  
  approach_dem <- glm(presapp ~ copartisan + independent + worryScale * black + crimeApproach + crime_victimization + 
               chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
               someCollege + baDegree + postGrad + rural + suburban + yr_factor,
             data = dem_dat_i, 
             family = "binomial")
  
  issue_dem <- glm(presapp ~ copartisan + independent + worryScale * black + gunLaws + deathPenalty + crime_victimization + 
                        chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                        someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                      data = dem_dat_i, 
                      family = "binomial")
  
  rep_dat_i <- slice_sample(repdata, n = nrow(repdata), replace = T)
  
  approach_rep <- glm(presapp ~ copartisan + independent + worryScale * black + crimeApproach + crime_victimization + 
               chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
               someCollege + baDegree + postGrad + rural + suburban + yr_factor,
             data = rep_dat_i,
             family = "binomial")
  
  issue_rep <- glm(presapp ~ copartisan + independent + worryScale * black + gunLaws + deathPenalty  + crime_victimization + 
                        chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + income50t75 + senior + under30 + 
                        someCollege + baDegree + postGrad + rural + suburban + yr_factor,
                      data = rep_dat_i,
                      family = "binomial")
  
  synth_all_approach <- data.frame(copartisan = mean(all_dat_i$copartisan, na.rm = T),
                          independent = mean(all_dat_i$independent, na.rm=T),
                          worryScale = c(0,5,0,5),
                          black = c(0,0,1,1),
                          crimeApproach = mean(all_dat_i$crimeApproach, na.rm = T),
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
                          yr_factor = "2003")
  
  synth_all_issue <- data.frame(copartisan = mean(all_dat_i$copartisan, na.rm = T),
    independent = mean(all_dat_i$independent, na.rm=T),
                               worryScale = c(0,5,0,5),
                               black = c(0,0,1,1),
                               gunLaws = mean(all_dat_i$gunLaws, na.rm = T),
                               deathPenalty = mean(all_dat_i$deathPenalty, na.rm = T),
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
                               yr_factor = "2019")

  synth_dem_approach <- data.frame(
    copartisan = mean(dem_dat_i$copartisan, na.rm = T),
    independent = mean(dem_dat_i$independent, na.rm=T),
    worryScale = c(0,5,0,5),
    black = c(0,0,1,1),
    crimeApproach = mean(dem_dat_i$crimeApproach, na.rm = T),
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
  
  synth_dem_issue <- data.frame(
    copartisan = mean(dem_dat_i$copartisan, na.rm = T),
    independent = mean(dem_dat_i$independent, na.rm=T),
    worryScale = c(0,5,0,5),
    black = c(0,0,1,1),
    gunLaws = mean(dem_dat_i$gunLaws, na.rm = T),
    deathPenalty = mean(dem_dat_i$deathPenalty, na.rm = T),
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
    yr_factor = "2009")
  
  synth_rep_approach <- data.frame(copartisan = mean(rep_dat_i$copartisan, na.rm = T),
                                independent = mean(rep_dat_i$independent, na.rm=T),
                                worryScale = c(0,5,0,5),
                                black = c(0,0,1,1),
                                crimeApproach = mean(rep_dat_i$crimeApproach, na.rm = T),
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
                                yr_factor = "2003")
  
  synth_rep_issue <- data.frame(copartisan = mean(rep_dat_i$copartisan, na.rm = T),
    independent = mean(rep_dat_i$independent, na.rm=T),
    worryScale = c(0,5,0,5),
    black = c(0,0,1,1),
    gunLaws = mean(rep_dat_i$gunLaws, na.rm = T),
    deathPenalty = mean(rep_dat_i$deathPenalty, na.rm = T),
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
  
  pp_approach_all_w[i] <- predict(approach_all, synth_all_approach, type = "response", se = F)[2] - predict(approach_all, synth_all_approach, type = "response", se = F)[1]
  pp_approach_all_b[i] <- predict(approach_all, synth_all_approach, type = "response", se = F)[4] - predict(approach_all, synth_all_approach, type = "response", se = F)[3]
  
  pp_issue_all_w[i] <- predict(issue_all, synth_all_issue, type = "response", se = F)[2] - predict(issue_all, synth_all_issue, type = "response", se = F)[1]
  pp_issue_all_b[i] <- predict(issue_all, synth_all_issue, type = "response", se = F)[4] - predict(issue_all, synth_all_issue, type = "response", se = F)[3]
  
  pp_approach_dem_w[i] <- predict(approach_dem, synth_dem_approach, type = "response", se = F)[2] - predict(approach_dem, synth_dem_approach, type = "response", se = F)[1]
  pp_approach_dem_b[i] <- predict(approach_dem, synth_dem_approach, type = "response", se = F)[4] - predict(approach_dem, synth_dem_approach, type = "response", se = F)[3]
  
  pp_issue_dem_w[i] <- predict(issue_dem, synth_dem_issue, type = "response", se = F)[2] - predict(issue_dem, synth_dem_issue, type = "response", se = F)[1]
  pp_issue_dem_b[i] <- predict(issue_dem, synth_dem_issue, type = "response", se = F)[4] - predict(issue_dem, synth_dem_issue, type = "response", se = F)[3]
  
  pp_approach_rep_w[i] <- predict(approach_rep, synth_rep_approach, type = "response", se = F)[2] - predict(approach_rep, synth_rep_approach, type = "response", se = F)[1]
  pp_approach_rep_b[i] <- predict(approach_rep, synth_rep_approach, type = "response", se = F)[4] - predict(approach_rep, synth_rep_approach, type = "response", se = F)[3]
  
  pp_issue_rep_w[i] <- predict(issue_rep, synth_rep_issue, type = "response", se = F)[2] - predict(issue_rep, synth_rep_issue, type = "response", se = F)[1]
  pp_issue_rep_b[i] <- predict(issue_rep, synth_rep_issue, type = "response", se = F)[4] - predict(issue_rep, synth_rep_issue, type = "response", se = F)[3]
}

pp_approach_all_wq <- quantile(pp_approach_all_w, c(.025, .5, .975))
pp_approach_all_wq[2] <- pp_approach_all_wfull
pp_approach_all_bq <- quantile(pp_approach_all_b, c(.025, .5, .975))
pp_approach_all_bq[2] <- pp_approach_all_bfull

pp_issue_all_wq <- quantile(pp_issue_all_w, c(.025, .5, .975))
pp_issue_all_wq[2] <- pp_issue_all_wfull
pp_issue_all_bq <- quantile(pp_issue_all_b, c(.025, .5, .975))
pp_issue_all_bq[2] <- pp_issue_all_bfull

preds_all <- data.frame(pp = c(pp_approach_all_wq[2], pp_approach_all_bq[2], pp_issue_all_wq[2], pp_issue_all_bq[2]),
                        high = c(pp_approach_all_wq[3], pp_approach_all_bq[3], pp_issue_all_wq[3], pp_issue_all_bq[3]),
                        low = c(pp_approach_all_wq[1], pp_approach_all_bq[1], pp_issue_all_wq[1], pp_issue_all_bq[1]),
                        president = "All",
                        race = rep(c("White","Black"), times = 2),
                        type = rep(c('Approach', 'Issues'), each = 2))

pp_approach_dem_wq <- quantile(pp_approach_dem_w, c(.025, .5, .975))
pp_approach_dem_wq[2] <- pp_approach_dem_wfull
pp_approach_dem_bq <- quantile(pp_approach_dem_b, c(.025, .5, .975))
pp_approach_dem_bq[2] <- pp_approach_dem_bfull

pp_issue_dem_wq <- quantile(pp_issue_dem_w, c(.025, .5, .975))
pp_issue_dem_wq[2] <- pp_issue_dem_wfull 
pp_issue_dem_bq <- quantile(pp_issue_dem_b, c(.025, .5, .975))
pp_issue_dem_bq[2] <- pp_issue_dem_bfull

preds_dem <- data.frame(pp = c(pp_approach_dem_wq[2], pp_approach_dem_bq[2], pp_issue_dem_wq[2], pp_issue_dem_bq[2]),
                        high = c(pp_approach_dem_wq[3], pp_approach_dem_bq[3], pp_issue_dem_wq[3], pp_issue_dem_bq[3]),
                        low = c(pp_approach_dem_wq[1], pp_approach_dem_bq[1], pp_issue_dem_wq[1], pp_issue_dem_bq[1]),
                        president = "Clinton/Obama",
                        race = rep(c("White","Black"), times = 2),
                        type = rep(c('Approach', 'Issues'), each = 2))

pp_approach_rep_wq <- quantile(pp_approach_rep_w, c(.025, .5, .975))
pp_approach_rep_wq[2] <- pp_approach_rep_wfull
pp_approach_rep_bq <- quantile(pp_approach_rep_b, c(.025, .5, .975))
pp_approach_rep_bq[2] <- pp_approach_rep_bfull

pp_issue_rep_wq <- quantile(pp_issue_rep_w, c(.025, .5, .975))
pp_issue_rep_wq[2] <- pp_issue_rep_wfull
pp_issue_rep_bq <- quantile(pp_issue_rep_b, c(.025, .5, .975))
pp_issue_rep_bq[2] <- pp_issue_rep_bfull

preds_rep <- data.frame(pp = c(pp_approach_rep_wq[2], pp_approach_rep_bq[2], pp_issue_rep_wq[2], pp_issue_rep_bq[2]),
                        high = c(pp_approach_rep_wq[3], pp_approach_rep_bq[3], pp_issue_rep_wq[3], pp_issue_rep_bq[3]),
                        low = c(pp_approach_rep_wq[1], pp_approach_rep_bq[1], pp_issue_rep_wq[1], pp_issue_rep_bq[1]),
                        president = "Bush/Trump",
                        race = rep(c("White","Black"), times = 2),
                        type = rep(c('Approach', 'Issues'), each = 2))

preds_stack <- bind_rows(preds_all, preds_dem, preds_rep)

order <- c("All","Clinton/Obama","Bush/Trump")
policy_fd <- arrange(transform(preds_stack, president = factor(president, levels = order)))
