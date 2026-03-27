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

set.seed(220)
for(i in 1:1000){
  print(i)
  all <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = slice_sample(crimeData, n = nrow(crimeData), replace = T), family = "binomial")
  
  demdata <- subset(crimeData, president %in% c("Clinton","Obama"))
  demdata$yr_factor <- droplevels(demdata$yr_factor)
  dem <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = slice_sample(demdata, n = nrow(demdata), replace = T), 
             family = "binomial")
  
  repdata <- subset(crimeData, president %in% c("Bush","Trump"))
  repdata$yr_factor <- droplevels(repdata$yr_factor)
  rep <- glm(presapp ~ independent + worryScale * black * democrat + crime_victimization
             + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
               income50t75 + senior + under30 + someCollege + baDegree + 
               postGrad + rural + suburban + yr_factor,
             data = slice_sample(repdata, n = nrow(repdata), replace = T),
             family = "binomial")
  
  
  synthwhite_all <- data.frame(independent = mean(crimeData$independent, na.rm=T),
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
                               independent = mean(demdata$independent, na.rm=T),
                               worryScale = c(0,5,0,5),
                               black = 0,
                               democrat = c(0,0,1,1),
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
  
  synthblack_dem <- data.frame(
                               independent = mean(demdata$independent, na.rm=T),
                               worryScale = c(0,5,0,5),
                               black = 1,
                               democrat = c(0,0,1,1),
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
  
  synthwhite_rep <- data.frame(
                               independent = mean(repdata$independent, na.rm=T),
                               worryScale = c(0,5,0,5),
                               black = 0,
                               democrat = c(0,0,1,1),
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
  
  synthblack_rep <- data.frame(
                               independent = mean(repdata$independent, na.rm=T),
                               worryScale = c(0,5,0,5),
                               black = 1,
                               democrat = c(0,0,1,1),
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
pp_white_all_qd <- quantile(pp_white_all_d, c(.025, .5, .975))

pp_black_all_qr <- quantile(pp_black_all_r, c(.025, .5, .975))
pp_black_all_qd <- quantile(pp_black_all_d, c(.025, .5, .975))

preds_all <- data.frame(pp = c(pp_white_all_qr[2], pp_white_all_qd[2], pp_black_all_qr[2], pp_black_all_qd[2]),
                        high = c(pp_white_all_qr[3], pp_white_all_qd[3], pp_black_all_qr[3], pp_black_all_qd[3]),
                        low = c(pp_white_all_qr[1], pp_white_all_qd[1], pp_black_all_qr[1], pp_black_all_qd[1]),
                        president = "All",
                        race = rep(c("White","Black"), each = 2),
                        party = rep(c("Rep","Dem"), times = 2))

pp_white_dem_qr <- quantile(pp_white_dem_r, c(.025, .5, .975))
pp_white_dem_qd <- quantile(pp_white_dem_d, c(.025, .5, .975))

pp_black_dem_qr <- quantile(pp_black_dem_r, c(.025, .5, .975))
pp_black_dem_qd <- quantile(pp_black_dem_d, c(.025, .5, .975))

preds_dem <- data.frame(pp = c(pp_white_dem_qr[2], pp_white_dem_qd[2], pp_black_dem_qr[2], pp_black_dem_qd[2]),
                        high = c(pp_white_dem_qr[3], pp_white_dem_qd[3], pp_black_dem_qr[3], pp_black_dem_qd[3]),
                        low = c(pp_white_dem_qr[1], pp_white_dem_qd[1], pp_black_dem_qr[1], pp_black_dem_qd[1]),
                        president = "Clinton/Obama",
                        race = rep(c("White","Black"), each = 2),
                        party = rep(c("Rep","Dem"), times = 2))

pp_white_rep_qr <- quantile(pp_white_rep_r, c(.025, .5, .975))
pp_white_rep_qd <- quantile(pp_white_rep_d, c(.025, .5, .975))

pp_black_rep_qr <- quantile(pp_black_rep_r, c(.025, .5, .975))
pp_black_rep_qd <- quantile(pp_black_rep_d, c(.025, .5, .975))

preds_rep <- data.frame(pp = c(pp_white_rep_qr[2], pp_white_rep_qd[2], pp_black_rep_qr[2], pp_black_rep_qd[2]),
                        high = c(pp_white_rep_qr[3], pp_white_rep_qd[3], pp_black_rep_qr[3], pp_black_rep_qd[3]),
                        low = c(pp_white_rep_qr[1], pp_white_rep_qd[1], pp_black_rep_qr[1], pp_black_rep_qd[1]),
                        president = "Bush/Trump",
                        race = rep(c("White","Black"), each = 2),
                        party = rep(c("Rep","Dem"), times = 2))

preds_stack <- bind_rows(preds_all, preds_dem, preds_rep)
# preds_stack$anxiety <- rep(c("Low","High"), 6)

order <- c("All","Clinton/Obama","Bush/Trump")
triple_int_diffs <- arrange(transform(preds_stack, president = factor(president, levels = order)))


# # interval95 <- -qnorm((1-0.95)/2)
# p1 <- ggplot(diffs, aes(race, pp, color = anxiety)) + geom_point(position=position_dodge(width = .5)) + coord_cartesian(ylim = c(0, 1))
# p1 <- p1 + facet_wrap(~president)
# p1 <- p1 + geom_errorbar(aes(x = race, ymin = low, ymax = high), width = 0, position = position_dodge(width = .5))
# # p1 <- p1 + geom_pointrange(aes(x = race, y = pp, ymin = pp - se*interval95, ymax = pp + se*interval95),
# #                            lwd = 1/2, position = position_dodge(width = 1/2), shape = 21)
# p1 <- p1  + labs(x = 'Race', y = 'Predicted Probability')
# 
# 
# 
# 
# ggsave(file = "~/Desktop/main_mod1.pdf", p1, width = 8, height = 4, units = 'in') # save plot
