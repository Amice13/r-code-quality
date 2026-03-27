pp_white_all_h <- vector()
pp_white_all_l <- vector()

pp_black_all_h <- vector()
pp_black_all_l <- vector()

pp_white_dem_h <- vector()
pp_white_dem_l <- vector()

pp_black_dem_h <- vector()
pp_black_dem_l <- vector()

pp_white_rep_h <- vector()
pp_white_rep_l <- vector()

pp_black_rep_h <- vector()
pp_black_rep_l <- vector()

set.seed(220)
for(i in 1:500){
  print(i)
all <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization
           + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
             income50t75 + senior + under30 + someCollege + baDegree + 
             postGrad + rural + suburban + yr_factor,
           data = slice_sample(crimeData, n = nrow(crimeData), replace = T), family = "binomial")

demdata <- subset(crimeData, president %in% c("Clinton","Obama"))
demdata$yr_factor <- droplevels(demdata$yr_factor)
dem <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization
           + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
             income50t75 + senior + under30 + someCollege + baDegree + 
             postGrad + rural + suburban + yr_factor,
           data = slice_sample(demdata, n = nrow(demdata), replace = T), 
           family = "binomial")

repdata <- subset(crimeData, president %in% c("Bush","Trump"))
repdata$yr_factor <- droplevels(repdata$yr_factor)
rep <- glm(presapp ~ copartisan + independent + worryScale * black + crime_victimization
           + chgCrimeRate + male + incomeLt20 + income20t30 + income30t50 + 
             income50t75 + senior + under30 + someCollege + baDegree + 
             postGrad + rural + suburban + yr_factor,
           data = slice_sample(repdata, n = nrow(repdata), replace = T),
           family = "binomial")


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
  
  pp_white_all_h[i] <- predict(all, synthwhite_all, type = "response", se = F)[2]
  pp_white_all_l[i] <- predict(all, synthwhite_all, type = "response", se = F)[1]
  
  pp_black_all_h[i] <- predict(all, synthblack_all, type = "response", se = F)[2]
  pp_black_all_l[i] <- predict(all, synthblack_all, type = "response", se = F)[1]
  
  pp_white_dem_h[i] <- predict(dem, synthwhite_dem, type = "response", se = F)[2]
  pp_white_dem_l[i] <- predict(dem, synthwhite_dem, type = "response", se = F)[1]
  
  pp_black_dem_h[i] <- predict(dem, synthblack_dem, type = "response", se = F)[2]
  pp_black_dem_l[i] <- predict(dem, synthblack_dem, type = "response", se = F)[1]
  
  pp_white_rep_h[i] <- predict(rep, synthwhite_rep, type = "response", se = F)[2]
  pp_white_rep_l[i] <- predict(rep, synthwhite_rep, type = "response", se = F)[1]
  
  pp_black_rep_h[i] <- predict(rep, synthblack_rep, type = "response", se = F)[2]
  pp_black_rep_l[i] <- predict(rep, synthblack_rep, type = "response", se = F)[1]
}



pp_white_all_qh <- quantile(pp_white_all_h, c(.025, .5, .975))
pp_white_all_ql <- quantile(pp_white_all_l, c(.025, .5, .975))

pp_black_all_qh <- quantile(pp_black_all_h, c(.025, .5, .975))
pp_black_all_ql <- quantile(pp_black_all_l, c(.025, .5, .975))

preds_all <- data.frame(pp = c(pp_white_all_qh[2], pp_white_all_ql[2], pp_black_all_qh[2], pp_black_all_ql[2]),
                        high = c(pp_white_all_qh[3], pp_white_all_ql[3], pp_black_all_qh[3], pp_black_all_ql[3]),
                        low = c(pp_white_all_qh[1], pp_white_all_ql[1], pp_black_all_qh[1], pp_black_all_ql[1]),
                        president = "All",
                        race = rep(c("White","Black"), each = 2),
                        anxiety = rep(c("High","Low"), times = 2))

pp_white_dem_qh <- quantile(pp_white_dem_h, c(.025, .5, .975))
pp_white_dem_ql <- quantile(pp_white_dem_l, c(.025, .5, .975))

pp_black_dem_qh <- quantile(pp_black_dem_h, c(.025, .5, .975))
pp_black_dem_ql <- quantile(pp_black_dem_l, c(.025, .5, .975))

preds_dem <- data.frame(pp = c(pp_white_dem_qh[2], pp_white_dem_ql[2], pp_black_dem_qh[2], pp_black_dem_ql[2]),
                        high = c(pp_white_dem_qh[3], pp_white_dem_ql[3], pp_black_dem_qh[3], pp_black_dem_ql[3]),
                        low = c(pp_white_dem_qh[1], pp_white_dem_ql[1], pp_black_dem_qh[1], pp_black_dem_ql[1]),
                        president = "Clinton/Obama",
                        race = rep(c("White","Black"), each = 2),
                        anxiety = rep(c("High","Low"), times = 2))

pp_white_rep_qh <- quantile(pp_white_rep_h, c(.025, .5, .975))
pp_white_rep_ql <- quantile(pp_white_rep_l, c(.025, .5, .975))

pp_black_rep_qh <- quantile(pp_black_rep_h, c(.025, .5, .975))
pp_black_rep_ql <- quantile(pp_black_rep_l, c(.025, .5, .975))

preds_rep <- data.frame(pp = c(pp_white_rep_qh[2], pp_white_rep_ql[2], pp_black_rep_qh[2], pp_black_rep_ql[2]),
                        high = c(pp_white_rep_qh[3], pp_white_rep_ql[3], pp_black_rep_qh[3], pp_black_rep_ql[3]),
                        low = c(pp_white_rep_qh[1], pp_white_rep_ql[1], pp_black_rep_qh[1], pp_black_rep_ql[1]),
                        president = "Bush/Trump",
                        race = rep(c("White","Black"), each = 2),
                        anxiety = rep(c("High","Low"), times = 2))

preds_stack <- bind_rows(preds_all, preds_dem, preds_rep)
# preds_stack$anxiety <- rep(c("Low","High"), 6)

order <- c("All","Clinton/Obama","Bush/Trump")
diffs <- arrange(transform(preds_stack, president = factor(president, levels = order)))

# interval95 <- -qnorm((1-0.95)/2)
p1 <- ggplot(diffs, aes(race, pp, color = anxiety)) + geom_point(position=position_dodge(width = .5)) + coord_cartesian(ylim = c(0, 1))
p1 <- p1 + facet_wrap(~president)
p1 <- p1 + geom_errorbar(aes(x = race, ymin = low, ymax = high), width = 0, position = position_dodge(width = .5))
# p1 <- p1 + geom_pointrange(aes(x = race, y = pp, ymin = pp - se*interval95, ymax = pp + se*interval95),
#                            lwd = 1/2, position = position_dodge(width = 1/2), shape = 21)
p1 <- p1  + labs(x = 'Race', y = 'Predicted Probability')




ggsave(file = "~/Desktop/main_mod1.pdf", p1, width = 8, height = 4, units = 'in') # save plot
