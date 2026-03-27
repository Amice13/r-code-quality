# This R-file is generating the Figure SI-4

library(stm)
library(dotwhisker)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

load("Data/stm_model.rdata")

labelTopics(stm_model$stm_topics, n=10)

content_covars <- estimateEffect(1:15 ~ newspaper , stm_model$stm_topics, meta = stm_model$out$meta, uncertainty = "Global")
sum_content_covars = summary(content_covars)

term = c("1. Education, school teacher, student",
         "2. Harpaz political scandal",
         "3. Murder, police, crime",
         "4. Budget, economy, market",
         "5. Elections, Likud, Liberman, Kadima, coalition",
         "6. Gilad Shalit, hostage deal",
         "7. Iran, nuclear",
         "8. Accident, traffic, vehicle",
         "9. Holocaust, memorial, independence",
         "10. Strike, unemployment, factory, worker",
         "11. Terror, assassination, france, passport",
         "12. Katzav, trial, rape",
         "13. Rocket, Qassam, Gaza, fighting, Sederot",
         "14. Palestinian, Abu Mazen, settlements",
         "15. Olmert corruption trial")



estimate = rep(NA, 15)
std.error = rep(NA, 15)

for(i in 1:length(sum_content_covars$tables)){

  estimate[i] = sum_content_covars$tables[[i]][2,1]
  std.error[i] = sum_content_covars$tables[[i]][2,2]

}

stm_results = data.frame("term" = term, "estimate" = estimate, "std.error"= std.error)

dwplot(stm_results,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Israel Hayom . . . . . . . . . . . . . . . Yediot Ahronot") +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  scale_colour_grey(start = 0, end = 0)
ggsave(file="Figures/Fig_SI-4.pdf", width=7, height=4)
