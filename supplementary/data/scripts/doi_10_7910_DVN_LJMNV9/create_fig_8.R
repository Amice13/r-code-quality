# This R-file is generating the Figure 8

library(readstata13)
library(ggplot2)
library(dotwhisker)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

results_by_party = read.dta13("Data/model5_yeshuvlong_allparties.dta")
results_by_party$party = c("Right Bloc", "Rightbloc2", "Likud", "Labor", "IB", "Kadima", "BY", "Shas")

results_to_plot = data.frame(term = "Fixed effects models",
                             estimate = results_by_party$estimate,
                             std.error = results_by_party$stderr,
                             model = results_by_party$party,
                             stringsAsFactors = FALSE)

results_to_plot = results_to_plot[results_to_plot$model %in% c("Right Bloc", "Likud", "Labor", "IB", "BY", "Shas"),]
results_to_plot$model = factor(results_to_plot$model, levels = c("Right Bloc", "Likud", "BY", "IB",  "Shas", "Labor"))

small_multiple(results_to_plot) +
  ylab("IH Exposure") + theme_bw()+
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(legend.position = "none")
ggsave("Figures/Fig_8.pdf", width=7, height=4)
