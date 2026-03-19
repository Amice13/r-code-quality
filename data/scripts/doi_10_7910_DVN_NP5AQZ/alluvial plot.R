library(ggalluvial)
library(scales)
library(ggrepel)

source("script.R")

sta <- reshape2::melt(
  survey,
  id.vars = c("ID","t"),
  measure.vars = c("pre","post","followup")
)

sta$belief <- ifelse(sta$value %in% 1:2, "Believe false campaing claim", ifelse(
  sta$value == 3, "Neither believe nor disbelieve", "Disbelieve false campaign claim"))

sta$belief <- factor(
  sta$belief, levels = c("Believe false campaing claim","Neither believe nor disbelieve", "Disbelieve false campaign claim")
)

cols = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6))

sta$facet <- ifelse(
  sta$t == "control", "Control group", ifelse(
    sta$t == "-fact", "Independent fact-checker", ifelse(
      sta$t == "-guardian", "The Guardian", "The Telegraph"
    )
  )
)

sta$variable <- stringr::str_to_title(sta$variable)
sta$variable <- factor(sta$variable, levels = c("Pre","Post","Followup"))

ggplot(sta, aes(x = variable, stratum = belief, alluvium = ID, fill = belief, label = belief)) + 
  geom_flow() +
  scale_fill_manual(values = cols) +
  geom_stratum(alpha = .5, linetype = "dotted", colour = "black") + 
  geom_label(stat = "stratum", fill = "white", size = 3, aes(label = percent(after_stat(prop),accuracy=1))) +
  xlab("") +
  ylab("Proportion") +
  facet_wrap(. ~ facet, ncol = 4) +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 12),
    panel.background = element_rect(fill = "white", color = "black"), 
    panel.border = element_rect(fill = NA, color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 12),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.ticks.x = element_line(color = "black"),
    panel.grid = element_blank())