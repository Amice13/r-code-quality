# This R-file is generating the Figure 3

library(ggplot2)
library(ggeffects)
library(dotwhisker)
library(gridExtra)
library(lemon)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")
getwd()

load("Data/newspapers.rdata")
newspapers$front_pages

## A. Right-wing slant

mod1_rw = lm(score_normalized ~ ih, data=newspapers$front_pages)
mod2_rw = lm(score_normalized ~ ih, data=newspapers$news_pages)
mod3_rw = lm(score_normalized ~ ih, data=newspapers$op_eds)

# Average right-wing slant across all of the above sections
all_sections = rbind(newspapers$front_pages, newspapers$news_pages, newspapers$op_eds)
mean_score_rw = mean(all_sections$score_normalized)

preds_front_page = ggpredict(mod1_rw, terms = c("ih"))
preds_front_page$Location = "Front page"
preds_news_pages = ggpredict(mod2_rw, terms = c("ih"))
preds_news_pages$Location = "News pages"
preds_opeds = ggpredict(mod3_rw, terms = c("ih"))
preds_opeds$Location = "Op-Eds"

preds = rbind(preds_front_page, preds_news_pages, preds_opeds)
preds$Location <- factor(preds$Location, levels = c("Front page", "News pages", "Op-Eds"))
preds$Newspaper = "Israel Hayom"
preds$Newspaper[preds$x==0] = "Yediot Ahronot"
preds = as.data.frame(preds)
preds$Newspaper = as.factor(preds$Newspaper)
preds$term = preds$Location
preds$estimate = preds$predicted
preds$model = preds$Newspaper

rw_dwplot = dwplot(preds,
                   vline = geom_vline(xintercept = mean_score_rw, colour = "grey60", linetype = 2),
                   dot_args = list(aes(shape = model))) +
  theme_bw() + xlab("Right-wing slant score") +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.key.size = unit(10, "pt"), legend.key = element_rect(size = 5))+
  scale_colour_grey(start = 0, end = .5,
                    name = "Newspaper") +
  scale_shape_discrete(name = "Newspaper") + ggtitle("A. Right-wing slant")

## B. Positive coverage

mod1_pc = lm(pc_score_normalized ~ ih, data=newspapers$front_pages)
mod2_pc = lm(pc_score_normalized ~ ih, data=newspapers$news_pages)
mod3_pc = lm(pc_score_normalized ~ ih, data=newspapers$op_eds)

## Average positive covrerage across all of the above sections
mean_score_pc = mean(all_sections$pc_score_normalized)

preds_front_page = ggpredict(mod1_pc, terms = c("ih"))
preds_front_page$Location = "Front page"
preds_news_pages = ggpredict(mod2_pc, terms = c("ih"))
preds_news_pages$Location = "News pages"
preds_opeds = ggpredict(mod3_pc, terms = c("ih"))
preds_opeds$Location = "Op-Eds"

preds = rbind(preds_front_page, preds_news_pages, preds_opeds)
preds$Location <- factor(preds$Location, levels = c("Front page", "News pages", "Op-Eds"))
preds$Newspaper = "Israel Hayom"
preds$Newspaper[preds$x==0] = "Yediot Ahronot"
preds = as.data.frame(preds)
preds$Newspaper = as.factor(preds$Newspaper)
preds$term = preds$Location
preds$estimate = preds$predicted
preds$model = preds$Newspaper

pc_dwplot = dwplot(preds,
                   vline = geom_vline(xintercept = mean_score_pc, colour = "grey60", linetype = 2),
                   dot_args = list(aes(shape = model))) +
  theme_bw() + xlab("Positive coverage score") +
  #theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.key.size = unit(10, "pt"), legend.key = element_rect(size = 5))+
  scale_colour_grey(start = 0, end = .5,
                    name = "Newspaper") +
  scale_shape_discrete(name = "Newspaper") + ggtitle("B. Positive coverage")

fig_3 = grid_arrange_shared_legend(rw_dwplot, pc_dwplot)
ggsave(file="Figures/Fig_3.pdf", fig_3, width=9, height=4)
