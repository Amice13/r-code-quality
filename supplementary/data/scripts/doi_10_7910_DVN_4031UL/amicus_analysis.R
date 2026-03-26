library(tidyverse)
library(xtable)
library(RecordLinkage)


### Read the MTurk results in
dat = read.csv("mturk_eval.csv")
dat = dat[,c("Input.item1_title", "Input.item2_title", "Answer.equal.label")]
dat$label = ifelse(dat$Answer.equal.label == "Yes", 1, 0)


dat2 = dat %>% group_by(Input.item1_title, Input.item2_title) %>%
  summarize(label = mean(label))
#write.csv(dat2, file="mturk_results_cleaned.csv")


## Read in the HITL results
res = read.csv("final_results.csv")
res$amicus = toupper(res$amicus)
res$bonica = toupper(res$bonica)


dat3 = merge(dat2, res, by.x=c("Input.item1_title", "Input.item2_title"),
             by.y=c("amicus", "bonica"))


## add jaro-winkler
dat3$jw = RecordLinkage::jarowinkler(str1 = dat3$Input.item1_title,
                                     str2 = dat3$Input.item2_title,
                                     r = 0.1)
summary(dat3$jw)

dat3$hitl_cut = cut(dat3$hitl_score, breaks = seq(0, 1, by=.05))
dat3$naive_cut = cut(dat3$basic_score, breaks = seq(0, 1, by=.05))
dat3$cosine_cut = cut(dat3$cosine, breaks = seq(0, 1, by=.05))
dat3$jaccard_cut = cut(dat3$jaccard, breaks = seq(0, 1, by=.05))
dat3$overlap_cut = cut(dat3$overlap, breaks = seq(0, 1, by=.05))
dat3$jw_cut = cut(dat3$jw, breaks = seq(-0.1, 1, by=.05))

dat3$label2 = ifelse(dat3$label.x > 0.5, 1, 0)

# melt so each row is a metric_cut and label (or label2)
plotdf = dat3 %>% select(c(contains("cut"), label.x, Input.item1_title)) %>%
  reshape2::melt(id.vars=c("Input.item1_title", "label.x")) %>%
  group_by(variable, value) %>%
  summarize(precision = mean(label.x))

val.to.x = data.frame(value = unique(plotdf$value), x = seq(0.0, 1, by=0.05))

plotdf = merge(plotdf, val.to.x)
plotdf$x[plotdf$value == "(-0.05,0]"] = 0

## in-text figures about precision
plotdf %>% filter(variable == "hitl_cut")

plotdf %>% filter(variable == "jw_cut")

plotdf %>% filter(variable == "jaccard_cut")


dat3[dat3$label2 == 0 & dat3$hitl_score > 0.97, ]


## Make Fig 1


plotdf$variable2 = dplyr::recode(plotdf$variable, hitl_cut = "HITL", naive_cut = "Base Model",
                                 jaccard_cut = "Jaccard", cosine_cut = "Cosine",
                                 overlap_cut = "n-Gram Overlap", jw_cut = "Jaro-Winkler")

p = ggplot(plotdf) + 
  geom_hline(yintercept=0.5, col="lightgrey", linetype="dashed", lwd=2) +
  ylim(c(0,1)) + xlim(c(0,1))+
  geom_smooth(aes(x = x, y=precision), se=F) + facet_wrap(~variable2) +
  theme_bw() + xlab("Match Score") + ylab("Precision: True Positives/Total Matches Found")
p
ggsave("fig1.pdf")

library(pROC)

pROC::roc(dat3, response=label2, predictor=hitl_score)
pROC::roc(dat3, response=label2, predictor=basic_score)


pdf("figb1.pdf")
plot(roc(dat3, response=label2, predictor=hitl_score), print.auc = TRUE, 
     col = "black",  xlim=c(1,0))
plot(roc(dat3, response=label2, predictor=basic_score), print.auc = TRUE, 
     col = "red", print.auc.y = .4, add = TRUE)
plot(roc(dat3, response=label2, predictor=jaccard), print.auc = TRUE, 
     col = "blue", print.auc.y = .3, add = TRUE)

text(x = rep(0.2, 3), y=c(.475,.375,.275),
     labels = c("HITL", "Basic Model", "Jaccard"), pos=4,
     col=c("black", "red", "blue"))
dev.off()


## Appendix figure: AUC by iteration
dat = read.csv("AUC_table.csv")

png("figb2.png")
plot(dat$Iteration.., dat$AUC, type = 'l',
     xlab = "HITL Iteration", ylab="AUC")
axis(1,at=1:10)
dev.off()


## Appendix table: feature importance by iteration
dat = read.csv("feature_importances_by_iteration.csv")
dat = dat %>% relocate(Iteration.Number)
colnames(dat) = c("Iteration", "Cosine", "Jaccard",
                  "Levenshtein", "LCSSTR", "Overlap", "True Positives",
                  "True Negatives", "Task")

writeLines(print(xtable::xtable(dat[,-9]), include.rownames=FALSE), "tableb5.tex")
