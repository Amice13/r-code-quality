library(readr)
library(tidyverse)
library(ggplot2)
library(ranger)
library(randomForest)
library(interplot)
library(gridExtra)

##Data Processing
d <- t(read_csv("[FILE PATH]/replication_main1.csv", skip = 6))
colnames(d) <- d[1,]
d <- data.frame(d[-1,])
d$influential.agents. <- recode(d$influential.agents., true = 1, false = 0)
d$target.influential <- recode(d$target.influential, true = 1, false = 0)
for(i in 1:ncol(d)) {
  d[,i] <- as.numeric(d[,i])
}

e <- t(read_csv("[FILE PATH]/replication_main2.csv", skip = 6))
colnames(e) <- e[1,]
e <- data.frame(e[-1,])
e$influential.agents. <- recode(e$influential.agents., true = 1, false = 0)
e$target.influential <- recode(e$target.influential, true = 1, false = 0)
for(i in 1:ncol(e)) {
  e[,i] <- as.numeric(e[,i])
}

f <- t(read_csv("[FILE PATH]/replication_main3.csv", skip = 6))
colnames(f) <- f[1,]
f <- data.frame(f[-1,])
f$influential.agents. <- recode(f$influential.agents., true = 1, false = 0)
f$target.influential <- recode(f$target.influential, true = 1, false = 0)
for(i in 1:ncol(f)) {
  f[,i] <- as.numeric(f[,i])
}

g <- t(read_csv("[FILE PATH]/replication_main4.csv", skip = 6))
colnames(g) <- g[1,]
g <- data.frame(g[-1,])
g$influential.agents. <- recode(g$influential.agents., true = 1, false = 0)
g$target.influential <- recode(g$target.influential, true = 1, false = 0)
for(i in 1:ncol(g)) {
  g[,i] <- as.numeric(g[,i])
}

h <- t(read_csv("[FILE PATH]/replication_main5.csv", skip = 6))
colnames(h) <- h[1,]
h <- data.frame(h[-1,])
h$influential.agents. <- recode(h$influential.agents., true = 1, false = 0)
h$target.influential <- recode(h$target.influential, true = 1, false = 0)
for(i in 1:ncol(h)) {
  h[,i] <- as.numeric(h[,i])
}

j <- t(read_csv("[FILE PATH]/replication_main6.csv", skip = 6))
colnames(j) <- j[1,]
j <- data.frame(j[-1,])
j$influential.agents. <- recode(j$influential.agents., true = 1, false = 0)
j$target.influential <- recode(j$target.influential, true = 1, false = 0)
for(i in 1:ncol(j)) {
  j[,i] <- as.numeric(j[,i])
}

k <- t(read_csv("[FILE PATH]/replication_main7.csv", skip = 6))
colnames(k) <- k[1,]
k <- data.frame(k[-1,])
k$influential.agents. <- recode(k$influential.agents., true = 1, false = 0)
k$target.influential <- recode(k$target.influential, true = 1, false = 0)
for(i in 1:ncol(k)) {
  k[,i] <- as.numeric(k[,i])
}

l <- t(read_csv("[FILE PATH]/replication_main8.csv", skip = 6))
colnames(l) <- l[1,]
l <- data.frame(l[-1,])
l$influential.agents. <- recode(l$influential.agents., true = 1, false = 0)
l$target.influential <- recode(l$target.influential, true = 1, false = 0)
for(i in 1:ncol(l)) {
  l[,i] <- as.numeric(l[,i])
}

d <- rbind(d, e, f, g, h, j, k, l)



##Example Runs
sub1 <- subset(d, d$share.type.1==0.7 & d$prob.link.other==0.5 & d$probability.of.cooperation==0.2 & d$learning==0.5 & d$influential.agents.==0 &
                 d$int.on.prob.coop==0 & d$int.on.network==0 & d$int.share.affected==0.15 & d$reward.for.coop==0 & d$penalty.for.def==0 & d$target.influential==0)
sub1 <- data.frame(t(sub1[,-1:-26]))[1:30,]
sub1$mean <- NA
sub1$lb <- NA
sub1$ub <- NA
for(i in 1:nrow(sub1)) {
  sub1$mean[i] <- mean(as.numeric(sub1[i, 1:10]))
  sub1$lb[i] <- quantile(as.numeric(sub1[i, 1:10]), probs = 0.95)
  sub1$ub[i] <- quantile(as.numeric(sub1[i, 1:10]), probs = 0.05)
}


sub2 <- subset(d, d$share.type.1==0.7 & d$prob.link.other==0.5 & d$probability.of.cooperation==0.2 & d$learning==0.5 & d$influential.agents.==0 &
                 d$int.on.prob.coop==0.8 & d$int.on.network==0.6 & d$int.share.affected==0.2 & d$reward.for.coop==0 & d$penalty.for.def==0 & d$target.influential==0)
sub2 <- data.frame(t(sub2[,-1:-26]))[1:30,]
sub2$mean <- NA
sub2$lb <- NA
sub2$ub <- NA
for(i in 1:nrow(sub2)) {
  sub2$mean[i] <- mean(as.numeric(sub2[i, 1:10]))
  sub2$lb[i] <- quantile(as.numeric(sub2[i, 1:10]), probs = 0.95)
  sub2$ub[i] <- quantile(as.numeric(sub2[i, 1:10]), probs = 0.05)
}

figure_1 <- ggplot() +
  geom_line(data=sub1, aes(x=seq(1:30), y=mean*100, color="orange"), linewidth = 1) +
  geom_ribbon(data = sub1, aes(x = seq(1:30), ymin = as.numeric(lb*100), ymax = as.numeric(ub*100),
                                alpha = 0.9), fill = "orange") +
  geom_line(data=sub2, aes(x=seq(1:30), y=mean*100, color="blue"), linewidth = 1) +
  geom_ribbon(data = sub2, aes(x = seq(1:30), ymin = as.numeric(lb*100), ymax = as.numeric(ub*100),
                                 alpha = 0.9), fill = "blue") +
  scale_color_identity(name = "",
                       breaks = c("orange", "blue"),
                       labels = c("No Intervention", "Significant Intervention"),
                       guide = "legend") +
  xlab("Round") +
  ylab("Intergroup Cooperation (%)") +
  guides(fill = "none", alpha = "none") + 
  theme_light() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 20))
figure_1

##Bias-corrected Random Forest
mod_crf <- ranger(inc_end ~ ., data = d1, num.trees = 500, splitrule = "maxstat", importance = "permutation")
importance <- mod_crf$variable.importance

imp_df <- data.frame(importance)
imp_df$Var.Names <- c("(C) Population Ratio", "(C) Intergroup Connectivity", "(C) Social Learning",
                      "(C) Social Structure", "(I) Direct Effect on Cooperation", "(I) Intervention on Social Network",
                      "(I) Reward for Cooperation", "(I) Punishment for Defection", "(I) Target Influential Individuals",
                      "(I) Intervention Scale")
imp_ordered <- imp_df %>%
  mutate(Var.Names = factor(Var.Names, levels = c("(I) Target Influential Individuals", "(I) Punishment for Defection",
                                                  "(I) Reward for Cooperation", "(I) Intervention Scale", 
                                                  "(I) Intervention on Social Network", "(I) Direct Effect on Cooperation",
                                                  "(C) Social Structure", "(C) Social Learning",
                                                  "(C) Intergroup Connectivity", "(C) Population Ratio"
  )))
ggplot(imp_ordered, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment(aes(x=Var.Names, xend=Var.Names, y=0, yend=`importance`), color="skyblue", linewidth = 10) +
  theme_light(base_size = 16) +
  coord_flip() +
  xlab("Parameters") +
  ylab("Parameter Importance")

##Regressions
d$scaled_final <- d$X.final.*100
mod1 <- lm(scaled_final ~ int.on.prob.coop + int.on.network + int.share.affected +
             reward.for.coop + penalty.for.def + target.influential, data=d)
summary(mod1)

mod2 <- lm(scaled_final ~ probability.of.cooperation + share.type.1 + prob.link.other +
             learning + influential.agents., data=d)
summary(mod2)

mod3 <- lm(scaled_final ~ int.on.prob.coop + int.on.network + int.share.affected +
             reward.for.coop + penalty.for.def + target.influential +
             probability.of.cooperation + share.type.1 + prob.link.other +
             learning + influential.agents., data=d)
summary(mod3)

mod4 <- lm(scaled_final ~ int.on.prob.coop + int.on.network + int.share.affected +
             reward.for.coop + penalty.for.def + target.influential +
             probability.of.cooperation + share.type.1 + prob.link.other +
             learning + influential.agents. +
             int.on.prob.coop*int.on.network + int.on.prob.coop*reward.for.coop +
             int.on.prob.coop*learning + reward.for.coop*learning +
             target.influential*influential.agents., data=d)
summary(mod4)

p1 <- interplot(m = mod4, var1 = "int.on.prob.coop", var2 = "int.on.network", esize = 3, size = 5) +
  theme_light(base_size = 16) +
  ylab("Coefficient on Direct Intervention on Cooperation") +
  xlab("Intervention on Social Network") 

p2 <- interplot(m = mod4, var1 = "int.on.prob.coop", var2 = "reward.for.coop", esize = 3, size = 5) +
  theme_light(base_size = 16) +
  ylab("Coefficient on Direct Intervention on Cooperation") +
  xlab("Reward for Cooperation")

p3 <- interplot(m = mod4, var1 = "int.on.prob.coop", var2 = "learning", point = TRUE, esize = 3, size = 5) +
  theme_light(base_size = 16) +
  ylab("Coefficient on Direct Intervention on Cooperation") +
  xlab("Social Learning")

grid.arrange(p1, p2, p3, ncol=3)


##Appendix Random Forest, main data, equilibrium cooperation

d1 <- data.frame(
  inc_end = d$X.final. - d$NA..1,
  poprat = d$share.type.1,
  net_init = d$prob.link.other,
  learning = d$learning,
  net_struct = d$influential.agents.,
  int_coop = d$int.on.prob.coop,
  int_net = d$int.on.network,
  int_reward = d$reward.for.coop,
  int_penalty = d$penalty.for.def,
  int_target = d$target.influential,
  int_scale = d$int.share.affected
)

mod_rf <- randomForest(inc_end ~ ., data = d1, ntree = 100,
                       keep.forest = FALSE, importance = TRUE)
imp <- as.data.frame(importance(mod_rf))
imp$Var.Names <- c("(C) Population Ratio", "(C) Intergroup Connectivity", "(C) Social Learning",
                       "(C) Social Structure", "(I) Direct Effect on Cooperation", "(I) Intervention on Social Network",
                       "(I) Reward for Cooperation", "(I) Punishment for Defection", "(I) Target Influential Individuals",
                       "(I) Intervention Scale")
imp$SD <- mod_rf$importanceSD
imp_ordered <- imp %>%
  mutate(Var.Names = factor(Var.Names, levels = c("(I) Target Influential Individuals", "(I) Punishment for Defection",
                                                  "(I) Reward for Cooperation", "(I) Intervention Scale", 
                                                  "(I) Intervention on Social Network", "(I) Direct Effect on Cooperation",
                                                  "(C) Social Structure", "(C) Social Learning",
                                                  "(C) Intergroup Connectivity", "(C) Population Ratio"
  )))

ggplot(imp_ordered, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment(aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue", linewidth = 10) +
  theme_light(base_size = 16) +
  coord_flip() +
  xlab("Parameters") +
  ylab("Predictive Importance")

  

##Appendix Random Forest, main data, maximum cooperation
d2 <- data.frame(
  inc_max = d$X.max. - d$NA..1,
  poprat = d$share.type.1,
  net_init = d$prob.link.other,
  learning = d$learning,
  net_struct = d$influential.agents.,
  int_coop = d$int.on.prob.coop,
  int_net = d$int.on.network,
  int_reward = d$reward.for.coop,
  int_penalty = d$penalty.for.def,
  int_target = d$target.influential,
  int_scale = d$int.share.affected
)

mod_rf2 <- randomForest(inc_max ~ ., data = d2, ntree = 100,
                       keep.forest = FALSE, importance = TRUE)
ImpData2 <- as.data.frame(importance(mod_rf2))
ImpData2$Var.Names <- c("(C) Population Ratio", "(C) Intergroup Connectivity", "(C) Social Learning",
                       "(C) Social Structure", "(I) Direct Effect on Cooperation", "(I) Intervention on Social Network",
                       "(I) Reward for Cooperation", "(I) Punishment for Defection", "(I) Target Influential Individuals",
                       "(I) Intervention Scale")
ImpData2$SD <- mod_rf2$importanceSD
ID2 <- ImpData2 %>%
  mutate(Var.Names = factor(Var.Names, levels = c("(I) Target Influential Individuals", "(I) Punishment for Defection",
                                                  "(I) Reward for Cooperation", "(I) Intervention Scale", 
                                                  "(I) Intervention on Social Network", "(I) Direct Effect on Cooperation",
                                                  "(C) Social Structure", "(C) Social Learning",
                                                  "(C) Intergroup Connectivity", "(C) Population Ratio"
  )))

ggplot(ID2, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment(aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue", linewidth = 10) +
  theme_light(base_size = 16) +
  coord_flip() +
  xlab("Parameters") +
  ylab("Predictive Importance")

##Appendix analyses on alternate data

##Process data
r1 <- t(read_csv("[FILE PATH]/replication_appendix1.csv", 
                         skip = 6))
colnames(r1) <- r1[1,]
r1 <- data.frame(r1[-1,])
r1$influential.agents. <- recode(r1$influential.agents., true = 1, false = 0)
r1$target.influential <- recode(r1$target.influential, true = 1, false = 0)
for(i in 1:ncol(r1)) {
  r1[,i] <- as.numeric(r1[,i])
}

r2 <- t(read_csv("[FILE PATH]/replication_appendix2.csv", 
                 skip = 6))
colnames(r2) <- r2[1,]
r2 <- data.frame(r2[-1,])
r2$influential.agents. <- recode(r2$influential.agents., true = 1, false = 0)
r2$target.influential <- recode(r2$target.influential, true = 1, false = 0)
for(i in 1:ncol(r2)) {
  r2[,i] <- as.numeric(r2[,i])
}

r3 <- t(read_csv("[FILE PATH]/replication_appendix3.csv", 
                 skip = 6))
colnames(r3) <- r3[1,]
r3 <- data.frame(r3[-1,])
r3$influential.agents. <- recode(r3$influential.agents., true = 1, false = 0)
r3$target.influential <- recode(r3$target.influential, true = 1, false = 0)
for(i in 1:ncol(r3)) {
  r3[,i] <- as.numeric(r3[,i])
}

r4 <- t(read_csv("[FILE PATH]/replication_appendix4.csv", 
                 skip = 6))
colnames(r4) <- r4[1,]
r4 <- data.frame(r4[-1,])
r4$influential.agents. <- recode(r4$influential.agents., true = 1, false = 0)
r4$target.influential <- recode(r4$target.influential, true = 1, false = 0)
for(i in 1:ncol(r4)) {
  r4[,i] <- as.numeric(r4[,i])
}

d <- rbind(r1, r2, r3, r4)

##Random forest, alternate data
d1 <- data.frame(
  inc_end = d$X.final. - d$NA..1,
  poprat = d$share.type.1,
  net_init = d$prob.link.other,
  learning = d$learning,
  net_struct = d$influential.agents.,
  int_coop = d$int.on.prob.coop,
  int_net = d$int.on.network,
  int_reward = d$reward.for.coop,
  int_penalty = d$penalty.for.def,
  int_target = d$target.influential,
  int_scale = d$int.share.affected
)

mod_rf2 <- randomForest(inc_end ~ ., data = d1, ntree = 100,
                       keep.forest = FALSE, importance = TRUE)
imp2 <- as.data.frame(importance(mod_rf2))
imp2$Var.Names <- c("(C) Population Ratio", "(C) Intergroup Connectivity", "(C) Social Learning",
                       "(C) Social Structure", "(I) Direct Effect on Cooperation", "(I) Intervention on Social Network",
                       "(I) Reward for Cooperation", "(I) Punishment for Defection", "(I) Target Influential Individuals",
                       "(I) Intervention Scale")
imp2$SD <- mod_rf2$importanceSD
imp2_ordered <- imp2 %>%
  mutate(Var.Names = factor(Var.Names, levels = c("(I) Target Influential Individuals", "(I) Punishment for Defection",
                                                  "(I) Reward for Cooperation", "(I) Intervention Scale", 
                                                  "(I) Intervention on Social Network", "(I) Direct Effect on Cooperation",
                                                  "(C) Social Structure", "(C) Social Learning",
                                                  "(C) Intergroup Connectivity", "(C) Population Ratio"
  )))

ggplot(imp2_ordered, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment(aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue", linewidth = 10) +
  theme_light(base_size = 16) +
  coord_flip() +
  xlab("Parameters") +
  ylab("Predictive Importance")


##Appendix Regressions on alternate data
d$scaled_final <- d$X.final.*100
mod1 <- lm(scaled_final ~ int.on.prob.coop + int.on.network + int.share.affected +
             reward.for.coop + penalty.for.def + target.influential, data=d)
summary(mod1)

mod2 <- lm(scaled_final ~ probability.of.cooperation + share.type.1 + prob.link.other +
             learning + influential.agents., data=d)
summary(mod2)

mod3 <- lm(scaled_final ~ int.on.prob.coop + int.on.network + int.share.affected +
             reward.for.coop + penalty.for.def + target.influential +
             probability.of.cooperation + share.type.1 + prob.link.other +
             learning + influential.agents., data=d)
summary(mod3)

mod4 <- lm(scaled_final ~ int.on.prob.coop + int.on.network + int.share.affected +
             reward.for.coop + penalty.for.def + target.influential +
             probability.of.cooperation + share.type.1 + prob.link.other +
             learning + influential.agents. +
             int.on.prob.coop*int.on.network + int.on.prob.coop*reward.for.coop +
             int.on.prob.coop*learning + reward.for.coop*learning +
             target.influential*influential.agents., data=d)
summary(mod4)

p1 <- interplot(m = mod4, var1 = "int.on.prob.coop", var2 = "int.on.network", esize = 3, size = 5) +
  theme_light(base_size = 16) +
  ylab("Coefficient on Direct Intervention on Cooperation") +
  xlab("Intervention on Social Network") 

p2 <- interplot(m = mod4, var1 = "int.on.prob.coop", var2 = "reward.for.coop", esize = 3, size = 5) +
  theme_light(base_size = 16) +
  ylab("Coefficient on Direct Intervention on Cooperation") +
  xlab("Reward for Cooperation")

p3 <- interplot(m = mod4, var1 = "int.on.prob.coop", var2 = "learning", point = TRUE, esize = 3, size = 5) +
  theme_light(base_size = 16) +
  ylab("Coefficient on Direct Intervention on Cooperation") +
  xlab("Social Learning")

grid.arrange(p1, p2, p3, ncol=3)