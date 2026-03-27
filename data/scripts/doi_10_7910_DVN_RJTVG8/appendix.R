
##APPENDIX MATERIALS##

##TO BE COMPLETED AFTER RUNNING SCRIPT: replication_electoralstudies.R ##

amce.1 <- amce(df, selected ~ prereg + method + vwindow + trials + pilot + 
                 fraud + cost + platform + private + party + endorse, id = ~ IDvar)

labels <- c(prereg = "Pre-registration", method = "Method",
            vwindow = "Voting window", trials = "Successful trials in",
            pilot = "Result of pilot", fraud = "Effect on fraud",
            cost = "Costs", platform = "Administered by",
            private = "Private sector role", party = "Proposing party",
            endorse = "Endorsed by")

plot(amce.1, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_text(face="bold"))  

ggsave("AMCEplot_support.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")

amce(df, trustnormal~ prereg + method + vwindow + trials + pilot + 
       fraud + cost + platform + private + party + endorse, id = ~ IDvar)

amce.2 <- amce(df, trustnormal~ prereg + method + vwindow + trials + pilot + 
                 fraud + cost + platform + private + party + endorse, id = ~ IDvar)


plot(amce.2, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_text(face="bold")) 

ggsave("AMCEplot_trust.png", dpi=500, 
       path = here("publicationfigures"),
       height=30, width=21, units="cm")




#MM differences by internetSAT
stacked.internet <- cj(df, selected ~ prereg + method + vwindow + trials + pilot + 
                         fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                       estimate = "mm", by = ~ satisfied_internet)

net1<- plot(stacked.internet, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("blue2", "#E3256B"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.2,.90),
        legend.text = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.internet <- mm_diffs(df, selected ~ prereg + method + vwindow + trials + pilot + 
                               fraud + cost + platform + private + party + endorse, 
                             ~ satisfied_internet, id = ~ IDvar)
plot(mm.diff.internet, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())

##no labels
net2<- plot(mm.diff.internet, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

net1+net2
ggsave("MM_internet.png", dpi=500, 
       path = here("publicationfigures"),
       height=30, width=21, units="cm")


#MM TRUST differences by internetSAT
stacked.internet <- cj(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                         fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                       estimate = "mm", by = ~ satisfied_internet)

net1<- plot(stacked.internet, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("blue2", "#E3256B"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.2,.90),
        legend.text = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.internet <- mm_diffs(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                               fraud + cost + platform + private + party + endorse, 
                             ~ satisfied_internet, id = ~ IDvar)
plot(mm.diff.internet, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())

##no labels
net2<- plot(mm.diff.internet, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

net1+net2
ggsave("MM_internetTRUST.png", dpi=500, 
       path = here("publicationfigures"),
       height=30, width=21, units="cm")

#MM differences by METHOD
stacked.meth <- cj(df, selected ~ prereg + method + vwindow + trials + pilot + 
                     fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                   estimate = "mm", by = ~ votemethod)

meth1<- plot(stacked.meth, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("blue2", "#E3256B"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.29,.90),
        legend.text = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.meth <- mm_diffs(df, selected ~ prereg + method + vwindow + trials + pilot + 
                           fraud + cost + platform + private + party + endorse, 
                         ~ votemethod, id = ~ IDvar)
plot(mm.diff.meth, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())

##no labels
meth2<- plot(mm.diff.meth, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

meth1+meth2
ggsave("MM_method.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")


#MM TRUST differences by METHOD
stacked.meth2 <- cj(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                      fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                    estimate = "mm", by = ~ votemethod)

meth3<- plot(stacked.meth2, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("blue2", "#E3256B"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.35,.90),
        legend.text = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.meth2 <- mm_diffs(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                            fraud + cost + platform + private + party + endorse, 
                          ~ votemethod, id = ~ IDvar)
plot(mm.diff.meth2, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())

##no labels
meth4<- plot(mm.diff.meth2, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

meth3+meth4
ggsave("MM_methodTRUST.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")




#MM differences by TRUST
stacked.trust <- cj(df, selected ~ prereg + method + vwindow + trials + pilot + 
                      fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                    estimate = "mm", by = ~ trustbin)

trust1<- plot(stacked.trust, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("blue2", "#E3256B"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.35,.90),
        legend.text = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.trust <- mm_diffs(df, selected ~ prereg + method + vwindow + trials + pilot + 
                            fraud + cost + platform + private + party + endorse, 
                          ~ trustbin, id = ~ IDvar)
plot(mm.diff.trust, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())

##no labels
trust2<- plot(mm.diff.trust, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

trust1+trust2
ggsave("MM_trust.png", dpi=500, 
       path = here("publicationfigures"),
       height=30, width=21, units="cm")


#MM differences by mainstream vote
stacked.party <- cj(df, selected ~ prereg + method + vwindow + trials + pilot + 
                      fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                    estimate = "mm", by = ~ votetory)


party1<- plot(stacked.party, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("red2", "blue2"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.2,.90),
        legend.text = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.party <- mm_diffs(df, selected ~ prereg + method + vwindow + trials + pilot + 
                            fraud + cost + platform + private + party + endorse, 
                          ~ votetory, id = ~ IDvar)
plot(mm.diff.party, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())

party2<- plot(mm.diff.party, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

party1+party2
ggsave("MM_party.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")

#MM differences by mainstream vote
stacked.party <- cj(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                      fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                    estimate = "mm", by = ~ votetory)


party1<- plot(stacked.party, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("red2", "blue2"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.2,.90),
        legend.text = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.party <- mm_diffs(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                            fraud + cost + platform + private + party + endorse, 
                          ~ votetory, id = ~ IDvar)
plot(mm.diff.party, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())

party2<- plot(mm.diff.party, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

party1+party2
ggsave("MM_partyTRUST.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")


##MM diff by elec satisfaction##
stacked.elecsat <- cj(df, selected ~ prereg + method + vwindow + trials + pilot + 
                        fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                      estimate = "mm", by = ~ satisfied_elections)

ES1<- plot(stacked.elecsat, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("blue2", "#E3256B"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.2,.90),
        legend.text = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.elecstat<- mm_diffs(df, selected ~ prereg + method + vwindow + trials + pilot + 
                              fraud + cost + platform + private + party + endorse, 
                            ~ satisfied_elections, id = ~ IDvar)
ES2<- plot(mm.diff.elecstat, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

ES1+ES2
ggsave("MM_electoralsatisfaction.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")


##MM diff TRUST by elec satisfaction##
stacked.elecsat <- cj(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                        fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                      estimate = "mm", by = ~ satisfied_elections)

ES1<- plot(stacked.elecsat, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("blue2", "#E3256B"))+
  theme(strip.text = element_blank(),
        legend.position =c(-1.2,.90),
        legend.text = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


mm.diff.elecstat<- mm_diffs(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                              fraud + cost + platform + private + party + endorse, 
                            ~ satisfied_elections, id = ~ IDvar)
ES2<- plot(mm.diff.elecstat, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill="#ffffff"),
        strip.text = element_text(face="bold", size=7),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

ES1+ES2
ggsave("MM_electoralsatisfactionTRUST.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")

##Frequency##
plot(cj_freqs(df, ~ prereg + method + vwindow + trials+ pilot + 
                fraud + cost+ platform + private + party + endorse, id = ~ IDvar))+
  theme(legend.position = "none")+
  labs(title="Frequency of conjoint attribute values)")

ggsave("frequencyplot.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")


##Task sensitivity test##

stacked.tasks <- cj(df, selected ~ prereg + method + vwindow + trials + pilot + 
                      fraud + cost + platform + private + party + endorse, id = ~ IDvar,
                    estimate = "mm", by = ~ task)

task1<- plot(stacked.tasks, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y", strip.position = "right") + 
  labs(title="Sensitivity test: marginal mean by task iteration")+
  theme(strip.text = element_blank(),
        plot.title = element_text(face="bold"),
        legend.text = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, colour="gray33")


ggsave("MM_individualtasks.png", 
       path = here("publicationfigures"),
       dpi=500, height=30, width=21, units="cm")

###TABLES###

mm.1 <- mm(df, selected ~ prereg + method + vwindow + trials + pilot + 
             fraud + cost + platform + private + party + endorse, id = ~ IDvar, h0 = 0.5)
esttab <- select(mm.1,
                 "Feature" = feature,
                 "Level" = level,
                 "Est." = estimate,
                 "SE" = std.error,
                 "LowerCI" = lower,
                 "UpperCI" = upper)

export_mm <- bind_cols(esttab) %>%
  arrange(as.numeric(Feature), -as.numeric(Level)) %>%
  select(Level, Est., SE, LowerCI, UpperCI)

xtable::xtable(export_mm, 
               caption = "Marginal means: proposal selected", 
               digits = 3) -> esttab

xtable::print.xtable(esttab, 
                     include.rownames = F,
                     type = "latex",
                     file = "MMselected.tex",
                     path = here("publicationtables"))


mm.2 <- mm(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
             fraud + cost + platform + private + party + endorse, id = ~ IDvar, h0 = 0.5)
esttab2 <- select(mm.2,
                  "Feature" = feature,
                  "Level" = level,
                  "Est." = estimate,
                  "SE" = std.error,
                  "LowerCI" = lower,
                  "UpperCI" = upper)

export_mm2 <- bind_cols(esttab2) %>%
  arrange(as.numeric(Feature), -as.numeric(Level)) %>%
  select(Level, Est., SE, LowerCI, UpperCI)

xtable::xtable(export_mm2, 
               caption = "Marginal means: proposal trust", 
               digits = 3) -> esttab2

xtable::print.xtable(esttab2, 
                     include.rownames = F,
                     type = "latex",
                     file = "MMtrust.tex",
                     path = here("publicationtables"))

mm.3 <- mm(df, probnormal ~ prereg + method + vwindow + trials + pilot + 
             fraud + cost + platform + private + party + endorse, id = ~ IDvar, h0 = 0.5)
esttab3 <- select(mm.3,
                  "Feature" = feature,
                  "Level" = level,
                  "Est." = estimate,
                  "SE" = std.error,
                  "LowerCI" = lower,
                  "UpperCI" = upper)

export_mm3 <- bind_cols(esttab3) %>%
  arrange(as.numeric(Feature), -as.numeric(Level)) %>%
  select(Level, Est., SE, LowerCI, UpperCI)

xtable::xtable(export_mm3, 
               caption = "Marginal means: likelihood proposal endorsed", 
               digits = 3) -> esttab3

xtable::print.xtable(esttab3, 
                     include.rownames = F,
                     type = "latex",
                     file = "MMprob.tex",
                     path = here("publicationtables"))

models <- list()
models [['Support']] <- lm(selected ~ prereg + method + vwindow + trials + pilot + fraud + cost+ platform + private + party + endorse, data=df)
models [['Trust']] <- lm(trustnormal ~ prereg + method + vwindow + trials + pilot + fraud + cost+ platform + private + party + endorse, data=df)

modelsummary(models, stars=TRUE, output='latex')




##AMIE##
library(FindIt)
df <- df %>%
  mutate(vwindow = factor(vwindow, levels = c("Available for two weeks up to\n(and including) polling day","Available for up to two weeks\nbefore polling day",
                                              "Available on polling day only")),
         fraud = factor(fraud, levels=c("Decreases risk of fraud",
                                        "None", "Increase risk of fraud")),
         cost = factor(cost, levels = c("-£5 (five pounds less)", "-£1 (one pound less)",
                                        "-£0.50 (50p less)", "£0 (none)", "£0.50 (50p more)",
                                        "£1 (one pound more)", "£5 (five pounds more)")))

amie1 <- CausalANOVA(formula = selected ~ prereg + method + vwindow + trials + pilot + 
                       fraud + cost + platform + private + party + endorse,
                     data = df, 
                     pair.id = df$IDvar,
                     diff = T,
                     cluster = df$task,
                     nway = 2,
                     screen=T,
                     ord.fac = c("FALSE", "FALSE", "TRUE", "FALSE", "FALSE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE"))

amie1.1 <- summary(amie1)[[4]]
limit <- max(abs(amie1.1$AMIE)) * c(-1, 1)

ggplot(amie1.1, aes(y= Level2, x=Level1)) +
  geom_raster(aes(fill=AMIE)) +
  theme_minimal() +
  scale_fill_gradientn(colours=c("coral", "white", "dodgerblue"),
                       breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 
                                0.06, 0.08), limit=limit) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

ggsave("amies.png", 
       path = here("publicationfigures"),
       dpi=500, height=25, width=32, units="cm")


amie2 <- CausalANOVA(formula = selected ~ prereg + method + vwindow + trials + pilot + 
                       fraud + cost + platform + private + party + endorse,
                     data = df, 
                     pair.id = df$IDvar,
                     diff = T,
                     cluster = df$task,
                     nway = 2,
                     screen=F,
                     ord.fac = c("No", "No", "Yes", "No", "No", "Yes", "Yes", "No", "No", "No", "No"))

amie2.1 <- summary(amie2)[[4]]
limit <- max(abs(amie2.1$AMIE)) * c(-1, 1)

ggplot(amie2.1, aes(y= Level2, x=Level1)) +
  geom_raster(aes(fill=AMIE)) +
  theme_minimal() +
  scale_fill_gradientn(colours=c("coral", "white", "dodgerblue"),
                       breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 
                                0.06, 0.08), limit=limit) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 




