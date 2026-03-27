#### 1. Read in Clean Iyengar and Westwood's data ####
load("data/analysis/iyengar_westwood_replication/iw_dat.rda")

## Factorize participantPID2
s1 <- iw_dat %>% 
  mutate(participantPID2 = factor(participantPID2, 
                                  levels=c("Democrat", "Lean Democrat", "Independent", 
                                           "Lean Republican", "Republican")))

#### 2. Running the models ####
dmq_mod <- glm(partisanSelection~participantPID2,
               data=subset(s1, mostQualifiedPerson == "Democrat More Qualified"),family = "binomial")

eq_mod <- glm(partisanSelection~participantPID2,
              data=subset(s1, mostQualifiedPerson == "Equally Qualified"),family = "binomial")

rmq_mod <- glm(partisanSelection~participantPID2,
               data=subset(s1, mostQualifiedPerson == "Republican More Qualified"),family = "binomial")

#### 3. Values for compact letter display ####
## General Linear Hypotheses
g_dmq <- summary(multcomp::glht(dmq_mod, linfct = mcp(participantPID2 = "Tukey")), test=adjusted("none"))
g_eq <- summary(multcomp::glht(eq_mod, linfct = mcp(participantPID2 = "Tukey")), test=adjusted("none"))
g_rmq <- summary(multcomp::glht(rmq_mod, linfct = mcp(participantPID2 = "Tukey")), test=adjusted("none"))

## abc values for each level of participantPID2
cld_dmq <- multcomp::cld(g_dmq)
cld_eq <- multcomp::cld(g_eq)
cld_rmq <- multcomp::cld(g_rmq)

#### 4. Predicted Probabilities for each model ####
eff_dmq <- ggpredict(dmq_mod, terms = "participantPID2")
eff_eq <- ggpredict(eq_mod, terms = "participantPID2")
eff_rmq <- ggpredict(rmq_mod, terms = "participantPID2")

#### 5. Plot ####
## Democrat Most Qualified facet
l1 <- letter_plot(eff_dmq, cld_dmq$mcletters$LetterMatrix) + 
  facet_wrap(~"Democrat Most Qualified") + 
  xlab("Predict Probability") + 
  theme_bw() + 
  theme(panel.grid=element_blank())

## Equally Qualified facet
l2 <- letter_plot(eff_eq, cld_eq$mcletters$LetterMatrix) + 
  facet_wrap(~"Equally Qualified") + 
  xlab("Predict Probability") + 
  theme_bw() + 
  theme(panel.grid=element_blank())

## Republican Most Qualified facet
l3 <- letter_plot(eff_rmq, cld_rmq$mcletters$LetterMatrix) + 
  facet_wrap(~"Republican Most Qualified") + 
  xlab("Predict Probability") + 
  theme_bw() + 
  theme(panel.grid=element_blank())

## saving
png("results/FigureA2.png", height=5, width=12.5, units="in", res = 300)
grid.arrange(l1, l2, l3, nrow=1)
dev.off()