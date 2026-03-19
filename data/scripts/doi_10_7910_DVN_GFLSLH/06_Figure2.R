#### 1. Read in Clean Iyengar and Westwood's data ####
load("data/analysis/iyengar_westwood_replication/iw_dat.rda")

#### 2. Model ####
model<-glm(partisanSelection~participantPID2*mostQualifiedPerson,
           data=iw_dat[iw_dat$scholarship=="partisan",],
           family = "binomial")

#### 3. Data for plot ####
#### ~~3.1 Extract effects ####
## Calculate effects of the interaction
eff<-effect(model,term="participantPID2*mostQualifiedPerson",as.table=T)

## Turn effects into data frame
dataeff<-as.data.frame(eff)

## Get x-values and turn them into shorter names
nms <- eff$x
nms <- nms %>% 
  mutate(mostQualifiedPerson = case_when(
    mostQualifiedPerson == "Equally Qualified" ~ "EQ",
    mostQualifiedPerson == "Republican More Qualified" ~ "RMQ",
    mostQualifiedPerson == "Democrat More Qualified" ~ "DMQ"), 
    participantPID2 = case_when(
      participantPID2 == "Independent" ~ "I", 
      participantPID2 == "Democrat" ~ "D", 
      participantPID2 == "Lean Democrat" ~ "LD", 
      participantPID2 == "Republican" ~ "R", 
      participantPID2 == "Lean Republican" ~ "LR")) %>%
  mutate(label = paste(participantPID2, mostQualifiedPerson, sep=":"))

## extract predicted values as b and label
b <- c(eff$fit)
names(b) <- nms$label

## identify estimates belonging to the three treatment conditions
w_eq <- grep("EQ", nms$label)
w_dmq <- grep("DMQ", nms$label)
w_rmq <- grep("RMQ", nms$label)

#### ~~3.2 Make data viztest friendly ####
## create data amenable for use in the viztest function. 
eff_eq <- structure(list(coef=b[w_eq], vcov=vcov(eff)[w_eq, w_eq]), class="vtcustom")
eff_dmq <- structure(list(coef=b[w_dmq], vcov=vcov(eff)[w_dmq, w_dmq]), class="vtcustom")
eff_rmq <- structure(list(coef=b[w_rmq], vcov=vcov(eff)[w_rmq, w_rmq]), class="vtcustom")

#### ~~3.3 Estimate optimal CI with viztest ####
vt_eq <- viztest(eff_eq, test_level = .025, level_increment = .001, include_zero=FALSE) # .856
vt_dmq <- viztest(eff_dmq, test_level = .025, level_increment = .001, include_zero=FALSE) # .824
vt_rmq <- viztest(eff_rmq, test_level = .025, level_increment = .001, include_zero=FALSE) # 0.779

## print the values
vt_eq
vt_dmq
vt_rmq


#### ~~3.4 Calculate 84% CI ####
opt_mult <- qnorm(1-(1-.84)/2)
dataeff <- dataeff %>% 
  dplyr::rename(lwr_95 = lower, upr_95 = upper) %>% 
  mutate(lwr_opt = plogis(qlogis(fit) - opt_mult*sqrt(diag(vcov(eff)))), 
         upr_opt = plogis(qlogis(fit) + opt_mult*sqrt(diag(vcov(eff)))))

#### ~~3.5 Final structure for plot ####
## Pivot the data to long for plotting
dataeff <- dataeff %>% 
  pivot_longer(c(contains("lwr"), contains("upr")), 
               names_pattern = "(.*)_(.*)", 
               names_to = c(".value", "level"))

## Define labels for variables. 
dataeff <- dataeff %>% 
  mutate(participantPID2 = factor(participantPID2, 
            levels=c("Democrat", "Lean Democrat", "Independent", 
                     "Lean Republican", "Republican")),
         lvl_lab = factor(level, levels=c("95", "opt"), labels=c("95%", "Inferential")))

#### ~~3.6 Data for vertical guides ####
## The data here are to draw the lines identifying the overlaps in 95% confidence intervals
## between stimuli that are significantly different from each other. 
segdat_s <- dataeff %>% 
  filter(level == "95") %>% 
  filter((mostQualifiedPerson == "Equally Qualified" & 
            (participantPID2 %in% c("Lean Democrat", "Independent"))) | 
           (mostQualifiedPerson == "Republican More Qualified" & 
              (participantPID2 %in% c("Democrat", "Independent"))) | 
           (mostQualifiedPerson == "Democrat More Qualified" & 
              (participantPID2 %in% c("Democrat", "Lean Democrat", "Independent")))) %>% 
  mutate(yend = c("Lean Republican", "Independent", "Republican", "Lean Republican", 
                  "Lean Republican", "Independent", "Lean Republican"), 
         yend = factor(yend, levels=levels(iw_dat$participantPID)))

#### 4. Make plot ####
ggplot(dataeff, aes(x=fit, xmin = lwr, xmax = upr, y=participantPID2)) + 
  geom_pointrange() + 
  facet_grid(lvl_lab ~ mostQualifiedPerson) + 
  theme_bw() + 
  labs(x = "Predicted Probability of Selecting Republican", y="") + 
  geom_segment(data=segdat_s, aes(x=upr, xend=upr, y=participantPID2, yend=yend), 
               inherit.aes = FALSE, col="gray50", linetype=3, linewidth=1.025) + 
  geom_point(data=segdat_s, aes(x=upr, y=yend), shape=4, color="red", inherit.aes = FALSE, size=3)
ggsave("results/Figure2.pdf", height=6, width=9, units="in", dpi=300)

