## Load data
load("data/analysis/iyengar_westwood_replication/iw_dat.rda")

## Estimate model 
model<-glm(partisanSelection~participantPID2*mostQualifiedPerson,data=iw_dat[iw_dat$scholarship=="partisan",],family = "binomial")

## Calculate predicted probabilities for the interaction
eff<-effect(model,term="participantPID2*mostQualifiedPerson",as.table=T)

## change class of effects object to a data frame
dataeff<-as.data.frame(eff)


## get the names of the values of the most qualified person and participant party id variables and change them to something shorter for plotting
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

## get the estimates from the effects object
b <- c(eff$fit)

## reset the names of the estimates to the labels produced above
names(b) <- nms$label

## identify the estimates that correspond with the three different "Most Qualified" options
w_eq <- grep("EQ", nms$label)
w_dmq <- grep("DMQ", nms$label)
w_rmq <- grep("RMQ", nms$label)

## Extract effects for each different "Most Qualified" option
eff_eq <- structure(list(coef=b[w_eq], vcov=vcov(eff)[w_eq, w_eq]), class="vtcustom")
eff_dmq <- structure(list(coef=b[w_dmq], vcov=vcov(eff)[w_dmq, w_dmq]), class="vtcustom")
eff_rmq <- structure(list(coef=b[w_rmq], vcov=vcov(eff)[w_rmq, w_rmq]), class="vtcustom")

## Find the inferential confidence levels for a two-tailed test at the 0.05 level for each
## different "Most Qualified" option.  
vt_eq <- viztest(eff_eq, test_level = .025, level_increment = .001, include_zero=FALSE) 
vt_eq

vt_dmq <- viztest(eff_dmq, test_level = .025, level_increment = .001, include_zero=FALSE) 
vt_dmq

vt_rmq <- viztest(eff_rmq, test_level = .025, level_increment = .001, include_zero=FALSE) 
vt_rmq

## Calculate effects at 84% level
eff_84 <- effect(model,term="participantPID2*mostQualifiedPerson",as.table=T, se=list(level=.84))

## Combine original and 84% intervals
dat_all <- bind_rows(as.data.frame(eff_84) %>% mutate(interval = "Inferential (84%)"), 
                     dataeff %>% mutate(interval = "Original (95%)")) %>% 
  mutate(participantPID2 = factor(participantPID2, 
                                  levels=c("Democrat", "Lean Democrat", "Independent", "Lean Republican", "Republican")), 
         interval = factor(interval, levels=c("Original (95%)", "Inferential (84%)")))

rownames(dat_all) <- NULL

## Plot Results
ggplot(dat_all, aes(x=fit, xmin=lower, xmax=upper, y=participantPID2)) + 
  geom_pointrange() + 
  facet_grid(interval ~ mostQualifiedPerson) + 
  theme_bw() + 
  labs(x = "Predicted Probability of Selecting Republican", y="") 
ggsave("results/FigureA9.png", height=6, width=9, units="in", dpi=300)  
