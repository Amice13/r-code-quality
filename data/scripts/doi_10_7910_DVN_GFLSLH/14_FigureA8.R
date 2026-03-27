## load data
load("data/analysis/gibson_replication/gibson_dat.rda")

## make survy design object using weights
des <- svydesign(ids=~1, weight=~WEIGHT, data=gibson_dat)

## estimate model which really just calculates the survey weighted mean by wave
m <- svyglm(agree1 ~ WAVE - 1, design=des)

## find the inferential confidence levels for a two-tailed test at the 0.05 level
v <- viztest(m, test_level=.025, include_intercept = FALSE, include_zero = TRUE, level_increment = .001)

## print results
v

## Calculate relevant confidence intervals
ci95 <- confint(m)
ci84 <- confint(m, level = .84)

## Combine data and create labels
plot_dat <- tibble(wave = factor(levels(gibson_dat$WAVE)[1:4], 
                                 levels = levels(gibson_dat$WAVE)[1:4]))%>% 
  bind_cols(as.data.frame(ci95) %>% setNames(c("lwr_95", "upr_95"))) %>% 
  bind_cols(as.data.frame(ci84) %>% setNames(c("lwr_84", "upr_84"))) %>% 
  mutate(estimate = coef(m))

## Make plot
ggplot(plot_dat, aes(x=wave, y=estimate)) + 
  geom_segment(aes(y = lwr_95, yend=upr_95, colour="Original (95%)"), 
               linewidth=1.1) + 
  geom_segment(aes(y = lwr_84, yend=upr_84, colour="Inferential (84%)"), 
               linewidth=3) + 
  geom_point(colour="white") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  scale_colour_manual(values=c("black", "gray50"))  + 
  labs(x="Survey Wave", y="Proportion Agreeing", colour="Confidence Level: ")
ggsave("results/FigureA8.png", height=6, width=6, units="in", dpi=300)  
