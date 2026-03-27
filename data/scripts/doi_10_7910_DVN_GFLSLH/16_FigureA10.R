load("data/raw/muraoka_rosas_replication/Left_Stan_July10.RData")
load("data/raw/muraoka_rosas_replication/Center_Stan_July10.RData")
load("data/raw/muraoka_rosas_replication/Right_Stan_July10.RData")
load("data/analysis/muraoka_rosas_replication/mr_data.rda")

left.eta <- extract(left.model)[["eta"]][,,2]
left.sd.mat <- c(sd(log(left.df$swiid_gini)), sd(left.df$enpp), sd(log(left.df$gdpcapita)))/sd(left.df$perception.error)
left.eta <- left.eta*left.sd.mat[1]
colnames(left.eta) <- paste0("L: ", c("Top", "Second-Top", "Second-Bottom", "Bottom"))

center.eta <- extract(center.model)[["eta"]][,,2]
center.sd.mat <- c(sd(log(center.df$swiid_gini)), sd(center.df$enpp), sd(log(center.df$gdpcapita)))/sd(center.df$perception.error)
center.eta <- center.eta* center.sd.mat[1]
colnames(center.eta) <- paste0("C: ", c("Top", "Second-Top", "Second-Bottom", "Bottom"))

right.eta <- extract(right.model)[["eta"]][,,2]
right.sd.mat <- c(sd(log(right.df$swiid_gini)), sd(right.df$enpp), sd(log(right.df$gdpcapita)))/sd(right.df$perception.error)
right.eta <- right.eta*right.sd.mat[1]
colnames(right.eta) <- paste0("R: ", c("Top", "Second-Top", "Second-Bottom", "Bottom"))

## Combine estimates across ideological directions
all.eta <- cbind(left.eta, center.eta, right.eta)

## create custom testing object
all_vt <- structure(.Data = list(est = all.eta), class="vtsim")

## find inferential credible masses for the HDIs
v_all <- viztest(all_vt, test=.05, range_levels=c(.6, .9), 
                 include_zero = TRUE, level_increment=.001, cifun="hdi")

## create original 95% and inferential HDIs 
hdi_90 <- t(apply(all.eta, 2, \(x)HDInterval::hdi(x, credMass = .9))) %>% 
  as_tibble(rownames="term") %>% 
  dplyr::rename(lwr_90 = lower, upr_90 = upper)

hdi_inf <- t(apply(all.eta, 2, \(x)HDInterval::hdi(x, credMass = .768))) %>% 
  as_tibble() %>% 
  dplyr::rename(lwr_inf = lower, upr_inf = upper)

## combine HDIs and make appropriate variables/labels for the data
plot_dat <- bind_cols(hdi_90, hdi_inf)
plot_dat <- plot_dat %>% 
  mutate(estimate = colMeans(all.eta)) %>% 
  separate_wider_delim(term, ": ", names = c("Direction", "Income")) %>% 
  mutate(Direction = factor(Direction, levels=c("L", "C", "R"), 
                            labels = c("Left", "Centre", "Right")), 
         Income = factor(Income, levels=c("Top", "Second-Top", "Second-Bottom", "Bottom")))

## Make plot
ggplot(plot_dat, 
       aes(x=estimate, y=Income)) + 
  geom_segment(aes(x=lwr_90, xend = upr_90, colour="Original (95%)"), linewidth=1.5) + 
  geom_segment(aes(x=lwr_inf, xend=upr_inf, colour="Inferential (76.8%)"), linewidth=3) + 
  geom_point(colour="white") + 
  geom_vline(xintercept=0, linetype=3) + 
  scale_colour_manual(values=c("black", "gray50")) + 
  facet_wrap(~Direction, ncol=1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position="top") + 
  labs(x="Posterior Mean and Inferential Credible\n Intervals of Inequality (Standardized)", 
       y="",
       colour="HDI: ")
ggsave("results/FigureA10.png", height=8, width=6, units="in", dpi=300)  
