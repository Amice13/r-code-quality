#### 1. Retreiving mod from 10_FigureA4.R ####
source("code/10_FigureA4.R")

#### 2. Calling viztest ####
## Calculated fitted values for model
eff <- effect("x", mod)

## make fitted values amenable to viztest evaluation
s <- structure(.Data = list(coef = eff$fit, vcov=vcov(eff)), 
               class="vtcustom")

## Calculate optimal levels
#### Also, use output of the following for reffered numbers in Appendix 4
v <- viztest(s, 
             test_level=.025, 
             level_increment=.001, 
             include_zero=TRUE)
v

#### 3. Data for plot ####
## Calculate effects at lowest, highest, middle and easiest levels
low_eff <- as.data.frame(effect("x", mod, se=list(level=.591)))
high_eff <- as.data.frame(effect("x", mod, se=list(level=.913)))
mid_eff <- as.data.frame(effect("x", mod, se=list(level=.752)))
easy_eff <- as.data.frame(effect("x", mod, se=list(level=.795)))

## collect all effect data in a single object
dat4 <- bind_rows(
  low_eff %>% mutate(choice = "Lowest"), 
  high_eff %>% mutate(choice = "Closest to 95%"), 
  mid_eff %>% mutate(choice = "Middle"), 
  easy_eff %>% mutate(choice = "Easiest")
) %>% 
  mutate(choice = factor(choice, 
    levels=c("Closest to 95%", "Lowest", "Middle", "Easiest")))

## build data to include colored polygons in figure
poly_dat1 <- tibble(
  x=c(0, 4, 4, 0, 0, 0, 4, 4, 0, 0, 0, 4, 4, 0, 0, 0, 4, 4, 0, 0), 
  y=c(low_eff$upper[c(2,2)], low_eff$lower[c(3,3)], low_eff$upper[2], 
      high_eff$upper[c(2,2)], high_eff$lower[c(3,3)], high_eff$upper[2], 
      mid_eff$upper[c(2,2)], mid_eff$lower[c(3,3)], mid_eff$upper[2], 
      easy_eff$upper[c(2,2)], easy_eff$lower[c(3,3)], easy_eff$upper[2]), 
  choice =rep(c("Lowest", "Closest to 95%", "Middle", "Easiest"), each=5), 
  comparison = "B vs C"
)  

poly_dat2 <- tibble(
  x=c(0, 4, 4, 0, 0, 0, 4, 4, 0, 0, 0, 4, 4, 0, 0, 0, 4, 4, 0, 0), 
  y=c(low_eff$upper[c(1,1)], low_eff$lower[c(2,2)], low_eff$upper[1], 
      high_eff$upper[c(1,1)], high_eff$lower[c(2,2)], high_eff$upper[1], 
      mid_eff$upper[c(1,1)], mid_eff$lower[c(2,2)], mid_eff$upper[1], 
      easy_eff$upper[c(1,1)], easy_eff$lower[c(2,2)], easy_eff$upper[1]), 
  choice =rep(c("Lowest", "Closest to 95%", "Middle", "Easiest"), each=5), 
  comparison = "A vs B"
)  
poly_dat <- bind_rows(poly_dat1, poly_dat2) %>% 
  mutate(choice = factor(choice, levels=c("Closest to 95%", "Lowest", "Middle", "Easiest")))

#### 4. Plot ####
## set color palette
cols <- RColorBrewer::brewer.pal(3, "Set2")[2:3]

## Make plot
ggplot() + 
  geom_pointrange(data=dat4, aes(x=x, y=fit, ymin=lower, ymax=upper)) + 
  geom_polygon(data=poly_dat, aes(x=x, y=y, fill=comparison, colour=comparison), inherit.aes = FALSE, alpha=.25) + 
  geom_hline(yintercept = 0, linetype=3) + 
  facet_wrap(~choice, ncol=2, as.table = TRUE) + 
  coord_cartesian(clip="on") + 
  scale_fill_manual(values=cols) + 
  scale_colour_manual(values=cols) + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        legend.position="top") + 
  labs(x="", y="Estimates and Inferential Confidence Intervals", 
       colour="Comparison", fill="Comparison")
ggsave("results/FigureA5.png", height=8, width=8, units="in", dpi=300)  