#### 1. Data for plot ####
## Set random number generator seed
set.seed(123)

## Generating variables
hyp <- tibble(
  y =c(
    rnorm(100, 0, 4), 
    rnorm(100, 0, 8)+2.2, 
    rnorm(100, 0, 6)+2.65
  ),
  x = as.factor(rep(c("A", "B", "C"), each=100))
)

## Run model
mod <- lm(y ~ x, data=hyp)

## Formatting
effdat <- as.data.frame(effect("x", mod))

#### 2. Plot ####
ggplot(effdat, aes(x=x, y=fit, ymin=lower, ymax=upper)) + 
  geom_pointrange() + 
  theme_bw() + 
  labs(y="Estimate (95% CI)", x="")
ggsave("results/FigureA4.png", width=6, height=6, units="in", dpi=300)