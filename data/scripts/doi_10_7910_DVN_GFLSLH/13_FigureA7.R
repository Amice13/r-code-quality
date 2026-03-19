#### 1. Read in Data from Muraoka and Rosas (Raw and Cleaned) ####
load("data/raw/muraoka_rosas_replication/Left_Stan_July10.RData")
load("data/raw/muraoka_rosas_replication/Center_Stan_July10.RData")
load("data/raw/muraoka_rosas_replication/Right_Stan_July10.RData")
load("data/analysis/muraoka_rosas_replication/mr_data.rda")

#### 2. Extracting sd and eta and formatting####
## Left
left.eta <- extract(left.model)[["eta"]][,,2]
left.sd.mat <- c(sd(log(left.df$swiid_gini)), sd(left.df$enpp), 
                 sd(log(left.df$gdpcapita)))/sd(left.df$perception.error)
left.eta <- left.eta*left.sd.mat[1]
colnames(left.eta) <- paste0("L: ", c("Top", "Second-Top", "Second-Bottom", "Bottom"))

## Center
center.eta <- extract(center.model)[["eta"]][,,2]
center.sd.mat <- c(sd(log(center.df$swiid_gini)), sd(center.df$enpp), 
                   sd(log(center.df$gdpcapita)))/sd(center.df$perception.error)
center.eta <- center.eta* center.sd.mat[1]
colnames(center.eta) <- paste0("C: ", c("Top", "Second-Top", "Second-Bottom", "Bottom"))

## Right
right.eta <- extract(right.model)[["eta"]][,,2]
right.sd.mat <- c(sd(log(right.df$swiid_gini)), sd(right.df$enpp), 
                  sd(log(right.df$gdpcapita)))/sd(right.df$perception.error)
right.eta <- right.eta*right.sd.mat[1]
colnames(right.eta) <- paste0("R: ", c("Top", "Second-Top", "Second-Bottom", "Bottom"))

## Merging the three models
all.eta <- cbind(left.eta, center.eta, right.eta)

#### 3. Calling viztest ####
all_vt <- structure(.Data = list(est = all.eta), class="vtsim")
v_all <- viztest(all_vt, test=.05, range_levels=c(.6, .9),level_increment=.001, include_zero = TRUE)
v_all

#### 4. Data for plot ####
ae_sum <- all.eta %>% 
  as.data.frame() %>% 
  mutate(obs = row_number()) %>% 
  pivot_longer(-obs, names_to = "param", values_to = "vals") %>% 
  group_by(param) %>% 
  reframe(mn = mean(vals), 
          p0 = mean(vals > 0), 
          sig0 = ifelse(p0 > .95 | p0 < .05, "Credible", "Not credible"), 
          sig0 = factor(sig0, levels=c("Not credible", "Credible")), 
          lwr = quantile(vals, .117), 
          upr = quantile(vals, .883), 
          lwr95 = quantile(vals, .05), 
          upr95 = quantile(vals, .95),
          param = first(param)) %>%  
  mutate(ideo = case_when(
    grepl("^L", param) ~ "Left", 
    grepl("^C", param) ~ "Centre", 
    grepl("^R", param) ~ "Right"),
    ideo = factor(ideo, levels=c("Left", "Centre", "Right")), 
    income_group = gsub("[LCR]: ", "", param), 
    income_group = factor(income_group, levels=c("Bottom", "Second-Bottom", "Second-Top", "Top")))

#### 5. Plot ####
ggplot(ae_sum, aes(x=-mn, y = income_group, colour=sig0)) + 
  geom_vline(xintercept=0, linetype=3) + 
  geom_segment(aes(x = -lwr95, xend=-upr95, yend=income_group, 
                   linewidth = "95%", colour="95%")) + 
  geom_segment(aes(x = -lwr, xend=-upr, yend=income_group, 
                   linewidth = "76.6%", colour="76.6%")) + 
  geom_point(color="white") + 
  facet_wrap(~ideo, ncol=1) + 
  theme_bw() + 
  theme(legend.position="top", 
        panel.grid = element_blank()) + 
  scale_colour_manual(values=c("black", "gray50")) + 
  scale_linewidth_manual(values=c(3, 1.5)) + 
  labs(x="Posterior Mean and Inferential Credible\n Intervals of Inequality (Standardized)", 
       y="",
       colour="Credibility", linewidth="Credibility"
  )
ggsave("results/FigureA7.png", height=8, width=6, units="in", dpi=300)