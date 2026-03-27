#############################
# COMPARISON AMONG MAPS IN HEG DATA
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#############################

# LOAD GLOBALS AND DATA ##
if(!exists("LOADED_GLOBALS")){
  source("scripts/analysis/analysis_globals.R")
}

# Data
data <- readRDS(file.path("data/analysis_data", "compare_heg_df.rds"))

# Drop comparisons to self
data <- data[data$x != data$y,]
data <- data[data$x.terr.tot > 0 & data$y.terr.tot > 0, ]

# Time difference
data$time.diff <- abs(as.numeric(data$x.year) - 
                           as.numeric(data$y.year))
data$time.diff.dec <- round(data$time.diff/10)

# Weights
data <- data %>% 
  group_by(time.diff.dec) %>% 
  mutate(pop.weight.dec = x.pop.tot/sum(x.pop.tot),
         terr.weight.dec = x.terr.tot/sum(x.terr.tot)) %>% 
  ungroup()

# GREG subset
data.grg <- data[data$x == "grg_guy_1" | data$y == "grg_guy_1", ] %>% 
  group_by(time.diff.dec) %>% 
  mutate(pop.weight.dec = x.pop.tot/sum(x.pop.tot),
         terr.weight.dec = x.terr.tot/sum(x.terr.tot)) %>% 
  ungroup()

# Combined Plot
plot.df <- rbind(cbind(data[!is.na(data$x.in.y.pop),],
                       value = data$x.in.y.pop[!is.na(data$x.in.y.pop)] *100,
                       type = "Population weights",
                       data = "All data"),
                 cbind(data[!is.na(data$x.in.y.terr),],
                       value = data$x.in.y.terr[!is.na(data$x.in.y.terr)] *100,
                       type = "Area weights",
                       data = "All data"),
                 cbind(data.grg[!is.na(data.grg$x.in.y.pop),],
                       value = data.grg$x.in.y.pop[!is.na(data.grg$x.in.y.pop)] *100,
                       type = "Population weights",
                       data = "ANM Comp."),
                 cbind(data.grg[!is.na(data.grg$x.in.y.terr),],
                       value = data.grg$x.in.y.terr[!is.na(data.grg$x.in.y.terr)] *100,
                       type = "Area weights",
                       data = "ANM Comp."))
plot.df <- plot.df[order(plot.df$time.diff.dec, decreasing = T),]
plot.df$grps <- factor(plot.df$time.diff.dec, levels = rev(sort(unique(plot.df$time.diff.dec))),
                       ordered = T)
plot.df$value.w <- ifelse(plot.df$type == "Area weights",
                          plot.df$value * plot.df$terr.weight.dec,
                          plot.df$value * plot.df$pop.weight.dec)


# Plot
g <- ggplot(plot.df, aes(x = value, 
                         col = time.diff.dec,
                         group = grps)) + 
  scale_color_viridis() +
  geom_density(data = plot.df[plot.df$type == "Population weights",], 
               aes(weight=pop.weight.dec)) +
  geom_density(data = plot.df[plot.df$type == "Area weights",], 
               aes(weight=terr.weight.dec)) +
  theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  facet_grid(data ~ type) +
  labs(col = "Time\ndifference\n(decades)",
       x = "% of group in map X in group in map Y",
       y = "Weighted density") +
  theme(panel.spacing = unit(1, "lines")) +
  NULL
  
# Save
png(file.path(fig.path, "comp_pop_dec.png"), width = 7, height = 3.5, res = 300,
    units = "in")
print(g)
dev.off()

## Means 
weight.df <- plot.df %>% 
  group_by(time.diff.dec, data) %>% 
  summarise(pop.mean = stats::weighted.mean(x.in.y.pop, pop.weight.dec),
            terr.mean = stats::weighted.mean(x.in.y.terr, terr.weight.dec)) 
print(weight.df, n = 1000)


