library(ggplot2)

df <- read.csv("PerGas_CO2_CH4_with_CO2eq_Mg_ha_mean95CI.csv", check.names = FALSE)

# Recode DOC from method
if ("method" %in% names(df)) {
  df$Gas <- ifelse(df$method == "discharge", "DOC", df$Gas)
}

# Rename All → Annual if needed
df$season[df$season == "All"] <- "Annual"
df$season <- factor(df$season, levels = c("Dry", "Rain", "Annual"))

# ---- Year by year totals ----
totals_year <- aggregate(
  cbind(CO2eq_AR6_Mg_ha_mean, CO2eq_AR6_Mg_ha_lo, CO2eq_AR6_Mg_ha_hi) ~
    Station + Year_label + season,
  data = df, sum, na.rm = TRUE
)

# Label: "mean (lo–hi)"
totals_year$label <- paste0(
  sprintf("%.1f", totals_year$CO2eq_AR6_Mg_ha_mean),
  " (",
  sprintf("%.1f", totals_year$CO2eq_AR6_Mg_ha_lo),
  "–",
  sprintf("%.1f", totals_year$CO2eq_AR6_Mg_ha_hi),
  ")"
)

# Position just above the top bar
totals_year$y_label <- -10
#totals_year$y_label <- totals_year$CO2eq_AR6_Mg_ha_mean +
#  0.05 * abs(totals_year$CO2eq_AR6_Mg_ha_mean)

p1 <- ggplot(df, aes(x = season, y = CO2eq_AR6_Mg_ha_mean, fill = Gas)) +
  geom_bar(stat = "identity") +
  geom_text(
    data = totals_year,
    aes(x = season, y = y_label, label = label),
    inherit.aes = FALSE, fontface = "bold", size = 3.7
  ) +
  facet_grid(Station ~ Year_label) +
  labs(y = "CO₂-eq (Mg ha⁻¹ yr⁻¹)", x = "Season",
       title = "Year by Year CO₂-eq contributions (incl. DOC) with net ±95% CI") +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic(base_size = 12) +
  theme(legend.title = element_blank(),
        plot.margin = ggplot2::margin(8,12,8,8,"pt")) +
  coord_cartesian(clip = "off")


# ---- Pooled across years ----
pooled_mean <- aggregate(CO2eq_AR6_Mg_ha_mean ~ Station + season + Gas, df, mean, na.rm = TRUE)
pooled_lo   <- aggregate(CO2eq_AR6_Mg_ha_lo   ~ Station + season + Gas, df, mean, na.rm = TRUE)
pooled_hi   <- aggregate(CO2eq_AR6_Mg_ha_hi   ~ Station + season + Gas, df, mean, na.rm = TRUE)
df_pooled <- merge(merge(pooled_mean, pooled_lo,
                         by=c("Station","season","Gas")),
                   pooled_hi, by=c("Station","season","Gas"))

totals_pool <- aggregate(
  cbind(CO2eq_AR6_Mg_ha_mean, CO2eq_AR6_Mg_ha_lo, CO2eq_AR6_Mg_ha_hi) ~
    Station + season,
  data = df_pooled, sum, na.rm = TRUE
)
totals_pool$label <- paste0(
  sprintf("%.1f", totals_pool$CO2eq_AR6_Mg_ha_mean),
  " (",
  sprintf("%.1f", totals_pool$CO2eq_AR6_Mg_ha_lo),
  "–",
  sprintf("%.1f", totals_pool$CO2eq_AR6_Mg_ha_hi),
  ")"
)
totals_pool$y_label <- 6
#totals_pool$y_label <- totals_pool$CO2eq_AR6_Mg_ha_mean +
#  0.05 * abs(totals_pool$CO2eq_AR6_Mg_ha_mean)

p2 <- ggplot(df_pooled, aes(x = season, y = CO2eq_AR6_Mg_ha_mean, fill = Gas)) +
  geom_bar(stat = "identity") +
  geom_text(
    data = totals_pool,
    aes(x = season, y = y_label, label = label),
    inherit.aes = FALSE, fontface = "bold", size = 3.7
  ) +
  facet_wrap(~Station, nrow = 1) +
  labs( x = "Season") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(name = expression(CO[2]*"-eq"~(Mg~ha^-1~yr^-1)),
  	lim=c(-5,7.5))+
  theme_classic(base_size = 12) +
  theme(legend.position=c(0.9,0.2),legend.title = element_blank(),
        plot.margin = ggplot2::margin(8,12,8,8,"pt")) +
  coord_cartesian(clip = "off")



df_pooled2<-subset(df_pooled,season=="Annual")
totals_pool2<-subset(totals_pool,season=="Annual")
p3 <- ggplot(df_pooled2, aes(x = Station, y = CO2eq_AR6_Mg_ha_mean, fill = Gas)) +
  geom_bar(stat = "identity") +
  geom_text(
    data = totals_pool2,
    aes(x = Station, y = y_label, label = label),
    inherit.aes = FALSE, fontface = "bold", size = 3.7) +
  labs( x = NULL) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(name = expression(CO[2]*"-eq"~(Mg~ha^-1~yr^-1)),
  	lim=c(-5,7.5))+
  theme_classic(base_size = 12) +
  theme(legend.position=c(0.9,0.2),legend.title = element_blank(),
        plot.margin = ggplot2::margin(8,12,8,8,"pt"),
        panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  ) +
  coord_cartesian(clip = "off")

p3+ geom_hline(yintercept = 0, linetype = "dashed", color = "black")

p1+ geom_hline(yintercept = 0, linetype = "dashed", color = "black")
p2+ geom_hline(yintercept = 0, linetype = "dashed", color = "black")


annotation_custom(g, -Inf, Inf, -Inf, Inf) +  

#using 2023 as a normal year



# ---- Load data ----
