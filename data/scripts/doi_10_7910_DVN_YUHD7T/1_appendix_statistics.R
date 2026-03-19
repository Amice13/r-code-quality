library(network)
library(ergm)
library(dplyr)
library(reshape2)
library(kableExtra)

library(tidyverse)
library(scales)
library(patchwork)

# setup
start = 1920
end = 1936

# load data
load("data/data_ready.rda")

# select binary file with threshold at 100USD
transfers = transfers_adapted_lvl_150_binary_100

# number of included countries
ind = (exist[, 1928 - 1918] == 1) & (colony[, 1928 - 1918] == 0)
ind_col = (rowSums(colony, na.rm = TRUE) > 0)
ind_excl = !ind & !ind_col

(n <- sum(ind))
(n_col <- sum(ind_col))
(n_excl <- sum(ind_excl))

# 142 = 76 (part-colonies) + 55 included + 13 excluded - 2 (Egypt + Ireland included, but part time colony)
intersect(nodelist$name[ind], nodelist$name[ind_col])

# period 1920:1936
period = 2:18

# because
c(1919:1939)[2:18]

# summary statistics of exogenous covariates (time-pooled)
df = data.frame(
  "Name" = character(),
  "N" = integer(),
  "Mean" = numeric(),
  "SD" = numeric(),
  "Min" = numeric(),
  "Max" = numeric(),
  "Median" = numeric()
)


## Time - Varying

# Transfers
transfers_pooled = list()

for (t in period) {
  tmp = transfers[[t]][ind, ind]
  diag(tmp) = NA
  tmp = melt(tmp)
  transfers_pooled[[t]] = tmp
}

transfers_pooled = do.call(rbind, transfers_pooled)

df[1, "Name"] = "SALW Trade Tie"
df[1, "N"] = sum(!is.na(transfers_pooled$value))
df[1, "Mean"] = mean(transfers_pooled$value, na.rm = TRUE)
df[1, "SD"] = sd(transfers_pooled$value, na.rm = TRUE)
df[1, "Median"] = median(transfers_pooled$value, na.rm = TRUE)
df[1, "Min"] = min(transfers_pooled$value, na.rm = TRUE)
df[1, "Max"] = max(transfers_pooled$value, na.rm = TRUE)


# GDP per capita
tmp = log(maddison_gdppc[ind, period])
df[2, "Name"] = "GDP per capita (log)"
df[2, "N"] = sum(ind) * length(period)
df[2, "Mean"] = mean(tmp)
df[2, "SD"] = sd(tmp)
df[2, "Median"] = median(tmp)
df[2, "Min"] = min(tmp)
df[2, "Max"] = max(tmp)


# Polity Index
polity_abs_diff_pooled = list()

for (t in period) {
  tmp = polity_polity2[ind, t]
  tmp = abs(outer(tmp, tmp, FUN = "-"))

  diag(tmp) = NA

  polity_abs_diff_pooled[[t]] = melt(tmp)
}

polity_abs_diff_pooled = do.call(rbind, polity_abs_diff_pooled)

df[3, "Name"] = "Difference in Polity (abs)"
df[3, "N"] = sum(!is.na(polity_abs_diff_pooled$value))
df[3, "Mean"] = mean(polity_abs_diff_pooled$value, na.rm = TRUE)
df[3, "SD"] = sd(polity_abs_diff_pooled$value, na.rm = TRUE)
df[3, "Median"] = median(polity_abs_diff_pooled$value, na.rm = TRUE)
df[3, "Min"] = min(polity_abs_diff_pooled$value, na.rm = TRUE)
df[3, "Max"] = max(polity_abs_diff_pooled$value, na.rm = TRUE)


# CINC
cinc_diff_pooled = list()

for (t in period) {
  tmp = nmc_cinc[ind, t] * 100 # we scale with 100 to have percentage points
  tmp = outer(tmp, tmp, FUN = "-")

  diag(tmp) = NA

  cinc_diff_pooled[[t]] = melt(tmp)
}

cinc_diff_pooled = do.call(rbind, cinc_diff_pooled)

df[4, "Name"] = "Difference in CINC (Sender-Receiver)"
df[4, "N"] = sum(!is.na(cinc_diff_pooled$value))
df[4, "Mean"] = mean(cinc_diff_pooled$value, na.rm = TRUE)
df[4, "SD"] = sd(cinc_diff_pooled$value, na.rm = TRUE)
df[4, "Median"] = median(cinc_diff_pooled$value, na.rm = TRUE)
df[4, "Min"] = min(cinc_diff_pooled$value, na.rm = TRUE)
df[4, "Max"] = max(cinc_diff_pooled$value, na.rm = TRUE)


# Defense Alliance
atop_defense_pooled = list()

for (t in period) {
  tmp = atop_defense[[t]][ind, ind]
  diag(tmp) = NA
  tmp = melt(tmp)
  atop_defense_pooled[[t]] = tmp
}

atop_defense_pooled = do.call(rbind, atop_defense_pooled)

df[5, "Name"] = "Defense Alliance"
df[5, "N"] = sum(!is.na(atop_defense_pooled$value))
df[5, "Mean"] = mean(atop_defense_pooled$value, na.rm = TRUE)
df[5, "SD"] = sd(atop_defense_pooled$value, na.rm = TRUE)
df[5, "Min"] = min(atop_defense_pooled$value, na.rm = TRUE)
df[5, "Max"] = max(atop_defense_pooled$value, na.rm = TRUE)



# Common Trade Block
common_block_pooled = list()

for (t in period) {
  tmp = common_block[[t]][ind, ind]
  diag(tmp) = NA
  tmp = melt(tmp)
  common_block_pooled[[t]] = tmp
}

common_block_pooled = do.call(rbind, common_block_pooled)

df[6, "Name"] = "Common Trade Block"
df[6, "N"] = sum(!is.na(common_block_pooled$value))
df[6, "Mean"] = mean(common_block_pooled$value, na.rm = TRUE)
df[6, "SD"] = sd(common_block_pooled$value, na.rm = TRUE)
df[6, "Min"] = min(common_block_pooled$value, na.rm = TRUE)
df[6, "Max"] = max(common_block_pooled$value, na.rm = TRUE)



## Time Constant

# Common Language
diag(common_language) = NA
tmp = common_language[ind, ind]
df[7, "Name"] = "Common Language"
df[7, "Mean"] = mean(tmp, na.rm = TRUE)
df[7, "SD"] = sd(tmp, na.rm = TRUE)
df[7, "N"] = sum(!is.na(tmp))
df[7, "Min"] = min(tmp, na.rm = TRUE)
df[7, "Max"] = max(tmp, na.rm = TRUE)


# Distance
diag(capdist) = NA
tmp = log(capdist[ind, ind])
df[8, "Name"] = "Distance in km (log)"
df[8, "N"] = sum(!is.na(tmp))
df[8, "Mean"] = mean(tmp, na.rm = TRUE)
df[8, "SD"] = sd(tmp, na.rm = TRUE)
df[8, "Median"] = median(tmp, na.rm = TRUE)
df[8, "Min"] = min(tmp, na.rm = TRUE)
df[8, "Max"] = max(tmp, na.rm = TRUE)



## Output

sink("tables/latex_summary_statistics.txt")

kbl(df[, 1:6],
  booktabs = T, format = "latex",
  digits = 3, escape = F, linesep = "",
  caption = "Summary Statistics 1920--1936", label = "summary_statistics"
) %>%
  kable_styling(font_size = 11, full_width = FALSE)

sink()


sink("tables/list_of_included_countries.txt")
cat(paste(nodelist$name[ind], collapse = "\n \n"), sep = "\n")
sink()


sink("tables/list_of_excluded_countries.txt")
cat(paste(nodelist$name[ind_excl], collapse = "\n \n"), sep = "\n")
sink()


## Network Statistics Plot

# Network Density
net = list()

for (i in seq_along(1919:1939)) {
  net[[i]] = network(transfers[[i]][ind, ind], directed = T)
}

df = data.frame("Year" = 1919:1939, "Density" = sapply(net, network.density))

plot1 = ggplot(
  data = df,
  mapping = aes(x = Year, y = Density)
) +
  geom_line() +
  scale_y_continuous(labels = label_number(accuracy = 0.01), limits = c(0, 0.12), expand = c(0, 0)) +
  xlim(c(1920, 1936)) +
  labs(title = "Network Density 1920-1936") +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 10),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Triad Census
dgwesp_otp = sapply(c(2:18), function(i) summary(net[[i]] ~ desp(1:54, type = "OTP")))
dgwesp_isp = sapply(c(2:18), function(i) summary(net[[i]] ~ desp(1:54, type = "ISP")))
dgwesp_osp = sapply(c(2:18), function(i) summary(net[[i]] ~ desp(1:54, type = "OSP")))

df = data.frame(
  "Year" = 1920:1936,
  "OTP" = colSums(dgwesp_otp),
  "ISP" = colSums(dgwesp_isp),
  "OSP" = colSums(dgwesp_osp)
) %>%
  pivot_longer(c("OTP", "ISP", "OSP"), names_to = "Type", values_to = "Count")

plot2 = ggplot(data = df, mapping = aes(x = Year, y = Count)) +
  geom_line(mapping = aes(group = Type, linetype = Type)) +
  labs(title = "Types of Triads 1920-1936") +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 10),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), legend.position = "bottom"
  )



# Degree Distribution
ideg = sapply(c(2:18), function(i) summary(net[[i]] ~ idegree(d = 0:20)))
odeg = sapply(c(2:18), function(i) summary(net[[i]] ~ odegree(d = 0:54)))

df = ideg %>%
  as.data.frame() %>%
  setNames(c(1920:1936)) %>%
  mutate(idegree = 0:20) %>%
  pivot_longer(cols = paste(c(1920:1936)), names_to = "Year", values_to = "Count") %>%
  mutate(Period = case_when(
    Year %in% 1920:1924 == 1 ~ "1920-1924",
    Year %in% 1925:1930 == 1 ~ "1925-1930",
    Year %in% 1931:1936 == 1 ~ "1931-1936"
  )) %>%
  group_by(idegree, Period) %>%
  summarise(Count = mean(Count))

plot3 = ggplot(data = df, mapping = aes(x = idegree, Count)) +
  geom_line(mapping = aes(linetype = Period), show.legend = TRUE) +
  scale_x_continuous(limits = c(0, 20), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 12), expand = c(0, 0)) +
  labs(title = "Average Indegree Distribution") +
  xlab("Indegree") +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 10),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )



df = odeg %>%
  as.data.frame() %>%
  setNames(c(1920:1936)) %>%
  mutate(idegree = 0:54) %>%
  pivot_longer(cols = paste(c(1920:1936)), names_to = "Year", values_to = "Count") %>%
  mutate(Period = case_when(
    Year %in% 1920:1924 == 1 ~ "1920-1924",
    Year %in% 1925:1930 == 1 ~ "1925-1930",
    Year %in% 1931:1936 == 1 ~ "1931-1936"
  )) %>%
  group_by(idegree, Period) %>%
  summarise(Count = mean(Count))

plot4 = ggplot(data = df, mapping = aes(x = idegree, Count)) +
  geom_line(mapping = aes(linetype = Period), show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0), trans=scales::pseudo_log_trans(base = 10)) +
  labs(title = "Average Outdegree Distribution") +
  xlab("Outdegree (log-scale)") +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 10),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


# generate output
patch = wrap_plots(plot1 + plot2 + plot3 + plot4)

# save
ggsave(
  filename = "figures/fig_appendix_network_statistics.pdf"
  , plot = patch
  , height = 130
  , width = 190
  , unit = "mm"
)
