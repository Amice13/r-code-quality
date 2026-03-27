
## load packages
require(tidyverse)

## load data
check_out <- read_rds("../results/application/03-check_all_summary.rds")
est_out   <- read_rds("../results/application/04-est_all_output.rds")



## ---------------------------------------
## parallel trends assumption
## ---------------------------------------

## select outcomes
var_pt <- check_out %>% filter(p_value > 0.1 & EqCI95_UB <= 0.2)

## re-lable parallel trends: outcome variable names
est_pt <- est_out %>% filter(estimator %in% c("Double-DID", "DID")) %>%
  filter(var %in% var_pt$var) %>%
  mutate(var = case_when(
    var == "animal_s"   ~ "Staff to Cure Animal",
    var == "broadcast"  ~ "Radio Broadcast",
    var == "irrigation" ~ "Irrigation Plants",
    var == "market"     ~ "Market or Inter-commune Market",
    var == "post"       ~ "Post Office",
    var == "pro3"       ~ "Socio-Dev't/ Infra. Project",
    var == "pro4"       ~ "Education and Cultural Program",
    var == "rm2c7a"     ~ "Prop. Households\n w/ Supported Credit",
    var == "rm2c7b"     ~ "Prop. Households\n w/ Supported Tuition",
    var == "rm2c7c"     ~ "Prop. Households\n w/ Supported Healthcare",
    var == "rm2c7e"     ~ "Prop. Households\n w/ Agricultural Extension",
    var == "roadv"      ~ "Paved Road",
    var == "useschool"  ~ "Upper Secondary School",
    var == "vmarket2"   ~ "Periodic Market",
    var == "vpost"      ~ "Village w/ Post Office"
  ))

## generate figure
fig_pt <- ggplot(est_pt, aes(x = estimator, y = estimate, color = estimator, shape = estimator)) +
  geom_hline(yintercept = 0, color = 'lightgray', linetype = "dotted") +
  geom_point() +
  geom_pointrange(aes(ymin = estimate - qnorm(0.95) * std.error,
                      ymax = estimate + qnorm(0.95) * std.error)) +
  facet_wrap(~var) +
  labs(y = "ATT estimates (90% CI)", x = "",
       title = "Estimates under Extended Parallel Trends Assumption") +
  scale_color_manual(values = c("gray50", "#1E88A8")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(size = 6)
  )

## save figure
ggsave(filename = "../results/figures/figureA4_malesky-all-ept-final.pdf", fig_pt, height = 6.5, width = 6)



## ---------------------------------------
## parallel trends-in-trends
## ---------------------------------------

## select outcomes
var_ptt <- check_out %>% filter(!(var %in% var_pt$var)) %>%
  filter(var %in% c("transport", "tapwater", "nonfarm", "plant_s",
                    "pro5", "rm2c7d", "rm2c7f", "vmarket1"))

## re-label outcome variable names
est_ptt <- est_out %>% filter(estimator %in% c("DID", "sDID")) %>%
  filter(var %in% var_ptt$var) %>%
  mutate(estimator = ifelse(estimator == "sDID", "Double-DID", "DID")) %>%
  mutate(var = case_when(
    var == "transport" ~ "Public Transport",
    var == "tapwater"  ~ "Tap Water",
    var == "nonfarm"   ~ "Nonfarm Business",
    var == "vmarket1"  ~ "Daily Market",
    var == "rm2c7d"    ~ "Prop. Households\n w/ Supported Crop",
    var == "plant_s"   ~ "Staff to Support Crops",
    var == "pro5"      ~ "Public Health Project",
    var == "rm2c7f"    ~ "Prop. Households\n w/ Business Tax Exemption"
  ))

## generate figure
fig_pt <- ggplot(est_ptt, aes(x = estimator, y = estimate, color = estimator, shape = estimator)) +
  geom_hline(yintercept = 0, color = 'lightgray', linetype = "dotted") +
  geom_point() +
  geom_pointrange(aes(ymin = estimate - qnorm(0.95) * std.error,
                      ymax = estimate + qnorm(0.95) * std.error)) +
  facet_wrap(~var, ncol = 4) +
  labs(y = "ATT estimates (90% CI)", x = "",
       title = "Estimates under Parallel Trends-in-Trends") +
  scale_color_manual(values = c("gray50", "#1E88A8")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(size = 6)
      )

## save figure
ggsave(filename = "../results/figures/figureA5_malesky-all-ptt-final.pdf", fig_pt, height = 4, width = 5.5)
