# setup -------------------------------------
library(tidyverse)
library(srvyr)
library(survey)
library(broom)
library(cowplot)
data <- read_csv("https://www.dropbox.com/s/5kfbsc7e5mif4xc/NS21_data_wtd.csv?dl=1")


# risk perceptions -------------------------------------
disinfo_risk_means <-
  data %>%
  select(weightfactor,
         `Party` = party,
         `US national security` = disinfo_natsec,
         `Public health in the US` = disinfo_pubhealth,
         `The stability of the US government` = disinfo_govstab,
         `The economic prosperity of the US` = disinfo_econ,
         `Stable relationships between\nthe US and other countries` = disinfo_intrel) %>%
  pivot_longer(`US national security`:`Stable relationships between\nthe US and other countries`) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(Party, name) %>%
  summarize(mean = survey_mean(value, vartype = "ci", na.rm = TRUE)) %>%
  filter(Party %in% 1:2) %>%
  mutate(Party = factor(Party, levels = 1:2, labels = c("Democrats", "Republicans")))
disinfo_risk_means <- disinfo_risk_means %>%
  pivot_wider(id_cols = name, names_from = Party, values_from = mean) %>%
  mutate(mean_difference = abs(Democrats - Republicans)) %>%
  left_join(., disinfo_risk_means, by = "name")

p1 <- ggplot(disinfo_risk_means, aes(y = fct_reorder(name, -mean_difference), x = mean, color = Party)) +
  geom_vline(xintercept = 5, color = "grey") +
  geom_line(aes(group = name), color = "black") +
  geom_point() +
  scale_x_continuous(
    breaks = 0:10,
    labels = c("No\nRisk", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extreme\nRisk"),
    limits = c(0, 10),
    expand = c(0, 0)) +
  labs(x = "Risk (Mean)", y = "") +
  scale_color_manual(breaks = c("Democrats", "Republicans"), values = c("#0015BC", "#FF0000")) +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.2, 0.9),
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 0.1, t = 0.1, r = 0.5, b = 0.1, "cm"))

disinfo_risk <- lm(cbind(disinfo_pubhealth, disinfo_govstab, disinfo_natsec, disinfo_econ, disinfo_intrel) ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2))
tidy(disinfo_risk) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

p2 <- tidy(disinfo_risk) %>% 
  filter(term == "party") %>% 
  mutate(response = factor(response, levels = c("disinfo_intrel", "disinfo_natsec", "disinfo_econ", "disinfo_govstab", "disinfo_pubhealth"))) %>%
  ggplot(., aes(x = estimate, y = response)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error)), height = 0.1) +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4), expand = c(0, 0)) +
  labs(x = "Multiple Regression Coefficient\nEstimate for Party (Republican = 1)") +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.9, 0.9),
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(l = 0.1, t = 0.1, r = 0.1, b = 0.1, "cm"))

p <- plot_grid(p1, p2, align = "h", rel_widths = c(2, 1))
ggsave("/Users/josephripberger/Dropbox (Univ. of Oklahoma)/disinfo/figures/disinfo_risk_comb.png", p, height = 500, width = 1800, bg = "transparent", units = "px")

# harm -------------------------------------
disinfo_harm_means <-
  data %>%
  select(weightfactor,
         `Type` = disinfo_rand,
         `Party` = party,
         `Right-wing political actors` = disinfo_harm_cons,
         `Left-wing political actors` = disinfo_harm_lib,
         `Officials of the US Government` = disinfo_harm_gov,
         `US citizens generally` = disinfo_harm_indiv) %>%
  pivot_longer(`Right-wing political actors`:`US citizens generally`) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(Type, Party, name) %>%
  summarize(mean = survey_mean(value, vartype = "ci", na.rm = TRUE)) %>%
  filter(Party %in% 1:2) %>%
  mutate(Party = factor(Party, levels = 1:2, labels = c("Democrats", "Republicans")))
disinfo_harm_means <- disinfo_harm_means %>%
  pivot_wider(id_cols = c(Type, name), names_from = Party, values_from = mean) %>%
  mutate(mean_difference = abs(Democrats - Republicans)) %>%
  left_join(., disinfo_harm_means, by = c("Type", "name")) %>%
  mutate(Type = factor(Type, levels = c("politics in the US", "international threats from adversaries like Russia, China, and North Korea", "COVID-19")))
disinfo_harm_means %>% group_by(Type) %>% summarise(mean = mean(mean_difference))

p1 <- ggplot(disinfo_harm_means, aes(y = fct_reorder(name, -mean_difference), x = mean, color = Party)) +
  geom_vline(xintercept = 5, color = "grey") +
  geom_line(aes(group = name), color = "black") +
  geom_point() +
  scale_x_continuous(
    breaks = 0:10,
    labels = c("No\nharm", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extreme\nharm"),
    limits = c(0, 10),
    expand = c(0, 0)) +
  labs(x = "Harm (Mean)", y = "") +
  scale_color_manual(breaks = c("Democrats", "Republicans"), values = c("#0015BC", "#FF0000")) +
  facet_wrap(~Type, ncol = 1, scales = "free_y") +
  theme_classic(base_size = 7) +
  theme(legend.position = c(0.2, 0.95),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.2, 'cm'),
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 4),
        plot.margin = margin(l = 0.1, t = 0.1, r = 0.5, b = 0.1, "cm"))

disinfo_harm_pol <- lm(cbind(disinfo_harm_gov, disinfo_harm_lib, disinfo_harm_indiv, disinfo_harm_cons) ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US"))
disinfo_harm_int <- lm(cbind(disinfo_harm_gov, disinfo_harm_lib, disinfo_harm_indiv, disinfo_harm_cons) ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))
disinfo_harm_cov <- lm(cbind(disinfo_harm_gov, disinfo_harm_lib, disinfo_harm_indiv, disinfo_harm_cons) ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))

tidy(disinfo_harm_pol) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_harm_int) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_harm_cov) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

p2 <- bind_rows(
  tidy(disinfo_harm_pol) %>% mutate(name = "disinfo_harm_pol"), 
  tidy(disinfo_harm_int) %>% mutate(name = "disinfo_harm_int"), 
  tidy(disinfo_harm_cov) %>% mutate(name = "disinfo_harm_cov")) %>% 
  mutate(name = factor(name, 
                       levels = c("disinfo_harm_pol", "disinfo_harm_int", "disinfo_harm_cov"), 
                       labels = c("politics in the US", "international threats from adversaries like Russia, China, and North Korea", "COVID-19"))) %>% 
  mutate(response = factor(response, levels = rev(c("disinfo_harm_gov", "disinfo_harm_lib", "disinfo_harm_indiv", "disinfo_harm_cons")))) %>% 
  filter(term == "party") %>% 
  ggplot(., aes(x = estimate, y = response)) +
  geom_vline(xintercept = 0, linetype = 1, color = "grey") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error)), height = 0.2) +
  facet_wrap(~name, ncol = 1) +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4), expand = c(0, 0)) +
  labs(x = "Multiple Regression Coefficient\nEstimate for Party (Republican = 1)") +
  theme_classic(base_size = 7) +
  theme(legend.position = c(0.9, 0.9),
        legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(size = 4),
        plot.margin = margin(l = 0.1, t = 0.1, r = 0.1, b = 0.1, "cm"))

p <- plot_grid(p1, p2, align = "h", rel_widths = c(2, 1))
ggsave("/Users/josephripberger/Dropbox (Univ. of Oklahoma)/disinfo/figures/disinfo_harm_comb.png", p, height = 900, width = 1800, bg = "transparent", units = "px")

# blame -------------------------------------
disinfo_resp_means <-
  data %>%
  select(weightfactor,
         `Party` = party,
         `Type` = disinfo_rand,
         `Foreign governments` = disinfo_resp_foreign,
         `Republican Party actors` = disinfo_resp_gop,
         `Democratic Party actors` = disinfo_resp_dem,
         `Right-wing political actors` = disinfo_resp_cons,
         `Left-wing political actors` = disinfo_resp_lib,
         `Officials of the US Government` = disinfo_resp_gov,
         `US citizens generally` = disinfo_resp_indiv) %>%
  pivot_longer(`Foreign governments`:`US citizens generally`) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(Type, Party, name) %>%
  summarize(mean = survey_mean(value, vartype = "ci", na.rm = TRUE)) %>%
  filter(Party %in% 1:2) %>%
  mutate(Party = factor(Party, levels = 1:2, labels = c("Democrats", "Republicans")))
disinfo_resp_means <- disinfo_resp_means %>%
  pivot_wider(id_cols = c(Type, name), names_from = Party, values_from = mean) %>%
  mutate(mean_difference = abs(Democrats - Republicans)) %>%
  left_join(., disinfo_resp_means, by = c("Type", "name")) %>%
  mutate(Type = factor(Type,
                       levels = c("politics in the US", "international threats from adversaries like Russia, China, and North Korea", "COVID-19"), 
                       labels = c("politics in the US", "international threats from adversaries like Russia, China, and North Korea", "COVID-19"))) 
disinfo_resp_means %>% group_by(name) %>% summarise(mean = mean(mean_difference)) %>% arrange(-mean)

p1 <- ggplot(disinfo_resp_means, aes(y = fct_reorder(name, -mean_difference), x = mean, color = Party)) +
  geom_vline(xintercept = 5, color = "grey") +
  geom_line(aes(group = name), color = "black") +
  geom_point() +
  scale_x_continuous(
    breaks = 0:10,
    labels = c("No\nharm", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Extreme\nharm"),
    limits = c(0, 10),
    expand = c(0, 0)) +
  labs(x = "Harm (Mean)", y = "") +
  scale_color_manual(breaks = c("Democrats", "Republicans"), values = c("#0015BC", "#FF0000")) +
  facet_wrap(~Type, ncol = 1, scales = "free_y") +
  theme_classic(base_size = 7) +
  theme(legend.position = c(0.2, 0.95),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.2, 'cm'),
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 4),
        plot.margin = margin(l = 0.1, t = 0.1, r = 0.5, b = 0.1, "cm"))

disinfo_resp_pol <- lm(cbind(disinfo_resp_foreign, disinfo_resp_gov, disinfo_resp_indiv, disinfo_resp_lib, disinfo_resp_cons, disinfo_resp_gop, disinfo_resp_dem) ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US"))
disinfo_resp_int <- lm(cbind(disinfo_resp_foreign, disinfo_resp_gov, disinfo_resp_indiv, disinfo_resp_lib, disinfo_resp_cons, disinfo_resp_gop, disinfo_resp_dem) ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))
disinfo_resp_cov <- lm(cbind(disinfo_resp_foreign, disinfo_resp_gov, disinfo_resp_indiv, disinfo_resp_lib, disinfo_resp_cons, disinfo_resp_gop, disinfo_resp_dem) ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))

tidy(disinfo_resp_pol) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_resp_int) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_resp_cov) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

p2 <- bind_rows(
  tidy(disinfo_resp_pol) %>% mutate(name = "disinfo_resp_pol"), 
  tidy(disinfo_resp_int) %>% mutate(name = "disinfo_resp_int"), 
  tidy(disinfo_resp_cov) %>% mutate(name = "disinfo_resp_cov")) %>% 
  mutate(name = factor(name, 
                       levels = c("disinfo_resp_pol", "disinfo_resp_int", "disinfo_resp_cov"), 
                       labels = c("politics in the US", "international threats from adversaries like Russia, China, and North Korea", "COVID-19"))) %>% 
  mutate(response = factor(response, levels = rev(c("disinfo_resp_foreign", "disinfo_resp_gov", "disinfo_resp_indiv", "disinfo_resp_lib", "disinfo_resp_cons", "disinfo_resp_gop", "disinfo_resp_dem")))) %>% 
  filter(term == "party") %>% 
  ggplot(., aes(x = estimate, y = response)) +
  geom_vline(xintercept = 0, linetype = 1, color = "grey") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error)), height = 0.25) +
  facet_wrap(~name, ncol = 1) +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4), expand = c(0, 0)) +
  labs(x = "Multiple Regression Coefficient\nEstimate for Party (Republican = 1)") +
  theme_classic(base_size = 7) +
  theme(legend.position = c(0.9, 0.9),
        legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(size = 4),
        plot.margin = margin(l = 0.1, t = 0.1, r = 0.1, b = 0.1, "cm"))

p <- plot_grid(p1, p2, align = "h", rel_widths = c(2, 1))
ggsave("/Users/josephripberger/Dropbox (Univ. of Oklahoma)/disinfo/figures/disinfo_resp_comb.png", p, height = 900, width = 1800, bg = "transparent", units = "px")





















# appendix -------------------------------------

# means -------------------------------------
data %>%
  select(weightfactor,
         `Party` = party,
         `National security` = disinfo_natsec,
         `Public health` = disinfo_pubhealth,
         `Stability of the US government` = disinfo_govstab,
         `Economic prosperity` = disinfo_econ,
         `Stable relationships between\nthe US and other countries` = disinfo_intrel) %>%
  pivot_longer(`National security`:`Stable relationships between\nthe US and other countries`) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(Party, name) %>%
  summarize(mean = survey_mean(value, vartype = "ci", na.rm = TRUE)) %>%
  filter(Party %in% 1:2) %>%
  mutate(Party = factor(Party, levels = 1:2, labels = c("Democrats", "Republicans"))) %>% 
  pivot_wider(id_cols = name, names_from = Party, values_from = mean)

data %>%
  select(weightfactor,
         `Party` = party,
         `Type` = disinfo_rand,
         `Foreign governments` = disinfo_resp_foreign,
         `Republican Party actors` = disinfo_resp_gop,
         `Democratic Party actors` = disinfo_resp_dem,
         `Right-wing political actors (like QAnon)` = disinfo_resp_cons,
         `Left-wing political actors (like Antifa)` = disinfo_resp_lib,
         `Officials of the US Government` = disinfo_resp_gov,
         `US citizens generally` = disinfo_resp_indiv) %>%
  pivot_longer(`Foreign governments`:`US citizens generally`) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(Type, Party, name) %>%
  summarize(mean = survey_mean(value, vartype = "ci", na.rm = TRUE)) %>%
  filter(Party %in% 1:2) %>%
  mutate(Party = factor(Party, levels = 1:2, labels = c("Democrats", "Republicans"))) %>% 
  pivot_wider(id_cols = c(Type, name), names_from = Party, values_from = mean) %>% 
  print(n = Inf)

data %>%
  select(weightfactor,
         `Type` = disinfo_rand,
         `Party` = party,
         `Right-wing political actors` = disinfo_harm_cons,
         `Left-wing political actors` = disinfo_harm_lib,
         `Officials of the US Government` = disinfo_harm_gov,
         `US citizens generally` = disinfo_harm_indiv) %>%
  pivot_longer(`Right-wing political actors`:`US citizens generally`) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(Type, Party, name) %>%
  summarize(mean = survey_mean(value, vartype = "ci", na.rm = TRUE)) %>%
  filter(Party %in% 1:2) %>%
  mutate(Party = factor(Party, levels = 1:2, labels = c("Democrats", "Republicans"))) %>% 
  pivot_wider(id_cols = c(Type, name), names_from = Party, values_from = mean) %>% 
  print(n = Inf)

# regression models -------------------------------------
tidy(disinfo_risk) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)
summary(disinfo_risk)

tidy(disinfo_harm_pol) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_harm_int) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_harm_cov) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_resp_pol) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_resp_int) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)

tidy(disinfo_resp_cov) %>% 
  mutate(value = paste0(round(estimate, 2), " (", round(std.error, 2), ")", ifelse(p.value < 0.05, "*", ""))) %>% 
  pivot_wider(id_cols = term, names_from = response, values_from = value)


#Multiple Corrections Tests for Regression Models

#risk perceptions
fit1<- lm(disinfo_pubhealth ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2))
fit2 <-lm(disinfo_intrel ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2))
fit3 <-lm(disinfo_econ ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2))
fit4 <- lm(disinfo_govstab ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2))
fit5 <- lm(disinfo_natsec ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2))

summary(fit1)

p1   <- car::Anova(fit1, type = 2)["party", "Pr(>F)"]
p2   <- car::Anova(fit2, type = 2)["party", "Pr(>F)"]
p3   <- car::Anova(fit3, type = 2)["party", "Pr(>F)"]
p4   <- car::Anova(fit4, type = 2)["party", "Pr(>F)"]
p5   <- car::Anova(fit5, type = 2)["party", "Pr(>F)"]

p.unadj  <- c(p1, p2, p3, p4, p5)
p.bonf   <- p.adjust(p.unadj, method = "bonferroni")
p.hommel <- p.adjust(p.unadj, method = "hommel")
p.fdr <- p.adjust(p.unadj, method = "fdr")

DF   <- data.frame(Unadjusted = round(p.unadj,  3),
                   Bonferroni = round(p.bonf,   3),
                   Hommel     = round(p.hommel, 3),
                   FDR = round(p.fdr, 3))
rownames(DF) <- c("Pub Health", "Int. Rel.", "Econ", "Gov. Stab.", "Nat.Sec.")


#Harm Politics
fit6 <- lm(disinfo_harm_gov ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US")) 
fit7 <- lm(disinfo_harm_lib  ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US"))
fit8 <- lm(disinfo_harm_indiv ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US")) 
fit9 <- lm(disinfo_harm_cons ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US")) 

p6   <- car::Anova(fit6, type = 2)["party", "Pr(>F)"]
p7   <- car::Anova(fit7, type = 2)["party", "Pr(>F)"]
p8   <- car::Anova(fit8, type = 2)["party", "Pr(>F)"]
p9   <- car::Anova(fit9, type = 2)["party", "Pr(>F)"]


p.unadj2  <- c(p6, p7, p8, p9)
p.bonf2   <- p.adjust(p.unadj2, method = "bonferroni")
p.hommel2 <- p.adjust(p.unadj2, method = "hommel")
p.fdr2 <-p.adjust(p.unadj2, method = "fdr")

DF2   <- data.frame(Unadjusted = round(p.unadj2,  3),
                    Bonferroni = round(p.bonf2,   3),
                    Hommel     = round(p.hommel2, 3),
                    FDR = round(p.fdr2))
rownames(DF2) <- c("Gov", "Lib", "Indiv", "Cons")



#Harm Int Rel
fit10 <- lm(disinfo_harm_gov ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))
fit11<- lm(disinfo_harm_lib ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea")) 
fit12 <- lm(disinfo_harm_indiv ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea")) 
fit13 <- lm(disinfo_harm_cons ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))

p10   <- car::Anova(fit10, type = 2)["party", "Pr(>F)"]
p11   <- car::Anova(fit11, type = 2)["party", "Pr(>F)"]
p12   <- car::Anova(fit12, type = 2)["party", "Pr(>F)"]
p13   <- car::Anova(fit13, type = 2)["party", "Pr(>F)"]


p.unadj3  <- c(p10, p11, p12, p13)
p.bonf3   <- p.adjust(p.unadj3, method = "bonferroni")
p.hommel3 <- p.adjust(p.unadj3, method = "hommel")
p.fdr3 <- p.adjust(p.unadj3, method = "fdr")

DF3   <- data.frame(Unadjusted = round(p.unadj3,  3),
                    Bonferroni = round(p.bonf3,   3),
                    Hommel     = round(p.hommel3, 3),
                    FDR = round(p.fdr3, 3))
rownames(DF3) <- c("Gov", "Lib", "Indiv", "Cons")


#Harm COVID

fit14 <- lm(disinfo_harm_gov ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))
fit15 <- lm(disinfo_harm_lib ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19")) 
fit16 <- lm(disinfo_harm_indiv ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19")) 
fit17 <- lm(disinfo_harm_cons ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))

p14   <- car::Anova(fit14, type = 2)["party", "Pr(>F)"]
p15   <- car::Anova(fit15, type = 2)["party", "Pr(>F)"]
p16   <- car::Anova(fit16, type = 2)["party", "Pr(>F)"]
p17   <- car::Anova(fit17, type = 2)["party", "Pr(>F)"]

p.unadj4  <- c(p14, p15, p16, p17)
p.bonf4   <- p.adjust(p.unadj4, method = "bonferroni")
p.hommel4 <- p.adjust(p.unadj4, method = "hommel")
p.fdr4 <- p.adjust(p.unadj4, method = "fdr")

DF4   <- data.frame(Unadjusted = round(p.unadj4,  3),
                    Bonferroni = round(p.bonf4,   3),
                    Hommel     = round(p.hommel4, 3),
                    FDR = round(p.fdr4, 3))
rownames(DF4) <- c("Gov", "Lib", "Indiv", "Cons")



#Responsibility Politics

fit18 <- lm(disinfo_resp_foreign  ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US"))
fit19 <- lm(disinfo_resp_gov ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US")) 
fit20<- lm(disinfo_resp_indiv ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US")) 
fit21<- lm(disinfo_resp_lib ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US")) 
fit22 <- lm(disinfo_resp_cons ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US"))
fit23 <- lm(disinfo_resp_gop~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US")) 
fit24 <- lm(disinfo_resp_dem ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "politics in the US"))

p18   <- car::Anova(fit18, type = 2)["party", "Pr(>F)"]
p19   <- car::Anova(fit19, type = 2)["party", "Pr(>F)"]
p20   <- car::Anova(fit20, type = 2)["party", "Pr(>F)"]
p21   <- car::Anova(fit21, type = 2)["party", "Pr(>F)"]
p22   <- car::Anova(fit22, type = 2)["party", "Pr(>F)"]
p23   <- car::Anova(fit23, type = 2)["party", "Pr(>F)"]
p24   <- car::Anova(fit24, type = 2)["party", "Pr(>F)"]

p.unadj5  <- c(p18, p19, p20, p21, p22, p23, p24)
p.bonf5  <- p.adjust(p.unadj5, method = "bonferroni")
p.hommel5 <- p.adjust(p.unadj5, method = "hommel")
p.fdr5 <- p.adjust(p.unadj5, method = "fdr")


DF5   <- data.frame(Unadjusted = round(p.unadj5,  3),
                    Bonferroni = round(p.bonf5,   3),
                    Hommel     = round(p.hommel5, 3),
                    FDR = round(p.fdr5, 3))
rownames(DF5) <- c("Foreign", "Gov", "Indiv", "Lib", "Cons", "GOP", "Dem")




#Responsibility Int Rel
fit25 <- lm(disinfo_resp_foreign ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))
fit26 <- lm(disinfo_resp_gov ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea")) 
fit27 <- lm(disinfo_resp_indiv ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))
fit28 <- lm(disinfo_resp_lib~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))
fit29 <- lm(disinfo_resp_cons ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea")) 
fit30 <- lm(disinfo_resp_gop ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))
fit31 <- lm(disinfo_resp_dem ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "international threats from adversaries like Russia, China, and North Korea"))

p25   <- car::Anova(fit25, type = 2)["party", "Pr(>F)"]
p26   <- car::Anova(fit26, type = 2)["party", "Pr(>F)"]
p27   <- car::Anova(fit27, type = 2)["party", "Pr(>F)"]
p28   <- car::Anova(fit28, type = 2)["party", "Pr(>F)"]
p29   <- car::Anova(fit29, type = 2)["party", "Pr(>F)"]
p30   <- car::Anova(fit30, type = 2)["party", "Pr(>F)"]
p31   <- car::Anova(fit31, type = 2)["party", "Pr(>F)"]

p.unadj6  <- c(p25, p26, p27, p28, p29, p30, p31)
p.bonf6  <- p.adjust(p.unadj6, method = "bonferroni")
p.hommel6 <- p.adjust(p.unadj6, method = "hommel")
p.fdr6 <- p.adjust(p.unadj6, method = "fdr")

DF6   <- data.frame(Unadjusted = round(p.unadj6,  3),
                    Bonferroni = round(p.bonf6,   3),
                    Hommel     = round(p.hommel6, 3),
                    FDR = round(p.fdr6, 3))
rownames(DF6) <- c("Foreign", "Gov", "Indiv", "Lib", "Cons", "GOP", "Dem")



#Responsibility COVID
fit32 <- lm(disinfo_resp_foreign  ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))
fit33 <- lm(disinfo_resp_gov  ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))
fit34 <- lm(disinfo_resp_indiv ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))
fit35 <- lm(disinfo_resp_lib  ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))
fit36 <- lm(disinfo_resp_cons  ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))
fit37 <- lm(disinfo_resp_gop  ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))
fit38 <-lm(disinfo_resp_dem ~ party + age + gend + edu + inc + hisp + factor(race), data = data %>% filter(party %in% 1:2 & disinfo_rand == "COVID-19"))

p32   <- car::Anova(fit32, type = 2)["party", "Pr(>F)"]
p33   <- car::Anova(fit33, type = 2)["party", "Pr(>F)"]
p34  <- car::Anova(fit34, type = 2)["party", "Pr(>F)"]
p35   <- car::Anova(fit35, type = 2)["party", "Pr(>F)"]
p36   <- car::Anova(fit36, type = 2)["party", "Pr(>F)"]
p37   <- car::Anova(fit37, type = 2)["party", "Pr(>F)"]
p38   <- car::Anova(fit38, type = 2)["party", "Pr(>F)"]

p.unadj7  <- c(p32, p33, p34, p35, p36, p37, p38)
p.bonf7  <- p.adjust(p.unadj7, method = "bonferroni")
p.hommel7 <- p.adjust(p.unadj7, method = "hommel")
p.fdr7 <- p.adjust(p.unadj7, method = "fdr")


DF7   <- data.frame(Unadjusted = round(p.unadj7,  3),
                    Bonferroni = round(p.bonf7,   3),
                    Hommel     = round(p.hommel7, 3),
                    FDR = round(p.fdr7, 3))
rownames(DF7) <- c("Foreign", "Gov", "Indiv", "Lib", "Cons", "GOP", "Dem")





