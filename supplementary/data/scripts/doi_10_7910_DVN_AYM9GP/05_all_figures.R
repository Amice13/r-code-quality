
## Make sure to start the Project within the "replication-package" file for seamless runs

today <- strftime(Sys.Date(), "%y%m%d")

#-------------------------------------------------------------------------------------#
# 0. Setup   ----
#-------------------------------------------------------------------------------------#
require(tidyverse)
require(haven)
require(readstata13)
require(here)

#-------------------------------------------------------------------------------------#
# Figure 2. Variation in sexism    ----
#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#
## Panel A     ----
#-------------------------------------------------------------------------------------#
require(statebins)

fun_prep_map_bound <- function(df, varname){
  
  quant.list <- unname(quantile(df[[varname]],
                                probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))
  df$share <- cut(df[[varname]],
                  breaks = quant.list,
                  labels = c(1:5),
                  include.lowest = TRUE,
                  right = FALSE)
  
  bound <- numeric(length(quant.list))
  for(i in seq_along(quant.list)){
    bound[i] <- format(round(quant.list[[i]], digits = 3), nsmall = 3)
  }
  
  df$share_lb <- 
    paste0("[", 
           bound[c(1, 2, 3, 4, 5)][as.numeric(df$share)], 
           # the first part selects the vector from the 'bound' list. 
           # then the second part brings in the 'share' value from the df 'genrole' 
           ", ",
           bound[c(2, 3, 4, 5, 6)][as.numeric(df$share)],
           ifelse(df$share=="5", "]", ")"))
  
  # make sure the levels are retained 
  df$share_lb <-
    factor(df$share_lb,
           levels = c(paste0("[", bound[1], ", ", bound[2], ")"),
                      paste0("[", bound[2], ", ", bound[3], ")"),
                      paste0("[", bound[3], ", ", bound[4], ")"),
                      paste0("[", bound[4], ", ", bound[5], ")"),
                      paste0("[", bound[5], ", ", bound[6], "]")))
  
  return(df)
  
}


# this uses 'share' instead of 'share_lb' 
# only displaying lowest to highest 
statebin_figure <- function(dataset, direction, name) { # direction = -1 for her share
  
  statebins(
    dataset,
    value_col = "share",
    state_col = "state_a",
    palette = "Greys",
    labels = c("Lowest", "", "", "", "Highest"),
    direction = direction,
    ggplot2_scale_function = scale_fill_brewer,
    name = name
  ) +
    labs(title = "") +
    theme_statebins(legend_position = "top") +
    theme(text = element_text(size = 12, family = "Arial")) +
    theme(plot.margin = unit(c(t = 0, 
                               r = 0, 
                               l = 0, 
                               b = 0), "cm"),
          legend.margin = margin(l = 4.5,
                                 b = -1.5,
                                 unit = "cm"),
          legend.key.size = unit(1.05, "cm"),
          legend.spacing.x = unit(-0.2, "cm"),
          legend.key = element_blank(),
          legend.title = element_text(size = 10.5)) +
    guides(fill = guide_legend(title.position = "top",
                               label.position = "bottom",
                               title.vjust = -1.2))
  
  
}


# this uses 'share_lb' instead of 'share' 
statebin_figure_bound <- function(dataset, direction, reverse) { 
  # direction = -1 if higher values are less gender inequality
  # accompany that with reverse = TRUE to keep the legend order coherent
  statebins(
    dataset,
    value_col = "share_lb",
    state_col = "state_a",
    palette = "OrRd",
    direction = direction,
    ggplot2_scale_function = scale_fill_brewer,
    name = ""
  ) +
    labs(title = "") +
    theme_statebins(legend_position = "right") +
    theme(text = element_text(size = 11, family = "Arial")) +
    theme(plot.margin = unit(c(t = 0, 
                               r = 0, 
                               l = 0, 
                               b = 0), "cm"),
          legend.margin = margin(l = -2,
                                 t = -2.05,
                                 unit = "cm"),
          legend.key.size = unit(0.25, "cm"),
          legend.spacing.x = unit(0, "cm"),
          legend.key = element_blank(),
          legend.text = element_text(size = 6, family = "Arial")) +
    guides(fill = guide_legend(title.position = "top",
                               label.position = "right",
                               title.vjust = -1,
                               reverse = reverse))
}



sexism <- read.dta13(here("data/results/sexism_by_state.dta"),
                     nonint.factors = TRUE)
sexism_map <- fun_prep_map_bound(df      = sexism,
                                 varname = "m_sexism")
levels(sexism_map$share_lb)

sexism_map$state_a <- as.character(sexism_map$state_a)
sexism_map$share <- as.character(sexism_map$share)

png(filename = here("figures", "figure2-sexism-map.png"),
    width = 5, height = 4, units = "in", res = 1080, 
    family = "Arial", bg = "transparent")
statebin_figure(dataset   = sexism_map,
                      direction = 1,
                      name   = "") 
dev.off()


#-------------------------------------------------------------------------------------#
## Panel B     ----
#-------------------------------------------------------------------------------------#
styear <- read.dta13(here("data/results", "state_context_by_year.dta"),
                     nonint.factors = TRUE)

styear_avg <- styear %>%
  group_by(state_a) %>%
  summarise(mean_sexism = mean(m_sexism), .groups = "drop")

styear_sample <- styear %>%
  select(state_a, year, m_sexism) %>%
  left_join(styear_avg, by = "state_a")

png(filename = here("figures", "figure2-sexism_scatter_bystate.png"), 
    width=7, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
styear_sample %>% 
  mutate(state_a = forcats::fct_reorder(state_a, mean_sexism, .desc = TRUE)) %>% 
  ggplot(aes(x = state_a, y = m_sexism, alpha = year)) +
  geom_point() +
  geom_point(data = styear_avg, 
             aes(x = state_a, y = mean_sexism),
             shape = 18, color = "red", size = 3, alpha = 0.6, inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0.15, 0.9)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color = NA), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent', color = NA), 
    legend.box.background = element_rect(fill='transparent', color = NA)
  ) +
  labs(x = "State", y = "Sexism (state-year level)", alpha = "Year")

dev.off()



#-------------------------------------------------------------------------------------#
# Figure 3. Predicted her share of earnings among new parents      ----
#-------------------------------------------------------------------------------------#
ld_fb <- read.dta13(here("data", "CPS_state_FB_ELDCH_analytic.dta"), 
                    nonint.factors = TRUE) %>%
  filter(fb_sample_marrcohab==1)

mg <- read.dta13(here("data/results", "mgp_sexism_her_share.dta")) %>%
  rename(b = `_margin`,
         lb = `_ci_lb`,
         ub = `_ci_ub`,
         sexism = `_at2`) %>%
  select(sexism, b, lb, ub)

nochild_base <- as.numeric(ld_fb %>% 
                             filter(firstbirth==0,
                                    l_year==2) %>%
                             summarise(mean(her_share)))

png(filename = here("figures", "figure3_mgp_sexism.png"),
    width = 6, height = 5, units = "in", res = 300, 
    family = "Arial", bg = "transparent")
ggplot(mg, aes(y=b, x=sexism)) +
  geom_ribbon(aes(x=sexism, ymin=lb, ymax=ub), 
              linetype=0, alpha=0.4, fill="grey") +
  stat_smooth(geom="smooth", se=F, na.rm=T, color = "black") +
  geom_segment(aes(x=-1.5, xend=1.5, 
                   y=nochild_base, yend=nochild_base),
               color="black", linewidth=0.5, linetype="dashed") +
  scale_x_continuous(breaks = seq(-1.5, 1.5, 0.5), limits = c(-1.5, 1.5)) +
  theme_bw() +
  theme(text = element_text(family="Arial", size=11), 
        legend.position = "none",
        panel.background = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  labs(x = "Sexism index",
       y = "Predicted wives' earnings share (MIS8)")
dev.off()

#-------------------------------------------------------------------------------------#
# Figure 4. Predicted her and his earnings     ----
#-------------------------------------------------------------------------------------#
herhis <- rbind(
  read.dta13(here("data/results", "mgp_sexism_her_earnings.dta")) %>% mutate(cate = "Wives"),
  read.dta13(here("data/results", "mgp_sexism_his_earnings.dta")) %>% mutate(cate = "Husbands"))
herhis <- herhis %>% 
  mutate(cate = factor(cate, levels = c("Wives", "Husbands")))

ld_ec <- ld_fb %>% 
  mutate(ln_her_earnings = log(her_earnings + 0.1),
         ln_his_earnings = log(his_earnings + 0.1))
hermean <- mean(ld_ec[ld_ec$firstbirth==0 & ld_ec$l_year==2, ]$ln_her_earnings)
hismean <- mean(ld_ec[ld_ec$firstbirth==0 & ld_ec$l_year==2, ]$ln_his_earnings)

herhis <- herhis %>%
  rename(b = `_margin`,
         lb = `_ci_lb`,
         ub = `_ci_ub`,
         sexism = `_at2`) %>%
  select(cate, sexism, b, lb, ub)

col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255) 
png(filename = here("figures", "figure4_mgp_her_his_earnings.png"), 
    width=6, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
ggplot(herhis, aes(y=b, x=sexism, group=cate)) +
  geom_ribbon(aes(x=sexism, ymin=lb, ymax=ub), 
              linetype=0, alpha=0.4, fill="grey") +
  stat_smooth(geom="smooth", se=F, na.rm=T, aes(color=cate)) +
  scale_color_manual(values=c("Wives" = "black", 
                              "Husbands" = "grey70"), 
                     name="") +
  scale_x_continuous(breaks = seq(-1.5, 1.5, .5), limits = c(-1.5, 1.5)) +
  geom_hline(yintercept = hermean, 
             linetype="dashed", color="black", 
             alpha=.75, linewidth=0.5) +
  geom_hline(yintercept = hismean, 
             linetype="dashed", color="grey70", 
             alpha=.75, linewidth=0.5) +
  theme_bw() +
  theme(text = element_text(family="Arial", size=11), 
        legend.position = "bottom",
        panel.background = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = col_grid)) +
  labs(x="Sexism index", 
       y="Predicted logged earnings (MIS8)")
dev.off()

#-------------------------------------------------------------------------------------#
# Figure 5. Subgroup AME     ----
#-------------------------------------------------------------------------------------#
ame <- read.dta13(here("data/results", "threeway_ame_sexism.dta"))
ame$cate <- c(rep(c("Race", "Educational attainment", "Age at first birth"), each = 2), 
              rep("Decade", each = 4))

ame$dum  <- c("Other\nracialized groups", "White\n(non-Hispanic)",
              "No college\ndegree", "With college\ndegree",
              "Younger\nthan 30", "30 or older",
              "1980s", "1990s", "2000s", "2010s")
ame$dum <- factor(ame$dum, 
                  levels = c("Other\nracialized groups", "White\n(non-Hispanic)",
                             "No college\ndegree", "With college\ndegree",
                             "Younger\nthan 30", "30 or older",
                             "1980s", "1990s", "2000s", "2010s"))
ame$type <- c(rep(c(2, 1, 3), each = 2), rep(4, each = 4))

p <- list()

for(i in 1:4){
  df = subset(ame, type==i)
  
  p[[i]] <- ggplot(df, aes(x = dum, y = b)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = .5) +
    geom_point() +
    geom_pointrange(aes(x=dum, ymin = lb, ymax = ub)) +
    theme_bw() +
    scale_y_continuous(limits = c(-.07, .024), breaks = seq(-.07, .02, .01)) +
    theme(axis.text.x = element_text(family = "Arial", size = 11),
          text = element_text(family = "Arial", size = 14),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(x="", y="")
}

#p[[1]] <- p[[1]] + labs(title = "Educational Attainment")
#p[[2]] <- p[[2]] + labs(title = "Race")
#p[[3]] <- p[[3]] + labs(title = "Age at first birth")

library(gridExtra)
grid.arrange(grobs = p, ncol = 3)


png(filename = here("figures",  "figure5_threeway_ame_educ.png"), 
    width=3.5, height=4.5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
p[[1]]
dev.off()

png(filename = here("figures", "figure5_threeway_ame_re.png"), 
    width=3.5, height=4.5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
p[[2]]
dev.off()

png(filename = here("figures", "figure5_threeway_ame_afb.png"), 
    width=3.5, height=4.5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
p[[3]]
dev.off()

png(filename = here("figures", "figure5_threeway_ame_decade.png"), 
    width=10.5, height=4.5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
p[[4]]
dev.off()

#-------------------------------------------------------------------------------------#
# Appendix Figures A1     ----
#-------------------------------------------------------------------------------------#
# data 
contexts <- read.dta13(here("data/results", "sexism_main_components.dta"), 
                       nonint.factors = TRUE)
contexts_mean <- contexts %>%
  select(state_a, starts_with("m_"))
contexts_mean$state_a <- as.character(contexts_mean$state_a)

# wage gap 
contexts_map <- fun_prep_map_bound(df = contexts_mean,
                                   varname = "m_wagegap")
contexts_map$share <- as.character(contexts_map$share)

png(filename = here("figures", "appen_01_wagegap.png"),
    width = 5, height = 4, units = "in", res = 1080, 
    family = "Arial", bg = "transparent")
statebin_figure(dataset = contexts_map,
                direction = 1, 
                name   = "")
dev.off()

# lfp gap
contexts_map <- fun_prep_map_bound(df = contexts_mean,
                                   varname = "m_lfpgap")
contexts_map$share <- as.character(contexts_map$share)

png(filename = here("figures", "appen_02_lfpgap.png"),
    width = 5, height = 4, units = "in", res = 1080, 
    family = "Arial", bg = "transparent")
statebin_figure(dataset = contexts_map,
                direction = 1, 
                name   = "")
dev.off()

# paygap
contexts_map <- fun_prep_map_bound(df = contexts_mean,
                                   varname = "m_paygap")
contexts_map$share <- as.character(contexts_map$share)

png(filename = here("figures", "appen_03_paygap.png"),
    width = 5, height = 4, units = "in", res = 1080, 
    family = "Arial", bg = "transparent")
statebin_figure(dataset = contexts_map,
                direction = 1, 
                name   = "")
dev.off()

# maleleg
contexts_map <- fun_prep_map_bound(df = contexts_mean,
                                   varname = "m_maleleg")
contexts_map$share <- as.character(contexts_map$share)

png(filename = here("figures", "appen_04_maleleg.png"),
    width = 5, height = 4, units = "in", res = 1080, 
    family = "Arial", bg = "transparent")
statebin_figure(dataset = contexts_map,
                direction = 1, 
                name   = "")
dev.off()

# evanpro
contexts_map <- fun_prep_map_bound(df = contexts_mean,
                                   varname = "m_evanpro")
contexts_map$share <- as.character(contexts_map$share)

png(filename = here("figures", "appen_05_evanpro.png"),
    width = 5, height = 4, units = "in", res = 1080, 
    family = "Arial", bg = "transparent")
statebin_figure(dataset = contexts_map,
                direction = 1, 
                name   = "")
dev.off()

# genrole
contexts_map <- fun_prep_map_bound(df = contexts_mean,
                                   varname = "m_genrole")
contexts_map$share <- as.character(contexts_map$share)

png(filename = here("figures", "appen_06_genrole.png"),
    width = 5, height = 4, units = "in", res = 1080, 
    family = "Arial", bg = "transparent")
statebin_figure(dataset = contexts_map,
                direction = 1, 
                name   = "")
dev.off()

# abany
contexts_map <- fun_prep_map_bound(df = contexts_mean,
                                   varname = "m_abany")
contexts_map$share <- as.character(contexts_map$share)

png(filename = here("figures", "appen_07_abany.png"),
    width = 5, height = 4, units = "in", res = 1080, 
    family = "Arial", bg = "transparent")
statebin_figure(dataset = contexts_map,
                direction = 1, 
                name   = "")
dev.off()

#-------------------------------------------------------------------------------------#
# Appendix Figures A2     ----
#-------------------------------------------------------------------------------------#
styear <- read.dta13(here("data/results", "z_state_context_by_year.dta"),
                     nonint.factors = TRUE) %>% select(year, state_a, starts_with("m_"))

actual_mean <- read.dta13(here("data/results", "sexism_main_components.dta"),
                          nonint.factors = TRUE) %>%
  select(state_a, starts_with("m_"))
names(actual_mean) <- str_replace_all(names(actual_mean), "m_", "avg_")
styear_sample <- left_join(styear, actual_mean, by = "state_a")

# wage gap 
png(filename = here("figures", "appen_scatter_wagegap.png"), 
    width=8, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
styear_sample %>% 
  mutate(state_a = forcats::fct_reorder(state_a, avg_wagegap, .desc = TRUE)) %>% 
  ggplot(aes(x = state_a, y = m_wagegap, alpha = year)) +
  geom_point() +
  geom_point(data = actual_mean, 
             aes(x = state_a, y = avg_wagegap),
             shape = 18, color = "red", size = 3, alpha = 0.6, inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0.15, 0.9)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color = NA), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent', color = NA), 
    legend.box.background = element_rect(fill='transparent', color = NA)
  ) +
  labs(x = "State", y = "Standardized mean value", alpha = "Year")
dev.off()

# lfp gap 
png(filename = here("figures", "appen_scatter_lfpgap.png"), 
    width=8, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
styear_sample %>% 
  mutate(state_a = forcats::fct_reorder(state_a, avg_lfpgap, .desc = TRUE)) %>% 
  ggplot(aes(x = state_a, y = m_lfpgap, alpha = year)) +
  geom_point() +
  geom_point(data = actual_mean, 
             aes(x = state_a, y = avg_lfpgap),
             shape = 18, color = "red", size = 3, alpha = 0.6, inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0.15, 0.9)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color = NA), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent', color = NA), 
    legend.box.background = element_rect(fill='transparent', color = NA)
  ) +
  labs(x = "State", y = "Standardized mean value", alpha = "Year")
dev.off()

# occ pay gap 
png(filename = here("figures", "appen_scatter_occpaygap.png"), 
    width=8, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
styear_sample %>% 
  mutate(state_a = forcats::fct_reorder(state_a, avg_paygap, .desc = TRUE)) %>% 
  ggplot(aes(x = state_a, y = m_paygap, alpha = year)) +
  geom_point() +
  geom_point(data = actual_mean, 
             aes(x = state_a, y = avg_paygap),
             shape = 18, color = "red", size = 3, alpha = 0.6, inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0.15, 0.9)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color = NA), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent', color = NA), 
    legend.box.background = element_rect(fill='transparent', color = NA)
  ) +
  labs(x = "State", y = "Standardized mean value", alpha = "Year")
dev.off()

# weak political representation of women
png(filename = here("figures", "appen_scatter_maleleg.png"), 
    width=8, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
styear_sample %>% 
  mutate(state_a = forcats::fct_reorder(state_a, avg_maleleg, .desc = TRUE)) %>% 
  ggplot(aes(x = state_a, y = m_maleleg, alpha = year)) +
  geom_point() +
  geom_point(data = actual_mean, 
             aes(x = state_a, y = avg_maleleg),
             shape = 18, color = "red", size = 3, alpha = 0.6, inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0.15, 0.9)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color = NA), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent', color = NA), 
    legend.box.background = element_rect(fill='transparent', color = NA)
  ) +
  labs(x = "State", y = "Standardized mean value", alpha = "Year")
dev.off()

# religious conservativsm
png(filename = here("figures", "appen_scatter_evanpro.png"), 
    width=8, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
styear_sample %>% 
  mutate(state_a = forcats::fct_reorder(state_a, avg_evanpro, .desc = TRUE)) %>% 
  ggplot(aes(x = state_a, y = m_evanpro, alpha = year)) +
  geom_point() +
  geom_point(data = actual_mean, 
             aes(x = state_a, y = avg_evanpro),
             shape = 18, color = "red", size = 3, alpha = 0.6, inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0.15, 0.9)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color = NA), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent', color = NA), 
    legend.box.background = element_rect(fill='transparent', color = NA)
  ) +
  labs(x = "State", y = "Standardized mean value", alpha = "Year")
dev.off()

# working mothers
png(filename = here("figures", "appen_scatter_genrole.png"), 
    width=8, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
styear_sample %>% 
  mutate(decade = case_when(year %in% 1982:1989 ~ "1980s",
                            year %in% 1990:1999 ~ "1990s",
                            year %in% 2000:2009 ~ "2000s",
                            year %in% 2010:2019 ~ "2010s")) %>%
  group_by(state_a, decade) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(state_a = forcats::fct_reorder(state_a, avg_genrole, .desc = TRUE)) %>% 
  ggplot(aes(x = state_a, y = m_genrole, alpha = year)) +
  geom_point() +
  geom_point(data = actual_mean, 
             aes(x = state_a, y = avg_genrole),
             shape = 18, color = "red", size = 3, alpha = 0.6, inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0.15, 0.9)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color = NA), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent', color = NA), 
    legend.box.background = element_rect(fill='transparent', color = NA)
  ) +
  labs(x = "State", y = "Standardized mean value", alpha = "Year")
dev.off()


# abortion attitude
png(filename = here("figures", "appen_scatter_abortion.png"), 
    width=8, height=5, units = "in", res = 1080, 
    family = "Arial", bg="transparent")
styear_sample %>% 
  mutate(decade = case_when(year %in% 1982:1989 ~ "1980s",
                            year %in% 1990:1999 ~ "1990s",
                            year %in% 2000:2009 ~ "2000s",
                            year %in% 2010:2019 ~ "2010s")) %>%
  group_by(state_a, decade) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(state_a = forcats::fct_reorder(state_a, avg_abany, .desc = TRUE)) %>% 
  ggplot(aes(x = state_a, y = m_abany, alpha = year)) +
  geom_point() +
  geom_point(data = actual_mean, 
             aes(x = state_a, y = avg_abany),
             shape = 18, color = "red", size = 3, alpha = 0.6, inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0.15, 0.9)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color = NA), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent', color = NA), 
    legend.box.background = element_rect(fill='transparent', color = NA)
  ) +
  labs(x = "State", y = "Standardized mean value", alpha = "Year")
dev.off()
