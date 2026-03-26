library(ggplot2)
library(tidyverse)
library(ggh4x)

#####################################
# Appendix B1                       #
#####################################

load("../Output/gift_sps.rdata")

# SPS selected sites
out$selected_sites

var_continuous <- c('pct_democrat', 'log_pop',  'log_pct_rural', 'pct_unemp', 'pct_bachelor')
pdf <- bind_rows(`Target Pop`  = as.data.frame(target[, var_continuous]) %>% rownames_to_column(var = 'site'),
                 `Original`    = as.data.frame(target[, var_continuous]) %>% rownames_to_column(var = 'site'),
                 `SPS`         = as.data.frame(target[, var_continuous]) %>% rownames_to_column(var = 'site'),
                 .id = 'version') %>%
  filter((version == 'Original' & site %in% c('Collin County, TX', 'Alameda County, CA')) |
           (version == 'SPS' & site %in% out$selected_sites) | 
           (version == 'Target Pop')) %>%
  rename(`Democratic Party Vote` = pct_democrat,
         `Logged Population` = log_pop,
         `Logged % Rural` = log_pct_rural,
         `% Unemployment` = pct_unemp,
         `% Bachelor Degree` = pct_bachelor) 
pdf <- pivot_longer(pdf, cols = `Democratic Party Vote`:`% Bachelor Degree`)
pdf$name <- factor(pdf$name, levels = c('Democratic Party Vote', '% Unemployment', '% Bachelor Degree', "Logged Population", 'Logged % Rural'))
pdf$version <- factor(pdf$version, levels = c('Target Pop','Original', 'SPS'))

#####################################
# Figure OA-1                       #
#####################################

pdf('../Figures/Figure OA-1.pdf', height = 3.5, width = 8)
pdf %>% 
  group_by(version, name) %>%
  group_by(name) %>%
  ggplot(aes(x = value, fill = version)) +
  geom_histogram(alpha=1, position="identity") +
  facet_grid(version ~ name, scale = 'free') + 
  scale_fill_manual(name = '', values = c('#999999', '#009E73', "#D55E00")) +
  facetted_pos_scales(y = list(version == 'Original'  ~ scale_y_continuous(limits = c(0,2)),
                               version == 'SPS' ~ scale_y_continuous(limits = c(0,2)),
                               version == 'Target Pop' ~ scale_y_continuous(limits = c(0,22)))) +
  xlab('') + ylab(NULL) + theme_bw() + 
  theme(legend.position = 'none', panel.grid = element_blank()) 
dev.off()

#####################################
# Appendix B2                       #
#####################################

load('../Output/lupu_sps.rdata')

# Target population: total number of countries
nrow(df_target)

# Target population: by region (Americas, Europe, Africa, and Asia)
length(rownames(df_target)[df_target[, "reg_am"] == 1])
length(rownames(df_target)[df_target[, "reg_eu"] == 1])
length(rownames(df_target)[df_target[, "reg_af"] == 1])
length(rownames(df_target)[df_target[, "reg_as"] == 1])

# SPS selected sites
out$selected_sites

cont_var <- c('polity_score', 'civil_liberty', 'opposition_size', 'opposition_ethnic', 'un_ratification', 'log_pop')
pdf <- bind_rows(`Target Pop`  = as.data.frame(df_target[, cont_var]) %>% rownames_to_column(var = 'site'),
                 `Eligible Pop`= as.data.frame(df_target[, cont_var]) %>% rownames_to_column(var = 'site'),
                 `Original`    = as.data.frame(df_target[, cont_var]) %>% rownames_to_column(var = 'site'),
                 `SPS` = as.data.frame(df_target[, cont_var]) %>% rownames_to_column(var = 'site'),
                 .id   = 'version') %>%
  filter((version == 'Original' & site %in% c('Argentina', 'Israel', 'India')) |
           (version == 'SPS' & site %in% out$selected_sites) |
           (version == 'Eligible Pop' & site %in% row.names(df_target)[!row.names(df_target) %in% st_survey]) |
           version == 'Target Pop') %>%
  mutate(version = factor(version, levels = c('Target Pop', 'Eligible Pop', 'Original', 'SPS'))) %>%
  rename(`Polity Score`      = polity_score,
         `Civil Liberty`     = civil_liberty,
         `Opposition Size`   = opposition_size,
         `Ethnic Opposition` = opposition_ethnic,
         `UN Ratifications`  = un_ratification,
         `Population`        = log_pop)
pdf <- pivot_longer(pdf, cols = `Polity Score`:`Population`)
pdf$name <- factor(pdf$name, levels = c('Opposition Size', 'Ethnic Opposition', 'Civil Liberty', 'UN Ratifications', 'Polity Score',  "Population"))

#####################################
# Figure OA-2                       #
#####################################

pdf('../Figures/Figure OA-2.pdf',  height = 4, width = 8.5)
pdf %>% 
  group_by(version, name) %>%
  mutate(min = ifelse(version == 'Eligible Pop', min(value), NA),
         max = ifelse(version == 'Eligible Pop', max(value), NA)) %>%
  group_by(name) %>%
  fill(min, max, .direction = c('downup')) %>%
  mutate_at(vars(min:max), ~ ifelse(version %in% c('Eligible Pop', 'Target Pop'), NA, .x)) %>%
  ggplot(aes(x = value, fill = version)) +
  geom_vline(aes(xintercept = min), color = '#0072B2', linetype = 'dotted') +
  geom_vline(aes(xintercept = max), color = '#0072B2', linetype = 'dotted') +
  geom_histogram(position = "identity") +
  facet_grid(version ~ name, scale = 'free') + 
  scale_fill_manual(name = '', values = c('#999999', '#0072B2', "#009E73", "#D55E00", "#D55E00")) +
  facetted_pos_scales(y = list(version == 'Target Pop'   ~ scale_y_continuous(limits = c(0,26), breaks = seq(0, 25, 5)),
                               version == 'Eligible Pop' ~ scale_y_continuous(limits = c(0,21)),
                               version == 'Original'     ~ scale_y_continuous(limits = c(0,2)),
                               version == 'SPS' ~ scale_y_continuous(limits = c(0,2)))) +
  xlab('') + ylab(NULL) + theme_bw() + 
  theme(legend.position = 'none', panel.grid = element_blank()) 
dev.off()

#####################################
# Appendix B3                       #                   
#####################################

load('../Output/bisbee_sps.rdata')

# Target population: number of unique countries
nrow(X_obs)

# SPS selected sites
out$selected_sites

# Estimated p-value
cv <- sps_cv(out, sps_est$estimates_selected)
cv$p_value

sps_var <- c('GDP per capita', 'Labor Force', 'Sex Ratio', 'Fertility Rate', 
             'Education', 'Population', "region_asi", "region_afr", "region_ame", "region_eur")
pdf <- bind_rows(`Target Pop`  = as.data.frame(X_obs[, sps_var[1:6]]) %>% rownames_to_column(var = 'site'),
                 `SPS`         = as.data.frame(X_obs[, sps_var[1:6]]) %>% rownames_to_column(var = 'site'),
                 .id = 'version') %>%
  filter((version == 'SPS' & site %in% out$selected_sites) | 
           (version == 'Target Pop'))
pdf         <- pivot_longer(pdf, cols = `GDP per capita`:`Population`)
pdf$name    <- factor(pdf$name, levels = sps_var[1:6])
pdf$version <- factor(pdf$version, levels = c('Target Pop','SPS'))

#####################################
# Figure OA-3                       #
#####################################

pdf('../Figures/Figure OA-3.pdf', height = 3, width = 9)
pdf %>% 
  ggplot(aes(x = value, fill = version)) +
  geom_histogram(alpha=1, position="identity") +
  facet_grid(version ~ name, scale = 'free') + 
  scale_fill_manual(name = '', values = c('gray40', '#D55E00')) +
  facetted_pos_scales(y = list(version == 'SPS' ~ scale_y_continuous(limits = c(0,3)),
                               version == 'Target Pop' ~ scale_y_continuous(limits = c(0,8)))) +
  xlab('') + ylab(NULL) + theme_bw() + 
  theme(legend.position = 'none', panel.grid = element_blank())
dev.off()

est <- c(sps_est$estimates_selected[,1], sps_est$average_site_ATE[1], est_rss$beta)
se  <- c(sps_est$estimates_selected[,2], sps_est$average_site_ATE[2], est_rss$se)
group <- c(rep('Site-Specific ATEs', nrow(sps_est$estimates_selected)), rep('Average-Site ATE', 2))
site  <- c(row.names(sps_est$estimates_selected), 'SPS', 'Benchmark')
pdata <- data.frame(est, se, group, site)
pdata$col <- ifelse(pdata$site == 'Benchmark', pdata$site, pdata$group)
pdata$group <- factor(pdata$group, levels = c('Site-Specific ATEs', 'Average-Site ATE'))
pdata$site  <- factor(pdata$site, levels = pdata[order(pdata$est, decreasing = FALSE), 'site'])

#####################################
# Figure OA-4                       #
#####################################

pdf('../Figures/Figure OA-4.pdf', height = 4, width = 6.5)
ggplot(data = pdata,
            aes(x = site,
                y = est,
                ymin = est - qnorm(1 - 0.05/2) * se,
                ymax = est + qnorm(1 - 0.05/2) * se,
                color = col)) +
  geom_hline(yintercept = 0, linetype = 'dotted', color = 'gray') +
  geom_point(size = 3) +
  geom_errorbar(aes(width = 0.1), linewidth = 1) +
  scale_color_manual(values = c('black', 'red', 'black')) +
  facet_grid(. ~ group, scales = 'free_x', space = 'free') +
  scale_y_continuous(breaks = seq(-4, 2, by = 0.5)) +
  ylab('Estimates') + xlab('Sites') +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
