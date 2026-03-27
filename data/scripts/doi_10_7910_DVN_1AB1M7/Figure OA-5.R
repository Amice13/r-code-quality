library(ggh4x)
library(ggplot2)
library(tidyverse)
library(patchwork)

#####################################
# Appendix B4                       #
#####################################

load('../Output/metaketa_sps.rdata')
load("../Data/Applications/metaketa_target.rdata")

cont_var <- c("regime_type", "freedom_score", "corruption_score", "criminal_justice_score",
              "log_crime", "log_police_personel", "gini", "log_gdp")
pdf <- bind_rows(`Target pop`  = as.data.frame(df_target[, cont_var]) %>% rownames_to_column(var = 'site'),
                 `Original`    = as.data.frame(df_target[, cont_var]) %>% rownames_to_column(var = 'site'),
                 `Eligible pop`= as.data.frame(df_target[, cont_var]) %>% rownames_to_column(var = 'site'),
                 `SPS`         = as.data.frame(df_target[, cont_var]) %>% rownames_to_column(var = 'site'),
                 .id = 'version') %>%
  filter((version == 'Original' & site %in% c('Colombia', 'Brazil', 'Liberia', 'Philippines', 'Uganda', 'Pakistan')) |
           (version == 'SPS' & site %in% out$selected_sites) |
           (version == 'Eligible pop' & site %in% df$country[df$partner_egap==1]) |
           (version == 'Target pop'))
names(pdf) <- c("version", "site", "Regime type", 'Freedom score', 'Corruption', 'Criminal justice', 'Crime rate', 'Police personnel', 'Gini', 'GDP')
pdf <- pivot_longer(pdf, cols = `Regime type`:`GDP`)
pdf$name    <- factor(pdf$name, levels = c('Regime type', 'Freedom score', 'Criminal justice', 'Corruption', 'Crime rate', 'Police personnel', 'Gini', 'GDP'))
pdf$version <- factor(pdf$version, levels = c('Target pop', 'Eligible pop', 'Original', 'SPS'))

#####################################
# Figure OA-5                       #
#####################################

p_all <-
  pdf %>%
  group_by(version, name) %>%
  mutate(min = ifelse(version == 'Eligible pop', min(value), NA),
         max = ifelse(version == 'Eligible pop', max(value), NA)) %>%
  group_by(name) %>%
  fill(min, max, .direction = c('downup')) %>%
  mutate_at(vars(min:max), ~ ifelse(version %in% c('Eligible pop', 'Target pop'), NA, .x)) %>%
  {.->> pdf2} %>%
  ggplot(aes(x = value, fill = version)) +
  geom_vline(aes(xintercept = min), color = "#0072B2", linetype = 'dotted') +
  geom_vline(aes(xintercept = max), color = "#0072B2", linetype = 'dotted') +
  geom_histogram(alpha=1, position="identity") +
  facet_grid(version ~ name, scale = 'free') +
  scale_fill_manual(name = '', values = c('gray40', "#0072B2", '#009E73', '#D55E00')) +
  facetted_pos_scales(y = list(version == 'Original'  ~ scale_y_continuous(limits = c(0,8)),
                               version == 'SPS' ~ scale_y_continuous(limits = c(0,8)),
                               version == 'Eligible pop' ~ scale_y_continuous(limits = c(0,8)),
                               version == 'Target pop' ~ scale_y_continuous(limits = c(0,42)))) +
  xlab('') + ylab(NULL) + theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(color = 'black'),
        axis.title = element_text(color = 'black'),
        panel.grid = element_blank())

pdf('../Figures/Figure OA-5.pdf', height = 5, width = 9)
print(p_all)
dev.off()