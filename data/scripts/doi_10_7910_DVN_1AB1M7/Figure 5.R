library(ggh4x)
library(ggplot2)
library(tidyverse)
library(patchwork)

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
# Figure 5                          #
#####################################

pdf2 <-
  pdf %>%
  group_by(version, name) %>%
  mutate(min = ifelse(version == 'Eligible pop', min(value), NA),
         max = ifelse(version == 'Eligible pop', max(value), NA)) %>%
  group_by(name) %>%
  fill(min, max, .direction = c('downup')) %>%
  mutate_at(vars(min:max), ~ ifelse(version %in% c('Eligible pop', 'Target pop'), NA, .x)) 
  
pdf3 <- pdf2 %>%
  filter(version != 'Original') %>%
  mutate(version = case_when(version == 'Target pop' ~ '(a) Target population',
                             version == 'Eligible pop' ~ '(b) Eligible population',
                             version == 'SPS' ~ '(c) Synthetic purposive sampling'),
         version2 = version)

pdf3 %>%
  ggplot(aes(x = value, fill = version)) +
  geom_histogram(alpha=1, position="identity") +
  facet_grid(version ~ name, scale = 'free') -> p_base
bin_lookup <- ggplot_build(p_base)$data[[1]] %>%
  dplyr::select(PANEL, xmin, xmax) %>%
  distinct() %>%
  group_by(PANEL) %>%
  summarise(breaks = list(unique(c(xmin, xmax)))) %>%
  left_join(ggplot_build(p_base)$layout$layout %>% dplyr::select(PANEL, version, name), by = "PANEL")

ver <- c('(a) Target population', '(b) Eligible population', '(c) Synthetic purposive sampling')
col <- c('gray40', "#0072B2", '#D55E00')
ylim <- c(42, 8, 8)
plist <- list()
for (i in 1:3){
  breaks_map <- bin_lookup %>%
    filter(version == ver[i]) %>%
    dplyr::select(name, breaks) %>%
    deframe() 
  
  plist[[i]] <-
    pdf3 %>%
    filter(version == ver[i]) %>%
    ggplot(aes(x = value, fill = version, alpha = draw)) +
    geom_vline(aes(xintercept = min), color = "#0072B2", linetype = 'dotted') +
    geom_vline(aes(xintercept = max), color = "#0072B2", linetype = 'dotted') +
    facet_grid(. ~ name, scale = 'free') +
    ggtitle(ver[i]) +
    scale_fill_manual(name = '', values = col[i]) +
    scale_y_continuous(limit = c(0, ylim[i])) +
    facetted_pos_scales(x = list(name == 'Regime type' ~ scale_x_continuous(limit = c(-1.5, 2.41)),
                                 name == 'Freedom score' ~ scale_x_continuous(limit = c(-1.58, 2.21)),
                                 name == 'Criminal justice' ~ scale_x_continuous(limit = c(-2, 4)),
                                 name == 'Corruption' ~ scale_x_continuous(limit = c(-2, 4)), 
                                 name == 'Crime rate' ~ scale_x_continuous(limit = c(-3, 2)),
                                 name == 'Police personnel' ~ scale_x_continuous(limit = c(-1.9, 2.66)),
                                 name == 'Gini' ~ scale_x_continuous(limit = c(-2.08, 3.35)),
                                 name == 'GDP' ~ scale_x_continuous(limit = c(-2.15, 3.45)))) +
    xlab('') + ylab(NULL) +
    theme_bw() +
    theme(legend.position = 'none', panel.grid = element_blank(),
          strip.background = element_blank(),
          axis.text = element_text(color = 'black', size = 7),
          axis.title = element_text(color = 'black'),
          strip.text = element_text(color = 'black'),
          plot.title = element_text(hjust = 0.5, color = 'black', size = 15, family = 'Times'))

  for (vname in unique(pdf3$name)) {
    df_panel <- pdf3 %>% filter(name == vname & version == ver[i])
    plist[[i]] <- plist[[i]] +
      geom_histogram(data = df_panel, breaks = breaks_map[[vname]],
                     alpha = 1, position = "identity")
  }
  
  }

pdf('../Figures/Figure 5.pdf', height = 5, width = 9)
plist[[1]] / plist[[2]] / plist[[3]]
dev.off()

#####################################
# In-Text Analyses                  #
#####################################

# SPS selected sites for Metaketa experiments on community policing:
out$selected_sites
