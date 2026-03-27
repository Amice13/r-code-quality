#######################################################################
# Setup
#######################################################################

# load pacman package manager
if (!require(pacman)) { install.packages("pacman") }

# load (install) packages
pacman::p_load(
  tidyverse,
  lubridate,
  janitor,
  ggthemes,
  magrittr,
  psych,
  broom,
  car,
  tseries,
  vars,
  imputeTS,
  seastests,
  strucchange,
  kableExtra
)

# undo function masking
select <- dplyr::select
filter <- dplyr::filter
lag <- dplyr::lag
lead <- dplyr::lead
annotate <- ggplot2::annotate

# options
options(scipen = 999)
Sys.setlocale(locale = "en_US")

# ggplot2 theme
theme_set(theme_tufte(base_size = 12,
                      base_family = "Times New Roman"))

theme_update(
  plot.subtitle = element_text(hjust = 0.5),
  plot.title = element_text(hjust = 0.5),
  plot.margin = unit(c(0.2, 0.1, 0.2, 0.1), "cm"),
  axis.line = element_line(size = 0.5)
)

######################################################################
# Load data
######################################################################

dailydata <- read_rds("repdata_daily.rds")

(measures_media <- unique(dailydata$measure_media))
select_measure <- "n_log" # set default

halfweekdata <- read_rds("repdata_halfweek.rds")

(issues <- unique(halfweekdata$issue))
(subsets <- unique(halfweekdata$subset_value))

################################################################
# MAIN PLOT (FIGURE 1) SALIENCE SERIES
################################################################

p_data <- dailydata %>%
  filter(measure_media == "n_log") %>%
  pivot_wider(names_from = measure_public, values_from = salience_public) %>%
  select(
    date,
    salience_rel_raw,
    salience_abs_raw,
    salience_abs_filter,
    salience_rel_filter,
    salience_media
  )

p <- p_data %>%
  gather(series, salience, -date) %>%
  mutate(
    type = case_when(
      str_detect(series, "rel") & !str_detect(series, "media") ~ 1,
      str_detect(series, "abs") &
        !str_detect(series, "media") ~ 2,
      str_detect(series, "media") ~ 3
    ),
    agenda = ifelse(str_detect(series, "media"), "media", "public")
  ) %>%
  ggplot(aes(x = date, y = salience, color = factor(type))) +
  ggforce::facet_col(~ factor(
    agenda,
    levels = c("public", "media"),
    labels = c(
      "Public salience of climate change (%)",
      "Media salience of climate change (index)"
    )
  ),
  scales = "free_y",
  space = "free") +
  geom_point(
    data = . %>% filter(!str_detect(series, "filter|media")),
    size = .9,
    shape = 20
  ) +
  geom_line(data = . %>% filter(str_detect(series, "filter")),  size = 0.55) +
  geom_line(data = . %>% filter(agenda == "media"), size = 0.4) +
  scale_x_date(
    date_labels = "%B",
    limits = c(NA_Date_, ymd("20190606")),
    expand = expansion(add = 3.2)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 5),
    limits = c(NA, NA),
    expand = c(0, .5)
  ) +
  scale_color_manual(
    values = c("grey60", "grey20", "grey40"),
    limits = c("2", "1", "3"),
    labels = c(
      "Absolute public salience of climate change (%)",
      "Relative public salience of climate change (%)",
      "Media salience"
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    strip.placement = "right",
    strip.background = element_rect(fill = "white", size = 0.8, color =
                                      "white"),
    strip.text = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2)
  ) +
  guides(color = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL
  )

# Annotations
# Series labels
p +
  geom_text(
    aes(
      x = last(date) + 0.4,
      y = last(salience),
      label = c(" relative")
    ),
    data = . %>% filter(type == 1),
    color = "grey20",
    hjust = "left",
    size = 2.5,
    family = "Times",
    check_overlap = T
  ) +
  geom_text(
    aes(
      x = last(date) + 0.4,
      y = last(salience),
      label = c(" absolute")
    ),
    data = . %>% filter(type == 2),
    color = "grey60",
    hjust = "left",
    size = 2.5,
    family = "Times",
    check_overlap = T
  ) +
  # Vertical lines
  geom_vline(
    aes(xintercept = ymd("20190315")),
    size = 0.2,
    alpha = 0.2,
    linetype = "dashed"
  ) +
  geom_vline(
    aes(xintercept = ymd("20190507")),
    size = 0.2,
    alpha = 0.2,
    linetype = "dashed"
  ) +
  geom_vline(
    aes(xintercept = ymd("20190525")),
    size = 0.2,
    alpha = 0.2,
    linetype = "dashed"
  ) +
  geom_vline(
    aes(xintercept = ymd("20190605")),
    size = 0.2,
    alpha = 0.2,
    linetype = "dashed"
  ) +
  # Protests
  geom_text(
    aes(
      x = ymd("20190315") - 10,
      y = p_data$salience_abs_filter[p_data$date == ymd("20190315")] +
        21,
      label = c("Global climate change demonstrations ")
    ),
    data = . %>% filter(agenda == "public"),
    hjust = "right",
    color = "black",
    size = 2.5,
    family = "Times",
    check_overlap = T
  ) +
  geom_curve(
    aes(
      x = ymd("20190315") - 10,
      xend = ymd("20190315"),
      y = p_data$salience_abs_filter[p_data$date == ymd("20190315")] +
        21,
      yend = p_data$salience_abs_filter[p_data$date == ymd("20190315")]
    ),
    data = . %>% filter(agenda == "public", type == 1),
    color = "black",
    curvature = -.2,
    size = 0.05
  ) +
  # Election called
  geom_text(
    aes(
      x = ymd("20190507") - 6,
      y = p_data$salience_abs_filter[p_data$date == ymd("20190507")] +
        12,
      label = c("National election called ")
    ),
    data = . %>% filter(agenda == "public"),
    hjust = "right",
    color = "black",
    size = 2.5,
    family = "Times",
    check_overlap = T
  ) +
  geom_curve(
    aes(
      x = ymd("20190507") - 6,
      xend = ymd("20190507"),
      y = p_data$salience_abs_filter[p_data$date == ymd("20190507")] +
        12,
      yend = p_data$salience_abs_filter[p_data$date == ymd("20190507")]
    ),
    data = . %>% filter(agenda == "public", type == 1),
    color = "black",
    curvature = -.2,
    size = 0.05
  ) +
  # EP election
  geom_text(
    aes(
      x = ymd("20190525") - 4,
      y = p_data$salience_abs_filter[p_data$date == ymd("20190525")] +
        7,
      label = c("European parliament election ")
    ),
    data = . %>% filter(agenda == "public"),
    hjust = "right",
    color = "black",
    size = 2.5,
    family = "Times",
    check_overlap = T
  ) +
  geom_curve(
    aes(
      x = ymd("20190525") - 4,
      xend = ymd("20190525"),
      y = p_data$salience_abs_filter[p_data$date == ymd("20190525")] +
        7,
      yend = p_data$salience_abs_filter[p_data$date == ymd("20190525")]
    ),
    data = . %>% filter(agenda == "public", type == 1),
    color = "black",
    curvature = -.15,
    size = 0.05
  ) +
  # National election
  geom_text(
    aes(
      x = ymd("20190605") - 2,
      y = 43,
      label = c("National election ")
    ),
    data = . %>% filter(agenda == "public"),
    hjust = "right",
    color = "black",
    size = 2.5,
    family = "Times",
    check_overlap = T
  ) +
  geom_curve(
    aes(
      x = ymd("20190605") - 2,
      xend = ymd("20190605"),
      y = 43,
      yend = 49
    ),
    data = . %>% filter(agenda == "public", type == 1),
    color = "black",
    curvature = .1,
    size = 0.05
  )

ggsave(
  "figure1.png",
  width = 8,
  height = 5.5,
  dpi = 600,
  bg = "white"
)

ggsave(
  "figure1.pdf",
  width = 8,
  height = 5.5,
  dpi = 600,
  device = cairo_pdf
)

#######################################################################
# CORRELATION ANALYSIS
#######################################################################

cor_data <- dailydata %>%
  filter(measure_media == "n_log",
         measure_public == "salience_rel_filter") %>%
  group_by(issue) %>%
  arrange(date) %>%
  mutate(
    alead1 = dplyr::lead(salience_media, 7),
    alead2 = dplyr::lead(salience_media, 6),
    alead3 = dplyr::lead(salience_media, 5),
    alead4 = dplyr::lead(salience_media, 4),
    alead5 = dplyr::lead(salience_media, 3),
    alead6 = dplyr::lead(salience_media, 2),
    alead7 = dplyr::lead(salience_media, 1),
    lag0 = salience_media,
    lag1 = dplyr::lag(salience_media, 1),
    lag2 = dplyr::lag(salience_media, 2),
    lag3 = dplyr::lag(salience_media, 3),
    lag4 = dplyr::lag(salience_media, 4),
    lag5 = dplyr::lag(salience_media, 5),
    lag6 = dplyr::lag(salience_media, 6),
    lag7 = dplyr::lag(salience_media, 7)
  ) %>%
  ungroup()

# Compute correlations
cor.output <- list()
x_lags <- c(paste0("alead", seq(1:7)), paste0("lag", 0:7))

for (l in seq_along(x_lags)) {
  d_lag <- cor_data %>% select(salience_public, x_lags[[l]])
  
  cor_spearman <- corr.test(d_lag, method = "spearman")
  
  cor.output[[l]] <- bind_cols(
    issue = "Climate change",
    x = x_lags[[l]],
    cor = "Spearman's rho",
    lower = cor_spearman$ci$lower,
    r = cor_spearman$ci$r,
    upper = cor_spearman$ci$upper,
    p = cor_spearman$ci$p
  )
}

# Collect results
cors_df <- cor.output %>%
  bind_rows() %>%
  mutate(
    text = paste0(
      round(r, 2),
      case_when(p < 0.01 ~ "***",
                p >= 0.01 & p < 0.05 ~ "**",
                p >= 0.05 & p < 0.1 ~ "*",
                TRUE ~ "")
    ),
    sig = ifelse(p < 0.05, "P<0.05", "P>=0.05"),
    sync = factor(ifelse(x == "lag0", "synchronous", "lags"))
  ) %>%
  group_by(issue, cor) %>%
  arrange(x) %>%
  mutate(
    x_num = -7:7,
    prepost = case_when(x_num < 0 ~ "pre",
                        x_num > 0 ~ "post",
                        TRUE ~ "middle")
  ) %>%
  group_by(prepost, issue, cor) %>%
  mutate(prepost_mean = mean(r)) %>%
  ungroup()

horizontal_line_pre <- cors_df %>%
  filter(prepost == "pre") %>%
  pull(prepost_mean) %>%
  unique()

horizontal_line_post <- cors_df %>%
  filter(prepost == "post") %>%
  pull(prepost_mean) %>%
  unique()

# Plot
cors_df %>%
  ggplot(aes(
    x = x_num,
    y = r,
    ymin = lower,
    ymax = upper
  )) +
  annotate(
    "curve",
    x = -7.4,
    xend = 0,
    y = horizontal_line_pre,
    yend = horizontal_line_pre,
    curvature = 0,
    linetype = "dashed",
    size = 0.33
  ) +
  annotate(
    "curve",
    x = 0,
    xend = 7.4,
    y = horizontal_line_post,
    yend = horizontal_line_post,
    curvature = 0,
    linetype = "dashed",
    size = 0.33
  ) +
  geom_vline(aes(xintercept = 0),
             size = 7,
             alpha = 1,
             color = "grey90") +
  geom_pointrange(size = 0.33) +
  geom_text(aes(y = upper + 0.04,
                label = str_replace(sprintf("%.2f", r), "0\\.", "\\.")),
            size = 2.5,
            family = "Times") +
  annotate(
    "text",
    label = str_replace(sprintf("%.2f", horizontal_line_pre), "0\\.", "\\."),
    x = -0.6,
    y = horizontal_line_pre - 0.03,
    size = 2.5,
    family = "Times"
  ) +
  annotate(
    "text",
    label = str_replace(sprintf("%.2f", horizontal_line_post), "0\\.", "\\."),
    x = 0.6,
    y = horizontal_line_post + 0.03,
    size = 2.5,
    family = "Times"
  ) +
  scale_x_continuous(
    breaks = -7:7,
    limits = c(-7.4, 7.4),
    expand = c(0, 0)
  )  +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, .73),
    expand = expansion(add = 0)
  ) +
  annotate(
    "text",
    label = "Public to Media",
    x = -(7 + 1) / 2,
    y = 0.7,
    vjust = "top",
    size = 2.7,
    family = "Times"
  ) +
  annotate(
    "text",
    label = "Synchroneous  ",
    x = 0,
    y = .725,
    vjust = "center",
    alpha = 0.8,
    hjust = "right",
    size = 2.7,
    angle = 90,
    family = "Times"
  ) +
  annotate(
    "text",
    label = "Media to Public",
    x = (7 + 1) / 2,
    y = 0.7,
    vjust = "top",
    size = 2.7,
    family = "Times"
  ) +
  labs(x = "Time lag (days)",
       y = "Spearman's rho")  +
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2)
  )

ggsave(
  "figure2.png",
  height = 3,
  width = 6,
  dpi = 600,
  bg = "white"
)

ggsave(
  "figure2.pdf",
  height = 3,
  width = 6,
  dpi = 600,
  device = cairo_pdf
)

################################################################################
# DAILY GRANGER ANALYSIS
################################################################################

# Measure combinations (used for robustness checks / appendix)
measure_combinations <-
  dailydata %>%
  filter(
    measure_public %in% c(
      "salience_rel_filter",
      "salience_rel_interpol",
      "salience_abs_filter"
    )
  ) %>%
  distinct(measure_media, measure_public) %>%
  mutate(measure_comb = row_number())

# Default public salience series = Kalman-filtered
dailydata_analysis <-
  dailydata %>%
  filter(
    measure_public %in% c(
      "salience_rel_filter",
      "salience_rel_interpol",
      "salience_abs_filter"
    )
  ) %>%
  left_join(measure_combinations, by = c("measure_media", "measure_public"))

#  Empty lists for loops
granger_list_all <- list()
mlist <- list()
max_lag <- 7 # Max lag length for VAR models: 7 days

# Loop
for (i in seq_along(measure_combinations$measure_comb)) {
  dailydata_loop <- dailydata_analysis %>%
    filter(measure_comb == measure_comb[[i]])
  
  vox <- ts(dailydata_loop$salience_public)
  dr <- ts(dailydata_loop$salience_media)
  ts <- ts.union(vox, dr)
  exo <- dailydata_loop %>%
    transmute(time = 1:n(),
              weekend,
              campaign)
  
  # Get structural break for series
  (sb <-
      strucchange::breakpoints(vox ~ dr, breaks = 1, h = 0.15)$breakpoints)
  
  # If break is detected
  if (!is.na(sb)) {
    exo$sb_dummy <- c(rep(0, sb), rep(1, length(vox) - sb))
    
    # Residuals for stationarity tests
    resid_vox <-
      residuals(lm(vox ~ exo$time + exo$weekend + exo$campaign + exo$sb_dummy))
    resid_dr <-
      residuals(lm(dr ~ exo$time + exo$weekend + exo$campaign + exo$sb_dummy))
    
    # If no break is detected
  } else {
    # Residuals for stationarity tests
    resid_vox <-
      residuals(lm(vox ~ exo$time + exo$weekend + exo$campaign))
    resid_dr <-
      residuals(lm(dr ~ exo$time + exo$weekend + exo$campaign))
  }
  
  # Model
  m <-
    VAR(
      ts,
      exogen = exo,
      lag.max = max_lag,
      ic = "AIC",
      type = "const"
    )
  
  mlist[[i]] <- m
  # Normalized series for IRF plots
  mlist[[i]] <-
    VAR(
      ts.union(vox = ts(scale(vox)), dr = ts(scale(dr))),
      exogen = exo,
      lag.max = max_lag,
      ic = "AIC",
      type = "const"
    )
  
  names(mlist)[[i]] <-
    paste0(measure_combinations$measure_media,
           "_",
           measure_combinations$measure_public)[[i]]
  
  # Coefs
  coef_dr_vox <-
    tidy(m$varresult$vox) %>% filter(str_detect(term, "dr")) %>% summarise(sum_of_coef = sum(estimate)) %>% pull(sum_of_coef)
  coef_vox_dr <-
    tidy(m$varresult$dr) %>% filter(str_detect(term, "vox")) %>% summarise(sum_of_coef = sum(estimate)) %>% pull(sum_of_coef)
  
  # Collect output
  
  granger_list_all[[i]] <- bind_rows(
    # Media to public
    bind_cols(
      Issue = "Climate change",
      Measure_media = dailydata_loop$measure_media[[1]],
      Measure_public = dailydata_loop$measure_public[[1]],
      Subgroup = "All",
      `Direction of causality` = "Media-Public",
      `No. of lags` = as.numeric(m$p),
      `Sum of coefficients` = coef_dr_vox,
      # Granger test
      P = causality(m, cause = "dr")$Granger$p.value %>% as.numeric(),
      `R^2` = glance(m$varresult$vox)$r.squared,
      N = as.numeric(m$obs),
      breakpoint = sb,
      p.dw = durbinWatsonTest(m$varresult$vox)$p,
      stationary_pp_vox = pp.test(resid_vox)$p.value,
      stationary_kpss_vox = kpss.test(resid_vox)$p.value,
      stationary_adf_vox = adf.test(resid_vox, k = as.numeric(m$p))$p.value,
      stationary_pp_dr = pp.test(resid_dr)$p.value,
      stationary_kpss_dr = kpss.test(resid_dr)$p.value,
      stationary_adf_dr = adf.test(resid_dr, k = as.numeric(m$p))$p.value
    ),
    
    # Public to media
    bind_cols(
      Issue = "Climate change",
      Measure_media = dailydata_loop$measure_media[[1]],
      Measure_public = dailydata_loop$measure_public[[1]],
      Subgroup = "All",
      `Direction of causality` = "Public-Media",
      `No. of lags` = as.numeric(m$p),
      `Sum of coefficients` = coef_vox_dr,
      # Granger test
      P = causality(m, cause = "vox")$Granger$p.value %>% as.numeric(),
      `R^2` = glance(m$varresult$dr)$r.squared,
      N = as.numeric(m$obs),
      breakpoint = sb,
      p.dw = durbinWatsonTest(m$varresult$dr)$p,
      stationary_pp_vox = pp.test(resid_vox)$p.value,
      stationary_kpss_vox = kpss.test(resid_vox)$p.value,
      stationary_adf_vox = adf.test(resid_vox, k = as.numeric(m$p))$p.value,
      stationary_pp_dr = pp.test(resid_dr)$p.value,
      stationary_kpss_dr = kpss.test(resid_dr)$p.value,
      stationary_adf_dr = adf.test(resid_dr, k = as.numeric(m$p))$p.value
    )
  )
  
}

# Bind
granger_daily <- bind_rows(granger_list_all) %>%
  mutate_if(is_numeric, round, 3) # round

granger_daily %>% filter(Measure_media == "n_log",
                         Measure_public == "salience_rel_filter")

# Effect size
# "Climate keywords"
# Move on Y-axis (percentage points) resulting from 1-unit shift in media coverage (log(keywords+1))
(sumoflags <-
    granger_daily[granger_daily$Measure_media == "n_log" &
                    granger_daily$Measure_public == "salience_rel_filter" &
                    
                    granger_daily$Subgroup == "All" &
                    granger_daily$`Direction of causality` == "Media-Public", "Sum of coefficients"])
(sumoflags_actual <-
    exp(sumoflags) - 1)                              # Move on Y-axis (percentage points) resulting from 1-unit shift in media coverage (keywords)
round(1 / sumoflags_actual)                                           # Number of additional keywords necessary to achieve one-point shift in Y

# "Climate articles"
(sumoflags <-
    granger_daily[granger_daily$Measure_media == "n_articles" &
                    granger_daily$Measure_public == "salience_rel_filter" &
                    granger_daily$Subgroup == "All" &
                    granger_daily$`Direction of causality` == "Media-Public", "Sum of coefficients"])
(sumoflags_actual <-
    exp(sumoflags) - 1)                              # Move on Y-axis (percentage points) resulting from 1-unit shift in media coverage (keywords)
round(1 / sumoflags_actual)                                           # Number of additional keywords necessary to achieve one-point shift in Y

# Impulse response function
get_irf_df <-
  function(list_of_models,
           model_name,
           shock,
           n_lookahead,
           n_iterations = 500) {
    library(tidyverse)
    library(magrittr)
    library(vars)
    
    output <- list()
    
    for (i in seq_along(shock)) {
      output[[i]] <- suppressWarnings({
        list_of_models %>%
          extract2(model_name) %>%
          irf(
            n.ahead = n_lookahead,
            response = c("vox", "dr"),
            ortho = FALSE,
            runs = n_iterations
          ) %>%
          transpose() %>%
          # "Cause"
          chuck(shock[[i]]) %>%
          extract(1:3) %>%
          map(as_tibble) %>%
          bind_rows(.id = "line") %>%
          group_by(line) %>%
          mutate(
            period = 0:n_lookahead,
            shock = shock[[i]],
            model_name = model_name
          ) %>%
          ungroup() %>%
          gather(agenda, salience, dr, vox) %>%
          spread(line, salience) %>%
          rename(lower = Lower, upper = Upper)
      })
    }
    return(bind_rows(output))
  }

# IRF
irf_df <-
  get_irf_df(
    mlist,
    model_name = "n_log_salience_rel_filter",
    shock = c("dr", "vox"),
    n_lookahead = 7,
    n_iterations = 500
  ) %>%
  bind_rows() %>%
  mutate(
    agenda = factor(
      agenda,
      levels = c("vox", "dr"),
      labels = c("Public", "Media")
    ),
    shock = factor(
      shock,
      levels = c("dr", "vox"),
      labels = c("Media", "Public")
    )
  )

# IRF plot
irf_df %>%
  filter(model_name %in% c("n_log_salience_rel_filter")) %>%
  ggplot(aes(
    x = period,
    y = irf,
    group = agenda,
    fill = agenda,
    linetype = agenda
  )) +
  geom_line(size = 0.4) +
  geom_hline(aes(yintercept = 0), size = 0.3) +
  facet_wrap(~ shock, scales = "free_y") +
  scale_x_continuous(breaks = 0:max(irf_df$period),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
  scale_linetype_manual(NULL, values = c("solid", "dashed")) +
  coord_cartesian(ylim = c(-0.05, 1)) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    plot.subtitle = element_text(hjust = 0),
    legend.background = element_rect(linetype = "solid", size = 0.5)
  ) +
  labs(subtitle = "Response of a shock in...",
       x = NULL,
       y = "Climate change\n")

ggsave(
  "appendix_irf.png",
  width = 7,
  height = 4,
  dpi = 600,
  bg = "white"
)

################################################################################
# SUBGROUP GRANGER ANALYSIS (HALFWEEKS)
################################################################################

# Empty lists for loops
granger_list_all <- list()
mlist <- list()
mlist_subgroup <- list()
mlist_subgroup_subset <- list()
granger_list_subgroup <- list()
granger_list_subgroup_subset <- list()

# Max lag length for VAR models
max_lag <- 4 # default: 4 halfweeks

# VAR MODELS / GRANGER TESTS
for (i in seq_along(issues)) {
  halfweekdata_issue <- halfweekdata %>%
    filter(issue == issues[[i]])
  
  for (s in seq_along(subsets)) {
    vox <- halfweekdata_issue %>%
      filter(subset_value == subsets[[s]]) %>%
      pull(salience_public) %>%
      ts()
    
    dr <- halfweekdata_issue %>%
      filter(subset_value == subsets[[s]]) %>%
      pull(salience_media) %>%
      ts()
    
    ts <- ts.union(vox, dr)
    exo <- tibble(time = 1:length(vox))
    
    # Get structural break for series
    (sb <-
        strucchange::breakpoints(vox ~ dr, breaks = 1, h = 0.15)$breakpoints)
    if (!is.na(sb)) {
      exo$sb_dummy <- c(rep(0, sb), rep(1, length(vox) - sb))
    }
    
    # Residuals for stationarity tests
    if (!is.na(sb)) {
      resid_vox <- residuals(lm(vox ~ exo$time + exo$sb_dummy))
      resid_dr <- residuals(lm(dr ~ exo$time + exo$sb_dummy))
    } else {
      resid_vox <- residuals(lm(vox ~ exo$time))
      resid_dr <- residuals(lm(dr ~ exo$time))
    }
    
    # Model
    m <-
      VAR(
        ts,
        exogen = exo,
        lag.max = max_lag,
        ic = "AIC",
        type = "const"
      )
    mlist_subgroup_subset[[s]] <- m
    names(mlist_subgroup_subset)[[s]] <- subsets[[s]]
    
    # Coefs
    coef_dr_vox <-
      tidy(m$varresult$vox) %>% filter(str_detect(term, "dr")) %>% summarise(sum_of_coef = sum(estimate)) %>% pull(sum_of_coef)
    coef_vox_dr <-
      tidy(m$varresult$dr) %>% filter(str_detect(term, "vox")) %>% summarise(sum_of_coef = sum(estimate)) %>% pull(sum_of_coef)
    
    # Collect outputs
    granger_list_subgroup_subset[[s]] <- bind_rows(
      # Media to public
      bind_cols(
        Issue = issues[[i]],
        Subgroup = subsets[[s]],
        `Direction of causality` = "Media-Public",
        `No. of lags` = as.numeric(m$p),
        `Sum of coefficients` = coef_dr_vox,
        P = causality(m, cause = "dr")$Granger$p.value %>% as.numeric(),
        `R^2` = glance(m$varresult$vox)$r.squared,
        N = as.numeric(m$obs),
        breakpoint = sb,
        p.dw = durbinWatsonTest(m$varresult$vox)$p,
        stationary_pp_vox = pp.test(resid_vox)$p.value,
        stationary_kpss_vox = kpss.test(resid_vox)$p.value,
        stationary_adf_vox = adf.test(resid_vox, k = as.numeric(m$p))$p.value,
        stationary_pp_dr = pp.test(resid_dr)$p.value,
        stationary_kpss_dr = kpss.test(resid_dr)$p.value,
        stationary_adf_dr = adf.test(resid_dr, k = as.numeric(m$p))$p.value
      ),
      
      # Public to media
      bind_cols(
        Issue = issues[[i]],
        Subgroup = subsets[[s]],
        `Direction of causality` = "Public-Media",
        `No. of lags` = as.numeric(m$p),
        `Sum of coefficients` = coef_vox_dr,
        P = causality(m, cause = "vox")$Granger$p.value %>% as.numeric(),
        `R^2` = glance(m$varresult$dr)$r.squared,
        N = as.numeric(m$obs),
        breakpoint = sb,
        p.dw = durbinWatsonTest(m$varresult$dr)$p,
        stationary_pp_vox = pp.test(resid_vox)$p.value,
        stationary_kpss_vox = kpss.test(resid_vox)$p.value,
        stationary_adf_vox = adf.test(resid_vox, k = as.numeric(m$p))$p.value,
        stationary_pp_dr = pp.test(resid_dr)$p.value,
        stationary_kpss_dr = kpss.test(resid_dr)$p.value,
        stationary_adf_dr = adf.test(resid_dr, k = as.numeric(m$p))$p.value
      )
    )
  }
  
  granger_list_subgroup[[i]] <-
    bind_rows(granger_list_subgroup_subset)
  mlist_subgroup[[i]] <- mlist_subgroup_subset
  names(mlist_subgroup)[[i]] <- issues[[i]]
}

# Collect results
# Bind and save alternative measures (interactively)
granger_subgroups <-
  bind_rows(granger_list_all, granger_list_subgroup) %>%
  mutate(measure_media = select_measure)

granger_subgroups %>%
  arrange(P) %>%
  filter(`Direction of causality` == "Media-Public")

# Subgroup table - climate
subgroup_table <- granger_subgroups %>%
  filter(Issue == "climate") %>%
  select(Subgroup,
         `Direction of causality`,
         `No. of lags`,
         `Sum of coefficients`,
         P,
         `R^2`) %>%
  pivot_longer(c(`Sum of coefficients`, P, `R^2`)) %>%
  pivot_wider(
    names_from = c("Direction of causality", "name"),
    values_from = c(value)
  ) %>%
  clean_names() %>%
  mutate(
    order = case_when(
      str_detect(subgroup, "All") ~ 13,
      str_detect(subgroup, "18-29") ~ 1,
      str_detect(subgroup, "30-49") ~ 2,
      str_detect(subgroup, "50-69") ~ 3,
      str_detect(subgroup, "70") ~ 4,
      str_detect(subgroup, "urban") ~ 5,
      str_detect(subgroup, "rural") ~ 6,
      str_detect(subgroup, "female") ~ 7,
      str_detect(subgroup, "male") ~ 8,
      str_detect(subgroup, "leftgreen") ~ 9,
      str_detect(subgroup, "socialdemocrats") ~ 10,
      str_detect(subgroup, "centerright") ~ 11,
      str_detect(subgroup, "populistright") ~ 12
    ),
    category = case_when(
      str_detect(subgroup, "ale") ~ "Gender",
      str_detect(subgroup, "[0-9]") ~ "Age",
      str_detect(subgroup, "urban|rural") ~ "Community",
      str_detect(subgroup, "green|soc|right") ~ "Party",
      TRUE ~ "All"
    ),
    sig = ifelse(media_public_p < 0.1, "Yes", "No"),
    label = factor(
      case_when(
        media_public_p < 0.01 ~ "***",
        media_public_p < 0.05 ~ "**",
        media_public_p < 0.1 ~ "*",
        media_public_p < 0.12 ~ "",
        TRUE ~ ""
      )
    ),
    r2_fmt = paste0("R^2==", format(round(media_public_r_2, 2), nsmall =
                                      2), "~(", no_of_lags, ")")
  ) %>%
  arrange(order)

# Subgroup plot
subgroup_table %>%
  filter(subgroup != "All") %>%
  mutate(
    r2_y = case_when(
      media_public_sum_of_coefficients > 0 ~ media_public_sum_of_coefficients -
        0.008,
      media_public_sum_of_coefficients < -0.02 ~ media_public_sum_of_coefficients +
        0.008,
      TRUE ~ -0.01
    ),
    r2_col = factor(
      case_when(
        media_public_sum_of_coefficients > -0.02 &
          media_public_sum_of_coefficients < 0 ~ "black",
        media_public_p < 0.05 ~ "white",
        media_public_p < 0.1 ~ "white",
        TRUE ~ "black"
      )
    )
  ) %>%
  ggplot(aes(
    x = fct_reorder(subgroup, order),
    y = media_public_sum_of_coefficients,
    fill = label
  )) +
  geom_col(color = "black",
           width = 1,
           size = 0.25) +
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "solid") +
  geom_text(
    aes(label = r2_fmt, color = r2_col, y = r2_y),
    size = 2.5,
    parse = T,
    hjust = 0.5,
    vjust = 0.5,
    family = "Times"
  ) +
  scale_fill_manual(
    breaks = c("**", "*", ""),
    values = c("black", "grey50", "white"),
    name = "Statistical significance",
    labels = c(expression(paste(
      "Yes (", italic("p"), " < 0.05)"
    )),
    expression(paste(
      "Yes (", italic("p"), " < 0.10)"
    )),
    expression(paste(
      "No (", italic("p"), " >= 0.10)"
    )))
  ) +
  scale_color_manual(values = c("black", "white"),
                     breaks = c("black", "white")) +
  scale_x_discrete(
    labels = c(
      "Age\n18-29",
      "Age\n30-49",
      "Age\n50-69",
      "Age\n70+",
      "Urban",
      "Rural",
      "Women",
      "Men",
      "Left-\nGreen",
      "Social\nDemocrats",
      "Center-\nRight",
      "Populist\nRight"
    ),
    breaks = c(
      "18-29",
      "30-49",
      "50-69",
      "70+",
      "urban",
      "rural",
      "female",
      "male",
      "leftgreen",
      "socialdemocrats",
      "centerright",
      "populistright"
    ),
    expand = c(0, 0.66)
  ) +
  scale_y_continuous(limits = c(-0.05, 0.125), expand = c(0, 0)) +
  guides(color = "none") +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.text.align = 0,
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  ) +
  labs(y = "Sum of coefficients",
       x = NULL,
       shape = NULL)

ggsave(
  "figure3.png",
  width = 9.2,
  height = 3.6,
  dpi = 600,
  bg = "white"
)

ggsave(
  "figure3.pdf",
  width = 9.2,
  height = 3.6,
  dpi = 600,
  device = cairo_pdf
)

# Women (lag 1)
tidy(mlist_subgroup$climate$female$varresult$vox) %>% filter(term == "dr.l1") %>% pull(estimate) %>% round(3)
tidy(mlist_subgroup$climate$female$varresult$vox) %>% filter(term == "dr.l2") %>% pull(estimate) %>% round(3)

#######################################################################
# Appendix: main analysis with varying lag lengths (specificed manually)
#######################################################################

#  Empty lists for loops
granger_list_all <- list()
mlist <- list()
max_lag <- 7

# Loop
for (i in 1:max_lag) {
  dailydata_loop <- dailydata %>%
    filter(measure_media == "n_log",
           measure_public == "salience_rel_filter")
  
  vox <- ts(dailydata_loop$salience_public)
  dr <- ts(dailydata_loop$salience_media)
  ts <- ts.union(vox, dr)
  exo <- dailydata_loop %>%
    transmute(time = 1:n(),
              weekend,
              campaign)
  
  # Get structural break for series
  (sb <-
      strucchange::breakpoints(vox ~ dr, breaks = 1, h = 0.15)$breakpoints)
  
  # If break is detected
  if (!is.na(sb)) {
    exo$sb_dummy <- c(rep(0, sb), rep(1, length(vox) - sb))
    
    # Residuals for stationarity tests
    resid_vox <-
      residuals(lm(vox ~ exo$time + exo$weekend + exo$campaign + exo$sb_dummy))
    resid_dr <-
      residuals(lm(dr ~ exo$time + exo$weekend + exo$campaign + exo$sb_dummy))
    
    # If no break is detected
  } else {
    # Residuals for stationarity tests
    resid_vox <-
      residuals(lm(vox ~ exo$time + exo$weekend + exo$campaign))
    resid_dr <-
      residuals(lm(dr ~ exo$time + exo$weekend + exo$campaign))
  }
  
  # Model
  m <- VAR(ts,
           exogen = exo,
           p = i,
           type = "const")
  
  mlist[[i]] <- m
  # Normalized series for IRF plots
  mlist[[i]] <-
    VAR(
      ts.union(vox = ts(scale(vox)), dr = ts(scale(dr))),
      exogen = exo,
      p = i,
      type = "const"
    )
  names(mlist)[[i]] <- select_measure
  
  # Coefs
  coef_dr_vox <-
    tidy(m$varresult$vox) %>% filter(str_detect(term, "dr")) %>% summarise(sum_of_coef = sum(estimate)) %>% pull(sum_of_coef)
  coef_vox_dr <-
    tidy(m$varresult$dr) %>% filter(str_detect(term, "vox")) %>% summarise(sum_of_coef = sum(estimate)) %>% pull(sum_of_coef)
  
  # Collect output
  
  granger_list_all[[i]] <- bind_rows(
    # Media to public
    bind_cols(
      Issue = "Climate change",
      Measure = select_measure,
      Subgroup = "All",
      `Direction of causality` = "Media-Public",
      `No. of lags` = as.numeric(m$p),
      `Sum of coefficients` = coef_dr_vox,
      # Granger test
      P = causality(m, cause = "dr")$Granger$p.value %>% as.numeric(),
      `R^2` = glance(m$varresult$vox)$r.squared,
      N = as.numeric(m$obs),
      breakpoint = sb,
      p.dw = durbinWatsonTest(m$varresult$vox)$p,
      stationary_pp_vox = pp.test(resid_vox)$p.value,
      stationary_kpss_vox = kpss.test(resid_vox)$p.value,
      stationary_adf_vox = adf.test(resid_vox, k = as.numeric(m$p))$p.value,
      stationary_pp_dr = pp.test(resid_dr)$p.value,
      stationary_kpss_dr = kpss.test(resid_dr)$p.value,
      stationary_adf_dr = adf.test(resid_dr, k = as.numeric(m$p))$p.value
    ),
    
    # Public to media
    bind_cols(
      Issue = "Climate change",
      Measure = select_measure,
      Subgroup = "All",
      `Direction of causality` = "Public-Media",
      `No. of lags` = as.numeric(m$p),
      `Sum of coefficients` = coef_vox_dr,
      # Granger test
      P = causality(m, cause = "vox")$Granger$p.value %>% as.numeric(),
      `R^2` = glance(m$varresult$dr)$r.squared,
      N = as.numeric(m$obs),
      breakpoint = sb,
      p.dw = durbinWatsonTest(m$varresult$dr)$p,
      stationary_pp_vox = pp.test(resid_vox)$p.value,
      stationary_kpss_vox = kpss.test(resid_vox)$p.value,
      stationary_adf_vox = adf.test(resid_vox, k = as.numeric(m$p))$p.value,
      stationary_pp_dr = pp.test(resid_dr)$p.value,
      stationary_kpss_dr = kpss.test(resid_dr)$p.value,
      stationary_adf_dr = adf.test(resid_dr, k = as.numeric(m$p))$p.value
    )
  )
  
}

# Bind
granger_daily_manual_lags <- bind_rows(granger_list_all) %>%
  mutate_if(is_numeric, round, 3) %>%
  print()

#######################################################################
# Appendix: decomposition of time series
#######################################################################

dailydata_loop <- dailydata %>%
  filter(measure_media == "n_log",
         measure_public == "salience_rel_filter")

vox <- ts(dailydata_loop$salience_public)
dr <- ts(dailydata_loop$salience_media)
ts <- ts.union(vox, dr)
exo <- dailydata_loop %>%
  transmute(time = 1:n(),
            weekend,
            campaign)

# Get structural break for series
(sb <-
    strucchange::breakpoints(vox ~ dr, breaks = 1, h = 0.15)$breakpoints)

# If break is detected
if (!is.na(sb)) {
  exo$sb_dummy <- c(rep(0, sb), rep(1, length(vox) - sb))
  
  # Residuals for stationarity tests
  resid_vox <-
    residuals(lm(vox ~ exo$time + exo$weekend + exo$campaign + exo$sb_dummy))
  resid_dr <-
    residuals(lm(dr ~ exo$time + exo$weekend + exo$campaign + exo$sb_dummy))
  
  # If no break is detected
} else {
  # Residuals for stationarity tests
  resid_vox <-
    residuals(lm(vox ~ exo$time + exo$weekend + exo$campaign))
  resid_dr <-
    residuals(lm(dr ~ exo$time + exo$weekend + exo$campaign))
}

tibble(public = resid_vox,
       media = resid_dr) %>%
  mutate(date = dailydata_loop$date,
         series = "filter") %>%
  pivot_longer(c(public, media), names_to = "agenda", values_to = "salience") %>%
  group_by(agenda) %>%
  mutate(salience_std = as.numeric(scale(salience))) %>%
  ungroup() %>%
  pivot_longer(
    cols = c("salience", "salience_std"),
    names_to = "type",
    values_to = "salience"
  ) %>%
  mutate(agenda = factor(agenda)) %>%
  ggplot(aes(
    x = date,
    y = salience,
    color = agenda,
    linetype = agenda
  )) +
  facet_wrap(~ factor(type, labels = c("Unstandardized", "Standardized")), ncol = 1, scales = "free_y") +
  geom_line(size = 0.55) +
  scale_x_date(
    date_labels = "%B",
    limits = c(NA_Date_, ymd("20190606")),
    expand = expansion(add = 4)
  ) +
  scale_y_continuous(
    breaks = seq(-100, 100, 2),
    limits = c(NA, NA),
    expand = c(0, 1)
  ) +
  scale_color_manual(
    values = c("grey20", "grey40"),
    limits = c("public", "media"),
    labels = c("Public salience", "Media salience")
  ) +
  scale_linetype_manual(
    values = c("solid", "longdash"),
    limits = c("public", "media"),
    labels = c("Public salience", "Media salience")
  ) +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    strip.placement = "right",
    strip.background = element_rect(fill = "white", size = 0.8, color =
                                      "white"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2)
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL
  ) +
  geom_hline(aes(yintercept = 0), size = 0.55)

ggsave(
  "appendix_decomposition_residuals.png",
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white"
)

# Generic decomp of raw series
dailydata_loop <- dailydata %>%
  filter(measure_media == "n_log",
         measure_public == "salience_rel_interpol")

decomp_public <- dailydata_loop %>%
  pull(salience_public) %>%
  ts(frequency = 7) %>%
  decompose()

decomp_public_df <- tibble(
  agenda = "public",
  observed = decomp_public$x,
  trend = decomp_public$trend,
  seasonal = decomp_public$seasonal,
  random = decomp_public$random
) %>%
  mutate(date = dailydata_loop$date)

decomp_media <- dailydata_loop %>%
  pull(salience_media) %>%
  ts(frequency = 7) %>%
  decompose()

decomp_media_df <- tibble(
  agenda = "media",
  observed = decomp_media$x,
  trend = decomp_media$trend,
  seasonal = decomp_media$seasonal,
  random = decomp_media$random
) %>%
  mutate(date = dailydata_loop$date)

bind_rows(decomp_public_df, decomp_media_df) %>%
  pivot_longer(
    cols = c(observed, trend, seasonal, random),
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    series = factor(
      series,
      levels = c("observed", "trend", "seasonal", "random"),
      labels = c(
        "Raw series",
        "Trend\n(moving average)",
        "Seasonal component\n(weekly)",
        "Random variance"
      )
    ),
    agenda = factor(
      agenda,
      levels = c("public", "media"),
      labels = c("Public salience", "Media salience")
    )
  ) %>%
  ggplot(aes(x = date, y = value, color = agenda)) +
  geom_line() +
  facet_grid(series ~ agenda, scales = "free_y") +
  scale_color_manual(values = c("grey20", "grey40")) +
  guides(color = "none") +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    strip.placement = "right",
    strip.background = element_rect(fill = "white", size = 0.8, color =
                                      "white"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2)
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL
  ) +
  geom_vline(aes(xintercept = dailydata_loop$date[[sb]]),
             linetype = "dashed",
             size = 0.33) +
  geom_hline(aes(yintercept = 0), size = 0.55)

ggsave(
  "appendix_decomposition_raw.png",
  width = 8,
  height = 7,
  dpi = 600,
  bg = "white"
)

#######################################################################
# Appendix
#######################################################################

# Code to reproduce tables from appendix. Note that kable output format is changed to "html" instead of "latex" (pdf) that was used to produce the original PDF appendix.

## ----stationarity----------------------------------------------------------------------------------------------------------------------------
daily <- granger_daily %>%
  clean_names() %>%
  filter(
    issue == "Climate change",
    direction_of_causality == "Media-Public",
    measure_media == "n_log",
    measure_public == "salience_rel_filter"
  ) %>%
  transmute(
    time_series = "all",
    N = 139,
    breakpoint,
    stationary_adf_vox,
    stationary_kpss_vox,
    stationary_adf_dr,
    stationary_kpss_dr
  )

subgroup <- granger_subgroups %>%
  clean_names() %>%
  filter(issue == "climate", direction_of_causality == "Media-Public") %>%
  transmute(
    time_series = subgroup,
    N = 40,
    breakpoint,
    stationary_adf_vox,
    stationary_kpss_vox,
    stationary_adf_dr,
    stationary_kpss_dr
  )

daily %>%
  bind_rows(subgroup) %>%
  mutate(
    order = case_when(
      str_detect(time_series, "all") ~ 1,
      str_detect(time_series, "18") ~ 2,
      str_detect(time_series, "30") ~ 3,
      str_detect(time_series, "50") ~ 4,
      str_detect(time_series, "70") ~ 5,
      str_detect(time_series, "urban") ~ 6,
      str_detect(time_series, "rural") ~ 7,
      str_detect(time_series, "female") ~ 8,
      str_detect(time_series, "male") ~ 9,
      str_detect(time_series, "green") ~ 10,
      str_detect(time_series, "socialdemocrats") ~ 11,
      str_detect(time_series, "centerright") ~ 12,
      str_detect(time_series, "populist") ~ 13
    ),
    time_series = case_when(
      str_detect(time_series, "all") ~ "Climate change",
      str_detect(time_series, "18") ~ "Age 18-29",
      str_detect(time_series, "30") ~ "Age 30-49",
      str_detect(time_series, "50") ~ "Age 50-69",
      str_detect(time_series, "70") ~ "Age 70+",
      str_detect(time_series, "urban") ~ "Urban community",
      str_detect(time_series, "rural") ~ "Rural community",
      str_detect(time_series, "female") ~ "Women",
      str_detect(time_series, "male") ~ "Men",
      str_detect(time_series, "green") ~ "Left-green parties",
      str_detect(time_series, "socialdemocrats") ~ "Social Democrats",
      str_detect(time_series, "centerright") ~ "Center-right parties",
      str_detect(time_series, "populist") ~ "Populist right parties"
    )
  ) %>%
  arrange(order) %>%
  select(-order) %>%
  mutate_at(vars(4:7), ~ format(round(.x, 3), nsmall = 3)) %>%
  #mutate_at(vars(contains("adf"), contains("pp")), ~cell_spec(.x, "latex", color = ifelse(.x < 0.05, "black", "red"), bold = ifelse(.x < 0.05, FALSE, TRUE))) %>%
  #mutate_at(vars(contains("kpss")), ~cell_spec(.x, "latex", color = ifelse(.x >= 0.05, "black", "red"), bold = ifelse(.x >= 0.05, FALSE, TRUE))) %>%
  kable(
    format = "html",
    caption = "Time series diagnostics: Structural break detection and stationarity test p-values.",
    digits = 3,
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    escape = FALSE,
    align = c("l", rep("c", 6)),
    col.names = c(
      "Time series",
      "N",
      "Structural break",
      "ADF test",
      "KPSS test",
      "ADF test",
      "KPSS test"
    )
  ) %>%
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "HOLD_position"),
    repeat_header_continued = "\\textit{(Continued on next page...)}"
  ) %>%
  add_header_above(
    c(
      " " = 3,
      "Public salience series" = 2,
      "Media salience series" = 2
    ),
    escape = FALSE,
    bold = FALSE,
    font_size = 9,
    line = TRUE,
    line_sep = 5,
    align = "c"
  ) %>%
  column_spec(1, width = "4.5cm") %>%
  pack_rows("Full sample (daily)", 1, 1) %>%
  pack_rows("Subgroups (semiweekly)", 2, 13) %>%
  footnote(
    "Note: P-values for ADF and KPSS tests of covariance stationarity for each time series, i.e., after controlling for time, a structural break, and for the daily series also weekends and the election campaign. For the ADF test, lag orders are selected using AIC. The media series is the same for all subgroups, but results may vary beause of differences in identified structrual break for the media-subgroup pair. Note that the tests are performed using the tseries R package which, for the ADF test, shows (highly stationary) p-values below 0.010 as 0.010 and, for the KPSS test, shows (highly stationary) p-values above 0.100 as 0.100.",
    threeparttable = TRUE,
    general_title = ""
  )

## ----varsubgroups----------------------------------------------------------------------------------------------------------------------------
granger_subgroups %>%
  filter(Issue == "climate", Subgroup != "All") %>%
  select(Subgroup,
         `Direction of causality`,
         N,
         `No. of lags`,
         `Sum of coefficients`,
         P,
         `R^2`) %>%
  pivot_longer(c(`Sum of coefficients`, P, `R^2`)) %>%
  pivot_wider(
    names_from = c("Direction of causality", "name"),
    values_from = c(value)
  ) %>%
  clean_names() %>%
  mutate(
    order = case_when(
      str_detect(subgroup, "18-29") ~ 1,
      str_detect(subgroup, "30-49") ~ 2,
      str_detect(subgroup, "50-69") ~ 3,
      str_detect(subgroup, "70") ~ 4,
      str_detect(subgroup, "urban") ~ 5,
      str_detect(subgroup, "rural") ~ 6,
      str_detect(subgroup, "female") ~ 7,
      str_detect(subgroup, "male") ~ 8,
      str_detect(subgroup, "green") ~ 9,
      str_detect(subgroup, "socialdemocrats") ~ 10,
      str_detect(subgroup, "centerright") ~ 11,
      str_detect(subgroup, "populist") ~ 12
    ),
    subgroup_out = case_when(
      str_detect(subgroup, "18-29") ~ "Age 18-29",
      str_detect(subgroup, "30-49") ~ "Age 30-49",
      str_detect(subgroup, "50-69") ~ "Age 50-69",
      str_detect(subgroup, "70") ~ "Age 70+",
      str_detect(subgroup, "urban") ~ "Urban community",
      str_detect(subgroup, "rural") ~ "Rural community",
      str_detect(subgroup, "female") ~ "Women",
      str_detect(subgroup, "male") ~ "Men",
      str_detect(subgroup, "green") ~ "Left-green parties",
      str_detect(subgroup, "socialdemocrats") ~ "Social Democrats",
      str_detect(subgroup, "centerright") ~ "Center-right parties",
      str_detect(subgroup, "populist") ~ "Populist right parties"
    )
  ) %>%
  arrange(order) %>%
  mutate_at(
    vars(
      media_public_sum_of_coefficients,
      media_public_p,
      media_public_r_2,
      public_media_sum_of_coefficients,
      public_media_p,
      public_media_r_2
    ),
    ~ format(round(.x, 2), nsmall = 2)
  ) %>%
  select(
    subgroup_out,
    n,
    no_of_lags,
    media_public_sum_of_coefficients,
    media_public_p,
    media_public_r_2,
    public_media_sum_of_coefficients,
    public_media_p,
    public_media_r_2
  ) %>%
  kable(
    format = "html",
    caption = "Full output of the subgroup VAR models.",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    escape = FALSE,
    align = c("l", rep("c", 7)),
    col.names = c(
      "Subgroup",
      "N",
      "No. of lags",
      linebreak("Sum of coefficients", align = "c"),
      "P",
      "R^2",
      linebreak("Sum of coefficients", align = "c"),
      "P",
      "R^2"
    )
  ) %>%
  kable_styling(font_size = 10,
                latex_options = c("repeat_header", "HOLD_position")) %>%
  add_header_above(
    c(
      " " = 3,
      "Media  \u2192  Public" = 3,
      "Public  \u2192  Media" = 3
    ),
    escape = FALSE,
    bold = FALSE,
    font_size = 9,
    line = TRUE,
    line_sep = 5,
    align = "c"
  ) %>%
  column_spec(1, width = "4.5cm") %>%
  footnote(
    "Note: The 12 subgroup VAR models include lags of semiweekly public salience series (computed for each group) and media salience series (reused across models), a deterministic time trend, and a structural break dummy variable. All series are covariance stationary according to the KPSS stationarity tests (see Appendix B). Lag orders are selected with AIC (max. 4 halfweeks).",
    threeparttable = TRUE,
    general_title = ""
  )

## ----varrob----------------------------------------------------------------------------------------------------------------------------------
alternative_media <- bind_rows(granger_daily) %>%
  select(
    Measure_media,
    Measure_public,
    Subgroup,
    `Direction of causality`,
    N,
    `No. of lags`,
    `Sum of coefficients`,
    P,
    `R^2`
  ) %>%
  pivot_longer(c(`Sum of coefficients`, P, `R^2`)) %>%
  pivot_wider(
    names_from = c("Direction of causality", "name"),
    values_from = c(value)
  ) %>%
  clean_names() %>%
  filter(measure_public == "salience_rel_filter") %>%
  mutate(measure = measure_media)

alternative_public <- granger_daily %>%
  filter(
    Measure_media == "n_log",
    Measure_public %in% c(
      "salience_rel_filter",
      "salience_rel_interpol",
      "salience_abs_filter"
    )
  ) %>%
  mutate(Measure = case_when(
    str_detect(Measure_public, "salience_rel_filter") ~ "relative",
    str_detect(Measure_public, "salience_abs_filter") ~ "absolute",
    str_detect(Measure_public, "salience_rel_interpol") ~ "raw"
  )) %>%
  select(
    Measure,
    Subgroup,
    `Direction of causality`,
    N,
    `No. of lags`,
    `Sum of coefficients`,
    P,
    `R^2`
  ) %>%
  pivot_longer(c(`Sum of coefficients`, P, `R^2`)) %>%
  pivot_wider(names_from = c("Direction of causality", "name"),
              values_from = value) %>%
  clean_names()

bind_rows(alternative_media,
          alternative_public) %>%
  mutate(
    order = case_when(
      measure == "n_log" ~ 1,
      measure == "n" ~ 2,
      measure == "n_sqrt" ~ 3,
      measure == "n_articles" ~ 4,
      measure == "relative" ~ 5,
      measure == "absolute" ~ 6,
      measure == "raw" ~ 7
    ),
    measure_out = case_when(
      measure == "n_log" ~ "Natural log (default)",
      measure == "n" ~ "Raw keywords (a)",
      measure == "n_sqrt" ~ "Square root (b)",
      measure == "n_articles" ~ "Climate articles (c)",
      measure == "relative" ~ "Relative, Kalman-filtered (default)",
      measure == "absolute" ~ "Absolute, Kalman-filtered (d)",
      measure == "raw" ~ "Relative, raw and interpolated (e)"
    )
  ) %>%
  arrange(order) %>%
  select(
    measure_out,
    n,
    no_of_lags,
    media_public_sum_of_coefficients,
    media_public_p,
    media_public_r_2,
    public_media_sum_of_coefficients,
    public_media_p,
    public_media_r_2
  ) %>%
  kable(
    format = "html",
    caption = "Main VAR model with alternative measures of media salience and public salience.",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    escape = FALSE,
    align = c("l", rep("c", 7)),
    col.names = c(
      "Alternative measure",
      "N",
      "No. of lags",
      linebreak("Sum of coefficients", align = "c"),
      "P",
      "R^2",
      linebreak("Sum of coefficients", align = "c"),
      "P",
      "R^2"
    )
  ) %>%
  kable_styling(font_size = 10,
                latex_options = c("repeat_header", "HOLD_position")) %>%
  add_header_above(
    c(
      " " = 3,
      "Media  \u2192  Public" = 3,
      "Public  \u2192  Media" = 3
    ),
    escape = FALSE,
    bold = FALSE,
    font_size = 9,
    line = TRUE,
    line_sep = 5,
    align = "c"
  ) %>%
  pack_rows("Media salience", 1, 4) %>%
  pack_rows("Public salience", 5, 7) %>%
  column_spec(1, width = "5cm") %>%
  footnote(
    "Note: Each model includes lags of the media and public salience series, a deterministic time trend, and dummies for a structural break, weekends, and the election campaign. Lag order is selected with AIC (max. 7 days).",
    threeparttable = TRUE,
    general_title = ""
  )

## ----varlags---------------------------------------------------------------------------------------------------------------------------------
granger_daily_manual_lags %>%
  select(
    Measure,
    Subgroup,
    `Direction of causality`,
    N,
    `No. of lags`,
    `Sum of coefficients`,
    P,
    `R^2`
  ) %>%
  pivot_longer(c(`Sum of coefficients`, P, `R^2`)) %>%
  pivot_wider(
    names_from = c("Direction of causality", "name"),
    values_from = c(value)
  ) %>%
  clean_names() %>%
  arrange(no_of_lags) %>%
  mutate_at(
    vars(
      media_public_sum_of_coefficients,
      media_public_p,
      media_public_r_2,
      public_media_sum_of_coefficients,
      public_media_p,
      public_media_r_2
    ),
    ~ format(round(.x, 2), nsmall = 2)
  ) %>%
  mutate(subgroup_out = ifelse(row_number() == 1, "Full sample", "")) %>%
  select(
    subgroup_out,
    n,
    no_of_lags,
    media_public_sum_of_coefficients,
    media_public_p,
    media_public_r_2,
    public_media_sum_of_coefficients,
    public_media_p,
    public_media_r_2
  ) %>%
  kable(
    format = "html",
    caption = "Main VAR model with varying lag orders.",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    escape = FALSE,
    align = c("l", rep("c", 8)),
    col.names = c(
      "Subgroup",
      "N",
      "No. of lags",
      linebreak("Sum of coefficients", align = "c"),
      "P",
      "R^2",
      linebreak("Sum of coefficients", align = "c"),
      "P",
      "R^2"
    )
  ) %>%
  kable_styling(font_size = 9,
                latex_options = c("repeat_header", "HOLD_position")) %>%
  add_header_above(
    c(
      " " = 3,
      "Media  \u2192  Public" = 3,
      "Public  \u2192  Media" = 3
    ),
    escape = FALSE,
    bold = FALSE,
    font_size = 9,
    line = TRUE,
    line_sep = 5,
    align = "c"
  ) %>%
  column_spec(1, width = "4.5cm") %>%
  footnote(
    "Note: Each version of the VAR model uses a different lag order and includes lags of the media and public salience series, a deterministic time trend, and dummies for a structural break, weekends, and the election campaign.",
    threeparttable = TRUE,
    general_title = ""
  )

## ----varsubgroupalt--------------------------------------------------------------------------------------------------------------------------
granger_subgroups %>%
  filter(Issue %in% c("education", "immigration"), Subgroup != "All") %>%
  select(
    Issue,
    Subgroup,
    `Direction of causality`,
    N,
    `No. of lags`,
    `Sum of coefficients`,
    P,
    `R^2`
  ) %>%
  pivot_longer(c(`Sum of coefficients`, P, `R^2`)) %>%
  pivot_wider(
    names_from = c("Direction of causality", "name"),
    values_from = c(value)
  ) %>%
  clean_names() %>%
  mutate(
    Issue = toupper(issue),
    order = case_when(
      str_detect(subgroup, "18-29") ~ 1,
      str_detect(subgroup, "30-49") ~ 2,
      str_detect(subgroup, "50-69") ~ 3,
      str_detect(subgroup, "70") ~ 4,
      str_detect(subgroup, "urban") ~ 5,
      str_detect(subgroup, "rural") ~ 6,
      str_detect(subgroup, "female") ~ 7,
      str_detect(subgroup, "male") ~ 8,
      str_detect(subgroup, "green") ~ 9,
      str_detect(subgroup, "socialdemocrats") ~ 10,
      str_detect(subgroup, "centerright") ~ 11,
      str_detect(subgroup, "populist") ~ 12
    ),
    subgroup_out = case_when(
      str_detect(subgroup, "18-29") ~ "Age 18-29",
      str_detect(subgroup, "30-49") ~ "Age 30-49",
      str_detect(subgroup, "50-69") ~ "Age 50-69",
      str_detect(subgroup, "70") ~ "Age 70+",
      str_detect(subgroup, "urban") ~ "Urban community",
      str_detect(subgroup, "rural") ~ "Rural community",
      str_detect(subgroup, "female") ~ "Women",
      str_detect(subgroup, "male") ~ "Men",
      str_detect(subgroup, "green") ~ "Left-green parties",
      str_detect(subgroup, "socialdemocrats") ~ "Social Democrats",
      str_detect(subgroup, "centerright") ~ "Center-right parties",
      str_detect(subgroup, "populist") ~ "Populist right parties"
    )
  ) %>%
  arrange(Issue, order) %>%
  mutate_at(
    vars(
      media_public_sum_of_coefficients,
      media_public_p,
      media_public_r_2,
      public_media_sum_of_coefficients,
      public_media_p,
      public_media_r_2
    ),
    ~ format(round(.x, 2), nsmall = 2)
  ) %>%
  select(
    subgroup_out,
    n,
    no_of_lags,
    media_public_sum_of_coefficients,
    media_public_p,
    media_public_r_2,
    public_media_sum_of_coefficients,
    public_media_p,
    public_media_r_2
  ) %>%
  kable(
    format = "html",
    caption = "Full output of the subgroup VAR models for education and immigration.",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    escape = FALSE,
    align = c("l", rep("c", 7)),
    col.names = c(
      "Subgroup",
      "N",
      "No. of lags",
      linebreak("Sum of coefficients", align = "c"),
      "P",
      "R^2",
      linebreak("Sum of coefficients", align = "c"),
      "P",
      "R^2"
    )
  ) %>%
  kable_styling(
    font_size = 9,
    latex_options = c("repeat_header", "HOLD_position"),
    repeat_header_continued = "\\textit{(Continued on next page...)}"
  ) %>%
  add_header_above(
    c(
      " " = 3,
      "Media  \u2192  Public" = 3,
      "Public  \u2192  Media" = 3
    ),
    escape = FALSE,
    bold = FALSE,
    font_size = 9,
    line = TRUE,
    line_sep = 5,
    align = "c"
  ) %>%
  pack_rows("Education", 1, 12) %>%
  pack_rows("Immigration", 13, 12 * 2) %>%
  column_spec(1, width = "4.5cm") %>%
  footnote(
    "Note: The two times 12 subgroup VAR models include lags of semiweekly public salience series (computed for each group) and media salience series (reused in all models of the issue), a deterministic time trend, and a structural break dummy. All series are covariance stationary according to the KPSS stationarity tests (see Appendix B). Lag orders are selected with AIC (max. 4 halfweeks).",
    threeparttable = TRUE,
    general_title = ""
  )