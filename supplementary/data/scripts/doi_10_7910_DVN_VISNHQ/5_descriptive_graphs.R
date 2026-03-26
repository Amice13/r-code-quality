##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Produce Figures on Overall Telework Trends 
# (Figure 1, A1, A2, A3, A4)
# 
##################################################################################
# Clear environment
rm(list = ls())

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

library(tidyverse)
library(janitor)
library(purrr)
library(ggrepel)
library(scales)
library(usmap)
library(sf)
library(purrr)
library(patchwork)
library(readxl)
library(lubridate)
library(ggthemes)
library(ggpattern)


# Load data ---------------------------------------------------------------

# Read in the codebook
ddi <- read_ipums_ddi("data/input_raw_ipums/usa_00027.xml")
# Read in the data and clean the variable names to be all lowercase
data <- read_ipums_micro(ddi) |> clean_names()


sample_data <-  data |> 
  # general restrictions
  filter(
    # non-institutionalized
    gq != 3,  
    # 50 states + DC
    statefip <= 56,
    # non-farm
    farm == 1
    ) 


xwalk <- read_csv("data/xwalk/irs_soi_fips_crosswalk.csv") |> 
  clean_names() |> 
  rename(fips = fips_code, full = state_name) |> 
  select(full, fips)


# Read in cleaned ACS + taxsim output data for all tax unit ---------------------------

# already non-institutionalized, non-farm, 50 states + DC
cleaned_all <- read_csv("data/derived_ipums/cleaned_all_wATR.csv")


# General cleaning --------------------------------------------------------
## add essential dummies ---------------------------------------------------

# add a dummy for identifying teleowrk tax units
# add dummys for interstate move & overall move
cleaned_all_4plot <- cleaned_all |> 
  mutate(
    # add dummy for if anyone in the couple telework
    any_telework = case_when(
      # married: true if you or spouse telework
      (marst == 1) & ((p1tranwork == 80 | p2tranwork == 80)) ~ TRUE,
      # not married: true if you telework
      (marst != 1) & (p1tranwork == 80) ~ TRUE,
      # if not married: missing if tranwork missing
      (marst != 1) & (p1tranwork == 0) ~ NA,
      
      # otherwise false
      .default = FALSE
    ),
    
    
    # construct federal AGI income bins
    real_fagi_bins = cut(
      real_fagi,
      breaks = c(25000, 50000, 75000, 100000, 250000, 500000, Inf),
      right  = FALSE,
      labels = c(
        "[$25k-$50k)",
        "[$50k-$75k)",
        "[$75k-$100k)",
        "[$100k-$250k)",
        "[$250k-$500k)",
        "[$500k+)"
      )
    )
   
  )

# Share of teleworkers ----------------------------------------------------

share_telework_yr <- sample_data |> 
  filter(empstat == 1, farm == 1) |> 
  mutate(
    nonfarm_tele = (tranwork == 80)
  ) |> 
  group_by(year) |> 
  summarise(prop_tele = weighted.mean(nonfarm_tele, perwt, na.rm = TRUE))


# Share of teleworking HHs in our data ----------------------------------------------------

share_telework_hh_yr <- cleaned_all_4plot |> 
  group_by(year) |> 
  summarise(prop_tele_hh = weighted.mean(any_telework, hhwt, na.rm = TRUE))

# Job postings by year------------------------------------------------------------

job_posting <- read_excel(
  "data/takeup/remote_work_in_job_ads_signup_data-1.xlsx",
  sheet = "country_by_month"
) |> 
  clean_names()

cleaned_job_posting <- job_posting |> 
  filter(country == "USA", year < 2025) |> 
  mutate(
    date = as.Date(paste(year, month, "01"), format = "%Y %b %d"),
    percent = percent / 100
  ) 

quarter_breaks <- cleaned_job_posting |>
  mutate(q_date = floor_date(date, unit = "quarter")) |>
  distinct(q_date) |>
  pull(q_date)

job_posting_annotations <- cleaned_job_posting |> 
  filter(date %in% c("2020-03-01", "2020-04-01", "2021-01-01", "2022-10-01", "2024-12-01"))


share_job_postings_yr <- cleaned_job_posting |> 
  group_by(year) |> 
  summarise(
    avg_percent = mean(percent)
  ) 


# Figure 1: Overlay Job postings by year with share of Teleworkers by year---------------------------------------------------

share_yr_combo <- share_telework_yr |> 
  left_join(share_telework_hh_yr) |> 
  full_join(share_job_postings_yr) |>
  pivot_longer(
    cols = c(prop_tele, prop_tele_hh, avg_percent),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = recode(series,
                    prop_tele = "Teleworkers",
                    prop_tele_hh = "Telework HHs",
                    avg_percent   = "Telework Job Postings"),
    series = factor(series,
                    levels = c("Teleworkers", "Telework HHs", "Telework Job Postings"))
  ) 


telework_trend_overlay <- share_yr_combo |> 
  ggplot(aes(x = year, y = value, shape = series, linetype = series)) +
  geom_point(size = 4) + 
  geom_line(linewidth = 1.5) + 
  #scale_colour_stata("s2color") +
  scale_x_continuous(name = NULL, 
                     breaks = seq(2016, 2024, by = 1), 
                     guide = guide_axis(angle = 45)) +
  scale_y_continuous(name = NULL, 
                     labels = label_percent(),
                     limits = c(0.02, 0.19),
                     breaks = seq(0, 0.2, by = 0.02)) +
  labs(shape = NULL, linetype = NULL, color = NULL) +
  guides(
    shape = guide_legend(nrow = 2),
    #color = guide_legend(nrow = 2),
    linetype = guide_legend(nrow = 2)
  ) +
  theme_bw()  +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    legend.title = NULL,
    legend.text = element_text(size = 18),
    legend.key.width = unit(2.5, "cm"),
    legend.position = "bottom"
  ) 

ggsave("output/figure_1.png", telework_trend_overlay, width = 8, height = 6)
ggsave("output/figure_1.tiff", telework_trend_overlay, width = 8, height = 6, dpi = 300)


#Figure A1: Share of teleworkers by state ----------------------------------------------------

share_telework_by_state <- sample_data |> 
  filter(empstat == 1, farm == 1) |> 
  mutate(
    nonfarm_tele = (tranwork == 80 & farm == 1)
  ) |> 
  group_by(year, statefip) |> 
  summarise(prop_tele = weighted.mean(nonfarm_tele, perwt, na.rm = TRUE)*100) |> 
  rename(fips = statefip)

# merge in full state names
share_telework_by_state <- share_telework_by_state |> 
  left_join(xwalk, by = "fips")

# get usmap coordinates for each state
state_map <- usmap::us_map("states")

# compute centroids
state_centers <- state_map  %>% 
  st_centroid()  %>%
  cbind(st_coordinates(.))  %>% 
  as_tibble()  |> 
  bind_cols(state_map  |>  st_drop_geometry() |>  select(abbr)) |> 
  select(-abbr...7) |> 
  rename(
    abbr = abbr...2
  )

# merge in coordinates
m_dta_labeled <- share_telework_by_state  |> 
  left_join(state_centers, by = c("full"))

# construct binned shares for each year
prep_year_data <- function(y) {
    df <- m_dta_labeled |> 
      filter(year == y) |> 
      mutate(
        # compute year-specific cutoffs (quantiles)
        prop_bins = cut(
          prop_tele,
          breaks = quantile(prop_tele, probs = seq(0, 1, length.out = 6), na.rm = TRUE),
          include.lowest = TRUE,
          dig.lab = 1
        ),
        prop_bins = forcats::fct_relabel(prop_bins, ~ gsub(",", ", ", .x, fixed = TRUE))
      ) 
    
    df
}


yr19 <- prep_year_data(2019)
yr24 <- prep_year_data(2024)


# choose small or overlapping states manually
repel_states <- c("RI", "CT", "NJ", "DE", "MD", "MA", "DC", "VT", "NH")

# dplit the data into two groups
labels_repel <- m_dta_labeled |> filter(abbr %in% repel_states)
labels_regular <- m_dta_labeled |> filter(!abbr %in% repel_states)


## [a] 2019 --------------------------------------------------------------------

p2_19 <- ggplot() +
  geom_sf(data = state_map, fill = NA, color = "black") +
  geom_sf(data = left_join(state_map, yr19, by = "full"),
          aes(fill = prop_bins)) +
  scale_fill_brewer(palette = "Oranges", direction =1, name = "Telework Share (%)") +
  # Regular labels (centered)
  geom_text(
    data = labels_regular |> filter(year == 2019),
    aes(x = X, y = Y, label = paste0(round(prop_tele, 0), "%")),
    color = "black", size = 6
  ) +
  # Repelled labels to the right
  geom_text_repel(
    data = labels_repel |> filter(year == 2019),
    aes(x = X, y = Y, label = paste0(round(prop_tele, 0), "%")),
    nudge_x = 6e5,         # same as offset above
    #hjust = 0,             # align text to left
    direction = "x",       # only repel along the x-axis
    segment.color = "gray50",
    point.padding = 0.2,
    max.overlaps = Inf,
    force = 5,                # controls repulsion strength
    force_pull = 0.5,         
    size = 6,
    min.segment.length = 0
  ) +
  theme_void() + 
  theme(
    legend.position = c(1.03, 0.05),  
    legend.justification = c("right", "bottom"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 1.1),
    panel.border     = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title = "(a) 2019")


## [b] 2024 --------------------------------------------------------------------

p2_24 <- ggplot() +
  geom_sf(data = state_map, fill = NA, color = "black") +
  geom_sf(data = left_join(state_map, yr24, by = "full"),
          aes(fill = prop_bins)) +
  scale_fill_brewer(palette = "Blues", direction =1, name = "Telework Share (%)") +
  # Regular labels (centered)
  geom_text(
    data = labels_regular |> filter(year == 2024),
    aes(x = X, y = Y, label = paste0(round(prop_tele, 0), "%")),
    color = "black", size = 6
  ) +
  # Repelled labels to the right
  geom_text_repel(
    data = labels_repel |> filter(year == 2024),
    aes(x = X, y = Y, label = paste0(round(prop_tele, 0), "%")),
    nudge_x = 6e5,         # same as offset above
    #hjust = 0,             # align text to left
    direction = "x",       # only repel along the x-axis
    segment.color = "gray50",
    point.padding = 0.2,
    max.overlaps = Inf,
    force = 5,                # controls repulsion strength
    force_pull = 0.5,         
    size = 6,
    min.segment.length = 0
  ) +
  theme_void() + 
  theme(
    legend.position = c(1.03, 0.05),  
    legend.justification = c("right", "bottom"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 1.1),
    panel.border     = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title = "(b) 2024")


combined_tele_state <- p2_19 + p2_24 +
  plot_layout(ncol = 2) &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.margin = margin(t = 5, r = 15, b = 5, l = 5)
  ) 

ggsave("output/figure_A1.png", combined_tele_state, width = 27, height = 6.5, dpi = 300)
ggsave("output/figure_A1.tiff", combined_tele_state, width = 27, height = 6.5, dpi = 300)


#Figure A2: Descriptive graphs on the decoupling of residence/employment --------

# Add a dummy for whether state of work is not the same as state of residence
diff_state <- sample_data |>
  filter(pwstate2 >= 1 & pwstate2 <=56 & statefip >=1 & statefip <= 56) |> # subset to 50 states + DC
  filter(
    empstat == 1 # people who are employed
  ) |> 
  mutate(
    # Some differences between the value labels for pwstate2 and statefip, but no difference for DC + 50 states
    # use as.factor() to discipline
    diff_state = ifelse(as.factor(pwstate2) != as.factor(statefip), 1, 0),
  ) 

# Create another sample with no telework
diff_state_no_telework <- diff_state |> filter(tranwork != 80)

# Write a function to compute diff. stats related to the decoupling of residence/employment
process_diff_state_rates <- function(df) {
  df |>
    group_by(year) |>
    summarise(
      n_tot = sum(perwt), # weighted total number of ppl in the sample
      n_diff_state = sum(diff_state * perwt), # weighted total number of ppl with different work state than residence state
      pct_diff_state = n_diff_state / n_tot  # weighted fraction 
    ) 
}

processed_diff_state <- process_diff_state_rates(diff_state) |> 
  mutate(sample = "All Employed")
processed_diff_state_no_telework <- process_diff_state_rates(diff_state_no_telework) |> 
  mutate(sample = "Excluding Teleworkers")

# Combine both samples
combined_diff_state <- bind_rows(
  processed_diff_state,
  processed_diff_state_no_telework
)

# Plot the share of workers with a different state of work than residence for the two samples
p_interstate_commuters <- combined_diff_state |> 
  ggplot(aes(x = year, y = pct_diff_state, shape = sample, linetype = sample)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_x_continuous(name = NULL, 
                     breaks = seq(2016, 2024, by = 1), 
                     guide = guide_axis(angle = 45)) +
  scale_y_continuous(
    name = NULL,
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(0.028, 0.04),
    breaks = seq(0.028, 0.04, by = 0.002)
  ) +
  geom_text_repel(data = combined_diff_state,
                  aes(label = scales::percent(pct_diff_state, accuracy = 0.01)),
                  size = 5.5, vjust = -1, hjust = 0.5,
                  show.legend = FALSE) +
  theme_bw() +
  labs(shape = NULL, linetype = NULL, color = NULL) +
  theme_bw()  +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    legend.title = NULL,
    legend.text = element_text(size = 18),
    legend.key.width = unit(2.5, "cm"),
    legend.position = "bottom"
  ) 

ggsave("output/figure_A2.png", p_interstate_commuters, width = 8, height = 6)
ggsave("output/figure_A2.tiff", p_interstate_commuters, width = 8, height = 6, dpi = 300)


# Figure A3 ---------------------------------------------------------------

# a) Number of telework HH in each income bin --------------------------------
num_hh <- cleaned_all_4plot |> 
  filter(p1empstat==1 | p2empstat==1) |> 
  group_by(year, real_fagi_bins) |> 
  summarise(
    n_hh = sum(hhwt)
  ) |> 
  filter(year %in% c(2019, 2024), !is.na(real_fagi_bins)) |> 
  mutate(
    year = factor(year, levels = c("2019", "2024"))
  )


trend_incbin_n_hh <- num_hh |> 
  ggplot(aes(x = n_hh, y = real_fagi_bins, 
             fill = as.factor(year))) +
  geom_col(
    color = "black",            # outline
    linewidth = 1,            # outline thickness
    position = position_dodge(reverse = TRUE)
  ) +
  scale_fill_manual(values = c("white", "black"))+
  scale_x_continuous(
    name = "Number of HHs",
    labels = label_number(scale = 1/1000000, suffix = "M"),
  ) +
  scale_y_discrete(
    name = "AGI Bins"
  ) +
  labs(fill = NULL, pattern = NULL) +
  theme_bw()  +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    axis.title.y  = element_text(size = 18),
    axis.title.x  = element_text(size = 18),
    legend.title = NULL,
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.5, "cm"),
    legend.position = "bottom"
  ) +
  labs(
    title = "(a) Number of HHs"
  )


# b) Share of teleworking HHs by income group in our data ----------------------------------------------------

share_telework_hh_incbin <- cleaned_all_4plot |> 
  group_by(year, real_fagi_bins) |> 
  summarise(
    prop_tele_hh = weighted.mean(any_telework, hhwt, na.rm = TRUE),
  ) |> 
  filter(year %in% c(2019, 2024), !is.na(real_fagi_bins)) |> 
  mutate(
    year = factor(year, levels = c("2019", "2024"))
  )


trend_tele_incbin <- share_telework_hh_incbin |> 
  ggplot(aes(x = prop_tele_hh, y = real_fagi_bins, 
             fill = as.factor(year))) +
  geom_col(
    color = "black",            # outline
    linewidth = 1,            # outline thickness
    position = position_dodge(reverse = TRUE)
  ) +
  scale_fill_manual(values = c("white", "black"))+
  scale_x_continuous(
    name = "Share of Telework HHs",
    labels = label_percent(),
    limits = c(0, 0.36),
    breaks = seq(0, 0.35, 0.05)
  ) +
  scale_y_discrete(
    name = "AGI Bins"
  ) +
  labs(fill = NULL, pattern = NULL) +
  theme_bw()  +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    axis.title.y  = element_text(size = 18),
    axis.title.x  = element_text(size = 18),
    legend.title = NULL,
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.5, "cm"),
    legend.position = "bottom"
  ) +
  labs(
    title = "(b) Share of Telework HHs"
  )


combined_figA3 <- trend_incbin_n_hh + trend_tele_incbin +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "bottom"
  ) 

ggsave("output/figure_A3.png", combined_figA3, width = 16, height = 6)
ggsave("output/figure_A3.tiff", combined_figA3, width = 16, height = 6, dpi = 300)


# Migration rates ---------------------------------------------------------
## Construct different samples ---------------------------------------------

# 1. All interstate movers
all_movers <- sample_data |> 
  filter(migrate1 !=0 & migrate1 != 4) # filter out people who were abroad 1 year ago or migrate1 status NA


# sample 1: (employed) non-teleworkers
nonteleworkers <-  all_movers |> 
  filter(empstat == 1, tranwork != 80)

# sample 2: teleworkers
teleworkers <- all_movers |> filter(tranwork == 80) 


# 4. Retirees
retirees <- all_movers |> 
  # Count people with positive retirement income (dropping missings) and not in the labor force, >age 64 as retirees
  filter(
    incretir > 0 & incretir != 999999 & empstat ==3,
    age > 64
  )


## Compute migration rates -------------------------------------------------

# Write a function to compute migration rates 
compute_migration_rates <- function(df) {
  df |> 
    filter(migrate1!=0) |> #drop N/As
    group_by(year) |> 
    count(migrate1, wt = perwt) |> # weight the counts by person weight
    mutate(
      sum_n = sum(n), # total population of the year
      pct = n / sum_n # compute the percentage of movers
    ) 
}

# Compute migration rates on different samples and add sample identifiers
nonteleworkers_rates <- compute_migration_rates(nonteleworkers) |> 
  mutate(sample = "Non-Teleworkers")

teleworkers_rates <- compute_migration_rates(teleworkers) |>
  mutate(sample = "Teleworkers")

retirees_rates <- compute_migration_rates(retirees) |>
  mutate(sample = "Retirees")


# Combine all migration rates into one data frame
migration_rates <- bind_rows(
  nonteleworkers_rates,
  teleworkers_rates,
  retirees_rates
) |> 
  mutate(
    sample = factor(sample,
                    levels = c("Non-Teleworkers", "Teleworkers", "Retirees"))
  )

# Split into within-state and interstate migration rate dataframes
within_state_rates <- migration_rates |> 
  filter(migrate1 == 2)

interstate_rates <- migration_rates |>
  filter(migrate1 == 3)

# Figure A4: Interstate migration rates --------------------------------------
p_interstate_mig <- interstate_rates |>
  ggplot(aes(x = year, y = pct, shape = sample, linetype = sample)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  scale_y_continuous(
    name = NULL,
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(0.01, 0.05),
    breaks = seq(0.01, 0.05, by = 0.005)
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = seq(2016, 2024, by = 1),
    guide = guide_axis(angle = 45)
  ) +
  geom_text(data = interstate_rates,
                  aes(label = scales::percent(pct, accuracy = 0.01)),
                  size = 6, vjust = -1, hjust = 0.5,
            show.legend = FALSE) +
  labs(
    shape = NULL,
    linetype = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.key.width = unit(2, "cm"),
    legend.position = "bottom"
  ) 

ggsave("output/figure_A4.png", p_interstate_mig, width = 8, height = 6)
ggsave("output/figure_A4.tiff", p_interstate_mig, width = 8, height = 6, dpi = 300)


