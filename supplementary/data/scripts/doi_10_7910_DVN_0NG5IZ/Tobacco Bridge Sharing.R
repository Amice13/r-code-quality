#### Tobacco Sharing ####
library("Hmisc")
library("tidyverse")
library("lubridate")
library("childsds")
library("tidytext")
library("readxl")

#manually makes a mode-estimating function
modeest <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### Datasets Involved ###
# 1. new_tob_share
# 2. clin_monthly
# 3. monthly_mean_share

#### Set Up ####
find <- 'your file here/'
# Read in new_tob_share dataset #
new_tob_share <- read_csv(paste0(find, "new_tob_share.csv"))
new_tob_share <- new_tob_share %>% dplyr::select(-...1)
# % smokers screened #
new_tob_share <- new_tob_share %>% mutate(month_start = as.Date(month_start, "%d%b%Y"))
perc_smokers <- new_tob_share %>% 
  group_by(delivery_site_id_masked, month_start) %>%
  summarise(perc_smokers_screened = tobac_cessation_denom_uds/utd_tobac_screen_uds*100) %>%
  mutate(year = year(month_start))
perc_smokers_yr <- perc_smokers %>% 
  group_by(year) %>%
  summarise(perc_smokers_screened1 = mean(perc_smokers_screened, na.rm = TRUE))
# RATES #
rates <- new_tob_share %>% 
  group_by(delivery_site_id_masked, month_start) %>%
  mutate(rate_screened = tobacco_patients_screened/tob_denominator_study*1000,
         rate_cessation = tobacco_patients_counseled/smoking_denominator_study*1000,
         rate_rx = tobac_rx_start_patients/smoking_denominator_study*1000,
         time_group = ifelse(month_start >= "2019-01-01" & month_start <= "2020-02-01", 1, 
                             ifelse(month_start >= "2020-03-01" & month_start <= "2020-05-01", 2, 3))) %>%
  group_by(month_start) %>%
  summarise(rate_screened = mean(rate_screened, na.rm = T),
            rate_cessation = mean(rate_cessation, na.rm = T),
            rate_rx = mean(rate_rx, na.rm = T))

pivot <- rates %>%
  pivot_longer(!month_start, names_to = "Type", values_to = "rate")

pivot <- pivot %>% mutate(month_start = as.Date(month_start, "%d%b%Y"))

#### Clinic Demographics ####
# number of encounters, health centers
#read in clin monthly dataset
clin_monthly <- read_csv(paste0(find, "clin_monthly.csv"))
clin_monthly <- clin_monthly %>% dplyr::select(-...1)
demo_info <- clin_monthly %>% 
  ungroup() %>%
  summarise(total_encs = sum(avth_encs, na.rm = T),
            #num_states = length(unique(state_abbr)), not shareable
            num_hc = length(unique(healthsystemid_masked)))
#Number of Patients
demo_month <- clin_monthly %>% 
  group_by(delivery_site_id_masked) %>% 
  filter(row_number()==1) %>%
  ungroup() %>%
  summarise(total_pats = sum(advance_avth_patients, na.rm=T))
#### Figure 1 ####
tobac <- ggplot(pivot, aes(month_start, rate, group = Type, color = Type)) +
  geom_line(size = 2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle=45, vjust=0.7, hjust=1),
        legend.position="bottom") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand=c(0,0)) +
  scale_color_manual(labels = c("Cessation", "Medication", "Screening"),
                     values = c("steelblue", "pink", "goldenrod3")) +
  labs(x = "Date",
       y = "Rate per 1000 Eligible Patients",
       group = "Measure")
tobac

#### Figure 2 ####
#read in monthly_mean_share dataset
monthly_mean_share <- read_csv(paste0(find, "monthly_mean_share.csv"))
monthly_mean_share <- monthly_mean_share %>% dplyr::select(-...1)

monthly2 <- monthly_mean_share %>%
  group_by(month_start) %>%
  select(month_start, pct_tele, pct_inperson, encounters) %>%
  pivot_longer(!c(month_start, encounters), names_to = "visit_type", values_to = "percent")
monthly3 <- monthly2 %>% filter(visit_type == "pct_tele")
# adjust colors as needed
perc_visits1 <- ggplot(monthly3, aes(month_start)) +
  geom_bar(aes(y = encounters), stat="identity", fill = "grey", alpha=0.6) + 
  geom_line(aes(y = percent*14, group = visit_type, color = visit_type), size=1.5) +
  scale_y_continuous(name = "Average Number of Monthly Visits Across 217 Clinics", breaks = seq(0, 1400, 200),
                     sec.axis = sec_axis(~./14, name="Percent Telehealth Visits", breaks = seq(0,100,20))) +
  
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle=45, vjust=1.1, hjust=0.9),
        legend.position="none",
        axis.title.y = element_text(color = "darkgray", size=8),
        axis.title.y.right = element_text(color = "black", size=8)) +
  labs(x = "Date",
       y = "Percent of Telemedicine Visits") +
  scale_color_manual(values=c("black"), name = "Visit Type", labels = c("Telemedicine")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand=c(0,0))

perc_visits1