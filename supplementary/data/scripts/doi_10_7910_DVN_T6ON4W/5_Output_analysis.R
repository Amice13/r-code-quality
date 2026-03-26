
# Load required libraries (install the packages if needed)
library(dplyr)
library(readr)
library(readxl)
library(haven)
library(ggplot2)
library(stringr)
library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(cowplot)
library(odbc)
library(xtable)
library(ggridges)
options(scipen = 999)


# Set directories (adjust paths as needed)
setwd("~/Desktop/Ch/Nature_food/")
# Set file paths
raw_data <- "_data/raw_data"
Eora_ <- "_data/processed_data/Eora_estimates"
Eora_ILO <- "_data/processed_data/Eora_ILO"
MixedIncome <- "_data/processed_data/Eora_estimates/MixedIncome"
PCE <- "_data/raw_data/PCE"
regression_use <- "_data/processed_data/regression"

# Import GNI values
gni_full <- read_xls(file.path(raw_data, "gni041024.xls"))
gni_full <- gni_full %>%
  pivot_longer(cols = -`Country Code`, names_to = "year", values_to = "value") %>%
  drop_na() %>%
  rename(code = `Country Code`) %>%
  mutate(code = paste(code, year, sep = "-")) %>%
  select(-year)
# Income class thresholds
# Thresholds in 2015 calendar year: 1025, 4035, 12475 ---> log: 6.9, 8.3, 9.4
# Thresholds in 2023 calendar year: 1145, 4515, 14005 --> converted to 2015USD (2023USD deflator: 125.64556): 911.3, 3593.4, 11146.4 --> log: 6.8, 8.2, 9.3

#/*****************************************************************************/
#/*                      Total AVC Value-added Data.                          */
#/*****************************************************************************/
# import Exports_VA data
va_faafh_exp <- read_xlsx(file.path(Eora_, "Tot_VA_faafhhExp.xlsx"))
va_fah_exp <- read_xlsx(file.path(Eora_, "Tot_VA_fahExp.xlsx"))
va_fah_exp$COU_column <- paste(va_fah_exp$COU_column, va_fah_exp$Year, sep = "-")
va_faafh_exp$COU_column <- paste(va_faafh_exp$COU_column, va_faafh_exp$Year, sep = "-")
va_fah_exp <- select(va_fah_exp, -Year)
va_faafh_exp <- select(va_faafh_exp, -Year)
va_fah_exp <- pivot_longer(va_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "TOTAL") # 'TOTAL' means it's total value of all primary factors
va_faafh_exp <- pivot_longer(va_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "TOTAL") # 'TOTAL' means it's total value of all primary factors
va_fah_exp$type_abb <- "FAH" 
va_faafh_exp$type_abb <- "FAAFH"
va_total <- bind_rows(va_fah_exp, va_faafh_exp)
names(va_total) <- c("code","industry_name","TOTAL","type_abb") 
# add FAH+FAAFH
sum_fah_faafh <- va_total %>%
  group_by(code, industry_name) %>%
  summarize(TOTAL = sum(TOTAL[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Exports")
va_total_exp <- sum_fah_faafh
# import DOM_Tot_VA data
va_faafh <- read_xlsx(file.path(Eora_, "Tot_VA_faafhDom.xlsx"))
va_fah <- read_xlsx(file.path(Eora_, "Tot_VA_fahDom.xlsx"))
va_fah$COU_column <- paste(va_fah$COU_column, va_fah$Year, sep = "-")
va_faafh$COU_column <- paste(va_faafh$COU_column, va_faafh$Year, sep = "-")
va_fah <- select(va_fah, -Year)
va_faafh <- select(va_faafh, -Year)
va_fah <- pivot_longer(va_fah, cols = -COU_column, names_to = "industry_name", values_to = "TOTAL") # 'TOTAL' means it's total value of all primary factors
va_faafh <- pivot_longer(va_faafh, cols = -COU_column, names_to = "industry_name", values_to = "TOTAL") # 'TOTAL' means it's total value of all primary factors
va_fah$type_abb <- "FAH" 
va_faafh$type_abb <- "FAAFH"
va_total <- bind_rows(va_fah, va_faafh)
names(va_total) <- c("code","industry_name","TOTAL","type_abb") 
# add FAH+FAAFH
sum_fah_faafh <- va_total %>%
  group_by(code, industry_name) %>%
  summarize(TOTAL = sum(TOTAL[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Dom_tot")
va_total <- bind_rows(va_total, sum_fah_faafh)
va_total <- bind_rows(va_total, va_total_exp)
sum_dom_exp <- va_total %>%
  group_by(code, industry_name) %>%
  summarize(TOTAL = sum(TOTAL[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")])) %>%
  ungroup() %>%
  mutate(type_abb = "Total")
va_total <- bind_rows(va_total, sum_dom_exp)
va_total <- va_total %>%
  mutate(industry_name = recode(industry_name,
                                "ag" = "Agriculture, forest and fishing",
                                "fb" = "Food and beverage",
                                "ws" = "Wholesale trade",
                                "rt" = "Retail trade",
                                "trans" = "Transport",
                                "afs" = "Hotels and restaurants"))
# record all country-year pairs with negative tot_va values
test <- subset(va_total, (code %in% code[TOTAL < 0])) 
unique(test$code)
#  [1] "SOM-2015" "ANT-2019" "GMB-2019" "GUY-2019" "IRQ-2019" "SOM-2019" "SYR-2019" "ANT-2018" "CZE-2018" "GMB-2018" "GUY-2018" "IRQ-2018" "SDS-2018" "SOM-2018" "SYR-2018" "SOM-2014" "ANT-2017" "GMB-2017" "GUY-2017"
# [20] "IRQ-2017" "SOM-2017" "SYR-2017" "ANT-2021" "GMB-2021" "GUY-2021" "IRQ-2021" "SOM-2021" "SYR-2021" "ANT-2020" "GMB-2020" "GUY-2020" "IRQ-2020" "MAC-2020" "SOM-2020" "SYR-2020" "ANT-2016" "GMB-2016" "GUY-2016"
# [39] "IRQ-2016" "SOM-2016" "SYR-2016"

#/*****************************************************************************/
#/*                      Total AVC Labor Value-added Data.                    */
#/*****************************************************************************/
# Import EXP_lb_VA data
va_lb_faafh_exp <- read_xlsx(file.path(Eora_, "VA_labor_tot_faafh_Exp.xlsx"))
va_lb_fah_exp <- read_xlsx(file.path(Eora_, "VA_labor_tot_fah_Exp.xlsx"))
va_dilb_faafh_exp <- read_xlsx(file.path(Eora_, "VA_labor_dir_faafh_Exp.xlsx"))
va_dilb_fah_exp <- read_xlsx(file.path(Eora_, "VA_labor_dir_fah_Exp.xlsx"))

va_lb_fah_exp$COU_column <- paste(va_lb_fah_exp$COU_column, va_lb_fah_exp$Year, sep = "-")
va_lb_faafh_exp$COU_column <- paste(va_lb_faafh_exp$COU_column, va_lb_faafh_exp$Year, sep = "-")
va_dilb_fah_exp$COU_column <- paste(va_dilb_fah_exp$COU_column, va_dilb_fah_exp$Year, sep = "-")
va_dilb_faafh_exp$COU_column <- paste(va_dilb_faafh_exp$COU_column, va_dilb_faafh_exp$Year, sep = "-")
va_lb_fah_exp <- select(va_lb_fah_exp, -Year)
va_lb_faafh_exp <- select(va_lb_faafh_exp, -Year)
va_dilb_fah_exp <- select(va_dilb_fah_exp, -Year)
va_dilb_faafh_exp <- select(va_dilb_faafh_exp, -Year)

va_lb_fah_exp <- pivot_longer(va_lb_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_faafh_exp <- pivot_longer(va_lb_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_fah_exp$type_abb <- "FAH"
va_lb_faafh_exp$type_abb <- "FAAFH"
va_dilb_fah_exp <- pivot_longer(va_dilb_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_faafh_exp <- pivot_longer(va_dilb_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_fah_exp$type_abb <- "FAH"
va_dilb_faafh_exp$type_abb <- "FAAFH"

va_lb_fah_exp$index <- paste(va_lb_fah_exp$COU_column, va_lb_fah_exp$industry_name, va_lb_fah_exp$type_abb, sep = "-")
va_lb_faafh_exp$index <- paste(va_lb_faafh_exp$COU_column, va_lb_faafh_exp$industry_name, va_lb_faafh_exp$type_abb, sep = "-")
va_dilb_fah_exp$index <- paste(va_dilb_fah_exp$COU_column, va_dilb_fah_exp$industry_name, va_dilb_fah_exp$type_abb, sep = "-")
va_dilb_faafh_exp$index <- paste(va_dilb_faafh_exp$COU_column, va_dilb_faafh_exp$industry_name, va_dilb_faafh_exp$type_abb, sep = "-")
va_lb_fah_exp <- select(va_lb_fah_exp, -COU_column)
va_lb_faafh_exp <- select(va_lb_faafh_exp, -COU_column)
va_dilb_fah_exp <- select(va_dilb_fah_exp, -COU_column)
va_dilb_faafh_exp <- select(va_dilb_faafh_exp, -COU_column)

va_lb_fah_exp <- merge(va_lb_fah_exp,va_dilb_fah_exp, by = "index")
va_lb_faafh_exp <- merge(va_lb_faafh_exp,va_dilb_faafh_exp, by = "index")
va_lb_fah_exp <- va_lb_fah_exp[, c("index", "LABOR", "Di_LABOR")]
va_lb_faafh_exp <- va_lb_faafh_exp[, c("index", "LABOR", "Di_LABOR")]
va_lb_total <- bind_rows(va_lb_fah_exp, va_lb_faafh_exp)
va_lb_total <- separate(va_lb_total, index, into = c("code", "year","industry_name","type_abb"), sep = "-")

sum_fah_faafh <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Exports")

va_lb_exp <- sum_fah_faafh

# Import DOM_lb_VA data
va_lb_faafh <- read_xlsx(file.path(Eora_, "VA_labor_tot_faafh_Dom.xlsx"))
va_lb_fah <- read_xlsx(file.path(Eora_, "VA_labor_tot_fah_Dom.xlsx"))
va_dilb_faafh <- read_xlsx(file.path(Eora_, "VA_labor_dir_faafh_Dom.xlsx"))
va_dilb_fah <- read_xlsx(file.path(Eora_, "VA_labor_dir_fah_Dom.xlsx"))

va_lb_fah$COU_column <- paste(va_lb_fah$COU_column, va_lb_fah$Year, sep = "-")
va_lb_faafh$COU_column <- paste(va_lb_faafh$COU_column, va_lb_faafh$Year, sep = "-")
va_dilb_fah$COU_column <- paste(va_dilb_fah$COU_column, va_dilb_fah$Year, sep = "-")
va_dilb_faafh$COU_column <- paste(va_dilb_faafh$COU_column, va_dilb_faafh$Year, sep = "-")
va_lb_fah <- select(va_lb_fah, -Year)
va_lb_faafh <- select(va_lb_faafh, -Year)
va_dilb_fah <- select(va_dilb_fah, -Year)
va_dilb_faafh <- select(va_dilb_faafh, -Year)

va_lb_fah <- pivot_longer(va_lb_fah, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_faafh <- pivot_longer(va_lb_faafh, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_fah$type_abb <- "FAH"
va_lb_faafh$type_abb <- "FAAFH"
va_dilb_fah <- pivot_longer(va_dilb_fah, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_faafh <- pivot_longer(va_dilb_faafh, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_fah$type_abb <- "FAH"
va_dilb_faafh$type_abb <- "FAAFH"

va_lb_fah$index <- paste(va_lb_fah$COU_column, va_lb_fah$industry_name, va_lb_fah$type_abb, sep = "-")
va_lb_faafh$index <- paste(va_lb_faafh$COU_column, va_lb_faafh$industry_name, va_lb_faafh$type_abb, sep = "-")
va_dilb_fah$index <- paste(va_dilb_fah$COU_column, va_dilb_fah$industry_name, va_dilb_fah$type_abb, sep = "-")
va_dilb_faafh$index <- paste(va_dilb_faafh$COU_column, va_dilb_faafh$industry_name, va_dilb_faafh$type_abb, sep = "-")
va_lb_fah <- select(va_lb_fah, -COU_column)
va_lb_faafh <- select(va_lb_faafh, -COU_column)
va_dilb_fah <- select(va_dilb_fah, -COU_column)
va_dilb_faafh <- select(va_dilb_faafh, -COU_column)

va_lb_fah <- merge(va_lb_fah,va_dilb_fah, by = "index")
va_lb_faafh <- merge(va_lb_faafh,va_dilb_faafh, by = "index")
va_lb_fah <- va_lb_fah[, c("index", "LABOR", "Di_LABOR")]
va_lb_faafh <- va_lb_faafh[, c("index", "LABOR", "Di_LABOR")]
va_lb_total <- bind_rows(va_lb_fah, va_lb_faafh)
va_lb_total <- separate(va_lb_total, index, into = c("code", "year","industry_name","type_abb"), sep = "-")

sum_fah_faafh <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Dom_tot")
va_lb_total <- bind_rows(va_lb_total, sum_fah_faafh)
va_lb_total <- bind_rows(va_lb_total, va_lb_exp)
sum_dom_exp <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")])) %>%
  ungroup() %>%
  mutate(type_abb = "Total")
va_lb_total <- bind_rows(va_lb_total, sum_dom_exp)
va_lb_total <- va_lb_total %>%
  mutate(industry_name = recode(industry_name,
                                "ag" = "Agriculture, forest and fishing",
                                "fb" = "Food and beverage",
                                "ws" = "Wholesale trade",
                                "rt" = "Retail trade",
                                "trans" = "Transport",
                                "afs" = "Hotels and restaurants"),
         InDi_LABOR = LABOR - Di_LABOR)
va_lb_total$code <- paste(va_lb_total$code, va_lb_total$year, sep = "-")
va_lb_total <- select(va_lb_total, -year)
# record all country-year pairs with negative LABOR VA values
test <- subset(va_lb_total, (code %in% code[LABOR < 0])) 
unique(test$code) 
# [1] "ANT-2018" "ANT-2019" "ANT-2020" "ANT-2021"

#/*****************************************************************************/
#/*           Merge tot_VA with lb_VA data into one va_full table             */
#/*****************************************************************************/
# combine VA_value table and VA_labor table
va_total$index <- paste(va_total$code, va_total$industry_name,va_total$type_abb, sep = "-")
va_lb_total$index <- paste(va_lb_total$code, va_lb_total$industry_name,va_lb_total$type_abb, sep = "-")
va_full <-  merge(va_lb_total, va_total, by = "index", all = FALSE) # keep only matched country-year pairs
# Subset the va_full data frame to keep only the desired columns
va_full <- va_full[, c("index", "TOTAL", "LABOR", "Di_LABOR","InDi_LABOR")]
va_full <- separate(va_full, index, into = c("code", "year","industry_name","type_abb"), sep = "-")
va_full$code <- paste(va_full$code, va_full$year, sep = "-")
va_full <- select(va_full, -year)
#/*****************************************************************************/
#/*                  Calculate Shares for Output Analysis.                    */
#/*****************************************************************************/
# Shares for each country_year pair in the four market destinations                      
va_full <- va_full %>%
  group_by(code, type_abb) %>%
  mutate(indVA_Share_bytype = TOTAL / sum(TOTAL),  # for Total VA (by_industry by_type)
         indLabor_share_bytype = LABOR / sum(LABOR),  # for Labor VA (by_industry by_type)
         Labor_VAshare_byindustry = LABOR / TOTAL,  # for the Labor Share of Total VA (by_industry)
         Labor_VAshare_bytype = sum(LABOR) / sum(TOTAL))  # sum share for the four market destinations (by_type)
unique(va_full$code) #5481 pairs
#/*****************************************************************************/
#/*            Merge with GNI and Create Country-Year Summary table.          */
#/*****************************************************************************/
# Merge with GNI_pc (constant 2015$) values
va_full <- merge(va_full, gni_full, by = "code", all.x = FALSE)  # obs from 164430 to 104340, total c_y pairs drop from 5481 to 3478.
unique(va_full$code) #3478 pairs
# Rule out countries that cannot net out Tobacco from FAH
va_full <- separate(va_full, code, into = c("code", "year"), sep = "-")
unique(va_full$code) # check the country list before further operations: 175
unique(va_full$year) # check the year list before further operations: 1993-2021
rmv_ctry <- c("CAN","GRL","HKG","ISR","KEN","NZL","PRY","RUS","ZAF","CHE","GBR")
va_full <- subset(va_full, !code %in% rmv_ctry) # obs from 104340 to 96510, total c_y pairs drop from 3478 to 3217
unique(va_full$code) #165 (removing 10 countries)
va_regression <- va_full # prepare for further regression use
test <- va_full %>%
  mutate(class = case_when(
    log(value) <= 6.8 ~ "LIC",
    log(value) > 6.8 & log(value) <= 8.2 ~ "LMIC",
    log(value) > 8.2 & log(value) <= 9.3 ~ "UMIC",
    log(value) > 9.3 ~ "HIC",
    TRUE ~ NA_character_  # This line is optional, handles cases that don't fit above conditions
  ))
class_counts <- test %>%
  count(class)
class_counts <- class_counts %>%
  mutate(total = sum(n),
       per = n/total)
print(class_counts)
# class     n total       per
#   HIC 29700 96510 0.3077401
#   LIC 13890 96510 0.1439229
#  LMIC 28320 96510 0.2934411
#  UMIC 24600 96510 0.2548959

# Generate a kernel density plot
k_density <- ggplot(test, aes(x = log(value))) +
  geom_density(fill = "lightblue", alpha = 0.5) + # All data pooled together in one color
  geom_vline(xintercept = c(6.8, 8.2, 9.3), color = "red", linetype = "dashed") + # Vertical lines for class thresholds
  annotate("text", x = c(6.3, 7.75, 8.75, 10.5), y = -0.02, label = c("LIC", "LMIC", "UMIC", "HIC"), color = "black", hjust = 0.5) + # Adding class labels horizontally
  labs(x = "Ln(GNI) per capita in constant 2015 USD",
       y = "Kernel Density") +
  theme_minimal()
k_density
ggsave("_outputs/kernel density EORA.jpeg", k_density, width = 9, height = 6)
# create a country-year pair table/summary
table <- va_full %>%
  select(code, year) %>%
  distinct() %>%
  group_by(code) %>%
  summarise(Year = paste(year, collapse = ", "))
table_export <- xtable(table)
# Print the LaTeX table code
print(table_export, include.rownames = FALSE)

va_full$code <- paste(va_full$code, va_full$year, sep = "-")
va_full <- select(va_full, -year)
unique(va_full$code) # 3217 country-year pairs in total
#/*****************************************************************************/
#/*         Generate Figures for tot_VA and labor_VA output analysis.         */
#/*****************************************************************************/
# Define Color sets and line types
set1_colors <- brewer.pal(9, "Set1")
# Create a line type mapping for specific industries
new_color_set <- c("Agriculture, forest and fishing" = "#4DAF4A",
                   "Hotels and restaurants" = "#E41A1C",
                   "Food and beverage" = "#377EB8",
                   "Retail trade" = "#984EA3",
                   "Wholesale trade" = "darkorange",
                   "Transport" = "#666666",
                   # Add Destinations
                   "FAH" = "darkgreen",
                   "FAAFH" = "red",
                   "Exports" = "darkblue",
                   "Total" = "darkorange"
)

# Create a line type mapping for specific industries
line_type_mapping <- c("Agriculture, forest and fishing" = "solid",
                       "Hotels and restaurants" = "dashed",
                       "Food and beverage" = "solid",
                       "Retail trade" = "dashed",
                       "Wholesale trade" = "solid",
                       "Transport" = "solid"
)

#-------- Figure 2 (A): Industry Share of Total AVC Value Added by Final Market ---------#
va_full_4p <- subset(va_full, !(type_abb == "Dom_tot")) # Subset data for four panels only

num_panels <- length(unique(va_full_4p$type_abb))
annotations_df_4p <- expand.grid(
  x = c(6.8, 8.2, 9.3),
  type_abb = unique(va_full_4p$type_abb)
)
annotations_df_4p$y <- -0.025  # Set the y position for annotations
annotations_df_4p$label <- as.character(annotations_df_4p$x)  # Convert x positions to labels

indVA_shares_gdppc <- ggplot(va_full_4p, aes(x = log(value), y = indVA_Share_bytype, color = industry_name, linetype = industry_name)) +
  geom_point(alpha = 0.00) +
  geom_smooth(se = FALSE, size = 0.7) +
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
    #strip.text = element_text(face = "bold")  # Make strip text bold for visibility
  ) +
  scale_colour_manual(values = new_color_set) +
  scale_linetype_manual(values = line_type_mapping) +
  guides(colour = guide_legend(nrow = 2, title = "Industry"),
         linetype = guide_legend(nrow = 2, title = "Industry")) +
  facet_wrap(~ factor(type_abb, levels = c("FAH", "FAAFH", "Exports", "Total")), nrow = 1) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_vline(data = annotations_df_4p, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df_4p, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE)
indVA_shares_gdppc
# Save the plot to a file
ggsave("_outputs/Fig2a_TotalVA_byind.jpeg", indVA_shares_gdppc, width = 10, height = 6)

#-------- Figure 2 (B): Labor Share by Industry ---------#
va_full_1p <- subset(va_full, type_abb == "Total") # Subset data for one panals only
annotations_df <- data.frame(
  x = c(6.8, 8.2, 9.3),
  y = -0.01,  # You may need to adjust this value
  label = c("6.8", "8.2", "9.3")
)
# Create the ggplot
indLaborVA_shares_gdppc <- ggplot(va_full_1p, aes(x = log(value), y = Labor_VAshare_byindustry, color = industry_name, linetype = industry_name)) +
  geom_point(alpha = 0.0) + 
  geom_smooth(se = FALSE, size = 0.8) +
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    #panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    #strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) +
  scale_colour_manual(values = new_color_set) +
  scale_linetype_manual(values = line_type_mapping) +
  guides(colour = guide_legend(nrow = 2, title = "Industry"),
         linetype = guide_legend(nrow = 2, title = "Industry")) +
  facet_wrap(~ factor(type_abb, levels = c("Total")), nrow = 1) +
  coord_cartesian(ylim = c(0, 0.8)) +
  geom_vline(data = annotations_df, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE)  # Add annotations
indLaborVA_shares_gdppc
# Save the plot as an image
ggsave("_outputs/Fig2b_LaborShare_byind.jpeg", indLaborVA_shares_gdppc, width = 6, height = 7)

#---- Figure SI 4(B): Labor share by market destination ----#
annotations_df <- data.frame(
  x = c(6.8, 8.2, 9.3),
  y = 0.2,  # You may need to adjust this value
  label = c("6.8", "8.2", "9.3")
)
Laborshares_gdppc <- ggplot(va_full_4p, aes(x = log(value), y = Labor_VAshare_bytype, color = type_abb)) +
  geom_point(alpha=.000) + 
  geom_smooth(se=FALSE, size = 0.75) +  
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    #panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    #strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) + 
  scale_colour_manual(values = new_color_set) +  # Set colors manually
  guides(colour = guide_legend(nrow = 1, title = "Market Destinations" )) +
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis limits to 0-1, remove outliers
  coord_cartesian(ylim = c(0.2, 0.8)) + # display y-axix with a limit
  geom_vline(data = annotations_df, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE)  # Add annotations
Laborshares_gdppc
ggsave("_outputs/SI4b_LaborShare_bytype.jpeg", Laborshares_gdppc, width = 7, height = 6)

#---- Figure SI 4(A): Industry share of labor value added ----#
annotations_df_4p$y <- -0 # Set the y position for annotations
LaborVA_shares_gdppc <- ggplot(va_full_4p, aes(x = log(value), y = indLabor_share_bytype, color = industry_name, linetype = industry_name)) +
  geom_point(alpha = 0.00) + geom_smooth(se = FALSE, size = 0.65) +
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    ) + 
  scale_colour_manual(values = new_color_set) +  # Set colors manually
  scale_linetype_manual(values = line_type_mapping) +  # Set line types using the mapping
  guides(colour = guide_legend(nrow = 2, title = "Industry"),
         linetype = guide_legend(nrow = 2, title = "Industry")) +  # Combine both color and line type legends
  facet_wrap(~ factor(type_abb, levels = c("FAH", "FAAFH", "Exports", "Total")), nrow = 1) +
  scale_y_continuous(limits = c(0, 1.0)) +  # Set y-axis limits to 0-1, remove outliers
  geom_vline(data = annotations_df_4p, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df_4p, aes(x = x, y = y, label = label), size = 2, inherit.aes = FALSE)  # Add annotations
LaborVA_shares_gdppc
ggsave("_outputs/SI4a_LaborVA_byind.jpeg", LaborVA_shares_gdppc, width = 10, height = 6)

#/*****************************************************************************/
#/*                     AVC Employment Headcounts Data.                       */
#/*****************************************************************************/
#/*                      Total AVC Labor Value-added Data.                    */
# Import EXP_lb_VA data
va_lb_faafh_exp <- read_xlsx(file.path(Eora_ILO, "VA_labor_tot_faafh_Exp.xlsx"))
va_lb_fah_exp <- read_xlsx(file.path(Eora_ILO, "VA_labor_tot_fah_Exp.xlsx"))
va_dilb_faafh_exp <- read_xlsx(file.path(Eora_ILO, "VA_labor_dir_faafh_Exp.xlsx"))
va_dilb_fah_exp <- read_xlsx(file.path(Eora_ILO, "VA_labor_dir_fah_Exp.xlsx"))

va_lb_fah_exp$COU_column <- paste(va_lb_fah_exp$COU_column, va_lb_fah_exp$Year, sep = "-")
va_lb_faafh_exp$COU_column <- paste(va_lb_faafh_exp$COU_column, va_lb_faafh_exp$Year, sep = "-")
va_dilb_fah_exp$COU_column <- paste(va_dilb_fah_exp$COU_column, va_dilb_fah_exp$Year, sep = "-")
va_dilb_faafh_exp$COU_column <- paste(va_dilb_faafh_exp$COU_column, va_dilb_faafh_exp$Year, sep = "-")
va_lb_fah_exp <- select(va_lb_fah_exp, -Year)
va_lb_faafh_exp <- select(va_lb_faafh_exp, -Year)
va_dilb_fah_exp <- select(va_dilb_fah_exp, -Year)
va_dilb_faafh_exp <- select(va_dilb_faafh_exp, -Year)

va_lb_fah_exp <- pivot_longer(va_lb_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_faafh_exp <- pivot_longer(va_lb_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_fah_exp$type_abb <- "FAH"
va_lb_faafh_exp$type_abb <- "FAAFH"
va_dilb_fah_exp <- pivot_longer(va_dilb_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_faafh_exp <- pivot_longer(va_dilb_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_fah_exp$type_abb <- "FAH"
va_dilb_faafh_exp$type_abb <- "FAAFH"

va_lb_fah_exp$index <- paste(va_lb_fah_exp$COU_column, va_lb_fah_exp$industry_name, va_lb_fah_exp$type_abb, sep = "-")
va_lb_faafh_exp$index <- paste(va_lb_faafh_exp$COU_column, va_lb_faafh_exp$industry_name, va_lb_faafh_exp$type_abb, sep = "-")
va_dilb_fah_exp$index <- paste(va_dilb_fah_exp$COU_column, va_dilb_fah_exp$industry_name, va_dilb_fah_exp$type_abb, sep = "-")
va_dilb_faafh_exp$index <- paste(va_dilb_faafh_exp$COU_column, va_dilb_faafh_exp$industry_name, va_dilb_faafh_exp$type_abb, sep = "-")
va_lb_fah_exp <- select(va_lb_fah_exp, -COU_column)
va_lb_faafh_exp <- select(va_lb_faafh_exp, -COU_column)
va_dilb_fah_exp <- select(va_dilb_fah_exp, -COU_column)
va_dilb_faafh_exp <- select(va_dilb_faafh_exp, -COU_column)

va_lb_fah_exp <- merge(va_lb_fah_exp,va_dilb_fah_exp, by = "index")
va_lb_faafh_exp <- merge(va_lb_faafh_exp,va_dilb_faafh_exp, by = "index")
va_lb_fah_exp <- va_lb_fah_exp[, c("index", "LABOR", "Di_LABOR")]
va_lb_faafh_exp <- va_lb_faafh_exp[, c("index", "LABOR", "Di_LABOR")]
va_lb_total <- bind_rows(va_lb_fah_exp, va_lb_faafh_exp)
va_lb_total <- separate(va_lb_total, index, into = c("code", "year","industry_name","type_abb"), sep = "-")

sum_fah_faafh <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Exports")
va_lb_exp <- sum_fah_faafh
# Import DOM_lb_VA data
va_lb_faafh <- read_xlsx(file.path(Eora_ILO, "VA_labor_tot_faafh_Dom.xlsx"))
va_lb_fah <- read_xlsx(file.path(Eora_ILO, "VA_labor_tot_fah_Dom.xlsx"))
va_dilb_faafh <- read_xlsx(file.path(Eora_ILO, "VA_labor_dir_faafh_Dom.xlsx"))
va_dilb_fah <- read_xlsx(file.path(Eora_ILO, "VA_labor_dir_fah_Dom.xlsx"))

va_lb_fah$COU_column <- paste(va_lb_fah$COU_column, va_lb_fah$Year, sep = "-")
va_lb_faafh$COU_column <- paste(va_lb_faafh$COU_column, va_lb_faafh$Year, sep = "-")
va_dilb_fah$COU_column <- paste(va_dilb_fah$COU_column, va_dilb_fah$Year, sep = "-")
va_dilb_faafh$COU_column <- paste(va_dilb_faafh$COU_column, va_dilb_faafh$Year, sep = "-")
va_lb_fah <- select(va_lb_fah, -Year)
va_lb_faafh <- select(va_lb_faafh, -Year)
va_dilb_fah <- select(va_dilb_fah, -Year)
va_dilb_faafh <- select(va_dilb_faafh, -Year)

va_lb_fah <- pivot_longer(va_lb_fah, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_faafh <- pivot_longer(va_lb_faafh, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_fah$type_abb <- "FAH"
va_lb_faafh$type_abb <- "FAAFH"
va_dilb_fah <- pivot_longer(va_dilb_fah, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_faafh <- pivot_longer(va_dilb_faafh, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_fah$type_abb <- "FAH"
va_dilb_faafh$type_abb <- "FAAFH"

va_lb_fah$index <- paste(va_lb_fah$COU_column, va_lb_fah$industry_name, va_lb_fah$type_abb, sep = "-")
va_lb_faafh$index <- paste(va_lb_faafh$COU_column, va_lb_faafh$industry_name, va_lb_faafh$type_abb, sep = "-")
va_dilb_fah$index <- paste(va_dilb_fah$COU_column, va_dilb_fah$industry_name, va_dilb_fah$type_abb, sep = "-")
va_dilb_faafh$index <- paste(va_dilb_faafh$COU_column, va_dilb_faafh$industry_name, va_dilb_faafh$type_abb, sep = "-")
va_lb_fah <- select(va_lb_fah, -COU_column)
va_lb_faafh <- select(va_lb_faafh, -COU_column)
va_dilb_fah <- select(va_dilb_fah, -COU_column)
va_dilb_faafh <- select(va_dilb_faafh, -COU_column)

va_lb_fah <- merge(va_lb_fah,va_dilb_fah, by = "index")
va_lb_faafh <- merge(va_lb_faafh,va_dilb_faafh, by = "index")
va_lb_fah <- va_lb_fah[, c("index", "LABOR", "Di_LABOR")]
va_lb_faafh <- va_lb_faafh[, c("index", "LABOR", "Di_LABOR")]
va_lb_total <- bind_rows(va_lb_fah, va_lb_faafh)
va_lb_total <- separate(va_lb_total, index, into = c("code", "year","industry_name","type_abb"), sep = "-")

sum_fah_faafh <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Dom_tot")
va_lb_total <- bind_rows(va_lb_total, sum_fah_faafh)
va_lb_total <- bind_rows(va_lb_total, va_lb_exp)
sum_dom_exp <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")])) %>%
  ungroup() %>%
  mutate(type_abb = "Total")
va_lb_total <- bind_rows(va_lb_total, sum_dom_exp)
va_lb_total <- va_lb_total %>%
  mutate(industry_name = recode(industry_name,
                                "ag" = "Agriculture, forest and fishing",
                                "fb" = "Food and beverage",
                                "ws" = "Wholesale trade",
                                "rt" = "Retail trade",
                                "trans" = "Transport",
                                "afs" = "Hotels and restaurants"),
         InDi_LABOR = LABOR - Di_LABOR)
unique(va_lb_total$code) #133
va_lb_total$code <- paste(va_lb_total$code, va_lb_total$year, sep = "-")
va_lb_total <- select(va_lb_total, -year)
unique(va_lb_total$code) #1632
# record all country-year pairs with negative LABOR VA values
test <- subset(va_lb_total, (code %in% code[LABOR < 0])) 
unique(test$code) # 0
va_lb_total$index <- paste(va_lb_total$code, va_lb_total$industry_name,va_lb_total$type_abb, sep = "-")

ILO <- read_xlsx(file.path(Eora_ILO, "ILO_labor.xlsx"))
emp_faafh_exp <- read_xlsx(file.path(Eora_ILO, "emp_faafh_Exp.xlsx"))
emp_fah_exp <- read_xlsx(file.path(Eora_ILO, "emp_fah_Exp.xlsx"))
diemp_faafh_exp <- read_xlsx(file.path(Eora_ILO, "emp_dir_faafh_Exp.xlsx"))
diemp_fah_exp <- read_xlsx(file.path(Eora_ILO, "emp_dir_fah_Exp.xlsx"))

emp_fah_exp$COU_column <- paste(emp_fah_exp$COU_column, emp_fah_exp$Year, sep = "-")
emp_faafh_exp$COU_column <- paste(emp_faafh_exp$COU_column, emp_faafh_exp$Year, sep = "-")
diemp_fah_exp$COU_column <- paste(diemp_fah_exp$COU_column, diemp_fah_exp$Year, sep = "-")
diemp_faafh_exp$COU_column <- paste(diemp_faafh_exp$COU_column, diemp_faafh_exp$Year, sep = "-")
emp_fah_exp <- select(emp_fah_exp, -Year)
emp_faafh_exp <- select(emp_faafh_exp, -Year)
diemp_fah_exp <- select(diemp_fah_exp, -Year)
diemp_faafh_exp <- select(diemp_faafh_exp, -Year)

emp_fah_exp <- pivot_longer(emp_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "emp") # 'TOTAL' means it's total value of all primary factors
emp_faafh_exp <- pivot_longer(emp_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "emp") # 'TOTAL' means it's total value of all primary factors
emp_fah_exp$type_abb <- "FAH" 
emp_faafh_exp$type_abb <- "FAAFH"
diemp_fah_exp <- pivot_longer(diemp_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "di_emp") # 'TOTAL' means it's total value of all primary factors
diemp_faafh_exp <- pivot_longer(diemp_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "di_emp") # 'TOTAL' means it's total value of all primary factors
diemp_fah_exp$type_abb <- "FAH" 
diemp_faafh_exp$type_abb <- "FAAFH"

emp_fah_exp$index <- paste(emp_fah_exp$COU_column, emp_fah_exp$industry_name, emp_fah_exp$type_abb, sep = "-")
emp_faafh_exp$index <- paste(emp_faafh_exp$COU_column, emp_faafh_exp$industry_name, emp_faafh_exp$type_abb, sep = "-")
diemp_fah_exp$index <- paste(diemp_fah_exp$COU_column, diemp_fah_exp$industry_name, diemp_fah_exp$type_abb, sep = "-")
diemp_faafh_exp$index <- paste(diemp_faafh_exp$COU_column, diemp_faafh_exp$industry_name, diemp_faafh_exp$type_abb, sep = "-")
emp_fah_exp <- merge(emp_fah_exp,diemp_fah_exp, by = "index")
emp_faafh_exp <- merge(emp_faafh_exp,diemp_faafh_exp, by = "index")

emp_total <- bind_rows(emp_fah_exp, emp_faafh_exp)
emp_total <- emp_total[, c("index", "emp", "di_emp")]
emp_total <- separate(emp_total, index, into = c("code", "year","industry_name","type_abb"), sep = "-")
sum_fah_faafh <- emp_total %>%
  group_by(code, year, industry_name) %>%
  summarize(emp = sum(emp[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")]),
            di_emp = sum(di_emp[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Exports")
emp_total_exp <- sum_fah_faafh
emp_total_exp$code <- paste(emp_total_exp$code, emp_total_exp$year, sep = "-")
emp_total_exp <- select(emp_total_exp, -year)
emp_total_exp <- emp_total_exp %>%
  mutate(industry_name = recode(industry_name,
                                "ag" = "Agriculture, forest and fishing",
                                "fb" = "Food and beverage",
                                "ws" = "Wholesale trade",
                                "rt" = "Retail trade",
                                "trans" = "Transport",
                                "afs" = "Hotels and restaurants"),
         ind_emp = emp - di_emp)
test <- subset(emp_total_exp, (code %in% code[emp < 0]))
unique(test$code) # 0

# Import DOM emp data
emp_faafh <- read_xlsx(file.path(Eora_ILO, "emp_faafh_Dom.xlsx"))
emp_fah <- read_xlsx(file.path(Eora_ILO, "emp_fah_Dom.xlsx"))
diemp_faafh <- read_xlsx(file.path(Eora_ILO, "emp_dir_faafh_Dom.xlsx"))
diemp_fah <- read_xlsx(file.path(Eora_ILO, "emp_dir_fah_Dom.xlsx"))

emp_fah$COU_column <- paste(emp_fah$COU_column, emp_fah$Year, sep = "-")
emp_faafh$COU_column <- paste(emp_faafh$COU_column, emp_faafh$Year, sep = "-")
diemp_fah$COU_column <- paste(diemp_fah$COU_column, diemp_fah$Year, sep = "-")
diemp_faafh$COU_column <- paste(diemp_faafh$COU_column, diemp_faafh$Year, sep = "-")
emp_fah <- select(emp_fah, -Year)
emp_faafh <- select(emp_faafh, -Year)
diemp_fah <- select(diemp_fah, -Year)
diemp_faafh <- select(diemp_faafh, -Year)

emp_fah <- pivot_longer(emp_fah, cols = -COU_column, names_to = "industry_name", values_to = "emp") # 'TOTAL' means it's total value of all primary factors
emp_faafh <- pivot_longer(emp_faafh, cols = -COU_column, names_to = "industry_name", values_to = "emp") # 'TOTAL' means it's total value of all primary factors
emp_fah$type_abb <- "FAH" 
emp_faafh$type_abb <- "FAAFH"
diemp_fah <- pivot_longer(diemp_fah, cols = -COU_column, names_to = "industry_name", values_to = "di_emp") # 'TOTAL' means it's total value of all primary factors

diemp_faafh <- pivot_longer(diemp_faafh, cols = -COU_column, names_to = "industry_name", values_to = "di_emp") # 'TOTAL' means it's total value of all primary factors
diemp_fah$type_abb <- "FAH" 
diemp_faafh$type_abb <- "FAAFH"

emp_fah$index <- paste(emp_fah$COU_column, emp_fah$industry_name, emp_fah$type_abb, sep = "-")
emp_faafh$index <- paste(emp_faafh$COU_column, emp_faafh$industry_name, emp_faafh$type_abb, sep = "-")
diemp_fah$index <- paste(diemp_fah$COU_column, diemp_fah$industry_name, diemp_fah$type_abb, sep = "-")
diemp_faafh$index <- paste(diemp_faafh$COU_column, diemp_faafh$industry_name, diemp_faafh$type_abb, sep = "-")
emp_fah <- merge(emp_fah,diemp_fah, by = "index")
emp_faafh <- merge(emp_faafh,diemp_faafh, by = "index")

emp_total <- bind_rows(emp_fah, emp_faafh)
emp_total <- emp_total[, c("index", "emp", "di_emp")]
emp_total <- separate(emp_total, index, into = c("code", "year","industry_name","type_abb"), sep = "-")
unique(emp_total$code) # 133 check the country list before further operations
unique(emp_total$year) # 1993-2021 check the year list before further operations
sum_fah_faafh <- emp_total %>%
  group_by(code, year, industry_name) %>%
  summarize(emp = sum(emp[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")]),
            di_emp = sum(di_emp[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Dom_tot")
emp_total <- bind_rows(emp_total, sum_fah_faafh)
emp_total$code <- paste(emp_total$code, emp_total$year, sep = "-")
emp_total <- select(emp_total, -year)
emp_total <- emp_total %>%
  mutate(industry_name = recode(industry_name,
                                "ag" = "Agriculture, forest and fishing",
                                "fb" = "Food and beverage",
                                "ws" = "Wholesale trade",
                                "rt" = "Retail trade",
                                "trans" = "Transport",
                                "afs" = "Hotels and restaurants"),
         ind_emp = emp - di_emp)
test <- subset(emp_total, (code %in% code[emp < 0]))
unique(test$code) # 0
emp_total <- bind_rows(emp_total, emp_total_exp)
sum_dom_exp <- emp_total %>%
  group_by(code, industry_name) %>%
  summarize(emp = sum(emp[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")]),
            di_emp = sum(di_emp[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")]),
            ind_emp = sum(ind_emp[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")])) %>%
  ungroup() %>%
  mutate(type_abb = "Total")
emp_total <- bind_rows(emp_total, sum_dom_exp)
emp_full <- emp_total # prepare this table for gender analysis use
#/*****************************************************************************/
#/*       Merge emp_total with va_lb_total data into one lb_full table        */
#/*****************************************************************************/
emp_total$code <- paste(emp_total$code, emp_total$industry_name, emp_total$type_abb, sep = "-")
emp_total <- select(emp_total, -industry_name, -type_abb)
va_lb_total <- select(va_lb_total, -industry_name, -type_abb, -code)
va_lb_total <- va_lb_total %>% 
  rename(code = index)
salary <- merge (va_lb_total, emp_total, by = "code") #48960
salary <- separate(salary, code, into = c("code", "year","industry_name", "type_abb"), sep = "-")
salary$code <- paste(salary$code, salary$year, sep = "-")
salary <- select(salary, -year)
# Merge with GNI_pc (constant 2015$) values
salary <- merge(salary, gni_full, by = "code", all.x = FALSE) # 42240
unique(salary$code) # 1408 country-year pairs
# Rule out countries that cannot net out Tobacco from FAH
salary <- separate(salary, code, into = c("code", "year"), sep = "-")
unique(salary$code) # check the country list before further operations: 117
unique(salary$year) # check the year list before further operations
# remove countries that cannot net out tobacco from FAH (5 countries are actually removed)
rmv_ctry <- c("CAN","GRL","HKG","ISR","KEN","NZL","PRY","RUS","ZAF","CHE","GBR") 
salary_clean2 <- subset(salary, !code %in% rmv_ctry) # obs cutting from 42240 to 39840, total c_y pairs drop from 1408 to 1328
salary <- salary_clean2 # 112
unique(salary_clean2$code)
test <- salary %>%
  mutate(class = case_when(
    log(value) <= 6.8 ~ "LIC",
    log(value) > 6.8 & log(value) <= 8.2 ~ "LMIC",
    log(value) > 8.2 & log(value) <= 9.3 ~ "UMIC",
    log(value) > 9.3 ~ "HIC",
    TRUE ~ NA_character_  # This line is optional, handles cases that don't fit above conditions
  ))
class_counts <- test %>%
  count(class)
class_counts <- class_counts %>%
  mutate(total = sum(n),
         per = n/total)
print(class_counts)
# class     n total        per
#   HIC 21240 39840 0.53313253
#   LIC  1470 39840 0.03689759
#  LMIC  6090 39840 0.15286145
#  UMIC 11040 39840 0.27710843

# Generate a kernel density plot
k_density <- ggplot(test, aes(x = log(value))) +
  geom_density(fill = "lightblue", alpha = 0.5) + # All data pooled together in one color
  geom_vline(xintercept = c(6.8, 8.2, 9.3), color = "red", linetype = "dashed") + # Vertical lines for class thresholds
  annotate("text", x = c(6.3, 7.75, 9, 10.5), y = -0.02, label = c("LIC", "LMIC", "UMIC", "HIC"), color = "black", hjust = 0.5) + # Adding class labels horizontally
  labs(x = "Ln(GNI) per capita in constant 2015 USD",
       y = "Kernel Density") +
  theme_minimal()
k_density
ggsave("_outputs/kernel density EORA_ILO.jpeg", k_density, width = 9, height = 6)

# create a country-year pair table/summary
table <- salary %>%
  select(code, year) %>%
  distinct() %>%
  group_by(code) %>%
  summarise(Year = paste(year, collapse = ", "))
# table <- table %>%
#   mutate(Year = sapply(strsplit(Year, ", "), length))
table_export <- xtable(table)
# Print the LaTeX table code
print(table_export, include.rownames = FALSE)

salary$code <- paste(salary$code, salary$year, sep = "-")
salary <- select(salary, -year)
unique(salary$code) #1328
ILO_total <- subset(ILO, activity == "Total")
ILO_total <- mutate(ILO_total, ILO_total = value*1000) # ILO data are in thousand unit
ILO_total <- select(ILO_total, -value)
unique(ILO_total$code) #147
ILO_total$code <- paste(ILO_total$code, ILO_total$year, sep = "-")
ILO_total <- select(ILO_total, -year)
unique(ILO_total$code) #1640 country year pairs
salary_ILO <- merge(salary, ILO_total, by = "code", x.all=FALSE) # 39840
unique(salary_ILO$code)
labor_regression <- salary_ILO # prepare for further regression analysis use
salary <- salary_ILO
salary <- salary %>%
  group_by(code, type_abb) %>%
  mutate(indEmp_Share_bytype = di_emp / ILO_total,  # calculate the share of AVC employment of total labor force by AVC industry
         indEmp_Share_byind = di_emp / sum(di_emp))  # calculate the industry share of direct employment
unique(salary$code) # 1328

#/*****************************************************************************/
#/*         Generate Figures for emp and compensation output analysis.        */
#/*****************************************************************************/

#-------- Figure 4(B): Industry Share of AVC Employment ---------#
salary_4P <- subset(salary, !(type_abb == "Dom_tot"))
annotations_df_4p$y <- -0.025 # Set the y position for annotations
combined_plots <- ggplot(salary_4P, aes(x = log(value), y = indEmp_Share_byind, color = industry_name, linetype = industry_name)) +
  geom_point(alpha = 0.00) + geom_smooth(method = "loess", se = FALSE, size = 0.7) +
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    #panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    #strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) + 
  scale_colour_manual(values = new_color_set) +  # Set colors manually
  scale_linetype_manual(values = line_type_mapping) +  # Set line types using the mapping
  guides(colour = guide_legend(nrow = 2, title = "Industry"),
         linetype = guide_legend(nrow = 2, title = "Industry")) +  # Combine both color and line type legends
  guides(colour = guide_legend(nrow = 2, title = "Industry")) +
  coord_cartesian(ylim = c(0, 1.0)) +
  facet_wrap(~ factor(type_abb, levels = c("FAH", "FAAFH", "Exports", "Total")), nrow = 1) +
  geom_vline(data = annotations_df_4p, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df_4p, aes(x = x, y = y, label = label), size = 2.75, inherit.aes = FALSE)  # Add annotations
combined_plots
ggsave("_outputs/Fig4b_dir_emp_byind.jpeg", combined_plots, width = 10, height = 6)

#-------- Figure 4(A): Employment share of labor force ---------#
salary_2p <- salary_4P %>%
  mutate(ag_type = ifelse(industry_name == "Agriculture, forest and fishing", "Direct AFF Employment Share", "Non-AFF AVC Employment Share"))
annotations_df_4p$y <- -0.01 # Set the y position for annotations
combined_plots <- ggplot(salary_2p, aes(x = log(value), y = indEmp_Share_bytype, color = ag_type)) +
  geom_point(alpha = 0.00) + geom_smooth(method = "loess", se = TRUE, size = 0.9) +
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    #panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    #strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) + 
  scale_colour_manual(values = c("#339966","#0066cc"), labels = c("Direct AFF Employment Share", "Non-AFF Employment Share")) +
  guides(colour = guide_legend(nrow = 1, title = " ")) +
  facet_wrap(~ factor(type_abb, levels = c("FAH", "FAAFH", "Exports", "Total")), nrow = 1) +
  coord_cartesian(ylim = c(0, 0.35)) +
  geom_vline(data = annotations_df_4p, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df_4p, aes(x = x, y = y, label = label), size = 2.75, inherit.aes = FALSE)  # Add annotations
combined_plots
ggsave("_outputs/Fig4a_ag_nonag_emp.jpeg", combined_plots, width = 10, height = 6)

#------------ Figure 6(A): Direct employment and subcontractor shares ------------#
salary_3p <- separate(salary_4P, code, into = c("code", "year"), sep = "-")
salary_3p <- salary_3p %>%
  group_by(code, year, type_abb) %>%
  mutate(di_share = sum(di_emp) / sum(emp),
         ind_share = sum(ind_emp) / sum(emp))
annotations_df_4p$y <- 0 # Set the y position for annotations
base_plot <- ggplot(salary_3p, aes(x = log(value))) +
  geom_point(aes(y = di_share, color = "Direct Employment"), size = 0, alpha = 0) +
  geom_point(aes(y = ind_share, color = "Subcontractor"), size = 0, alpha = 0) +
  geom_smooth(aes(y = di_share, color = "Direct Employment"), method = "loess", se = TRUE, size = 0.9) +
  geom_smooth(aes(y = ind_share, color = "Subcontractor"), method = "loess", se = TRUE, size = 0.9) +
  scale_color_manual(values = c("Direct Employment" = "orange", "Subcontractor" = "navy")) +
  #scale_y_continuous(limits = c(0, 1.0)) +
  coord_cartesian(ylim = c(0, 1.0)) +
  labs(color = NULL) +  # Remove 'colour' from the legend
  theme(legend.position = "bottom") +
  #labs(x = "Log GNI per Capita (constant 2015$)", y = "Employment per AVC Industry as a Share of Total Employment") +
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    #panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    #strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) 
combined_plot <- base_plot +
  facet_wrap(~ factor(type_abb, levels = c("FAH", "FAAFH", "Exports", "Total")), nrow = 1) +
  geom_vline(data = annotations_df_4p, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df_4p, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE)  # Add annotations
combined_plot
ggsave("_outputs/Fig6a_di_ind_emp_bytype.jpeg", combined_plot, width = 12, height = 8)

#/*****************************************************************************/
#/*                Everything related to average compensation                 */
#/*****************************************************************************/
indsal_wis <- read_xlsx(file.path(Eora_ILO, "SalaryWinsorized_ind.xlsx"))
names(indsal_wis) <- c("code","type","year","sector","ind")
indsal_wis$index <- paste(indsal_wis$code, indsal_wis$year, indsal_wis$sector, sep = "-")
dirsalary_wis <- read_xlsx(file.path(Eora_ILO, "SalaryWinsorized_di.xlsx"))
names(dirsalary_wis) <- c("code","type","year","sector","dir")
dirsalary_wis$index <- paste(dirsalary_wis$code, dirsalary_wis$year, dirsalary_wis$sector, sep = "-")
salary_wis <- merge(dirsalary_wis,indsal_wis, by = "index", x.all = FALSE)
salary_wis <- salary_wis[, c("index", "ind", "dir")]
salary_wis <- separate(salary_wis, index, into = c("code", "year", "sector"), sep = "-")
# NOTE: the Unit in Eora is current US dollar, we use deflators of USD received from ... to transfer lb_va to constant 2015$.
deflator <- read_xls(file.path(raw_data, "deflator.xls")) # https://fred.stlouisfed.org/series/USAGDPDEFAISMEI
salary <- merge(salary_wis, deflator, by = "year") #9618
salary <- salary %>%
  mutate(saldi_con = dir/(deflator/100),
         salind_con = ind/(deflator/100))   # change current USD to 2015USD, but didnt change the employment unit by this step--> the current unit is thousand1015USD per person.
salary$code <- paste(salary$code, salary$year, sep = "-")
salary <- select(salary, -year)
unique(salary$code) # 1603 country-year pairs
salary <- salary %>%
  mutate(sector = recode(sector,
                         "A01T02" = "Agriculture, forest and fishing",
                         "A04" = "Food and beverage",
                         "A16" = "Wholesale trade",
                         "A17" = "Retail trade",
                         "A19" = "Transport",
                         "A18" = "Hotels and restaurants"))
#/*****************************************************************************/
#/*            Merge with GNI and Create Country-Year Summary table.          */
#/*****************************************************************************/
# Merge with GNI_pc (constant 2015$) values
salary <- merge(salary, gni_full, by = "code", all.x = FALSE) # 8448
unique(salary$code) # 1408 country-year pairs
# Rule out countries that cannot net out Tobacco from FAH
salary <- separate(salary, code, into = c("code", "year"), sep = "-")
unique(salary$code) # check the country list before further operations: 117
unique(salary$year) # check the year list before further operations
# remove countries that cannot net out tobacco from FAH
rmv_ctry <- c("CAN","GRL","HKG","ISR","KEN","NZL","PRY","RUS","ZAF","CHE","GBR")
salary_clean2 <- subset(salary, !code %in% rmv_ctry)
salary <- salary_clean2
unique(salary_clean2$code) #112
salary$code <- paste(salary$code, salary$year, sep = "-")
salary <- select(salary, -year)
unique(salary$code) #1328 is the final country year pairs for Eora-ILO analysis
salary_regression <- salary # prepare for further regression use
#/*****************************************************************************/
#/*           Generate Figures for ave_compensation and employment.           */
#/*****************************************************************************/
#-------- Figure 5: Ave Compensation per AVC Worker ---------#
annotations_df <- data.frame(
  x = c(6.8, 8.2, 9.3),
  y = 1.8,  # You may need to adjust this value
  label = c("6.8", "8.2", "9.3")
)

# add a line showing the slope for isoelastic compensation
salary$ln_GNI <- log(salary$value)
salary$ln_compensation <- log(salary$saldi_con * 1000)
# Calculate the means
mean_ln_GNI <- mean(salary$ln_GNI)
mean_ln_compensation <- mean(salary$ln_compensation)
# intercept
isoelastic_intercept <- mean_ln_compensation - mean_ln_GNI

plot_fah_faafh <- ggplot(salary, aes(x = log(value), y = log(saldi_con*1000), color = sector, linetype = sector)) +
  geom_point(alpha = 0.00) + geom_smooth(method = "loess", se = FALSE, size = 0.75) +
  geom_abline(intercept = isoelastic_intercept, slope = 1, linetype = "dotted", color = "black") +  # Isoelastic line
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       atop(italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~"), 
                            italic("The black dotted line refers to the isoelastic compensation with slope = 1."))))) +
                    ylab("Ln Average Annual Compensation per Worker in AVC (constant 2015USD)") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    #panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    #strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) + 
  scale_colour_manual(values = new_color_set) +  # Set colors manually
  scale_linetype_manual(values = line_type_mapping) +  # Set line types using the mapping
  guides(colour = guide_legend(nrow = 2, title = "Industry"),
         linetype = guide_legend(nrow = 2, title = "Industry")) +  # Combine both color and line type legends
  guides(colour = guide_legend(nrow = 2, title = "Industry")) +
  coord_cartesian(ylim = c(2, 13)) +
  theme(axis.text.x = element_text(size = 8),  # Change x-axis label font size
        axis.text.y = element_text(size = 8),  # Change y-axis label font size
        axis.title = element_text(size = 11)) +
  geom_vline(data = annotations_df, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE)  # Add annotations
plot_fah_faafh
# Save the combined plot
ggsave("_outputs/Fig5_ave_salary_byind_wins.jpeg", plot_fah_faafh, width = 9, height = 7.5)

#------------ Prepare estimates for Figure SI.6: Compensation per Worker by AVC Industry ------------#
salary_tot <- salary %>%
  group_by(sector) %>%
  mutate(di_com = sum(saldi_con),
         ind_com = sum(salind_con)) %>%
  ungroup()
salary_tot <- salary_tot %>%
  mutate(di_com = di_com / 1328,
         ind_com = ind_com / 1328) #1328 is the final country year pairs for Eora-ILO analysis
salary_tot <- salary_tot[!duplicated(salary_tot$sector), ]
salary_tot <- salary_tot[c("sector", "di_com", "ind_com")]
salary_tot <- salary_tot[order(salary_tot$sector, decreasing = TRUE), ]

write.xlsx(salary_tot, file = "_outputs/ave_decomposed_comp_wins.xlsx", sheetName = "bar")
# then generate the horizantal bar chart in Excel

#/*****************************************************************************/
#/*               Figure 3: Labor Share of Total AVC Value added              */
#/*****************************************************************************/
F3 <- read_xlsx(file.path(Eora_ILO, "df_labor_va_ctry_tot_yrs_gni_ForJasmine.xlsx"))
F3 <- F3 %>%
  rename(code = `Country Code`, year = Year, share = LaborToVA, value = Log_GNIpc)
# Manually fit LOESS model with degree = 1 (locally linear smoothing)
loess_fit <- loess(share ~ value, data = F3, degree = 1, span = 0.7)
predictions <- predict(loess_fit, se = TRUE)
F3$loess_fitted <- predictions$fit
F3$loess_upper <- predictions$fit + (1.96 * predictions$se.fit)  # 95% CI upper bound
F3$loess_lower <- predictions$fit - (1.96 * predictions$se.fit)  # 95% CI lower bound

# Create the ggplot with the LOESS curve and confidence band
indLaborVA_shares_gdppc <- ggplot(F3, aes(x = value, y = share)) +
  geom_point(alpha = 0.0) +  
  # Add confidence band using geom_ribbon()
  geom_ribbon(aes(ymin = loess_lower, ymax = loess_upper), fill = "darkgrey", alpha = 0.3) +
  # Add manually fitted LOESS line
  geom_line(aes(y = loess_fitted), size = 0.8, color = "blue") +
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Labor to VA Ratio") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  coord_cartesian(ylim = c(0.1, 0.8)) +
  geom_vline(data = annotations_df, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE)
indLaborVA_shares_gdppc
ggsave("_outputs/Fig3_Labor_VA_Ratio_dgree1.jpeg", indLaborVA_shares_gdppc, width = 6, height = 4)

#/*****************************************************************************/
#/*                    Gender Analysis across AVC sectors                     */
#/*****************************************************************************/
gni <- read_xls(file.path(raw_data, "gni041024.xls"))
gni <- gni %>%
  pivot_longer(cols = -`Country Code`, names_to = "year", values_to = "value") %>%
  drop_na() %>%
  rename(code = `Country Code`) %>%
  mutate(index = paste(code, year, sep = "-")) %>%
  select(-year)
#---------------------EMP stripped of Non-AVC workforce via multiplier analysis---------------------#
emp_full <- subset(emp_full, type_abb == "Total")
emp_full$index <- paste(emp_full$code, emp_full$industry_name, sep = "-")
emp_full <- emp_full[, c("index","di_emp")]
emp_full <- emp_full %>%
  mutate(di_emp = di_emp /1000) # change the unit to thousand
#---------------------Calculate gender ratios from ILO----------------#
gender_ILO <- read_xlsx(file.path(Eora_ILO, "ILO_gender.xlsx"))
direct_activity <- c("A01T02","A04","A16","A17","A18","A19")
gender_ILO <- subset(gender_ILO, activity %in% direct_activity)
gender_ILO <- gender_ILO %>%
  mutate(activity = recode(activity,
                           "A01T02" = "Agriculture, forest and fishing",
                           "A04" = "Food and beverage",
                           "A16" = "Wholesale trade",
                           "A17" = "Retail trade",
                           "A19" = "Transport",
                           "A18" = "Hotels and restaurants"),
         f_share = female/(female+male),
         m_share = male/(female+male))
gender_ILO$index <- paste(gender_ILO$index, gender_ILO$activity, sep = "-")
emp_avc <- merge(gender_ILO, emp_full, by = "index")
emp_avc <- emp_avc %>%
  mutate(female = di_emp*f_share,
         male = di_emp*m_share)
emp_avc$index <- paste(emp_avc$code, emp_avc$year, sep = "-")
emp_avc <- emp_avc[, c("index", "activity","year","female", "male","total","di_emp")]
emp_avc <- merge(emp_avc, gni, by = "index") #from 9612 to 8442

workforce <- read_xlsx(file.path(raw_data, "woking_age_pop.xlsx"))
f_pop <- subset(workforce, sex == "SEX_F")
f_pop <- rename(f_pop, "fpop" = "workforce")
m_pop <- subset(workforce, sex == "SEX_M")
m_pop <- rename(m_pop, "mpop" = "workforce")
f_pop$index <- paste(f_pop$code, f_pop$year, sep = "-")
m_pop$index <- paste(m_pop$code, m_pop$year, sep = "-")
workforce <- merge(f_pop, m_pop, by = "index", all.x = FALSE)
workforce <- select(workforce, c("index","fpop","mpop"))
gender <- merge(emp_avc, workforce, by = "index", all.x = FALSE)
unique(gender$index) #1407 country-year pairs
rmv_ctry <- c("ISR","KEN","ZAF","CHE","GBR")
gender <- subset(gender, !code %in% rmv_ctry)
unique(gender$index) #1327 country-year pairs
unique(gender$code) #112 countries

gender_ratio <- gender %>%
  group_by(index) %>%
  mutate(avc_flfp = sum(female)/fpop,
         avc_mlfp = sum(male)/mpop,
         ind_share = female/sum(female),
         ind_share_m = male/sum(male)) %>%
  ungroup()
gender_ratio <- gender_ratio %>%
  group_by(activity, index) %>%
  mutate(
    ind_avc_flfp = female / fpop,
    ind_avc_mlfp = male / mpop,
    GR = female/male,
    f_share = female/(female+male)) %>%
  ungroup()
unique(gender_ratio$index) #1636 country-year pairs --> 1327

#---------------------Use the winsorized salary decomposition---------------------#
salary_wis <- read_xlsx(file.path(Eora_ILO, "SalaryWinsorized_di.xlsx"))
deflator <- read_xls(file.path(raw_data, "deflator.xls"))
names(salary_wis) <- c("code","type","year","activity","salperwkr")
salary_wis <- select(salary_wis, -type)
salary <- merge(salary_wis, deflator, by = "year") #9618
salary <- salary %>%
  mutate(activity = recode(activity,
                           "A01T02" = "Agriculture, forest and fishing",
                           "A04" = "Food and beverage",
                           "A16" = "Wholesale trade",
                           "A17" = "Retail trade",
                           "A19" = "Transport",
                           "A18" = "Hotels and restaurants"),
         salperwkr_con = (salperwkr*1000)/(deflator/100))
salary$index <- paste(salary$code, salary$year, salary$activity, sep = "-")
salary <- salary[, c("index","salperwkr_con")]
gender_ratio_plot <- gender_ratio
gender_ratio_plot$index <- paste(gender_ratio_plot$index, gender_ratio_plot$activity, sep = "-")
reg_plot <- merge(gender_ratio_plot,salary, by = "index", all.x = FALSE)
reg_plot <- reg_plot %>%
  mutate(class = case_when(
    log(value) <= 7 ~ "LIC",
    log(value) > 7 & log(value) <= 8.2 ~ "LMIC",
    log(value) > 8.2 & log(value) <= 9.3 ~ "UMIC",
    log(value) > 9.3 ~ "HIC",
    TRUE ~ NA_character_  # This line is optional, handles cases that don't fit above conditions
  ))
unique(reg_plot$code) #112
reg_plot <- reg_plot[, c("code", "year","activity", "female", "male","avc_flfp","avc_mlfp","ind_avc_flfp","ind_avc_mlfp","ind_share","ind_share_m","f_share","salperwkr_con", "GR", "value","class")]

#------------------------ Figure SI.7: AVC Labor Force Participation Rate by Gender---------------------#
annotations_df <- data.frame(
  x = c(7, 8.2, 9.3),
  y = 0.00,  # You may need to adjust this value
  label = c("7", "8.2", "9.3")
)
grid_line_type <- element_line(colour = "grey", linetype = "dotted")
plot <- ggplot(reg_plot, aes(x = log(value), y = avc_flfp)) +
  geom_point(alpha = 0.0, color = "darkred") +  # Include point color in legend
  geom_point(alpha = 0.0, color = "darkblue", aes(y = avc_mlfp), data = reg_plot) +
  coord_cartesian(ylim = c(0, 0.5)) +
  geom_smooth(aes(color = "Female"), method = "loess", se = TRUE) +  # First LOESS line with legend
  geom_smooth(data = reg_plot, aes(y = avc_mlfp, color = "Male"), method = "loess", se = TRUE) +  # Second LOESS line with legend
  geom_vline(data = annotations_df, aes(xintercept = x-0.03), color = "darkgrey", linetype = "twodash") +
  geom_text(data = annotations_df, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE) +
  labs(
    x = "Ln GNI per capita (constant 2015USD)",
    y = "AVC Labor Force Participation Rate",
    color = "Gender"  # Title for the color legend
  ) +
  scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +  # Manual color assignment
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.caption = element_text(hjust = 0, vjust = -1),  # Position the caption/footnote
    panel.grid.major = element_blank(),  # No major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    panel.background = element_blank(),  # Remove panel background
    axis.line = element_line(color = "black"),  # Black axis lines
    panel.border = element_rect(colour = "black", fill=NA, size=1),  # Black border around the plot
    legend.position = c(0.95, 0.95),  # Adjust these values to move the legend inside the plot at the top right
    legend.justification = c("right", "top"),  # Anchor point of the legend
    # legend.box.background = element_rect(color = "black", size = 1, linetype = "solid"),  # Adds a border around the legend
    legend.background = element_rect(fill = "white", colour = "black"),  # White fill with a black border for clarity
    legend.box.margin = margin(4, 4, 6, 6)  # Adjust spacing around the legend box within the plot
  )
print(plot)
#title: Total Female labor force in AVC as a share of total female population (age 15+)
ggsave("_outputs/SI7_AVC workforce share.jpeg", plot, width = 6, height = 4.5)

#------------------------ Figure SI.8: Gender Annual Compensation Rate in AVC across Income Classes---------------------#
ind_share <- ggplot(reg_plot, aes(x = log(value), y = f_share, color = activity, linetype = activity)) +
  geom_point(alpha = 0.00) +
  geom_smooth(method = "loess", se = FALSE, size = 0.8) +
  xlab("Log GNI per capita (2015$)") +
  ylab("Female Share of Total Employment within AVC") +
  theme_minimal() +  # Set a minimal theme to clear the background
  theme(panel.grid.major = element_blank(),  # Grey dotted grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "white"),  # White background
        panel.border = element_blank(),  # Remove the plot outline
        axis.line = element_line(color = "black"),  # Black axis lines
        legend.position = "bottom") +
  scale_colour_manual(values = new_color_set) +  # Set colors manually
  scale_linetype_manual(values = line_type_mapping) +  # Set line types using the mapping
  guides(colour = guide_legend(nrow = 2, title = "Industry"),
         linetype = guide_legend(nrow = 2, title = "Industry")) +  # Combine both color and line type legends
  coord_cartesian(ylim = c(0, 0.8)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 11)) +
  geom_vline(xintercept = c(7, 8.2, 9.3), color = "#cccccc", linetype = "twodash") +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  annotate("text", x = c(7, 8.2, 9.3), y = -0.12, label = c("7", "8.2", "9.3"), size = 3)
ind_share
ggsave("_outputs/SI8_Gender_ratio.jpeg", ind_share, width = 6, height = 5, dpi = 300) # among all females working in AVC industries, what is the share for each industry

#------------------------ Figure 6(B): Gender Ratio by Industry ---------------------#
reg_plot <- reg_plot %>%
  mutate(f_comp_i = salperwkr_con*ind_share,
         m_comp_i = salperwkr_con*ind_share_m)
reg_plot <- reg_plot %>%
  group_by(code, year) %>%
  mutate(comp_ratio = sum(f_comp_i) / sum(m_comp_i))
reg_plot <- reg_plot %>%
  group_by(code, year) %>%
  mutate(emp_ratio = sum(female) / sum(male))
reg_plot <- reg_plot %>%
  mutate(class = case_when(
    class %in% c("LMIC", "UMIC") ~ "MIC",
    TRUE ~ class
  ))
reg_plot$class <- factor(reg_plot$class, levels = c("LIC", "MIC", "HIC"))
means <- reg_plot %>%
  group_by(class) %>%
  summarize(mean_com = mean(comp_ratio),
            mean_emp = mean(emp_ratio))
means # You need to copy this below
mean <- data.frame(
  x = c(6.3, 8.2, 10.32),
  y = c(1.032, 0.990, 0.941),  
  label = c("1.032", "0.990", "0.941")
)

plot <- ggplot(reg_plot, aes(x = log(value), y = comp_ratio)) +
  geom_point(alpha = 0., size = 0) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  geom_boxplot(aes(group = class, fill = class), alpha = 0.2, width = 0.2, position = position_dodge(width = 0.8)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  geom_vline(data = annotations_df, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE) + 
  geom_point(data = mean, aes(x = x, y = y), size = 1,color = "#ff0000", inherit.aes = FALSE) +
  geom_text(data = mean, aes(x = x, y = y - 0.02, label = label), size = 3.5,color = "#ff0000", inherit.aes = FALSE) +
  labs(x = "GNI per capita (constant 2015 USD)",
       y = "Female to Male Compensation Ratio",
       fill = "Income Class") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, vjust = -1),  # Position the caption/footnote
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Adds major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    axis.line = element_line(color = "black"),  # Black axis lines
    panel.border = element_rect(colour = "black", fill=NA, size=1),  # Black border around the plot
    legend.position = "bottom"  # Position legend at the bottom
  ) +
  scale_fill_brewer(palette = "Set3")  # Use a color palette for the boxe
plot
ggsave("_outputs/Fig6b_GR_comp_3.jpeg", plot, width = 5, height = 5)


#/*****************************************************************************/
#/*     Figure SI.5: Mixed Income as Share of Total AVC Value Addition        */
#/*****************************************************************************/
# import Exports_VA data
va_faafh_exp <- read_xlsx(file.path(MixedIncome, "Tot_VA_faafhhExp.xlsx"))
va_fah_exp <- read_xlsx(file.path(MixedIncome, "Tot_VA_fahExp.xlsx"))
  
va_fah_exp$COU_column <- paste(va_fah_exp$COU_column, va_fah_exp$Year, sep = "-")
va_faafh_exp$COU_column <- paste(va_faafh_exp$COU_column, va_faafh_exp$Year, sep = "-")
va_fah_exp <- select(va_fah_exp, -Year)
va_faafh_exp <- select(va_faafh_exp, -Year)
va_fah_exp <- pivot_longer(va_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "TOTAL") # 'TOTAL' means it's total value of all primary factors
va_faafh_exp <- pivot_longer(va_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "TOTAL") # 'TOTAL' means it's total value of all primary factors
va_fah_exp$type_abb <- "FAH" 
va_faafh_exp$type_abb <- "FAAFH"
va_total <- bind_rows(va_fah_exp, va_faafh_exp)
names(va_total) <- c("code","industry_name","TOTAL","type_abb") 
# add FAH+FAAFH
sum_fah_faafh <- va_total %>%
  group_by(code, industry_name) %>%
  summarize(TOTAL = sum(TOTAL[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Exports")
va_total_exp <- sum_fah_faafh
# import DOM_Tot_VA data
va_faafh <- read_xlsx(file.path(MixedIncome, "Tot_VA_faafhDom.xlsx"))
va_fah <- read_xlsx(file.path(MixedIncome, "Tot_VA_fahDom.xlsx"))
 
va_fah$COU_column <- paste(va_fah$COU_column, va_fah$Year, sep = "-")
va_faafh$COU_column <- paste(va_faafh$COU_column, va_faafh$Year, sep = "-")
va_fah <- select(va_fah, -Year)
va_faafh <- select(va_faafh, -Year)
va_fah <- pivot_longer(va_fah, cols = -COU_column, names_to = "industry_name", values_to = "TOTAL") # 'TOTAL' means it's total value of all primary factors
va_faafh <- pivot_longer(va_faafh, cols = -COU_column, names_to = "industry_name", values_to = "TOTAL") # 'TOTAL' means it's total value of all primary factors
va_fah$type_abb <- "FAH" 
va_faafh$type_abb <- "FAAFH"
va_total <- bind_rows(va_fah, va_faafh)
names(va_total) <- c("code","industry_name","TOTAL","type_abb") 
# add FAH+FAAFH
sum_fah_faafh <- va_total %>%
  group_by(code, industry_name) %>%
  summarize(TOTAL = sum(TOTAL[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Dom_tot")
va_total <- bind_rows(va_total, sum_fah_faafh)
va_total <- bind_rows(va_total, va_total_exp)
sum_dom_exp <- va_total %>%
  group_by(code, industry_name) %>%
  summarize(TOTAL = sum(TOTAL[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")])) %>%
  ungroup() %>%
  mutate(type_abb = "Total")
va_total <- bind_rows(va_total, sum_dom_exp)
va_total <- va_total %>%
  mutate(industry_name = recode(industry_name,
                                "ag" = "Agriculture, forest and fishing",
                                "fb" = "Food and beverage",
                                "ws" = "Wholesale trade",
                                "rt" = "Retail trade",
                                "trans" = "Transport",
                                "afs" = "Hotels and restaurants"))
# record all country-year pairs with negative tot_va values
test <- subset(va_total, (code %in% code[TOTAL < 0])) 
unique(test$code)
#  [1] "SOM-2015" "ANT-2019" "GMB-2019" "GUY-2019" "IRQ-2019" "SOM-2019" "SYR-2019" "ANT-2018" "CZE-2018" "GMB-2018" "GUY-2018" "IRQ-2018" "SDS-2018" "SOM-2018" "SYR-2018" "SOM-2014" "ANT-2017" "GMB-2017" "GUY-2017"
# [20] "IRQ-2017" "SOM-2017" "SYR-2017" "ANT-2021" "GMB-2021" "GUY-2021" "IRQ-2021" "SOM-2021" "SYR-2021" "ANT-2020" "GMB-2020" "GUY-2020" "IRQ-2020" "MAC-2020" "SOM-2020" "SYR-2020" "ANT-2016" "GMB-2016" "GUY-2016"
# [39] "IRQ-2016" "SOM-2016" "SYR-2016"

# Import EXP_lb_VA data
va_lb_faafh_exp <- read_xlsx(file.path(MixedIncome, "VA_labor_tot_faafh_Exp.xlsx"))
va_lb_fah_exp <- read_xlsx(file.path(MixedIncome, "VA_labor_tot_fah_Exp.xlsx"))
va_dilb_faafh_exp <- read_xlsx(file.path(MixedIncome, "VA_labor_dir_faafh_Exp.xlsx"))
va_dilb_fah_exp <- read_xlsx(file.path(MixedIncome, "VA_labor_dir_fah_Exp.xlsx"))

va_lb_fah_exp$COU_column <- paste(va_lb_fah_exp$COU_column, va_lb_fah_exp$Year, sep = "-")
va_lb_faafh_exp$COU_column <- paste(va_lb_faafh_exp$COU_column, va_lb_faafh_exp$Year, sep = "-")
va_dilb_fah_exp$COU_column <- paste(va_dilb_fah_exp$COU_column, va_dilb_fah_exp$Year, sep = "-")
va_dilb_faafh_exp$COU_column <- paste(va_dilb_faafh_exp$COU_column, va_dilb_faafh_exp$Year, sep = "-")
va_lb_fah_exp <- select(va_lb_fah_exp, -Year)
va_lb_faafh_exp <- select(va_lb_faafh_exp, -Year)
va_dilb_fah_exp <- select(va_dilb_fah_exp, -Year)
va_dilb_faafh_exp <- select(va_dilb_faafh_exp, -Year)

va_lb_fah_exp <- pivot_longer(va_lb_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_faafh_exp <- pivot_longer(va_lb_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_fah_exp$type_abb <- "FAH"
va_lb_faafh_exp$type_abb <- "FAAFH"
va_dilb_fah_exp <- pivot_longer(va_dilb_fah_exp, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_faafh_exp <- pivot_longer(va_dilb_faafh_exp, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_fah_exp$type_abb <- "FAH"
va_dilb_faafh_exp$type_abb <- "FAAFH"

va_lb_fah_exp$index <- paste(va_lb_fah_exp$COU_column, va_lb_fah_exp$industry_name, va_lb_fah_exp$type_abb, sep = "-")
va_lb_faafh_exp$index <- paste(va_lb_faafh_exp$COU_column, va_lb_faafh_exp$industry_name, va_lb_faafh_exp$type_abb, sep = "-")
va_dilb_fah_exp$index <- paste(va_dilb_fah_exp$COU_column, va_dilb_fah_exp$industry_name, va_dilb_fah_exp$type_abb, sep = "-")
va_dilb_faafh_exp$index <- paste(va_dilb_faafh_exp$COU_column, va_dilb_faafh_exp$industry_name, va_dilb_faafh_exp$type_abb, sep = "-")
va_lb_fah_exp <- select(va_lb_fah_exp, -COU_column)
va_lb_faafh_exp <- select(va_lb_faafh_exp, -COU_column)
va_dilb_fah_exp <- select(va_dilb_fah_exp, -COU_column)
va_dilb_faafh_exp <- select(va_dilb_faafh_exp, -COU_column)

va_lb_fah_exp <- merge(va_lb_fah_exp,va_dilb_fah_exp, by = "index")
va_lb_faafh_exp <- merge(va_lb_faafh_exp,va_dilb_faafh_exp, by = "index")
va_lb_fah_exp <- va_lb_fah_exp[, c("index", "LABOR", "Di_LABOR")]
va_lb_faafh_exp <- va_lb_faafh_exp[, c("index", "LABOR", "Di_LABOR")]
va_lb_total <- bind_rows(va_lb_fah_exp, va_lb_faafh_exp)
va_lb_total <- separate(va_lb_total, index, into = c("code", "year","industry_name","type_abb"), sep = "-")

sum_fah_faafh <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Exports")

va_lb_exp <- sum_fah_faafh

# Import DOM_lb_VA data
va_lb_faafh <- read_xlsx(file.path(MixedIncome, "VA_labor_tot_faafh_Dom.xlsx"))
va_lb_fah <- read_xlsx(file.path(MixedIncome, "VA_labor_tot_fah_Dom.xlsx"))
va_dilb_faafh <- read_xlsx(file.path(MixedIncome, "VA_labor_dir_faafh_Dom.xlsx"))
va_dilb_fah <- read_xlsx(file.path(MixedIncome, "VA_labor_dir_fah_Dom.xlsx"))

va_lb_fah$COU_column <- paste(va_lb_fah$COU_column, va_lb_fah$Year, sep = "-")
va_lb_faafh$COU_column <- paste(va_lb_faafh$COU_column, va_lb_faafh$Year, sep = "-")
va_dilb_fah$COU_column <- paste(va_dilb_fah$COU_column, va_dilb_fah$Year, sep = "-")
va_dilb_faafh$COU_column <- paste(va_dilb_faafh$COU_column, va_dilb_faafh$Year, sep = "-")
va_lb_fah <- select(va_lb_fah, -Year)
va_lb_faafh <- select(va_lb_faafh, -Year)
va_dilb_fah <- select(va_dilb_fah, -Year)
va_dilb_faafh <- select(va_dilb_faafh, -Year)

va_lb_fah <- pivot_longer(va_lb_fah, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_faafh <- pivot_longer(va_lb_faafh, cols = -COU_column, names_to = "industry_name", values_to = "LABOR") # 'LABOR' means it's total labor value of all primary factors
va_lb_fah$type_abb <- "FAH"
va_lb_faafh$type_abb <- "FAAFH"
va_dilb_fah <- pivot_longer(va_dilb_fah, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_faafh <- pivot_longer(va_dilb_faafh, cols = -COU_column, names_to = "industry_name", values_to = "Di_LABOR") # 'LABOR' means it's total labor value of all primary factors
va_dilb_fah$type_abb <- "FAH"
va_dilb_faafh$type_abb <- "FAAFH"

va_lb_fah$index <- paste(va_lb_fah$COU_column, va_lb_fah$industry_name, va_lb_fah$type_abb, sep = "-")
va_lb_faafh$index <- paste(va_lb_faafh$COU_column, va_lb_faafh$industry_name, va_lb_faafh$type_abb, sep = "-")
va_dilb_fah$index <- paste(va_dilb_fah$COU_column, va_dilb_fah$industry_name, va_dilb_fah$type_abb, sep = "-")
va_dilb_faafh$index <- paste(va_dilb_faafh$COU_column, va_dilb_faafh$industry_name, va_dilb_faafh$type_abb, sep = "-")
va_lb_fah <- select(va_lb_fah, -COU_column)
va_lb_faafh <- select(va_lb_faafh, -COU_column)
va_dilb_fah <- select(va_dilb_fah, -COU_column)
va_dilb_faafh <- select(va_dilb_faafh, -COU_column)

va_lb_fah <- merge(va_lb_fah,va_dilb_fah, by = "index")
va_lb_faafh <- merge(va_lb_faafh,va_dilb_faafh, by = "index")
va_lb_fah <- va_lb_fah[, c("index", "LABOR", "Di_LABOR")]
va_lb_faafh <- va_lb_faafh[, c("index", "LABOR", "Di_LABOR")]
va_lb_total <- bind_rows(va_lb_fah, va_lb_faafh)
va_lb_total <- separate(va_lb_total, index, into = c("code", "year","industry_name","type_abb"), sep = "-")

sum_fah_faafh <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("FAH", "FAAFH")])) %>%
  ungroup() %>%
  mutate(type_abb = "Dom_tot")
va_lb_total <- bind_rows(va_lb_total, sum_fah_faafh)
va_lb_total <- bind_rows(va_lb_total, va_lb_exp)
sum_dom_exp <- va_lb_total %>%
  group_by(code, year, industry_name) %>%
  summarize(LABOR = sum(LABOR[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")]),
            Di_LABOR = sum(Di_LABOR[is.na(type_abb) | type_abb %in% c("Dom_tot", "Exports")])) %>%
  ungroup() %>%
  mutate(type_abb = "Total")
va_lb_total <- bind_rows(va_lb_total, sum_dom_exp)
va_lb_total <- va_lb_total %>%
  mutate(industry_name = recode(industry_name,
                                "ag" = "Agriculture, forest and fishing",
                                "fb" = "Food and beverage",
                                "ws" = "Wholesale trade",
                                "rt" = "Retail trade",
                                "trans" = "Transport",
                                "afs" = "Hotels and restaurants"),
         InDi_LABOR = LABOR - Di_LABOR)
va_lb_total$code <- paste(va_lb_total$code, va_lb_total$year, sep = "-")
va_lb_total <- select(va_lb_total, -year)
# record all country-year pairs with negative LABOR VA values
test <- subset(va_lb_total, (code %in% code[LABOR < 0])) 
unique(test$code) 
# [1] "ANT-2018" "ANT-2019" "ANT-2020" "ANT-2021"

# combine VA_value table and VA_labor table
va_total$index <- paste(va_total$code, va_total$industry_name,va_total$type_abb, sep = "-")
va_lb_total$index <- paste(va_lb_total$code, va_lb_total$industry_name,va_lb_total$type_abb, sep = "-")
va_full <-  merge(va_lb_total, va_total, by = "index", all = FALSE) # keep only matched country-year pairs
# Subset the va_full data frame to keep only the desired columns
va_full <- va_full[, c("index", "TOTAL", "LABOR", "Di_LABOR","InDi_LABOR")]
va_full <- separate(va_full, index, into = c("code", "year","industry_name","type_abb"), sep = "-")
va_full$code <- paste(va_full$code, va_full$year, sep = "-")
va_full <- select(va_full, -year)
# Shares for each country_year pair in the four market destinations                      
va_full <- va_full %>%
  group_by(code, type_abb) %>%
  mutate(indVA_Share_bytype = TOTAL / sum(TOTAL),  # for Total VA (by_industry by_type)
         indLabor_share_bytype = LABOR / sum(LABOR),  # for Labor VA (by_industry by_type)
         Labor_VAshare_byindustry = LABOR / TOTAL,  # for the Labor Share of Total VA (by_industry)
         Labor_VAshare_bytype = sum(LABOR) / sum(TOTAL))  # sum share for the four market destinations (by_type)
unique(va_full$code) #5481 pairs

# Merge with GNI_pc (constant 2015$) values
va_full <- merge(va_full, gni_full, by = "code", all.x = FALSE)  # obs from 164430 to 104340, total c_y pairs drop from 5481 to 3478.
unique(va_full$code) #3478 pairs
# Rule out countries that cannot net out Tobacco from FAH
va_full <- separate(va_full, code, into = c("code", "year"), sep = "-")
unique(va_full$code) # check the country list before further operations: 175
unique(va_full$year) # check the year list before further operations: 1993-2021
rmv_ctry <- c("CAN","GRL","HKG","ISR","KEN","NZL","PRY","RUS","ZAF","CHE","GBR")
va_full <- subset(va_full, !code %in% rmv_ctry) # obs from 104340 to 96510, total c_y pairs drop from 3478 to 3217
unique(va_full$code) #165 (removing 10 countries)
va_full$code <- paste(va_full$code, va_full$year, sep = "-")
va_full <- select(va_full, -year)
unique(va_full$code) # 3217 country-year pairs in total

#-------- Generate the figure --------#
va_full_1p <- subset(va_full, type_abb == "Total") # Subset data for one panals only
annotations_df <- data.frame(
  x = c(6.8, 8.2, 9.3),
  y = -0.01,  # You may need to adjust this value
  label = c("6.8", "8.2", "9.3")
)
test <- subset(va_full_1p, industry_name == "Food and beverage")
# Create the ggplot
indLaborVA_shares_gdppc <- ggplot(va_full_1p, aes(x = log(value), y = Labor_VAshare_byindustry, color = industry_name, linetype = industry_name)) +
  geom_point(alpha = 0.0) + 
  geom_smooth(method = "loess", se = FALSE, size = 0.8) +
  xlab(expression(atop("Ln(GNI) per capita in constant 2015 USD", 
                       italic("Income Group: LIC: ~ < 6.8; LMIC: [6.8, 8.2]; UMIC: (8.2, 9.3]; HIC: 9.3 < ~")))) +
  ylab("Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    #panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    #strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) +
  scale_colour_manual(values = new_color_set) +
  scale_linetype_manual(values = line_type_mapping) +
  guides(colour = guide_legend(nrow = 2, title = "Industry"),
         linetype = guide_legend(nrow = 2, title = "Industry")) +
  facet_wrap(~ factor(type_abb, levels = c("Total")), nrow = 1) +
  coord_cartesian(ylim = c(0, 0.3)) +
  geom_vline(data = annotations_df, aes(xintercept = x), color = "#cccccc", linetype = "twodash") +
  geom_text(data = annotations_df, aes(x = x, y = y, label = label), size = 3, inherit.aes = FALSE)  # Add annotations
indLaborVA_shares_gdppc
ggsave("_outputs/SI5_LaborShare_byind_mix.jpeg", indLaborVA_shares_gdppc, width = 6, height = 7)


#/*****************************************************************************/
#/*              Domestic fah/FAAFH PCE from Injection Matrix                 */
#/*****************************************************************************/
# Initialize an empty list to store results for each year
all_PCE <- list()
# Loop through each year from 1993 to 2021
for (year in 1993:2021) {
  
  pp_file_path <- paste0("_data/raw_data/PCE/unzipped/Eora26_", year, "_pp_FD.txt")
  pp <- read.table(pp_file_path, header = FALSE)
  
  # Calculate total PCE before netting out tobacco
  tot_PCE_or <- colSums(pp, na.rm = TRUE)
  
  label_FD <- read.table("_data/raw_data/PCE/labels_FD.txt", 
                         header = FALSE, sep = "\t", fill = TRUE, quote = "")
  label_FD <- label_FD %>%
    select(V1, V4) %>%
    rename(CCOU = V1, CActOrInst = V4) %>%
    mutate(CActOrInst = recode(CActOrInst, "Household final consumption P.3h" = "XH"))
  COL <- paste(label_FD$CCOU, label_FD$CActOrInst, sep = "_")
  
  # Check if the length of COL matches the number of columns in pp
  if (length(COL) == ncol(pp)) {
    colnames(pp) <- COL
  } else {
    warning("The number of columns in pp does not match the length of COL.")
  }
  
  # Select columns containing 'XH' and clean the column names
  pp_XH <- pp %>%
    select(contains("XH"))
  colnames(pp_XH) <- gsub("_XH", "", colnames(pp_XH))
  
  label_T <- read.table("_data/raw_data/PCE/labels_T.txt", 
                        header = FALSE, sep = "\t", fill = TRUE, quote = "")
  label_T <- label_T %>%
    select(V1, V4) %>%
    rename(RCOU = V1, RActOrInst = V4) %>%
    mutate(RActOrInst = recode(RActOrInst,
                               "Agriculture" = "A01", "Fishing" = "A02", "Mining and Quarrying" = "A03", 
                               "Food & Beverages" = "A04", "Textiles and Wearing Apparel" = "A05", 
                               "Wood and Paper" = "A06", "Petroleum, Chemical and Non-Metallic Mineral Products" = "A07", 
                               "Metal Products" = "A08", "Electrical and Machinery" = "A09", "Transport Equipment" = "A10", 
                               "Other Manufacturing" = "A11", "Recycling" = "A12", "Electricity, Gas and Water" = "A13", 
                               "Construction" = "A14", "Maintenance and Repair" = "A15", "Wholesale Trade" = "A16", 
                               "Retail Trade" = "A17", "Hotels and Restraurants" = "A18", "Transport" = "A19", 
                               "Post and Telecommunications" = "A20", "Financial Intermediation and Business Activities" = "A21", 
                               "Public Administration" = "A22", "Education, Health and Other Services" = "A23", 
                               "Private Households" = "A24", "Others" = "A25", "Re-export & Re-import" = "A26"))
  
  ROW <- paste(label_T$RCOU, label_T$RActOrInst, sep = "_")
  
  # Check if the length of ROW matches the number of rows in pp_XH
  if (length(ROW) == nrow(pp_XH)) {
    rownames(pp_XH) <- ROW
  } else {
    warning("The number of rows in pp_XH does not match the length of ROW.")
  }
  
  pp_XH <- pp_XH[!grepl("A16|A17|A19", rownames(pp_XH)), ]
  
  pp_XH_or <- pp_XH # Save this matrix before netting out tobacco
  
  # Linearize the matrix
  pp_XH$RCOU_ACT <- rownames(pp_XH)
  long_df <- pp_XH %>%
    pivot_longer(cols = -RCOU_ACT, names_to = "CCOU", values_to = "Value") %>%
    separate(RCOU_ACT, into = c("RCOU", "RACT"), sep = "_")
  
  # Import and linearize the tobacco share matrix for the current year
  nontob_file_path <- paste0("_data/raw_data/PCE/NoTobShr/nonTobShr_", year, ".csv")
  nontob <- read.csv(nontob_file_path, sep = "|")
  colnames(nontob) <- gsub("_XH", "", colnames(nontob))
  nontob <- subset(nontob, !Ctry == c("ZZZ_A01", "ZZZ_A04"))
  
  # Convert nontob into long format
  long_share <- nontob %>%
    pivot_longer(cols = -Ctry, names_to = "CCOU", values_to = "Share") %>%
    separate(Ctry, into = c("RCOU", "RACT"), sep = "_")
  
  # Merge and multiply the shares to XH values
  long_df$index <- paste(long_df$CCOU, long_df$RCOU, long_df$RACT, sep = "_")
  long_share$index <- paste(long_share$CCOU, long_share$RCOU, long_share$RACT, sep = "_")
  long <- merge(long_df, long_share, by = "index", all.x = TRUE) %>%
    mutate(Share = replace_na(Share, 1), # Replaces NA in Share with 1
           Value = Value * Share)        # Multiplies Value by Share
  
  long <- long[, c("index", "Value")] %>%
    separate(index, into = c("CCOU", "RCOU", "RACT"), sep = "_")
  
  # Convert the long data back to the wide matrix format
  pp_XH <- long %>%
    mutate(RCOU_ACT = paste(RCOU, RACT, sep = "_")) %>%
    select(-RCOU, -RACT) %>%
    pivot_wider(names_from = CCOU, values_from = Value) %>%
    column_to_rownames(var = "RCOU_ACT")
  
  # Calculate specific values for checks and PCE metrics
  food_rows <- pp_XH[grepl("A01|A02|A04|A18", rownames(pp_XH)), ]
  food_PCE <- colSums(food_rows, na.rm = TRUE)
  tot_PCE <- colSums(pp_XH, na.rm = TRUE)
  faafh_rows <- pp_XH[grepl("A18", rownames(pp_XH)), ]
  faafh_PCE <- colSums(faafh_rows, na.rm = TRUE)
  
  # Create the PCE data frame for the current year
  PCE <- data.frame(
    food_PCE = food_PCE,
    faafh_PCE = faafh_PCE,
    tot_PCE = tot_PCE
  )
  
  PCE$COU <- paste(rownames(PCE), year, sep = "_") # Add a column with country-year identifier
  all_PCE[[as.character(year)]] <- PCE   # Store the result in the list
}

final_PCE <- do.call(rbind, all_PCE) # Combine all results into a single data frame


final_PCE$fafh_food_share <- final_PCE$faafh_PCE/final_PCE$food_PCE
final_PCE$tot_FOOD_PCE <- final_PCE$food_PCE/final_PCE$tot_PCE
final_PCE <- rename(final_PCE, "code" = "COU")
# Import GNI values
gni_full <- read_xls(file.path(raw_data, "gni041024.xls"))
gni_full <- gni_full %>%
  pivot_longer(cols = -`Country Code`, names_to = "year", values_to = "value") %>%
  drop_na() %>%
  rename(code = `Country Code`) %>%
  mutate(code = paste(code, year, sep = "_")) %>%
  select(-year)
final_PCE <- merge(final_PCE, gni_full, by = "code")
# write.xlsx(final_PCE, file = "/Users/jasminejiang/Desktop/Ch/ASTAR-sj/Output082024/PCE_nontrd.xlsx", sheetName = "PCE")
# Remove outliers
PCE_xoutliers <- final_PCE %>%
  filter(tot_FOOD_PCE > 0,
         tot_FOOD_PCE < 1,
         faafh_PCE > 0)

#-------- FIGURE SI.2(a): Consumer Food Expenditures as a Share of Total Expenditures ---------#
rmv_ctry <- c("CAN","GRL","HKG","ISR","KEN","NZL","PRY","RUS","ZAF","CHE","GBR")
si_2 <- subset(PCE_xoutliers, !`Country Code` %in% rmv_ctry) 
outlier_ctry <- c("BGD", "COD", "KHM", "MDG", "MLI", "NPL", "SYR", "TZA", "UGA", "BLR") # The identification of the 10 countries is included in 1_NatureFoodCode.py. 
si_2 <- subset(PCE_xoutliers, !`Country Code` %in% outlier_ctry) 
grid_line_type <- element_line(colour = "grey", linetype = "dotted")
PCE_shares_gdppc_b <- ggplot(PCE_xoutliers) +
  geom_point(aes(x = log(value), y = fafh_food_share, color = "FAAFH PCE as Share of Total Food PCE"), alpha = 0.00) +
  geom_smooth(aes(x = log(value), y = fafh_food_share, color = "FAAFH PCE as Share of Total Food PCE"), method = "lm", se = TRUE) +
  geom_point(aes(x = log(value), y = tot_FOOD_PCE, color = "Total Food PCE as Share of Total PCE"), alpha = 0.00) +
  geom_smooth(aes(x = log(value), y = tot_FOOD_PCE, color = "Total Food PCE as Share of Total PCE"), method = "lm", se = TRUE) +
  xlab("Log GNI per capita (2015$)") +
  ylab("Personal Food Consumption Expenditure Share") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    #panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    #strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) +
  scale_color_manual(
    values = c("FAAFH PCE as Share of Total Food PCE" = "#339966", "Total Food PCE as Share of Total PCE" = "#0066cc"),
    labels = c("FAAFH PCE as Share of Total Food PCE", "Total Food PCE as Share of Total PCE")
  ) +
  coord_cartesian(ylim = c(0, 0.40)) +
  labs(
    color = "Variable"
  )
PCE_shares_gdppc_b <- PCE_shares_gdppc_b + 
  geom_vline(xintercept = c(6.8, 8.2, 9.3), color = "#cccccc", linetype = "twodash") +
  annotate("text", x = c(6.8, 8.2, 9.3), y = 0, label = c("6.8", "8.2", "9.3"), size = 3)
PCE_shares_gdppc_b
# Save the plot as a JPEG file
ggsave(file.path(Dir_si, "SI2_PCE_lm_xoutliers.jpeg"), PCE_shares_gdppc_b, width = 8, height = 5)

#-------- FIGURE SI.2(b): Consumer Food Expenditures as a Share of Total Expenditures ---------#
si_2 <- read_csv(file.path(Eora_, "df_pce_11_exp.csv"))
# Outliers and the 11 countries have already been removed from the dataset, df_pce_11_exp.csv. 
# Details on the construction of df_pce_11_exp.csv can be found in 1_NatureFoodCode.py. 
unique(si_2$`Country Code`) # check the country list before further operations
unique(si_2$Year) # check the year list before further operations: 1993-2021
rmv_ctry <- c("CAN","GRL","HKG","ISR","KEN","NZL","PRY","RUS","ZAF","CHE","GBR")
si_2 <- subset(si_2, !`Country Code` %in% rmv_ctry) # obs from 104340 to 96510, total c_y pairs drop from 3478 to 3217
unique(si_2$`Country Code`) #165 (removing 10 countries)
si_2$code <- paste(si_2$`Country Code`, si_2$Year, sep = "-")
si_2 <- select(si_2, -Year)
unique(si_2$code) # 3217 country-year pairs in total


grid_line_type <- element_line(colour = "grey", linetype = "dotted")
PCE_shares_gdppc_b <- ggplot(si_2) +
  geom_point(aes(x = log(GNIPcConst), y = Exp_shr, color = "Exports as Share of Total VA"), alpha = 0.00) +
  geom_smooth(aes(x = log(GNIPcConst), y = Exp_shr, color = "Exports as Share of Total VA"), method = "lm", se = TRUE) +
  geom_point(aes(x = log(GNIPcConst), y = Fah_shr, color = "FAH as Share of Total VA"), alpha = 0.00) +
  geom_smooth(aes(x = log(GNIPcConst), y = Fah_shr, color = "FAH as Share of Total VA"), method = "lm", se = TRUE) +
  geom_point(aes(x = log(GNIPcConst), y = Faafh_shr, color = "FAAFH as Share of Total VA"), alpha = 0.00) +
  geom_smooth(aes(x = log(GNIPcConst), y = Faafh_shr, color = "FAAFH as Share of Total VA"), method = "lm", se = TRUE) +
  xlab("Log GNI per capita (2015$)") +
  ylab("Share of Total VA") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add black border to all panels
    strip.background = element_rect(color = "black", fill = "white"),  # Ensure strip labels have the same border
  ) +
  scale_color_manual(
    values = c("Exports as Share of Total VA" = "darkorange", "FAAFH as Share of Total VA" = "#339966", "FAH as Share of Total VA" = "#0066cc"),
    labels = c("Exports as Share of Total VA", "FAAFH PCE as Share of Total VA", "FAH as Share of Total VA")
  ) +
  coord_cartesian(ylim = c(0, 0.8)) +
  labs(
    color = "Variable"
  )
PCE_shares_gdppc_a <- PCE_shares_gdppc_b + 
  geom_vline(xintercept = c(6.8, 8.2, 9.3), color = "#cccccc", linetype = "twodash") +
  annotate("text", x = c(6.8, 8.2, 9.3), y = 0, label = c("6.8", "8.2", "9.3"), size = 3)
PCE_shares_gdppc_a
ggsave(file.path(Dir_si, "SI2_share.jpeg"), PCE_shares_gdppc_a, width = 8, height = 5)


#/*****************************************************************************/
#/*             Economic Developemnt Indicator for regression use.            */
#/*****************************************************************************/
# Import GNI values
POP <- read_xls(file.path(raw_data, "Total_POP.xls"))
POP <- POP %>%   #8401
  pivot_longer(cols = -`Country Code`, names_to = "year", values_to = "pop") %>%
  drop_na() %>%
  rename(code = `Country Code`) %>%
  mutate(code = paste(code, year, sep = "-")) %>%
  select(-year)
AgTFP <- read_xlsx(file.path(raw_data, "AgTFP.xlsx"), sheet = "AgTFP")
AgTFP <- AgTFP %>%  #5230
  drop_na() %>%
  rename(code = `ISO3`, year = Year, TFP = Value) %>%
  mutate(code = paste(code, year, sep = "-"),
         TFP = TFP/100) %>%
  select(-year) %>%
  select(-Attribute)
urb <- read_xls(file.path(raw_data, "urb_pop.xls"), sheet = "Data")
urb <- urb %>%   #8215
  pivot_longer(cols = -`Country Code`, names_to = "year", values_to = "urb") %>%
  drop_na() %>%  #16539
  rename(code = `Country Code`) %>%
  mutate(code = paste(code, year, sep = "-")) %>%
  select(-year)
X <- merge(POP, AgTFP, by = "code", x.all = "FALSE")  #4998
X <- merge(X, urb, by = "code", x.all = "FALSE")   #4998

#/*****************************************************************************/
#/*                        Employment regressions  .                          */
#/*****************************************************************************/
unique(labor_regression$code) #1328 country-year pairs
test <- separate(labor_regression, code, into = c("code", "year"), sep = "-")
unique(test$code) #112
reg <- merge(labor_regression, X, by = "code", x.all = "FALSE")
unique(reg$code) #1308 country-year pairs

reg <- reg[, c("code", "industry_name", "type_abb","emp","di_emp", "LABOR", "Di_LABOR","value","ILO_total","pop","TFP","urb")]
reg <- reg %>%
  group_by(code, type_abb) %>%
  mutate(AVC = sum(emp)) %>%
  ungroup()
reg <- reg %>%
  group_by(code, industry_name, type_abb) %>%
  mutate(ind_share_LFP = emp / ILO_total,
         ind_share = emp / AVC) %>%
  ungroup()
reg <- separate(reg, code, into = c("code", "year"), sep = "-")
unique(reg$code) #109

#/*****************************************************************************/
#/*                               Industry Dummy.  .                          */
#/*****************************************************************************/
industry_dummies <- model.matrix(~ factor(industry_name) - 1, data = reg)
industry_dummy <- as.data.frame(industry_dummies)
colnames(industry_dummy) <- gsub("factor\\(industry_name\\)", "gni_x_", colnames(industry_dummy))
regression_dummy <- cbind(reg, industry_dummy*log(reg$value))

industry_dummy <- as.data.frame(industry_dummies)
colnames(industry_dummy) <- gsub("factor\\(industry_name\\)", "tfp_x_", colnames(industry_dummy))
reg <- cbind(regression_dummy, industry_dummy*reg$TFP)

FAH <- subset(reg, type_abb == "FAH")
FAAFH <- subset(reg, type_abb == "FAAFH")
Exports <- subset(reg, type_abb == "Exports")
Total <- subset(reg, type_abb == "Total")
write.xlsx(FAH, file = file.path(regression_use, "FAH.xlsx"), sheetName = "FAH")
write.xlsx(FAAFH, file = file.path(regression_use, "FAAFH.xlsx"), sheetName = "FAAFH")
write.xlsx(Exports, file = file.path(regression_use, "Exports.xlsx"), sheetName = "Exports")
write.xlsx(Total, file = file.path(regression_use, "Total.xlsx"), sheetName = "Total")


unique(salary_regression$code) #1328 country-year pairs
reg <- merge(salary_regression, X, by = "code", x.all = "FALSE")
unique(reg$code) #1308 country-year pairs

reg <- reg[, c("code", "sector","saldi_con","salind_con","value","pop","TFP","urb")]
reg <- separate(reg, code, into = c("code", "year"), sep = "-")
unique(reg$code) #109
unique(reg$year) #1993-2021
industry_dummies <- model.matrix(~ factor(sector) - 1, data = reg)
industry_dummy <- as.data.frame(industry_dummies)
colnames(industry_dummy) <- gsub("factor\\(sector\\)", "gni_x_", colnames(industry_dummy))
reg <- cbind(reg, industry_dummy*log(reg$value))
write.xlsx(reg, file = file.path(regression_use, "salary.xlsx"), sheetName = "salary")
