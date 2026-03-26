## Data analyses ##

## Reproduce Fig. 1 ##

# Load R packages
library(tidyverse)
library(ggpubr)

## Figure 1A ##

# Military demographics - active duty military personnel #
# Left side of Figure 1A

# Numbers from 2019 military demographics report:
# https://download.militaryonesource.mil/12038/MOS/Reports/2019-demographics-report.pdf

mycols <- c("#009E73", "#0072B2", "#D55E00", "#999999", "#F0E442")

# Sex
sex <- tibble(
  sex = c("Male", "Female"),
  prop = c(83.1, 16.9)
) %>%
  arrange(desc(prop)) %>% 
  mutate(sex = factor(sex, sex)) %>% 
  arrange(desc(sex)) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

s_pie <- ggplot(sex, aes(x = "", y = prop, fill = sex)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white", size = 3.5) +
  scale_fill_manual(name = "Sex",
                    values = mycols) +
  theme_void() +
  theme(legend.justification = "left")

# Age
age <- tibble(
  age_group = c("<26", "26-30", "31-35", "36-40", ">40"),
  prop = c(45.7, 21.2, 14.7, 10.6, 7.8)
) %>%
  arrange(desc(prop)) %>% 
  mutate(age_group = factor(age_group, age_group)) %>% 
  arrange(desc(age_group)) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

a_pie <- ggplot(age, aes(x = "", y = prop, fill = age_group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white", size = 3.5) +
  scale_fill_manual(name = "Age",
                    values = mycols) +
  theme_void() +
  theme(legend.justification = "left")

# Race/ethnicity
race <- tibble(
  race = c("White", "Black", "Hispanic", "Other"),
  prop = c(744770/1326200, 215601/1326200, 221554/1326200, NA_real_)
) %>% 
  mutate(
    prop = if_else(is.na(prop), 1 - sum(prop, na.rm = TRUE), prop),
    prop = round(prop*100, 1)
  ) %>%
  arrange(match(race, c("White", "Black", "Hispanic", "White"))) %>% 
  mutate(race = factor(race, race)) %>% 
  arrange(match(race, c("Other", "Hispanic", "Black", "White"))) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

r_pie <- ggplot(race, aes(x = "", y = prop, fill = race)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white", size = 3.5) +
  scale_fill_manual(name = "Race/ethnicity",
                    values = mycols) +
  theme_void() +
  theme(legend.justification = "left")

# Combine in one figure
pie <- ggarrange(s_pie, a_pie, r_pie, align = "v",
                 ncol = 1, nrow = 3, widths = c(1, 0.5, 0.5, 1))

# Output figure
ggsave("source_population_pie_charts.pdf", 
       width = 3.5, height = 6, device = cairo_pdf)

# Study participants demographics
# Right side of Figure 1A

# Read in dataset
bsl_chr_all <- read_csv("baseline_chr_all.csv")

# Sex
sex_sub <- bsl_chr_all %>% 
  group_by(sex) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(
    prop = n / sum(n),
    prop = round(prop*100, 1)
  ) %>% 
  arrange(desc(prop)) %>% 
  mutate(sex = factor(sex, sex)) %>% 
  arrange(desc(sex)) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) 

s_pie_sub <- ggplot(sex_sub, aes(x = "", y = prop, fill = sex)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = prop), color = "white", size = 3.5) +
  scale_fill_manual(name = "Sex",
                    values = mycols) +
  theme_void() +
  theme(legend.justification = "left")

# Age
age_sub <- bsl_chr_all %>% 
  filter(!is.na(age_baseline)) %>% 
  group_by(age_baseline) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(
    prop = n / sum(n),
    prop = round(prop*100, 1),
    order = case_when(age_baseline == "<26" ~ 1,
                      age_baseline == "26-30" ~ 2,
                      age_baseline == "31-35" ~ 3,
                      age_baseline == "36-40" ~ 4,
                      age_baseline == ">40" ~ 5)
  ) %>% 
  arrange(desc(order)) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

a_pie_sub <- ggplot(age_sub, aes(x = "", y = prop, fill = fct_reorder(age_baseline, order))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(data = age_sub %>% filter(order <= 3),
            aes(y = lab.ypos, label = prop), color = "white", size = 3.5) +
  scale_fill_manual(name = "Age, first sample",
                    values = mycols) +
  theme_void() +
  theme(legend.justification = "left")

# Race/ethnicity
race_sub <- bsl_chr_all %>%
  filter(!is.na(race)) %>% 
  group_by(race) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(
    prop = n / sum(n),
    prop = round(prop*100, 1)
  ) %>% 
  arrange(desc(prop)) %>% 
  mutate(race = factor(race, race)) %>% 
  arrange(desc(race)) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) 

r_pie_sub <- ggplot(race_sub, aes(x = "", y = prop, fill = race)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(data = race_sub %>% filter(prop > 5), aes(y = lab.ypos, label = prop), color = "white", size = 3.5)+
  scale_fill_manual(name = "Race/ethnicity",
                    values = mycols) +
  theme_void() +
  theme(legend.justification = "left")

# Combine into one figure
pies_sub <- ggarrange(s_pie_sub, a_pie_sub, r_pie_sub, align = "v",
                      ncol = 1, nrow = 3)

# Output figure
ggsave("study_population_pie_charts.pdf", 
       width = 3.5, height = 6, device = cairo_pdf)

### Figure 1B ###

# Read in dataset
bsl_chr_cases <- read_csv("baseline_chr_cases.csv")

median_onset <- bsl_chr_cases %>% 
  ungroup() %>% 
  summarise(median = median(ageonset)) %>% 
  pull(median)

median_age <- bsl_chr_cases %>% 
  ungroup() %>% 
  summarise(median = median(age_baseline)) %>% 
  pull(median)

fig1b <- bsl_chr_cases %>%
  select(age_baseline, ageonset) %>% 
  pivot_longer(age_baseline:ageonset, names_to = "variable", values_to = "values") %>% 
  mutate(variable = factor(variable, levels = c("age_baseline", "ageonset"))) %>% 
  ggplot(aes(x = values, fill = variable)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = median_age), color = "#009E73", linetype = "dashed") +
  geom_vline(aes(xintercept = median_onset), color = "#0072B2", linetype = "dashed") +
  scale_fill_manual(values = mycols,
                    labels = c("Age at first sample", "Age at MS onset")) +
  theme_minimal() +
  xlab("Age , years") +
  ylab("Density") +
  expand_limits(x = c(15, 40)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

### Figure 1C ###

# Read in dataset
interval <- read_csv("ebv_data.csv")

fig1c <- interval %>%
  group_by(id) %>%
  arrange(id, serumn) %>% 
  filter(casestat == 1, first(ebv_pos) == 0) %>%
  mutate(time_to_ms = time_to_MS*-1,
         samples_cat = case_when(serumn == 1 ~ "1st",
                                 serumn == 2 ~ "2nd",
                                 serumn == 3 ~ "3rd"),
         samples_cat = factor(samples_cat)) %>% 
  ggplot(aes(x = samples_cat, y = time_to_ms)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(width = 0.5, fill = "#D55E00") +
  coord_flip() +
  scale_x_discrete(limits = c("3rd","2nd","1st"),
                   name = "Serum sample") +
  scale_fill_manual(guide = "none",
                    values = mycols) +
  ylab("Time to MS onset, years") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"))

# Combine into one figure
fig1bc <- ggarrange(fig1b, fig1c, ncol = 2, nrow = 1, labels = c("B", "C"))

# Output figure
ggsave("figure1bc.pdf", 
       width = 8, height = 3, device = cairo_pdf)    
