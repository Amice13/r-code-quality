library(tidyverse)
library(imputeTS)
library(patchwork)
library(ggh4x)
library(ggpubr)
library(quanteda)
library(thematic)
library(gghighlight)

#####Balance sheets
#Load balance sheet data
balance_sheets <- read_csv("data/cb_balance_sheet.csv", 
                           col_types = cols(year = col_date(format = "%Y-%m")))

#Impute missing values (linear interpolation)
balance_sheets$value <- na_interpolation(balance_sheets$value, option = "linear", maxgap = 10)

#Summarize non-QE group
non_qe <- balance_sheets |> 
  filter(qe == "Non-QE") |> 
  summarize(value = mean(value, na.rm = TRUE), .by = c(year, qe, qe_dummy)) |> 
  mutate("country" = "All non-QE countries") |> 
  mutate("central_bank" = "All non-QE central banks")

#Join
balance_sheets <- balance_sheets |> 
  filter(qe == "QE") |> 
  full_join(non_qe)

#QE periods data frame
qe_periods <- balance_sheets |> 
  select(year, central_bank, qe_dummy) |> 
  filter(qe_dummy == 1) |> 
  group_by(central_bank) |> 
  arrange(year) |> 
  slice(c(1,n())) |> 
  mutate("start_end" = seq(1, 2)) |> 
  pivot_wider(names_from = start_end,
              values_from = year) |> 
  rename("Start date" = `1`, 
         "End date" = `2`) |> 
  mutate(qe_dummy = NULL)

#Join
balance_sheets <- balance_sheets |> 
  left_join(qe_periods)

#Plot
p1 <- balance_sheets |> 
  filter(year > "1999-12-01") |> 
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  geom_rect(aes(xmin = `Start date`,
                xmax = `End date`,
                ymin=0,ymax=Inf,
                fill = "manual"),
            alpha = 0.008) +
  scale_fill_manual(values = okabe_ito()) +
  labs(x = NULL,
       y = "% of GDP") +
  facet_wrap(vars(central_bank), scales = "free_y") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),)

p1 + ggh4x::facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(5, 40)),
      scale_y_continuous(limits = c(5, 40)),
      scale_y_continuous(limits = c(12.5, 105)),
      scale_y_continuous(limits = c(5, 40)),
      scale_y_continuous(limits = c(5, 40)),
      scale_y_continuous(limits = c(5, 40))
    )
  )

ggsave("plots/balance_sheets.pdf", device = "pdf")


####
#House prices
load("data/df_main.RData")

house_prices <- Doc_statistics_agg |> 
  select(Central_Bank, Time, Property_prices)
  
house_prices$Time <- parse_date_time(house_prices$Time, "y%q")

house_prices$Time <- as_date(house_prices$Time)

house_prices <- as_tibble(house_prices)

house_prices <- unique(house_prices)

house_prices <- house_prices |> 
  mutate(
    country = case_match(Central_Bank,
              "Bank of England" ~ "UK",
              "ECB" ~ "Euro area",
              "FED" ~ "USA",
              "Bank of Sweden" ~ "Sweden",
              "Bank of Japan" ~ "Japan",
              .default = "Non-QE")
  )

house_prices <- house_prices |> 
  summarize(Property_prices = mean(Property_prices, na.rm = TRUE), .by = c(Time, country))

house_prices_rebased <- house_prices |> 
  mutate(Property_prices = Property_prices/Property_prices[match("2006-10-01", Time)], .by = country)

house_prices_rebased |> 
ggplot(aes(x = Time, y = Property_prices*100, color = country)) +
  geom_line(linewidth = 0.7) +
  gghighlight() +
  labs(x = NULL,
       y = "House price index",
       color = "Country") +
  scale_color_manual(values = okabe_ito()) +
  ylim(c(50, 215)) +
  expand_limits(x = as.Date(x = c("2000-01-01","2022-01-01"))) +
  theme_bw(base_size = 15) +
  theme(panel.grid.minor = element_blank())

ggsave("plots/house_prices.eps", device = "eps")


house_prices_rebased |> 
  ggplot(aes(x = Time, y = Property_prices*100)) +
  geom_line(linewidth = 0.7) +
  labs(x = NULL,
       y = "House price index") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  facet_wrap(vars(country)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank())

ggsave("plots/house_prices_facets.eps", device = "eps")