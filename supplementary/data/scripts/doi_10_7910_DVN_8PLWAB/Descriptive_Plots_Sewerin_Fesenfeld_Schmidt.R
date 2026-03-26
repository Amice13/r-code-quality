## authors:   Sebastian Sewerin, Lukas P. Fesenfeld, Tobias Schmidt, Yash Dubey
## contact:   lukas.fesenfeld@ir.gess.ethz.ch
## file name: stickiness_paper_plotting_code
## Context:   Plotting Code for Paper "The Role of Policy Design in Explaining Policy Continuity and Supersession"
## Date:      2023-07-06


# Import Libraries -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(tidyr)
library(cowplot)

# Read relevant cleaned data
data2_sm <- read_csv("data2_sm.csv")
data2_sm <- data2_sm %>%
  mutate(Country = str_replace_all(Country, "UK", "United Kingdom"))
data2_sm <- data2_sm %>%
  rename("Research and Development" = "R.D",
         "Public Investment" = "Public.Investment")


# Pre-assign variables.
IPA <- data2_sm$IPA
country <- data2_sm$Country
year <- data2_sm$Start.Year.New.Num
tech_specificity <- data2_sm$Technology.Specificity

# Get average policy sensitivity for each country for each year
average_data <- aggregate(IPA ~ country + year, data = data2_sm, FUN = mean)

ylimit <- max(average_data$IPA)
# Create separate plots for each country
plot_list <- split(average_data, average_data$country)

# Generate individual plots 
for (country_plot in plot_list) {
  country <- unique(country_plot$country)
  p <- ggplot(country_plot, aes(x = factor(year), y = IPA)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Year", y ="") +
    theme_minimal()+
    ylim(0,ylimit)+
    ggtitle(country) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  filename <- paste(" Policy_Stringency_for_", country,".jpeg", sep = "")
  
  # Save the plot with the filename
  ggsave(filename = filename, plot = p, device = "jpeg", path = getwd())
  
  # Print each plot to the plotter.
  print(p)
}



# Data grouped by country and start year
grouped_data <- data2_sm %>% 
  group_by(Country, Start.Year.New.Num)

# Get average tech specificity of policies by country and year.
average_data <- grouped_data %>% 
  summarize(avg_Tech.Spec = mean(IPA.Tech.Spec))
ylimit <- max(average_data$avg_Tech.Spec)

# Create separate plots for each country
for (country in unique(grouped_data$Country)) {
  country_data <- subset(average_data, Country == country)
  
  p <- ggplot(country_data, aes(x = Start.Year.New.Num, y = avg_Tech.Spec)) +
    geom_bar(stat = "identity") +
    labs(x = "Year", y ="") +
    theme_minimal() + 
    ylim(0,ylimit) + 
    ggtitle(country) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  filename <- paste(" Technology_Specificity_for_", country,".jpeg", sep = "")
  
  # Save the plot with the filename
  ggsave(filename = filename, plot = p, device = "jpeg", path = getwd())
  
  # Print each plot to the plotter.
  print(p)
}

# Select columns that indicate instrument type
stacked_data <- data2_sm %>%
  select(Country, Start.Year.New.Num, Integration, Incentives, 'Public Investment', Tradable, Voluntary, Education, Financial, 'Research and Development', Regulatory, Framework)

# Convert data to long format
melted_data <- stacked_data %>%
  pivot_longer(cols = Integration:Framework, names_to = "variable", values_to = "value") %>%
  filter(value == 1)

# Calculate count of each variable in each year for each country
count_data <- melted_data %>%
  group_by(Country, Start.Year.New.Num, variable) %>%
  summarize(count = n())

total_counts <- count_data %>% 
  group_by(Start.Year.New.Num, Country) %>%
  summarize(max_total_count = max(sum(count)))

ylimit = max(total_counts$max_total_count)


# Create separate stacked bar plots for each country
for (country in unique(count_data$Country)) {
  country_data <- count_data %>%
    filter(Country == country)
  
  p <- ggplot(country_data, aes(x = factor(Start.Year.New.Num), y = count, fill = variable)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "white", size = 3) +
    labs(x = "Year", y = "", fill = "Instrument Type") +
    scale_color_viridis_d() +  
    theme_minimal() +
    guides(fill = guide_legend(title = NULL)) +  # Create a dummy legend
    ylim(0, ylimit) +  # You need to use ylim for vertical bar plots
    ggtitle(country) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")  # Hide the legend in the main plot
  
  # Save the main plot
  filename <- paste("Policy_Count_Instrument_Type_", country, ".jpeg", sep = "")
  ggsave(filename = filename, plot = p, device = "jpeg", path = getwd())
  print(p)
}   

# Section to extract legend from the plot.
p <- ggplot(country_data, aes(x = count, y = factor(Start.Year.New.Num), fill = variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(x = "", y = "Year", fill = "Instrument Type") +
  scale_color_viridis_d() +  
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  xlim(0, ylimit) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top",  # Position the legend at the top
        legend.direction = "horizontal")  # Make the legend horizontal

# Extract the legend using cowplot::get_legend() and customize it
legend <- get_legend(p)
legend_plot <- cowplot::plot_grid(NULL, legend, ncol = 1, rel_widths = c(0.8, 0.2))
  
# Adjust the legend's appearance (font size)
legend_plot <- legend_plot + 
  theme(legend.text = element_text(size = 12))

# Save the legend plot as a separate JPEG
legend_filename <- paste("Legend_Policy_Count_Instrument_Type.jpeg")
ggsave(filename = legend_filename, plot = legend_plot, device = "jpeg", path = getwd())
# End of section
  
library(stringr)

# Select data for control variables
controls <- data2_sm %>%
  select(Country, Start.Year.New.Num, Oil_price_brent, GDP_capita, GDP_growth, Change.in.Govt, Veto.Player.Index, Green.Party.Seat.Share)

# Modify column names to title case
colnames(controls)[-c(1, 2)] <- str_to_title(str_replace_all(colnames(controls)[-c(1, 2)], "[._]", " "))

# Group by country and year then summarize to obtain value unique to each year
grouped_controls <- controls %>%
  group_by(Country, Start.Year.New.Num) %>%
  summarise(across(everything(), first))

# Iterate over each country
for (country in unique(grouped_controls$Country)) {
  country_data <- subset(grouped_controls, Country == country)
  
  # Create separate bar plots for each variable within the country
  for (var in colnames(country_data)) {
    if (var != "Country" && var != "Start.Year.New.Num") {
      p <- ggplot(country_data, aes(x = Start.Year.New.Num, y = .data[[var]])) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Year", y = var) +
        ggtitle(paste("Trend in", var, "for:", country)) +
        theme_minimal()
        filename <- paste("Trend in_", var,"_for_", country,".jpeg", sep = "")
      
      # Save the plot with the filename
      ggsave(filename = filename, plot = p, device = "jpeg", path = getwd())
      # Print plot to plotter
      print(p)
    }
  }
}





