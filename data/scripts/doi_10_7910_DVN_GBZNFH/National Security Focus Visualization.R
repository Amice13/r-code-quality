install.packages("cli")
library(devtools)
devtools::install_github("coolbutuseless/ggpattern")
devtools::install_github("coolbutuseless/ggpattern", force = TRUE)
library(ggpattern)
library(ggplot2)
library(dplyr)


data_2 <- read.csv("D:/Class Material/PHD Stuff/Research Projects/China Cyberspace Securitization Study/Data Visualizations/National Security Discourse(Metadata).csv")
# Exclude rows where the 'Note' column contains "No mention"
data_2_clean <- data_2 %>%
  filter(!grepl("No mention", Note, ignore.case = TRUE))

# Exclude items where 'Doc_Type' is "Resolution"
data_2_no_resolution <- data_2_clean %>%
  filter(Doc_Type != "Resolution")

# Summarize data by Year and Doc_Type (Document Type)
data_summary <- data_2_no_resolution %>%
  group_by(Year, Doc_Type) %>%
  summarize(Count = n(), .groups = 'drop')  # Ensures the data is ungrouped after summarizing

data_summary$Doc_Type <- factor(data_summary$Doc_Type, levels = c("Communique", "Reports", "Remarks"))

# Create the stacked bar chart with custom colors for each document type
ggplot(data_summary, aes(x = as.factor(Year), y = Count, fill = Doc_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Annual Security Mentions in Official Documents",
       x = "Year",
       y = "Count of Narratives",
       fill = "Document Type") +
  theme_minimal() +
  theme(legend.position = "right") +
  # Set custom colors for each document type
  scale_fill_manual(values = c(
    "Communique" = "lightblue",    # Set Communique to light blue
    "Reports" = "steelblue",       # Set Reports to steel blue
    "Remarks" = "coral"            # Set Remarks to coral
  ))



# Create the stacked bar chart (use this)
ggplot(data_grouped, aes(x = Year, y = Count, fill = Natl_Security_Focus)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "National Security Focus by Year",
       x = "Year",
       y = "Count of Entries",
       fill = "National Security Focus") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Making the stacked bar chart showing the allocation of Tone Rhetoric
# Summarize data to get counts of each 'Tone_Rhetoric' by 'Year'
data_grouped <- data_2_no_resolution %>%
  group_by(Year, Tone_Rhetoric) %>%
  summarise(Count = n(), .groups = 'drop')  # .groups='drop' avoids a nested grouping warning

# Create the stacked bar chart (Use this)
ggplot(data_grouped, aes(x = Year, y = Count, fill = Tone_Rhetoric)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "National Security Focus by Tone Rhetoric",
       x = "Year",
       y = "Count of Entries",
       fill = "Tone Rhetoric") +
  theme_minimal() +
  theme(legend.position = "bottom")
