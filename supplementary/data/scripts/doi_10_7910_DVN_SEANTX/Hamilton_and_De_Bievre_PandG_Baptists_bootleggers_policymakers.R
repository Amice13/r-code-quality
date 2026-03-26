### Load necessary libraries
library(tidyverse)
library(dplyr)

### Step 1: Load original Twitter Data
all_tweets_with_users <- read_csv("CSV/matched_data_2025.csv", col_types = cols(
  tweet_id = col_character(),
  author_id = col_character()
))
glimpse(all_tweets_with_users)

### Step 2: Filter tweets by country and preprocess
# Filter for relevant countries and preprocess
filtered_scraped <- all_tweets_with_users %>%
  filter(country_code %in% c("fr", "ie", "py", "uy", "br", "ar")) %>%
  mutate(
    truncated_text = text %>%
      str_replace("\\.\\.\\.$", "") %>%  # Remove trailing "..."
      str_to_lower() %>%                # Convert to lowercase
      str_trim()                        # Remove leading/trailing whitespace
  )

### Step 3: Load and combine rescraped tweet text
# Load rescraped tweet data
# Combine all rescraped data files
folder_path <- "CSV/rescrape2025"
file_list <- list.files(path = folder_path, pattern = "tweet_texts.*\\.csv", full.names = TRUE)

rescraped_data <- file_list %>%
  map_dfr(~ read_csv(.x, col_types = cols(tweet_id = col_character()))) %>%
  distinct(tweet_id, tweet_text)  # Ensure unique entries

# Also include tweets from the post-scrape file
post_scrape <- read_csv("CSV/tweet_texts_post_scrape.csv", col_types = cols(tweet_id = col_character())) %>%
  distinct(tweet_id, tweet_text)

# Combine all rescraped data
rescraped_data_combined <- bind_rows(rescraped_data, post_scrape) %>%
  distinct(tweet_id, tweet_text)  # Ensure unique entries

### Step 4: Preprocess tweet text
# Preprocess rescraped data
rescraped_data_combined <- rescraped_data_combined %>%
  mutate(
    tweet_text = tweet_text %>%
      str_to_lower() %>%                # Convert to lowercase
      str_replace_all("[[:punct:]]", "") %>%  # Remove punctuation
      str_trim()                        # Remove leading/trailing whitespace
  )

### Step 5: Join tweet text to original data by tweet ID
# Match by tweet_id
matched_data <- filtered_scraped %>%
  left_join(rescraped_data_combined, by = "tweet_id", suffix = c("", "_rescraped"))

### Step 6: Construct final tweet text column (with fallback logic)
# Create `final_text` with prioritization
matched_data <- matched_data %>%
  mutate(
    final_text = case_when(
      !is.na(tweet_text) & !str_starts(tweet_text, regex("error", ignore_case = TRUE)) ~ tweet_text,  # Use valid tweet_text
      TRUE ~ text  # Fallback to original text
    )
  )

unmatched_tweets <- matched_data %>%
  filter(is.na(final_text))

# Summary of results
processed_count <- sum(!is.na(matched_data$final_text))
unprocessed_count <- nrow(matched_data) - processed_count

cat("Total tweets processed:", processed_count, "\n")
cat("Unprocessed tweets:", unprocessed_count, "\n")
cat("Processing coverage:", round((processed_count / nrow(matched_data)) * 100, 2), "%\n")

##### matched_data now contains my best effort to reconstruct the text using selenium in february 2025
##### no add user coding

library(tidyverse)

### Step 1: Load original tweet-user data
# Load FR_KEY.csv and IE_KEY.csv
FR_key <- read_delim("CSV/FR_KEY.csv", delim = ";", col_names = TRUE, show_col_types = FALSE) %>%
  select(username, country_code, class, notes)

IE_key <- read_delim("CSV/IE_KEY.csv", delim = ";", col_names = TRUE, show_col_types = FALSE) %>%
  select(username, country_code, class, notes)

### Step 2: Filter tweets by country and preprocess
# Convert usernames to lowercase
FR_key <- FR_key %>%
  mutate(username = str_to_lower(username))

IE_key <- IE_key %>%
  mutate(username = str_to_lower(username))

### Step 3: Load and combine rescraped tweet text
# Combine the keys and remove duplicates
combined_key <- bind_rows(FR_key, IE_key) %>%
  distinct(username, .keep_all = TRUE)  # Keep the first occurrence of each username

### Step 4: Preprocess tweet text
# Re-label `class` values based on `notes`
combined_key <- combined_key %>%
  mutate(class = case_when(
    class == "baptist" & notes == "activist" ~ "individual",
    TRUE ~ class  # Keep existing class otherwise
  ))
glimpse(combined_key)

### Step 5: Join tweet text to original data by tweet ID
# Merge with matched_data
matched_data_with_classes <- matched_data %>%
  mutate(username = str_to_lower(username)) %>%  # Ensure lowercase for consistency
  left_join(combined_key, by = "username")

# Inspect the merged dataset
glimpse(matched_data_with_classes)
glimpse(matched_data)

####Now filter jsut france and ireland

# Filter for France and Ireland based on `country_code`
glimpse(matched_data_with_classes)

france_ireland_data <- matched_data_with_classes %>%
  filter(country_code.x %in% c("fr", "ie"))

# Inspect the filtered dataset
glimpse(france_ireland_data)


cat("Filtered data for France and Ireland saved to 'CSV/france_ireland_data.csv'.\n")

table(france_ireland_data$class)

#####ANALYSIS BEGINS HERE######
# Start by labeling narratives#
# Load necessary libraries
library(dplyr)
library(stringi)
library(quanteda)



###New dictionary with word stems, too:
dict <- dictionary(list(
  fires_en = c("deforestation", "amazon", "environment", "environmental", "environmentalist", 
               "environmentalists", "climate", "greenhouse", "amazonia", "rainforest", "forest", 
               "indigenous", "ecosystems", "biodiversity", "planet", "fires", 
               "deforest*", "amazo*", "envir*", "clim*", "green*", "bio*", "indigen*", "rainforest*"),
  fires_fr = c("planete", "ecolos", "deforestation", "environnement*", "lenvironnement", "climat", 
               "climatiques", "menti", "foret", "amazon", "biodiversite", "ecologique", 
               "ecologi*", "amazonie", "amazo*", "brule", 
               "deforest*", "envir*", "clim*", "bio*", "foret*", "brul*"),
  fires_sp = c("amazonas", "clima", "amazonia", "deforestation", "ecologi*", "incendios", 
               "medioambiental", "forestales", "natura", "gas", 
               "deforest*", "amazo*", "clim*", "medioamb*", "forest*", "natura*"),
  fire_po = c("fires", "climat", "amazon", "amazonia", "clim", "ambiental", "environment*", "deforestation", 
              "amazo*", "clim*", "envir*", "ambient*", "bio*", "forest*"),
  farmers_en = c("farmers", "farming", "beef", "agriculture", "exports", 
                 "farm*", "beef*", "agri*", "export*", "livestock*"),
  farmers_fr = c("agricole", "volaille", "filagricole", "produits", "importation", "elevage", 
                 "agriculteurs", "agriculte", "l’agriculture", "viande", "viandes", "bovine", "boeuf", 
                 "eleveurs", 
                 "agri*", "eleve*", "viand*", "produit*"),
  farmers_sp = c("agricultores", "carne", "productos", "proteccionismo", "competitividad", 
                 "export", "agricultura", "aranceles", "agricolas", 
                 "agri*", "ganad*", "product*", "carne*"),
  farmers_po = c("producers", "comercial", "agrobusiness", "agricultural", "carne", "bovine", "beef", 
                 "agri*", "pecu*", "produt*", "bovi*")
))

# Preprocess text column
france_ireland_data <- france_ireland_data %>%
  mutate(text_cleaned = stri_trans_general(final_text, "Latin-ASCII") %>%
           tolower() %>%
           str_replace_all("[[:punct:]]", " ") %>%
           str_trim())

# Create a corpus and tokenize the text
corp <- corpus(france_ireland_data$text_cleaned)
tokens <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) %>% tokens_tolower()

# Apply dictionary to identify term matches
dfm <- dfm_lookup(dfm(tokens), dictionary = dict)

# Summarize dictionary term matches
narrative_matches <- convert(dfm, to = "data.frame") %>%
  bind_cols(france_ireland_data) %>%
  mutate(
    FIRES = rowSums(select(., starts_with("fires_"))) > 0,
    FARMERS = rowSums(select(., starts_with("farmers_"))) > 0
  )

# Inspect the output
glimpse(narrative_matches)

####Option to save output and start from here###
###narrative_matches <- read_csv(file="CSV/narrative_matches_2025.csv")

library(tidyr)
library(lubridate)
library(dplyr)

# Reshape data to long format
narrative_long <- narrative_matches %>%
  pivot_longer(
    cols = c(FIRES, FARMERS),  # Columns to reshape
    names_to = "narrative",    # New column to hold narrative names
    values_to = "narrative_match"  # New column for narrative match values
  )

# Create time bins and summarize
time_binned_results <- narrative_long %>%
  mutate(time_bin = floor_date(created_at, "quarter")) %>%  # Bin by quarter
  group_by(narrative, class, time_bin) %>%  # Group by narrative, class, and time bin
  summarize(
    n_obs = n(),  # Count observations in each time bin
    avg_probability = mean(narrative_match, na.rm = TRUE)  # Calculate average probability
  ) %>%
  ungroup()

library(ggplot2)

# Create the plot
ggplot(time_binned_results, aes(x = time_bin, y = avg_probability, color = class, group = class)) +
  geom_line(size = 1) +  # Line for each class
  geom_point(aes(size = n_obs), alpha = 0.6) +  # Points scaled by observation count
  facet_wrap(~narrative, scales = "free_y") +  # Facet by narrative
  labs(
    title = "Change in Narrative Use Over Time (Quarterly)",
    x = "Time (Quarterly Bins)",
    y = "Average Probability of Resonance",
    color = "Class",
    size = "Observations per Time Bin"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

####
library(ggplot2)

# Ensure all desired classes are included
time_binned_results <- time_binned_results %>%
  filter(class %in% c("baptist", "bootlegger", "public_policy_maker", "individual")) %>%
  mutate(
    time_numeric = as.numeric(as.Date(time_bin)),  # Convert time_bin to numeric for regression
    breakpoints = case_when(
      time_bin == as.Date("2015-12-12") ~ "Breakpoint 1",
      time_bin == as.Date("2016-02-01") ~ "Breakpoint 2",
      time_bin == as.Date("2019-12-01") ~ "Breakpoint 3",
      time_bin == as.Date("2019-06-29") ~ "Breakpoint 4",
      time_bin == as.Date("2019-08-23") ~ "Breakpoint 5",
      TRUE ~ "Other"
    )
  )

# Plot with polynomial regression trendlines
ggplot(time_binned_results, aes(x = time_numeric, y = avg_probability, color = class)) +
  geom_point(aes(size = n_obs), alpha = 0.6) +  # Scatterplot points, size reflects n_obs
  geom_smooth(
    aes(group = class), 
    method = "lm", 
    formula = y ~ poly(x, 3),  # Third-degree polynomial regression
    se = FALSE, 
    size = 1
  ) + 
  facet_wrap(~narrative, scales = "free_y") +  # Facet for FIRES and FARMERS
  scale_x_continuous(
    breaks = as.numeric(as.Date(c("2015-12-12-", "2016-02-01", "2018-12-01", "2019-06-29", "2019-08-23"))),
    labels = c("Paris Climate Agreement (PCA)", "Copa-Cogeca links farming and climate", "Macron threatens to withdraw", "Agreement in principle", "Macron withdraws")
  ) +
  labs(
    title = "Polynomial Trendlines of Narrative Use (Quarterly Bins)",
    x = "Time (Quarterly Bins with Key Dates Highlighted)",
    y = "Average Probability of Resonance",
    color = "Class",
    size = "Observations per Time Bin"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

####update dates and labels
ggplot(time_binned_results, aes(x = time_numeric, y = avg_probability, color = class)) +
  geom_point(aes(size = n_obs), alpha = 0.6) +  # Scatterplot points, size reflects n_obs
  geom_smooth(
    aes(group = class), 
    method = "lm", 
    formula = y ~ poly(x, 3),  # Third-degree polynomial regression
    se = FALSE, 
    size = 1
  ) + 
  facet_wrap(~narrative, scales = "free_y") +  # Facet for FIRES and FARMERS
  scale_x_continuous(
    breaks = as.numeric(as.Date(c("2015-12-12", "2016-02-01", "2018-12-01", "2019-06-29", "2019-08-23"))),
    labels = c(
      "Paris Agreement", 
      "First issue linkage", 
      "Macron G20 threat", 
      "Agreement in principle", 
      "Macron withdraws"
    )
  ) +
  labs(
    title = "Polynomial Trendlines of Narrative Use (Quarterly Bins)",
    x = "Time (Quarterly Bins with Key Dates Highlighted)",
    y = "Average Probability of Resonance",
    color = "Class",
    size = "Observations per Time Bin"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Legend position
    axis.text.x = element_text(angle = 60, hjust = 1)  # Adjust x-axis text
  )

glimpse(time_binned_results)

####When did the narrative shift?
library(dplyr)
library(ggplot2)
library(lubridate)

# Filter for individuals and narratives of interest
filtered_data <- time_binned_results %>%
  filter(class == "individual", narrative %in% c("FARMERS", "FIRES"))

## Convert time_bin to Date
filtered_data <- filtered_data %>%
  mutate(time_bin = as.Date(time_bin))

# Plotting
ggplot(filtered_data, aes(x = time_bin, y = avg_probability, color = narrative)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2019-08-31"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2019-08-31"), y = max(filtered_data$avg_probability, na.rm = TRUE), 
           label = "Agreement stalled", vjust = -0.5, hjust = 1.1, angle = 90) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(title = "Narrative Use by Individuals Over Time",
       subtitle = "Dashed line marks August 2019 cutoff",
       x = "Time",
       y = "Average Narrative Use (Probability)",
       color = "Narrative") +
  theme_minimal(base_size = 13)

####PLot only narrative trend lines:


library(dplyr)
library(ggplot2)
library(lubridate)

# Filter for individuals and narratives of interest
filtered_data <- time_binned_results %>%
  filter(class == "individual", narrative %in% c("FARMERS", "FIRES")) %>%
  mutate(time_bin = as.Date(time_bin))

# Plot with smoothed trend lines only
ggplot(filtered_data, aes(x = time_bin, y = avg_probability, color = narrative)) +
  geom_smooth(method = "loess", se = FALSE, span = 2, size = 1.2) +
  geom_vline(xintercept = as.Date("2019-08-31"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2019-08-31"), y = max(filtered_data$avg_probability, na.rm = TRUE),
           label = "Ratification stalled", vjust = -0.5, hjust = 1.1, angle = 90) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(title = "Narrative Use by Individuals Over Time",
       subtitle = "Trend lines highlight narrative shift; dashed line = August 2019 cutoff",
       x = "Time",
       y = "Proportion of Narrative Use (Smoothed)",
       color = "Narrative") +
  theme_minimal(base_size = 13)


##### Try with horizaontal bar labels

# Plot with smoothed trend lines and horizontal labels for periods
ggplot(filtered_data, aes(x = time_bin, y = avg_probability, color = narrative)) +
  geom_smooth(method = "loess", se = FALSE, span = 2, size = 1.2) +
  geom_vline(xintercept = as.Date("2019-08-31"), linetype = "dashed", color = "black") +
  
  # Agreement Pending: move left
  annotate("segment", x = as.Date("2019-08-31"), xend = as.Date("2015-01-01"),
           y = 1.02, yend = 1.02, arrow = arrow(length = unit(0.2, "cm")), color = "gray30") +
  annotate("text", x = as.Date("2018-5-01"), y = .93, label = "Agreement Pending", size = 4, color = "gray30") +
  
  # Ratification Stalled: move right
  annotate("segment", x = as.Date("2019-09-01"), xend = as.Date("2022-03-01"),
           y = 1.02, yend = 1.02, arrow = arrow(length = unit(0.2, "cm")), color = "gray30") +
  annotate("text", x = as.Date("2020-11-01"), y = .93, label = "Ratification Stalled", size = 4, color = "gray30") +
  
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(title = "",
       subtitle = "",
       x = "Time",
       y = "Proportion of Narrative Use",
       color = "Narrative") +
  coord_cartesian(ylim = c(0, 1.05)) +  # Adjust y-axis to make room for annotations
  theme_minimal(base_size = 13)

#### ANALYSIS OF CONGRUENCE OVER TIME


###Compute narrative scores for each tweet
glimpse(narrative_matches)
table(narrative_matches$class)
narrative_matches <- narrative_matches %>%
  mutate(
    FIRES_score = pmax(fires_en, fires_fr, fires_sp, fire_po, na.rm = TRUE),
    FARMERS_score = pmax(farmers_en, farmers_fr, farmers_sp, farmers_po, na.rm = TRUE)
  )

summary(narrative_matches$FIRES_score)
summary(narrative_matches$FARMERS_score)


narrative_matches %>%
  filter(created_at == "2021-04-05 09:33:51") %>%
  select(starts_with("fire"), starts_with("farmers"), FIRES_score, FARMERS_score)
str(narrative_matches$created_at)
attr(narrative_matches$created_at, "tzone")
narrative_matches %>%
  mutate(ts_char = as.character(created_at)) %>%
  filter(ts_char == "2021-04-05 09:33:51")
narrative_matches <- narrative_matches %>%
  mutate(created_at = lubridate::floor_date(created_at, unit = "second"))

###Filter relevant data
filtered_matches <- narrative_matches %>%
  select(class, created_at, FIRES_score, FARMERS_score) %>%
  filter(!is.na(class))

###Create monthly bins:
filtered_matches <- filtered_matches %>%
  mutate(month_bin = as.Date(cut(created_at, "month")))


filtered_scores <- filtered_matches %>%
  group_by(class, month_bin) %>%
  summarize(
    avg_FIRES_score = mean(FIRES_score, na.rm = TRUE),
    avg_FARMERS_score = mean(FARMERS_score, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(filtered_scores)

#### I will test the hypothesis that individuals' narrative use drives congruence among other actor types
aggregated_scores <- filtered_scores %>%
  group_by(month_bin, class) %>%
  summarise(
    avg_FIRES = mean(avg_FIRES_score, na.rm = TRUE),
    avg_FARMERS = mean(avg_FARMERS_score, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = class,
    values_from = c(avg_FIRES, avg_FARMERS)
  )
normalize_score <- function(score, max_score) {
  ifelse(score == 0, 0, 50 + 50 * (score / max_score))
}

aggregated_scores <- aggregated_scores %>%
  mutate(
    norm_FIRES_baptist = normalize_score(avg_FIRES_baptist, 7),
    norm_FIRES_bootlegger = normalize_score(avg_FIRES_bootlegger, 7),
    norm_FIRES_public_policy_maker = normalize_score(avg_FIRES_public_policy_maker, 7),
    norm_FIRES_individual = normalize_score(avg_FIRES_individual, 7),
    norm_FARMERS_baptist = normalize_score(avg_FARMERS_baptist, 5),
    norm_FARMERS_bootlegger = normalize_score(avg_FARMERS_bootlegger, 5),
    norm_FARMERS_public_policy_maker = normalize_score(avg_FARMERS_public_policy_maker, 5),
    norm_FARMERS_individual = normalize_score(avg_FARMERS_individual, 5)
  )

calculate_congruence <- function(scores) {
  # Remove NA values
  scores <- na.omit(scores)
  
  # Return NA if there are fewer than two non-NA values
  if (length(scores) < 2) {
    return(NA)
  }
  
  # Compute congruence for pairs
  congruence_values <- combn(scores, 2, function(pair) {
    if (any(pair == 0)) {
      0  # If either score is zero, congruence is zero
    } else {
      1 - abs(diff(pair)) / 100  # Calculate congruence
    }
  })
  
  # Return the mean congruence, excluding any potential NAs
  mean(congruence_values, na.rm = TRUE)
}

aggregated_scores <- aggregated_scores %>%
  rowwise() %>% # Row-wise operation for each month
  mutate(
    congruence_FIRES = calculate_congruence(
      c(norm_FIRES_baptist, norm_FIRES_bootlegger, norm_FIRES_public_policy_maker)
    ),
    congruence_FARMERS = calculate_congruence(
      c(norm_FARMERS_baptist, norm_FARMERS_bootlegger, norm_FARMERS_public_policy_maker)
    )
  ) %>%
  ungroup() # Remove row-wise grouping

model_FIRES <- lm(congruence_FIRES ~ norm_FIRES_individual, data = aggregated_scores)
model_FARMERS <- lm(congruence_FARMERS ~ norm_FARMERS_individual, data = aggregated_scores)

summary(model_FIRES)
summary(model_FARMERS)

###Compute congruence for each pair...
# Function to calculate pairwise congruence
calculate_pairwise_congruence <- function(actor1, actor2) {
  if (is.na(actor1) || is.na(actor2)) {
    return(NA) # Return NA if any value is missing
  }
  if (actor1 == 0 && actor2 == 0) {
    return(0) # No congruence if both have zero use
  }
  return(1 - abs(actor1 - actor2) / max(actor1, actor2)) # Congruence formula
}

# Calculate congruence for each pair
aggregated_scores <- aggregated_scores %>%
  rowwise() %>%
  mutate(
    # Pairwise congruence for FIRES
    congruence_FIRES_baptist_bootlegger = calculate_pairwise_congruence(norm_FIRES_baptist, norm_FIRES_bootlegger),
    congruence_FIRES_baptist_policymaker = calculate_pairwise_congruence(norm_FIRES_baptist, norm_FIRES_public_policy_maker),
    congruence_FIRES_bootlegger_policymaker = calculate_pairwise_congruence(norm_FIRES_bootlegger, norm_FIRES_public_policy_maker),
    
    # Pairwise congruence for FARMERS
    congruence_FARMERS_baptist_bootlegger = calculate_pairwise_congruence(norm_FARMERS_baptist, norm_FARMERS_bootlegger),
    congruence_FARMERS_baptist_policymaker = calculate_pairwise_congruence(norm_FARMERS_baptist, norm_FARMERS_public_policy_maker),
    congruence_FARMERS_bootlegger_policymaker = calculate_pairwise_congruence(norm_FARMERS_bootlegger, norm_FARMERS_public_policy_maker)
  ) %>%
  ungroup()

# Models for each pair
model_FIRES_baptist_bootlegger <- lm(congruence_FIRES_baptist_bootlegger ~ norm_FIRES_individual, data = aggregated_scores)
model_FIRES_baptist_policymaker <- lm(congruence_FIRES_baptist_policymaker ~ norm_FIRES_individual, data = aggregated_scores)
model_FIRES_bootlegger_policymaker <- lm(congruence_FIRES_bootlegger_policymaker ~ norm_FIRES_individual, data = aggregated_scores)

model_FARMERS_baptist_bootlegger <- lm(congruence_FARMERS_baptist_bootlegger ~ norm_FARMERS_individual, data = aggregated_scores)
model_FARMERS_baptist_policymaker <- lm(congruence_FARMERS_baptist_policymaker ~ norm_FARMERS_individual, data = aggregated_scores)
model_FARMERS_bootlegger_policymaker <- lm(congruence_FARMERS_bootlegger_policymaker ~ norm_FARMERS_individual, data = aggregated_scores)

# Summarize results for FIRES
summary(model_FIRES_baptist_bootlegger)
summary(model_FIRES_baptist_policymaker)
summary(model_FIRES_bootlegger_policymaker)

# Summarize results for FARMERS
summary(model_FARMERS_baptist_bootlegger)
summary(model_FARMERS_baptist_policymaker)
summary(model_FARMERS_bootlegger_policymaker)

glimpse(aggregated_scores)

####Binary instead:

# Convert narrative scores to binary
aggregated_scores <- aggregated_scores %>%
  mutate(
    binary_FIRES_baptist = ifelse(norm_FIRES_baptist > 0, 1, 0),
    binary_FIRES_bootlegger = ifelse(norm_FIRES_bootlegger > 0, 1, 0),
    binary_FIRES_policymaker = ifelse(norm_FIRES_public_policy_maker > 0, 1, 0),
    binary_FIRES_individual = ifelse(norm_FIRES_individual > 0, 1, 0),
    binary_FARMERS_baptist = ifelse(norm_FARMERS_baptist > 0, 1, 0),
    binary_FARMERS_bootlegger = ifelse(norm_FARMERS_bootlegger > 0, 1, 0),
    binary_FARMERS_policymaker = ifelse(norm_FARMERS_public_policy_maker > 0, 1, 0),
    binary_FARMERS_individual = ifelse(norm_FARMERS_individual > 0, 1, 0)
  )

# Calculate congruence as proportion of agreement
aggregated_scores <- aggregated_scores %>%
  rowwise() %>%
  mutate(
    congruence_FIRES_baptist_bootlegger = mean(c(binary_FIRES_baptist, binary_FIRES_bootlegger), na.rm = TRUE),
    congruence_FIRES_baptist_policymaker = mean(c(binary_FIRES_baptist, binary_FIRES_policymaker), na.rm = TRUE),
    congruence_FIRES_bootlegger_policymaker = mean(c(binary_FIRES_bootlegger, binary_FIRES_policymaker), na.rm = TRUE),
    congruence_FARMERS_baptist_bootlegger = mean(c(binary_FARMERS_baptist, binary_FARMERS_bootlegger), na.rm = TRUE),
    congruence_FARMERS_baptist_policymaker = mean(c(binary_FARMERS_baptist, binary_FARMERS_policymaker), na.rm = TRUE),
    congruence_FARMERS_bootlegger_policymaker = mean(c(binary_FARMERS_bootlegger, binary_FARMERS_policymaker), na.rm = TRUE)
  ) %>%
  ungroup()


# Fit models for each pair and narrative
model_binary_FIRES_baptist_bootlegger <- lm(congruence_FIRES_baptist_bootlegger ~ binary_FIRES_individual, data = aggregated_scores)
model_binary_FIRES_baptist_policymaker <- lm(congruence_FIRES_baptist_policymaker ~ binary_FIRES_individual, data = aggregated_scores)
model_binary_FIRES_bootlegger_policymaker <- lm(congruence_FIRES_bootlegger_policymaker ~ binary_FIRES_individual, data = aggregated_scores)

model_binary_FARMERS_baptist_bootlegger <- lm(congruence_FARMERS_baptist_bootlegger ~ binary_FARMERS_individual, data = aggregated_scores)
model_binary_FARMERS_baptist_policymaker <- lm(congruence_FARMERS_baptist_policymaker ~ binary_FARMERS_individual, data = aggregated_scores)
model_binary_FARMERS_bootlegger_policymaker <- lm(congruence_FARMERS_bootlegger_policymaker ~ binary_FARMERS_individual, data = aggregated_scores)

# Summarize results
summary(model_binary_FIRES_baptist_bootlegger)
summary(model_binary_FIRES_baptist_policymaker)
summary(model_binary_FIRES_bootlegger_policymaker)

summary(model_binary_FARMERS_baptist_bootlegger)
summary(model_binary_FARMERS_baptist_policymaker)
summary(model_binary_FARMERS_bootlegger_policymaker)

#####
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(broom)

# Combine model summaries into a single data frame
models <- list(
  "FIRES: Baptist-Bootlegger" = model_binary_FIRES_baptist_bootlegger,
  "FIRES: Baptist-Policymaker" = model_binary_FIRES_baptist_policymaker,
  "FIRES: Bootlegger-Policymaker" = model_binary_FIRES_bootlegger_policymaker,
  "FARMERS: Baptist-Bootlegger" = model_binary_FARMERS_baptist_bootlegger,
  "FARMERS: Baptist-Policymaker" = model_binary_FARMERS_baptist_policymaker,
  "FARMERS: Bootlegger-Policymaker" = model_binary_FARMERS_bootlegger_policymaker
)

# Extract tidy data from models with broom::tidy
model_summaries <- lapply(seq_along(models), function(i) {
  tidy(models[[i]]) %>%
    mutate(model_name = names(models)[i])
}) %>% bind_rows()
glimpse(model_summaries)
# Keep only the coefficient of interest (binary narrative use by individuals)
model_summaries <- model_summaries %>% 
  filter(term == "binary_FIRES_individual" | term == "binary_FARMERS_individual") %>% 
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Create a coefficient plot
ggplot(model_summaries, aes(x = reorder(model_name, estimate), y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_text(aes(label = significance, vjust = -1), color = "red", size = 5) +
  coord_flip() +
  labs(
    title = "Effect of Individuals' narratives on congruence",
    x = "Narrative: Actor Pair",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10)
  )

########



library(ggplot2)
library(dplyr)
library(broom)
library(ggimage)  # Required to add images as points

# Define paths to images (update if using direct links)
image_paths <- c("FIRES" = "fire.png", "FARMERS" = "tractor.png")  # Adjust file paths as needed

# Combine model summaries into a single data frame
models <- list(
  "FIRES: Baptist-Bootlegger" = model_binary_FIRES_baptist_bootlegger,
  "FIRES: Baptist-Policymaker" = model_binary_FIRES_baptist_policymaker,
  "FIRES: Bootlegger-Policymaker" = model_binary_FIRES_bootlegger_policymaker,
  "FARMERS: Baptist-Bootlegger" = model_binary_FARMERS_baptist_bootlegger,
  "FARMERS: Baptist-Policymaker" = model_binary_FARMERS_baptist_policymaker,
  "FARMERS: Bootlegger-Policymaker" = model_binary_FARMERS_bootlegger_policymaker
)

# Extract tidy data from models
model_summaries <- lapply(seq_along(models), function(i) {
  tidy(models[[i]]) %>%
    mutate(model_name = names(models)[i])
}) %>% bind_rows()

# Keep only the coefficient of interest (binary narrative use by individuals)
model_summaries <- model_summaries %>%
  filter(term == "binary_FIRES_individual" | term == "binary_FARMERS_individual") %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # Extract narrative type from model name
    Narrative = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),
    Image = image_paths[Narrative]  # Assign image path
  )

# Create the plot using images
ggplot(model_summaries, aes(x = reorder(model_name, estimate), y = estimate)) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2, color = "black") +
  geom_image(aes(image = Image), size = 0.07) +  # Add images as markers
  geom_text(aes(label = significance, vjust = -1), size = 5, color = "black") +  # Keep stars black
  coord_flip() +
  labs(
    title = "",
    x = "Narrative: Actor Pair",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Hide legend since icons indicate categories
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10)
  )

####One more try to fix actor pair labels:

# Define a lookup table for renaming labels (removing "FIRES: " and "FARMERS: ")
model_labels <- c(
  "FIRES: Baptist-Bootlegger" = "Baptist-Bootlegger",
  "FIRES: Baptist-Policymaker" = "Baptist-Policymaker",
  "FIRES: Bootlegger-Policymaker" = "Bootlegger-Policymaker",
  "FARMERS: Baptist-Bootlegger" = "Baptist-Bootlegger",
  "FARMERS: Baptist-Policymaker" = "Baptist-Policymaker",
  "FARMERS: Bootlegger-Policymaker" = "Bootlegger-Policymaker"
)

# Apply the label change aesthetically in ggplot2
final_plot <- ggplot(model_summaries, aes(x = reorder(model_name, estimate), y = estimate)) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2, color = "black") +
  geom_image(aes(image = Image), size = 0.07) +  # Add images as markers
  geom_text(aes(label = significance, vjust = -1), size = 5, color = "black") +  # Keep stars black
  coord_flip() +
  labs(
    title = "",
    x = "Actor Pair",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  scale_x_discrete(labels = model_labels) +  # Change only the axis labels
  theme_minimal() +
  theme(
    legend.position = "none",  # Hide legend since icons indicate categories
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10)
  )

# Display the updated plot
print(final_plot)

#####

library(broom)
library(kableExtra)
####model summaries with r2
# Create tidy summaries for all models
model_summaries_with_r2 <- bind_rows(
  tidy(model_binary_FIRES_baptist_bootlegger) %>% mutate(Model = "FIRES: Baptist-Bootlegger"),
  tidy(model_binary_FIRES_baptist_policymaker) %>% mutate(Model = "FIRES: Baptist-Policymaker"),
  tidy(model_binary_FIRES_bootlegger_policymaker) %>% mutate(Model = "FIRES: Bootlegger-Policymaker"),
  tidy(model_binary_FARMERS_baptist_bootlegger) %>% mutate(Model = "FARMERS: Baptist-Bootlegger"),
  tidy(model_binary_FARMERS_baptist_policymaker) %>% mutate(Model = "FARMERS: Baptist-Policymaker"),
  tidy(model_binary_FARMERS_bootlegger_policymaker) %>% mutate(Model = "FARMERS: Bootlegger-Policymaker")
)

# Extract R-squared values for each model
r_squared_values <- tibble(
  Model = c(
    "FIRES: Baptist-Bootlegger", "FIRES: Baptist-Policymaker", "FIRES: Bootlegger-Policymaker",
    "FARMERS: Baptist-Bootlegger", "FARMERS: Baptist-Policymaker", "FARMERS: Bootlegger-Policymaker"
  ),
  R_squared = c(
    summary(model_binary_FIRES_baptist_bootlegger)$r.squared,
    summary(model_binary_FIRES_baptist_policymaker)$r.squared,
    summary(model_binary_FIRES_bootlegger_policymaker)$r.squared,
    summary(model_binary_FARMERS_baptist_bootlegger)$r.squared,
    summary(model_binary_FARMERS_baptist_policymaker)$r.squared,
    summary(model_binary_FARMERS_bootlegger_policymaker)$r.squared
  )
)

# Filter and merge R-squared values into the model summaries
model_summaries_with_r2 <- model_summaries_with_r2 %>%
  filter(term == "binary_FIRES_individual" | term == "binary_FARMERS_individual") %>%
  left_join(r_squared_values, by = "Model") %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  arrange(desc(abs(estimate))) # Sort by absolute value of the coefficient

# Glimpse the updated data
glimpse(model_summaries_with_r2)

# Create a formatted table
model_summaries_with_r2 %>%
  select(Model, term, estimate, std.error, statistic, p.value, significance, R_squared) %>%
  kbl(
    digits = 3,
    caption = "Regression Results: Effect of Individuals' Narrative Use on Actor Pair Congruence",
    col.names = c("Model", "Term", "Estimate", "Std. Error", "Statistic", "p-value", "Significance", "R-squared")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Update column labels and text
model_summaries_with_r2_cleaned <- model_summaries_with_r2 %>%
  select(term, Model, estimate, std.error, statistic, p.value, significance, R_squared) %>%
  rename(
    Narrative = term,
    `Actor pair` = Model,
    Estimate = estimate,
    `Std. Error` = std.error,
    Statistic = statistic,
    `P-value` = p.value,
    Significance = significance,
    `R-squared` = R_squared
  ) %>%
  mutate(
    Narrative = case_when(
      Narrative == "binary_FIRES_individual" ~ "Fires",
      Narrative == "binary_FARMERS_individual" ~ "Farmers"
    ),
    `Actor pair` = str_remove(`Actor pair`, "FIRES: |FARMERS: ")
  )

# Create the formatted table
table_4_1 <- model_summaries_with_r2_cleaned %>%
  kbl(
    digits = 3,
    caption = "Effect of Individuals' Narrative Use on Actor Pair Congruence",
    col.names = c("Narrative", "Actor pair", "Estimate", "Std. Error", "Statistic", "P-value", "Significance", "R-squared")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

print(table_4_1)

#######################
## What could be driving these similarities?
## Our answer is "resonance"
## Actors taking the narratives of others when they are salient
## Test at two different stages - once when agreement is pending, and again after agreement stalled

# Divide data into two time periods
glimpse(aggregated_scores)
pre_august_2019 <- aggregated_scores %>%
  filter(as.Date(month_bin) <= as.Date("2019-08-31")) # Replace `date_column` with your actual date column name
post_august_2019 <- aggregated_scores %>%
  filter(as.Date(month_bin) > as.Date("2019-08-31"))

# Define a function to fit models and create tables for a given dataset
run_analysis <- function(data, caption) {
  # Define and fit models
  models <- list(
    "Baptist-Bootlegger" = lm(congruence_FIRES_baptist_bootlegger ~ binary_FIRES_individual, data = data),
    "Baptist-Policymaker" = lm(congruence_FIRES_baptist_policymaker ~ binary_FIRES_individual, data = data),
    "Bootlegger-Policymaker" = lm(congruence_FIRES_bootlegger_policymaker ~ binary_FIRES_individual, data = data),
    "Farmers: Baptist-Bootlegger" = lm(congruence_FARMERS_baptist_bootlegger ~ binary_FARMERS_individual, data = data),
    "Farmers: Baptist-Policymaker" = lm(congruence_FARMERS_baptist_policymaker ~ binary_FARMERS_individual, data = data),
    "Farmers: Bootlegger-Policymaker" = lm(congruence_FARMERS_bootlegger_policymaker ~ binary_FARMERS_individual, data = data)
  )
  
  # Summarize models
  model_summaries <- bind_rows(
    tidy(models[["Baptist-Bootlegger"]]) %>% mutate(`Actor pair` = "Baptist-Bootlegger"),
    tidy(models[["Baptist-Policymaker"]]) %>% mutate(`Actor pair` = "Baptist-Policymaker"),
    tidy(models[["Bootlegger-Policymaker"]]) %>% mutate(`Actor pair` = "Bootlegger-Policymaker"),
    tidy(models[["Farmers: Baptist-Bootlegger"]]) %>% mutate(`Actor pair` = "Baptist-Bootlegger"),
    tidy(models[["Farmers: Baptist-Policymaker"]]) %>% mutate(`Actor pair` = "Baptist-Policymaker"),
    tidy(models[["Farmers: Bootlegger-Policymaker"]]) %>% mutate(`Actor pair` = "Bootlegger-Policymaker")
  ) %>%
    filter(term == "binary_FIRES_individual" | term == "binary_FARMERS_individual") %>%
    rename(
      Narrative = term,
      Estimate = estimate,
      `Std. Error` = std.error,
      Statistic = statistic,
      `P-value` = p.value
    ) %>%
    mutate(
      Narrative = case_when(
        Narrative == "binary_FIRES_individual" ~ "Fires",
        Narrative == "binary_FARMERS_individual" ~ "Farmers"
      ),
      Significance = case_when(
        `P-value` < 0.001 ~ "***",
        `P-value` < 0.01 ~ "**",
        `P-value` < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  
  # Add R-squared values for each model
  r_squared_values <- map_dbl(models, ~ summary(.x)$r.squared)
  
  # Combine R-squared with model summaries
  model_summaries <- model_summaries %>%
    mutate(R_squared = rep(r_squared_values, each = nrow(model_summaries) / length(models)))
  
  # Create the formatted table
  model_summaries %>%
    select(Narrative, `Actor pair`, Estimate, `Std. Error`, Statistic, `P-value`, Significance, R_squared) %>%
    kbl(
      digits = 3,
      caption = caption,
      col.names = c("Narrative", "Actor pair", "Estimate", "Std. Error", "Statistic", "P-value", "Significance", "R-squared")
    ) %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
}

# Run analyses for both time periods
run_analysis(pre_august_2019, "Regression Results: Pre-August 2019")
run_analysis(post_august_2019, "Regression Results: Post-August 2019")

###viz-ualzie
# Function to create visualizations for a given dataset
# Function to create visualizations for a given dataset
create_visualization <- function(data, title) {
  # Define and fit models
  models <- list(
    "FIRES: Baptist-Bootlegger" = lm(congruence_FIRES_baptist_bootlegger ~ binary_FIRES_individual, data = data),
    "FIRES: Baptist-Policymaker" = lm(congruence_FIRES_baptist_policymaker ~ binary_FIRES_individual, data = data),
    "FIRES: Bootlegger-Policymaker" = lm(congruence_FIRES_bootlegger_policymaker ~ binary_FIRES_individual, data = data),
    "FARMERS: Baptist-Bootlegger" = lm(congruence_FARMERS_baptist_bootlegger ~ binary_FARMERS_individual, data = data),
    "FARMERS: Baptist-Policymaker" = lm(congruence_FARMERS_baptist_policymaker ~ binary_FARMERS_individual, data = data),
    "FARMERS: Bootlegger-Policymaker" = lm(congruence_FARMERS_bootlegger_policymaker ~ binary_FARMERS_individual, data = data)
  )
  
  # Extract tidy summaries
  model_summaries <- bind_rows(
    tidy(models[["FIRES: Baptist-Bootlegger"]]) %>% mutate(`Model` = "FIRES: Baptist-Bootlegger"),
    tidy(models[["FIRES: Baptist-Policymaker"]]) %>% mutate(`Model` = "FIRES: Baptist-Policymaker"),
    tidy(models[["FIRES: Bootlegger-Policymaker"]]) %>% mutate(`Model` = "FIRES: Bootlegger-Policymaker"),
    tidy(models[["FARMERS: Baptist-Bootlegger"]]) %>% mutate(`Model` = "FARMERS: Baptist-Bootlegger"),
    tidy(models[["FARMERS: Baptist-Policymaker"]]) %>% mutate(`Model` = "FARMERS: Baptist-Policymaker"),
    tidy(models[["FARMERS: Bootlegger-Policymaker"]]) %>% mutate(`Model` = "FARMERS: Bootlegger-Policymaker")
  ) %>%
    filter(term == "binary_FIRES_individual" | term == "binary_FARMERS_individual") %>%
    mutate(
      Narrative = case_when(
        term == "binary_FIRES_individual" ~ "FIRES",
        term == "binary_FARMERS_individual" ~ "FARMERS"
      ),
      Significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  
  # Create the plot
  ggplot(model_summaries, aes(x = reorder(Model, estimate), y = estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_text(aes(label = Significance, vjust = -1), color = "red", size = 5) +
    coord_flip() +
    labs(
      title = title,
      x = "Narrative: Actor Pair",
      y = "Coefficient Estimate",
      caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      plot.caption = element_text(size = 10)
    )
}

# Create visualizations for both time periods
viz_pre <- create_visualization(pre_august_2019, "Effect of Individuals' Narratives on Congruence (Pre-August 2019)")
viz_post <- create_visualization(post_august_2019, "Effect of Individuals' Narratives on Congruence (Post-August 2019)")

# Display the plots
print(viz_pre)
print(viz_post)

# Calculate changes in narrative use for individuals
aggregated_scores_with_changes <- aggregated_scores %>%
  arrange(month_bin) %>%
  mutate(
    change_FIRES_individual = norm_FIRES_individual - lag(norm_FIRES_individual, default = 0),
    change_FARMERS_individual = norm_FARMERS_individual - lag(norm_FARMERS_individual, default = 0)
  )

# Calculate changes in narrative use for other actors
narrative_changes <- aggregated_scores_with_changes %>%
  mutate(
    # Changes for FIRES
    change_FIRES_baptist = norm_FIRES_baptist - lag(norm_FIRES_baptist, default = 0),
    change_FIRES_bootlegger = norm_FIRES_bootlegger - lag(norm_FIRES_bootlegger, default = 0),
    change_FIRES_policymaker = norm_FIRES_public_policy_maker - lag(norm_FIRES_public_policy_maker, default = 0),
    # Changes for FARMERS
    change_FARMERS_baptist = norm_FARMERS_baptist - lag(norm_FARMERS_baptist, default = 0),
    change_FARMERS_bootlegger = norm_FARMERS_bootlegger - lag(norm_FARMERS_bootlegger, default = 0),
    change_FARMERS_policymaker = norm_FARMERS_public_policy_maker - lag(norm_FARMERS_public_policy_maker, default = 0)
  )

# Regressions for narrative changes in response to individuals
response_to_individuals <- list(
  FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual, data = narrative_changes),
  FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual, data = narrative_changes),
  FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual, data = narrative_changes),
  FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual, data = narrative_changes),
  FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual, data = narrative_changes),
  FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual, data = narrative_changes)
)

# Changes in congruence and their drivers
congruence_changes <- narrative_changes %>%
  mutate(
    # Changes in congruence for FIRES
    change_congruence_FIRES_baptist_bootlegger = congruence_FIRES_baptist_bootlegger - lag(congruence_FIRES_baptist_bootlegger, default = 0),
    change_congruence_FIRES_baptist_policymaker = congruence_FIRES_baptist_policymaker - lag(congruence_FIRES_baptist_policymaker, default = 0),
    change_congruence_FIRES_bootlegger_policymaker = congruence_FIRES_bootlegger_policymaker - lag(congruence_FIRES_bootlegger_policymaker, default = 0),
    # Changes in congruence for FARMERS
    change_congruence_FARMERS_baptist_bootlegger = congruence_FARMERS_baptist_bootlegger - lag(congruence_FARMERS_baptist_bootlegger, default = 0),
    change_congruence_FARMERS_baptist_policymaker = congruence_FARMERS_baptist_policymaker - lag(congruence_FARMERS_baptist_policymaker, default = 0),
    change_congruence_FARMERS_bootlegger_policymaker = congruence_FARMERS_bootlegger_policymaker - lag(congruence_FARMERS_bootlegger_policymaker, default = 0)
  )

# Regressions for congruence changes
congruence_drivers <- list(
  FIRES_baptist_bootlegger = lm(
    change_congruence_FIRES_baptist_bootlegger ~ change_FIRES_baptist + change_FIRES_bootlegger + change_FIRES_individual,
    data = congruence_changes
  ),
  FIRES_baptist_policymaker = lm(
    change_congruence_FIRES_baptist_policymaker ~ change_FIRES_baptist + change_FIRES_policymaker + change_FIRES_individual,
    data = congruence_changes
  ),
  FIRES_bootlegger_policymaker = lm(
    change_congruence_FIRES_bootlegger_policymaker ~ change_FIRES_bootlegger + change_FIRES_policymaker + change_FIRES_individual,
    data = congruence_changes
  ),
  FARMERS_baptist_bootlegger = lm(
    change_congruence_FARMERS_baptist_bootlegger ~ change_FARMERS_baptist + change_FARMERS_bootlegger + change_FARMERS_individual,
    data = congruence_changes
  ),
  FARMERS_baptist_policymaker = lm(
    change_congruence_FARMERS_baptist_policymaker ~ change_FARMERS_baptist + change_FARMERS_policymaker + change_FARMERS_individual,
    data = congruence_changes
  ),
  FARMERS_bootlegger_policymaker = lm(
    change_congruence_FARMERS_bootlegger_policymaker ~ change_FARMERS_bootlegger + change_FARMERS_policymaker + change_FARMERS_individual,
    data = congruence_changes
  )
)

# Summarize regression results for congruence drivers
lapply(congruence_drivers, summary)

#####That works, and now I want to see which actors are
#### driving congruence in each pair.

# Function to extract regression results
extract_regression_results <- function(models, type) {
  lapply(seq_along(models), function(i) {
    broom::tidy(models[[i]]) %>%
      filter(term != "(Intercept)") %>%  # Exclude intercept
      mutate(
        model_name = names(models)[i],  # Use index-based pairing
        type = type
      )
  }) %>% bind_rows()
}

# Extract congruence regression results
congruence_results <- extract_regression_results(congruence_drivers, "Congruence")

# Extract driver regression results
driver_results <- extract_regression_results(response_to_individuals, "Driver")
glimpse(driver_results)

# Combine results for visualization
all_results <- bind_rows(congruence_results, driver_results) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

driver_results_plot <- driver_results %>%
  mutate(
    group = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Plot 2: Actor response to individuals
plot2 <- ggplot(driver_results_plot, aes(x = reorder(model_name, estimate), y = estimate, color = term)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_text(aes(label = significance, vjust = -1), size = 4) +
  coord_flip() +
  labs(
    title = "Actor Response: Effect of Individual Narrative Use",
    x = "Actor Type",
    y = "Coefficient Estimate",
    color = "Actor Type",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10),
    legend.position = "top"
  )

plot2

###shows resonance of policymakers with FIRES
###shows resonance of baptists with FARMERS

narrative_changes_pre <- narrative_changes %>%
  filter(as.Date(month_bin) >= as.Date("2019-01-01") & as.Date(month_bin) <= as.Date("2019-09-01"))
narrative_changes_post <- narrative_changes %>%
  filter(as.Date(month_bin) > as.Date("2019-09-01"))

# Add group column within the fit_driver_models function
# Add group and significance columns within the fit_driver_models function
fit_driver_models <- function(data, period_label) {
  # Fit regressions
  response_to_individuals <- list(
    FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual, data = data),
    FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual, data = data),
    FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual, data = data),
    FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual, data = data),
    FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual, data = data),
    FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual, data = data)
  )
  
  # Extract tidy summaries and add columns
  driver_results <- lapply(seq_along(response_to_individuals), function(i) {
    broom::tidy(response_to_individuals[[i]]) %>%
      filter(term != "(Intercept)") %>%  # Exclude intercept
      mutate(
        model_name = names(response_to_individuals)[i],  # Use index-based pairing
        period = period_label,
        group = ifelse(grepl("FIRES", names(response_to_individuals)[i]), "FIRES", "FARMERS"),
        significance = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
  }) %>%
    bind_rows()
  
  return(driver_results)
}

# Fit models for both periods
driver_results_pre <- fit_driver_models(narrative_changes_pre, "Threat of agreement")
driver_results_post <- fit_driver_models(narrative_changes_post, "Ratification stalled")

# Visualization function remains the same
plot_driver_results <- function(data, title) {
  ggplot(data, aes(x = reorder(model_name, estimate), y = estimate, color = group)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_text(aes(label = significance, vjust = -1), size = 4) +
    coord_flip() +
    labs(
      title = title,
      x = "Actor Type",
      y = "Coefficient Estimate",
      color = "Narrative Group",
      caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      plot.caption = element_text(size = 10)
    )
}

# Generate plots for both periods
plot_pre <- plot_driver_results(driver_results_pre, "Actor Response: Threat of Agreement")
plot_post <- plot_driver_results(driver_results_post, "Actor Response: Ratificaiton Stalled")

# Display the plots
print(plot_pre)
###Bootleggers resonate when agreement is pending
###Policymakers and baptists resonate with bootleggers

print(plot_post)
###Then baptists resonate with farmers after the agreement has stalled while
###Policymakers resonate with baptists

### Make a publication-ready regression table
library(modelsummary)
library(kableExtra)  # Optional for enhanced formatting

# Fit models for both periods
models_pre <- list(
  FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual, data = narrative_changes_pre),
  FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual, data = narrative_changes_pre),
  FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual, data = narrative_changes_pre),
  FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual, data = narrative_changes_pre),
  FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual, data = narrative_changes_pre),
  FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual, data = narrative_changes_pre)
)

models_post <- list(
  FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual, data = narrative_changes_post),
  FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual, data = narrative_changes_post),
  FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual, data = narrative_changes_post),
  FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual, data = narrative_changes_post),
  FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual, data = narrative_changes_post),
  FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual, data = narrative_changes_post)
)

# Combine models into a named list
models <- c(models_pre, models_post)

# Create the table
modelsummary(
  models,
  stars = TRUE,  # Adds stars for significance
  title = "Regression Results: Actor Response to Individual Narratives",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),  # Include key goodness-of-fit statistics
  notes = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05",
  output = "latex"  # Change to "html" for viewing in RStudio or "word" for Word export
)

modelsummary(
  models,
  stars = TRUE,
  title = "Regression Results: Actor Response to Individual Narratives",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "kableExtra"
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

###make long rather than wide
library(dplyr)
library(broom)
library(modelsummary)
library(kableExtra)

# Function to convert model results into a tidy format with renamed columns
tidy_models <- function(model_list, period_label) {
  bind_rows(lapply(names(model_list), function(name) {
    broom::tidy(model_list[[name]]) %>%
      filter(term != "(Intercept)") %>%  # Remove intercept
      mutate(
        Actor = case_when(
          grepl("bootlegger", name) ~ "bootlegger",
          grepl("baptist", name) ~ "baptist",
          grepl("policymaker", name) ~ "policymaker",
          TRUE ~ name
        ),
        Narrative = case_when(
          term == "change_FIRES_individual" ~ "Fires",
          term == "change_FARMERS_individual" ~ "Farmers",
          TRUE ~ term
        ),
        Period = period_label,
        Significance = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
  }))
}

# Tidy the regression results for each period
tidy_pre <- tidy_models(models_pre, "Agreement pending")
tidy_post <- tidy_models(models_post, "Ratification stalled")

# Combine both into a long-format table and rename columns
long_results <- bind_rows(tidy_pre, tidy_post) %>%
  select(Period, Actor, Narrative, estimate, std.error, statistic, p.value, Significance)

# Reorder Period so that Summer 2019 appears first
long_results <- long_results %>%
  mutate(
    Period = factor(Period, levels = c("Agreement pending", "Ratification stalled")),
    Actor = factor(Actor, levels = c("bootlegger", "baptist", "policymaker"))
  ) %>%
  arrange(Period, Actor)

# Display the table in a long format with correct ordering
table_4_3 <- long_results %>%
  kbl(format = "html", digits = 3, align = "c", caption = "Regression Results: Actor Response to Narrative salience") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
table_4_3

####Publication ready regressions for congruence among actor types as well as drivers of congruence

# Filter and summarize driver results
impact_table <- driver_results %>%
  filter(term %in% c("change_FIRES_individual", "change_FARMERS_individual")) %>%
  mutate(
    Actor = case_when(
      grepl("baptist", model_name) ~ "Baptist",
      grepl("bootlegger", model_name) ~ "Bootlegger",
      grepl("policymaker", model_name) ~ "Policymaker"
    ),
    Narrative = case_when(
      grepl("FIRES", model_name) ~ "FIRES",
      grepl("FARMERS", model_name) ~ "FARMERS"
    ),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Narrative, Actor, Estimate = estimate, `Std. Error` = std.error, `p-value` = p.value, Significance)

# Display the regression table
library(knitr)
kable(impact_table, caption = "Impact of Individuals' Narrative Use on Actor Groups")

# Load the knitr package
library(knitr)

# Create and display the table in the Viewer
impact_table %>%
  kable(
    caption = "Impact of Individuals' Narrative Use on Actor Groups",
    format = "html",  # HTML format displays well in RStudio Viewer
    digits = 3        # Round estimates and p-values to 3 decimal places
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  )

###Add r2
# Extract R-squared values from the models
r_squared_values <- sapply(response_to_individuals, function(model) {
  summary(model)$r.squared
})

# Dynamically adjust the repetition to match the number of rows in impact_table
impact_table <- impact_table %>%
  mutate(model_name = rep(names(response_to_individuals), length.out = n()))

# Ensure model_name matches the number of rows in impact_table
impact_table <- impact_table %>%
  mutate(model_name = rep(names(response_to_individuals), length.out = n()))

# Add R-squared values
r_squared_values <- sapply(response_to_individuals, function(model) {
  summary(model)$r.squared
})

# Create a data frame for R-squared values
r_squared_df <- data.frame(
  model_name = names(r_squared_values),
  R2 = round(r_squared_values, 3)
)

# Join R-squared values with impact_table
impact_table_with_r2 <- impact_table %>%
  left_join(r_squared_df, by = "model_name")

# Display table in RStudio Viewer
impact_table_with_r2 %>%
  kable(
    caption = "Impact of Individuals' Narrative Use on Actor Groups (with R²)",
    format = "html",
    digits = 3
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  )

####Re-organize impact table

library(dplyr)
library(knitr)
library(kableExtra)

# Define correct order for sorting
desired_order <- c(
  "FIRES_bootlegger",
  "FIRES_baptist",
  "FIRES_policymaker",
  "FARMERS_bootlegger",
  "FARMERS_baptist",
  "FARMERS_policymaker"
)
glimpse(driver_results)

# Filter and format impact table
impact_table <- driver_results %>%
  filter(term %in% c("change_FIRES_individual", "change_FARMERS_individual")) %>%
  mutate(
    Actor = case_when(
      grepl("baptist", model_name) ~ "Baptist",
      grepl("bootlegger", model_name) ~ "Bootlegger",
      grepl("policymaker", model_name) ~ "Policymaker"
    ),
    Narrative = case_when(
      grepl("FIRES", model_name) ~ "FIRES",
      grepl("FARMERS", model_name) ~ "FARMERS"
    ),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(model_name, Narrative, Actor, Estimate = estimate, `Std. Error` = std.error, `p-value` = p.value, Significance)

# Ensure correct order
impact_table <- impact_table %>%
  mutate(model_name = factor(model_name, levels = desired_order)) %>%
  arrange(model_name)

# Extract R-squared values from models
r_squared_values <- sapply(response_to_individuals, function(model) {
  summary(model)$r.squared
})

# Convert R² values into a dataframe
r_squared_df <- data.frame(
  model_name = names(r_squared_values),
  R2 = round(r_squared_values, 3)
)

# Merge R² values using model_name
impact_table_with_r2 <- impact_table %>%
  left_join(r_squared_df, by = "model_name") %>%  # Ensure join works correctly
  select(-model_name)  # Remove model_name after the join

# Display table in RStudio Viewer
table_4_2 <- impact_table_with_r2 %>%
  kable(
    caption = "Impact of Individuals' Narrative Use on Actor Groups (with R²)",
    format = "html",
    digits = 3
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

print(table_4_2)


# Create the actor response plot with fixed order
actor_response_plot <- ggplot(driver_results_plot, aes(x = model_name, y = estimate)) +
  geom_point(aes(), size = 3) +  # Dots represent estimates
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                width = 0.2) +
  geom_text(aes(label = significance, vjust = -1), color = "red", size = 5) +
  coord_flip() +
  labs(
    title = "Actor Response to narrative salience",
    x = "NARRATIVE: Actor",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

actor_response_plot

#### Replace the dots with tractors and flames?

library(ggplot2)
library(dplyr)
library(ggimage)

# Define correct order for sorting
desired_order <- c(
  "FARMERS_baptist",
  "FARMERS_policymaker",
  "FARMERS_bootlegger",
  "FIRES_policymaker",
  "FIRES_baptist",
  "FIRES_bootlegger"
)

# Define image paths for fire and tractor icons
image_paths <- c("FIRES" = "fire.png", "FARMERS" = "tractor.png")

# Update dataset with proper ordering and assign image paths
driver_results_plot <- driver_results_plot %>%
  mutate(
    model_name = factor(model_name, levels = desired_order),  # Ensure correct order
    Narrative = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),  # Assign narrative
    Image = image_paths[Narrative]  # Assign appropriate image path
  )

# Create a lookup table for cleaner labels
# Define simpler display labels for each model_name (not unique, by design)
axis_labels <- c(
  "FARMERS_baptist" = "Baptist",
  "FARMERS_policymaker" = "Policymaker",
  "FARMERS_bootlegger" = "Bootlegger",
  "FIRES_policymaker" = "Policymaker",
  "FIRES_baptist" = "Baptist",
  "FIRES_bootlegger" = "Bootlegger"
)

# Ensure model_name is ordered and Narrative info is added
driver_results_plot <- driver_results_plot %>%
  mutate(
    model_name = factor(model_name, levels = desired_order),
    Narrative = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),
    Image = image_paths[Narrative]
  )

# Plot using model_name (for correct order), and override axis text labels manually
actor_response_plot <- ggplot(driver_results_plot, aes(x = model_name, y = estimate)) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_image(aes(image = Image), size = 0.07) +
  geom_text(aes(label = significance, vjust = -1), size = 5, color = "red") +
  coord_flip() +
  labs(
    title = "",
    x = "Actor Type",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  scale_x_discrete(labels = axis_labels) +  # Override y-axis labels
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# Display the plot
print(actor_response_plot)


#### So I will corroborate
## H1 - As salience of fires increases, bootleggers resonate with baptists 
## This is only true insomuch as there is a political problem - not after ratification is stalled
## H2 - As the salience of farmers increases, baptists resonate with bootleggers
## h3 - As the salience of fires increases, policymakers resonate with baptists
## H4 - as the salience of FARMERS increase, policymakers resonate with bootleggers
## All four corroborated

##All findings:
#Figure 4-1
final_plot
table_4_1
#Figure 4-2
actor_response_plot
table_4_2
#Table 4-3
table_4_3

####ROBUSTNESS CHECKS###
# Table 4-1
# Congruence ~ Narrative use by individuals

#Autocorrelation risk?
# Congruence in month t might be related to congruence in t-1
# Check for correlation:

library(lmtest)

# Durbin-Watson tests
dwtest(model_binary_FIRES_baptist_bootlegger)
dwtest(model_binary_FIRES_baptist_policymaker)
dwtest(model_binary_FIRES_bootlegger_policymaker)

dwtest(model_binary_FARMERS_baptist_bootlegger)
dwtest(model_binary_FARMERS_baptist_policymaker)
dwtest(model_binary_FARMERS_bootlegger_policymaker)


#Heteroscedasticity? 
#Could be present if variance changes over time, e.g. high conflict periods
# Check for heteroscedasticity

# Breusch-Pagan tests
bptest(model_binary_FIRES_baptist_bootlegger)
bptest(model_binary_FIRES_baptist_policymaker)
bptest(model_binary_FIRES_bootlegger_policymaker)

bptest(model_binary_FARMERS_baptist_bootlegger)
bptest(model_binary_FARMERS_baptist_policymaker)
bptest(model_binary_FARMERS_bootlegger_policymaker)

# Generate table for appendix:
library(kableExtra)
library(dplyr)

# Build diagnostics table manually from your results
diagnostics_table_4_1 <- tribble(
  ~Model, ~DW_stat, ~DW_p, ~BP_stat, ~BP_p,
  "FIRES: Baptist-Bootlegger",     2.358, 0.8213, 0.284, 0.594,
  "FIRES: Baptist-Policymaker",    1.571, 0.0853, 0.218, 0.641,
  "FIRES: Bootlegger-Policymaker", 2.005, 0.4548, 3.516, 0.0608,
  "FARMERS: Baptist-Bootlegger",   1.639, 0.1341, 0.856, 0.355,
  "FARMERS: Baptist-Policymaker",  1.954, 0.4216, 0.371, 0.543,
  "FARMERS: Bootlegger-Policymaker", 2.138, 0.6401, 0.286, 0.593
)

# Format and print table
table_diagnostics_4_1 <- diagnostics_table_4_1 %>%
  kbl(
    caption = "Table A1. Regression Diagnostics for Models in Table 4.1",
    col.names = c("Model", "Durbin-Watson", "DW p-value", "Breusch-Pagan", "BP p-value"),
    digits = 3,
    format = "html"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

print(table_diagnostics_4_1)

##Additional robustness checks:
library(sandwich)
# Clustered SEs by month for each model
coeftest(model_binary_FIRES_baptist_bootlegger, vcov = vcovCL(model_binary_FIRES_baptist_bootlegger, cluster = ~ month_bin))
coeftest(model_binary_FIRES_baptist_policymaker, vcov = vcovCL(model_binary_FIRES_baptist_policymaker, cluster = ~ month_bin))
coeftest(model_binary_FIRES_bootlegger_policymaker, vcov = vcovCL(model_binary_FIRES_bootlegger_policymaker, cluster = ~ month_bin))

coeftest(model_binary_FARMERS_baptist_bootlegger, vcov = vcovCL(model_binary_FARMERS_baptist_bootlegger, cluster = ~ month_bin))
coeftest(model_binary_FARMERS_baptist_policymaker, vcov = vcovCL(model_binary_FARMERS_baptist_policymaker, cluster = ~ month_bin))
coeftest(model_binary_FARMERS_bootlegger_policymaker, vcov = vcovCL(model_binary_FARMERS_bootlegger_policymaker, cluster = ~ month_bin))

cluster_var <- ~ month_bin
#### Can add this to the original table_4_1:
tidy_clustered <- function(model, cluster_var, model_name) {
  ct <- coeftest(model, vcovCL(model, cluster = cluster_var))
  tibble(
    term = rownames(ct),
    estimate = ct[, "Estimate"],
    std.error = ct[, "Std. Error"],
    statistic = ct[, "t value"],
    p.value = ct[, "Pr(>|t|)"],
    Model = model_name
  )
}

cluster_var <- ~ month_bin
# Use cluster-robust version of model summaries
model_summaries_with_r2 <- bind_rows(
  tidy_clustered(model_binary_FIRES_baptist_bootlegger, cluster_var, "FIRES: Baptist-Bootlegger"),
  tidy_clustered(model_binary_FIRES_baptist_policymaker, cluster_var, "FIRES: Baptist-Policymaker"),
  tidy_clustered(model_binary_FIRES_bootlegger_policymaker, cluster_var, "FIRES: Bootlegger-Policymaker"),
  tidy_clustered(model_binary_FARMERS_baptist_bootlegger, cluster_var, "FARMERS: Baptist-Bootlegger"),
  tidy_clustered(model_binary_FARMERS_baptist_policymaker, cluster_var, "FARMERS: Baptist-Policymaker"),
  tidy_clustered(model_binary_FARMERS_bootlegger_policymaker, cluster_var, "FARMERS: Bootlegger-Policymaker")
)

library(broom)
library(kableExtra)

# Extract R-squared values for each model
r_squared_values <- tibble(
  Model = c(
    "FIRES: Baptist-Bootlegger", "FIRES: Baptist-Policymaker", "FIRES: Bootlegger-Policymaker",
    "FARMERS: Baptist-Bootlegger", "FARMERS: Baptist-Policymaker", "FARMERS: Bootlegger-Policymaker"
  ),
  R_squared = c(
    summary(model_binary_FIRES_baptist_bootlegger)$r.squared,
    summary(model_binary_FIRES_baptist_policymaker)$r.squared,
    summary(model_binary_FIRES_bootlegger_policymaker)$r.squared,
    summary(model_binary_FARMERS_baptist_bootlegger)$r.squared,
    summary(model_binary_FARMERS_baptist_policymaker)$r.squared,
    summary(model_binary_FARMERS_bootlegger_policymaker)$r.squared
  )
)

# Filter and merge R-squared values into the model summaries
model_summaries_with_r2 <- model_summaries_with_r2 %>%
  filter(term == "binary_FIRES_individual" | term == "binary_FARMERS_individual") %>%
  left_join(r_squared_values, by = "Model") %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  arrange(desc(abs(estimate))) # Sort by absolute value of the coefficient

# Glimpse the updated data
glimpse(model_summaries_with_r2)

# Create a formatted table
model_summaries_with_r2 %>%
  select(Model, term, estimate, std.error, statistic, p.value, significance, R_squared) %>%
  kbl(
    digits = 3,
    caption = "Regression Results: Effect of Individuals' Narrative Use on Actor Pair Congruence",
    col.names = c("Model", "Term", "Estimate", "Std. Error", "Statistic", "p-value", "Significance", "R-squared")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Update column labels and text
model_summaries_with_r2_cleaned <- model_summaries_with_r2 %>%
  select(term, Model, estimate, std.error, statistic, p.value, significance, R_squared) %>%
  rename(
    Narrative = term,
    `Actor pair` = Model,
    Estimate = estimate,
    `Std. Error` = std.error,
    Statistic = statistic,
    `P-value` = p.value,
    `Significance` = significance,
    `R-squared` = R_squared
  ) %>%
  mutate(
    Narrative = case_when(
      Narrative == "binary_FIRES_individual" ~ "Fires",
      Narrative == "binary_FARMERS_individual" ~ "Farmers"
    ),
    `Actor pair` = str_remove(`Actor pair`, "FIRES: |FARMERS: ")
  )

# Create the formatted table
table_4_1 <- model_summaries_with_r2_cleaned %>%
  kbl(
    digits = 3,
    caption = "Effect of Individuals' Narrative Use on Actor Pair Congruence",
    col.names = c("Narrative", "Actor pair", "Estimate", "Std. Error", "Statistic", "P-value", "Significance", "R-squared")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

print(table_4_1)

#To ensure the robustness of our linear models in Table 4.1, we conducted standard diagnostics. 
#The Durbin-Watson tests suggest no substantial autocorrelation in the residuals, 
#and the Breusch-Pagan tests indicate no significant heteroscedasticity. 
#Full results are reported in Appendix Table A1.
###	“Standard errors are clustered by month to account for potential autocorrelation due to discursive 
#events or actor coordination within the same time period. Results are robust to this correction, with 
#only minor changes in standard errors and no change in substantive conclusions.”

####control for activity
library(dplyr)
library(lubridate)

narrative_matches <- narrative_matches %>%
  mutate(
    class = str_trim(tolower(class)),  # normalize group names
    month_bin = floor_date(created_at, unit = "month")  # YYYY-MM binning
  )
group_activity <- narrative_matches %>%
  filter(!is.na(class)) %>%
  count(month_bin, class, name = "total_tweets") %>%
  pivot_wider(
    names_from = class,
    values_from = total_tweets,
    values_fill = 0
  ) %>%
  rename_with(~ paste0("total_tweets_", .), -month_bin)

narrative_changes <- narrative_changes %>%
  left_join(group_activity, by = "month_bin")

narrative_changes_pre <- narrative_changes_pre %>%
  left_join(group_activity, by = "month_bin")

narrative_changes_post <- narrative_changes_post %>%
  left_join(group_activity, by = "month_bin")
glimpse(narrative_changes)

# Define your control models with correct dependent variables
model_binary_FIRES_baptist_bootlegger_control <- lm(congruence_FIRES_baptist_bootlegger ~ binary_FIRES_individual + total_tweets_baptist + total_tweets_bootlegger, data = narrative_changes)
model_binary_FIRES_baptist_policymaker_control <- lm(congruence_FIRES_baptist_policymaker ~ binary_FIRES_individual + total_tweets_baptist + total_tweets_public_policy_maker, data = narrative_changes)
model_binary_FIRES_bootlegger_policymaker_control <- lm(congruence_FIRES_bootlegger_policymaker ~ binary_FIRES_individual + total_tweets_bootlegger + total_tweets_public_policy_maker, data = narrative_changes)

model_binary_FARMERS_baptist_bootlegger_control <- lm(congruence_FARMERS_baptist_bootlegger ~ binary_FARMERS_individual + total_tweets_baptist + total_tweets_bootlegger, data = narrative_changes)
model_binary_FARMERS_baptist_policymaker_control <- lm(congruence_FARMERS_baptist_policymaker ~ binary_FARMERS_individual + total_tweets_baptist + total_tweets_public_policy_maker, data = narrative_changes)
model_binary_FARMERS_bootlegger_policymaker_control <- lm(congruence_FARMERS_bootlegger_policymaker ~ binary_FARMERS_individual + total_tweets_bootlegger + total_tweets_public_policy_maker, data = narrative_changes)

cluster_var <- ~ month_bin

models_table_4_1_control <- list(
  "FIRES: Baptist-Bootlegger" = model_binary_FIRES_baptist_bootlegger_control,
  "FIRES: Baptist-Policymaker" = model_binary_FIRES_baptist_policymaker_control,
  "FIRES: Bootlegger-Policymaker" = model_binary_FIRES_bootlegger_policymaker_control,
  "FARMERS: Baptist-Bootlegger" = model_binary_FARMERS_baptist_bootlegger_control,
  "FARMERS: Baptist-Policymaker" = model_binary_FARMERS_baptist_policymaker_control,
  "FARMERS: Bootlegger-Policymaker" = model_binary_FARMERS_bootlegger_policymaker_control
)

library(lmtest)
library(sandwich)
library(tibble)

tidy_with_clustered_se <- function(model, cluster_var, model_name = NULL) {
  clustered <- coeftest(model, vcov = vcovCL(model, cluster = cluster_var))
  coef_row <- clustered[2, , drop = FALSE]  # assumes the key predictor is 2nd
  tibble(
    term = rownames(coef_row),
    estimate = coef_row[, "Estimate"],
    std.error = coef_row[, "Std. Error"],
    statistic = coef_row[, "t value"],
    p.value = coef_row[, "Pr(>|t|)"],
    model_name = model_name
  )
}

model_summaries_with_r2_control <- map_df(names(models_table_4_1_control), function(model_name) {
  model <- models_table_4_1_control[[model_name]]
  clustered <- tidy_with_clustered_se(model, cluster_var = cluster_var, model_name = model_name)
  clustered$R_squared <- summary(model)$r.squared
  clustered
}) %>%
  filter(term %in% c("binary_FIRES_individual", "binary_FARMERS_individual")) %>%
  mutate(
    Narrative = case_when(
      term == "binary_FIRES_individual" ~ "Fires",
      term == "binary_FARMERS_individual" ~ "Farmers"
    ),
    `Actor pair` = str_remove(model_name, "FIRES: |FARMERS: "),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Narrative, `Actor pair`, Estimate = estimate, `Std. Error` = std.error,
         Statistic = statistic, `P-value` = p.value, Significance, `R-squared` = R_squared)

model_summaries_with_r2_control <- map_df(names(models_table_4_1_control), function(model_name) {
  model <- models_table_4_1_control[[model_name]]
  clustered <- tidy_with_clustered_se(model, cluster_var = cluster_var, model_name = model_name)
  clustered$R_squared <- summary(model)$r.squared
  clustered
}) %>%
  filter(term %in% c("binary_FIRES_individual", "binary_FARMERS_individual")) %>%
  mutate(
    Narrative = case_when(
      term == "binary_FIRES_individual" ~ "Fires",
      term == "binary_FARMERS_individual" ~ "Farmers"
    ),
    `Actor pair` = str_remove(model_name, "FIRES: |FARMERS: "),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Narrative, `Actor pair`, Estimate = estimate, `Std. Error` = std.error,
         Statistic = statistic, `P-value` = p.value, Significance, `R-squared` = R_squared)


table_4_1_control <- model_summaries_with_r2_control %>%
  kable(
    caption = "Table A1. Actor Congruence Explained by Narrative Use (with Group Activity Controls)",
    format = "html",
    digits = 3
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  )

print(table_4_1_control)

################ Diagnostics of those results
# Load necessary diagnostic packages

names(models_table_4_1_control)
class(models_table_4_1_control[[1]])

library(lmtest)
library(car)
library(broom)

# Function to extract DW and BP test results for a model
# SAFER version of diagnostics function
get_diagnostics <- function(model, model_name) {
  # Durbin-Watson test for autocorrelation
  dw <- tryCatch({
    dwtest(model)
  }, error = function(e) NULL)
  
  # Breusch-Pagan test for heteroskedasticity
  bp <- tryCatch({
    bptest(model)
  }, error = function(e) NULL)
  
  tibble(
    model_name = model_name,
    dw_statistic = if (!is.null(dw)) as.numeric(dw$statistic) else NA,
    dw_p_value = if (!is.null(dw)) dw$p.value else NA,
    bp_statistic = if (!is.null(bp)) as.numeric(bp$statistic) else NA,
    bp_p_value = if (!is.null(bp)) bp$p.value else NA
  )
}

# Run diagnostics on all models
diagnostic_results <- map_df(names(models_table_4_1_control), function(name) {
  model <- models_table_4_1_control[[name]]
  get_diagnostics(model, model_name = name)
})

# Clean up names for table display
diagnostic_results <- diagnostic_results %>%
  mutate(
    `Actor pair` = str_remove(model_name, "FIRES: |FARMERS: "),
    Narrative = case_when(
      str_detect(model_name, "FIRES") ~ "Fires",
      str_detect(model_name, "FARMERS") ~ "Farmers"
    )
  ) %>%
  select(Narrative, `Actor pair`, 
         `Durbin-Watson stat` = dw_statistic, `DW p-value` = dw_p_value,
         `Breusch-Pagan stat` = bp_statistic, `BP p-value` = bp_p_value)

# Print as HTML table
diagnostics_table <- diagnostic_results %>%
  kable(
    caption = "Table A2. Regression Diagnostics for Actor Congruence Models",
    format = "html",
    digits = 3
  ) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

print(diagnostics_table)


#####BOOKMARK###
#############################
#Re-create figure 4-1

library(ggplot2)
library(dplyr)
library(ggimage)

# Use this line only if you haven't already added image paths:
image_paths <- c("Fires" = "fire.png", "Farmers" = "tractor.png")  # Update paths as needed

# Start from model_summaries_with_r2_control and rename to model_summaries for compatibility
model_summaries <- model_summaries_with_r2_control %>%
  mutate(
    model_name = paste(Narrative, `Actor pair`, sep = ": "),
    significance = case_when(
      `P-value` < 0.001 ~ "***",
      `P-value` < 0.01 ~ "**",
      `P-value` < 0.05 ~ "*",
      TRUE ~ ""
    ),
    Image = image_paths[Narrative]
  )

# Define lookup table for relabeling axis
model_labels <- c(
  "Fires: Baptist-Bootlegger" = "Baptist-Bootlegger",
  "Fires: Baptist-Policymaker" = "Baptist-Policymaker",
  "Fires: Bootlegger-Policymaker" = "Bootlegger-Policymaker",
  "Farmers: Baptist-Bootlegger" = "Baptist-Bootlegger",
  "Farmers: Baptist-Policymaker" = "Baptist-Policymaker",
  "Farmers: Bootlegger-Policymaker" = "Bootlegger-Policymaker"
)

# Final plot
final_plot <- ggplot(model_summaries, aes(x = reorder(model_name, Estimate), y = Estimate)) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), width = 0.2, color = "black") +
  geom_image(aes(image = Image), size = 0.07) +
  geom_text(aes(label = significance, vjust = -1), size = 5, color = "black") +
  coord_flip() +
  labs(
    title = "",
    x = "Actor Pair",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  scale_x_discrete(labels = model_labels) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10)
  )

# Print it
print(final_plot)



##############################
#Table 4-2
# Narrative change by group ~ Narrative use by individuals
names(response_to_individuals)

library(lmtest)
library(dplyr)

# Function to perform diagnostics for each model
run_diagnostics <- function(model_list) {
  data.frame(
    Model = names(model_list),
    DW_statistic = sapply(model_list, function(m) dwtest(m)$statistic),
    DW_p_value   = sapply(model_list, function(m) dwtest(m)$p.value),
    BP_statistic = sapply(model_list, function(m) bptest(m)$statistic),
    BP_p_value   = sapply(model_list, function(m) bptest(m)$p.value)
  )
}

# Run diagnostics for Table 4.2 models
diagnostics_table_4_2 <- run_diagnostics(response_to_individuals)

# Preview results
print(diagnostics_table_4_2)

## make table
library(kableExtra)

diagnostics_table_4_2 %>%
  mutate(
    DW_p_value = sprintf("%.3f", DW_p_value),
    BP_p_value = sprintf("%.3f", BP_p_value)
  ) %>%
  kbl(
    digits = 3,
    caption = "Table A2. Regression Diagnostics for Table 4.2 Models",
    col.names = c("Model", "DW Statistic", "DW p-value", "BP Statistic", "BP p-value")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


#Table 4-3 - Same as 4.2, but split into pre-/post- failure periods
library(lmtest)
library(dplyr)
library(purrr)

# Function to run diagnostics on each model
run_diagnostics <- function(models, period_label) {
  map_df(names(models), function(name) {
    model <- models[[name]]
    
    dw <- dwtest(model)
    bp <- bptest(model)
    
    tibble(
      Model = name,
      Period = period_label,
      DW_statistic = dw$statistic,
      DW_p_value = dw$p.value,
      BP_statistic = bp$statistic,
      BP_p_value = bp$p.value
    )
  })
}

# Run diagnostics for both periods
diagnostics_pre <- run_diagnostics(models_pre, "Agreement pending")
diagnostics_post <- run_diagnostics(models_post, "Ratification stalled")

# Combine and print results
diagnostics_table_4_3 <- bind_rows(diagnostics_pre, diagnostics_post)
print(diagnostics_table_4_3)

library(kableExtra)

# Round the statistics for readability
diagnostics_table_4_3_formatted <- diagnostics_table_4_3 %>%
  mutate(
    DW_statistic = round(DW_statistic, 3),
    DW_p_value = round(DW_p_value, 3),
    BP_statistic = round(BP_statistic, 3),
    BP_p_value = round(BP_p_value, 3),
    Model = gsub("_", " ", Model),  # Replace underscores with spaces
    Model = tools::toTitleCase(Model)
  )

# Create the table
diagnostics_table_4_3_kable <- diagnostics_table_4_3_formatted %>%
  kbl(
    caption = "Regression Diagnostics for Table 4.3: Actor Response to Narrative Salience, by Period",
    col.names = c("Model", "Period", "DW Statistic", "DW p-value", "BP Statistic", "BP p-value"),
    align = "lcccccc",
    format = "html"  # or "latex" or "markdown"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Print table
diagnostics_table_4_3_kable

##Writeup:
#Appendix Table A3 reports Durbin-Watson and Breusch-Pagan tests for all actor response models (Table 4.3). 
#While most models do not exhibit significant autocorrelation or heteroscedasticity, 
#some FIRES-related models—particularly in the pre-agreement period—show signs of mild negative 
#autocorrelation (DW > 2). This does not strongly bias estimates but indicates possible temporal 
#structuring in actor responses. 
####
### Trying clustering standard errors:

library(lmtest)
library(sandwich)

tidy_with_clustered_se <- function(model, cluster_var, model_name = NULL) {
  clustered <- coeftest(model, vcov = vcovCL(model, cluster = cluster_var))
  coef_row <- clustered[2, , drop = FALSE]  # assumes second row is key predictor
  tibble(
    term = rownames(coef_row),
    estimate = coef_row[, "Estimate"],
    std.error = coef_row[, "Std. Error"],
    statistic = coef_row[, "t value"],
    p.value = coef_row[, "Pr(>|t|)"],
    model_name = model_name
  )
}




# Function to extract regression results
extract_regression_results <- function(models, type) {
  lapply(seq_along(models), function(i) {
    broom::tidy(models[[i]]) %>%
      filter(term != "(Intercept)") %>%  # Exclude intercept
      mutate(
        model_name = names(models)[i],  # Use index-based pairing
        type = type
      )
  }) %>% bind_rows()
}

# Extract congruence regression results
congruence_results <- extract_regression_results(congruence_drivers, "Congruence")

# Extract driver regression results
# Use clustered SEs instead of broom::tidy()
cluster_var <- ~ month_bin
library(purrr)
get_driver_results <- function(model_list, period_label) {
  map_df(names(model_list), function(model_name) {
    tidy_with_clustered_se(
      model = model_list[[model_name]],
      cluster_var = cluster_var,
      model_name = model_name
    ) %>%
      mutate(
        period = period_label,
        group = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),
        significance = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
  })
}

get_clustered_results <- function(model_list, period_label = NULL, cluster_var = ~ month_bin) {
  map_df(names(model_list), function(model_name) {
    result <- tidy_with_clustered_se(
      model = model_list[[model_name]],
      cluster_var = cluster_var,
      model_name = model_name
    ) %>%
      mutate(
        group = case_when(
          grepl("FIRES", model_name) ~ "FIRES",
          grepl("FARMERS", model_name) ~ "FARMERS",
          TRUE ~ NA_character_
        ),
        actor = case_when(
          grepl("baptist", model_name) ~ "baptist",
          grepl("bootlegger", model_name) ~ "bootlegger",
          grepl("policymaker", model_name) ~ "policymaker",
          TRUE ~ NA_character_
        ),
        significance = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
    if (!is.null(period_label)) {
      result <- mutate(result, period = period_label)
    }
    result
  })
}

# Table 4.2 (full dataset, no period label needed)
driver_results <- get_clustered_results(response_to_individuals)
glimpse(driver_results)

# Combine results for visualization
all_results <- bind_rows(congruence_results, driver_results) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )
driver_results_plot <- driver_results %>%
  mutate(
    group = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )
# Plot 2: Actor response to individuals
plot2 <- ggplot(driver_results_plot, aes(x = reorder(model_name, estimate), y = estimate, color = term)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_text(aes(label = significance, vjust = -1), size = 4) +
  coord_flip() +
  labs(
    title = "Actor Response: Effect of Individual Narrative Use",
    x = "Actor Type",
    y = "Coefficient Estimate",
    color = "Actor Type",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10),
    legend.position = "top"
  )

plot2

###shows resonance of policymakers with FIRES
###shows resonance of baptists with FARMERS

narrative_changes_pre <- narrative_changes %>%
  filter(as.Date(month_bin) >= as.Date("2019-01-01") & as.Date(month_bin) <= as.Date("2019-09-01"))
narrative_changes_post <- narrative_changes %>%
  filter(as.Date(month_bin) > as.Date("2019-09-01"))

# Add group column within the fit_driver_models function
# Add group and significance columns within the fit_driver_models function
fit_driver_models <- function(data, period_label) {
  # Fit regressions
  response_to_individuals <- list(
    FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual, data = data),
    FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual, data = data),
    FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual, data = data),
    FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual, data = data),
    FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual, data = data),
    FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual, data = data)
  )
  
  # Extract tidy summaries and add columns
  driver_results <- lapply(seq_along(response_to_individuals), function(i) {
    broom::tidy(response_to_individuals[[i]]) %>%
      filter(term != "(Intercept)") %>%  # Exclude intercept
      mutate(
        model_name = names(response_to_individuals)[i],  # Use index-based pairing
        period = period_label,
        group = ifelse(grepl("FIRES", names(response_to_individuals)[i]), "FIRES", "FARMERS"),
        significance = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
  }) %>%
    bind_rows()
  
  return(driver_results)
}

# Fit models for both periods
driver_results_pre <- get_clustered_results(models_pre, period_label = "Agreement pending")
driver_results_post <- get_clustered_results(models_post, period_label = "Ratification stalled")


# Visualization function remains the same
plot_driver_results <- function(data, title) {
  ggplot(data, aes(x = reorder(model_name, estimate), y = estimate, color = group)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_text(aes(label = significance, vjust = -1), size = 4) +
    coord_flip() +
    labs(
      title = title,
      x = "Actor Type",
      y = "Coefficient Estimate",
      color = "Narrative Group",
      caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      plot.caption = element_text(size = 10)
    )
}

# Generate plots for both periods
plot_pre <- plot_driver_results(driver_results_pre, "Actor Response: Threat of Agreement")
plot_post <- plot_driver_results(driver_results_post, "Actor Response: Ratification Stalled")

# Display the plots
print(plot_pre)
###Bootleggers resonate when agreement is pending
###Policymakers and baptists resonate with bootleggers

print(plot_post)
###Then baptists resonate with farmers after the agreement has stalled while
###Policymakers resonate with baptists

### Make a publication-ready regression table
library(modelsummary)
library(kableExtra)  # Optional for enhanced formatting

# Fit models for both periods
models_pre <- list(
  FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual, data = narrative_changes_pre),
  FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual, data = narrative_changes_pre),
  FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual, data = narrative_changes_pre),
  FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual, data = narrative_changes_pre),
  FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual, data = narrative_changes_pre),
  FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual, data = narrative_changes_pre)
)

models_post <- list(
  FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual, data = narrative_changes_post),
  FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual, data = narrative_changes_post),
  FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual, data = narrative_changes_post),
  FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual, data = narrative_changes_post),
  FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual, data = narrative_changes_post),
  FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual, data = narrative_changes_post)
)

# Combine models into a named list
models <- c(models_pre, models_post)

# Create the table
modelsummary(
  models,
  stars = TRUE,  # Adds stars for significance
  title = "Regression Results: Actor Response to Individual Narratives",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),  # Include key goodness-of-fit statistics
  notes = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05",
  output = "latex"  # Change to "html" for viewing in RStudio or "word" for Word export
)

modelsummary(
  models,
  stars = TRUE,
  title = "Regression Results: Actor Response to Individual Narratives",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "kableExtra"
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

###make long rather than wide
library(dplyr)
library(broom)
library(modelsummary)
library(kableExtra)

# Function to convert model results into a tidy format with renamed columns
tidy_models <- function(model_list, period_label) {
  bind_rows(lapply(names(model_list), function(name) {
    broom::tidy(model_list[[name]]) %>%
      filter(term != "(Intercept)") %>%  # Remove intercept
      mutate(
        Actor = case_when(
          grepl("bootlegger", name) ~ "bootlegger",
          grepl("baptist", name) ~ "baptist",
          grepl("policymaker", name) ~ "policymaker",
          TRUE ~ name
        ),
        Narrative = case_when(
          term == "change_FIRES_individual" ~ "Fires",
          term == "change_FARMERS_individual" ~ "Farmers",
          TRUE ~ term
        ),
        Period = period_label,
        Significance = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
  }))
}

# Tidy the regression results for each period
tidy_pre <- get_clustered_results(models_pre, "Agreement pending")
tidy_post <- get_clustered_results(models_post, "Ratification stalled")

# Combine both into a long-format table and rename columns
long_results <- bind_rows(tidy_pre, tidy_post) %>%
  select(Period = period, Actor = actor, Narrative = group, estimate, std.error, statistic, p.value, Significance = significance) %>%
  mutate(
    Period = factor(Period, levels = c("Agreement pending", "Ratification stalled")),
    Actor = factor(Actor, levels = c("bootlegger", "baptist", "policymaker"))
  ) %>%
  arrange(Period, Actor)

# Reorder Period so that Summer 2019 appears first
long_results <- long_results %>%
  mutate(
    Period = factor(Period, levels = c("Agreement pending", "Ratification stalled")),
    Actor = factor(Actor, levels = c("bootlegger", "baptist", "policymaker"))
  ) %>%
  arrange(Period, Actor)

# Display the table in a long format with correct ordering
table_4_3 <- long_results %>%
  kbl(format = "html", digits = 3, align = "c",
      caption = "Regression Results: Actor Response to Narrative Salience (Clustered SEs by Month)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
table_4_3
###################Diagnostics for appendix
get_diagnostics <- function(model, model_name) {
  dw <- tryCatch({
    lmtest::dwtest(model)
  }, error = function(e) NULL)
  
  bp <- tryCatch({
    lmtest::bptest(model)
  }, error = function(e) NULL)
  
  tibble(
    model_name = model_name,
    dw_statistic = if (!is.null(dw)) as.numeric(dw$statistic) else NA,
    dw_p_value = if (!is.null(dw)) dw$p.value else NA,
    bp_statistic = if (!is.null(bp)) as.numeric(bp$statistic) else NA,
    bp_p_value = if (!is.null(bp)) bp$p.value else NA
  )
}

# Apply to all pre- and post-agreement models
diagnostics_pre <- map_df(names(models_pre), function(name) {
  get_diagnostics(models_pre[[name]], model_name = paste(name, "(Agreement pending)"))
})

diagnostics_post <- map_df(names(models_post), function(name) {
  get_diagnostics(models_post[[name]], model_name = paste(name, "(Ratification stalled)"))
})

# Combine and label
diagnostics_4_3 <- bind_rows(diagnostics_pre, diagnostics_post) %>%
  mutate(
    Narrative = case_when(
      str_detect(model_name, "FIRES") ~ "Fires",
      str_detect(model_name, "FARMERS") ~ "Farmers"
    ),
    Actor = case_when(
      str_detect(model_name, "baptist") ~ "Baptist",
      str_detect(model_name, "bootlegger") ~ "Bootlegger",
      str_detect(model_name, "policymaker") ~ "Policymaker"
    ),
    Period = case_when(
      str_detect(model_name, "Agreement pending") ~ "Agreement pending",
      str_detect(model_name, "Ratification stalled") ~ "Ratification stalled"
    )
  ) %>%
  select(Period, Narrative, Actor, `Durbin-Watson stat` = dw_statistic, `DW p-value` = dw_p_value,
         `Breusch-Pagan stat` = bp_statistic, `BP p-value` = bp_p_value)

diagnostics_4_3 %>%
  kbl(format = "html", digits = 3,
      caption = "Table A3. Regression Diagnostics for Actor Response Models (Table 4.3)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


###########################



#####Control for activity:
# Step 1: Fit OLS models for both periods, including activity controls

models_pre_control <- list(
  FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual + total_tweets_baptist, data = narrative_changes_pre),
  FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual + total_tweets_bootlegger, data = narrative_changes_pre),
  FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual + total_tweets_public_policy_maker, data = narrative_changes_pre),
  FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual + total_tweets_baptist, data = narrative_changes_pre),
  FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual + total_tweets_bootlegger, data = narrative_changes_pre),
  FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual + total_tweets_public_policy_maker, data = narrative_changes_pre)
)

models_post_control <- list(
  FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual + total_tweets_baptist, data = narrative_changes_post),
  FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual + total_tweets_bootlegger, data = narrative_changes_post),
  FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual + total_tweets_public_policy_maker, data = narrative_changes_post),
  FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual + total_tweets_baptist, data = narrative_changes_post),
  FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual + total_tweets_bootlegger, data = narrative_changes_post),
  FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual + total_tweets_public_policy_maker, data = narrative_changes_post)
)


tidy_with_clustered_se <- function(model, cluster_var, model_name = NULL) {
  clustered <- coeftest(model, vcov = vcovCL(model, cluster = cluster_var))
  
  # Get all predictor terms (excluding intercept)
  terms <- rownames(clustered)[rownames(clustered) != "(Intercept)"]
  
  if (length(terms) == 0) {
    stop(paste("No predictor terms found in model:", model_name))
  }
  
  # Extract all rows except the intercept
  coef_rows <- clustered[terms, , drop = FALSE]
  
  tibble(
    term = rownames(coef_rows),
    estimate = coef_rows[, "Estimate"],
    std.error = coef_rows[, "Std. Error"],
    statistic = coef_rows[, "t value"],
    p.value = coef_rows[, "Pr(>|t|)"],
    model_name = model_name
  )
}

model_test <- models_pre_control[[1]]
clustered_test <- coeftest(model_test, vcov = vcovCL(model_test, cluster = ~ month_bin))
print(clustered_test)

nrow(narrative_changes_pre)
length(unique(narrative_changes_pre$month_bin))

tidy_with_clustered_se <- function(model, cluster_var, model_name = NULL) {
  clustered <- coeftest(model, vcov = vcovCL(model, cluster = cluster_var))
  
  # Get main predictor row(s)
  main_index <- grep("change_.*_individual", rownames(clustered))
  if (length(main_index) == 0) {
    stop(paste("❌ No matching predictor found in model:", model_name))
  }
  coef_row <- clustered[main_index, , drop = FALSE]
  
  # Detect column names for statistic and p-value
  stat_col <- if ("t value" %in% colnames(coef_row)) "t value" else "z value"
  pval_col <- if ("Pr(>|t|)" %in% colnames(coef_row)) "Pr(>|t|)" else "Pr(>|z|)"
  
  tibble(
    term = rownames(coef_row),
    estimate = coef_row[, "Estimate"],
    std.error = coef_row[, "Std. Error"],
    statistic = coef_row[, stat_col],
    p.value = coef_row[, pval_col],
    model_name = model_name
  )
}

# Step 2: Get clustered SEs for new models with activity control
driver_results_pre_control <- get_clustered_results(models_pre_control, period_label = "Agreement pending")
driver_results_post_control <- get_clustered_results(models_post_control, period_label = "Ratification stalled")

##Step 3!
library(dplyr)
library(knitr)
library(kableExtra)

# Combine both into a long-format table
long_results_control <- bind_rows(driver_results_pre_control, driver_results_post_control) %>%
  select(
    Period = period,
    Actor = actor,
    Narrative = group,
    Estimate = estimate,
    `Std. Error` = std.error,
    Statistic = statistic,
    `P-value` = p.value,
    Significance = significance
  ) %>%
  mutate(
    Period = factor(Period, levels = c("Agreement pending", "Ratification stalled")),
    Actor = factor(Actor, levels = c("bootlegger", "baptist", "policymaker"))
  ) %>%
  arrange(Period, Actor)

# Display the table in HTML format
table_4_3_control <- long_results_control %>%
  kbl(
    format = "html",
    digits = 3,
    align = "c",
    caption = "Table A3. Regression Results: Actor Response to Narrative Salience (with Activity Controls and Clustered SEs)"
  ) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover")
  )

# Print the table
print(table_4_3_control)



############
#Continue to making tables 4_2 and the actor response plot


driver_results <- get_clustered_results(response_to_individuals)
# Filter and summarize driver results
# Regenerate the impact table including model_name for joining
impact_table <- driver_results %>%
  filter(term %in% c("change_FIRES_individual", "change_FARMERS_individual")) %>%
  mutate(
    Actor = case_when(
      grepl("baptist", model_name) ~ "Baptist",
      grepl("bootlegger", model_name) ~ "Bootlegger",
      grepl("policymaker", model_name) ~ "Policymaker"
    ),
    Narrative = case_when(
      grepl("FIRES", model_name) ~ "FIRES",
      grepl("FARMERS", model_name) ~ "FARMERS"
    ),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(model_name, Narrative, Actor, Estimate = estimate, `Std. Error` = std.error, `p-value` = p.value, Significance)

# Join R² values
impact_table_with_r2 <- impact_table %>%
  left_join(r_squared_df, by = "model_name") %>%
  select(-model_name)  # Now safe to drop

# Display styled HTML table
table_4_2 <- impact_table_with_r2 %>%
  kable(
    caption = "Table 4.2. Impact of Individuals’ Narrative Use on Actor Groups (Clustered SEs by Month)",
    format = "html",
    digits = 3
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

print(table_4_2)


####Re-organize impact table

library(dplyr)
library(knitr)
library(kableExtra)

# Define correct order for sorting
desired_order <- c(
  "FIRES_bootlegger",
  "FIRES_baptist",
  "FIRES_policymaker",
  "FARMERS_bootlegger",
  "FARMERS_baptist",
  "FARMERS_policymaker"
)
glimpse(driver_results)

# Filter and format impact table
impact_table <- driver_results %>%
  filter(term %in% c("change_FIRES_individual", "change_FARMERS_individual")) %>%
  mutate(
    Actor = case_when(
      grepl("baptist", model_name) ~ "Baptist",
      grepl("bootlegger", model_name) ~ "Bootlegger",
      grepl("policymaker", model_name) ~ "Policymaker"
    ),
    Narrative = case_when(
      grepl("FIRES", model_name) ~ "FIRES",
      grepl("FARMERS", model_name) ~ "FARMERS"
    ),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(model_name, Narrative, Actor, Estimate = estimate, `Std. Error` = std.error, `p-value` = p.value, Significance)

# Ensure correct order
impact_table <- impact_table %>%
  mutate(model_name = factor(model_name, levels = desired_order)) %>%
  arrange(model_name)

# Extract R-squared values from models
r_squared_values <- sapply(response_to_individuals, function(model) {
  summary(model)$r.squared
})

# Convert R² values into a dataframe
r_squared_df <- data.frame(
  model_name = names(r_squared_values),
  R2 = round(r_squared_values, 3)
)

# Merge R² values using model_name
impact_table_with_r2 <- impact_table %>%
  left_join(r_squared_df, by = "model_name") %>%  # Ensure join works correctly
  select(-model_name)  # Remove model_name after the join

# Display table in RStudio Viewer
table_4_2 <- impact_table_with_r2 %>%
  kable(
    caption = "Impact of Individuals' Narrative Use on Actor Groups (with R²)",
    format = "html",
    digits = 3
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

print(table_4_2)

# Create the actor response plot with fixed order
actor_response_plot <- ggplot(driver_results_plot, aes(x = model_name, y = estimate)) +
  geom_point(aes(), size = 3) +  # Dots represent estimates
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                width = 0.2) +
  geom_text(aes(label = significance, vjust = -1), color = "red", size = 5) +
  coord_flip() +
  labs(
    title = "Actor Response to narrative salience",
    x = "NARRATIVE: Actor",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

actor_response_plot

#### Replace the dots with tractors and flames?

library(ggplot2)
library(dplyr)
library(ggimage)

# Define correct order for sorting
desired_order <- c(
  "FARMERS_baptist",
  "FARMERS_policymaker",
  "FARMERS_bootlegger",
  "FIRES_policymaker",
  "FIRES_baptist",
  "FIRES_bootlegger"
)

# Define image paths for fire and tractor icons
image_paths <- c("FIRES" = "fire.png", "FARMERS" = "tractor.png")

driver_results_plot <- driver_results %>%
  mutate(
    model_name = paste0(group, "_", actor),
    Narrative = group,
    Image = image_paths[Narrative],
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    model_name = factor(model_name, levels = desired_order)
  )

# Update dataset with proper ordering and assign image paths
driver_results_plot <- driver_results_plot %>%
  mutate(
    model_name = factor(model_name, levels = desired_order),  # Ensure correct order
    Narrative = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),  # Assign narrative
    Image = image_paths[Narrative]  # Assign appropriate image path
  )

# Create a lookup table for cleaner labels
# Define simpler display labels for each model_name (not unique, by design)
axis_labels <- c(
  "FARMERS_baptist" = "Baptist",
  "FARMERS_policymaker" = "Policymaker",
  "FARMERS_bootlegger" = "Bootlegger",
  "FIRES_policymaker" = "Policymaker",
  "FIRES_baptist" = "Baptist",
  "FIRES_bootlegger" = "Bootlegger"
)

# Ensure model_name is ordered and Narrative info is added
driver_results_plot <- driver_results_plot %>%
  mutate(
    model_name = factor(model_name, levels = desired_order),
    Narrative = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),
    Image = image_paths[Narrative]
  )

# Plot using model_name (for correct order), and override axis text labels manually
actor_response_plot <- ggplot(driver_results_plot, aes(x = model_name, y = estimate)) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_image(aes(image = Image), size = 0.07) +
  geom_text(aes(label = significance, vjust = -1), size = 5, color = "red") +
  coord_flip() +
  labs(
    title = "",
    x = "Actor Type",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  scale_x_discrete(labels = axis_labels) +  # Override y-axis labels
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# Display the plot
print(actor_response_plot)
################Control for activity######
response_to_individuals_control <- list(
  FIRES_bootlegger = lm(change_FIRES_bootlegger ~ change_FIRES_individual + total_tweets_bootlegger, data = narrative_changes),
  FIRES_baptist = lm(change_FIRES_baptist ~ change_FIRES_individual + total_tweets_baptist, data = narrative_changes),
  FIRES_policymaker = lm(change_FIRES_policymaker ~ change_FIRES_individual + total_tweets_public_policy_maker, data = narrative_changes),
  FARMERS_bootlegger = lm(change_FARMERS_bootlegger ~ change_FARMERS_individual + total_tweets_bootlegger, data = narrative_changes),
  FARMERS_baptist = lm(change_FARMERS_baptist ~ change_FARMERS_individual + total_tweets_baptist, data = narrative_changes),
  FARMERS_policymaker = lm(change_FARMERS_policymaker ~ change_FARMERS_individual + total_tweets_public_policy_maker, data = narrative_changes)
)
driver_results <- get_clustered_results(response_to_individuals_control)
# Filter and summarize driver results
# Regenerate the impact table including model_name for joining
impact_table <- driver_results %>%
  filter(term %in% c("change_FIRES_individual", "change_FARMERS_individual")) %>%
  mutate(
    Actor = case_when(
      grepl("baptist", model_name) ~ "Baptist",
      grepl("bootlegger", model_name) ~ "Bootlegger",
      grepl("policymaker", model_name) ~ "Policymaker"
    ),
    Narrative = case_when(
      grepl("FIRES", model_name) ~ "FIRES",
      grepl("FARMERS", model_name) ~ "FARMERS"
    ),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(model_name, Narrative, Actor, Estimate = estimate, `Std. Error` = std.error, `p-value` = p.value, Significance)

# Join R² values
impact_table_with_r2 <- impact_table %>%
  left_join(r_squared_df, by = "model_name") %>%
  select(-model_name)  # Now safe to drop

# Display styled HTML table
table_4_2 <- impact_table_with_r2 %>%
  kable(
    caption = "Table 4.2. Impact of Individuals’ Narrative Use on Actor Groups (Clustered SEs by Month)",
    format = "html",
    digits = 3
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

print(table_4_2)


####Re-organize impact table

library(dplyr)
library(knitr)
library(kableExtra)

# Define correct order for sorting
desired_order <- c(
  "FIRES_bootlegger",
  "FIRES_baptist",
  "FIRES_policymaker",
  "FARMERS_bootlegger",
  "FARMERS_baptist",
  "FARMERS_policymaker"
)
glimpse(driver_results)

# Filter and format impact table
impact_table <- driver_results %>%
  filter(term %in% c("change_FIRES_individual", "change_FARMERS_individual")) %>%
  mutate(
    Actor = case_when(
      grepl("baptist", model_name) ~ "Baptist",
      grepl("bootlegger", model_name) ~ "Bootlegger",
      grepl("policymaker", model_name) ~ "Policymaker"
    ),
    Narrative = case_when(
      grepl("FIRES", model_name) ~ "FIRES",
      grepl("FARMERS", model_name) ~ "FARMERS"
    ),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(model_name, Narrative, Actor, Estimate = estimate, `Std. Error` = std.error, `p-value` = p.value, Significance)

# Ensure correct order
impact_table <- impact_table %>%
  mutate(model_name = factor(model_name, levels = desired_order)) %>%
  arrange(model_name)

# Extract R-squared values from models
r_squared_values <- sapply(response_to_individuals, function(model) {
  summary(model)$r.squared
})

# Convert R² values into a dataframe
r_squared_df <- data.frame(
  model_name = names(r_squared_values),
  R2 = round(r_squared_values, 3)
)

# Merge R² values using model_name
impact_table_with_r2 <- impact_table %>%
  left_join(r_squared_df, by = "model_name") %>%  # Ensure join works correctly
  select(-model_name)  # Remove model_name after the join

# Display table in RStudio Viewer
table_4_2 <- impact_table_with_r2 %>%
  kable(
    caption = "Impact of Individuals' Narrative Use on Actor Groups (with R²)",
    format = "html",
    digits = 3
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

print(table_4_2)

##########Regression diagnostics
# Reuse the diagnostic function
# Reuse the diagnostic function
get_diagnostics <- function(model, model_name) {
  dw <- tryCatch({
    lmtest::dwtest(model)
  }, error = function(e) NULL)
  
  bp <- tryCatch({
    lmtest::bptest(model)
  }, error = function(e) NULL)
  
  tibble(
    model_name = model_name,
    dw_statistic = if (!is.null(dw)) as.numeric(dw$statistic) else NA,
    dw_p_value = if (!is.null(dw)) dw$p.value else NA,
    bp_statistic = if (!is.null(bp)) as.numeric(bp$statistic) else NA,
    bp_p_value = if (!is.null(bp)) bp$p.value else NA
  )
}

# Run diagnostics for each model in response_to_individuals
diagnostics_4_2 <- map_df(names(response_to_individuals), function(name) {
  get_diagnostics(response_to_individuals[[name]], model_name = name)
})

# Recode model names for clarity
diagnostics_4_2 <- diagnostics_4_2 %>%
  mutate(
    Actor = case_when(
      str_detect(model_name, "baptist") ~ "Baptist",
      str_detect(model_name, "bootlegger") ~ "Bootlegger",
      str_detect(model_name, "policymaker") ~ "Policymaker"
    ),
    Narrative = case_when(
      str_detect(model_name, "FIRES") ~ "Fires",
      str_detect(model_name, "FARMERS") ~ "Farmers"
    )
  ) %>%
  select(Narrative, Actor, 
         `Durbin-Watson stat` = dw_statistic, `DW p-value` = dw_p_value,
         `Breusch-Pagan stat` = bp_statistic, `BP p-value` = bp_p_value)

# Create a diagnostics table for Appendix
diagnostics_table_4_2 <- diagnostics_4_2 %>%
  kbl(
    format = "html", digits = 3,
    caption = "Table A4. Regression Diagnostics for Table 4.2 Models"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

print(diagnostics_table_4_2)

# Run diagnostics for each model in response_to_individuals
diagnostics_4_2 <- map_df(names(response_to_individuals), function(name) {
  get_diagnostics(response_to_individuals[[name]], model_name = name)
})



################


# Create the actor response plot with fixed order
actor_response_plot <- ggplot(driver_results_plot, aes(x = model_name, y = estimate)) +
  geom_point(aes(), size = 3) +  # Dots represent estimates
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                width = 0.2) +
  geom_text(aes(label = significance, vjust = -1), color = "red", size = 5) +
  coord_flip() +
  labs(
    title = "Actor Response to narrative salience",
    x = "NARRATIVE: Actor",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

actor_response_plot

#### Replace the dots with tractors and flames?

library(ggplot2)
library(dplyr)
library(ggimage)

# Define correct order for sorting
desired_order <- c(
  "FARMERS_baptist",
  "FARMERS_policymaker",
  "FARMERS_bootlegger",
  "FIRES_policymaker",
  "FIRES_baptist",
  "FIRES_bootlegger"
)

# Define image paths for fire and tractor icons
image_paths <- c("FIRES" = "fire.png", "FARMERS" = "tractor.png")

driver_results_plot <- driver_results %>%
  mutate(
    model_name = paste0(group, "_", actor),
    Narrative = group,
    Image = image_paths[Narrative],
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    model_name = factor(model_name, levels = desired_order)
  )

# Update dataset with proper ordering and assign image paths
driver_results_plot <- driver_results_plot %>%
  mutate(
    model_name = factor(model_name, levels = desired_order),  # Ensure correct order
    Narrative = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),  # Assign narrative
    Image = image_paths[Narrative]  # Assign appropriate image path
  )

# Create a lookup table for cleaner labels
# Define simpler display labels for each model_name (not unique, by design)
axis_labels <- c(
  "FARMERS_baptist" = "Baptist",
  "FARMERS_policymaker" = "Policymaker",
  "FARMERS_bootlegger" = "Bootlegger",
  "FIRES_policymaker" = "Policymaker",
  "FIRES_baptist" = "Baptist",
  "FIRES_bootlegger" = "Bootlegger"
)

# Ensure model_name is ordered and Narrative info is added
driver_results_plot <- driver_results_plot %>%
  mutate(
    model_name = factor(model_name, levels = desired_order),
    Narrative = ifelse(grepl("FIRES", model_name), "FIRES", "FARMERS"),
    Image = image_paths[Narrative]
  )

# Plot using model_name (for correct order), and override axis text labels manually
actor_response_plot <- ggplot(driver_results_plot, aes(x = model_name, y = estimate)) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_image(aes(image = Image), size = 0.07) +
  geom_text(aes(label = significance, vjust = -1), size = 5, color = "red") +
  coord_flip() +
  labs(
    title = "",
    x = "Actor Type",
    y = "Coefficient Estimate",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) +
  scale_x_discrete(labels = axis_labels) +  # Override y-axis labels
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# Display the plot
print(actor_response_plot)



#######SUPPLEMENTARY PACKAGE...
grep("change_FIRES_individual", readLines("April 25 - final output for reviewers.R"), value = TRUE)
grep("norm_FIRES_individual", readLines("April 25 - final output for reviewers.R"), value = TRUE)

#Goodness of fit table for 4.1, 4.2 and 4.3
# Save as HTML (for Word)
save_kable(table_4_1_control, file = "SA/Table_4_1_Goodness_of_Fit.html")
save_kable(table_4_2, file = "SA/Table_4_2_Actor_Response.html")
save_kable(table_4_3, file = "SA/Table_4_3_Split_Period_Response.html")

#Descriptives
## Load necessary libraries
library(dplyr)
library(gt)

# Create descriptive statistics table
desc_stats <- narrative_matches_subset %>%
  mutate(
    FIRES_binary = as.integer(FIRES),    # Convert TRUE/FALSE to 1/0
    FARMERS_binary = as.integer(FARMERS)
  ) %>%
  group_by(class) %>%
  summarise(
    n_tweets = n(),                                        # Number of tweets per actor group
    pct_using_FIRES = mean(FIRES_binary, na.rm = TRUE) * 100,  # % using FIRES narrative
    pct_using_FARMERS = mean(FARMERS_binary, na.rm = TRUE) * 100, # % using FARMERS narrative
    .groups = "drop"
  ) %>%
  arrange(desc(n_tweets))  # Optional: sort by tweet volume

# Generate and save the table
desc_stats %>%
  gt() %>%
  fmt_number(columns = c(pct_using_FIRES, pct_using_FARMERS), decimals = 1) %>%
  cols_label(
    class = "Actor Group",
    n_tweets = "Number of Tweets",
    pct_using_FIRES = "% Using FIRES Narrative",
    pct_using_FARMERS = "% Using FARMERS Narrative"
  ) %>%
  tab_header(
    title = "Descriptive Statistics: Narrative Use by Actor Group"
  ) %>%
  gtsave("SA/Table_Descriptive_Narrative_Use.html")  # Save as HTML


########Diagnostic Tests:
# Load necessary libraries
library(lmtest)
library(sandwich)
library(dplyr)
library(broom)
library(kableExtra)

# Assuming your models are stored here:
# models_table_4_1_control <- list(...)

# Function to extract diagnostics
get_diagnostics <- function(model, model_name) {
  dw <- tryCatch(dwtest(model), error = function(e) NULL)
  bp <- tryCatch(bptest(model), error = function(e) NULL)
  
  tibble(
    Model = model_name,
    DW_statistic = if (!is.null(dw)) as.numeric(dw$statistic) else NA,
    DW_p_value = if (!is.null(dw)) dw$p.value else NA,
    BP_statistic = if (!is.null(bp)) as.numeric(bp$statistic) else NA,
    BP_p_value = if (!is.null(bp)) bp$p.value else NA
  )
}

# Run diagnostics
diagnostic_results_4_1 <- bind_rows(
  lapply(names(models_table_4_1_control), function(name) {
    get_diagnostics(models_table_4_1_control[[name]], model_name = name)
  })
)

# Save as a nice HTML table
diagnostic_results_4_1 %>%
  mutate(
    DW_p_value = sprintf("%.3f", DW_p_value),
    BP_p_value = sprintf("%.3f", BP_p_value)
  ) %>%
  kbl(
    digits = 3,
    caption = "Table A1. Regression Diagnostics for Table 4.1 Models",
    col.names = c("Model", "DW Statistic", "DW p-value", "BP Statistic", "BP p-value"),
    format = "html"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable("SA/Table_Diagnostics_Table4_1.html")

###Table 4-2
# Assuming your models are stored here:
# response_to_individuals <- list(...)

# Function to run diagnostics
run_diagnostics_4_2 <- function(model_list) {
  tibble(
    Model = names(model_list),
    DW_statistic = sapply(model_list, function(m) dwtest(m)$statistic),
    DW_p_value   = sapply(model_list, function(m) dwtest(m)$p.value),
    BP_statistic = sapply(model_list, function(m) bptest(m)$statistic),
    BP_p_value   = sapply(model_list, function(m) bptest(m)$p.value)
  )
}

# Run diagnostics
diagnostic_results_4_2 <- run_diagnostics_4_2(response_to_individuals)

# Save as a nice HTML table
diagnostic_results_4_2 %>%
  mutate(
    DW_p_value = sprintf("%.3f", DW_p_value),
    BP_p_value = sprintf("%.3f", BP_p_value)
  ) %>%
  kbl(
    digits = 3,
    caption = "Table A2. Regression Diagnostics for Table 4.2 Models",
    col.names = c("Model", "DW Statistic", "DW p-value", "BP Statistic", "BP p-value"),
    format = "html"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable("SA/Table_Diagnostics_Table4_2.html")

#### Table 4-3
# Assuming your models are stored here:
# models_pre <- list(...)
# models_post <- list(...)

# Function to run diagnostics and label by period
run_diagnostics_4_3 <- function(models, period_label) {
  bind_rows(
    lapply(names(models), function(name) {
      model <- models[[name]]
      dw <- dwtest(model)
      bp <- bptest(model)
      
      tibble(
        Model = name,
        Period = period_label,
        DW_statistic = dw$statistic,
        DW_p_value = dw$p.value,
        BP_statistic = bp$statistic,
        BP_p_value = bp$p.value
      )
    })
  )
}

# Run diagnostics
diagnostic_results_pre <- run_diagnostics_4_3(models_pre, "Agreement pending")
diagnostic_results_post <- run_diagnostics_4_3(models_post, "Ratification stalled")
diagnostic_results_4_3 <- bind_rows(diagnostic_results_pre, diagnostic_results_post)

# Save as a nice HTML table
diagnostic_results_4_3 %>%
  mutate(
    DW_statistic = round(DW_statistic, 3),
    DW_p_value = sprintf("%.3f", DW_p_value),
    BP_statistic = round(BP_statistic, 3),
    BP_p_value = sprintf("%.3f", BP_p_value)
  ) %>%
  kbl(
    digits = 3,
    caption = "Table A3. Regression Diagnostics for Table 4.3 Models (Pre- and Post-Agreement)",
    col.names = c("Model", "Period", "DW Statistic", "DW p-value", "BP Statistic", "BP p-value"),
    format = "html"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable("SA/Table_Diagnostics_Table4_3.html")
####VIFs?
library(car)

# Compute VIFs for activity-controlled models
vif_controlled <- lapply(response_to_individuals_control, function(model) {
  vif_df <- as.data.frame(vif(model))
  vif_df$Variable <- rownames(vif_df)
  rownames(vif_df) <- NULL
  vif_df
})

names(vif_controlled) <- names(response_to_individuals_control)
vif_combined <- dplyr::bind_rows(vif_controlled, .id = "Model")
print(vif_combined)

library(kableExtra)

# Create VIF summary table with custom caption and formatting
vif_combined %>%
  mutate(
    `VIF` = round(`vif(model)`, 3),  # Rename and round VIF values
    Variable = gsub("_", " ", Variable),  # Optional: clean variable names
    Model = gsub("_", " ", Model)        # Optional: clean model names
  ) %>%
  select(Model, Variable, VIF) %>%
  kbl(
    format = "html",
    caption = "Table A3. Variance Inflation Factors (VIF) for Activity-Controlled Models",
    align = "llc",
    col.names = c("Model", "Variable", "VIF")
  ) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

