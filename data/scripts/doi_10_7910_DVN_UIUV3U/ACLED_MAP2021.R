# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# ACLED_MAP2021.R
# Generate Figure 9A in main paper
# Jennifer Lin and Kristin Lunz Trujillo
# Created On: 2021 08 11

# ** Before running this file, ensure that you have ran
#   - ACLED_FUNCTIONS.R
#   - Load_RUCA.R
#  or components of this code will result in an error. See 
#  00_GENERAL.R for more details **

# *****************************************************************
# LOAD DATA ####
# *****************************************************************

# Load the simplified data that we provide with our replication
#   files
protest21 <- read_csv(
  "Aux_Data/acled_county_21.csv",
  show_col_types = FALSE) %>% 
  select(-1)

# Parse the census codes to state, county, tract and block
protest21 <- parse_census_codes(protest21)

# *****************************************************************
# CREATE MAP ####
# *****************************************************************

# Generate a data frame that contains the number of protests
#   per county
protests_sum <- protest21 %>% 
  mutate(
    tractid = paste0(county, tract)
  ) %>% 
  group_by(county) %>% 
  summarise(
    n = n()
  )

# Join the above data with the US Counties shape file
us_counties <- left_join(
  us_counties,
  protests_sum,
  by = c("GEOID" = "county")
) %>% 
  mutate(
    n = replace_na(n, 0)
  )

# Generate map
ggplot()+ 
  geom_sf(
    us_counties, 
    mapping = aes(
      geometry = geometry, 
      fill = n),
    size = 0.5
  )+
  labs(
    fill = "Number of\nProtests",
    title = "Protests in the United States",
    subtitle = "By County",
    caption = "Data: ACLED -- North America"
  )+
  scale_fill_gradient(
    low = "gray100",
    high = "gray0",
    na.value = "black", 
    limits   = c(0, 50)
  )+
  theme_bw()+
  theme(
    title      = element_text(size = 20, colour="black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.text = element_text(size = 12, colour="black")
  )

# *****************************************************************
# CORRELATIONS ####
# *****************************************************************

# The following code generates a correlation between the number
#   of protests and the RUCC codes for the county.

# Prepare the data by joining the data frame used for mapping
#   with the RUCC codes data
protest_w_rucc <- left_join(
  us_counties,
  rucc,
  by = c("GEOID" = "FIPS")
)

# Calculate correlation
cor(
  protest_w_rucc$RUCC_2013, 
  protest_w_rucc$n, use = "complete.obs")
