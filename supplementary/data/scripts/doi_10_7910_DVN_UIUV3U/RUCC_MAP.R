# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# RUCC_MAP.R
# Generate RUCC Map in Supplemental Appendix G
# Jennifer Lin and Kristin Lunz Trujillo
# Created On: 2021 08 11

# ** Before running this file, ensure that you have ran
#   - ACLED_FUNCTIONS.R
#   - Load_RUCA.R
#  or components of this code will result in an error. See 
#  00_GENERAL.R for more details **

# To create the generat RUCC map, join the RUCC codes with the
#   US Counties shape file.
us_rural <- left_join(
  us_counties,
  rucc,
  by = c("GEOID" = "FIPS")
) 

# Generate the map
ggplot()+ 
  geom_sf(
    us_rural, 
    mapping = aes(
      geometry = geometry, 
      fill = RUCC_2013),
    size = 0.5
  )+
  labs(
    fill = "RUCC Score",
    title = "Rural-Urban Continuum Codes",
    subtitle = "By County",
    caption = "Data: Rural-Urban Continuum Codes (USDA)"
  )+
  scale_fill_gradient(
    low = "gray0",
    high = "gray100"
    #na.value = "black", 
    #limits   = c(0, 200)
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
