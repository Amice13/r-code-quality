### Facilities
### 01 Data preparation and descriptive statistics

rm( list = ls( ) )

### Load packages
install.packages("pacman")
pacman::p_load(dplyr, ggplot2)

### Set working directory
setwd("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Data")

### Load facility and firm data
load("facility_ownership_vge.Rdata")
load("firm_ownership_imp.Rdata")

### Remove all facilities with more or less than one ownership change in the observation period
facility_ownership_change <- eprtr_facility_pollution %>%
  dplyr::filter(Num_changes_GUO25C == 1)

### Table 1
summary(facility_ownership_change$adj_emissions_tot)

### Figure 1
dist_own_change_one_facility_firm <- eprtr_facility_pollution %>% 
  dplyr::filter( Num_changes_GUO25C == 1 ) %>%
  dplyr::distinct( FacilityID, .keep_all = TRUE ) %>%
  dplyr::mutate( type = "Facility" ) %>%
  select( type, ownership_change_year ) %>%
  dplyr::bind_rows( 
    firm_ownership_imp %>%
      dplyr::ungroup( ) %>%
      dplyr::filter( oc_n == 1 ) %>%
      dplyr::distinct( BVDID, .keep_all = TRUE ) %>%
      dplyr::mutate( type = "Firm" ) %>%
      dplyr::select( type, ownership_change_year ) ) %>%
  ggplot( ) + 
  theme_classic( ) +
  geom_bar( mapping = aes( x = as.factor( ownership_change_year ), fill = type ), position = "dodge" ) + 
  labs( x = "Year", y = "Number of ownership changes" ) + 
  scale_y_continuous( expand = c( 0, 0 ), limits = c( 0, 300 ) ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  scale_fill_manual( values = c( "gray40", "gray70" ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )

dist_own_change_one_facility_firm

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "Figure1.png" ), 
        plot = dist_own_change_one_facility_firm,
        width = 6, height = 4 )

### Figure B4
plot_facility_entry_exit <- eprtr_facility_pollution %>% 
  dplyr::filter( Num_changes_GUO25C == 1 ) %>%
  dplyr::distinct( FacilityID, .keep_all = TRUE ) %>%
  select( FacilityID, first_report_facility, last_report_facility ) %>% 
  tidyr::pivot_longer( -FacilityID ) %>%
  ggplot( ) + 
  theme_classic( ) + 
  geom_bar( mapping = aes( x = value, fill = name ), position = "dodge" ) + 
  scale_fill_manual( values = c( "gray40", "gray70" ), labels = c( "First report", "Last report" ) ) +
  scale_y_continuous( expand = c( 0, 0 ) ) + 
  scale_x_continuous( expand = c( 0, 0 ), breaks = seq( 2007, 2016, 1 ), labels = seq( 2007, 2016, 1 ) ) + 
  ggtitle( "Facilities" ) + 
  labs( x = "Year", y = "Number of facilities" ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )

plot_facility_entry_exit

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB4.png" ), 
        plot = plot_facility_entry_exit,
        width = 6, height = 4 )















