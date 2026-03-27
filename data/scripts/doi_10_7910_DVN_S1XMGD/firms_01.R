### Firms
### 01 Data preparation and descriptive statistics

rm( list = ls( ) )

### Load packages
install.packages("pacman")
pacman::p_load(dplyr, prodest, ggplot2)

### Set working directory
setwd("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Data")

### Load firm data
load("firm_ownership_imp.Rdata")

########### 1. Estimation of TFP ###########
tfp_prod <- firm_ownership_imp %>% 
  dplyr::ungroup( ) %>%
  dplyr::select( BVDID, year, EMPL, TFAS, AV, INVEST ) %>%
  tidyr::drop_na( ) %>%
  dplyr::filter( !( EMPL == 0 | TFAS == 0 | AV == 0 | INVEST == 0 ) ) %>%
  dplyr::mutate( dplyr::across( .cols = c( EMPL, TFAS, AV, INVEST ), 
                                .fns = ~log( ( . ) ), 
                                .names = "transform_{.col}" ) ) %>%
  tidyr::drop_na( )

WRDG_results <- prodest::prodestWRDG( Y = tfp_prod$transform_AV, 
                                      fX = tfp_prod$transform_EMPL, 
                                      sX = tfp_prod$transform_TFAS, 
                                      pX = tfp_prod$transform_INVEST, 
                                      idvar = as.numeric( as.factor( tfp_prod$BVDID ) ), 
                                      timevar = tfp_prod$year )
summary( WRDG_results )

tfp_results <- tibble(WRDG = c( prodest::omega( WRDG_results ) ), 
                       id = seq( 1, nrow( prodest::omega( WRDG_results ) ), 1 ) ) 

tfp_prod$tfp_WRDG <- tfp_results$WRDG


tfp_prod <- tfp_prod %>%
  dplyr::select( BVDID, year, tfp_WRDG )

### Add TFP estimates to firm data
firm_ownership_imp <- dplyr::left_join( firm_ownership_imp, tfp_prod, by = c( "BVDID", "year" ) )
firm_ownership_imp_0 <- dplyr::left_join( firm_ownership_imp_0, tfp_prod, by = c( "BVDID", "year" ) )
firm_ownership_imp_threshold <- dplyr::left_join( firm_ownership_imp_threshold, tfp_prod, by = c( "BVDID", "year" ) )


########### 2. Prepare data for analysis ###########

### Remove all firms with more or less than one ownership change in the observation period
firm_ownership_change <- firm_ownership_imp %>%
  dplyr::filter(oc_n == 1) %>%
  dplyr::mutate(exit = ifelse(last_reporting < 2016, 1, 0))
firm_ownership_exit <- firm_ownership_change %>%
  dplyr::filter(exit == 1) 
firm_ownership_stay <- firm_ownership_change %>%
  dplyr::filter(exit == 0)


########### 3. Descriptive statistics ###########

### Table 1
# total emissions
summary(firm_ownership_change$adj_emissions_tot)
sd(firm_ownership_change$adj_emissions_tot, na.rm = TRUE)
quantile(firm_ownership_change$adj_emissions_tot, probs = c(0.25, 0.75), na.rm = TRUE)
# operating revenues
summary(firm_ownership_change$OPRE)
sd(firm_ownership_change$OPRE, na.rm = TRUE)
quantile(firm_ownership_change$OPRE, probs = c(0.25, 0.75), na.rm = TRUE)
# emissions intensity
summary(firm_ownership_change$em_intensity)
sd(firm_ownership_change$em_intensity, na.rm = TRUE)
quantile(firm_ownership_change$em_intensity, probs = c(0.25, 0.75), na.rm = TRUE)
# TFP
summary(firm_ownership_change$tfp_WRDG)
sd(firm_ownership_change$tfp_WRDG, na.rm = TRUE)
quantile(firm_ownership_change$tfp_WRDG, probs = c(0.25, 0.75), na.rm = TRUE)
# Employment
summary(firm_ownership_change$EMPL)
sd(firm_ownership_change$EMPL, na.rm = TRUE)
quantile(firm_ownership_change$EMPL, probs = c(0.25, 0.75), na.rm = TRUE)
# Capital
summary(firm_ownership_change$TFAS)
sd(firm_ownership_change$TFAS, na.rm = TRUE)
quantile(firm_ownership_change$TFAS, probs = c(0.25, 0.75), na.rm = TRUE)
# Intangibles
summary(firm_ownership_change$IFAS)
sd(firm_ownership_change$IFAS, na.rm = TRUE)
quantile(firm_ownership_change$IFAS, probs = c(0.25, 0.75), na.rm = TRUE)
# Labor expenditures
summary(firm_ownership_change$STAF)
sd(firm_ownership_change$STAF, na.rm = TRUE)
quantile(firm_ownership_change$STAF, probs = c(0.25, 0.75), na.rm = TRUE)
# Operating profits
summary(firm_ownership_change$OPPL)
sd(firm_ownership_change$OPPL, na.rm = TRUE)
quantile(firm_ownership_change$OPPL, probs = c(0.25, 0.75), na.rm = TRUE)


### Figure B1
country_counts <- firm_ownership_imp %>%
  dplyr::filter(Country_SUBBVDID != "BR" & Country_SUBBVDID != "RS") %>%
  dplyr::group_by(Country_SUBBVDID) %>%
  dplyr::summarize(n = sum(ownership_change_GUO25C, na.rm = TRUE),
                   sum = n()) %>%
  dplyr::mutate(proportion = n / sum)

# Panel A
B1A <- ggplot(country_counts, aes(x = as.factor(Country_SUBBVDID), y = n)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(y = "Number of ownership changes", 
       x = "Country") +
  theme_classic( ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )
B1A
ggsave( file = paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB1a.png"  ), plot = B1A )


# Panel B
B1B <- ggplot(country_counts, aes(x = as.factor(Country_SUBBVDID), y = proportion)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(y = "Percentage of observations with ownership changes", 
       x = "Country") +
  theme_classic( ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )
B1B
ggsave( file = paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB1b.png"  ), plot = B1B )

### Figure B2
sector_counts <- firm_ownership_imp %>%
  dplyr::group_by(NACE_2) %>%
  dplyr::summarize(n = sum(ownership_change_GUO25C, na.rm = TRUE),
                   sum = n()) %>%
  dplyr::mutate(proportion = n / sum)

# Panel A
B2A <- ggplot(sector_counts, aes(x = as.factor(NACE_2), y = n)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(y = "Number of ownership changes", 
       x = "Sector") +
  theme_classic( ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )
B2A
ggsave( file = paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB2a.png"  ), plot = B2A )


# Panel B
B2B <- ggplot(sector_counts, aes(x = as.factor(NACE_2), y = proportion)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(y = "Percentage of observations with ownership changes", 
       x = "Sector") +
  theme_classic( ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )
B2B
ggsave( file = paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB2b.png"  ), plot = B2B )


### Figure B3
year_counts <- firm_ownership_imp %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(n = sum(ownership_change_GUO25C, na.rm = TRUE),
                   sum = n()) %>%
  dplyr::mutate(proportion = n / sum)

# Panel A
B3A <- ggplot(year_counts, aes(x = as.factor(year), y = n)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(y = "Number of ownership changes", 
       x = "Year") +
  theme_classic( ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )
B3A
ggsave( file = paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB3a.png"  ), plot = B3A )


# Panel B
B3B <- ggplot(year_counts, aes(x = as.factor(year), y = proportion)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(y = "Percentage of observations with ownership changes", 
       x = "Year") +
  theme_classic( ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )
B3B
ggsave( file = paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB3b.png"  ), plot = B3B )

### Figure B5
plot_firm_entry_exit <- firm_ownership_change %>%
  dplyr::ungroup( ) %>%
  dplyr::filter( oc_n == 1 ) %>%
  dplyr::distinct( BVDID, .keep_all = TRUE ) %>%
  select( BVDID, first_reporting, last_reporting ) %>% 
  tidyr::pivot_longer( -BVDID ) %>%
  ggplot( ) + 
  theme_classic( ) + 
  geom_bar( mapping = aes( x = value, fill = name ), position = "dodge" ) + 
  scale_fill_manual( values = c( "black", "blue" ), labels = c( "First report", "Last report" ) ) +
  scale_y_continuous( expand = c( 0, 0 ) ) + 
  scale_x_continuous( expand = c( 0, 0 ), breaks = seq( 2007, 2016, 1 ), labels = seq( 2007, 2016, 1 ) ) + 
  labs( x = "Year", y = "Number of firms" ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom", legend.title = element_blank( ) )

ggsave( file = paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB5.png"  ), plot = plot_firm_entry_exit )


### Figure B6
own_change_diff <-  firm_ownership_imp %>% 
  dplyr::ungroup( ) %>%
  dplyr::mutate( own_change = ( oc_n >= 1 ) ) %>%
  dplyr::mutate( own_change = ifelse( oc_n == 0, 0, 
                                      ifelse( oc_n == 1, 1, 2 ) ) ) %>%
  dplyr::group_by( own_change ) %>%
  dplyr::select( own_change, adj_emissions_tot, OPRE, TFAS, EMPL, LTDB ) %>%
  tidyr::pivot_longer( -own_change ) %>%                    
  dplyr::mutate( value = log( ( value ) + ( sqrt( value )^2 + 1 ) ), 
                 name = ifelse( name == "adj_emissions_tot", "Emissions", 
                                ifelse( name == "EMPL", "Employment", 
                                        ifelse( name == "LTDB", "Debt", 
                                                ifelse( name == "OPRE", "Revenues", "Capital" ) ) ) ) ) %>%
  ggplot( mapping = aes( x = name ) ) + 
  theme_classic( ) +
  geom_boxplot( mapping = aes( y = value, x = name, fill = as.factor( own_change ) ), 
                position = "dodge", 
                outlier.shape = NA ) + 
  labs( x = "Firm characteristics", y = "Transformed values", fill = "Number of ownership changes" ) + 
  scale_fill_manual( values = c( "grey", "red", "dark green" ), labels = c( "0", "1", ">1" ) ) + 
  theme( axis.title = element_text( size = 14 ), axis.text = element_text( size = 10 ) ) + 
  theme( legend.position = "bottom" ) 

ggsave( file = paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureB6.png"  ), plot = own_change_diff )














