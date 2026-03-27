### Parent companies
### 01 Data preparation and analysis for parent company level and average facility of parent company level

rm( list = ls( ) )

### Load packages
install.packages("pacman")
pacman::p_load(dplyr, ggplot2, prodest)

### Set working directory
setwd("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Data")

### Load facility, firm and GUO data
load("facility_ownership_vge.Rdata")
load("firm_ownership_imp.Rdata")


############# 1. Prepare data by aggregating facilities at GUO level and merging with firm characteristics #################

GUO_facility <- eprtr_facility_pollution %>%
  dplyr::mutate( exit = ifelse( adj_emissions_tot == 0, 1, 0 ), 
                 new_owner_facility = ifelse( time_to_treat == 0, 1, 0 ), 
                 last_owner_year = ifelse( time_to_treat == -1, 1, 0 ) ) %>%
  dplyr::mutate(facility_exit = ifelse(last_report_facility < 2016 & last_report_facility >= ownership_change_year, 1, 0)) %>%
  dplyr::mutate(facility_oc_exit = ifelse(facility_exit == 1 & treat == 1, 1, 0)) %>%
  dplyr::mutate(facility_stay = ifelse(last_report_facility == 2016, 1, 0)) %>%
  dplyr::mutate(facility_oc_stay = ifelse(facility_stay == 1 & treat == 1, 1, 0)) %>%
  dplyr::group_by( GUO25C, ReportingYear ) %>%
  dplyr::summarise( number_facilities = n( ), 
                    exit_facilities = sum( exit, na.rm = TRUE ), 
                    adj_emissions_tot = sum( adj_emissions_tot, na.rm = TRUE ), 
                    adj_emissions_air = sum( adj_emissions_air, na.rm = TRUE ), 
                    adj_emissions_water = sum( adj_emissions_water, na.rm = TRUE ), 
                    adj_emissions_land = sum( adj_emissions_land, na.rm = TRUE ), 
                    new_owned_facilities = sum( new_owner_facility, na.rm = TRUE ), 
                    losing_facilities_next_year = sum( last_owner_year, na.rm = TRUE ), 
                    facilities_with_oc = sum( treat, na.rm = TRUE ),
                    facility_sum_exit = sum(facility_oc_exit, na.rm = TRUE),
                    facility_sum_stay = sum(facility_oc_stay, na.rm = TRUE)) %>% 
  dplyr::filter( is.na( as.numeric( GUO25C ) ) )


### Restrict to parent companies with maximum number of facilities larger than one
multiple_facilities <- GUO_facility %>% 
  dplyr::group_by( GUO25C ) %>%
  dplyr::summarise( nu = max( number_facilities, na.rm = TRUE ) ) %>%
  dplyr::filter( nu > 1 ) %>%
  dplyr::select( GUO25C ) 

GUO_facility_2 <- GUO_facility %>%
  dplyr::filter( GUO25C %in% multiple_facilities$GUO25C ) %>%
  dplyr::mutate( event_year = ifelse( new_owned_facilities > 0, ReportingYear, NA ) ) %>%
  tidyr::fill( event_year, .direction = "downup" ) %>%
  ## Restrict to GUO with at least one new facility 
  dplyr::filter( !is.na( event_year ) ) %>%
  dplyr::group_by( GUO25C ) %>%
  # Number of ownership events
  dplyr::mutate( n_oc = length( unique( event_year ) ) ) %>%
  dplyr::ungroup( ) %>%
  dplyr::mutate( time_to_treat = ReportingYear - event_year, 
                 treat = 1 ) %>%
  dplyr::filter( n_oc == 1 )


### Add firm level variables 
GUO_firm <- firm_ownership_imp %>%
  dplyr::group_by( GUO25C, ReportingYear ) %>%
  dplyr::summarise( across( .cols = c( OPRE, EMPL, OPEXP, OPPL, INVEST, PLAT,
                                       TFAS, LTDB, AV, STAF, RD, IFAS, TOAS ), 
                            .fns = ~sum( .x, na.rm = TRUE )  ) ) 


GUO_facility_firm <- dplyr::left_join( GUO_facility_2, GUO_firm, 
                                       by = c( "GUO25C", "ReportingYear" ) )

##### Estimate TFP for GUO
# Restrict to relevant variables and remove zero and missing values from sample
tfp_prod <- GUO_facility_firm %>% 
  dplyr::ungroup( ) %>%
  dplyr::select( GUO25C, ReportingYear, EMPL, TFAS, AV, INVEST ) %>%
  tidyr::drop_na( ) %>%
  dplyr::filter( !( EMPL == 0 | TFAS == 0 | AV == 0 | INVEST == 0 ) ) %>%
  dplyr::mutate( dplyr::across( .cols = c( EMPL, TFAS, AV, INVEST ), 
                                #.fns = ~log( ( . ) + ( sqrt( . )^2 + 1 ) ), 
                                .fns = ~log( ( . ) ), 
                                .names = "transform_{.col}" ) ) %>%
  tidyr::drop_na( )



WRDG_results <- prodest::prodestWRDG( Y = tfp_prod$transform_AV, 
                                      fX = tfp_prod$transform_EMPL, 
                                      sX = tfp_prod$transform_TFAS, 
                                      pX = tfp_prod$transform_INVEST, 
                                      idvar = as.numeric( as.factor( tfp_prod$GUO25C ) ), 
                                      timevar = tfp_prod$ReportingYear )

summary( WRDG_results )

# Extract tfp estimates
tfp_prod$tfp_WRDG <- prodest::omega( WRDG_results )

# Join tfp estimates to dataset
GUO_facility_firm <- dplyr::left_join( GUO_facility_firm, tfp_prod[ , c( "GUO25C", "ReportingYear", "tfp_WRDG" ) ], 
                                       by = c( "GUO25C", "ReportingYear" ) )


#### Generate additional variables
GUO_facility_firm <- GUO_facility_firm %>%
  dplyr::ungroup( ) %>%
  dplyr::mutate( OPRE_facility = OPRE / number_facilities, 
                 EMPL_facility = EMPL / number_facilities, 
                 TFAS_facility = TFAS / number_facilities, 
                 OPPL_facility = OPPL / number_facilities, 
                 PLAT_facility = PLAT / number_facilities, 
                 RD_facility = RD / number_facilities,
                 IFAS_facility = IFAS / number_facilities, 
                 tfp_WRDG_facility = tfp_WRDG / number_facilities,  
                 adj_emissions_tot_facility = adj_emissions_tot / number_facilities, 
                 STAF_facility = STAF / number_facilities,
                 EMPL_OPRE = EMPL / OPRE, 
                 TFAS_OPRE = TFAS / OPRE, 
                 IFAS_OPRE = IFAS / OPRE, 
                 OPPL_OPRE = OPPL / OPRE,
                 PLAT_OPRE = PLAT / OPRE,
                 RD_OPRE = RD / OPRE, 
                 tfp_WRDG_OPRE = tfp_WRDG / OPRE, 
                 STAF_OPRE = STAF / OPRE, 
                 adj_emissions_tot_OPRE = adj_emissions_tot / OPRE * 100000,
                 adj_emissions_tot_OPRE_facility = adj_emissions_tot_OPRE / number_facilities ) %>%
  dplyr::mutate( adj_emissions_tot_OPRE = ifelse( adj_emissions_tot_OPRE == "NaN" | adj_emissions_tot_OPRE == "Inf" , NA, adj_emissions_tot_OPRE),
                 adj_emissions_tot_OPRE_facility = ifelse( adj_emissions_tot_OPRE_facility == "NaN" | adj_emissions_tot_OPRE_facility == "Inf", NA, adj_emissions_tot_OPRE_facility))

### dataset with only GUOs that keep all their acquired facilities in operation
GUO_facility_firm_stay <- GUO_facility_firm %>%
  dplyr::filter(facility_sum_exit == 0)


############# 2. Descriptive statistics #################

### Table 1
# total emissions
summary(GUO_facility_firm$adj_emissions_tot)
sd(GUO_facility_firm$OPPL, na.rm = TRUE)
quantile(GUO_facility_firm$OPPL, probs = c(0.25, 0.75), na.rm = TRUE)
# operating revenues
summary(GUO_facility_firm$OPRE)
sd(GUO_facility_firm$OPRE, na.rm = TRUE)
quantile(GUO_facility_firm$OPRE, probs = c(0.25, 0.75), na.rm = TRUE)
# emissions intensity
summary(GUO_facility_firm$adj_emissions_tot_OPRE, na.rm = TRUE)
sd(GUO_facility_firm$adj_emissions_tot_OPRE, na.rm = TRUE)
quantile(GUO_facility_firm$adj_emissions_tot_OPRE, probs = c(0.25, 0.75), na.rm = TRUE)
# TFP
summary(GUO_facility_firm$tfp_WRDG, na.rm = TRUE)
sd(GUO_facility_firm$tfp_WRDG, na.rm = TRUE)
quantile(GUO_facility_firm$tfp_WRDG, probs = c(0.25, 0.75), na.rm = TRUE)
# Employment
summary(GUO_facility_firm$EMPL)
sd(GUO_facility_firm$EMPL, na.rm = TRUE)
quantile(GUO_facility_firm$EMPL, probs = c(0.25, 0.75), na.rm = TRUE)
# Capital
summary(GUO_facility_firm$TFAS)
sd(GUO_facility_firm$TFAS, na.rm = TRUE)
quantile(GUO_facility_firm$TFAS, probs = c(0.25, 0.75), na.rm = TRUE)
# Intangibles
summary(GUO_facility_firm$IFAS)
sd(GUO_facility_firm$IFAS, na.rm = TRUE)
quantile(GUO_facility_firm$IFAS, probs = c(0.25, 0.75), na.rm = TRUE)
# Labor expenditures
summary(GUO_facility_firm$STAF)
sd(GUO_facility_firm$STAF, na.rm = TRUE)
quantile(GUO_facility_firm$STAF, probs = c(0.25, 0.75), na.rm = TRUE)
# Operating profits
summary(GUO_facility_firm$OPPL)
sd(GUO_facility_firm$OPPL, na.rm = TRUE)
quantile(GUO_facility_firm$OPPL, probs = c(0.25, 0.75), na.rm = TRUE)


################### 3.  Main analysis #############################################

### Prepare function for estimation
reg_fun_all <- function( dat, outcome, cluster, IHS = FALSE, t_window = 7, yaxis = "Estimate and 95% Conf. Int", 
                         SAVE_PLOT = FALSE, lab_size = 1, file_name ){
  ### Prepare data
  dat_fin <- dat %>% 
    dplyr::mutate( time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat < -t_window, -t_window, time_to_treat ), 
                   time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat >= t_window, t_window-1, time_to_treat ) )
  
  dat_fin <- dat_fin %>% 
    dplyr::mutate( outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Year = ReportingYear, 
                   ParentCompany = GUO25C )
  
  if( IHS == TRUE ){
    dat_fin$outcome_var <- asinh( dat_fin$outcome_var )  
  }else{}
  
  
  ### TWFE results
  results_TWFE <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                   Year + ParentCompany, 
                                 data = dat_fin, cluster = c( "cluster_var" ) )
  
  ### Sun and Abraham (2021) results
  results_sunab <- fixest::feols( fml = outcome_var ~ sunab( event_year, time_to_treat, bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                    Year + ParentCompany, 
                                  data = dat_fin[ dat_fin$Year < 2016, ], cluster = c( "cluster_var" ) )  
  
  print( summary( results_sunab, agg = "att" ) )
  
  results_all <- tibble(  variable = c( rep( c( seq( from = -t_window, to = -2, by = 1 ), seq( from = 0, to = t_window-1, by = 1 ) ), 2 ), 
                                        rep( -1, 2 ) ),
                          estimate = c( results_TWFE$coeftable[ , 1 ], 
                                        results_sunab$coeftable[ , 1 ], 
                                        rep( 0, 2 ) ), 
                          se = c( results_TWFE$coeftable[ , 2 ], 
                                  results_sunab$coeftable[ , 2 ], 
                                  rep( 0, 2 ) ),
                          type = c( rep( "TWFE", 2 * t_window - 1 ), 
                                    rep( "SUNAB", 2 * t_window - 1 ),
                                    "TWFE", "SUNAB" ) ) %>%
    dplyr::mutate( conf_low = estimate - se * 1.96, 
                   conf_high = estimate + se * 1.96 )
  
  if( SAVE_PLOT == TRUE ){
    png( paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png" ), width = 600, height = 400 )
    print( 
      ggplot( data = results_all ) + 
        theme_classic( ) + 
        geom_point( mapping = aes( x = variable, y = estimate, group = type, shape = type ), 
                    position = position_dodge( width = 0.75 ) ) + 
        geom_errorbar( mapping = aes( x = variable, ymin = conf_low, ymax = conf_high, linetype = type ), 
                       position = position_dodge( width = 0.75 ) ) + 
        labs( y = yaxis, x = "Time from ownership change" ) + 
        geom_vline( mapping = aes( xintercept = -0.5 ), linetype = "dashed" ) + 
        geom_hline( mapping = aes( yintercept = 0 ), linetype = "solid" ) + 
        scale_shape_manual(values = c(16, 17)) +
        scale_linetype_manual(values = c("solid", "dashed")) + 
        scale_x_continuous( breaks = seq( -t_window, t_window -1, 1 ), labels = seq( -t_window, t_window -1, 1 ) ) + 
        theme( panel.grid.major = element_line( ), 
               panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) + 
        theme( axis.title = element_text( size = lab_size * 15 ), 
               axis.text = element_text( size = lab_size * 15 * 0.8 ), 
               legend.text = element_text( size = lab_size * 15 * 0.5 ) ) + 
        theme( legend.position = "bottom", legend.title = element_blank( ) )
    )
    dev.off( )
  }else{
    print( 
      ggplot( data = results_all ) + 
        theme_classic( ) + 
        geom_point( mapping = aes( x = variable, y = estimate, group = type, shape = type ), 
                    position = position_dodge( width = 0.75 ) ) + 
        geom_errorbar( mapping = aes( x = variable, ymin = conf_low, ymax = conf_high, linetype = type ), 
                       position = position_dodge( width = 0.75 ) ) + 
        labs( y = yaxis, x = "Time from ownership change" ) + 
        geom_vline( mapping = aes( xintercept = -0.5 ), linetype = "dashed" ) + 
        geom_hline( mapping = aes( yintercept = 0 ), linetype = "solid" ) + 
        scale_shape_manual(values = c(16, 17)) +
        scale_linetype_manual(values = c("solid", "dashed")) + 
        scale_color_grey() + 
        scale_x_continuous( breaks = seq( -t_window, t_window -1, 1 ), labels = seq( -t_window, t_window -1, 1 ) ) + 
        theme( panel.grid.major = element_line( ), 
               panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) + 
        theme( axis.title = element_text( size = lab_size * 15 ), 
               axis.text = element_text( size = lab_size * 15 * 0.8 ), 
               legend.text = element_text( size = lab_size * 15 * 0.5 ) ) + 
        theme( legend.position = "bottom", legend.title = element_blank( ) )
    )
  }
  
  return( results_all )
  
}

### Figure 6/ Table 4
reg_fun_all( dat = GUO_facility_firm, outcome = "adj_emissions_tot", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Total emissions", 
             SAVE_PLOT = TRUE,
             file_name = "Figure6", 
             lab_size = 1.6 )

### Figure 7/ Table 4
reg_fun_all( dat = GUO_facility_firm, outcome = "adj_emissions_tot_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Total emissions", 
             SAVE_PLOT = TRUE,
             file_name = "Figure7", 
             lab_size = 1.6 )

### Figure 11/ Table 4
reg_fun_all( dat = GUO_facility_firm, outcome = "OPRE", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Total emissions", 
             SAVE_PLOT = TRUE,
             file_name = "Figure11", 
             lab_size = 1.6 )

### Table 4: parent company level, full sample
reg_fun_all( dat = GUO_facility_firm, outcome = "adj_emissions_tot_OPRE", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Emissions intensity")

reg_fun_all( dat = GUO_facility_firm, outcome = "tfp_WRDG", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "TFP")

reg_fun_all( dat = GUO_facility_firm, outcome = "OPPL", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Operating profits")

reg_fun_all( dat = GUO_facility_firm, outcome = "EMPL", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Labor input")

reg_fun_all( dat = GUO_facility_firm, outcome = "TFAS", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Capital input")

reg_fun_all( dat = GUO_facility_firm, outcome = "STAF", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Labor expenditures")

reg_fun_all( dat = GUO_facility_firm, outcome = "IFAS", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Intangible fixed assets")

### Table 4: parent company level, only parent companies that do not close down their acquisition
reg_fun_all( dat = GUO_facility_firm_stay, outcome = "adj_emissions_tot", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Total emissions")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "OPRE", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Operating revenues")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "adj_emissions_tot_OPRE", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Emissions intensity")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "tfp_WRDG", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "TFP")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "OPPL", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Operating profits")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "EMPL", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Labor input")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "TFAS", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Capital input")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "STAF", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Labor expenditures")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "IFAS", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Intangible fixed assets")


### Table 4: average facility of parent company, only parent companies that do not close down their acquisition
reg_fun_all( dat = GUO_facility_firm_stay, outcome = "adj_emissions_tot_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average total emissions")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "OPRE_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average operating revenues")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "adj_emissions_tot_OPRE_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average emissions intensity")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "tfp_WRDG_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average TFP")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "OPPL_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average operating profits")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "EMPL_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average labor input")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "TFAS_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average capital input")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "STAF_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average labor expenditures")

reg_fun_all( dat = GUO_facility_firm_stay, outcome = "IFAS_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average intangible fixed assets")

### Table 4: average facility of parent company
reg_fun_all( dat = GUO_facility_firm, outcome = "adj_emissions_tot_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average total emissions")

reg_fun_all( dat = GUO_facility_firm, outcome = "OPRE_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average operating revenues")

reg_fun_all( dat = GUO_facility_firm, outcome = "adj_emissions_tot_OPRE_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average emissions intensity")

reg_fun_all( dat = GUO_facility_firm, outcome = "tfp_WRDG_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average TFP")

reg_fun_all( dat = GUO_facility_firm, outcome = "OPPL_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average operating profits")

reg_fun_all( dat = GUO_facility_firm, outcome = "EMPL_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average labor input")

reg_fun_all( dat = GUO_facility_firm, outcome = "TFAS_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average capital input")

reg_fun_all( dat = GUO_facility_firm, outcome = "STAF_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average labor expenditures")

reg_fun_all( dat = GUO_facility_firm, outcome = "IFAS_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average intangible fixed assets")




################### 4.  Main analysis: Appendix Figures #############################################

### Prepare function for estimation
reg_fun_all <- function( dat, outcome, cluster, IHS = FALSE, t_window = 7, yaxis = "Estimate and 95% Conf. Int", 
                         SAVE_PLOT = FALSE, lab_size = 1, file_name ){
  ### Prepare data
  dat_fin <- dat %>% 
    dplyr::mutate( time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat < -t_window, -t_window, time_to_treat ), 
                   time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat >= t_window, t_window-1, time_to_treat ) )
  
  dat_fin <- dat_fin %>% 
    dplyr::mutate( outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Year = ReportingYear, 
                   ParentCompany = GUO25C )
  
  if( IHS == TRUE ){
    dat_fin$outcome_var <- asinh( dat_fin$outcome_var )  
  }else{}
  
  
  ### TWFE results
  results_TWFE <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                   Year + ParentCompany, 
                                 data = dat_fin, cluster = c( "cluster_var" ) )
  
  ### Sun and Abraham (2021) results
  results_sunab <- fixest::feols( fml = outcome_var ~ sunab( event_year, time_to_treat, bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                    Year + ParentCompany, 
                                  data = dat_fin[ dat_fin$Year < 2016, ], cluster = c( "cluster_var" ) )  
  
  print( summary( results_sunab, agg = "att" ) )
  
  results_all <- tibble(  variable = c( rep( c( seq( from = -t_window, to = -2, by = 1 ), seq( from = 0, to = t_window-1, by = 1 ) ), 2 ), 
                                        rep( -1, 2 ) ),
                          estimate = c( results_TWFE$coeftable[ , 1 ], 
                                        results_sunab$coeftable[ , 1 ], 
                                        rep( 0, 2 ) ), 
                          se = c( results_TWFE$coeftable[ , 2 ], 
                                  results_sunab$coeftable[ , 2 ], 
                                  rep( 0, 2 ) ),
                          type = c( rep( "TWFE", 2 * t_window - 1 ), 
                                    rep( "SUNAB", 2 * t_window - 1 ),
                                    "TWFE", "SUNAB" ) ) %>%
    dplyr::mutate( conf_low = estimate - se * 1.96, 
                   conf_high = estimate + se * 1.96 )
  
  if( SAVE_PLOT == TRUE ){
    png( paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png" ), width = 600, height = 400 )
    print( 
      ggplot( data = results_all ) + 
        theme_classic( ) + 
        geom_point( mapping = aes( x = variable, y = estimate, color = type, group = type ), 
                    position = position_dodge( width = 0.75 ) ) + 
        geom_errorbar( mapping = aes( x = variable, ymin = conf_low, ymax = conf_high, color = type ), 
                       position = position_dodge( width = 0.75 ) ) + 
        labs( y = yaxis, x = "Time from ownership change" ) + 
        geom_vline( mapping = aes( xintercept = -0.5 ), linetype = "dashed" ) + 
        geom_hline( mapping = aes( yintercept = 0 ), linetype = "solid" ) + 
        scale_x_continuous( breaks = seq( -t_window, t_window -1, 1 ), labels = seq( -t_window, t_window -1, 1 ) ) + 
        theme( panel.grid.major = element_line( ), 
               panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) + 
        theme( axis.title = element_text( size = lab_size * 15 ), 
               axis.text = element_text( size = lab_size * 15 * 0.8 ), 
               legend.text = element_text( size = lab_size * 15 * 0.5 ) ) + 
        theme( legend.position = "bottom", legend.title = element_blank( ) )
    )
    dev.off( )
  }else{
    print( 
      ggplot( data = results_all ) + 
        theme_classic( ) + 
        geom_point( mapping = aes( x = variable, y = estimate, color = type, group = type ), 
                    position = position_dodge( width = 0.75 ) ) + 
        geom_errorbar( mapping = aes( x = variable, ymin = conf_low, ymax = conf_high, color = type ), 
                       position = position_dodge( width = 0.75 ) ) + 
        labs( y = yaxis, x = "Time from ownership change" ) + 
        geom_vline( mapping = aes( xintercept = -0.5 ), linetype = "dashed" ) + 
        geom_hline( mapping = aes( yintercept = 0 ), linetype = "solid" ) + 
        scale_x_continuous( breaks = seq( -t_window, t_window -1, 1 ), labels = seq( -t_window, t_window -1, 1 ) ) + 
        theme( panel.grid.major = element_line( ), 
               panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) + 
        theme( axis.title = element_text( size = lab_size * 15 ), 
               axis.text = element_text( size = lab_size * 15 * 0.8 ), 
               legend.text = element_text( size = lab_size * 15 * 0.5 ) ) + 
        theme( legend.position = "bottom", legend.title = element_blank( ) )
    )
  }
  
  return( results_all )
  
}

### Figure B11
reg_fun_all( dat = GUO_facility_firm_stay, outcome = "adj_emissions_tot", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Total emissions", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB11", 
             lab_size = 1.6 )

### Figure B12
reg_fun_all( dat = GUO_facility_firm_stay, outcome = "adj_emissions_tot_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average emissions", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB12", 
             lab_size = 1.6 )

### Figure B14
reg_fun_all( dat = GUO_facility_firm, outcome = "adj_emissions_tot_OPRE", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Emissions intensity", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB14", 
             lab_size = 1.6 )

### Figure B15
reg_fun_all( dat = GUO_facility_firm_stay, outcome = "adj_emissions_tot_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Emissions intensity", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB15", 
             lab_size = 1.6 )

### Figure B16
reg_fun_all( dat = GUO_facility_firm, outcome = "adj_emissions_tot_OPRE_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average emissions intensity", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB16", 
             lab_size = 1.6 )

### Figure B17
reg_fun_all( dat = GUO_facility_firm_stay, outcome = "adj_emissions_tot_OPRE_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average emissions intensity", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB17", 
             lab_size = 1.6 )

### Figure B24
reg_fun_all( dat = GUO_facility_firm, outcome = "TFAS", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Capital input", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB24", 
             lab_size = 1.6 )

### Figure B25
reg_fun_all( dat = GUO_facility_firm, outcome = "EMPL", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Labor input", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB25", 
             lab_size = 1.6 )

### Figure B26
reg_fun_all( dat = GUO_facility_firm, outcome = "STAF", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Labor expenditures", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB26", 
             lab_size = 1.6 )

### Figure B27
reg_fun_all( dat = GUO_facility_firm, outcome = "OPRE_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average operating revenues", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB27", 
             lab_size = 1.6 )

### Figure B28
reg_fun_all( dat = GUO_facility_firm, outcome = "TFAS_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average capital input", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB28", 
             lab_size = 1.6 )

### Figure B29
reg_fun_all( dat = GUO_facility_firm, outcome = "EMPL_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Employment per facility", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB29", 
             lab_size = 1.6 )

### Figure B30
reg_fun_all( dat = GUO_facility_firm, outcome = "STAF_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average labor expenses", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB30", 
             lab_size = 1.6 )

### Figure B41
reg_fun_all( dat = GUO_facility_firm, outcome = "tfp_WRDG", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "TFP", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB41", 
             lab_size = 1.6 )

### Figure B42
reg_fun_all( dat = GUO_facility_firm, outcome = "OPPL", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Operating profits", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB42", 
             lab_size = 1.6 )

### Figure B43
reg_fun_all( dat = GUO_facility_firm, outcome = "IFAS", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Intangible fixed assets", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB43", 
             lab_size = 1.6 )

### Figure B44
reg_fun_all( dat = GUO_facility_firm, outcome = "tfp_WRDG", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average TFP", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB44", 
             lab_size = 1.6 )

### Figure B45
reg_fun_all( dat = GUO_facility_firm, outcome = "OPPL_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average operating profits", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB45", 
             lab_size = 1.6 )

### Figure B46
reg_fun_all( dat = GUO_facility_firm, outcome = "IFAS_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average intangible fixed assets", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB46", 
             lab_size = 1.6 )

### Figure B50
reg_fun_all( dat = GUO_facility_firm, outcome = "OPRE", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Operating revenues", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB50", 
             lab_size = 1.6 )

### Figure B51
reg_fun_all( dat = GUO_facility_firm, outcome = "EMPL", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Labor input", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB51", 
             lab_size = 1.6 )
             
### Figure B52
reg_fun_all( dat = GUO_facility_firm, outcome = "TFAS", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Capital input", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB52", 
             lab_size = 1.6 )
             
### Figure B53
reg_fun_all( dat = GUO_facility_firm, outcome = "STAF", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Labor expenditures", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB53", 
             lab_size = 1.6 )
             
### Figure B54
reg_fun_all( dat = GUO_facility_firm, outcome = "tfp_WRDG", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "TFP", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB54", 
             lab_size = 1.6 )
             
### Figure B55
reg_fun_all( dat = GUO_facility_firm, outcome = "IFAS", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Intangible fixed assets", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB55", 
             lab_size = 1.6 )
             
### Figure B56
reg_fun_all( dat = GUO_facility_firm, outcome = "tfp_WRDG_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average TFP", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB56", 
             lab_size = 1.6 )
             
### Figure B57
reg_fun_all( dat = GUO_facility_firm, outcome = "TFAS_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average capital input", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB57", 
             lab_size = 1.6 )
             
### Figure B58
reg_fun_all( dat = GUO_facility_firm, outcome = "STAF_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average labor expenditures", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB58", 
             lab_size = 1.6 )
             
### Figure B59
reg_fun_all( dat = GUO_facility_firm, outcome = "IFAS_facility", 
             cluster = "GUO25C",
             IHS = TRUE,
             t_window = 4, 
             yaxis = "Average intangible fixed assets", 
             SAVE_PLOT = TRUE,
             file_name = "FigureB59", 
             lab_size = 1.6 )

