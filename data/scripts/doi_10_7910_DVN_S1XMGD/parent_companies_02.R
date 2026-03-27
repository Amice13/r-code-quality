### Parent companies
### 02 Data preparation and analysis for other facilities of acquiring parent companies

rm( list = ls( ) )

### Load packages
install.packages("pacman")
pacman::p_load(dplyr, ggplot2)

### Set working directory
setwd("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Data")

### Load facility, firm and GUO data
load("facility_ownership_vge.Rdata")
load("firm_ownership_imp.Rdata")


############# 1. Prepare data by aggregating facilities at GUO level and merging with firm characteristics #################

# Add TFP to firm data for later merge
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


############# 2. Find other facilities of acquiring parent company #################

eprtr_facility_pollution_other_facility <-  eprtr_facility_pollution %>%
  dplyr::select( FacilityID, ReportingYear, GUO25C, adj_emissions_tot ) %>%
  dplyr::left_join( GUO_facility_firm[ , c("GUO25C", "ReportingYear", "time_to_treat", "n_oc", "event_year" )], 
                    by = c("GUO25C", "ReportingYear" ) ) %>%
  dplyr::group_by( FacilityID ) %>%
  dplyr::filter( min( time_to_treat, na.rm = TRUE ) < 0 & n_oc == 1  ) %>%
  dplyr::mutate( treat = 1 )

t_window <- 4

eprtr_facility_pollution_other_facility_fin <- eprtr_facility_pollution_other_facility %>% 
  dplyr::mutate( time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat < -t_window, -t_window, time_to_treat ), 
                 time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat >= t_window, t_window-1, time_to_treat ) )


############# 3. Main analysis #################

### Prepare function for event study
reg_fun_all_firm <- function( dat, outcome, cluster, IHS = FALSE, t_window = 7, yaxis = "Estimate and 95% Conf. Int", 
                              SAVE_PLOT = FALSE, lab_size = 1, file_name ){
  ### Prepare data
  dat_fin <- dat %>% 
    dplyr::mutate( time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat < -t_window, -t_window, time_to_treat ), 
                   time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat >= t_window, t_window-1, time_to_treat ) )
  
  dat_fin <- dat_fin %>% 
    dplyr::mutate( outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Year = ReportingYear, 
                   ParentCompany = GUO25C, 
                   Sector = NACE_2, 
                   Country = Country_SUBBVDID )
  
  if( IHS == TRUE ){
    dat_fin$outcome_var <- asinh( dat_fin$outcome_var )  
  }else{}
  
  
  ### TWFE results
  results_TWFE <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                   Sector^Year + Country^Year + BVDID, 
                                 data = dat_fin, cluster = c( "cluster_var" ) )
  
  ### Sun and Abraham (2021) results
  results_sunab <- fixest::feols( fml = outcome_var ~ sunab( event_year, time_to_treat, bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                    Sector^Year + Country^Year + BVDID,  
                                  data = dat_fin[ dat_fin$Year < 2016, ], cluster = c( "cluster_var" ) )  
  
  print( summary( results_sunab, agg = "att" ) )
  
  results_all <- tibble(  variable = c( rep( c( seq( from = -t_window, to = -2, by = 1 ), seq( from = 0, to = t_window-1, by = 1 ) ), 2 ), 
                                        rep( -1, 2 ) ),
                          estimate = c( results_TWFE$coeftable[ , 1 ], 
                                        results_sunab$coeftable[ , 1 ], 
                                        #results_cs$att.egt[ !is.na( results_cs$se.egt ) ], 
                                        rep( 0, 2 ) ), 
                          se = c( results_TWFE$coeftable[ , 2 ], 
                                  results_sunab$coeftable[ , 2 ], 
                                  #results_cs$se.egt[ !is.na( results_cs$se.egt ) ], 
                                  rep( 0, 2 ) ),
                          type = c( rep( "TWFE", 2 * t_window - 1 ), 
                                    rep( "SUNAB", 2 * t_window - 1 ),
                                    #rep( "CS", 2 * t_window - 1 ), 
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

firm_ownership_imp_other_facility <- firm_ownership_imp %>%
  dplyr::ungroup( ) %>%
  dplyr::select( BVDID, ReportingYear, GUO25C, Country_SUBBVDID, NACE_2, adj_emissions_tot, OPRE, EMPL, IFAS, 
                 RD, TFAS, OPPL, tfp_WRDG, OPEXP, PLAT, STAF, INVEST ) %>%
  dplyr::left_join( GUO_facility_firm[ , c("GUO25C", "ReportingYear", "time_to_treat", "n_oc", "event_year" )], 
                    by = c("GUO25C", "ReportingYear" ) ) %>%
  dplyr::group_by( BVDID ) %>%
  dplyr::filter( min( time_to_treat, na.rm = TRUE ) < 0 & n_oc == 1  ) %>%
  dplyr::mutate( treat = 1, 
                 em_intensity = adj_emissions_tot / OPRE * 100000 ) %>%
  dplyr::ungroup( )



### Figure 8
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "adj_emissions_tot", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Total emissions", 
                  SAVE_PLOT = TRUE, 
                  file_name = "Figure8",
                  lab_size = 1 )

### Table 5

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "adj_emissions_tot", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "OPRE", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "em_intensity", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "tfp_WRDG", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "OPPL", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "EMPL", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "TFAS", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "STAF", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)

reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "IFAS", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4)





############# 4. Appendix Figures #################

### Prepare function for event study
reg_fun_all_firm <- function( dat, outcome, cluster, IHS = FALSE, t_window = 7, yaxis = "Estimate and 95% Conf. Int", 
                              SAVE_PLOT = FALSE, lab_size = 1, file_name ){
  ### Prepare data
  dat_fin <- dat %>% 
    dplyr::mutate( time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat < -t_window, -t_window, time_to_treat ), 
                   time_to_treat = ifelse(  time_to_treat > -999 & time_to_treat >= t_window, t_window-1, time_to_treat ) )
  
  dat_fin <- dat_fin %>% 
    dplyr::mutate( outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Year = ReportingYear, 
                   ParentCompany = GUO25C, 
                   Sector = NACE_2, 
                   Country = Country_SUBBVDID )
  
  if( IHS == TRUE ){
    dat_fin$outcome_var <- asinh( dat_fin$outcome_var )  
  }else{}
  
  
  ### TWFE results
  results_TWFE <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                   Sector^Year + Country^Year + BVDID, 
                                 data = dat_fin, cluster = c( "cluster_var" ) )
  
  ### Sun and Abraham (2021) results
  results_sunab <- fixest::feols( fml = outcome_var ~ sunab( event_year, time_to_treat, bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                    Sector^Year + Country^Year + BVDID,  
                                  data = dat_fin[ dat_fin$Year < 2016, ], cluster = c( "cluster_var" ) )  
  
  print( summary( results_sunab, agg = "att" ) )
  
  results_all <- tibble(  variable = c( rep( c( seq( from = -t_window, to = -2, by = 1 ), seq( from = 0, to = t_window-1, by = 1 ) ), 2 ), 
                                        rep( -1, 2 ) ),
                          estimate = c( results_TWFE$coeftable[ , 1 ], 
                                        results_sunab$coeftable[ , 1 ], 
                                        #results_cs$att.egt[ !is.na( results_cs$se.egt ) ], 
                                        rep( 0, 2 ) ), 
                          se = c( results_TWFE$coeftable[ , 2 ], 
                                  results_sunab$coeftable[ , 2 ], 
                                  #results_cs$se.egt[ !is.na( results_cs$se.egt ) ], 
                                  rep( 0, 2 ) ),
                          type = c( rep( "TWFE", 2 * t_window - 1 ), 
                                    rep( "SUNAB", 2 * t_window - 1 ),
                                    #rep( "CS", 2 * t_window - 1 ), 
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

### Figure B18
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "em_intensity", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Emissions intensity", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB18",
                  lab_size = 1 )

### Figure B31
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "OPRE", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Operating revenues", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB31",
                  lab_size = 1 )

### Figure B32
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "TFAS", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Capital input", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB32",
                  lab_size = 1 )

### Figure B33
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "EMPL", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Labor input", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB33",
                  lab_size = 1 )

### Figure B34
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "STAF", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Labor expenditures", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB34",
                  lab_size = 1 )

### Figure B47
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "tfp_WRDG", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "TFP", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB47",
                  lab_size = 1 )

### Figure B48
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "OPPL", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Operating profits", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB48",
                  lab_size = 1 )

### Figure B49
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "IFAS", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Intangible fixed assets", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB49",
                  lab_size = 1 )

### Figure B60
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "tfp_WRDG", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "TFP", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB60",
                  lab_size = 1 )

### Figure B61
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "OPPL", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Operating profits", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB61",
                  lab_size = 1 )

### Figure B62
reg_fun_all_firm( dat = firm_ownership_imp_other_facility, 
                  outcome = "IFAS", 
                  cluster = "BVDID", 
                  IHS = TRUE, 
                  t_window = 4,
                  yaxis = "Intangible fixed assets", 
                  SAVE_PLOT = TRUE,
                  file_name = "FigureB62",
                  lab_size = 1 )
