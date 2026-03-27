### Firms
### 02 Data analysis

rm( list = ls( ) )

### Load packages
install.packages("pacman")
pacman::p_load(dplyr, ggplot2, HonestDiD, fixest)

### Set working directory
setwd("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Data")

### Load firm data
load("firm_ownership_imp.Rdata")

################### 1. Prepare data for analysis #############################################

### Estimation of TFP
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

### Remove all firms with more or less than one ownership change in the observation period
firm_ownership_change <- firm_ownership_imp %>%
  dplyr::filter(oc_n == 1) %>%
  dplyr::mutate(exit = ifelse(last_reporting < 2016, 1, 0))
firm_ownership_exit <- firm_ownership_change %>%
  dplyr::filter(exit == 1) 
firm_ownership_stay <- firm_ownership_change %>%
  dplyr::filter(exit == 0)

################### 2.  Main analysis #############################################

### Generate function to run event study regressions
reg_data <- function( dat, outcome, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, 
                      ONLY_ONE = FALSE, t_window = 7, linear_trend = FALSE ){
  
  # Define outcome variable
  dat <- dat %>% 
    dplyr::mutate( Sector_Country = paste0( NACE_2, ":", Country_SUBBVDID ), 
                   outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Sector = NACE_2, 
                   Country = Country_SUBBVDID, 
                   Year = year )
  
  # Only maximum one ownership change
  if( ONLY_ONE == TRUE ){
    dat <- dat %>%
      dplyr::filter( oc_n <= 1 )
  }else{ }
  
  # Binning of begin and end points defined by t_window and set ownership change year to 3000 for never-treated facilities
  dat <- dat %>%
    dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -t_window , -t_window, time_to_treat ), 
                   time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= t_window, t_window - 1, time_to_treat ), 
                   ownership_change_year = ifelse( is.na( oc_year ), 3000, oc_year ), 
                   treat_trend = treat * time_to_treat )
  
  # Balanced sample for outcome variable
  if( BALANCED == TRUE ){
    dat <- dat %>%
      dplyr::filter( outcome_var > 0 ) %>% 
      dplyr::group_by( BVDID ) %>%
      dplyr::mutate( n = n( ) ) %>%
      dplyr::filter( n == 10 )          
  }else{}
  
  # Treatment only
  if( TREAT_ONLY == TRUE ){
    dat <- dat %>%
      dplyr::filter( treat == 1 )
  }else{}
  
  if( LOG_OUTCOME == TRUE ){
    dat$outcome_var <- log( dat$outcome_var )
  }else{}
  
  if( IHS_OUTCOME == TRUE ){
    dat$outcome_var <- log( ( dat$outcome_var ) + ( sqrt( dat$outcome_var )^2 + 1 ) ) 
  }else{}
  
  return( dat )
}


reg_fun_all <- function( dat, outcome, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, 
                         IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, ONLY_ONE = FALSE, 
                         t_window = 7, yaxis = "Estimate and 95% Conf. Int", SAVE_PLOT = FALSE, 
                         file_name, lab_size = 1 ){
  
  ### Prepare data
  dat <- reg_data( dat = dat, outcome = outcome, cluster = cluster, 
                   BALANCED = BALANCED, TREAT_ONLY = TREAT_ONLY, IHS_OUTCOME = IHS_OUTCOME, 
                   LOG_OUTCOME = LOG_OUTCOME, ONLY_ONE = ONLY_ONE, t_window = t_window )
  
  ### TWFE
  results_TWFE <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                   Country^Year + Sector^Year + BVDID, 
                                 data = dat, cluster = c( "cluster_var" ) )  
  ### Sun and Abraham
  results_sunab <- fixest::feols( fml = outcome_var ~ sunab( oc_year, time_to_treat, 
                                                             bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                    Country^Year + Sector^Year + BVDID, 
                                  data = dat[ dat$year < 2016, ], cluster = c( "cluster_var" ) ) 
  print( summary( results_sunab, agg = "att" ) )
  
  results_all <- tibble(  variable = c( rep( c( seq( from = -t_window, to = -2, by = 1 ), 
                                                seq( from = 0, to = t_window-1, by = 1 ) ), 2 ), 
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
    png( paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png"), width = 600, height = 400 )
    print( 
      ggplot( data = results_all ) + 
        theme_classic( ) + 
        geom_point( mapping = aes( x = variable, y = estimate, group = type, shape = type ), 
                    position = position_dodge( width = 0.75 ) ) + 
        geom_errorbar( mapping = aes( x = variable, ymin = conf_low, ymax = conf_high, linetype = type ), 
                       position = position_dodge( width = 0.75 ) ) + 
        labs( y = yaxis, x = "Time from ownership change" ) + 
        geom_vline( mapping = aes( xintercept = -0.5 )) + 
        geom_hline( mapping = aes( yintercept = 0 )) + 
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
}

###### Emissions

### Figure 4/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "adj_emissions_tot", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Total emissions", 
             SAVE_PLOT = TRUE, 
             file_name = "Figure4", 
             lab_size = 1.6 )

### Figure 5A/Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "adj_emissions_tot", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Total emissions", 
             SAVE_PLOT = TRUE, 
             file_name = "Figure5A", 
             lab_size = 1.6 )

### Figure 5B/Table 3
reg_fun_all( dat = firm_ownership_exit, 
             outcome = "adj_emissions_tot", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Total emissions", 
             SAVE_PLOT = TRUE, 
             file_name = "Figure5B", 
             lab_size = 1.6 )

###### Emissions intensity

### Figure 9/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "em_intensity", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Emissions intensity", 
             SAVE_PLOT = TRUE, 
             file_name = "Figure9", 
             lab_size = 1.6 )

###### Operating profits

### Figure 10/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "OPRE", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating revenues", 
             SAVE_PLOT = TRUE, 
             file_name = "Figure10", 
             lab_size = 1.6 )



################### 3. Main analysis: Appendix Figures #############################################


### Generate function to run event study regressions
reg_data <- function( dat, outcome, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, 
                      ONLY_ONE = FALSE, t_window = 7, linear_trend = FALSE ){
  
  # Define outcome variable
  dat <- dat %>% 
    dplyr::mutate( Sector_Country = paste0( NACE_2, ":", Country_SUBBVDID ), 
                   outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Sector = NACE_2, 
                   Country = Country_SUBBVDID, 
                   Year = year )
  
  # Only maximum one ownership change
  if( ONLY_ONE == TRUE ){
    dat <- dat %>%
      dplyr::filter( oc_n <= 1 )
  }else{ }
  
  # Binning of begin and end points defined by t_window and set ownership change year to 3000 for never-treated facilities
  dat <- dat %>%
    dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -t_window , -t_window, time_to_treat ), 
                   time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= t_window, t_window - 1, time_to_treat ), 
                   ownership_change_year = ifelse( is.na( oc_year ), 3000, oc_year ), 
                   treat_trend = treat * time_to_treat )
  
  # Balanced sample for outcome variable
  if( BALANCED == TRUE ){
    dat <- dat %>%
      dplyr::filter( outcome_var > 0 ) %>% 
      dplyr::group_by( BVDID ) %>%
      dplyr::mutate( n = n( ) ) %>%
      dplyr::filter( n == 10 )          
  }else{}
  
  # Treatment only
  if( TREAT_ONLY == TRUE ){
    dat <- dat %>%
      dplyr::filter( treat == 1 )
  }else{}
  
  if( LOG_OUTCOME == TRUE ){
    dat$outcome_var <- log( dat$outcome_var )
  }else{}
  
  if( IHS_OUTCOME == TRUE ){
    dat$outcome_var <- log( ( dat$outcome_var ) + ( sqrt( dat$outcome_var )^2 + 1 ) ) 
  }else{}
  
  return( dat )
}


reg_fun_all <- function( dat, outcome, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, 
                         IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, ONLY_ONE = FALSE, 
                         t_window = 7, yaxis = "Estimate and 95% Conf. Int", SAVE_PLOT = FALSE, 
                         file_name, lab_size = 1 ){
  
  ### Prepare data
  dat <- reg_data( dat = dat, outcome = outcome, cluster = cluster, 
                   BALANCED = BALANCED, TREAT_ONLY = TREAT_ONLY, IHS_OUTCOME = IHS_OUTCOME, 
                   LOG_OUTCOME = LOG_OUTCOME, ONLY_ONE = ONLY_ONE, t_window = t_window )
  
  ### TWFE
  results_TWFE <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                   Country^Year + Sector^Year + BVDID, 
                                 data = dat, cluster = c( "cluster_var" ) )  
  ### Sun and Abraham
  results_sunab <- fixest::feols( fml = outcome_var ~ sunab( oc_year, time_to_treat, 
                                                             bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                    Country^Year + Sector^Year + BVDID, 
                                  data = dat[ dat$year < 2016, ], cluster = c( "cluster_var" ) ) 
  print( summary( results_sunab, agg = "att" ) )
  
  results_all <- tibble(  variable = c( rep( c( seq( from = -t_window, to = -2, by = 1 ), 
                                                seq( from = 0, to = t_window-1, by = 1 ) ), 2 ), 
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
                                    "TWFE", "SUNAB" ) ) %>%
    dplyr::mutate( conf_low = estimate - se * 1.96, 
                   conf_high = estimate + se * 1.96 )
  
  
  if( SAVE_PLOT == TRUE ){
    png( paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png"), width = 600, height = 400 )
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
}



###### Operating revenues

### Figure B20A/Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "OPRE", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating revenues", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB20A", 
             lab_size = 1.6 )

### Figure B20B/Table 3
reg_fun_all( dat = firm_ownership_exit, 
             outcome = "OPRE", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating revenues", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB20B", 
             lab_size = 1.6 )



###### TFP

### Figure B35/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "tfp_WRDG", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "TFP", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB35", 
             lab_size = 1.6 )

### Figure B36A/Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "tfp_WRDG", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "TFP", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB36A", 
             lab_size = 1.6 )

### Figure B36B/Table 3
reg_fun_all( dat = firm_ownership_exit, 
             outcome = "tfp_WRDG", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "TFP", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB36B", 
             lab_size = 1.6 )



###### Employment

### Figure B21/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "EMPL", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Employment", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB21", 
             lab_size = 1.6 )

### Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "EMPL", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Employment", 
             SAVE_PLOT = FALSE )

### Table 3
reg_fun_all( dat = firm_ownership_exit, 
             outcome = "EMPL", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Employment", 
             SAVE_PLOT = FALSE)



###### Capital

### Figure B23/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "TFAS", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Capital", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB23", 
             lab_size = 1.6 )

### Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "TFAS", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Capital", 
             SAVE_PLOT = FALSE)

### Table 3
reg_fun_all( dat = firm_ownership_exit, 
             outcome = "TFAS", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Capital", 
             SAVE_PLOT = FALSE)



###### Intangible fixed assets

### Figure B39/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "IFAS", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Intangible fixed assets", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB39", 
             lab_size = 1.6 )

### Figure B40A/Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "IFAS", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Intangible fixed assets", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB40A", 
             lab_size = 1.6 )

### Figure B40B/Table 3
reg_fun_all( dat = firm_ownership_exit, 
             outcome = "IFAS", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Intangible fixed assets", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB40B", 
             lab_size = 1.6 )



###### Labor expenditures

### Figure B22/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "STAF", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Labor expenditures", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB22", 
             lab_size = 1.6 )

### Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "STAF", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Labor expenditures", 
             SAVE_PLOT = FALSE)

### Table 3
reg_fun_all( dat = firm_ownership_exit, 
             outcome = "STAF", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Labor expenditures", 
             SAVE_PLOT = FALSE)


###### Operating profits

### Figure B37/Table 3
reg_fun_all( dat = firm_ownership_change, 
             outcome = "OPPL", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating profits", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB37", 
             lab_size = 1.6 )

### Figure B38A/Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "OPPL", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating profits", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB38A", 
             lab_size = 1.6 )

### Figure B38B/Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "OPPL", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating profits", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB38B", 
             lab_size = 1.6 )


###### Emissions intensity

### Figure B13A/Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "em_intensity", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Emissions intensity", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB13A", 
             lab_size = 1.6 )

### Figure B13B/Table 3
reg_fun_all( dat = firm_ownership_stay, 
             outcome = "em_intensity", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Emissions intensity", 
             SAVE_PLOT = TRUE, 
             file_name = "FigureB13B", 
             lab_size = 1.6 )


################### 4. Heterogeneity analysis: By sector #############################################

### Generate function
reg_data <- function( dat, outcome, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, 
                      ONLY_ONE = FALSE, t_window = 7 ){
  
  # Define outcome variable
  dat <- dat %>% 
    dplyr::mutate( Sector_Country = paste0( NACE_2, ":", Country_SUBBVDID ), 
                   outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Sector = NACE_2, 
                   Country = Country_SUBBVDID, 
                   Year = year )
  
  # Only maximum one ownership change
  if( ONLY_ONE == TRUE ){
    dat <- dat %>%
      dplyr::filter( oc_n <= 1 )
  }else{ }
  
  # Binning of begin and end points defined by t_window and set ownership change year to 3000 for never-treated facilities
  dat <- dat %>%
    dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -t_window , -t_window, time_to_treat ), 
                   time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= t_window, t_window - 1, time_to_treat ), 
                   ownership_change_year = ifelse( is.na( oc_year ), 3000, oc_year ), 
                   treat_trend = treat * time_to_treat )
  
  # Balanced sample for outcome variable
  if( BALANCED == TRUE ){
    dat <- dat %>%
      dplyr::filter( outcome_var > 0 ) %>% 
      dplyr::group_by( BVDID ) %>%
      dplyr::mutate( n = n( ) ) %>%
      dplyr::filter( n == 10 )          
  }else{}
  
  # Treatment only
  if( TREAT_ONLY == TRUE ){
    dat <- dat %>%
      dplyr::filter( treat == 1 )
  }else{}
  
  if( LOG_OUTCOME == TRUE ){
    dat$outcome_var <- log( dat$outcome_var )
  }else{}
  
  if( IHS_OUTCOME == TRUE ){
    dat$outcome_var <- log( ( dat$outcome_var ) + ( sqrt( dat$outcome_var )^2 + 1 ) ) 
  }else{}
  
  return( dat )
}


reg_fun_indiv <- function( dat, outcome, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, 
                           IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, METHOD = "Naive", ONLY_ONE = FALSE, 
                           t_window = 7, yaxis = "Estimate and 95% Conf. Int", SAVE_PLOT = FALSE, 
                           file_name = outcome, lab_size = 1, sector_list = "all" ){
  
  dat <- reg_data( dat = dat, outcome = outcome, cluster = cluster, 
                   BALANCED = BALANCED, TREAT_ONLY = TREAT_ONLY, IHS_OUTCOME = IHS_OUTCOME, 
                   LOG_OUTCOME = LOG_OUTCOME, ONLY_ONE = ONLY_ONE, t_window = t_window )
  
  # Define split samples
  results_list <- vector( mode = "list", length = length( sector_list ) )
  
  for( i in seq_along( sector_list ) ){
    dat_new <- dat %>%
      dplyr::filter( Sector == sector_list[ i ] | treat == 0 )
    
    if( METHOD == "SUNAB" ){
      results_list[[ i ]] <- fixest::feols( fml = outcome_var ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                              Country^Year + Year + BVDID, 
                                            data = dat_new, cluster = c( "cluster_var" ) )  
    }else{
      results_list[[ i ]] <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                              Country^Year + Year + BVDID, 
                                            data = dat_new, cluster = c( "cluster_var" ) )
    }  
  }
  
  print( paste0( "Outcome: ", outcome, " and se clustered by: ", cluster ) )
  print( etable( results_list, headers = sector_list, depvar = FALSE, se.row = FALSE ) )
  print( lapply( results_list, function( x ){ summary( x, agg = "att" ) }  ) )
  
  if( SAVE_PLOT == TRUE){
    png( paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png" ), width = 600, height = 400 )
    par( cex.lab = lab_size, cex.axis = 0.8 * lab_size, cex = 0.8 * lab_size )
    print( 
      iplot( results_list, 
             xlab = "Time to ownership change", 
             ylab = yaxis, 
             main = "", 
             ref.line = -0.5,
             headers = sector_list ) )
    print( 
      legend( "topleft", 
              col = c( 1:length( sector_list ) ), lwd = 1, 
              legend = sector_list, 
              cex = 0.7 )
    )
    dev.off()
  }else{ 
    print( iplot( results_list, 
                  xlab = "Time to ownership change", 
                  ylab = yaxis, 
                  main = "", 
                  ref.line = -0.5, 
                  headers = sector_list ) 
    )
    print( 
      legend( "topleft", 
              col = c( 1:length( sector_list ) ), lwd = 1, 
              legend = sector_list, 
              cex = 0.7 )
    )
  }
  return( results_list )
}


### Figure B10
het_results_firm <- reg_fun_indiv( dat = firm_ownership_imp, 
                                   outcome = "adj_emissions_tot", 
                                   cluster = "BVDID",
                                   TREAT_ONLY = TRUE,
                                   IHS_OUTCOME = TRUE, 
                                   ONLY_ONE = TRUE, 
                                   t_window = 4, 
                                   yaxis = "Normalized emissions", 
                                   SAVE_PLOT = TRUE, 
                                   file_name = "FigureB10", 
                                   lab_size = 1.6, 
                                   METHOD = "SUNAB", 
                                   sector_list = c( names( table( firm_ownership_imp$NACE_2 )[ table( firm_ownership_imp$NACE_2 ) > 1000 ] ) ) )



### Figure B19

het_results_firm <- reg_fun_indiv( dat = firm_ownership_imp, 
                                   outcome = "OPRE", 
                                   cluster = "BVDID",
                                   TREAT_ONLY = TRUE,
                                   IHS_OUTCOME = TRUE, 
                                   ONLY_ONE = TRUE, 
                                   t_window = 4, 
                                   yaxis = "Normalized emissions", 
                                   SAVE_PLOT = TRUE, 
                                   file_name = "FigureB19", 
                                   lab_size = 1.6, 
                                   METHOD = "SUNAB", 
                                   sector_list = c( names( table( firm_ownership_imp$NACE_2 )[ table( firm_ownership_imp$NACE_2 ) > 1000 ] ) ) )


################### 5.  Robustness #############################################


########### Sample of 50% largest firms

### data preparation
firm_ownership_imp_2007 <- firm_ownership_imp %>%
  dplyr::filter(year == 2007)
summary(firm_ownership_imp_2007$OPRE)

# for the subsample of firms that report OPRE in 2007
firm_ownership_50 <- firm_ownership_imp %>%
  dplyr::ungroup() %>%
  dplyr::filter(oc_n == 1) %>%
  dplyr::group_by(BVDID) %>%
  dplyr::mutate(large_50 = ifelse(OPRE >= 110158980 & year == 2007, 1, 0)) %>%
  dplyr::mutate(large_50 = ifelse(!is.na(large_50), max(large_50), large_50)) %>%
  dplyr::filter(large_50 == 1)

### Table B5

reg_fun_all( dat = firm_ownership_50, 
             outcome = "adj_emissions_tot", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Total emissions", 
             SAVE_PLOT = FALSE)

### Emissions intensity
reg_fun_all( dat = firm_ownership_50, 
             outcome = "em_intensity", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Emissions intensity", 
             SAVE_PLOT = FALSE)

### Operating revenues
reg_fun_all( dat = firm_ownership_50, 
             outcome = "OPRE", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating revenues", 
             SAVE_PLOT = FALSE)

### Total factor productivity
reg_fun_all( dat = firm_ownership_50, 
             outcome = "tfp_WRDG", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "TFP", 
             SAVE_PLOT = FALSE)



### Operating expenditures
reg_fun_all( dat = firm_ownership_50, 
             outcome = "OPEXP", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating expenditures", 
             SAVE_PLOT = FALSE)


### Operating profits
reg_fun_all( dat = firm_ownership_50, 
             outcome = "OPPL", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Operating profits", 
             SAVE_PLOT = FALSE)


### Employment
reg_fun_all( dat = firm_ownership_50, 
             outcome = "EMPL", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Employees", 
             SAVE_PLOT = FALSE)

### Capital
reg_fun_all( dat = firm_ownership_50, 
             outcome = "TFAS", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Capital", 
             SAVE_PLOT = FALSE)



### Labor expenditures
reg_fun_all( dat = firm_ownership_50, 
             outcome = "STAF", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Labor expenditures", 
             SAVE_PLOT = FALSE)



### Intangible fixed assets
reg_fun_all( dat = firm_ownership_50, 
             outcome = "IFAS", 
             cluster = "BVDID",
             TREAT_ONLY = TRUE,
             IHS_OUTCOME = TRUE, 
             ONLY_ONE = TRUE, 
             t_window = 4, 
             yaxis = "Intangible fixed assets", 
             SAVE_PLOT = FALSE)



########### Imputation of emissions data

### Table B3

### Prepare data
firm_ownership_change_imp <- firm_ownership_change %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate(em_intensity_ihs = log( ( em_intensity ) + ( sqrt( em_intensity )^2 + 1 ) )) %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(ReportingYear < 2016)

firm_ownership_change_imp_0 <- firm_ownership_imp_0 %>%
  dplyr::filter(oc_n == 1) %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(ReportingYear < 2016)

firm_ownership_change_imp_threshold <- firm_ownership_imp_threshold %>%
  dplyr::filter(oc_n == 1) %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(ReportingYear < 2016)

### Table B3: column (1)
m1 <- feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
        BVDID + Country_SUBBVDID^ReportingYear + NACE_2^ReportingYear, firm_ownership_change_imp)

### Table B3: column (2)
m2 <- feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
        BVDID + Country_SUBBVDID^ReportingYear + NACE_2^ReportingYear, firm_ownership_change_imp_0)

### Table B3: column (3)
m3 <- feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
        BVDID + Country_SUBBVDID^ReportingYear + NACE_2^ReportingYear, firm_ownership_change_imp_threshold)

### Table B3: column (4)
m4 <- feols(em_intensity_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
        BVDID + Country_SUBBVDID^ReportingYear + NACE_2^ReportingYear, firm_ownership_change_imp)

etable(m1, m2, m3, m4)


########### Predictive power of firm characteristics for ownership change timing

### Table B1

column_1 <- fixest::feols(ownership_change_year ~ adj_emissions_tot + em_intensity + OPRE + TFAS + STAF + EMPL + OPPL + IFAS,
                         data = firm_ownership_change)

column_2 <- fixest::feols(ownership_change_year ~ adj_emissions_tot + em_intensity + OPRE + TFAS + STAF + EMPL + OPPL + IFAS |
                            Country_SUBBVDID^ReportingYear + NACE_2^ReportingYear, data = firm_ownership_change)

etable(column_1, column_2, tex = TRUE)


########### Trend adjustments

### Prepare data

firm_ownership_change <- firm_ownership_change %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate(em_intensity_ihs = log( ( em_intensity ) + ( sqrt( em_intensity )^2 + 1 ) )) %>%
  dplyr::mutate(OPRE_ihs = log( ( OPRE ) + ( sqrt( OPRE )^2 + 1 ) )) %>%
  dplyr::mutate(EMPL_ihs = log( ( EMPL ) + ( sqrt( EMPL )^2 + 1 ) )) %>%
  dplyr::mutate(STAF_ihs = log( ( STAF ) + ( sqrt( STAF )^2 + 1 ) )) %>%
  dplyr::mutate(TFAS_ihs = log( ( TFAS ) + ( sqrt( TFAS )^2 + 1 ) )) %>%
  dplyr::mutate(tfp_ihs = log( ( tfp_WRDG) + ( sqrt( tfp_WRDG )^2 + 1 ) )) %>%
  dplyr::mutate(OPPL_ihs = log( ( OPPL ) + ( sqrt( OPPL )^2 + 1 ) ))


firm_ownership_change <- firm_ownership_change %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( oc_year ), 3000, oc_year ), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(year < 2016) %>%
  dplyr::mutate(ownership_change_year = ifelse( is.na( oc_year ), 3000, oc_year ))

firm_ownership_exit <- firm_ownership_change %>%
  dplyr::filter( exit == 1)

firm_ownership_stay <- firm_ownership_change %>%
  dplyr::filter( exit == 0)

### Set up function to run SUn-Abraham Estimator with HonestDiD package

sunab_beta_vcv <-
  function(sunab_fixest){
    
    ## The following code block extracts the weights on individual coefs used in
    # the fixest aggregation ##
    sunab_agg   <- sunab_fixest$model_matrix_info$sunab$agg_period
    sunab_names <- names(sunab_fixest$coefficients)
    sunab_sel   <- grepl(sunab_agg, sunab_names, perl=TRUE)
    sunab_names <- sunab_names[sunab_sel]
    if(!is.null(sunab_fixest$weights)){
      sunab_wgt <- colSums(sunab_fixest$weights * sign(model.matrix(sunab_fixest)[, sunab_names, drop=FALSE]))
    } else {
      sunab_wgt <- colSums(sign(model.matrix(sunab_fixest)[, sunab_names, drop=FALSE]))
    }
    
    #Construct matrix sunab_trans such that sunab_trans %*% non-aggregated coefs = aggregated coefs,
    sunab_cohorts <- as.numeric(gsub(paste0(".*", sunab_agg, ".*"), "\\2", sunab_names, perl=TRUE))
    sunab_mat     <- model.matrix(~ 0 + factor(sunab_cohorts))
    sunab_trans   <- solve(t(sunab_mat) %*% (sunab_wgt * sunab_mat)) %*% t(sunab_wgt * sunab_mat)
    
    #Get the coefs and vcv
    sunab_coefs   <- sunab_trans %*% cbind(sunab_fixest$coefficients[sunab_sel])
    sunab_vcov    <- sunab_trans %*% sunab_fixest$cov.scaled[sunab_sel, sunab_sel] %*% t(sunab_trans)
    
    return(list(beta    = sunab_coefs,
                sigma   = sunab_vcov,
                cohorts = sort(unique(sunab_cohorts))))
  }


### Figure C3

figC3 <- feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                 BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_change)

beta_vcv <- sunab_beta_vcv(figC3)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC3 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                             original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC3.png" ), 
        plot = figC3,
        width = 6, height = 4 )


### Figure C4

figC4 <- feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                 BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_exit)

beta_vcv <- sunab_beta_vcv(figC4)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC4 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                             original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC4.png" ), 
        plot = figC4,
        width = 6, height = 4 )


### Figure C5

figC5 <- feols(OPRE_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                 BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_change)

beta_vcv <- sunab_beta_vcv(figC5)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC5 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                             original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC5.png" ), 
        plot = figC5,
        width = 6, height = 4 )


### Figure C6

figC6 <- feols(OPRE_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                 BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_stay)

beta_vcv <- sunab_beta_vcv(figC6)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC6 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                             original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC6.png" ), 
        plot = figC6,
        width = 6, height = 4 )

### Figure C7

figC7 <- feols(OPRE_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                 BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_exit)

beta_vcv <- sunab_beta_vcv(figC7)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC7 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                             original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC7.png" ), 
        plot = figC7,
        width = 6, height = 4 )

### Figure C8

figC8 <- feols(EMPL_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                 BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_change)

beta_vcv <- sunab_beta_vcv(figC8)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC8 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                             original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC8.png" ), 
        plot = figC8,
        width = 6, height = 4 )


### Figure C9

figC9 <- feols(STAF_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                 BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_change)

beta_vcv <- sunab_beta_vcv(figC9)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC9 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                             original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC9.png" ), 
        plot = figC9,
        width = 6, height = 4 )

### Figure C10

figC10 <- feols(TFAS_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                  BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_change)

beta_vcv <- sunab_beta_vcv(figC10)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC10 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                              original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC10.png" ), 
        plot = figC10,
        width = 6, height = 4 )

### Figure C11

figC11 <- feols(OPPL_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                  BVDID + year^Country_SUBBVDID + NACE_2^year, firm_ownership_change)

beta_vcv <- sunab_beta_vcv(figC11)

kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1),
               l_vec = basisVector(4,4)
)
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

figC11 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                              original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% conf. int.", x = "Smoothness restriction M" ) +
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) )

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC11.png" ), 
        plot = figC11,
        width = 6, height = 4 )
