### Facilities
### 02 Data analysis

rm( list = ls( ) )

### Load packages
install.packages("pacman")
pacman::p_load(dplyr, ggplot2, HonestDiD, fixest)

### Set working directory
setwd("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Data")

### Load facility and firm data
load("facility_ownership_vge.Rdata")

### Remove all facilities with more or less than one ownership change in the observation period
facility_ownership_change <- eprtr_facility_pollution %>%
  dplyr::filter(Num_changes_GUO25C == 1)

### Create a dataset with only facilities that exit after ownership
facility_ownership_exit <- facility_ownership_change %>%
  dplyr::filter(last_report_facility < 2016) %>%
  dplyr::mutate(exit_years = last_report_facility - ownership_change_year)

### Create a dataset with only facilities that remain in operation after ownership change
facility_ownership_stay <- facility_ownership_change %>%
  dplyr::filter(last_report_facility == 2016)



################### Main analysis #############################################

### Generate function to run event study regressions
reg_data <- function( dat, outcome, scale = NULL, thresh = 1, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, 
                      ONLY_ONE = FALSE, t_window = 7 ){
  # Define outcome variable
  
  if( is.null( scale ) ){
    dat <- dat %>% 
      dplyr::mutate( outcome_var = !!sym( outcome ), 
                     cluster_var = !!sym( cluster ), 
                     Year = ReportingYear, 
                     Facility = FacilityID, 
                     Sector = NACE_2, 
                     Country = CountryCode )
    
  }else{
    dat <- dat %>% 
      dplyr::mutate( outcome_var = ifelse( !!sym( outcome ) == 0 & !!sym( scale ) == 0, 0, !!sym( outcome ) / ( !!sym( scale ) / thresh ) ), 
                     cluster_var = !!sym( cluster ), 
                     Year = ReportingYear, 
                     Facility = FacilityID, 
                     Sector = NACE_2, 
                     #Sector = NACE_1, 
                     Country = CountryCode )
  }
  
  
  # Only maximum one ownership change
  if( ONLY_ONE == TRUE ){
    dat <- dat %>%
      dplyr::filter( Num_changes_GUO25C <= 1 )
  }else{ }
  
  # Binning of begin and end points defined by t_window and set ownership change year to 3000 for never-treated facilities
  dat <- dat %>%
    dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -t_window , -t_window, time_to_treat ), 
                   time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= t_window, t_window - 1, time_to_treat ), 
                   ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year ))  
  
  
  # Balanced sample for outcome variable
  if( BALANCED == TRUE ){
    dat <- dat %>%
      dplyr::filter( outcome_var > 0 ) %>% 
      dplyr::group_by( FacilityID ) %>%
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

reg_fun_all <- function(dat, outcome, scale = NULL, thresh = 1, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, 
                        IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, ONLY_ONE = FALSE, t_window = 7, 
                        yaxis = "Estimate and 95% Conf. Int", SAVE_PLOT = FALSE, file_name = outcome, lab_size = 1) {
  
  # Prepare data
  dat <- reg_data(dat = dat, outcome = outcome, scale = scale, thresh = thresh, cluster = cluster, 
                  BALANCED = BALANCED, TREAT_ONLY = TREAT_ONLY, IHS_OUTCOME = IHS_OUTCOME, 
                  LOG_OUTCOME = LOG_OUTCOME, ONLY_ONE = ONLY_ONE, t_window = t_window)
  
  # TWFE and Sun and Abraham results as defined in the original code
  results_TWFE <- fixest::feols(fml = outcome_var ~ i(time_to_treat, treat, ref = c(-1, -1000)) | 
                                  Country^Year + Sector^Year + Facility, data = dat, cluster = c("cluster_var"))
  
  results_sunab <- fixest::feols(fml = outcome_var ~ sunab(ownership_change_year, time_to_treat, 
                                                           bin.rel = c(-t_window:-9, t_window:9)) | 
                                   Country^Year + Sector^Year + FacilityID, data = dat[dat$Year < 2016, ], 
                                 cluster = c("cluster_var"))
  
  print(summary(results_sunab, agg = "ATT"))
  
  results_all <- tibble(variable = c(rep(c(seq(from = -t_window, to = -2, by = 1), seq(from = 0, to = t_window-1, by = 1)), 2), rep(-1, 2)),
                        estimate = c(results_TWFE$coeftable[, 1], results_sunab$coeftable[, 1], rep(0, 2)),
                        se = c(results_TWFE$coeftable[, 2], results_sunab$coeftable[, 2], rep(0, 2)),
                        type = c(rep("TWFE", 2 * t_window - 1), rep("SUNAB", 2 * t_window - 1), "TWFE", "SUNAB")) %>%
    dplyr::mutate(conf_low = estimate - se * 1.96, conf_high = estimate + se * 1.96)
  
  plot <- ggplot(data = results_all) + 
    theme_classic() + 
    geom_point(mapping = aes(x = variable, y = estimate, shape = type, group = type), 
               position = position_dodge(width = 0.75)) + 
    geom_errorbar(mapping = aes(x = variable, ymin = conf_low, ymax = conf_high, linetype = type), 
                  position = position_dodge(width = 0.75)) + 
    labs(y = yaxis, x = "Time from ownership change") + 
    geom_vline(mapping = aes(xintercept = -0.5), linetype = "dashed") + 
    geom_hline(mapping = aes(yintercept = 0), linetype = "solid") + 
    scale_x_continuous(breaks = seq(-t_window, t_window - 1, 1), labels = seq(-t_window, t_window - 1, 1)) + 
    scale_shape_manual(values = c(16, 17)) +
    scale_linetype_manual(values = c("solid", "dashed")) + 
    theme(panel.grid.major = element_line(), 
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.title = element_text(size = lab_size * 15), 
          axis.text = element_text(size = lab_size * 15 * 0.8), 
          legend.text = element_text(size = lab_size * 15 * 0.5),
          legend.position = "bottom", legend.title = element_blank())
  
  # Save or print the plot
  if (SAVE_PLOT == TRUE) {
    png(paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png"), width = 600, height = 400)
    print(plot)
    dev.off()
  } else {
    print(plot)
  }
  
  return(results_all)
}


### Figure 2/Table 2
reg_fun_all( dat = facility_ownership_change, 
                     outcome = "adj_emissions_tot", 
                     cluster = "FacilityID", 
                     ONLY_ONE = TRUE, 
                     TREAT_ONLY = TRUE, 
                     IHS_OUTCOME = TRUE, 
                     t_window = 4, 
                     SAVE_PLOT = TRUE,
                     yaxis = "Total emissions", 
                     file_name = "Figure2", 
                     lab_size = 1.6 )


### Figure 3a/Table 2
fig3a <- reg_fun_all( dat = facility_ownership_stay, 
                     outcome = "adj_emissions_tot", 
                     cluster = "FacilityID", 
                     ONLY_ONE = TRUE, 
                     TREAT_ONLY = TRUE, 
                     IHS_OUTCOME = TRUE, 
                     t_window = 4, 
                     SAVE_PLOT = TRUE,
                     yaxis = "Total emissions", 
                     file_name = "Figure3a", 
                     lab_size = 1.6 )


### Figure 3b/Table 2
fig3b <- reg_fun_all( dat = facility_ownership_exit, 
                     outcome = "adj_emissions_tot", 
                     cluster = "FacilityID", 
                     ONLY_ONE = TRUE, 
                     TREAT_ONLY = TRUE, 
                     IHS_OUTCOME = TRUE, 
                     t_window = 4,
                     SAVE_PLOT = TRUE,
                     yaxis = "Total emissions", 
                     file_name = "Figure3b", 
                     lab_size = 1.6 )


################### Heterogeneity analysis: Type of ownership #############################################

### Generate function for regressions on different ownership changes split sample
reg_fun_split <- function( dat, outcome, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, METHOD = "Naive", ONLY_ONE = FALSE, 
                           t_window = 7, SAVE_PLOT = FALSE, file_name, lab_size = 1, yaxis, 
                           own_type_list = "all" ){
  # Define outcome variable
  dat <- dat %>% 
    dplyr::mutate( outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Year = ReportingYear, 
                   Facility = FacilityID, 
                   Sector = NACE_2, 
                   #Sector = NACE_1, 
                   Country = CountryCode )
  
  # Only one ownership change
  if( ONLY_ONE == TRUE ){
    dat <- dat %>%
      dplyr::filter( Num_changes_GUO25C == 1 )
  }else{ }
  
  # Binning of begin and end points defined by t_window and set ownership change year to 3000 for never-treated facilities
  dat <- dat %>%
    dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -t_window , -t_window, time_to_treat ), 
                   time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= t_window, t_window - 1, time_to_treat ), 
                   ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year ))
  
  # Balanced sample for outcome variable
  if( BALANCED == TRUE ){
    dat <- dat %>%
      dplyr::filter( outcome_var > 0 ) %>% 
      dplyr::group_by( FacilityID ) %>%
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
  
  # Define split samples
  if( length( own_type_list ) > 1 ){
    results_list <- vector( mode = "list", length = length( own_type_list ) )
    
    for( i in seq_along( own_type_list ) ){
      dat_new <- dat %>%
        dplyr::filter( own_type == own_type_list[ i ] | treat == 0 )
      
      if( METHOD == "Staggered" ){
        # Either time_to_treat or ReportingYear can be used, but then begin and end points cannot be binned
        results_list[[ i ]] <- fixest::feols( fml = outcome_var ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                                Country^Year + Sector^Year + Facility, 
                                              data = dat_new[ dat_new$Year < 2016, ], cluster = c( "cluster_var" ) )  
      }else{
        results_list[[ i ]] <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                                Country^Year + Sector^Year + Facility, 
                                              data = dat_new, cluster = c( "cluster_var" ) )
      }  
    }
    
    print( paste0( "Outcome: ", outcome, " and se clustered by: ", cluster ) )
    print( etable( results_list, headers = own_type_list, depvar = FALSE, se.row = FALSE ) )
    print( lapply( results_list, function( x ){ summary( x, agg = "att" ) }  ) )
    
    
    if( SAVE_PLOT == TRUE){
      png( paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png" ), width = 600, height = 400 )
      par( cex.lab = lab_size, cex.axis = 0.8 * lab_size, cex = 0.8 * lab_size )
      print( 
        iplot( results_list, 
               xlab = "Time to ownership change", 
               ylab = yaxis, 
               main = "", 
               #main = paste0( "Effect on ", outcome, "; clustered by ", cluster ), 
               ref.line = -0.5,
               headers = own_type_list ) )
      print( legend( "bottomleft", 
                     col = c( 1:length( own_type_list ) ), lwd = 1, 
                     #pch = c( 1:length( own_type_list ) ), 
                     legend = own_type_list, 
                     cex = 0.7 ) 
      )
      dev.off()
    }else{ 
      iplot( results_list, 
             xlab = "Time to ownership change", 
             ylab = yaxis, 
             main = "", 
             #main = paste0( "Effect on ", outcome, "; clustered by ", cluster ), 
             ref.line = -0.5, 
             headers = own_type_list )  
      legend( "topleft", 
              col = c( 1:length( own_type_list ) ), lwd = 1, 
              #pch = c( 1:length( own_type_list ) ), 
              legend = own_type_list, 
              cex = 0.7 )
      
    }
  }else{
    if( METHOD == "Staggered" ){
      # Either time_to_treat or ReportingYear can be used, but then begin and end points cannot be binned
      results <- fixest::feols( fml = outcome_var ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -t_window:-9, ( t_window + 1 ):9 ) ) | 
                                  Country^Year + Sector^Year + Facility, 
                                data = dat_new[ dat_new$Year < 2016, ], cluster = c( "cluster_var" ) )  
    }else{
      results <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | Country^Year + Sector^Year + Facility, 
                                data = dat_new, cluster = c( "cluster_var" ) )
    }
    
    print( paste0( "Outcome: ", outcome, " and se clustered by: ", cluster ) )
    print( etable( results, headers = own_type, depvar = FALSE, se.row = FALSE ) )
    
    if( SAVE_PLOT == TRUE ){
      png( paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png" ), width = 600, height = 400 )
      par( cex.lab = lab_size, cex.axis = 0.8 * lab_size, cex = 0.8 * lab_size )
      print( 
        iplot( results, 
               xlab = "Time to ownership change", 
               ylab = yaxis, 
               main = "", 
               #main = paste0( "Effect on ", outcome, "; clustered by ", cluster ), 
               ref.line = -0.5,
               headers = own_type_list ) ) 
      dev.off()
    }else{ 
      iplot( results, 
             xlab = "Time to ownership change", 
             ylab = yaxis, 
             main = "", 
             #main = paste0( "Effect on ", outcome, "; clustered by ", cluster ), 
             ref.line = -0.5, 
             headers = own_type_list )  
      
    }
  }
}

reg_fun_split( dat = facility_ownership_change, 
               outcome = "adj_emissions_tot", 
               cluster = "FacilityID", 
               own_type_list = c( "DOM_DOM", "DOM_FOR", "FOR_DOM", "FOR_FOR" ), 
               ONLY_ONE = TRUE, 
               TREAT_ONLY = TRUE, 
               IHS_OUTCOME = TRUE, 
               lab_size = 1.5, 
               SAVE_PLOT = TRUE, 
               yaxis = "Total emissions",
               file_name = "FigureB7",
               t_window = 4, 
               METHOD = "Staggered" )



################### Heterogeneity analysis: By sector #############################################

### Generate function for regressions on different ownership changes split sample
reg_fun_split <- function( dat, outcome, cluster, BALANCED = FALSE, TREAT_ONLY = FALSE, IHS_OUTCOME = FALSE, LOG_OUTCOME = FALSE, METHOD = "Naive", ONLY_ONE = FALSE, 
                           t_window = 7, SAVE_PLOT = FALSE, file_name, lab_size = 1, yaxis, 
                           sector_list = "all" ){
  # Define outcome variable
  dat <- dat %>% 
    dplyr::mutate( outcome_var = !!sym( outcome ), 
                   cluster_var = !!sym( cluster ), 
                   Year = ReportingYear, 
                   Facility = FacilityID, 
                   Sector = NACE_2, 
                   #Sector = NACE_1, 
                   Country = CountryCode )
  
  # Only one ownership change
  if( ONLY_ONE == TRUE ){
    dat <- dat %>%
      dplyr::filter( Num_changes_GUO25C == 1 )
  }else{ }
  
  # Binning of begin and end points defined by t_window and set ownership change year to 3000 for never-treated facilities
  dat <- dat %>%
    dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -t_window , -t_window, time_to_treat ), 
                   time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= t_window, t_window - 1, time_to_treat ), 
                   ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year ))
  
  # Balanced sample for outcome variable
  if( BALANCED == TRUE ){
    dat <- dat %>%
      dplyr::filter( outcome_var > 0 ) %>% 
      dplyr::group_by( FacilityID ) %>%
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
  
  # Define split samples
  if( length( sector_list ) > 1 ){
    results_list <- vector( mode = "list", length = length( sector_list ) )
    
    for( i in seq_along( sector_list ) ){
      dat_new <- dat %>%
        dplyr::filter( Sector == sector_list[ i ] | treat == 0 )
      
      if( METHOD == "Staggered" ){
        # Either time_to_treat or ReportingYear can be used, but then begin and end points cannot be binned
        results_list[[ i ]] <- fixest::feols( fml = outcome_var ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -t_window:-9, t_window:9 ) ) | 
                                                Country^Year + Sector^Year + Facility, 
                                              data = dat_new[ dat_new$Year < 2016, ], cluster = c( "cluster_var" ) )  
      }else{
        results_list[[ i ]] <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | 
                                                Country^Year + Sector^Year + Facility, 
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
               #main = paste0( "Effect on ", outcome, "; clustered by ", cluster ), 
               ref.line = -0.5,
               headers = sector_list ) )
      print( legend( "bottomleft", 
                     col = c( 1:length( sector_list ) ), lwd = 1, 
                     #pch = c( 1:length( sector_list ) ), 
                     legend = sector_list, 
                     cex = 0.7 ) 
      )
      dev.off()
    }else{ 
      iplot( results_list, 
             xlab = "Time to ownership change", 
             ylab = yaxis, 
             main = "", 
             #main = paste0( "Effect on ", outcome, "; clustered by ", cluster ), 
             ref.line = -0.5, 
             headers = sector_list )  
      legend( "topleft", 
              col = c( 1:length( sector_list ) ), lwd = 1, 
              #pch = c( 1:length( sector_list ) ), 
              legend = sector_list, 
              cex = 0.7 )
      
    }
  }else{
    if( METHOD == "Staggered" ){
      # Either time_to_treat or ReportingYear can be used, but then begin and end points cannot be binned
      results <- fixest::feols( fml = outcome_var ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -t_window:-9, ( t_window + 1 ):9 ) ) | 
                                  Country^Year + Sector^Year + Facility, 
                                data = dat_new[ dat_new$Year < 2016, ], cluster = c( "cluster_var" ) )  
    }else{
      results <- fixest::feols( fml = outcome_var ~ i( time_to_treat, treat, ref = c( -1, -1000 ) ) | Country^Year + Sector^Year + Facility, 
                                data = dat_new, cluster = c( "cluster_var" ) )
    }
    
    print( paste0( "Outcome: ", outcome, " and se clustered by: ", cluster ) )
    print( etable( results, headers = own_type, depvar = FALSE, se.row = FALSE ) )
    
    if( SAVE_PLOT == TRUE ){
      png( paste0( "I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", file_name, ".png" ), width = 600, height = 400 )
      par( cex.lab = lab_size, cex.axis = 0.8 * lab_size, cex = 0.8 * lab_size )
      print( 
        iplot( results, 
               xlab = "Time to ownership change", 
               ylab = yaxis, 
               main = "", 
               #main = paste0( "Effect on ", outcome, "; clustered by ", cluster ), 
               ref.line = -0.5,
               headers = sector_list ) ) 
      dev.off()
    }else{ 
      iplot( results, 
             xlab = "Time to ownership change", 
             ylab = yaxis, 
             main = "", 
             #main = paste0( "Effect on ", outcome, "; clustered by ", cluster ), 
             ref.line = -0.5, 
             headers = sector_list )  
      
    }
  }
}

reg_fun_split( dat = eprtr_facility_pollution, 
               outcome = "adj_emissions_tot", 
               cluster = "FacilityID", 
               sector_list = names( table( eprtr_facility_pollution$NACE_2 )[ table( eprtr_facility_pollution$NACE_2 ) > 1000 ] ), 
               ONLY_ONE = TRUE, 
               TREAT_ONLY = TRUE, 
               IHS_OUTCOME = TRUE, 
               lab_size = 1.6, 
               SAVE_PLOT = TRUE, 
               file_name = "FigureB8",
               yaxis = "Normalized emissions",
               t_window = 4, 
               METHOD = "Staggered" )


################### Robustness #############################################

########### Imputation of emissions data

### Prepare data
facility_ownership_change <- facility_ownership_change %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(ReportingYear < 2016)
facility_ownership_change_impute_0 <- eprtr_facility_pollution_impute_0 %>%
  dplyr::filter(Num_changes_GUO25C == 1) %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(ReportingYear < 2016)
facility_ownership_change_impute_threshold <- eprtr_facility_pollution_impute_threshold %>%
  dplyr::filter(Num_changes_GUO25C == 1) %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(ReportingYear < 2016)

### Table B2: column (1)
feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
        FacilityID + ReportingYear^CountryCode + NACE_2^ReportingYear, facility_ownership_change)

### Table B2: column (2)
feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
        FacilityID + ReportingYear^CountryCode + NACE_2^ReportingYear, facility_ownership_change_impute_0)

### Table B2: column (3)
feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
        FacilityID + ReportingYear^CountryCode + NACE_2^ReportingYear, facility_ownership_change_impute_threshold)


########### Trend adjustments

### Prepare data: full dataset
facility_ownership_change <- facility_ownership_change %>%
  dplyr::filter(treat == 1) %>%
  dplyr::filter(Num_changes_GUO25C == 1) %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(ReportingYear < 2016)

### Prepare data: only exiting facilities
facility_exit <- eprtr_facility_pollution %>%
  dplyr::mutate(exit = ifelse(last_report_facility < 2016, 1, 0)) %>%
  dplyr::filter(exit == 1) %>%
  dplyr::mutate(adj_emissions_ihs = log( ( adj_emissions_tot ) + ( sqrt( adj_emissions_tot )^2 + 1 ) )) %>%
  dplyr::mutate( time_to_treat = ifelse( time_to_treat > -999 & time_to_treat < -4 , -4, time_to_treat ), 
                 time_to_treat = ifelse( time_to_treat > -999 & time_to_treat >= 4, 4 - 1, time_to_treat ), 
                 ownership_change_year = ifelse( is.na( ownership_change_year ), 3000, ownership_change_year), 
                 treat_trend = treat * time_to_treat ) %>%
  dplyr::filter(ReportingYear < 2016)

### Set up function to run the honestDiD package
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


### Figure C1
res_sunab = feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                    FacilityID + ReportingYear^CountryCode + NACE_2^ReportingYear, facility_ownership_change)
etable(res_sunab)
summary(res_sunab, agg = "ATT")
iplot(res_sunab)

# Extract the beta and vcv
beta_vcv <- sunab_beta_vcv(res_sunab)

# Run sensitivity analysis for relative magnitudes
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

figC1 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                    original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% CI", x = "Bounds on relative magnitude" ) + 
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) ) 

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC1.png" ), 
        plot = figC1,
        width = 6, height = 4 )


### Figure C2
res_sunab = feols(adj_emissions_ihs ~ sunab( ownership_change_year, time_to_treat, bin.rel = c( -4:-9, 4:9 )) |
                    FacilityID + ReportingYear^CountryCode + NACE_2^ReportingYear, facility_exit)
etable(res_sunab)
summary(res_sunab, agg = "ATT")
iplot(res_sunab)

# Extract the beta and vcv
beta_vcv <- sunab_beta_vcv(res_sunab)

# Run sensitivity analysis for relative magnitudes
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

figC2 <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                             original_results) +
  theme_classic( ) +
  theme( legend.position = "bottom", legend.title = element_blank( ) ) +
  labs( y = "Robust 95% CI", x = "Bounds on relative magnitude" ) + 
  theme( panel.grid.minor = element_line(color = "lightgrey" ), 
         panel.grid.major = element_line(color = "lightgrey" ),
         panel.border = element_rect( colour = "black", fill = NA, size = 1 ) ) +
  theme( axis.title = element_text( size = 15 ), 
         axis.text = element_text( size = 15 * 0.8 ), 
         legend.text = element_text( size = 15 * 0.5 ) ) 

ggsave( filename = paste0("I:/Mitarbeiter/BCH/Backup Meine Bibliotheken/MEFINE R&R JAERE/Replication/Output/", "FigureC2.png" ), 
        plot = figC2,
        width = 6, height = 4 )
  


