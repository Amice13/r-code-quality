######################################################################################################
# Goal: 	Replication article "Retrospective Voting under Supranational Constraints"
#			Published in Electoral Studies		
# Data:		Survey experiment fielded btw May and June 2018 in Spain by Netquest
# file:		IJYUES_144022_20180608.dta
# Software:	4.1.1
# Author: 	Pablo Fernandez-Vazquez
# Date:		October 2022
######################################################################################################

# loading groundhog package
# which ensures that all other packages are loaded in the version that was used to create this replication file.
# this ensures full reproducibility into the future
	library(groundhog)

# Loading remaining packages with the right version
	groundhog.day="2022-10-1"
	pkgs=c('ggplot2','tidyverse','margins','haven', 'stargazer')
	groundhog.library(pkgs, groundhog.day)

# set working directory
	setwd( "~/Dropbox/Jurado_Fernandez_RoomToManeuver/roomtomaneuver_data/replication_rtm_ES/" )

# Loading RAW SURVEY DATA
	survey <- read_dta( "IJYUES_144022_20180608.dta", encoding = "latin1" )

# DATA TIDY-UP
	# Treatment groups
	table( survey$GRUPO )

	# Exposed to negative economic outlook
		survey$economy <- NA
		survey$economy <- ifelse( survey$GRUPO > 1, 1, 0 )

	# Exposed to supranational constraint
		survey$constraint <- ifelse( survey$GRUPO == 2, 0, NA )
		survey$constraint <- ifelse( survey$GRUPO == 3, 1, survey$constraint )
		survey$constraint <- factor ( survey$constraint, levels = c( 0, 1 ), labels = c( "NO constraint", "constraint" ) )

	# Propensity to vote for the INCUMBENT PP ( 0 lowest probability - 10 highest probability )
		survey$PP_ptv <- survey$P7_1
	# Approval of the INCUMBENT ( higher numbers - more positive view )
		survey$approval <- survey$P16
		survey$approval <- ifelse( survey$approval == 11, NA, survey$P16 )
	# Approval of the INCUMBENT ( as dummy )
		survey$approval_dummy <- ifelse( survey$approval >=5 , 1, 0 )
	# Intention to vote for the INCUMBENT
		survey$vote_intention <- survey$P6
		survey$vote_intention <- ifelse ( survey$vote_intention == 11, NA, survey$vote_intention )
		survey$PP_intention <- ifelse( survey$vote_intention == 1, 1, 0)
	# Propensity to vote for the MAIN CHALLENGER, the PSOE ( 0 lowest probability - 10 highest probability )
		survey$PSOE_ptv <- survey$P7_2
	# Propensity to vote for PODEMOS
		survey$Ps_ptv <- survey$P7_3
	# Propensity to vote for Ciudadanos
		survey$Cs_ptv <- survey$P7_4
	# Turnout ( higher numbers indicate higher probability )
		survey$turnout <- survey$P5
		survey$turnout <- ifelse( survey$turnout == 6, NA, survey$P5 )

	# Government Effort - "The government made a great effort to implement the economic policy it had promised", where 0 indicates lack of agreement and 10 maximum agreement
		survey$effort <- survey$P12
	# Retrospective economic evaluation (higher numbers indicate positive evaluation)
		survey$retrospective <- survey$P4
		survey$retrospective <- ifelse( survey$retrospective == 6, NA, survey$retrospective )
	# Approval of the European Union (higher value, more positive view)
		survey$evaluation_eu <- survey$P17
	# Responsibility attribution. Who controls economic outcomes? 0 - economy is fully shaped by the national government. 10 - economy is fully shaped by international factors
		survey$int_factors <- survey$P8
	# Responsibility attribution (2). Is the government responsible for economic performance? 0 - the government has NO responsibility, 10- The government has FULL responsibility.
		survey$resp_economy <- survey$P9
	# Responsibility attribution (3). Who is responsible for the economic situation, the domestic government or the European Union.
		survey$responsibility_eu <- survey$P10
	# Responsibility attribution (5): How internationalized the economy is. 0 - economy depends on the government, 10 - economy is fully internationalized.
		survey$internationalization <- survey$P13
	# Abortion (10 abortion is regulated by EU )
		survey$abortion <- survey$P11 
		# Recoding so that higher values indicate  regulation by domestic governments
			survey$abortion <- dplyr::recode( as.numeric ( survey$abortion ), `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, `6` = 4L, `7`= 3L, `8` = 2L, `9` = 1L, `10` = 0L )
	# Feeling represented ( 0 don't feel represented - 10 feel represented )
		survey$represented <- survey$P15


# FIGURE 2
	# data for figure 2
		fig2_data <- data.frame( AME = c( -0.55, 0 ), lower = c( - 0.8, -0.25), upper = c( -0.3, 0.25 ), treatment = c( "economy", "supranational" )  )
		fig2_data <- fig2_data %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )

	# plot
		fig2 <- ggplot( data= fig2_data ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL )
		fig2 <- fig2 + scale_y_continuous( limits =  c(-0.8, 0.25 ), breaks = c(0), name = "MARGINAL TREATMENT EFFECTS" )
		fig2 <- fig2 + ggtitle("PROPENSITY TO VOTE FOR PARTIDO POPULAR", subtitle = "THEORETICAL EXPECTATIONS") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		fig2 <- fig2 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		fig2 <- fig2 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		fig2 <- fig2 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		fig2 <- fig2 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		fig2


# FIGURE 3
	# Treatment effects	
		ptv_pp_model <- lm( PP_ptv ~ factor( GRUPO ), data = survey )
	# margins for Fig 3
		data_fig3 <- margins( ptv_pp_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		fig3 <- ggplot( data= data_fig3 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits =  c(-0.8, 0.25 )  ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		fig3 <- fig3 + ggtitle("PROPENSITY TO VOTE FOR PARTIDO POPULAR", subtitle = "EMPIRICAL RESULTS") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		fig3 <- fig3 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		fig3 <- fig3 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		fig3 <- fig3 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		fig3 <- fig3 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		fig3
			

# FIGURE 4
	# Treatment effects
		ptv_psoe_model <- lm( PSOE_ptv ~ factor( GRUPO ), data = survey )
	# margins
		data_fig4 <- margins( ptv_psoe_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		fig4 <- ggplot( data= data_fig4 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		fig4 <- fig4 + ggtitle("PROPENSITY TO VOTE FOR SOCIALIST PARTY (Main Challenger)", subtitle = "EMPIRICAL RESULTS") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		fig4 <- fig4 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		fig4 <- fig4 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		fig4 <- fig4 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		fig4 <- fig4 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		fig4


# FIGURE 5 - left plot
	# treatment effects
		resp_economy_model <- lm( resp_economy ~ factor( GRUPO ), data = survey )
	# margins
		data_fig5_left <- margins( resp_economy_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		fig5_left <- ggplot( data= data_fig5_left ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits =  c(-0.75, 0.4 )  ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		fig5_left <- fig5_left + ggtitle("RESPONSIBILITY ATTRIBUTION", subtitle = "GOVERNMENT RESPONSIBILITY OVER ECONOMY") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		fig5_left <- fig5_left + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		fig5_left <- fig5_left + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		fig5_left <- fig5_left + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		fig5_left <- fig5_left + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		fig5_left

# FIGURE 5 - right plot
	# treatment effects
		effort_model <- lm( effort ~ factor( GRUPO ), data = survey )
	# marings
		data_fig5_right <- margins( effort_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT") ) )
	# plot
		fig5_right <- ggplot( data= data_fig5_right ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits = c( -1, 0.5 ) ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		fig5_right <- fig5_right + ggtitle("GOVERNMENT EFFORT", subtitle = "EFFORT TO IMPLEMENT ECONOMIC PROMISES") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		fig5_right <- fig5_right + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		fig5_right <- fig5_right + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		fig5_right <- fig5_right + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		fig5_right <- fig5_right + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		fig5_right



####################################
# OUTPUT IN THE SUPPLEMENTARY FILE
####################################

# FIGURE A.1
	# Treatment effect
		manipulation_econ_model <- lm( retrospective ~ economy, data = survey )
	# margins
		data_figA1 <- margins( manipulation_econ_model ) %>% summary() %>% mutate( treatment = factor( c( "economy_treatment" ), levels = c( "economy_treatment" ), labels = c( "BAD ECONOMY" ) ) )
	# plot
		figA1 <- ggplot( data= data_figA1 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits = c( -0.4, 0.02 )) + ylab( "MARGINAL TREATMENT EFFECT") + xlab(NULL)
		figA1 <- figA1 + ggtitle("MANIPULATION CHECK", subtitle = "RETROSPECTIVE EVALUATION OF THE ECONOMY") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA1 <- figA1 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.5 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c" ) ) 
		figA1 <- figA1 + geom_point( aes ( y = AME , x = c( 0.5 ), color = treatment ), size=3 , alpha = .75 )
		figA1 <- figA1 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA1 <- figA1 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA1

# FIGURE A.2
	# treatment effects
		evaluation_eu_model <- lm( evaluation_eu ~ factor( GRUPO ), data = survey )
	# margins
		lm( evaluation_eu ~  relevel( factor( GRUPO ), ref = "2"), data = survey ) %>% summary()
	# plot
		data_figA2 <- margins( evaluation_eu_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )

		figA2 <- ggplot( data= data_figA2 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA2 <- figA2 + ggtitle("", subtitle = "OVERALL EVALUATION OF THE EUROPEAN UNION") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA2 <- figA2 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA2 <- figA2 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA2 <- figA2 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA2 <- figA2 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA2

# FIGURE A.3
	# Treatment effects
		int_factors_model <- lm( int_factors ~ factor( GRUPO ), data = survey )
	# margins
		data_figA3 <- margins( int_factors_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA3 <- ggplot( data= data_figA3 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits =  c(-0.75, 0.4 )  ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA3 <- figA3 + ggtitle("RESPONSIBILITY ATTRIBUTION", subtitle = "DOMESTIC GOVERNMENT OR INTERNATIONAL FACTORS") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA3 <- figA3 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA3 <- figA3 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA3 <- figA3 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA3 <- figA3 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA3

# FIGURE A.4	
	# Treatment Effects
		responsibility_eu_model <- lm( responsibility_eu ~ factor( GRUPO ), data = survey )
	# margins
		data_figA4 <- margins( responsibility_eu_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA4 <- ggplot( data= data_figA4 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits =  c(-0.75, 0.4 )  ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA4 <- figA4 + ggtitle("RESPONSIBILITY ATTRIBUTION", subtitle = "DOMESTIC GOVERNMENT or EUROPEAN UNION") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA4 <- figA4 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA4 <- figA4 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA4 <- figA4 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA4 <- figA4 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA4

# FIGURE A.5
	# Treatment Effects
		internationalized_model <- lm( internationalization ~ factor( GRUPO ), data = survey )
	# margins
		data_figA5 <- margins( internationalized_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot	
		figA5 <- ggplot( data= data_figA5 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits =  c(-0.75, 0.4 )  ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA5 <- figA5 + ggtitle("RESPONSIBILITY ATTRIBUTION", subtitle = "ECONOMY: Depends on GOVERNMENT or FULLY INTERNATIONALIZED") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA5 <- figA5 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA5 <- figA5 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA5 <- figA5 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA5 <- figA5 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA5

# FIGURE A.6
	# treatment effects
		abortion_model <- lm( abortion ~ factor( GRUPO ), data = survey )
	# margins
		data_abortion_model_margins <- margins( abortion_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA6 <- ggplot( data= data_abortion_model_margins ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA6 <- figA6 + ggtitle("PLACEBO TEST", subtitle = "RESPONSIBILITY ATTRIBUTION over ABORTION:\nSPANISH GOVERNMENT or EU") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA6 <- figA6 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA6 <- figA6 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA6 <- figA6 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA6 <- figA6 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA6

# FIGURE A.7
	# treatment effects
		approval_model <- lm( approval ~ factor( GRUPO ), data = survey )
	# margins
		data_figA7 <- margins( approval_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA7 <- ggplot( data= data_figA7 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits =  c(-0.75, 0.4 )  ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA7 <- figA7 + ggtitle("APPROVAL for PARTIDO POPULAR", subtitle = "EMPIRICAL RESULTS") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA7 <- figA7 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA7 <- figA7 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA7 <- figA7 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA7 <- figA7 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA7

# FIGURE A.8
	# treatment effects
		approval_dummy_model <- lm( approval_dummy ~ factor( GRUPO ), data = survey )
	# margins
		data_figA8 <- margins( approval_dummy_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA8 <- ggplot( data= data_figA8 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits =  c(-0.2, 0.12 )  ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA8 <- figA8 + ggtitle("APPROVAL for PARTIDO POPULAR", subtitle = "measured as dummy variable") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA8 <- figA8 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA8 <- figA8 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA8 <- figA8 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA8 <- figA8 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA8

# FIGURE A.9
	# treatment effects
		PP_intention_model <- lm( PP_intention ~ factor( GRUPO ), data = survey )
	# margins
		data_figA9 <- margins( PP_intention_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA9 <- ggplot( data= data_figA9 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits =  c(-0.06, 0.06 ) ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA9 <- figA9 + ggtitle("INTENTION to VOTE for the INCUMBENT (Popular Party)", subtitle = "EMPIRICAL RESULTS") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA9 <- figA9 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA9 <- figA9 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA9 <- figA9 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA9 <- figA9 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA9

# FIGURE A.10
	# Treatment Effects
		ptv_Ps_model <- lm( Ps_ptv ~ factor( GRUPO ), data = survey )
	# margins
		data_figA10 <- margins( ptv_Ps_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA10 <- ggplot( data= data_figA10 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA10 <- figA10 + ggtitle("PROPENSITY TO VOTE FOR PODEMOS") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA10 <- figA10 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA10 <- figA10 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA10 <- figA10 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA10 <- figA10 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA10

# FIGURE A.11
	# Treatment Effects
		ptv_Cs_model <- lm( Cs_ptv ~ factor( GRUPO ), data = survey )
	# margins
		data_figA11 <- margins( ptv_Cs_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA11 <- ggplot( data= data_figA11 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA11 <- figA11 + ggtitle("PROPENSITY TO VOTE FOR CIUDADANOS") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA11 <- figA11 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA11 <- figA11 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA11 <- figA11 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA11 <- figA11 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA11

# FIGURE A.12
	# treatment effects
		turnout_model <- lm( turnout ~ factor( GRUPO ), data = survey )
	# margins
		data_figA12 <- margins( turnout_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA12 <- ggplot( data= data_figA12 ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits = c( -0.5, 0.5 ) ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA12 <- figA12 + ggtitle("TURNOUT", subtitle = "PROPENSITY TO TURN OUT TO VOTE") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA12 <- figA12 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA12 <- figA12 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA12 <- figA12 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA12 <- figA12 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA12

# FIGURE A.13
	# treatment effects
		represented_model <- lm( represented ~ factor( GRUPO ), data = survey )
	# margins
		data_represented_model_margins <- margins( represented_model ) %>% summary() %>% mutate( treatment = factor( c( "grupo 2", "grupo 3" ), levels = c( "grupo 2", "grupo 3" ), labels = c( "BAD ECONOMY", "BAD ECONOMY &\nSUP CONSTRAINT" ) ) )
	# plot
		figA13 <- ggplot( data= data_represented_model_margins ) + scale_x_continuous( limits=c( 0, 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous( limits = c( -0.7, 0.4 ) ) + ylab( "MARGINAL TREATMENT EFFECTS") + xlab(NULL)
		figA13 <- figA13 + ggtitle("REPRESENTATION", subtitle = "FEELING REPRESENTED BY POLITICAL PARTIES") + theme( plot.title = element_text( color = "grey0", hjust = 0.5 ), plot.subtitle = element_text( color = "grey0", hjust = 0.5, face = "bold" ) )
		figA13 <- figA13 + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 0.33, 0.66 ), color = treatment ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "#ef3b2c", "#a50f15" ) ) 
		figA13 <- figA13 + geom_point( aes ( y = AME , x = c( 0.33, 0.66 ), color = treatment ), size=3 , alpha = .75 )
		figA13 <- figA13 + geom_hline( yintercept = 0 , color = "black", alpha = 0.75, linetype = "dashed" )
		figA13 <- figA13 + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "TREATMENT" ) )
		figA13


# TABLE A.2
	# GENDER Since the outcome is binary, and there are three groups, I use the chi-squared test. Reference: https://www.graphpad.com/support/faqid/1790/
		# Proportion of women in each group
			mean( subset( survey, GRUPO == 1 )$sex ) - 1
			mean( subset( survey, GRUPO == 2 )$sex ) - 1
			mean( subset( survey, GRUPO == 3 )$sex ) - 1
		# Statistical test: We need to create a contingency table, with the groupings in rows and the outcome in columns.
			table( survey$GRUPO, survey$sex ) %>% prop.table() %>% round(2)
			sex_gr <- table( survey$GRUPO, survey$sex )
			chisq.test( sex_gr )

	# AGE. The outcome is continuous. But, to avoid making assumptions about the shape of the distribution, I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
		# averages by groups
			aggregate( age ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test(age ~ GRUPO, data = survey )

	# EDUCATION. Ordinal variable 1-8 scale.  Reference: https://www.graphpad.com/support/faqid/1790/
			survey$educ <- ifelse( survey$EDUCATION == 99, NA, survey$EDUCATION )
		# averages by groups
			aggregate( educ ~ GRUPO, survey, median )
		# statistical test
			kruskal.test( educ ~ GRUPO, data = survey )

	# HEALTH POLICY Evaluation. The outcome is ordinal (1-6). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
			survey$P1_1 <- ifelse( survey$P1_1 == 6, NA, survey$P1_1 )
		# averages by groups
			aggregate( P1_1 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P1_1 ~ GRUPO, data = survey )

	# EDUCATION POLICY Evaluation. The outcome is ordinal (1-6). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
			survey$P1_2 <- ifelse( survey$P1_2 == 6, NA, survey$P1_2 )
		# averages by groups
			aggregate( P1_2 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P1_2 ~ GRUPO, data = survey )

	# PENSION POLICY Evaluation. The outcome is ordinal (1-6). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
			survey$P1_3 <- ifelse( survey$P1_3 == 6, NA, survey$P1_3 )
		# averages by groups
			aggregate( P1_3 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P1_3 ~ GRUPO, data = survey )

	# IMMIGRATION POLICY Evaluation. The outcome is ordinal (1-6). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
			survey$P1_4 <- ifelse( survey$P1_4 == 6, NA, survey$P1_4 )
		# averages by groups
			aggregate( P1_4 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P1_4 ~ GRUPO, data = survey )

	# NATIONAL UNITY POLICY Evaluation. The outcome is ordinal (1-6). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
			survey$P1_5 <- ifelse( survey$P1_5 == 6, NA, survey$P1_5 )
		# averages by groups
			aggregate( P1_5 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P1_5 ~ GRUPO, data = survey )

	# CITIZEN SAFETY POLICY Evaluation. The outcome is ordinal (1-6). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
			survey$P1_6 <- ifelse( survey$P1_6 == 6, NA, survey$P1_6 )
		# averages by groups
			aggregate( P1_6 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P1_6 ~ GRUPO, data = survey )

	# MARIANO RAJOY Evaluation. The outcome is ordinal (0-10). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
		# averages by groups
			aggregate( P2_1 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P2_1 ~ GRUPO, data = survey )
	# PEDRO SANCHEZ Evaluation. The outcome is ordinal (0-10). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
		# averages by groups
			aggregate( P2_2 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P1_6 ~ GRUPO, data = survey )
	# PABLO IGLESIAS Evaluation. The outcome is ordinal (0-10). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
		# averages by groups
			aggregate( P2_3 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P2_3 ~ GRUPO, data = survey )
	# ALBERT RIVERA Evaluation. The outcome is ordinal (0-10). I use a non-parametric test, the Kruskal-Wallis. Reference: https://www.graphpad.com/support/faqid/1790/
		# averages by groups
			aggregate( P2_4 ~ GRUPO, survey, mean )
		# statistical test
			kruskal.test( P2_4 ~ GRUPO, data = survey )

# TABLE A.3
	ptv_pp_mreg <- lm( PP_ptv ~ factor( GRUPO ) + sex + age + as.factor( educ ) + as.factor( LABORAL ), data = survey )
	summary( ptv_pp_mreg )
		# rendering
			stargazer( ptv_pp_mreg, column.labels= "PTV Partido Popular", title="Propensity to vote for the Incumbent. Estimated treatment effects with covariate adjustment. Linear regression.",
			digits = 1, star.cutoffs = c( 0.05 ), style = "apsr", covariate.labels=c( "Bad Economy", "Bad Economy +\n Sup Constraints",
				"gender", "age", "EDUC:No degree", "EDUC:primary", "EDUC:low secondary", "EDUC:high secondary", "EDUC:some college", "EDUC:college", "EDUC:Masters", "EDUC:PhD",
				"ACTIVITY:employed", "ACTIVITY:retired", "ACTIVITY:unemployed", "ACTIVITY:never employed", "ACTIVITY:student", "ACTIVITY:Home" ),
				omit.stat=c("LL","ser","f", "adj.rsq", "rsq"), no.space=TRUE )