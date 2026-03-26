#################################
# GOAL:		Replication file - Manuscript ``Believe It or Not? The Credibility of Campaign Promises''
# CODEBOOK: CCES16_UCM_OUTPUT_Feb2017_CODEBOOK.txt
# SOFTWARE:	R 3.5.1
# DATA:		CCES16_UCM_UMB_OUTPUT_Feb2017_unmatched.sav
# AUTHORS:	Pablo Fernandez-Vazquez & Alexander Theodoridis
#################################

setwd("~/Dropbox/Believe_it_or_Not_Theodoridis_Fernandez/JEPS_submission/replication_JEPS")

install.packages("haven", repos = "http://cran.us.r-project.org")
install.packages("foreign", repos = "http://cran.us.r-project.org")
install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("labelled", repos = "http://cran.us.r-project.org")
install.packages("margins", repos = "http://cran.us.r-project.org")
install.packages("stargazer", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("plotrix", repos = "http://cran.us.r-project.org")

library(haven)
library(foreign)
library(dplyr)
library(labelled)
library(margins)
library(stargazer)
library(ggplot2)
library(plotrix)



# Creating LOG FILE
	sink( "replication_logFile_JEPS.txt" )


# Loading raw data
cces_unmatched <- read_spss("CCES16_UCM_UMB_OUTPUT_Feb2017_unmatched.sav")



################################################
#### 	MANAGING and RECODING DATA 
################################################

# RESPONDENT PREFERENCE for minimum wage in R's area -- UCMwagepref
	table( cces_unmatched$UCMwagepref )

	# Transforming into FACTOR
		cces_unmatched$UCMwagepref <- factor ( cces_unmatched$UCMwagepref , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )

	# NUMERIC version of this variable
		cces_unmatched$UCMwagepref_num <- as.numeric(cces_unmatched$UCMwagepref)


# PERCEPTION about PREFERENCES for minimum wage in R's area
	
	table( cces_unmatched$UCMwageamerican )
	table( cces_unmatched$UCMwagedem ) 
	table( cces_unmatched$UCMwagerep ) 


	# Transforming to FACTORs
		cces_unmatched$UCMwageamerican <- factor ( cces_unmatched$UCMwageamerican , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )
		cces_unmatched$UCMwagedem <- factor ( cces_unmatched$UCMwagedem , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )
		cces_unmatched$UCMwagerep <- factor ( cces_unmatched$UCMwagerep , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )

	# Plotting Stereotypes
		dem_stereotype_plot <- ggplot( data = subset ( cces_unmatched, is.na( UCMwagedem ) == FALSE ) ) 
		dem_stereotype_plot <- dem_stereotype_plot + ggtitle("Stereotype about Democrat Candidate position on Minimum Wage" )
		dem_stereotype_plot <- dem_stereotype_plot + geom_bar( aes( x = UCMwagedem ) )
		dem_stereotype_plot

		rep_stereotype_plot <- ggplot( data = subset ( cces_unmatched, is.na( UCMwagerep ) == FALSE ) ) 
		rep_stereotype_plot <- rep_stereotype_plot + ggtitle("Stereotype about Republican Candidate position on Minimum Wage" )
		rep_stereotype_plot <- rep_stereotype_plot + geom_bar( aes( x = UCMwagerep ) )
		rep_stereotype_plot

	# NUMERIC versions of these variables
		cces_unmatched$UCMwageamerican_num <- as.numeric(cces_unmatched$UCMwageamerican)
		cces_unmatched$UCMwagedem_num <- as.numeric(cces_unmatched$UCMwagedem)
		cces_unmatched$UCMwagerep_num <- as.numeric(cces_unmatched$UCMwagerep)



# PERCEPTION of CANDIDATE's POSITION

	table ( cces_unmatched$UCMwagecandidatedem ) 
	table ( cces_unmatched$UCMwagecandidaterep ) 

	# Transforming to FACTORs
		cces_unmatched$UCMwagecandidatedem <- factor ( cces_unmatched$UCMwagecandidatedem , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )
		cces_unmatched$UCMwagecandidaterep <- factor ( cces_unmatched$UCMwagecandidaterep , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )

	# NUMERIC versions of these variables
		cces_unmatched$UCMwagecandidatedem_num <- as.numeric(cces_unmatched$UCMwagecandidatedem)
		cces_unmatched$UCMwagecandidaterep_num <- as.numeric(cces_unmatched$UCMwagecandidaterep)

	# Plotting CANDIDATE PERCEIVED POSITION
		perceived_candposition_dem <- ggplot( data = subset ( cces_unmatched, is.na( UCMwagecandidatedem ) == FALSE ) ) 
		perceived_candposition_dem <- perceived_candposition_dem + ggtitle("Perceived Candidate Position on Minimum Wage\nDEMOCRATIC CANDIDATE" )
		perceived_candposition_dem <- perceived_candposition_dem + geom_bar( aes( x = UCMwagecandidatedem ) )
		perceived_candposition_dem

		perceived_candposition_rep <- ggplot( data = subset ( cces_unmatched, is.na( UCMwagecandidaterep ) == FALSE ) ) 
		perceived_candposition_rep <- perceived_candposition_rep + ggtitle("Perceived Candidate Position on Minimum Wage\nREPUBLICAN CANDIDATE" )
		perceived_candposition_rep <- perceived_candposition_rep + geom_bar( aes( x = UCMwagecandidaterep ) )
		perceived_candposition_rep




# OPINION about the HONESTY of the candidate
	
	table( cces_unmatched$UCMwagehonestdem )
	table( cces_unmatched$UCMwagehonestrep )

	# Transforming to FACTORs
		cces_unmatched$UCMwagehonestdem <- factor ( cces_unmatched$UCMwagehonestdem , levels = c ( 1:5), labels = c ( "Extremely well", "Very well", "Moderately well", "Somewhat well", "Not well at all" ) )
		cces_unmatched$UCMwagehonestrep <- factor ( cces_unmatched$UCMwagehonestrep , levels = c ( 1:5), labels = c ( "Extremely well", "Very well", "Moderately well", "Somewhat well", "Not well at all" ) )

	# NUMERIC versions of these variables --- SCALE is REVErSED
		cces_unmatched$UCMwagehonestdem_rev <- 6 - as.numeric( cces_unmatched$UCMwagehonestdem )
 		cces_unmatched$UCMwagehonestrep_rev <- 6 - as.numeric( cces_unmatched$UCMwagehonestrep )

 			table( cces_unmatched$UCMwagehonestdem_rev )
			table( cces_unmatched$UCMwagehonestrep_rev )



###### ORIGINAL RANDOMIZATION VARIABLES

# Whether the PREAMBLE is shown
	table ( cces_unmatched$UCMwagepreamble , useNA = "always")

	# recoding
		cces_unmatched$UCMwagepreamble <- ifelse ( cces_unmatched$UCMwagepreamble == 2, 0, cces_unmatched$UCMwagepreamble )


# The POSITION stated by a DEMOCRATIC CANDIDATE
	# Source: variables T1dem and T2dem

	# Variable TYPES
		cces_unmatched %>% select ( T1dem, T1rep, T2dem, T2rep ) %>% apply( 2, function( x ) class( x ) )
		labelsTvars <- val_labels ( cces_unmatched$T1rep )

		cces_unmatched %>% select ( T1dem, T1rep, T2dem, T2rep ) %>% val_labels
		labelsTvars <- val_labels ( cces_unmatched$T1rep )

	table ( cces_unmatched$T1dem )
	table ( cces_unmatched$T2dem )
	table ( cces_unmatched$T1rep )
	table ( cces_unmatched$T2rep )
		
	# For the sake of comparability with the other variables, I REVERSE the order of the data
		cces_unmatched$T1dem_rev <- 6 - cces_unmatched$T1dem 
		cces_unmatched$T1rep_rev <- 6 - cces_unmatched$T1rep
		cces_unmatched$T2dem_rev <- 6 - cces_unmatched$T2dem 
		cces_unmatched$T2rep_rev <- 6 - cces_unmatched$T2rep

	# Transforming to FACTORs
		cces_unmatched$T1dem_rev <- factor ( cces_unmatched$T1dem_rev , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )
		cces_unmatched$T2dem_rev <- factor ( cces_unmatched$T2dem_rev , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )
		cces_unmatched$T1rep_rev <- factor ( cces_unmatched$T1rep_rev , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )
		cces_unmatched$T2rep_rev <- factor ( cces_unmatched$T2rep_rev , levels = c ( 1:5), labels = c ("$7.25", "$9.50", "$10.50", "$12.50", "$15 or higher" ) )
	
	# Checking
		table ( cces_unmatched$T1dem_rev )
		table ( cces_unmatched$T2dem_rev )
		table ( cces_unmatched$T1rep_rev )
		table ( cces_unmatched$T2rep_rev )
	
	# Creating numeric versions of these variables
		cces_unmatched$T1dem_rev_num <- as.numeric( cces_unmatched$T1dem_rev )
		cces_unmatched$T2dem_rev_num <- as.numeric( cces_unmatched$T2dem_rev )
		cces_unmatched$T1rep_rev_num <- as.numeric( cces_unmatched$T1rep_rev )
		cces_unmatched$T2rep_rev_num <- as.numeric( cces_unmatched$T2rep_rev )
	
		table ( cces_unmatched$T1dem_rev_num )
		table ( cces_unmatched$T2dem_rev_num )
		table ( cces_unmatched$T1rep_rev_num )
		table ( cces_unmatched$T2rep_rev_num )

	###### The type of race: PRIMARY vs GENERAL ELECTION
		table ( cces_unmatched$race_rep )
		table ( cces_unmatched$race_dem )
	
	# recoding
		cces_unmatched$primary_rep <- ifelse ( cces_unmatched$race_rep == 2, 0, cces_unmatched$race_rep )
		cces_unmatched$primary_dem <- ifelse ( cces_unmatched$race_dem == 2, 0, cces_unmatched$race_dem )




##### CREATING INDEPENDENT VARIABLES

	# [1] Difference between T1 and T2 advocated positions
		cces_unmatched$ShiftT2T1_dem <- cces_unmatched$T2dem_rev_num - cces_unmatched$T1dem_rev_num
		cces_unmatched$ShiftT2T1_rep <- cces_unmatched$T2rep_rev_num - cces_unmatched$T1rep_rev_num

		# quick histograms
			qplot(cces_unmatched$ShiftT2T1_dem, xlim = c( -4, 4 ) )
			qplot(cces_unmatched$ShiftT2T1_rep, xlim = c( -4, 4 ) )


	# [2] ABSOLUTE Distance between T1 and T2 advocated positions
		cces_unmatched$DistT2T1_dem <- abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$T1dem_rev_num )
		cces_unmatched$DistT2T1_rep <- abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$T1rep_rev_num )

		# quick histograms
			qplot(cces_unmatched$DistT2T1_dem, xlim = c( 0, 4 ) )
			qplot(cces_unmatched$DistT2T1_rep, xlim = c( 0, 4 ) )


 	# [3] Difference between T2 CANDIDATE STANCE and STEREOTYPE about the party (measured at the individual level)
		cces_unmatched$Dist_T2_Stereotype_dem <- cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwagedem_num
		cces_unmatched$Dist_T2_Stereotype_rep <- cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwagerep_num
		
		# Plotting (FIGRE A7 in the Online Appendix)
			plot_dist_T2_stereotype_dem <-  ggplot( data = cces_unmatched ) + ggtitle("Distance between T2 CANDIDATE STANCE and PARTY STEREOTYPE", subtitle = "Democratic Candidate") + theme( plot.subtitle = element_text( color = "blue" ) )
			plot_dist_T2_stereotype_dem <- plot_dist_T2_stereotype_dem + scale_x_continuous( breaks = c( -4:4 ), labels = c( "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4" ), name = "Distance between T2 CANDIDATE STANCE and PARTY STEREOTYPE" )
			plot_dist_T2_stereotype_dem <- plot_dist_T2_stereotype_dem + geom_bar( aes( x = Dist_T2_Stereotype_dem ) )
			plot_dist_T2_stereotype_dem
			ggsave(filename= "figA7_dem.pdf", plot = plot_dist_T2_stereotype_dem, width = 8 , height = 6  )

			plot_dist_T2_stereotype_rep <-  ggplot( data = cces_unmatched ) + ggtitle("Distance between T2 CANDIDATE STANCE and PARTY STEREOTYPE", subtitle = "Republican Candidate") + theme( plot.subtitle = element_text( color = "red" ) )
			plot_dist_T2_stereotype_rep <- plot_dist_T2_stereotype_rep + scale_x_continuous( breaks = c( -4:4 ), labels = c( "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4" ), name = "Distance between T2 CANDIDATE STANCE and PARTY STEREOTYPE" )
			plot_dist_T2_stereotype_rep <- plot_dist_T2_stereotype_rep + geom_bar( aes( x = Dist_T2_Stereotype_rep ) )
			plot_dist_T2_stereotype_rep
			ggsave(filename= "figA7_rep.pdf", plot = plot_dist_T2_stereotype_rep, width = 8 , height = 6  )



###### CREATING DEPENDENT VARIABLES

	# [1] Difference between Respondent's PERCEIVED CANDIDATE POSITION and the T1 CANDIDATE STANCE
		cces_unmatched$ShiftFromUsedto_dem <-  cces_unmatched$UCMwagecandidatedem_num - cces_unmatched$T1dem_rev_num
		cces_unmatched$ShiftFromUsedto_rep <-  cces_unmatched$UCMwagecandidaterep_num - cces_unmatched$T1rep_rev_num

		# quick histograms
			qplot(cces_unmatched$ShiftFromUsedto_dem, xlim = c( -4, 4 ) )
			qplot(cces_unmatched$ShiftFromUsedto_rep, xlim = c( -4, 4 ) )

		# checking
			cces_unmatched %>% dplyr::select( UCMwagecandidatedem_num, T1dem_rev_num, ShiftFromUsedto_dem ) %>% head()
			cces_unmatched %>% dplyr::select( UCMwagecandidaterep_num, T1rep_rev_num, ShiftFromUsedto_rep ) %>% head()

	# [2] *Absolute* Distance between Respondent's PERCEIVED CANDIDATE POSITION and the T2 CANDIDATE STANCE
		cces_unmatched$ClosenessT2_dem <-  abs( cces_unmatched$UCMwagecandidatedem_num - cces_unmatched$T2dem_rev_num )
		cces_unmatched$ClosenessT2_rep <-  abs( cces_unmatched$UCMwagecandidaterep_num - cces_unmatched$T2rep_rev_num )
		
		# checking
			cces_unmatched %>% dplyr::select( UCMwagecandidatedem_num, T2dem_rev_num, ClosenessT2_dem ) %>% head()
			cces_unmatched %>% dplyr::select( UCMwagecandidaterep_num, T2rep_rev_num, ClosenessT2_rep ) %>% head()


	# [3] Ratio ABSOLUTE Distance between T1 CANDIDATE STANCE and Respondent's PERCEIVED CANDIDATE POSITION OVER absolute distance between T2 and T1
		cces_unmatched$ratioshift_dem <-  abs( cces_unmatched$ShiftFromUsedto_dem ) / cces_unmatched$DistT2T1_dem
		cces_unmatched$ratioshift_rep <-  abs( cces_unmatched$ShiftFromUsedto_rep ) / cces_unmatched$DistT2T1_rep

		# checking
			cces_unmatched %>% dplyr::select( UCMwagecandidatedem_num, T1dem_rev_num, T2dem_rev_num, ratioshift_dem ) %>% head()
			cces_unmatched %>% dplyr::select( UCMwagecandidaterep_num, T1rep_rev_num, T2rep_rev_num, ratioshift_rep ) %>% head()


	# DIFFERENCE between PERCEIVED CANDIDATE PLACEMENT and STEREOTYPE about party
		cces_unmatched$Dist_Placement_Stereotype_dem <-  cces_unmatched$UCMwagecandidatedem_num - cces_unmatched$UCMwagedem_num
		cces_unmatched$Dist_Placement_Stereotype_rep <-  cces_unmatched$UCMwagecandidaterep_num - cces_unmatched$UCMwagerep_num

		# checking
			cces_unmatched %>% dplyr::select( UCMwagecandidatedem_num, UCMwagedem_num, Dist_Placement_Stereotype_dem ) %>% head()
			cces_unmatched %>% dplyr::select( UCMwagecandidaterep_num, UCMwagerep_num, Dist_Placement_Stereotype_rep ) %>% head()

		# Plotting  (This is FIGURE A6 in the Appendix)
			plot_dist_placement_stereotype_dem <-  ggplot( data = cces_unmatched ) + ggtitle("Distance between PERCEIVED CANDIDATE PLACEMENT and PARTY STEREOTYPE", subtitle = "Democratic Candidate") + theme( plot.subtitle = element_text( color = "blue" ) )
			plot_dist_placement_stereotype_dem <- plot_dist_placement_stereotype_dem + geom_bar( aes( x = Dist_Placement_Stereotype_dem ) ) + scale_x_continuous( breaks = c( -4:4 ), labels = c( "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4" ), name = "Distance between PERCEIVED CANDIDATE PLACEMENT and PARTY STEREOTYPE" )
			plot_dist_placement_stereotype_dem
			ggsave(filename= "figA6_dem.pdf", plot = plot_dist_placement_stereotype_dem, width = 8 , height = 6  )

			plot_dist_placement_stereotype_rep <-  ggplot( data = cces_unmatched ) + ggtitle("Distance between PERCEIVED CANDIDATE PLACEMENT and PARTY STEREOTYPE", subtitle = "Republican Candidate") + theme( plot.subtitle = element_text( color = "red" ) )
			plot_dist_placement_stereotype_rep <- plot_dist_placement_stereotype_rep + geom_bar( aes( x = Dist_Placement_Stereotype_rep ) ) + scale_x_continuous( breaks = c( -4:4 ), labels = c( "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4" ), name = "Distance between PERCEIVED CANDIDATE PLACEMENT and PARTY STEREOTYPE" )
			plot_dist_placement_stereotype_rep
			ggsave(filename= "figA6_rep.pdf", plot = plot_dist_placement_stereotype_rep, width = 8 , height = 6  )


	# PROPORTION between the DIFFERENCE between PERCEIVED CANDIDATE PLACEMENT and STEREOTYPE about party AND THE DIFFERENCE between T2 STANCE and the STEREOTYPE 
		cces_unmatched$prop_dist_withStereotype_dem <-  cces_unmatched$Dist_Placement_Stereotype_dem / ( cces_unmatched$Dist_T2_Stereotype_dem + 0.5 )
		cces_unmatched$prop_dist_withStereotype_rep <-  cces_unmatched$Dist_Placement_Stereotype_rep / ( cces_unmatched$Dist_T2_Stereotype_rep + 0.5 )

		# checking
			cces_unmatched %>% dplyr::select( UCMwagecandidatedem_num, T2dem_rev_num, UCMwagedem_num, prop_dist_withStereotype_dem ) %>% head()
			cces_unmatched %>% dplyr::select( UCMwagecandidaterep_num, T2rep_rev_num, UCMwagerep_num, prop_dist_withStereotype_rep ) %>% head()

		# Plotting (FIGURE A8 in the ONLINE APPENDIX)
			plot_prop_dist_withStereotype_dem <-  ggplot( data = cces_unmatched ) + ggtitle("Ratio Difference Candidate Perception and Stereotype over Difference Stance and Stereotype", subtitle = "DEMOCRATIC CANDIDATE") + theme( plot.subtitle = element_text( color = "blue" ) )
			plot_prop_dist_withStereotype_dem <- plot_prop_dist_withStereotype_dem + geom_histogram( aes( x = prop_dist_withStereotype_dem ) ) + scale_x_continuous( breaks = c( -8:8 ), name = "Ratio Difference Candidate Perception and Stereotype over Difference Stance and Stereotype" )
			plot_prop_dist_withStereotype_dem
			ggsave(filename= "figA8_dem.pdf", plot = plot_prop_dist_withStereotype_dem, width = 8.5 , height = 6  )

			plot_prop_dist_withStereotype_rep <-  ggplot( data = cces_unmatched ) + ggtitle("Ratio Difference Candidate Perception and Stereotype over Difference Stance and Stereotype", subtitle = "REPUBLICAN CANDIDATE") + theme( plot.subtitle = element_text( color = "red" ) )
			plot_prop_dist_withStereotype_rep <- plot_prop_dist_withStereotype_rep + geom_histogram( aes( x = prop_dist_withStereotype_rep ) ) + scale_x_continuous( breaks = c( -8:8 ), name = "Ratio Difference Candidate Perception and Stereotype over Difference Stance and Stereotype" )
			plot_prop_dist_withStereotype_rep
			ggsave(filename= "figA8_rep.pdf", plot = plot_prop_dist_withStereotype_rep, width = 8.5 , height = 6  )


	# PROPORTION between the DIFFERENCE between PERCEIVED CANDIDATE PLACEMENT and STEREOTYPE about party AND THE DIFFERENCE between T2 STANCE and the STEREOTYPE 
	# But considering cases where T2 = STEREOTYPE as NA
		cces_unmatched$prop_dist_withStereotype_restricted_dem <- ifelse( cces_unmatched$T2dem_rev_num != cces_unmatched$UCMwagedem_num,
		 cces_unmatched$Dist_Placement_Stereotype_dem / ( cces_unmatched$Dist_T2_Stereotype_dem ), NA )
	
		cces_unmatched$prop_dist_withStereotype_restricted_rep <-  ifelse( cces_unmatched$T2rep_rev_num != cces_unmatched$UCMwagerep_num,
			cces_unmatched$Dist_Placement_Stereotype_rep / ( cces_unmatched$Dist_T2_Stereotype_rep ), NA )

		# checking
			cces_unmatched %>% dplyr::select( UCMwagecandidatedem_num, T2dem_rev_num, UCMwagedem_num, prop_dist_withStereotype_restricted_dem ) %>% head()
			cces_unmatched %>% dplyr::select( UCMwagecandidaterep_num, T2rep_rev_num, UCMwagerep_num, prop_dist_withStereotype_restricted_rep ) %>% head()


##### CREATING CONDITIONAL VARIABLES

	# [1] Shift in postion towards AVERAGE AMERICAN
		# Operationalization: The dummy variable takes value of 1 iff
			# i) ShiftT2T1_ is POSITIVE and so is the difference between PREF of AVERAGE AMERICAN and T1 position OR
			# ii) ShiftT2T1_ is NEGATIVE and so is the difference between PREF of AVERAGE AMERICAN and T1 position
		cces_unmatched$ShiftToAverage_dem <- ifelse( cces_unmatched$ShiftT2T1_dem * ( cces_unmatched$UCMwageamerican_num - cces_unmatched$T1dem_rev_num ) > 0, 1, 0 )
		cces_unmatched$ShiftToAverage_rep <- ifelse( cces_unmatched$ShiftT2T1_rep * ( cces_unmatched$UCMwageamerican_num - cces_unmatched$T1rep_rev_num ) > 0, 1, 0 )

		# checking
			cces_unmatched %>% dplyr::select( T2dem_rev_num, T1dem_rev_num, UCMwageamerican_num, ShiftToAverage_dem ) %>% head()
			cces_unmatched %>% dplyr::select( T2rep_rev_num, T1rep_rev_num, UCMwageamerican_num, ShiftToAverage_rep ) %>% head()


	# [2] Shift in postion towards PARTY STEREOTYPE
		# Operationalization: The dummy variable takes value of 1 iff
			# i) ShiftT2T1_ is POSITIVE and so is the difference between PREF of DEMOCRAT/REPUB and T1 position OR
			# ii) ShiftT2T1_ is NEGATIVE and so is the difference between PREF of DEMOCRAT/REPUB and T1 position 
		cces_unmatched$ShiftToStereotype_dem <- ifelse( cces_unmatched$ShiftT2T1_dem * ( cces_unmatched$UCMwagedem_num - cces_unmatched$T1dem_rev_num ) > 0, 1, 0 )
		cces_unmatched$ShiftToStereotype_rep <- ifelse( cces_unmatched$ShiftT2T1_rep * ( cces_unmatched$UCMwagerep_num - cces_unmatched$T1rep_rev_num ) > 0, 1, 0 )

		# checking
			cces_unmatched %>% dplyr::select( T2dem_rev_num, T1dem_rev_num, UCMwagedem_num, ShiftToStereotype_dem ) %>% head()
			cces_unmatched %>% dplyr::select( T2rep_rev_num, T1rep_rev_num, UCMwagerep_num, ShiftToStereotype_rep ) %>% head()

		# Plot
			ShiftToStereotype_dem_plot <- ggplot( data = subset ( cces_unmatched, is.na( ShiftToStereotype_dem ) == FALSE ) ) 
			ShiftToStereotype_dem_plot <- ShiftToStereotype_dem_plot + ggtitle("Candidate Shift is Towards Stereotype about Party\nDemocratic Candidate" )
			ShiftToStereotype_dem_plot <- ShiftToStereotype_dem_plot + geom_bar( aes( x = as.factor( ShiftToStereotype_dem ) ) )
			ShiftToStereotype_dem_plot
	
			ShiftToStereotype_rep_plot <- ggplot( data = subset ( cces_unmatched, is.na( ShiftToStereotype_rep ) == FALSE ) ) 
			ShiftToStereotype_rep_plot <- ShiftToStereotype_rep_plot + ggtitle("Candidate Shift is Towards Stereotype about Party\nRepublican Candidate" )
			ShiftToStereotype_rep_plot <- ShiftToStereotype_rep_plot + geom_bar( aes( x = as.factor( ShiftToStereotype_rep ) ) )
			ShiftToStereotype_rep_plot


	# [2 bis] T2 STANCE closer to PARTY STEREOTYPE
		# Operationalization: The dummy variable takes value of 1 iff T2 is closer to UCMwagedem / UCMwagerep than T1 
		cces_unmatched$T2CloserToStereotype_dem <- ifelse( abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwagedem_num ) < abs ( cces_unmatched$T1dem_rev_num - cces_unmatched$UCMwagedem_num ), 1, 0 )
		cces_unmatched$T2CloserToStereotype_rep <- ifelse( abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwagerep_num ) < abs ( cces_unmatched$T1rep_rev_num - cces_unmatched$UCMwagerep_num ), 1, 0 )

		# checking
			cces_unmatched %>% dplyr::select( T2dem_rev_num, T1dem_rev_num, UCMwagedem_num, T2CloserToStereotype_dem ) %>% head()
			cces_unmatched %>% dplyr::select( T2rep_rev_num, T1rep_rev_num, UCMwagerep_num, T2CloserToStereotype_rep ) %>% head()


	# [3] Shift in postion towards RESPONDENT
		# Operationalization: The dummy variable takes value of 1 iff
			# i) ShiftT2T1_ is POSITIVE and so is the difference between RESPONDENT PREFERENCE and T1 position OR
			# ii) ShiftT2T1_ is NEGATIVE and so is the difference between RESPONDENT PREFERENCE and T1 position 
		cces_unmatched$ShiftToRespondent_dem <- ifelse( cces_unmatched$ShiftT2T1_dem * ( cces_unmatched$UCMwagepref_num - cces_unmatched$T1dem_rev_num ) > 0, 1, 0 )
		cces_unmatched$ShiftToRespondent_rep <- ifelse( cces_unmatched$ShiftT2T1_rep * ( cces_unmatched$UCMwagepref_num - cces_unmatched$T1rep_rev_num ) > 0, 1, 0 )

		# checking
			cces_unmatched %>% dplyr::select( T2dem_rev_num, T1dem_rev_num, UCMwagepref_num, ShiftToRespondent_dem ) %>% head()
			cces_unmatched %>% dplyr::select( T2rep_rev_num, T1rep_rev_num, UCMwagepref_num, ShiftToRespondent_rep ) %>% head()


	# [4] DIRECTION of the Shift in candidate positions
		# i) ShiftToRight takes value of 1 if T2 > T1
		# ii) ShiftToLeft takes value of 1 if T2 < T1
		cces_unmatched$ShiftToRight_dem <- ifelse( cces_unmatched$ShiftT2T1_dem  > 0, 1, 0 )
		cces_unmatched$ShiftToRight_rep <- ifelse( cces_unmatched$ShiftT2T1_rep  > 0, 1, 0 )

		cces_unmatched$ShiftToLeft_dem <- ifelse( cces_unmatched$ShiftT2T1_dem  < 0, 1, 0 )
		cces_unmatched$ShiftToLeft_rep <- ifelse( cces_unmatched$ShiftT2T1_rep  < 0, 1, 0 )

		# checking
			cces_unmatched %>% dplyr::select( T2dem_rev_num, T1dem_rev_num, ShiftToRight_dem ) %>% head()
			cces_unmatched %>% dplyr::select( T2rep_rev_num, T1rep_rev_num, ShiftToRight_rep ) %>% head()

			cces_unmatched %>% dplyr::select( T2dem_rev_num, T1dem_rev_num, ShiftToLeft_dem ) %>% head()
			cces_unmatched %>% dplyr::select( T2rep_rev_num, T1rep_rev_num, ShiftToLeft_rep ) %>% head()


	# [5] PARTY ID -- Original variable is pid3lean -- Provided by Alex Theodoridis ( -1 is Democrat, 0 Independent, 1 Republican )
		table ( cces_unmatched$pid3lean )
		class ( cces_unmatched$pid3lean )

		cces_unmatched$Democrat <- ifelse ( cces_unmatched$pid3lean == - 1 , 1, 0 )
		cces_unmatched$Republican <- ifelse ( cces_unmatched$pid3lean == 1 , 1, 0 )
		cces_unmatched$Independent <- ifelse ( cces_unmatched$pid3lean == 0 , 1, 0 )


		# IDEOLOGICAL partisans
			cces_unmatched$Ideological_Democrat <- ifelse ( cces_unmatched$Democrat == 1 &
			 ( cces_unmatched$UCMwagepref == "$10.50" | cces_unmatched$UCMwagepref == "$12.50" | cces_unmatched$UCMwagepref == "$15 or higher"), 1, 0 )
			cces_unmatched$Ideological_Republican <- ifelse ( cces_unmatched$Republican == 1 &
			 ( cces_unmatched$UCMwagepref == "$10.50" | cces_unmatched$UCMwagepref == "$9.50" | cces_unmatched$UCMwagepref == "$7.25"), 1, 0 )
		
		# NON-IDEOLOGICAL partisans
			cces_unmatched$NONideological_Democrat <- ifelse ( cces_unmatched$Democrat == 1 &
			 ( cces_unmatched$UCMwagepref == "$10.50" | cces_unmatched$UCMwagepref == "$9.50" | cces_unmatched$UCMwagepref == "$7.25"), 1, 0 )
			cces_unmatched$NONideological_Republican <- ifelse ( cces_unmatched$Republican == 1 &
			 ( cces_unmatched$UCMwagepref == "$10.50" | cces_unmatched$UCMwagepref == "$12.50" | cces_unmatched$UCMwagepref == "$15 or higher"), 1, 0 )
		

	# [6] T2 CLOSER to AVERAGE AMERICAN
		# Operationalization: The dummy variable takes value of 1 iff T2 is closer to UCMwageamerican than T1
		cces_unmatched$T2CloserToAverage_dem <- ifelse( abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwageamerican_num ) < abs ( cces_unmatched$T1dem_rev_num - cces_unmatched$UCMwageamerican_num ), 1, 0 )
		cces_unmatched$T2CloserToAverage_rep <- ifelse( abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwageamerican_num ) < abs ( cces_unmatched$T1rep_rev_num - cces_unmatched$UCMwageamerican_num ), 1, 0 )

		# checking
			cces_unmatched %>% dplyr::select( T2dem_rev_num, T1dem_rev_num, UCMwageamerican_num, T2CloserToAverage_dem ) %>% head()
			cces_unmatched %>% dplyr::select( T2rep_rev_num, T1rep_rev_num, UCMwageamerican_num, T2CloserToAverage_rep ) %>% head()

	

	# [7] T2 CLOSER to RESPONDENT
		# Operationalization: The dummy variable takes value of 1 iff T2 is closer to UCMwagedem / UCMwagerep than T1 and 0 otherwise
		cces_unmatched$T2CloserToRespondent_dem <- ifelse( abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwagepref_num ) < abs ( cces_unmatched$T1dem_rev_num - cces_unmatched$UCMwagepref_num ), 1, 0 )
		cces_unmatched$T2CloserToRespondent_rep <- ifelse( abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwagepref_num ) < abs ( cces_unmatched$T1rep_rev_num - cces_unmatched$UCMwagepref_num ), 1, 0 )
	
		# checking
			cces_unmatched %>% dplyr::select( T2dem_rev_num, T1dem_rev_num, UCMwagepref_num, T2CloserToRespondent_dem ) %>% head()
			cces_unmatched %>% dplyr::select( T2rep_rev_num, T1rep_rev_num, UCMwagepref_num, T2CloserToRespondent_rep ) %>% head()


	# [7 bis] T2 CLOSER to RESPONDENT (Alternative definition)
		# Operationalization: The dummy variable takes value of 
		#	1 iff T2 is closer to UCMwagepref than T1 
		# 	0 if T2 and T1 are equally distanced to the otherwise
		#	-1 if T2 if farther away than T1
		cces_unmatched$T2CloserToResp_alt_dem <- ifelse( abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwagepref_num ) < abs ( cces_unmatched$T1dem_rev_num - cces_unmatched$UCMwagepref_num ), 1, NA )
		cces_unmatched$T2CloserToResp_alt_dem <- ifelse( abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwagepref_num ) > abs ( cces_unmatched$T1dem_rev_num - cces_unmatched$UCMwagepref_num ), -1, cces_unmatched$T2CloserToResp_alt_dem )
		cces_unmatched$T2CloserToResp_alt_dem <- ifelse( abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwagepref_num ) == abs (cces_unmatched$T1dem_rev_num - cces_unmatched$UCMwagepref_num ), 0, cces_unmatched$T2CloserToResp_alt_dem )

		cces_unmatched$T2CloserToResp_alt_rep <- ifelse( abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwagepref_num ) < abs ( cces_unmatched$T1rep_rev_num - cces_unmatched$UCMwagepref_num ), 1, NA )
		cces_unmatched$T2CloserToResp_alt_rep <- ifelse( abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwagepref_num ) > abs ( cces_unmatched$T1rep_rev_num - cces_unmatched$UCMwagepref_num ), -1, cces_unmatched$T2CloserToResp_alt_rep )
		cces_unmatched$T2CloserToResp_alt_rep <- ifelse( abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwagepref_num ) == abs (cces_unmatched$T1rep_rev_num - cces_unmatched$UCMwagepref_num ), 0, cces_unmatched$T2CloserToResp_alt_rep )

		# Checking
			cces_unmatched %>% select( UCMwagepref_num, T1dem_rev_num, T2dem_rev_num,  T2CloserToResp_alt_dem ) %>% head()
			cces_unmatched %>% select( UCMwagepref_num, T1rep_rev_num, T2rep_rev_num,  T2CloserToResp_alt_rep ) %>% head()

			table( cces_unmatched$T2CloserToResp_alt_dem )
			table( cces_unmatched$T2CloserToResp_alt_rep )


	# [8] T2 is CLOSER to PIVOTAL VOTER
		# Operationalization: The dummy variable takes value of 1 iff T2 is closer to UCMwageamerican if GENERAL ELECTION and closer to UCMwagedem / UCMwagerep if PRIMARY
		cces_unmatched$T2Optimal_dem <- ifelse( abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwageamerican_num ) < abs ( cces_unmatched$T1dem_rev_num - cces_unmatched$UCMwageamerican_num )   & cces_unmatched$primary_dem == 0 , 1, 0 )
		cces_unmatched$T2Optimal_dem <- ifelse( abs ( cces_unmatched$T2dem_rev_num - cces_unmatched$UCMwagedem_num ) < abs ( cces_unmatched$T1dem_rev_num - cces_unmatched$UCMwagedem_num )   & cces_unmatched$primary_dem == 1 , 1, cces_unmatched$T2Optimal_dem )
		
		cces_unmatched$T2Optimal_rep <- ifelse( abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwageamerican_num ) < abs ( cces_unmatched$T1rep_rev_num - cces_unmatched$UCMwageamerican_num )   & cces_unmatched$primary_rep == 0 , 1, 0 )
		cces_unmatched$T2Optimal_rep <- ifelse( abs ( cces_unmatched$T2rep_rev_num - cces_unmatched$UCMwagerep_num ) < abs ( cces_unmatched$T1rep_rev_num - cces_unmatched$UCMwagerep_num )   & cces_unmatched$primary_rep == 1 , 1, cces_unmatched$T2Optimal_rep )


 		# Checking
			cces_unmatched %>% select( T1dem_rev_num, T2dem_rev_num, UCMwageamerican_num, UCMwagedem_num, primary_dem, T2Optimal_dem ) %>% head()
			cces_unmatched %>% select( T1rep_rev_num, T2rep_rev_num, UCMwageamerican_num, UCMwagerep_num, primary_rep, T2Optimal_rep ) %>% head()



################################################################################################################################################
				# EVIDENCE 
################################################################################################################################################


#####################	FIGURE 6 : How the policy shift affects voter perceptions of where the candidate stands.

	avdiff_republican <- data.frame ( CandidateParty = NA ) 
	avdiff_democrat <- data.frame ( CandidateParty = NA ) 

	# Average Distance between Respondent's perception and the initial candidate position, BY CANDIDATE PARTY
		avdiff_republican$shift <- mean ( abs( cces_unmatched$ShiftFromUsedto_rep ), na.rm = TRUE )
		avdiff_republican$se <- std.error ( abs( cces_unmatched$ShiftFromUsedto_rep ), na.rm = TRUE )
		avdiff_republican <- avdiff_republican %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )
	
		avdiff_democrat$shift <- mean ( abs( cces_unmatched$ShiftFromUsedto_dem ), na.rm = TRUE )
		avdiff_democrat$se <- std.error ( abs( cces_unmatched$ShiftFromUsedto_dem ), na.rm = TRUE )
		avdiff_democrat <- avdiff_democrat %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Party
		avdiff_plot_republican <- ggplot( data= avdiff_republican ) + scale_x_continuous( limits=c(0, length( avdiff_republican$shift )+ 1 ), name="", breaks = NULL ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("REPUBLICAN CANDIDATE")
		avdiff_plot_republican <- avdiff_plot_republican + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ) ), size=1.5, alpha = .75)
		avdiff_plot_republican <- avdiff_plot_republican + geom_point( aes ( y = shift , x = c( 1 : length( up ) ) ), size=1.5 , alpha = .75 )
		avdiff_plot_republican <- avdiff_plot_republican + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "red" ))
		avdiff_plot_republican
		ggsave(filename= "fig6_REP.pdf", plot = avdiff_plot_republican, width = 11/3 , height = 8/3  )


	# Democratic Party
		avdiff_plot_democrat <- ggplot( data= avdiff_democrat ) + scale_x_continuous( limits=c(0, length( avdiff_democrat$shift )+ 1 ), name="", breaks = NULL ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("DEMOCRATIC CANDIDATE")
		avdiff_plot_democrat <- avdiff_plot_democrat + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ) ), size=1.5, alpha = .75)
		avdiff_plot_democrat <- avdiff_plot_democrat + geom_point( aes ( y = shift , x = c( 1 : length( up ) ) ), size=1.5 , alpha = .75 )
		avdiff_plot_democrat <- avdiff_plot_democrat + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "blue" ))
		avdiff_plot_democrat
		ggsave(filename= "fig6_DEM.pdf", plot = avdiff_plot_democrat, width = 11/3 , height = 8/3  )




#####################	FIGURE 7 : How the policy shift affects voter perceptions of where the candidate stands. BY PARTISAN GROUP.

##### Getting rid of independents
	noindep_unm <- subset( cces_unmatched, Independent == 0 )

# Democratic Candidate
	t.test( abs( noindep_unm$ShiftFromUsedto_dem ) ~  noindep_unm$Democrat )	

# Republican Candidate
	t.test( abs( noindep_unm$ShiftFromUsedto_rep ) ~  noindep_unm$Republican )	


#### GENERATING GRAPH -- Democratic candidate

	avdiff_PARTYID_dem <- data.frame ( partyID = factor ( c( 2, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (D)","out-partisan (R)" ) ) ) 

	avdiff_PARTYID_dem$shift [ avdiff_PARTYID_dem$partyID == "out-partisan (R)"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Democrat == 0 ] ), na.rm = TRUE )
	avdiff_PARTYID_dem$shift [ avdiff_PARTYID_dem$partyID == "in-partisan (D)"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Democrat == 1 ] ), na.rm = TRUE )

	avdiff_PARTYID_dem$se [ avdiff_PARTYID_dem$partyID == "out-partisan (R)"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Democrat == 0 ] ), na.rm = TRUE )
	avdiff_PARTYID_dem$se [ avdiff_PARTYID_dem$partyID == "in-partisan (D)"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Democrat == 1 ] ), na.rm = TRUE )

	avdiff_PARTYID_dem <- avdiff_PARTYID_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

		## ggplot
		pid_nomodel_dem <- ggplot( data= avdiff_PARTYID_dem ) + scale_x_continuous( limits=c(0, length( avdiff_PARTYID_dem$shift )+ 1 ), name="", breaks = NULL ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("DEMOCRATIC CANDIDATE")
		pid_nomodel_dem <- pid_nomodel_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		pid_nomodel_dem <- pid_nomodel_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=2 , alpha = .75 )
		pid_nomodel_dem <- pid_nomodel_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "blue" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		pid_nomodel_dem
		ggsave("fig7_bw_DEM.pdf", plot = pid_nomodel_dem, width = 15/3 , height = 8/3  )


	#### GENERATING GRAPH -- Republican candidate
	
	avdiff_PARTYID_rep <- data.frame ( partyID = factor ( c( 1, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ) ) 

	avdiff_PARTYID_rep$shift [ avdiff_PARTYID_rep$partyID == "in-partisan (R)"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Democrat == 0 ] ), na.rm = TRUE )
	avdiff_PARTYID_rep$shift [ avdiff_PARTYID_rep$partyID == "out-partisan (D)"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Democrat == 1 ] ), na.rm = TRUE )

	avdiff_PARTYID_rep$se [ avdiff_PARTYID_rep$partyID == "in-partisan (R)"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Democrat == 0 ] ), na.rm = TRUE )
	avdiff_PARTYID_rep$se [ avdiff_PARTYID_rep$partyID == "out-partisan (D)"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Democrat == 1 ] ), na.rm = TRUE )

	avdiff_PARTYID_rep <- avdiff_PARTYID_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

		## ggplot
		pid_nomodel_rep <- ggplot( data= avdiff_PARTYID_rep ) + scale_x_continuous( limits=c(0, length( avdiff_PARTYID_rep$shift )+ 1 ), name="", breaks = NULL ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("REPUBLICAN CANDIDATE")
		pid_nomodel_rep <- pid_nomodel_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		pid_nomodel_rep <- pid_nomodel_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=2 , alpha = .75 )
		pid_nomodel_rep <- pid_nomodel_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "red" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		pid_nomodel_rep
		ggsave("fig7_bw_REP.pdf", plot = pid_nomodel_rep, width = 15/3 , height = 8/3  )





#####################	FIGURE 8 : The effect of the candidate's shift as a function of the RESPONDENT'S PARTY ID and HER OWN PREFERENCE on the policy issue
	
	### Getting rid of independents
	noindep_unm <- subset( cces_unmatched, Independent == 0 )

	avdiff_closer_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	avdiff_closer_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		avdiff_closer_dem$shift [ avdiff_closer_dem$partyID == "out-partisan (R)" & avdiff_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_dem$shift [ avdiff_closer_dem$partyID == "out-partisan (R)" & avdiff_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		avdiff_closer_dem$shift [ avdiff_closer_dem$partyID == "in-partisan (D)" & avdiff_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_dem$shift [ avdiff_closer_dem$partyID == "in-partisan (D)" & avdiff_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_dem$se [ avdiff_closer_dem$partyID == "out-partisan (R)" & avdiff_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_dem$se [ avdiff_closer_dem$partyID == "out-partisan (R)" & avdiff_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		avdiff_closer_dem$se [ avdiff_closer_dem$partyID == "in-partisan (D)" & avdiff_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_dem$se [ avdiff_closer_dem$partyID == "in-partisan (D)" & avdiff_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_dem <- avdiff_closer_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		avdiff_closer_rep$shift [ avdiff_closer_rep$partyID == "in-partisan (R)" & avdiff_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_rep$shift [ avdiff_closer_rep$partyID == "in-partisan (R)" & avdiff_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		avdiff_closer_rep$shift [ avdiff_closer_rep$partyID == "out-partisan (D)" & avdiff_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_rep$shift [ avdiff_closer_rep$partyID == "out-partisan (D)" & avdiff_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_rep$se [ avdiff_closer_rep$partyID == "in-partisan (R)" & avdiff_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_rep$se [ avdiff_closer_rep$partyID == "in-partisan (R)" & avdiff_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		avdiff_closer_rep$se [ avdiff_closer_rep$partyID == "out-partisan (D)" & avdiff_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_rep$se [ avdiff_closer_rep$partyID == "out-partisan (D)" & avdiff_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$ShiftFromUsedto_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_rep <- avdiff_closer_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	## PLOTS
		plot_pid_closer_nomodel_dem <- ggplot( data= avdiff_closer_dem ) + scale_x_continuous( limits=c(0, length( avdiff_closer_dem$shift )+ 1 ), name="", labels = avdiff_closer_dem$Closer, breaks = c( 1 : length( avdiff_closer_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("DEMOCRATIC CANDIDATE")
		plot_pid_closer_nomodel_dem <- plot_pid_closer_nomodel_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_pid_closer_nomodel_dem <- plot_pid_closer_nomodel_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_pid_closer_nomodel_dem <- plot_pid_closer_nomodel_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "blue" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_pid_closer_nomodel_dem
		ggsave("fig8_dem.pdf", plot = plot_pid_closer_nomodel_dem, width = 25/4 , height = 10/3  )

		plot_pid_closer_nomodel_rep <- ggplot( data= avdiff_closer_rep ) + scale_x_continuous( limits=c(0, length( avdiff_closer_rep$shift )+ 1 ), name="", labels = avdiff_closer_rep$Closer, breaks = c( 1 : length( avdiff_closer_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("REPUBLICAN CANDIDATE")
		plot_pid_closer_nomodel_rep <- plot_pid_closer_nomodel_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_pid_closer_nomodel_rep <- plot_pid_closer_nomodel_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_pid_closer_nomodel_rep <- plot_pid_closer_nomodel_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "red" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_pid_closer_nomodel_rep
		ggsave("fig8_rep.pdf", plot = plot_pid_closer_nomodel_rep, width = 25/4 , height = 10/3  )





#############################		ONLINE APPENDIX		#############################


#####################	FIGURE A3 : How the policy shift affects voter perceptions of where the candidate stands. INDEPENDENT RESPONDENTS ONLY.
	
	avdiff_closer_INDEP_dem <- data.frame ( Closer = factor ( c( 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	avdiff_closer_INDEP_rep <- data.frame ( Closer = factor ( c( 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate
		avdiff_closer_INDEP_dem$shift [ avdiff_closer_INDEP_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( cces_unmatched$ShiftFromUsedto_dem[ cces_unmatched$Independent == 1 & cces_unmatched$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_INDEP_dem$shift [ avdiff_closer_INDEP_dem$Closer == "TOWARDS\nRESPONDENT"  ]    <- mean ( abs( cces_unmatched$ShiftFromUsedto_dem[ cces_unmatched$Independent == 1 & cces_unmatched$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_INDEP_dem$se [ avdiff_closer_INDEP_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( cces_unmatched$ShiftFromUsedto_dem[ cces_unmatched$Independent == 1 & cces_unmatched$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_INDEP_dem$se [ avdiff_closer_INDEP_dem$Closer == "TOWARDS\nRESPONDENT"  ]    <- std.error ( abs( cces_unmatched$ShiftFromUsedto_dem[ cces_unmatched$Independent == 1 & cces_unmatched$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		
		avdiff_closer_INDEP_dem <- avdiff_closer_INDEP_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate
		avdiff_closer_INDEP_rep$shift [ avdiff_closer_INDEP_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( cces_unmatched$ShiftFromUsedto_rep[ cces_unmatched$Independent == 1 & cces_unmatched$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_INDEP_rep$shift [ avdiff_closer_INDEP_rep$Closer == "TOWARDS\nRESPONDENT"  ]    <- mean ( abs( cces_unmatched$ShiftFromUsedto_rep[ cces_unmatched$Independent == 1 & cces_unmatched$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_INDEP_rep$se [ avdiff_closer_INDEP_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( cces_unmatched$ShiftFromUsedto_rep[ cces_unmatched$Independent == 1 & cces_unmatched$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_INDEP_rep$se [ avdiff_closer_INDEP_rep$Closer == "TOWARDS\nRESPONDENT"  ]    <- std.error ( abs( cces_unmatched$ShiftFromUsedto_rep[ cces_unmatched$Independent == 1 & cces_unmatched$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		
		avdiff_closer_INDEP_rep <- avdiff_closer_INDEP_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )
	

	## PLOTS
		plot_closer_nomodel_INDEP_dem <- ggplot( data= avdiff_closer_INDEP_dem ) + scale_x_continuous( limits=c(0, length( avdiff_closer_INDEP_dem$shift )+ 1 ), name="", labels = avdiff_closer_INDEP_dem$Closer, breaks = c( 1 : length( avdiff_closer_INDEP_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) 
		plot_closer_nomodel_INDEP_dem <- plot_closer_nomodel_INDEP_dem + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "Independent Respondents Only") + theme( plot.title = element_text( color = "#006BA6" ))
		plot_closer_nomodel_INDEP_dem <- plot_closer_nomodel_INDEP_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = Closer ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey40", "grey0" ) ) 
		plot_closer_nomodel_INDEP_dem <- plot_closer_nomodel_INDEP_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = Closer ), size=3 , alpha = .75 )
		plot_closer_nomodel_INDEP_dem <- plot_closer_nomodel_INDEP_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides ( color = FALSE )
		plot_closer_nomodel_INDEP_dem
		ggsave("figA3_dem.pdf", plot = plot_closer_nomodel_INDEP_dem, width = 7.5 , height = 4  )

		plot_closer_nomodel_INDEP_rep <- ggplot( data= avdiff_closer_INDEP_rep ) + scale_x_continuous( limits=c(0, length( avdiff_closer_INDEP_rep$shift )+ 1 ), name="", labels = avdiff_closer_INDEP_rep$Closer, breaks = c( 1 : length( avdiff_closer_INDEP_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) 
		plot_closer_nomodel_INDEP_rep <- plot_closer_nomodel_INDEP_rep + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Independent Respondents Only") + theme( plot.title = element_text( color = "#E91D0E" ))
		plot_closer_nomodel_INDEP_rep <- plot_closer_nomodel_INDEP_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = Closer ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey40", "grey0" ) ) 
		plot_closer_nomodel_INDEP_rep <- plot_closer_nomodel_INDEP_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = Closer ), size=3 , alpha = .75 )
		plot_closer_nomodel_INDEP_rep <- plot_closer_nomodel_INDEP_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides ( color = FALSE )
		plot_closer_nomodel_INDEP_rep
		ggsave("figA3_rep.pdf", plot = plot_closer_nomodel_INDEP_rep, width = 7.5 , height = 4  )



#####################	FIGURE A4 and A5


# 	Estimate the presence of PMPM conditioning on whether that the T2 stance is CLOSER or FARTHER to the stereotype (measured at the respondent-level)
#	The conditioning variable is T2CloserToStereotype_dem/_rep

	Stereotype_T2closer_dem <- subset( cces_unmatched, T2CloserToStereotype_dem == 1 )
	Stereotype_T2closer_rep <- subset( cces_unmatched, T2CloserToStereotype_rep == 1 )

	Stereotype_T2farther_dem <- subset( cces_unmatched, T2CloserToStereotype_dem == 0 )
	Stereotype_T2farther_rep <- subset( cces_unmatched, T2CloserToStereotype_rep == 0 )


# ONLY IF T2 IS CLOSER TO THE STEREOTYPE (figure A5)
	avdiff_closer_stereotype_T2closer_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	avdiff_closer_stereotype_T2closer_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		avdiff_closer_stereotype_T2closer_dem$shift [ avdiff_closer_stereotype_T2closer_dem$partyID == "out-partisan (R)" & avdiff_closer_stereotype_T2closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2closer_dem$ShiftFromUsedto_dem[ Stereotype_T2closer_dem$Republican == 1 &  Stereotype_T2closer_dem$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_dem$shift [ avdiff_closer_stereotype_T2closer_dem$partyID == "out-partisan (R)" & avdiff_closer_stereotype_T2closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2closer_dem$ShiftFromUsedto_dem[ Stereotype_T2closer_dem$Republican == 1 & Stereotype_T2closer_dem$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_dem$shift [ avdiff_closer_stereotype_T2closer_dem$partyID == "in-partisan (D)" & avdiff_closer_stereotype_T2closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2closer_dem$ShiftFromUsedto_dem[ Stereotype_T2closer_dem$Democrat == 1 & Stereotype_T2closer_dem$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_dem$shift [ avdiff_closer_stereotype_T2closer_dem$partyID == "in-partisan (D)" & avdiff_closer_stereotype_T2closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2closer_dem$ShiftFromUsedto_dem[ Stereotype_T2closer_dem$Democrat == 1 & Stereotype_T2closer_dem$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_stereotype_T2closer_dem$se [ avdiff_closer_stereotype_T2closer_dem$partyID == "out-partisan (R)" & avdiff_closer_stereotype_T2closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2closer_dem$ShiftFromUsedto_dem[ Stereotype_T2closer_dem$Republican == 1 &  Stereotype_T2closer_dem$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_dem$se [ avdiff_closer_stereotype_T2closer_dem$partyID == "out-partisan (R)" & avdiff_closer_stereotype_T2closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2closer_dem$ShiftFromUsedto_dem[ Stereotype_T2closer_dem$Republican == 1 & Stereotype_T2closer_dem$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_dem$se [ avdiff_closer_stereotype_T2closer_dem$partyID == "in-partisan (D)" & avdiff_closer_stereotype_T2closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2closer_dem$ShiftFromUsedto_dem[ Stereotype_T2closer_dem$Democrat == 1 & Stereotype_T2closer_dem$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_dem$se [ avdiff_closer_stereotype_T2closer_dem$partyID == "in-partisan (D)" & avdiff_closer_stereotype_T2closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2closer_dem$ShiftFromUsedto_dem[ Stereotype_T2closer_dem$Democrat == 1 & Stereotype_T2closer_dem$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_stereotype_T2closer_dem <- avdiff_closer_stereotype_T2closer_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		avdiff_closer_stereotype_T2closer_rep$shift [ avdiff_closer_stereotype_T2closer_rep$partyID == "in-partisan (R)" & avdiff_closer_stereotype_T2closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2closer_rep$ShiftFromUsedto_rep[ Stereotype_T2closer_rep$Republican == 1 &  Stereotype_T2closer_rep$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_rep$shift [ avdiff_closer_stereotype_T2closer_rep$partyID == "in-partisan (R)" & avdiff_closer_stereotype_T2closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2closer_rep$ShiftFromUsedto_rep[ Stereotype_T2closer_rep$Republican == 1 & Stereotype_T2closer_rep$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_rep$shift [ avdiff_closer_stereotype_T2closer_rep$partyID == "out-partisan (D)" & avdiff_closer_stereotype_T2closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2closer_rep$ShiftFromUsedto_rep[ Stereotype_T2closer_rep$Democrat == 1 & Stereotype_T2closer_rep$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_rep$shift [ avdiff_closer_stereotype_T2closer_rep$partyID == "out-partisan (D)" & avdiff_closer_stereotype_T2closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2closer_rep$ShiftFromUsedto_rep[ Stereotype_T2closer_rep$Democrat == 1 & Stereotype_T2closer_rep$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_stereotype_T2closer_rep$se [ avdiff_closer_stereotype_T2closer_rep$partyID == "in-partisan (R)" & avdiff_closer_stereotype_T2closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2closer_rep$ShiftFromUsedto_rep[ Stereotype_T2closer_rep$Republican == 1 &  Stereotype_T2closer_rep$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_rep$se [ avdiff_closer_stereotype_T2closer_rep$partyID == "in-partisan (R)" & avdiff_closer_stereotype_T2closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2closer_rep$ShiftFromUsedto_rep[ Stereotype_T2closer_rep$Republican == 1 & Stereotype_T2closer_rep$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_rep$se [ avdiff_closer_stereotype_T2closer_rep$partyID == "out-partisan (D)" & avdiff_closer_stereotype_T2closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2closer_rep$ShiftFromUsedto_rep[ Stereotype_T2closer_rep$Democrat == 1 & Stereotype_T2closer_rep$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2closer_rep$se [ avdiff_closer_stereotype_T2closer_rep$partyID == "out-partisan (D)" & avdiff_closer_stereotype_T2closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2closer_rep$ShiftFromUsedto_rep[ Stereotype_T2closer_rep$Democrat == 1 & Stereotype_T2closer_rep$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_stereotype_T2closer_rep <- avdiff_closer_stereotype_T2closer_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	## PLOTS
		plot_pid_closer_nomodel_stereotypeT2closer_dem <- ggplot( data= avdiff_closer_stereotype_T2closer_dem ) + scale_x_continuous( limits=c(0, length( avdiff_closer_stereotype_T2closer_dem$shift )+ 1 ), name="", labels = avdiff_closer_dem$Closer, breaks = c( 1 : length( avdiff_closer_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("DEMOCRATIC CANDIDATE", subtitle ="Stereotype Consistent Candidate Shifts ONLY")
		plot_pid_closer_nomodel_stereotypeT2closer_dem <- plot_pid_closer_nomodel_stereotypeT2closer_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_pid_closer_nomodel_stereotypeT2closer_dem <- plot_pid_closer_nomodel_stereotypeT2closer_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_pid_closer_nomodel_stereotypeT2closer_dem <- plot_pid_closer_nomodel_stereotypeT2closer_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "blue" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_pid_closer_nomodel_stereotypeT2closer_dem
		ggsave("figA5_dem.pdf", plot = plot_pid_closer_nomodel_stereotypeT2closer_dem, width = 7.5 , height = 4  )

		plot_pid_closer_nomodel_stereotypeT2closer_rep <- ggplot( data= avdiff_closer_stereotype_T2closer_rep ) + scale_x_continuous( limits=c(0, length( avdiff_closer_stereotype_T2closer_rep$shift )+ 1 ), name="", labels = avdiff_closer_rep$Closer, breaks = c( 1 : length( avdiff_closer_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Stereotype Consistent Candidate Shifts ONLY")
		plot_pid_closer_nomodel_stereotypeT2closer_rep <- plot_pid_closer_nomodel_stereotypeT2closer_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_pid_closer_nomodel_stereotypeT2closer_rep <- plot_pid_closer_nomodel_stereotypeT2closer_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_pid_closer_nomodel_stereotypeT2closer_rep <- plot_pid_closer_nomodel_stereotypeT2closer_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "red" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_pid_closer_nomodel_stereotypeT2closer_rep
		ggsave("figA5_rep.pdf", plot = plot_pid_closer_nomodel_stereotypeT2closer_rep, width = 7.5 , height = 4  )




# ONLY IF T2 IS FARTHER TO THE STEREOTYPE
	avdiff_closer_stereotype_T2farther_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	avdiff_closer_stereotype_T2farther_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		avdiff_closer_stereotype_T2farther_dem$shift [ avdiff_closer_stereotype_T2farther_dem$partyID == "out-partisan (R)" & avdiff_closer_stereotype_T2farther_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2farther_dem$ShiftFromUsedto_dem[ Stereotype_T2farther_dem$Republican == 1 &  Stereotype_T2farther_dem$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_dem$shift [ avdiff_closer_stereotype_T2farther_dem$partyID == "out-partisan (R)" & avdiff_closer_stereotype_T2farther_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2farther_dem$ShiftFromUsedto_dem[ Stereotype_T2farther_dem$Republican == 1 & Stereotype_T2farther_dem$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_dem$shift [ avdiff_closer_stereotype_T2farther_dem$partyID == "in-partisan (D)" & avdiff_closer_stereotype_T2farther_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2farther_dem$ShiftFromUsedto_dem[ Stereotype_T2farther_dem$Democrat == 1 & Stereotype_T2farther_dem$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_dem$shift [ avdiff_closer_stereotype_T2farther_dem$partyID == "in-partisan (D)" & avdiff_closer_stereotype_T2farther_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2farther_dem$ShiftFromUsedto_dem[ Stereotype_T2farther_dem$Democrat == 1 & Stereotype_T2farther_dem$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_stereotype_T2farther_dem$se [ avdiff_closer_stereotype_T2farther_dem$partyID == "out-partisan (R)" & avdiff_closer_stereotype_T2farther_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2farther_dem$ShiftFromUsedto_dem[ Stereotype_T2farther_dem$Republican == 1 &  Stereotype_T2farther_dem$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_dem$se [ avdiff_closer_stereotype_T2farther_dem$partyID == "out-partisan (R)" & avdiff_closer_stereotype_T2farther_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2farther_dem$ShiftFromUsedto_dem[ Stereotype_T2farther_dem$Republican == 1 & Stereotype_T2farther_dem$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_dem$se [ avdiff_closer_stereotype_T2farther_dem$partyID == "in-partisan (D)" & avdiff_closer_stereotype_T2farther_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2farther_dem$ShiftFromUsedto_dem[ Stereotype_T2farther_dem$Democrat == 1 & Stereotype_T2farther_dem$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_dem$se [ avdiff_closer_stereotype_T2farther_dem$partyID == "in-partisan (D)" & avdiff_closer_stereotype_T2farther_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2farther_dem$ShiftFromUsedto_dem[ Stereotype_T2farther_dem$Democrat == 1 & Stereotype_T2farther_dem$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_stereotype_T2farther_dem <- avdiff_closer_stereotype_T2farther_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		avdiff_closer_stereotype_T2farther_rep$shift [ avdiff_closer_stereotype_T2farther_rep$partyID == "in-partisan (R)" & avdiff_closer_stereotype_T2farther_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2farther_rep$ShiftFromUsedto_rep[ Stereotype_T2farther_rep$Republican == 1 &  Stereotype_T2farther_rep$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_rep$shift [ avdiff_closer_stereotype_T2farther_rep$partyID == "in-partisan (R)" & avdiff_closer_stereotype_T2farther_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2farther_rep$ShiftFromUsedto_rep[ Stereotype_T2farther_rep$Republican == 1 & Stereotype_T2farther_rep$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_rep$shift [ avdiff_closer_stereotype_T2farther_rep$partyID == "out-partisan (D)" & avdiff_closer_stereotype_T2farther_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2farther_rep$ShiftFromUsedto_rep[ Stereotype_T2farther_rep$Democrat == 1 & Stereotype_T2farther_rep$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_rep$shift [ avdiff_closer_stereotype_T2farther_rep$partyID == "out-partisan (D)" & avdiff_closer_stereotype_T2farther_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( Stereotype_T2farther_rep$ShiftFromUsedto_rep[ Stereotype_T2farther_rep$Democrat == 1 & Stereotype_T2farther_rep$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_stereotype_T2farther_rep$se [ avdiff_closer_stereotype_T2farther_rep$partyID == "in-partisan (R)" & avdiff_closer_stereotype_T2farther_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2farther_rep$ShiftFromUsedto_rep[ Stereotype_T2farther_rep$Republican == 1 &  Stereotype_T2farther_rep$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_rep$se [ avdiff_closer_stereotype_T2farther_rep$partyID == "in-partisan (R)" & avdiff_closer_stereotype_T2farther_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2farther_rep$ShiftFromUsedto_rep[ Stereotype_T2farther_rep$Republican == 1 & Stereotype_T2farther_rep$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_rep$se [ avdiff_closer_stereotype_T2farther_rep$partyID == "out-partisan (D)" & avdiff_closer_stereotype_T2farther_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2farther_rep$ShiftFromUsedto_rep[ Stereotype_T2farther_rep$Democrat == 1 & Stereotype_T2farther_rep$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avdiff_closer_stereotype_T2farther_rep$se [ avdiff_closer_stereotype_T2farther_rep$partyID == "out-partisan (D)" & avdiff_closer_stereotype_T2farther_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( Stereotype_T2farther_rep$ShiftFromUsedto_rep[ Stereotype_T2farther_rep$Democrat == 1 & Stereotype_T2farther_rep$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avdiff_closer_stereotype_T2farther_rep <- avdiff_closer_stereotype_T2farther_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	## PLOTS
		plot_pid_closer_nomodel_stereotypeT2farther_dem <- ggplot( data= avdiff_closer_stereotype_T2farther_dem ) + scale_x_continuous( limits=c(0, length( avdiff_closer_stereotype_T2farther_dem$shift )+ 1 ), name="", labels = avdiff_closer_dem$Closer, breaks = c( 1 : length( avdiff_closer_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2\nmaximum\ndifference" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "Stereotype INconsistent Candidate Shifts ONLY")
		plot_pid_closer_nomodel_stereotypeT2farther_dem <- plot_pid_closer_nomodel_stereotypeT2farther_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_pid_closer_nomodel_stereotypeT2farther_dem <- plot_pid_closer_nomodel_stereotypeT2farther_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_pid_closer_nomodel_stereotypeT2farther_dem <- plot_pid_closer_nomodel_stereotypeT2farther_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "blue" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_pid_closer_nomodel_stereotypeT2farther_dem
		ggsave("figA4_dem.pdf", plot = plot_pid_closer_nomodel_stereotypeT2farther_dem, width = 7.5 , height = 4  )

		plot_pid_closer_nomodel_stereotypeT2farther_rep <- ggplot( data= avdiff_closer_stereotype_T2farther_rep ) + scale_x_continuous( limits=c(0, length( avdiff_closer_stereotype_T2farther_rep$shift )+ 1 ), name="", labels = avdiff_closer_rep$Closer, breaks = c( 1 : length( avdiff_closer_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2\nmaximum\ndifference" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL) + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Stereotype INconsistent Candidate Shifts ONLY")
		plot_pid_closer_nomodel_stereotypeT2farther_rep <- plot_pid_closer_nomodel_stereotypeT2farther_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_pid_closer_nomodel_stereotypeT2farther_rep <- plot_pid_closer_nomodel_stereotypeT2farther_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_pid_closer_nomodel_stereotypeT2farther_rep <- plot_pid_closer_nomodel_stereotypeT2farther_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "red" )) + guides( color = guide_legend( title = "Respondent" ) )
		plot_pid_closer_nomodel_stereotypeT2farther_rep
		ggsave("figA4_rep.pdf", plot = plot_pid_closer_nomodel_stereotypeT2farther_rep, width = 7.5 , height = 4  )




############## FIGURE A6

	plot_dist_placement_stereotype_dem <-  ggplot( data = cces_unmatched ) + ggtitle("Distance between PERCEIVED CANDIDATE PLACEMENT and PARTY STEREOTYPE", subtitle = "Democratic Candidate") + theme( plot.subtitle = element_text( color = "blue" ) )
	plot_dist_placement_stereotype_dem <- plot_dist_placement_stereotype_dem + geom_bar( aes( x = Dist_Placement_Stereotype_dem ) ) + scale_x_continuous( breaks = c( -4:4 ), labels = c( "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4" ), name = "Distance between PERCEIVED CANDIDATE PLACEMENT and PARTY STEREOTYPE" )
	plot_dist_placement_stereotype_dem
	ggsave(filename= "figA6_dem.pdf", plot = plot_dist_placement_stereotype_dem, width = 8 , height = 6  )
	
	plot_dist_placement_stereotype_rep <-  ggplot( data = cces_unmatched ) + ggtitle("Distance between PERCEIVED CANDIDATE PLACEMENT and PARTY STEREOTYPE", subtitle = "Republican Candidate") + theme( plot.subtitle = element_text( color = "red" ) )
	plot_dist_placement_stereotype_rep <- plot_dist_placement_stereotype_rep + geom_bar( aes( x = Dist_Placement_Stereotype_rep ) ) + scale_x_continuous( breaks = c( -4:4 ), labels = c( "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4" ), name = "Distance between PERCEIVED CANDIDATE PLACEMENT and PARTY STEREOTYPE" )
	plot_dist_placement_stereotype_rep
	ggsave(filename= "figA6_rep.pdf", plot = plot_dist_placement_stereotype_rep, width = 8 , height = 6  )


############## FIGURE A7

	plot_dist_T2_stereotype_dem <-  ggplot( data = cces_unmatched ) + ggtitle("Distance between T2 CANDIDATE STANCE and PARTY STEREOTYPE", subtitle = "Democratic Candidate") + theme( plot.subtitle = element_text( color = "blue" ) )
	plot_dist_T2_stereotype_dem <- plot_dist_T2_stereotype_dem + scale_x_continuous( breaks = c( -4:4 ), labels = c( "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4" ), name = "Distance between T2 CANDIDATE STANCE and PARTY STEREOTYPE" )
	plot_dist_T2_stereotype_dem <- plot_dist_T2_stereotype_dem + geom_bar( aes( x = Dist_T2_Stereotype_dem ) )
	plot_dist_T2_stereotype_dem
	ggsave(filename= "figA7_dem.pdf", plot = plot_dist_T2_stereotype_dem, width = 8 , height = 6  )

	plot_dist_T2_stereotype_rep <-  ggplot( data = cces_unmatched ) + ggtitle("Distance between T2 CANDIDATE STANCE and PARTY STEREOTYPE", subtitle = "Republican Candidate") + theme( plot.subtitle = element_text( color = "red" ) )
	plot_dist_T2_stereotype_rep <- plot_dist_T2_stereotype_rep + scale_x_continuous( breaks = c( -4:4 ), labels = c( "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4" ), name = "Distance between T2 CANDIDATE STANCE and PARTY STEREOTYPE" )
	plot_dist_T2_stereotype_rep <- plot_dist_T2_stereotype_rep + geom_bar( aes( x = Dist_T2_Stereotype_rep ) )
	plot_dist_T2_stereotype_rep
	ggsave(filename= "figA7_rep.pdf", plot = plot_dist_T2_stereotype_rep, width = 8 , height = 6  )


############## FIGURE A8

			plot_prop_dist_withStereotype_dem <-  ggplot( data = cces_unmatched ) + ggtitle("Ratio Difference Candidate Perception and Stereotype over Difference Stance and Stereotype", subtitle = "DEMOCRATIC CANDIDATE") + theme( plot.subtitle = element_text( color = "blue" ) )
			plot_prop_dist_withStereotype_dem <- plot_prop_dist_withStereotype_dem + geom_histogram( aes( x = prop_dist_withStereotype_dem ) ) + scale_x_continuous( breaks = c( -8:8 ), name = "Ratio Difference Candidate Perception and Stereotype over Difference Stance and Stereotype" )
			plot_prop_dist_withStereotype_dem
			ggsave(filename= "figA8_dem.pdf", plot = plot_prop_dist_withStereotype_dem, width = 8.5 , height = 6  )

			plot_prop_dist_withStereotype_rep <-  ggplot( data = cces_unmatched ) + ggtitle("Ratio Difference Candidate Perception and Stereotype over Difference Stance and Stereotype", subtitle = "REPUBLICAN CANDIDATE") + theme( plot.subtitle = element_text( color = "red" ) )
			plot_prop_dist_withStereotype_rep <- plot_prop_dist_withStereotype_rep + geom_histogram( aes( x = prop_dist_withStereotype_rep ) ) + scale_x_continuous( breaks = c( -8:8 ), name = "Ratio Difference Candidate Perception and Stereotype over Difference Stance and Stereotype" )
			plot_prop_dist_withStereotype_rep
			ggsave(filename= "figA8_rep.pdf", plot = plot_prop_dist_withStereotype_rep, width = 8.5 , height = 6  )


############## FIGURE A9

#  	Outcome is the proportion of the difference between PERCEIVED CANDIDATE PLACEMENT and STEREOTYPE about the PARTY and the DIFFERENCE between T2 and the PARTY STEREOTYPE
#	The conditioning variable is T2CloserToStereotype_dem/_rep

	prop_dist_stereotype_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	prop_dist_stereotype_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		prop_dist_stereotype_dem$shift [ prop_dist_stereotype_dem$partyID == "out-partisan (R)" & prop_dist_stereotype_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$prop_dist_withStereotype_dem[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		prop_dist_stereotype_dem$shift [ prop_dist_stereotype_dem$partyID == "out-partisan (R)" & prop_dist_stereotype_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$prop_dist_withStereotype_dem[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		prop_dist_stereotype_dem$shift [ prop_dist_stereotype_dem$partyID == "in-partisan (D)" & prop_dist_stereotype_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$prop_dist_withStereotype_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		prop_dist_stereotype_dem$shift [ prop_dist_stereotype_dem$partyID == "in-partisan (D)" & prop_dist_stereotype_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$prop_dist_withStereotype_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		prop_dist_stereotype_dem$se [ prop_dist_stereotype_dem$partyID == "out-partisan (R)" & prop_dist_stereotype_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$prop_dist_withStereotype_dem[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		prop_dist_stereotype_dem$se [ prop_dist_stereotype_dem$partyID == "out-partisan (R)" & prop_dist_stereotype_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$prop_dist_withStereotype_dem[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		prop_dist_stereotype_dem$se [ prop_dist_stereotype_dem$partyID == "in-partisan (D)" & prop_dist_stereotype_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$prop_dist_withStereotype_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		prop_dist_stereotype_dem$se [ prop_dist_stereotype_dem$partyID == "in-partisan (D)" & prop_dist_stereotype_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$prop_dist_withStereotype_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		prop_dist_stereotype_dem <- prop_dist_stereotype_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		prop_dist_stereotype_rep$shift [ prop_dist_stereotype_rep$partyID == "in-partisan (R)" & prop_dist_stereotype_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$prop_dist_withStereotype_rep[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		prop_dist_stereotype_rep$shift [ prop_dist_stereotype_rep$partyID == "in-partisan (R)" & prop_dist_stereotype_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$prop_dist_withStereotype_rep[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		prop_dist_stereotype_rep$shift [ prop_dist_stereotype_rep$partyID == "out-partisan (D)" & prop_dist_stereotype_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$prop_dist_withStereotype_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		prop_dist_stereotype_rep$shift [ prop_dist_stereotype_rep$partyID == "out-partisan (D)" & prop_dist_stereotype_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$prop_dist_withStereotype_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		prop_dist_stereotype_rep$se [ prop_dist_stereotype_rep$partyID == "in-partisan (R)" & prop_dist_stereotype_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$prop_dist_withStereotype_rep[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		prop_dist_stereotype_rep$se [ prop_dist_stereotype_rep$partyID == "in-partisan (R)" & prop_dist_stereotype_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$prop_dist_withStereotype_rep[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		prop_dist_stereotype_rep$se [ prop_dist_stereotype_rep$partyID == "out-partisan (D)" & prop_dist_stereotype_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$prop_dist_withStereotype_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		prop_dist_stereotype_rep$se [ prop_dist_stereotype_rep$partyID == "out-partisan (D)" & prop_dist_stereotype_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$prop_dist_withStereotype_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		prop_dist_stereotype_rep <- prop_dist_stereotype_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	## PLOTS
		plot_prop_dist_stereotype_dem <- ggplot( data= prop_dist_stereotype_dem ) + scale_x_continuous( limits=c(0, length( prop_dist_stereotype_dem$shift )+ 1 ), name="", labels = prop_dist_stereotype_dem$Closer, breaks = c( 1 : length( prop_dist_stereotype_dem$up ) ) ) 
		plot_prop_dist_stereotype_dem <- plot_prop_dist_stereotype_dem + scale_y_continuous(limits=c( 0 , 2 ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION AND PARTY STEREOTYPE\nRELATIVE TO DIFFERENCE BETWEEN SECOND CANDIDATE STANCE AND PARTY STEREOTYPE") + xlab(NULL) + ggtitle("DEMOCRATIC CANDIDATE")
		plot_prop_dist_stereotype_dem <- plot_prop_dist_stereotype_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_prop_dist_stereotype_dem <- plot_prop_dist_stereotype_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_prop_dist_stereotype_dem <- plot_prop_dist_stereotype_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "blue" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_prop_dist_stereotype_dem
		ggsave("figA9_dem.pdf", plot = plot_prop_dist_stereotype_dem, width = 7.5 , height = 4  )

		plot_prop_dist_stereotype_rep <- ggplot( data= prop_dist_stereotype_rep ) + scale_x_continuous( limits=c(0, length( prop_dist_stereotype_rep$shift )+ 1 ), name="", labels = prop_dist_stereotype_rep$Closer, breaks = c( 1 : length( prop_dist_stereotype_rep$up ) ) ) 
		plot_prop_dist_stereotype_rep <- plot_prop_dist_stereotype_rep + scale_y_continuous(limits=c( 0 , 2 ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION AND PARTY STEREOTYPE\nRELATIVE TO DIFFERENCE BETWEEN SECOND CANDIDATE STANCE AND PARTY STEREOTYPE") + xlab(NULL) + ggtitle("REPUBLICAN CANDIDATE")
		plot_prop_dist_stereotype_rep <- plot_prop_dist_stereotype_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_prop_dist_stereotype_rep <- plot_prop_dist_stereotype_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_prop_dist_stereotype_rep <- plot_prop_dist_stereotype_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "red" )) + guides( color = guide_legend( title = "Respondent" ) )
		plot_prop_dist_stereotype_rep
		ggsave("figA9_rep.pdf", plot = plot_prop_dist_stereotype_rep, width = 7.5 , height = 4  )



#####################	TABLE AI

	unm_dem <- lm( UCMwagecandidatedem_num ~  T1dem_rev_num + ShiftT2T1_dem + UCMwagepref_num + UCMwagedem_num, data = cces_unmatched )
	unm_rep <- lm( UCMwagecandidaterep_num ~  T1rep_rev_num + ShiftT2T1_rep + UCMwagepref_num + UCMwagerep_num, data = cces_unmatched )
	
		stargazer(unm_dem, unm_rep, title="Regression Results. Baseline model.",
		align=TRUE, dep.var.labels= NULL , covariate.labels = c( "Stance (t-1)", "Policy Shift", "Stance (t-1)", "Policy Shift", "Voter Position", "Perception of D/R voter",  "Perception of D/R voter"),	
		omit.stat=c("LL","ser","f", "adj.rsq", "rsq"), no.space=TRUE, digits = 1, star.cutoffs = c( 0.05 ) )



#####################	TABLE AII
 
	partisans_unm <- subset (cces_unmatched, pid3lean != 0 )	

	partisanship_simple_noindeps_unm_dem <- lm( UCMwagecandidatedem_num ~  T1dem_rev_num + ShiftT2T1_dem * Democrat + UCMwagepref_num + UCMwagedem_num, data = partisans_unm )
	summary( partisanship_simple_noindeps_unm_dem )

	partisanship_simple_noindeps_unm_rep <- lm( UCMwagecandidaterep_num ~  T1rep_rev_num + ShiftT2T1_rep * Republican + UCMwagepref_num + UCMwagerep_num, data = partisans_unm )
	summary( partisanship_simple_noindeps_unm_rep )

	## Regression Table
		stargazer(partisanship_simple_noindeps_unm_dem, partisanship_simple_noindeps_unm_rep, title="Regression Results. Testing for Partisan Motivation.",
		align=TRUE, dep.var.labels= NULL, covariate.labels = c( "Stance (t-1)", "Policy Shift", "Democrat", "Stance (t-1)", "Policy Shift", "Republican", "Voter Position", "Perception of D/R voter", "Policy Shift * Democrat",  "Perception of D/R voter", "Policy Shift * Republican") ,
		omit.stat=c("LL","ser","f", "adj.rsq", "rsq"), no.space=TRUE, digits = 1, star.cutoffs = c( 0.05 ) )



#####################	FIGURE A10 : REGRESSION MODEL. 

	## Calculating MARGINAL EFFECTS
		mar_partisanship_simple_noindeps_unm_dem <- margins ( partisanship_simple_noindeps_unm_dem, at = list ( Democrat = 0:1 ) ) 
		summary( mar_partisanship_simple_noindeps_unm_dem )

		mar_partisanship_simple_noindeps_unm_rep <- margins ( partisanship_simple_noindeps_unm_rep, at = list ( Republican = 0:1 ) ) 
		summary( mar_partisanship_simple_noindeps_unm_rep )

	# Extracting data set with marginal effects
		dem_unmresults_noindeps_pid <- as.data.frame ( summary( mar_partisanship_simple_noindeps_unm_dem )[3:4,] )
		rep_unmresults_noindeps_pid <- as.data.frame ( summary( mar_partisanship_simple_noindeps_unm_rep )[3:4,] )

		dem_unmresults_noindeps_pid <- dem_unmresults_noindeps_pid %>% mutate ( PartyID = factor ( Democrat, levels = c(1, 0 ), labels = c ( "in-partisan (D)", "out-partisan (R)" ) ) )
		rep_unmresults_noindeps_pid <- rep_unmresults_noindeps_pid %>% mutate ( PartyID = factor ( Republican, levels = c(1, 0 ), labels = c ( "in-partisan (R)", "out-partisan (D)" ) ) )

	# PLOTS 
		dem_unm_pid_noindeps <- ggplot( data= dem_unmresults_noindeps_pid ) + scale_x_continuous( limits=c( 0, length( dem_unmresults_noindeps_pid$upper ) + 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous(limits=c(0, 0.5 )) + ylab("Estimated Marginal Effect of a Candidate Position Shift") + xlab(NULL)
		dem_unm_pid_noindeps <- dem_unm_pid_noindeps + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "DV: perceived candidate position") + theme( plot.title = element_text( color = "blue" ) )
		dem_unm_pid_noindeps <- dem_unm_pid_noindeps + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 1 : length(upper) ), color = PartyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		dem_unm_pid_noindeps <- dem_unm_pid_noindeps + geom_point( aes ( y = AME , x = c( 1 : length(upper) ), color = PartyID), size=3 , alpha = .75 )
		dem_unm_pid_noindeps <- dem_unm_pid_noindeps + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "Respondent" ) )
		dem_unm_pid_noindeps
		ggsave("figA10_dem.pdf", plot = dem_unm_pid_noindeps, width = 7.5 , height = 4 )

		rep_unm_pid_noindeps <- ggplot( data= rep_unmresults_noindeps_pid ) + scale_x_continuous( limits=c( 0, length( rep_unmresults_noindeps_pid$upper ) + 1 ), name="", labels = NULL , breaks = NULL ) + scale_y_continuous(limits=c(0, 0.5 )) + ylab("Estimated Marginal Effect of a Candidate Position Shift") + xlab(NULL)
		rep_unm_pid_noindeps <- rep_unm_pid_noindeps + ggtitle("REPUBLICAN CANDIDATE", subtitle = "DV: perceived candidate position") + theme( plot.title = element_text( color = "red" ) )
		rep_unm_pid_noindeps <- rep_unm_pid_noindeps + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 1 : length(upper) ), color = PartyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		rep_unm_pid_noindeps <- rep_unm_pid_noindeps + geom_point( aes ( y = AME , x = c( 1 : length(upper) ), color = PartyID), size=3 , alpha = .75 )
		rep_unm_pid_noindeps <- rep_unm_pid_noindeps + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "Respondent" ) )
		rep_unm_pid_noindeps
		ggsave("figA10_rep.pdf", plot = rep_unm_pid_noindeps, width = 7.5 , height = 4 )





##################### TABLE AIII

	pid_closerto_dem <- lm( UCMwagecandidatedem_num ~ T1dem_rev_num + ShiftT2T1_dem * Democrat * T2CloserToRespondent_dem + UCMwagepref_num + UCMwagedem_num, data = noindep_unm )
	summary( pid_closerto_dem )

	pid_closerto_rep <- lm( UCMwagecandidaterep_num ~ T1rep_rev_num + ShiftT2T1_rep * Republican * T2CloserToRespondent_rep + UCMwagepref_num + UCMwagerep_num, data = noindep_unm )
	summary( pid_closerto_rep )

		## Regression Table
		stargazer(pid_closerto_dem, pid_closerto_rep, title="Regression Results. Testing for Preference Mediated Partisan Motivation.",
		align=TRUE, dep.var.labels= NULL, covariate.labels = c( "Stance (t-1)", "Policy Shift", "Democrat", "Closer to Respondent", "Stance (t-1)", "Policy Shift", "Republican", "Closer to Respondent", "Voter Position", "Perception of D/R voter", "Policy Shift * Democrat", "Policy Shift * Closer to Respondent", "Democrat * Closer to Respondent", "Policy Shift * Democrat * Closer to Respondent" , "Perception of D/R voter", "Policy Shift * Republican", "Policy Shift * Closer to Respondent", "Republican * Closer to Respondent", "Policy Shift * Republican * Closer to Respondent") ,
		omit.stat=c("LL","ser","f", "adj.rsq", "rsq"), no.space=TRUE, digits = 1, star.cutoffs = c( 0.05 ) )


#####################	FIGURE A11 : REGRESSION MODEL. Testing for Preference Mediated Partisan Motivation.


	## Calculating MARGINAL EFFECTS
		marg_pid_closerto_dem <- margins ( pid_closerto_dem, at = list ( Democrat = 0:1, T2CloserToRespondent_dem = 0:1 ) ) 
		summary( marg_pid_closerto_dem )

		marg_pid_closerto_rep <- margins ( pid_closerto_rep, at = list ( Republican = 0:1, T2CloserToRespondent_rep = 0:1 ) ) 
		summary( marg_pid_closerto_rep )

		summary_marg_pid_closerto_dem <- as.data.frame ( summary( marg_pid_closerto_dem )[5:8,] )
		summary_marg_pid_closerto_rep <- as.data.frame ( summary( marg_pid_closerto_rep )[5:8,] )

		summary_marg_pid_closerto_dem <- summary_marg_pid_closerto_dem %>% mutate ( PartyID = factor ( Democrat, levels = c(1, 0 ), labels = c (  "in-partisan (D)", "out-partisan (R)" ) ), Direction= factor( T2CloserToRespondent_dem, levels = c( 0, 1 ), labels = c( "FARTHER FROM\nRESPONDENT", "CLOSER TO\nRESPONDENT") ) )
		summary_marg_pid_closerto_rep <- summary_marg_pid_closerto_rep %>% mutate ( PartyID = factor ( Republican, levels = c(1, 0 ), labels = c ( "in-partisan (R)", "out-partisan (D)"  ) ), Direction= factor( T2CloserToRespondent_rep, levels = c( 0, 1 ), labels = c( "FARTHER FROM\nRESPONDENT", "CLOSER TO\nRESPONDENT") ) )

	# PLOTS
		plot_pid_closerto_dem <- ggplot( data= summary_marg_pid_closerto_dem ) + scale_x_continuous( limits=c(0,5), name="", labels = summary_marg_pid_closerto_dem$Direction, breaks = c( 1 : length( summary_marg_pid_closerto_dem$upper ) ) ) + scale_y_continuous(limits=c(0 , 0.5 ) ) + ylab("Estimated Marginal Effect of a Candidate Position Shift") + xlab(NULL)
		plot_pid_closerto_dem <- plot_pid_closerto_dem + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "DV: perceived candidate position") + theme( plot.title = element_text( color = "blue" ) )
		plot_pid_closerto_dem <- plot_pid_closerto_dem + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 1 : length(upper) ), color = PartyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_pid_closerto_dem <- plot_pid_closerto_dem + geom_point( aes ( y = AME , x = c( 1 : length(upper) ), color = PartyID), size=3 , alpha = .75 )
		plot_pid_closerto_dem <- plot_pid_closerto_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "Respondent" ) )
		plot_pid_closerto_dem
		ggsave("figA11_dem.pdf", plot = plot_pid_closerto_dem, width = 7.5 , height = 4 )

		plot_pid_closerto_rep <- ggplot( data= summary_marg_pid_closerto_rep) + scale_x_continuous( limits=c(0,5), name="", labels = summary_marg_pid_closerto_rep$Direction, breaks = c( 1 : length( summary_marg_pid_closerto_rep$upper ) ) ) + scale_y_continuous(limits=c(0, 0.5 )) + ylab("Estimated Marginal Effect of a Candidate Position Shift") + xlab(NULL)
		plot_pid_closerto_rep <- plot_pid_closerto_rep + ggtitle("REPUBLICAN CANDIDATE", subtitle = "DV: perceived candidate position") + theme( plot.title = element_text( color = "red" ) )
		plot_pid_closerto_rep <- plot_pid_closerto_rep + geom_linerange(aes( ymin = lower, ymax = upper, x = c( 1 : length(upper) ), color = PartyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_pid_closerto_rep <- plot_pid_closerto_rep + geom_point( aes ( y = AME , x = c( 1 : length(upper) ), color = PartyID), size=3 , alpha = .75 )
		plot_pid_closerto_rep <- plot_pid_closerto_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0")) + guides( color = guide_legend( title = "Respondent" ) )
		plot_pid_closerto_rep
		ggsave("figA11_rep.pdf", plot = plot_pid_closerto_rep, width = 7.5 , height = 4 )






####################### FIGURES A12-A16



############ Addressing the endogeneity of variable moving towards/away to/from respondent

## Strategy: Replicate FIG 8 but only among respondents (of either party) that share the same position, so that move away/towards does not correlate with the preference of the individual any more.

	
### Getting rid of independents
	noindep_unm <- subset( cces_unmatched, Independent == 0 )


### ONLY THOSE THAT HAVE A PREFERENCE FOR $7.25 
	seven_twentyfive_partisans <- subset( noindep_unm, UCMwagepref_num == 1 )

# Plot distribution of partisanship among those that want $7.25
	seven_twentyfive_partisans_pidPLOT <- ggplot( data = seven_twentyfive_partisans ) 
	seven_twentyfive_partisans_pidPLOT <- seven_twentyfive_partisans_pidPLOT + ggtitle("partisanship", subtitle = "Respondents who prefer $7.25 min wage")
	seven_twentyfive_partisans_pidPLOT <- seven_twentyfive_partisans_pidPLOT + geom_bar( aes( x = Democrat ) )
	seven_twentyfive_partisans_pidPLOT

	seven_twentyfive_partisans_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	seven_twentyfive_partisans_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		seven_twentyfive_partisans_dem$shift [ seven_twentyfive_partisans_dem$partyID == "out-partisan (R)" & seven_twentyfive_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( seven_twentyfive_partisans$ShiftFromUsedto_dem[ seven_twentyfive_partisans$Republican == 1 &  seven_twentyfive_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_dem$shift [ seven_twentyfive_partisans_dem$partyID == "out-partisan (R)" & seven_twentyfive_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( seven_twentyfive_partisans$ShiftFromUsedto_dem[ seven_twentyfive_partisans$Republican == 1 & seven_twentyfive_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_dem$shift [ seven_twentyfive_partisans_dem$partyID == "in-partisan (D)" & seven_twentyfive_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( seven_twentyfive_partisans$ShiftFromUsedto_dem[ seven_twentyfive_partisans$Democrat == 1 & seven_twentyfive_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_dem$shift [ seven_twentyfive_partisans_dem$partyID == "in-partisan (D)" & seven_twentyfive_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( seven_twentyfive_partisans$ShiftFromUsedto_dem[ seven_twentyfive_partisans$Democrat == 1 & seven_twentyfive_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		seven_twentyfive_partisans_dem$se [ seven_twentyfive_partisans_dem$partyID == "out-partisan (R)" & seven_twentyfive_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( seven_twentyfive_partisans$ShiftFromUsedto_dem[ seven_twentyfive_partisans$Republican == 1 &  seven_twentyfive_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_dem$se [ seven_twentyfive_partisans_dem$partyID == "out-partisan (R)" & seven_twentyfive_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( seven_twentyfive_partisans$ShiftFromUsedto_dem[ seven_twentyfive_partisans$Republican == 1 & seven_twentyfive_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_dem$se [ seven_twentyfive_partisans_dem$partyID == "in-partisan (D)" & seven_twentyfive_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( seven_twentyfive_partisans$ShiftFromUsedto_dem[ seven_twentyfive_partisans$Democrat == 1 & seven_twentyfive_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_dem$se [ seven_twentyfive_partisans_dem$partyID == "in-partisan (D)" & seven_twentyfive_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( seven_twentyfive_partisans$ShiftFromUsedto_dem[ seven_twentyfive_partisans$Democrat == 1 & seven_twentyfive_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		seven_twentyfive_partisans_dem <- seven_twentyfive_partisans_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		seven_twentyfive_partisans_rep$shift [ seven_twentyfive_partisans_rep$partyID == "in-partisan (R)" & seven_twentyfive_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( seven_twentyfive_partisans$ShiftFromUsedto_rep[ seven_twentyfive_partisans$Republican == 1 &  seven_twentyfive_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_rep$shift [ seven_twentyfive_partisans_rep$partyID == "in-partisan (R)" & seven_twentyfive_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( seven_twentyfive_partisans$ShiftFromUsedto_rep[ seven_twentyfive_partisans$Republican == 1 & seven_twentyfive_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_rep$shift [ seven_twentyfive_partisans_rep$partyID == "out-partisan (D)" & seven_twentyfive_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( seven_twentyfive_partisans$ShiftFromUsedto_rep[ seven_twentyfive_partisans$Democrat == 1 & seven_twentyfive_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_rep$shift [ seven_twentyfive_partisans_rep$partyID == "out-partisan (D)" & seven_twentyfive_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( seven_twentyfive_partisans$ShiftFromUsedto_rep[ seven_twentyfive_partisans$Democrat == 1 & seven_twentyfive_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		seven_twentyfive_partisans_rep$se [ seven_twentyfive_partisans_rep$partyID == "in-partisan (R)" & seven_twentyfive_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( seven_twentyfive_partisans$ShiftFromUsedto_rep[ seven_twentyfive_partisans$Republican == 1 &  seven_twentyfive_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_rep$se [ seven_twentyfive_partisans_rep$partyID == "in-partisan (R)" & seven_twentyfive_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( seven_twentyfive_partisans$ShiftFromUsedto_rep[ seven_twentyfive_partisans$Republican == 1 & seven_twentyfive_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_rep$se [ seven_twentyfive_partisans_rep$partyID == "out-partisan (D)" & seven_twentyfive_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( seven_twentyfive_partisans$ShiftFromUsedto_rep[ seven_twentyfive_partisans$Democrat == 1 & seven_twentyfive_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		seven_twentyfive_partisans_rep$se [ seven_twentyfive_partisans_rep$partyID == "out-partisan (D)" & seven_twentyfive_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( seven_twentyfive_partisans$ShiftFromUsedto_rep[ seven_twentyfive_partisans$Democrat == 1 & seven_twentyfive_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		seven_twentyfive_partisans_rep <- seven_twentyfive_partisans_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	## PLOTS
		plot_seven_twentyfive_partisans_dem <- ggplot( data= seven_twentyfive_partisans_dem ) + scale_x_continuous( limits=c(0, length( seven_twentyfive_partisans_dem$shift )+ 1 ), name="", labels = seven_twentyfive_partisans_dem$Closer, breaks = c( 1 : length( seven_twentyfive_partisans_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2.5 ), labels = c( "0", "0.5", "1", "1.5", "2", "" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL)
		plot_seven_twentyfive_partisans_dem <- plot_seven_twentyfive_partisans_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_seven_twentyfive_partisans_dem <- plot_seven_twentyfive_partisans_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_seven_twentyfive_partisans_dem <- plot_seven_twentyfive_partisans_dem + coord_flip() + theme( plot.title = element_text( color = "blue" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_seven_twentyfive_partisans_dem <- plot_seven_twentyfive_partisans_dem + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "Only respondents who prefer $7.25 wage" )
		plot_seven_twentyfive_partisans_dem
		ggsave("figA16_dem.pdf", plot = plot_seven_twentyfive_partisans_dem, width = 7.5 , height = 4.5  )

		plot_seven_twentyfive_partisans_rep <- ggplot( data= seven_twentyfive_partisans_rep ) + scale_x_continuous( limits=c(0, length( seven_twentyfive_partisans_rep$shift )+ 1 ), name="", labels = seven_twentyfive_partisans_rep$Closer, breaks = c( 1 : length( seven_twentyfive_partisans_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2.5 ), labels = c( "0", "0.5", "1", "1.5", "2", "" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL)
		plot_seven_twentyfive_partisans_rep <- plot_seven_twentyfive_partisans_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_seven_twentyfive_partisans_rep <- plot_seven_twentyfive_partisans_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_seven_twentyfive_partisans_rep <- plot_seven_twentyfive_partisans_rep + coord_flip() + theme( plot.title = element_text( color = "red" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_seven_twentyfive_partisans_rep <- plot_seven_twentyfive_partisans_rep + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Only respondents who prefer $7.25 wage" )
		plot_seven_twentyfive_partisans_rep
		ggsave("figA16_rep.pdf", plot = plot_seven_twentyfive_partisans_rep, width = 7.5 , height = 4.5  )


### ONLY THOSE THAT HAVE A PREFERENCE FOR $9.50 ( 387 cases )
	nine_fifty_partisans <- subset( noindep_unm, UCMwagepref_num == 2 )

# Plot distribution of partisanship among those that want $9.50
	nine_fifty_partisans_pidPLOT <- ggplot( data = nine_fifty_partisans ) 
	nine_fifty_partisans_pidPLOT <- nine_fifty_partisans_pidPLOT + ggtitle("partisanship", subtitle = "Respondents who prefer $9.50 min wage")
	nine_fifty_partisans_pidPLOT <- nine_fifty_partisans_pidPLOT + geom_bar( aes( x = Democrat ) )
	nine_fifty_partisans_pidPLOT

	nine_fifty_partisans_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	nine_fifty_partisans_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		nine_fifty_partisans_dem$shift [ nine_fifty_partisans_dem$partyID == "out-partisan (R)" & nine_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( nine_fifty_partisans$ShiftFromUsedto_dem[ nine_fifty_partisans$Republican == 1 &  nine_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		nine_fifty_partisans_dem$shift [ nine_fifty_partisans_dem$partyID == "out-partisan (R)" & nine_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( nine_fifty_partisans$ShiftFromUsedto_dem[ nine_fifty_partisans$Republican == 1 & nine_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		nine_fifty_partisans_dem$shift [ nine_fifty_partisans_dem$partyID == "in-partisan (D)" & nine_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( nine_fifty_partisans$ShiftFromUsedto_dem[ nine_fifty_partisans$Democrat == 1 & nine_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		nine_fifty_partisans_dem$shift [ nine_fifty_partisans_dem$partyID == "in-partisan (D)" & nine_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( nine_fifty_partisans$ShiftFromUsedto_dem[ nine_fifty_partisans$Democrat == 1 & nine_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		nine_fifty_partisans_dem$se [ nine_fifty_partisans_dem$partyID == "out-partisan (R)" & nine_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( nine_fifty_partisans$ShiftFromUsedto_dem[ nine_fifty_partisans$Republican == 1 &  nine_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		nine_fifty_partisans_dem$se [ nine_fifty_partisans_dem$partyID == "out-partisan (R)" & nine_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( nine_fifty_partisans$ShiftFromUsedto_dem[ nine_fifty_partisans$Republican == 1 & nine_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		nine_fifty_partisans_dem$se [ nine_fifty_partisans_dem$partyID == "in-partisan (D)" & nine_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( nine_fifty_partisans$ShiftFromUsedto_dem[ nine_fifty_partisans$Democrat == 1 & nine_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		nine_fifty_partisans_dem$se [ nine_fifty_partisans_dem$partyID == "in-partisan (D)" & nine_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( nine_fifty_partisans$ShiftFromUsedto_dem[ nine_fifty_partisans$Democrat == 1 & nine_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		nine_fifty_partisans_dem <- nine_fifty_partisans_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		nine_fifty_partisans_rep$shift [ nine_fifty_partisans_rep$partyID == "in-partisan (R)" & nine_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( nine_fifty_partisans$ShiftFromUsedto_rep[ nine_fifty_partisans$Republican == 1 &  nine_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		nine_fifty_partisans_rep$shift [ nine_fifty_partisans_rep$partyID == "in-partisan (R)" & nine_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( nine_fifty_partisans$ShiftFromUsedto_rep[ nine_fifty_partisans$Republican == 1 & nine_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		nine_fifty_partisans_rep$shift [ nine_fifty_partisans_rep$partyID == "out-partisan (D)" & nine_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( nine_fifty_partisans$ShiftFromUsedto_rep[ nine_fifty_partisans$Democrat == 1 & nine_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		nine_fifty_partisans_rep$shift [ nine_fifty_partisans_rep$partyID == "out-partisan (D)" & nine_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( nine_fifty_partisans$ShiftFromUsedto_rep[ nine_fifty_partisans$Democrat == 1 & nine_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		nine_fifty_partisans_rep$se [ nine_fifty_partisans_rep$partyID == "in-partisan (R)" & nine_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( nine_fifty_partisans$ShiftFromUsedto_rep[ nine_fifty_partisans$Republican == 1 &  nine_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		nine_fifty_partisans_rep$se [ nine_fifty_partisans_rep$partyID == "in-partisan (R)" & nine_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( nine_fifty_partisans$ShiftFromUsedto_rep[ nine_fifty_partisans$Republican == 1 & nine_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		nine_fifty_partisans_rep$se [ nine_fifty_partisans_rep$partyID == "out-partisan (D)" & nine_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( nine_fifty_partisans$ShiftFromUsedto_rep[ nine_fifty_partisans$Democrat == 1 & nine_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		nine_fifty_partisans_rep$se [ nine_fifty_partisans_rep$partyID == "out-partisan (D)" & nine_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( nine_fifty_partisans$ShiftFromUsedto_rep[ nine_fifty_partisans$Democrat == 1 & nine_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		nine_fifty_partisans_rep <- nine_fifty_partisans_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )


	## PLOTS
		plot_nine_fifty_partisans_dem <- ggplot( data= nine_fifty_partisans_dem ) + scale_x_continuous( limits=c(0, length( nine_fifty_partisans_dem$shift )+ 1 ), name="", labels = nine_fifty_partisans_dem$Closer, breaks = c( 1 : length( nine_fifty_partisans_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("Average difference between perception and initial statement") + xlab(NULL)
		plot_nine_fifty_partisans_dem <- plot_nine_fifty_partisans_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_nine_fifty_partisans_dem <- plot_nine_fifty_partisans_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_nine_fifty_partisans_dem <- plot_nine_fifty_partisans_dem + coord_flip() + theme( plot.title = element_text( color = "blue" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_nine_fifty_partisans_dem <- plot_nine_fifty_partisans_dem + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "Only respondents who prefer $9.50 wage" )
		plot_nine_fifty_partisans_dem
		ggsave("figA15_dem.pdf", plot = plot_nine_fifty_partisans_dem, width = 7.5 , height = 4.5  )

		plot_nine_fifty_partisans_rep <- ggplot( data= nine_fifty_partisans_rep ) + scale_x_continuous( limits=c(0, length( nine_fifty_partisans_rep$shift )+ 1 ), name="", labels = nine_fifty_partisans_rep$Closer, breaks = c( 1 : length( nine_fifty_partisans_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2.5 ), labels = c( "0", "0.5", "1", "1.5", "2", "" ) ) + ylab("Average difference between perception and initial statement") + xlab(NULL)
		plot_nine_fifty_partisans_rep <- plot_nine_fifty_partisans_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_nine_fifty_partisans_rep <- plot_nine_fifty_partisans_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_nine_fifty_partisans_rep <- plot_nine_fifty_partisans_rep + coord_flip() + theme( plot.title = element_text( color = "red" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_nine_fifty_partisans_rep <- plot_nine_fifty_partisans_rep + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Only respondents who prefer $9.50 wage" )
		plot_nine_fifty_partisans_rep
		ggsave("figA15_rep.pdf", plot = plot_nine_fifty_partisans_rep, width = 7.5 , height = 4.5  )


### ONLY THOSE THAT HAVE A PREFERENCE FOR $10.50 ( 541 cases )
	ten_fifty_partisans <- subset( noindep_unm, UCMwagepref_num == 3 )

# Plot distribution of partisanship among those that want $10.50
	ten_fifty_partisans_pidPLOT <- ggplot( data = ten_fifty_partisans ) 
	ten_fifty_partisans_pidPLOT <- ten_fifty_partisans_pidPLOT + ggtitle("partisanship", subtitle = "Respondents who prefer $10.50 min wage")
	ten_fifty_partisans_pidPLOT <- ten_fifty_partisans_pidPLOT + geom_bar( aes( x = Democrat ) )
	ten_fifty_partisans_pidPLOT


	ten_fifty_partisans_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	ten_fifty_partisans_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		ten_fifty_partisans_dem$shift [ ten_fifty_partisans_dem$partyID == "out-partisan (R)" & ten_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( ten_fifty_partisans$ShiftFromUsedto_dem[ ten_fifty_partisans$Republican == 1 &  ten_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		ten_fifty_partisans_dem$shift [ ten_fifty_partisans_dem$partyID == "out-partisan (R)" & ten_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( ten_fifty_partisans$ShiftFromUsedto_dem[ ten_fifty_partisans$Republican == 1 & ten_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		ten_fifty_partisans_dem$shift [ ten_fifty_partisans_dem$partyID == "in-partisan (D)" & ten_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( ten_fifty_partisans$ShiftFromUsedto_dem[ ten_fifty_partisans$Democrat == 1 & ten_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		ten_fifty_partisans_dem$shift [ ten_fifty_partisans_dem$partyID == "in-partisan (D)" & ten_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( ten_fifty_partisans$ShiftFromUsedto_dem[ ten_fifty_partisans$Democrat == 1 & ten_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		ten_fifty_partisans_dem$se [ ten_fifty_partisans_dem$partyID == "out-partisan (R)" & ten_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( ten_fifty_partisans$ShiftFromUsedto_dem[ ten_fifty_partisans$Republican == 1 &  ten_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		ten_fifty_partisans_dem$se [ ten_fifty_partisans_dem$partyID == "out-partisan (R)" & ten_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( ten_fifty_partisans$ShiftFromUsedto_dem[ ten_fifty_partisans$Republican == 1 & ten_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		ten_fifty_partisans_dem$se [ ten_fifty_partisans_dem$partyID == "in-partisan (D)" & ten_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( ten_fifty_partisans$ShiftFromUsedto_dem[ ten_fifty_partisans$Democrat == 1 & ten_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		ten_fifty_partisans_dem$se [ ten_fifty_partisans_dem$partyID == "in-partisan (D)" & ten_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( ten_fifty_partisans$ShiftFromUsedto_dem[ ten_fifty_partisans$Democrat == 1 & ten_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		ten_fifty_partisans_dem <- ten_fifty_partisans_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		ten_fifty_partisans_rep$shift [ ten_fifty_partisans_rep$partyID == "in-partisan (R)" & ten_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( ten_fifty_partisans$ShiftFromUsedto_rep[ ten_fifty_partisans$Republican == 1 &  ten_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		ten_fifty_partisans_rep$shift [ ten_fifty_partisans_rep$partyID == "in-partisan (R)" & ten_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( ten_fifty_partisans$ShiftFromUsedto_rep[ ten_fifty_partisans$Republican == 1 & ten_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		ten_fifty_partisans_rep$shift [ ten_fifty_partisans_rep$partyID == "out-partisan (D)" & ten_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( ten_fifty_partisans$ShiftFromUsedto_rep[ ten_fifty_partisans$Democrat == 1 & ten_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		ten_fifty_partisans_rep$shift [ ten_fifty_partisans_rep$partyID == "out-partisan (D)" & ten_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( ten_fifty_partisans$ShiftFromUsedto_rep[ ten_fifty_partisans$Democrat == 1 & ten_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		ten_fifty_partisans_rep$se [ ten_fifty_partisans_rep$partyID == "in-partisan (R)" & ten_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( ten_fifty_partisans$ShiftFromUsedto_rep[ ten_fifty_partisans$Republican == 1 &  ten_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		ten_fifty_partisans_rep$se [ ten_fifty_partisans_rep$partyID == "in-partisan (R)" & ten_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( ten_fifty_partisans$ShiftFromUsedto_rep[ ten_fifty_partisans$Republican == 1 & ten_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		ten_fifty_partisans_rep$se [ ten_fifty_partisans_rep$partyID == "out-partisan (D)" & ten_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( ten_fifty_partisans$ShiftFromUsedto_rep[ ten_fifty_partisans$Democrat == 1 & ten_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		ten_fifty_partisans_rep$se [ ten_fifty_partisans_rep$partyID == "out-partisan (D)" & ten_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( ten_fifty_partisans$ShiftFromUsedto_rep[ ten_fifty_partisans$Democrat == 1 & ten_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		ten_fifty_partisans_rep <- ten_fifty_partisans_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	## PLOTS
		plot_ten_fifty_partisans_dem <- ggplot( data= ten_fifty_partisans_dem ) + scale_x_continuous( limits=c(0, length( ten_fifty_partisans_dem$shift )+ 1 ), name="", labels = ten_fifty_partisans_dem$Closer, breaks = c( 1 : length( ten_fifty_partisans_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL)
		plot_ten_fifty_partisans_dem <- plot_ten_fifty_partisans_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_ten_fifty_partisans_dem <- plot_ten_fifty_partisans_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_ten_fifty_partisans_dem <- plot_ten_fifty_partisans_dem + coord_flip() + theme( plot.title = element_text( color = "blue" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_ten_fifty_partisans_dem <- plot_ten_fifty_partisans_dem + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "Only respondents who prefer $10.50 wage" )
		plot_ten_fifty_partisans_dem
		ggsave("figA14_dem.pdf", plot = plot_ten_fifty_partisans_dem, width = 7.5 , height = 4.5  )

		plot_ten_fifty_partisans_rep <- ggplot( data= ten_fifty_partisans_rep ) + scale_x_continuous( limits=c(0, length( ten_fifty_partisans_rep$shift )+ 1 ), name="", labels = ten_fifty_partisans_rep$Closer, breaks = c( 1 : length( ten_fifty_partisans_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL)
		plot_ten_fifty_partisans_rep <- plot_ten_fifty_partisans_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_ten_fifty_partisans_rep <- plot_ten_fifty_partisans_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_ten_fifty_partisans_rep <- plot_ten_fifty_partisans_rep + coord_flip() + theme( plot.title = element_text( color = "red" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_ten_fifty_partisans_rep <- plot_ten_fifty_partisans_rep + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Only respondents who prefer $10.50 wage" )
		plot_ten_fifty_partisans_rep
		ggsave("figA14_rep.pdf", plot = plot_ten_fifty_partisans_rep, width = 7.5 , height = 4.5  )



### ONLY THOSE THAT HAVE A PREFERENCE FOR $12.50 ( 537 cases )
	twelve_fifty_partisans <- subset( noindep_unm, UCMwagepref_num == 4 )

# Plot distribution of partisanship among those that want $10.50
	twelve_fifty_partisans_pidPLOT <- ggplot( data = twelve_fifty_partisans ) 
	twelve_fifty_partisans_pidPLOT <- twelve_fifty_partisans_pidPLOT + ggtitle("partisanship", subtitle = "Respondents who prefer $12.50 min wage")
	twelve_fifty_partisans_pidPLOT <- twelve_fifty_partisans_pidPLOT + geom_bar( aes( x = Democrat ) )
	twelve_fifty_partisans_pidPLOT

	twelve_fifty_partisans_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	twelve_fifty_partisans_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		twelve_fifty_partisans_dem$shift [ twelve_fifty_partisans_dem$partyID == "out-partisan (R)" & twelve_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( twelve_fifty_partisans$ShiftFromUsedto_dem[ twelve_fifty_partisans$Republican == 1 &  twelve_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		twelve_fifty_partisans_dem$shift [ twelve_fifty_partisans_dem$partyID == "out-partisan (R)" & twelve_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( twelve_fifty_partisans$ShiftFromUsedto_dem[ twelve_fifty_partisans$Republican == 1 & twelve_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		twelve_fifty_partisans_dem$shift [ twelve_fifty_partisans_dem$partyID == "in-partisan (D)" & twelve_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( twelve_fifty_partisans$ShiftFromUsedto_dem[ twelve_fifty_partisans$Democrat == 1 & twelve_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		twelve_fifty_partisans_dem$shift [ twelve_fifty_partisans_dem$partyID == "in-partisan (D)" & twelve_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( twelve_fifty_partisans$ShiftFromUsedto_dem[ twelve_fifty_partisans$Democrat == 1 & twelve_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		twelve_fifty_partisans_dem$se [ twelve_fifty_partisans_dem$partyID == "out-partisan (R)" & twelve_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( twelve_fifty_partisans$ShiftFromUsedto_dem[ twelve_fifty_partisans$Republican == 1 &  twelve_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		twelve_fifty_partisans_dem$se [ twelve_fifty_partisans_dem$partyID == "out-partisan (R)" & twelve_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( twelve_fifty_partisans$ShiftFromUsedto_dem[ twelve_fifty_partisans$Republican == 1 & twelve_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		twelve_fifty_partisans_dem$se [ twelve_fifty_partisans_dem$partyID == "in-partisan (D)" & twelve_fifty_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( twelve_fifty_partisans$ShiftFromUsedto_dem[ twelve_fifty_partisans$Democrat == 1 & twelve_fifty_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		twelve_fifty_partisans_dem$se [ twelve_fifty_partisans_dem$partyID == "in-partisan (D)" & twelve_fifty_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( twelve_fifty_partisans$ShiftFromUsedto_dem[ twelve_fifty_partisans$Democrat == 1 & twelve_fifty_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		twelve_fifty_partisans_dem <- twelve_fifty_partisans_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		twelve_fifty_partisans_rep$shift [ twelve_fifty_partisans_rep$partyID == "in-partisan (R)" & twelve_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( twelve_fifty_partisans$ShiftFromUsedto_rep[ twelve_fifty_partisans$Republican == 1 &  twelve_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		twelve_fifty_partisans_rep$shift [ twelve_fifty_partisans_rep$partyID == "in-partisan (R)" & twelve_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( twelve_fifty_partisans$ShiftFromUsedto_rep[ twelve_fifty_partisans$Republican == 1 & twelve_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		twelve_fifty_partisans_rep$shift [ twelve_fifty_partisans_rep$partyID == "out-partisan (D)" & twelve_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( twelve_fifty_partisans$ShiftFromUsedto_rep[ twelve_fifty_partisans$Democrat == 1 & twelve_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		twelve_fifty_partisans_rep$shift [ twelve_fifty_partisans_rep$partyID == "out-partisan (D)" & twelve_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( twelve_fifty_partisans$ShiftFromUsedto_rep[ twelve_fifty_partisans$Democrat == 1 & twelve_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		twelve_fifty_partisans_rep$se [ twelve_fifty_partisans_rep$partyID == "in-partisan (R)" & twelve_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( twelve_fifty_partisans$ShiftFromUsedto_rep[ twelve_fifty_partisans$Republican == 1 &  twelve_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		twelve_fifty_partisans_rep$se [ twelve_fifty_partisans_rep$partyID == "in-partisan (R)" & twelve_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( twelve_fifty_partisans$ShiftFromUsedto_rep[ twelve_fifty_partisans$Republican == 1 & twelve_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		twelve_fifty_partisans_rep$se [ twelve_fifty_partisans_rep$partyID == "out-partisan (D)" & twelve_fifty_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( twelve_fifty_partisans$ShiftFromUsedto_rep[ twelve_fifty_partisans$Democrat == 1 & twelve_fifty_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		twelve_fifty_partisans_rep$se [ twelve_fifty_partisans_rep$partyID == "out-partisan (D)" & twelve_fifty_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( twelve_fifty_partisans$ShiftFromUsedto_rep[ twelve_fifty_partisans$Democrat == 1 & twelve_fifty_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		twelve_fifty_partisans_rep <- twelve_fifty_partisans_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	## PLOTS
		plot_twelve_fifty_partisans_dem <- ggplot( data= twelve_fifty_partisans_dem ) + scale_x_continuous( limits=c(0, length( twelve_fifty_partisans_dem$shift )+ 1 ), name="", labels = twelve_fifty_partisans_dem$Closer, breaks = c( 1 : length( twelve_fifty_partisans_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL)
		plot_twelve_fifty_partisans_dem <- plot_twelve_fifty_partisans_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_twelve_fifty_partisans_dem <- plot_twelve_fifty_partisans_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_twelve_fifty_partisans_dem <- plot_twelve_fifty_partisans_dem + coord_flip() + theme( plot.title = element_text( color = "blue" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_twelve_fifty_partisans_dem <- plot_twelve_fifty_partisans_dem + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "Only respondents who prefer $12.50 wage" )
		plot_twelve_fifty_partisans_dem
		ggsave("figA13_dem.pdf", plot = plot_twelve_fifty_partisans_dem, width = 7.5 , height = 4.5  )

		plot_twelve_fifty_partisans_rep <- ggplot( data= twelve_fifty_partisans_rep ) + scale_x_continuous( limits=c(0, length( twelve_fifty_partisans_rep$shift )+ 1 ), name="", labels = twelve_fifty_partisans_rep$Closer, breaks = c( 1 : length( twelve_fifty_partisans_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL)
		plot_twelve_fifty_partisans_rep <- plot_twelve_fifty_partisans_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_twelve_fifty_partisans_rep <- plot_twelve_fifty_partisans_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_twelve_fifty_partisans_rep <- plot_twelve_fifty_partisans_rep + coord_flip() + theme( plot.title = element_text( color = "red" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_twelve_fifty_partisans_rep <- plot_twelve_fifty_partisans_rep + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Only respondents who prefer $12.50 wage" )
		plot_twelve_fifty_partisans_rep
		ggsave("figA13_rep.pdf", plot = plot_twelve_fifty_partisans_rep, width = 7.5 , height = 4.5  )




### ONLY THOSE THAT HAVE A PREFERENCE FOR $15 or more (517 respondents - only about 50 republicans)
	fifteen_partisans <- subset( noindep_unm, UCMwagepref_num == 5 )

# Plot distribution of partisanship among those that want $10.50
	fifteen_partisans_pidPLOT <- ggplot( data = fifteen_partisans ) 
	fifteen_partisans_pidPLOT <- fifteen_partisans_pidPLOT + ggtitle("partisanship", subtitle = "Respondents who prefer $15 min wage")
	fifteen_partisans_pidPLOT <- fifteen_partisans_pidPLOT + geom_bar( aes( x = Democrat ) )
	fifteen_partisans_pidPLOT


	fifteen_partisans_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	fifteen_partisans_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		fifteen_partisans_dem$shift [ fifteen_partisans_dem$partyID == "out-partisan (R)" & fifteen_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( fifteen_partisans$ShiftFromUsedto_dem[ fifteen_partisans$Republican == 1 &  fifteen_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		fifteen_partisans_dem$shift [ fifteen_partisans_dem$partyID == "out-partisan (R)" & fifteen_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( fifteen_partisans$ShiftFromUsedto_dem[ fifteen_partisans$Republican == 1 & fifteen_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		fifteen_partisans_dem$shift [ fifteen_partisans_dem$partyID == "in-partisan (D)" & fifteen_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( fifteen_partisans$ShiftFromUsedto_dem[ fifteen_partisans$Democrat == 1 & fifteen_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		fifteen_partisans_dem$shift [ fifteen_partisans_dem$partyID == "in-partisan (D)" & fifteen_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( fifteen_partisans$ShiftFromUsedto_dem[ fifteen_partisans$Democrat == 1 & fifteen_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		fifteen_partisans_dem$se [ fifteen_partisans_dem$partyID == "out-partisan (R)" & fifteen_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( fifteen_partisans$ShiftFromUsedto_dem[ fifteen_partisans$Republican == 1 &  fifteen_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		fifteen_partisans_dem$se [ fifteen_partisans_dem$partyID == "out-partisan (R)" & fifteen_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( fifteen_partisans$ShiftFromUsedto_dem[ fifteen_partisans$Republican == 1 & fifteen_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		fifteen_partisans_dem$se [ fifteen_partisans_dem$partyID == "in-partisan (D)" & fifteen_partisans_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( fifteen_partisans$ShiftFromUsedto_dem[ fifteen_partisans$Democrat == 1 & fifteen_partisans$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		fifteen_partisans_dem$se [ fifteen_partisans_dem$partyID == "in-partisan (D)" & fifteen_partisans_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( fifteen_partisans$ShiftFromUsedto_dem[ fifteen_partisans$Democrat == 1 & fifteen_partisans$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		fifteen_partisans_dem <- fifteen_partisans_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		fifteen_partisans_rep$shift [ fifteen_partisans_rep$partyID == "in-partisan (R)" & fifteen_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( fifteen_partisans$ShiftFromUsedto_rep[ fifteen_partisans$Republican == 1 &  fifteen_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		fifteen_partisans_rep$shift [ fifteen_partisans_rep$partyID == "in-partisan (R)" & fifteen_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( fifteen_partisans$ShiftFromUsedto_rep[ fifteen_partisans$Republican == 1 & fifteen_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		fifteen_partisans_rep$shift [ fifteen_partisans_rep$partyID == "out-partisan (D)" & fifteen_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( fifteen_partisans$ShiftFromUsedto_rep[ fifteen_partisans$Democrat == 1 & fifteen_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		fifteen_partisans_rep$shift [ fifteen_partisans_rep$partyID == "out-partisan (D)" & fifteen_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( fifteen_partisans$ShiftFromUsedto_rep[ fifteen_partisans$Democrat == 1 & fifteen_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		fifteen_partisans_rep$se [ fifteen_partisans_rep$partyID == "in-partisan (R)" & fifteen_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( fifteen_partisans$ShiftFromUsedto_rep[ fifteen_partisans$Republican == 1 &  fifteen_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		fifteen_partisans_rep$se [ fifteen_partisans_rep$partyID == "in-partisan (R)" & fifteen_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( fifteen_partisans$ShiftFromUsedto_rep[ fifteen_partisans$Republican == 1 & fifteen_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		fifteen_partisans_rep$se [ fifteen_partisans_rep$partyID == "out-partisan (D)" & fifteen_partisans_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( fifteen_partisans$ShiftFromUsedto_rep[ fifteen_partisans$Democrat == 1 & fifteen_partisans$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		fifteen_partisans_rep$se [ fifteen_partisans_rep$partyID == "out-partisan (D)" & fifteen_partisans_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( fifteen_partisans$ShiftFromUsedto_rep[ fifteen_partisans$Democrat == 1 & fifteen_partisans$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		fifteen_partisans_rep <- fifteen_partisans_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )
	
	## PLOTS
		plot_fifteen_partisans_dem <- ggplot( data= fifteen_partisans_dem ) + scale_x_continuous( limits=c(0, length( fifteen_partisans_dem$shift )+ 1 ), name="", labels = fifteen_partisans_dem$Closer, breaks = c( 1 : length( fifteen_partisans_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL)
		plot_fifteen_partisans_dem <- plot_fifteen_partisans_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_fifteen_partisans_dem <- plot_fifteen_partisans_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_fifteen_partisans_dem <- plot_fifteen_partisans_dem + coord_flip() + theme( plot.title = element_text( color = "blue" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_fifteen_partisans_dem <- plot_fifteen_partisans_dem + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "Only respondents who prefer $15 wage" )
		plot_fifteen_partisans_dem
		ggsave("figA12_dem.pdf", plot = plot_fifteen_partisans_dem, width = 7.5 , height = 4.5  )

		plot_fifteen_partisans_rep <- ggplot( data= fifteen_partisans_rep ) + scale_x_continuous( limits=c(0, length( fifteen_partisans_rep$shift )+ 1 ), name="", labels = fifteen_partisans_rep$Closer, breaks = c( 1 : length( fifteen_partisans_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND INITIAL CANDIDATE POSITION") + xlab(NULL)
		plot_fifteen_partisans_rep <- plot_fifteen_partisans_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		plot_fifteen_partisans_rep <- plot_fifteen_partisans_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		plot_fifteen_partisans_rep <- plot_fifteen_partisans_rep + coord_flip() + theme( plot.title = element_text( color = "red" ), panel.background = element_rect(fill = "white", colour = "grey0"), legend.position = "bottom" ) + guides( color = guide_legend( title = "Respondent" ) )
		plot_fifteen_partisans_rep <- plot_fifteen_partisans_rep + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Only respondents who prefer $15 wage" )
 		plot_fifteen_partisans_rep
		ggsave("figA12_rep.pdf", plot = plot_fifteen_partisans_rep, width = 7.5 , height = 4.5  )







#####################	FIGURE A.17 : Preference Mediated Partisan Motivation in opinions about the CANDIDATE’S HONESTY

avHONESTY_closer_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
avHONESTY_closer_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate
		avHONESTY_closer_dem$honesty [ avHONESTY_closer_dem$partyID == "out-partisan (R)" & avHONESTY_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$UCMwagehonestdem_rev [ noindep_unm$Democrat == 0 & noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avHONESTY_closer_dem$honesty [ avHONESTY_closer_dem$partyID == "out-partisan (R)" & avHONESTY_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$UCMwagehonestdem_rev [ noindep_unm$Democrat == 0 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		avHONESTY_closer_dem$honesty [ avHONESTY_closer_dem$partyID == "in-partisan (D)" & avHONESTY_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$UCMwagehonestdem_rev [ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avHONESTY_closer_dem$honesty [ avHONESTY_closer_dem$partyID == "in-partisan (D)" & avHONESTY_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$UCMwagehonestdem_rev [ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avHONESTY_closer_dem$honesty_SE [ avHONESTY_closer_dem$partyID == "out-partisan (R)" & avHONESTY_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$UCMwagehonestdem_rev [ noindep_unm$Democrat == 0 & noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avHONESTY_closer_dem$honesty_SE [ avHONESTY_closer_dem$partyID == "out-partisan (R)" & avHONESTY_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$UCMwagehonestdem_rev [ noindep_unm$Democrat == 0 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
		avHONESTY_closer_dem$honesty_SE [ avHONESTY_closer_dem$partyID == "in-partisan (D)" & avHONESTY_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$UCMwagehonestdem_rev [ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 0 ] ), na.rm = TRUE )
		avHONESTY_closer_dem$honesty_SE [ avHONESTY_closer_dem$partyID == "in-partisan (D)" & avHONESTY_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$UCMwagehonestdem_rev [ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ] ), na.rm = TRUE )
	
		avHONESTY_closer_dem <- avHONESTY_closer_dem %>% mutate ( lo = ( honesty - 1.96 * honesty_SE), up = ( honesty + 1.96 * honesty_SE )  )

	## PLOT
		honesty_nomodel_closer_dem <- ggplot( data= avHONESTY_closer_dem ) + scale_x_continuous( limits=c(0, length( avHONESTY_closer_dem$honesty )+ 1 ), name="", labels = avHONESTY_closer_dem$Closer, breaks = c( 1 : length( avHONESTY_closer_dem$up ) ) ) + scale_y_continuous(limits=c( 1 , 5 ), labels = c( "1\nminimum\nhonesty", "2", "3", "4", "5\nmaximum\nhonesty" ) ) + ylab("Average Level of Honesty Attributed to the Candidate") + xlab(NULL) + ggtitle("DEMOCRATIC CANDIDATE", subtitle = "Candidate Honesty")
		honesty_nomodel_closer_dem <- honesty_nomodel_closer_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		honesty_nomodel_closer_dem <- honesty_nomodel_closer_dem + geom_point( aes ( y = honesty , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		honesty_nomodel_closer_dem <- honesty_nomodel_closer_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "blue" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		honesty_nomodel_closer_dem
		ggsave("figA18_DEM.pdf", plot = honesty_nomodel_closer_dem, width = 12 , height = 3  )

	# Republican Candidate
		avHONESTY_closer_rep$honesty [ avHONESTY_closer_rep$partyID == "in-partisan (R)" & avHONESTY_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$UCMwagehonestrep_rev [ noindep_unm$Democrat == 0 & noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avHONESTY_closer_rep$honesty [ avHONESTY_closer_rep$partyID == "in-partisan (R)" & avHONESTY_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$UCMwagehonestrep_rev [ noindep_unm$Democrat == 0 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		avHONESTY_closer_rep$honesty [ avHONESTY_closer_rep$partyID == "out-partisan (D)" & avHONESTY_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( abs( noindep_unm$UCMwagehonestrep_rev [ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avHONESTY_closer_rep$honesty [ avHONESTY_closer_rep$partyID == "out-partisan (D)" & avHONESTY_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( abs( noindep_unm$UCMwagehonestrep_rev [ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avHONESTY_closer_rep$honesty_SE [ avHONESTY_closer_rep$partyID == "in-partisan (R)" & avHONESTY_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$UCMwagehonestrep_rev [ noindep_unm$Democrat == 0 & noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avHONESTY_closer_rep$honesty_SE [ avHONESTY_closer_rep$partyID == "in-partisan (R)" & avHONESTY_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$UCMwagehonestrep_rev [ noindep_unm$Democrat == 0 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
		avHONESTY_closer_rep$honesty_SE [ avHONESTY_closer_rep$partyID == "out-partisan (D)" & avHONESTY_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$UCMwagehonestrep_rev [ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 0 ] ), na.rm = TRUE )
		avHONESTY_closer_rep$honesty_SE [ avHONESTY_closer_rep$partyID == "out-partisan (D)" & avHONESTY_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( abs( noindep_unm$UCMwagehonestrep_rev [ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ] ), na.rm = TRUE )
	
		avHONESTY_closer_rep <- avHONESTY_closer_rep %>% mutate ( lo = ( honesty - 1.96 * honesty_SE), up = ( honesty + 1.96 * honesty_SE )  )

	## PLOT
		honesty_nomodel_closer_rep <- ggplot( data= avHONESTY_closer_rep ) + scale_x_continuous( limits=c(0, length( avHONESTY_closer_rep$honesty )+ 1 ), name="", labels = avHONESTY_closer_rep$Closer, breaks = c( 1 : length( avHONESTY_closer_rep$up ) ) ) + scale_y_continuous(limits=c( 1 , 5 ), labels = c( "1\nminimum\nhonesty", "2", "3", "4", "5\nmaximum\nhonesty" ) ) + ylab("Average Level of Honesty Attributed to the Candidate") + xlab(NULL) + ggtitle("REPUBLICAN CANDIDATE", subtitle = "Candidate Honesty")
		honesty_nomodel_closer_rep <- honesty_nomodel_closer_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" )  ) 
		honesty_nomodel_closer_rep <- honesty_nomodel_closer_rep + geom_point( aes ( y = honesty , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		honesty_nomodel_closer_rep <- honesty_nomodel_closer_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "red" )) + guides( color = guide_legend( title = "Respondent" ) )
		honesty_nomodel_closer_rep
		ggsave("figA18_REP.pdf", plot = honesty_nomodel_closer_rep, width = 12 , height = 3  )









######################	FIGURE A18

############### 	ALTERNATIVE DEPENDENT VARIABLE: DISTANCE BETWEEN RESPONDENT PERCEPTION AND THE *SECOND* CANDIDATE POSITION
#####################	The effect of the candidate's shift as a function of the RESPONDENT'S PARTY ID and HER OWN PREFERENCE on the policy issue
	
	### Getting rid of independents
	noindep_unm <- subset( cces_unmatched, Independent == 0 )

	avDistT2_closer_dem <- data.frame ( partyID = factor ( c( 1, 1, 2, 2 ), levels = c( 2, 1 ), labels = c( "in-partisan (D)", "out-partisan (R)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )
	avDistT2_closer_rep <- data.frame ( partyID = factor ( c( 2, 2, 1, 1 ), levels = c( 1, 2 ), labels = c( "in-partisan (R)", "out-partisan (D)" ) ), Closer = factor ( c( 0, 1, 0, 1 ), levels = c( 0, 1 ), labels= c( "AWAY FROM\nRESPONDENT", "TOWARDS\nRESPONDENT" ) )  )

	# Democratic Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		avDistT2_closer_dem$shift [ avDistT2_closer_dem$partyID == "out-partisan (R)" & avDistT2_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( noindep_unm$ClosenessT2_dem[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_dem == 0 ], na.rm = TRUE )
		avDistT2_closer_dem$shift [ avDistT2_closer_dem$partyID == "out-partisan (R)" & avDistT2_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( noindep_unm$ClosenessT2_dem[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ], na.rm = TRUE )
		avDistT2_closer_dem$shift [ avDistT2_closer_dem$partyID == "in-partisan (D)" & avDistT2_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( noindep_unm$ClosenessT2_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 0 ], na.rm = TRUE )
		avDistT2_closer_dem$shift [ avDistT2_closer_dem$partyID == "in-partisan (D)" & avDistT2_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( noindep_unm$ClosenessT2_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ], na.rm = TRUE )
	
		avDistT2_closer_dem$se [ avDistT2_closer_dem$partyID == "out-partisan (R)" & avDistT2_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( noindep_unm$ClosenessT2_dem[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_dem == 0 ], na.rm = TRUE )
		avDistT2_closer_dem$se [ avDistT2_closer_dem$partyID == "out-partisan (R)" & avDistT2_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( noindep_unm$ClosenessT2_dem[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ], na.rm = TRUE )
		avDistT2_closer_dem$se [ avDistT2_closer_dem$partyID == "in-partisan (D)" & avDistT2_closer_dem$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( noindep_unm$ClosenessT2_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 0 ], na.rm = TRUE )
		avDistT2_closer_dem$se [ avDistT2_closer_dem$partyID == "in-partisan (D)" & avDistT2_closer_dem$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( noindep_unm$ClosenessT2_dem[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_dem == 1 ], na.rm = TRUE )
	
		avDistT2_closer_dem <- avDistT2_closer_dem %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	# Republican Candidate: mean shift and standard error, by respondent's PARTY ID and DIRECTION of the SHIFT
		avDistT2_closer_rep$shift [ avDistT2_closer_rep$partyID == "in-partisan (R)" & avDistT2_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( noindep_unm$ClosenessT2_rep[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_rep == 0 ], na.rm = TRUE )
		avDistT2_closer_rep$shift [ avDistT2_closer_rep$partyID == "in-partisan (R)" & avDistT2_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( noindep_unm$ClosenessT2_rep[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ], na.rm = TRUE )
		avDistT2_closer_rep$shift [ avDistT2_closer_rep$partyID == "out-partisan (D)" & avDistT2_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- mean ( noindep_unm$ClosenessT2_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 0 ], na.rm = TRUE )
		avDistT2_closer_rep$shift [ avDistT2_closer_rep$partyID == "out-partisan (D)" & avDistT2_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- mean ( noindep_unm$ClosenessT2_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ], na.rm = TRUE )
	
		avDistT2_closer_rep$se [ avDistT2_closer_rep$partyID == "in-partisan (R)" & avDistT2_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( noindep_unm$ClosenessT2_rep[ noindep_unm$Republican == 1 &  noindep_unm$T2CloserToRespondent_rep == 0 ], na.rm = TRUE )
		avDistT2_closer_rep$se [ avDistT2_closer_rep$partyID == "in-partisan (R)" & avDistT2_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( noindep_unm$ClosenessT2_rep[ noindep_unm$Republican == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ], na.rm = TRUE )
		avDistT2_closer_rep$se [ avDistT2_closer_rep$partyID == "out-partisan (D)" & avDistT2_closer_rep$Closer == "AWAY FROM\nRESPONDENT"  ] <- std.error ( noindep_unm$ClosenessT2_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 0 ], na.rm = TRUE )
		avDistT2_closer_rep$se [ avDistT2_closer_rep$partyID == "out-partisan (D)" & avDistT2_closer_rep$Closer == "TOWARDS\nRESPONDENT"  ] <- std.error ( noindep_unm$ClosenessT2_rep[ noindep_unm$Democrat == 1 & noindep_unm$T2CloserToRespondent_rep == 1 ], na.rm = TRUE )
	
		avDistT2_closer_rep <- avDistT2_closer_rep %>% mutate ( lo = ( shift - 1.96 * se), up = ( shift + 1.96 * se )  )

	## PLOTS
		avDistT2_pid_closer_nomodel_dem <- ggplot( data= avDistT2_closer_dem ) + scale_x_continuous( limits=c(0, length( avDistT2_closer_dem$shift )+ 1 ), name="", labels = avDistT2_closer_dem$Closer, breaks = c( 1 : length( avdiff_closer_dem$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND SECOND CANDIDATE POSITION") + xlab(NULL) + ggtitle("DEMOCRATIC CANDIDATE")
		avDistT2_pid_closer_nomodel_dem <- avDistT2_pid_closer_nomodel_dem + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		avDistT2_pid_closer_nomodel_dem <- avDistT2_pid_closer_nomodel_dem + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		avDistT2_pid_closer_nomodel_dem <- avDistT2_pid_closer_nomodel_dem + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "blue" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		avDistT2_pid_closer_nomodel_dem
		ggsave("figA18_dem.pdf", plot = avDistT2_pid_closer_nomodel_dem, width = 25/4 , height = 10/3  )

		avDistT2_pid_closer_nomodel_rep <- ggplot( data= avDistT2_closer_rep ) + scale_x_continuous( limits=c(0, length( avDistT2_closer_rep$shift )+ 1 ), name="", labels = avDistT2_closer_rep$Closer, breaks = c( 1 : length( avDistT2_closer_rep$up ) ) ) + scale_y_continuous(limits=c( 0 , 2 ), labels = c( "0", "0.5", "1", "1.5", "2" ) ) + ylab("DIFFERENCE BETWEEN PERCEPTION\nAND SECOND CANDIDATE POSITION") + xlab(NULL) + ggtitle("REPUBLICAN CANDIDATE")
		avDistT2_pid_closer_nomodel_rep <- avDistT2_pid_closer_nomodel_rep + geom_linerange(aes( ymin = lo, ymax = up, x = c( 1 : length( up ) ), color = partyID ), size=1.5, alpha = .75) + scale_colour_manual( values = c( "grey0", "grey50" ) ) 
		avDistT2_pid_closer_nomodel_rep <- avDistT2_pid_closer_nomodel_rep + geom_point( aes ( y = shift , x = c( 1 : length( up ) ), color = partyID ), size=3 , alpha = .75 )
		avDistT2_pid_closer_nomodel_rep <- avDistT2_pid_closer_nomodel_rep + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0"), plot.title = element_text( color = "red" ) ) + guides( color = guide_legend( title = "Respondent" ) )
		avDistT2_pid_closer_nomodel_rep
		ggsave("figA18_rep.pdf", plot = avDistT2_pid_closer_nomodel_rep, width = 25/4 , height = 10/3  )



# Close LOG FILE
	sink()