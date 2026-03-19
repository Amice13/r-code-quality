################################################
# Replication code 
# Article: The Credibility of Party Policy Rhetoric: Survey Experimental Evidence
# Author: Pablo Fernandez-Vazquez
# Purpose: Replicate results in the paper that rely on BRITISH ELECTION STUDY
# Source Data: 	1) BES2015_W7_v1.3.dta
#				2) nhs_SurvExp.csv
# Software: R version 3.4.3
################################################


######## LOADING PACKAGES
	install.packages( "tidyverse", repos = "http://cran.rstudio.com" )
	install.packages( "haven", repos = "http://cran.rstudio.com" )

	library( "tidyverse" )
	library( "haven" )


####### setting working directory
	setwd("/Users/pablofernandez/Dropbox/dissertation/survey_experiment/survey_experiment_paper/jop_short_submission/jop_short_submission_replication_files")


############ 	LOADING PUBLIC OPINION DATA
############	British Election Study Internet Panel Study Wave 7
	w7 <- read_dta( "BES2015_W7_v1.3.dta", encoding = "latin1" )



############	FIGURE A.1

##### Top Plot:	Distribution of preferences about immigration

	# First, assign missing values
		w7 [, c("immigSelf", "immigCon", "immigLab") ] <- w7 %>% select (immigSelf, immigCon, immigLab) %>% apply( 2, function(x) ifelse(x==9999,NA,x) )
	
	
	# Histogram of voter preferences
		immh <- ggplot( data = w7 , aes ( immigSelf, weight = wt_core_W7 ) , title = "aaa")  + geom_bar ( fill = "grey100", color = "grey0" ) + scale_x_continuous( breaks = c( 0, 5, 10 ) , labels = c( "0 - Many Fewer", "5", "10 - Many More" ) , name = "Desired flow of immigrants into the UK" )
		immh <- immh + ggtitle("PREFERENCES OVER IMMIGRATION INTO THE UK")
		immh
		ggsave(immh, file = "immigration_preferences.pdf",  width=6, height=3.5)



##### Bottom-left Plot:	OPINION ABOUT EVOLUTION OF THE NHS

	# First, recode CUTS IN NHS variable
		w7 <- w7 %>% mutate(cutsTooFarNHS = ifelse( cutsTooFarNHS == 9999 , NA, cutsTooFarNHS )) 
	
	# Histogram
		cutsNHS <- ggplot( data = w7  )  + geom_bar ( aes ( x = cutsTooFarNHS, weight = wt_core_W7 ), na.rm = TRUE , fill = "grey0", color ="grey100") + scale_x_continuous( breaks = c( 1:5 ) , labels = c( "not nearly far enough", "not far enough", "about right", "too far", "much too far" )  , name = "'Cuts in the NHS have gone...'" )
		cutsNHS <- cutsNHS + ggtitle("OPINION ABOUT CUTS TO NHS FUNDING")
		cutsNHS
		ggsave(cutsNHS, file = "cutsNHS.pdf",  width=6.1, height=3.5 )
	

##### Bottom-right Plot:	OPINION ABOUT CUTS TO NHS FUNDING

	# First, recode CHANGE IN THE NHS variable and transform into factor
		w7 <- w7 %>% mutate(changeNHS = ifelse(changeNHS == 9999 , NA, changeNHS )) %>% mutate(changeNHS = 6 - changeNHS ) 
	
	# histogram
		chNHS <- ggplot( data = w7  )  + geom_bar ( aes ( x = changeNHS, weight = wt_core_W7 ), na.rm = TRUE, fill = "grey0", color ="grey100" ) + scale_x_continuous( breaks = c( 1:5 ) , labels = c( "a lot better", "a little better", "about the same", "a little worse", "a lot worse" )  , name = "'The NHS is getting...'" )
		chNHS <- chNHS + ggtitle("OPINION ABOUT EVOLUTION OF THE NHS")
		chNHS
		ggsave( chNHS, file = "evolution_in_NHS.pdf",  width=6.1, height=3.5 )
		



############	FIGURE A.2
############	Histogram of voter preferences on immigration BY REGION  (Nation) of the UK

	# Convert Nation into factor variable
		w7 <- w7 %>% mutate( country = factor( country , levels = c( 1:3 ), labels = c( "ENGLAND", "SCOTLAND", "WALES" ) ) )
	
		immhnation <- ggplot( data = w7 , aes ( immigSelf ) )  + geom_density ( ) + scale_x_continuous( breaks = c( 0, 5, 10 ) , labels = c( "0 - Fewer", "5", "10 - More" ) , name = "Desired flow of immigrants into the UK" )
		immhnation <- immhnation + facet_grid ( country ~ . )
		immhnation
		ggsave(immhnation, file = "immigration_preferences_byNation.pdf",  width=4, height=6)





############	FIGURE A.3
############	Voter Preferences and Parties' Perceived Placements

	# Loading the NHS Survey Experiment Data - Used for NHS placements
		nhs <- read.csv("nhs_SurvExp.csv", header=TRUE, stringsAsFactors=FALSE)

	# Creating a data frame for the plot
		voters_short <- data.frame( position = c( apply ( subset( w7, select = c("immigSelf") ), 2, function(x) mean( x, na.rm  = T ) ), apply ( subset( nhs, select = c("nhs_self") ), 2, function(x) mean( x, na.rm  = T ) ) ), party = factor ( c( "Self", "Self" ), levels = c( "Self", "Con", "Lab" ), labels = c( "SELF", "CONSERVAT", "LABOUR" ) ), issue = c( "immigration", "nhs"  ) )
		position_short <- data.frame( position = c( apply ( subset( w7, select = c("immigSelf", "immigCon", "immigLab") ), 2, function(x) mean( x, na.rm  = T ) ), apply ( subset( nhs, select = c("nhs_self", "nhs_con", "nhs_lab") ), 2, function(x) mean( x, na.rm  = T ) ) ), party = factor ( c( "Self", "Con", "Lab", "Self", "Con", "Lab" ), levels = c( "Self", "Con", "Lab" ), labels = c( "SELF", "CONSERVAT", "LABOUR" ) ), issue = c( "immigration", "immigration", "immigration", "nhs", "nhs", "nhs"  ) )

	# Creating the plot
		immigration_and_nhs_placements <- ggplot( ) + scale_x_continuous(limits=c( 0, 1 ), name="", labels=NULL, breaks=NULL ) + scale_y_continuous( limits=c( -0.44 , 10.44 ) ) + ylab("Issue Scale\n\nSOURCE: British Election Study 2015 and Survey Experiment") + xlab( NULL ) + ggtitle("Voter Preferences and Parties' Perceived Placements")
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_point( aes( y = position_short$position[ position_short$issue == "immigration" ], x = 0.7, color = position_short$party[ position_short$issue == "immigration" ] ), shape = 17, alpha = .75, size = 5 ) 
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_point( aes( y = position_short$position[ position_short$issue == "nhs" ], x = 0.2, color = position_short$party[ position_short$issue == "nhs" ] ), shape = 17, alpha = .75, size = 5 ) 
		immigration_and_nhs_placements <- immigration_and_nhs_placements + scale_color_manual(values=c("black", "#0087dc", "#d50000", "#70147A", "#ffb81c")) # Here I am using the official party colours (hex code)
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_errorbar ( aes( ymin = 0, ymax = 10, x = 0.68 ), color = "grey60", width = 0.05 )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_errorbar ( aes( ymin = 0, ymax = 10, x = 0.18 ), color = "grey60", width = 0.05 )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_text( aes( y = 0 , x = 0.58 ), label = "MANY\nFEWER",  size = 3 )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_text( aes( y = 10 , x = 0.58 ), label = "MANY\nMORE",  size = 3 )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_text( aes( y = 0 , x = 0.08 ), label = "LESS\nINVESTMENT",  size = 3 )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_text( aes( y = 10 , x = 0.08 ), label = "MORE\nINVESTMENT",  size = 3 )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_text( aes( y = 5 , x = 0.8 ), label = "IMMIGRATION",  size = 4 )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + geom_text( aes( y = 5 , x = 0.3 ), label = "NHS",  size = 4 )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + theme( panel.background = element_rect( fill = "white", colour = "grey0" ) ) + guides( color = guide_legend( title = "Position" ) )
		immigration_and_nhs_placements <- immigration_and_nhs_placements + coord_flip()
		immigration_and_nhs_placements
		ggsave("nhs_immigration_placements.pdf", plot = immigration_and_nhs_placements, width = 7 , height = 4.5 )

