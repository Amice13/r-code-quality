################################################
# Replication code 
# Article: The Credibility of Party Policy Rhetoric: Survey Experimental Evidence
# Author: Pablo Fernandez-Vazquez
# Purpose: Replicate results in the paper that rely on data from IMMIGRATION and NHS survey experiments
# Source Data: 	1) immigration_SurvExp.csv
#				2) nhs_SurvExp.csv
# Software: R version 3.4.3
################################################

####### Install and load packages 
install.packages("dplyr", repos = "http://cran.rstudio.com")
install.packages("ggplot2", repos = "http://cran.rstudio.com")
install.packages("margins", repos = "http://cran.rstudio.com")


library( dplyr )
library( ggplot2 )
library( margins )


####### setting working directory
	setwd("/Users/pablofernandez/Dropbox/dissertation/survey_experiment/survey_experiment_paper/jop_short_submission/jop_short_submission_replication_files")



####### LOADING DATA

	## Immigration Survey Experiment
	imm <- read.csv("immigration_SurvExp.csv", header=TRUE, stringsAsFactors=FALSE)

	## NHS Survey Experiment
	nhs <- read.csv("nhs_SurvExp.csv", header=TRUE, stringsAsFactors=FALSE)



####### CODING OUTCOME VARIABLES

	## Pos-treatment perception

		## Immigration Experiment. Conservative Party
			imm$imm_c_postcomp <- imm$imm_c_post
			imm$imm_c_postcomp <-ifelse(is.na(imm$imm_c_postcomp) & !is.na(imm$imm_c_post.1), imm$imm_c_post.1, imm$imm_c_postcomp)
			table( imm$imm_c_postcomp )
	
		## Immigration Experiment. Labour Party
			imm$imm_l_postcomp <- imm$imm_l_post
			imm$imm_l_postcomp <-ifelse(is.na(imm$imm_l_postcomp) & !is.na(imm$imm_l_post.1), imm$imm_l_post.1, imm$imm_l_postcomp)
			table( imm$imm_l_postcomp )
	
		## NHS Experiment. Conservative Party
			nhs$con_post <- nhs$con_post_d
			nhs$con_post <-ifelse(is.na(nhs$con_post) & !is.na(nhs$con_post_i), nhs$con_post_i, nhs$con_post)
			table( nhs$con_post )
	
		## NHS Experiment. Labour Party
			nhs$lab_post <- nhs$lab_post_d
			nhs$lab_post <-ifelse(is.na(nhs$lab_post) & !is.na(nhs$lab_post_i), nhs$lab_post_i, nhs$lab_post)
			table( nhs$lab_post )


	## Change in the respondent's perception between pre and post treatment

		## Immigration			
			imm$update_c <- imm$imm_c_postcomp - imm$imm_c_pre
			imm$update_l <- imm$imm_l_postcomp - imm$imm_l_pre
			table( imm$update_c )
			table( imm$update_l )
	
		## NHS
			nhs$update_c <- nhs$con_post-nhs$nhs_con
			nhs$update_l <- nhs$lab_post-nhs$nhs_lab
			table( nhs$update_c )
			table( nhs$update_l )



####### TREATMENT ( coded as factor )

	# Immigration experiment. Conservative Party	
		imm$treat_c <- NA
		imm$treat_c <- ifelse(!is.na(imm$con_restr),"restrict",imm$treat_c)
		imm$treat_c <- ifelse(!is.na(imm$con_open),"open",imm$treat_c)
		imm$treat_c <- factor ( imm$treat_c, levels = c( "open", "restrict" ), labels = c( "pro-immigration", "anti-immigration" ) )

	# Immigration experiment. Labour Party	
		imm$treat_l <- NA
		imm$treat_l <- ifelse(!is.na(imm$lab_restr),"restrict",imm$treat_l)
		imm$treat_l <- ifelse(!is.na(imm$lab_open),"open",imm$treat_l)
		imm$treat_l <- factor ( imm$treat_l, levels = c( "open", "restrict" ), labels = c( "pro-immigration", "anti-immigration" ) )
	
	# NHS experiment. Conservative Party		
		nhs$treat_c <- NA
		nhs$treat_c <- ifelse(!is.na(nhs$con_dec),"decrease",nhs$treat_c)
		nhs$treat_c <- ifelse(!is.na(nhs$con_inc),"increase",nhs$treat_c)
		nhs$treat_c <- factor( nhs$treat_c, levels = c( "increase", "decrease" ), labels = c( "pro-NHS", "anti-NHS" ) )
	
	# NHS experiment. Labour Party		
		nhs$treat_l <- NA
		nhs$treat_l <- ifelse(!is.na(nhs$lab_dec),"decrease",nhs$treat_l)
		nhs$treat_l <- ifelse(!is.na(nhs$lab_inc),"increase",nhs$treat_l)
		nhs$treat_l <- factor( nhs$treat_l, levels = c( "increase", "decrease" ), labels = c( "pro-NHS", "anti-NHS" ) )
		



####### TREATMENT INDICATOR as NUMERIC DUMMY

	# Immigration Experiment. Conservative Party.
		imm$proimmtreat_con <- ifelse( imm$treat_c == "pro-immigration", 1, NA )
		imm$proimmtreat_con <- ifelse( imm$treat_c == "anti-immigration", 0, imm$proimmtreat_con )

		imm$antiimmtreat_con <- ifelse( imm$treat_c == "pro-immigration", 0, NA )
		imm$antiimmtreat_con <- ifelse( imm$treat_c == "anti-immigration", 1, imm$antiimmtreat_con )


	# Immigration Experiment. Labour Party
		imm$proimmtreat_lab <- ifelse( imm$treat_l == "pro-immigration", 1, NA )
		imm$proimmtreat_lab <- ifelse( imm$treat_l == "anti-immigration", 0, imm$proimmtreat_lab )

		imm$antiimmtreat_lab <- ifelse( imm$treat_l == "pro-immigration", 0, NA )
		imm$antiimmtreat_lab <- ifelse( imm$treat_l == "anti-immigration", 1, imm$antiimmtreat_lab )
	

	# NHS Experiment. Conservative Party
		nhs$pronhs_con <- ifelse( nhs$treat_c == "pro-NHS", 1, NA )
		nhs$pronhs_con <- ifelse( nhs$treat_c == "anti-NHS", 0, nhs$pronhs_con )
	
		nhs$antinhs_con <- ifelse( nhs$treat_c == "pro-NHS", 0, NA )
		nhs$antinhs_con <- ifelse( nhs$treat_c == "anti-NHS", 1, nhs$antinhs_con )

	# NHS Experiment. Labour Party
		nhs$pronhs_lab <- ifelse( nhs$treat_l == "pro-NHS", 1, NA )
		nhs$pronhs_lab <- ifelse( nhs$treat_l == "anti-NHS", 0, nhs$pronhs_lab )
	
		nhs$antinhs_lab <- ifelse( nhs$treat_l == "pro-NHS", 0, NA )
		nhs$antinhs_lab <- ifelse( nhs$treat_l == "anti-NHS", 1, nhs$antinhs_lab )


####### POSITION EXPRESSED IN THE STATEMENT - as perceived by the respondent

		## Immigration Experiment. Conservative Party
			imm$treat_c_pos <- imm$c_restr_ch
			imm$treat_c_pos <- ifelse(is.na(imm$treat_c_pos) & !is.na(imm$c_open_ch), imm$c_open_ch, imm$treat_c_pos)
			table( imm$treat_c_pos )

		## Immigration Experiment. Labour Party
			imm$treat_l_pos <- imm$l_restr_ch
			imm$treat_l_pos <-ifelse(is.na(imm$treat_l_pos) & !is.na(imm$l_open_ch), imm$l_open_ch, imm$treat_l_pos)
			table( imm$treat_l_pos )
	
		## NHS Experiment. Conservative Party
			nhs$treat_c_pos <- nhs$c_dec_ch
			nhs$treat_c_pos <-ifelse(is.na(nhs$treat_c_pos) & !is.na(nhs$c_inc_ch), nhs$c_inc_ch, nhs$treat_c_pos)
			table( nhs$treat_c_pos )

		## NHS Experiment. Conservative Party
			nhs$treat_l_pos <- nhs$l_dec_ch
			nhs$treat_l_pos <-ifelse(is.na(nhs$treat_l_pos) & !is.na(nhs$l_inc_ch), nhs$l_inc_ch, nhs$treat_l_pos)
			table( nhs$treat_l_pos )
	



####### MAXIMUM POTENTIAL UPDATE
#######		Difference between pre-treatment of the party and the maximum post-treatment perception that could be generated by the treatment	

	# Immigration Experiment. Conservative Party
		imm$max_effect_con <- ifelse( imm$treat_c == "anti-immigration", abs( 0 - imm$imm_c_pre ), NA )
		imm$max_effect_con <- ifelse( imm$treat_c == "pro-immigration", abs( 10 - imm$imm_c_pre ), imm$max_effect_con )
	
	# Immigration Experiment. Labour Party
		imm$max_effect_lab <- ifelse( imm$treat_l == "anti-immigration", abs( 0 - imm$imm_l_pre ), NA )
		imm$max_effect_lab <- ifelse( imm$treat_l == "pro-immigration", abs( 10 - imm$imm_l_pre ), imm$max_effect_lab )

	# NHS Experiment. Conservative Party
		nhs$max_effect_con <- ifelse( nhs$treat_c == "anti-NHS", abs( 0 - nhs$nhs_con ), NA )
		nhs$max_effect_con <- ifelse( nhs$treat_c == "pro-NHS", abs( 10 - nhs$nhs_con ), nhs$max_effect_con )
	
	# NHS Experiment. Labour Party
		nhs$max_effect_lab <- ifelse( nhs$treat_l == "anti-NHS", abs( 0 - nhs$nhs_lab ), NA )
		nhs$max_effect_lab <- ifelse( nhs$treat_l == "pro-NHS", abs( 10 - nhs$nhs_lab ), nhs$max_effect_lab )
			


####### PERCEIVED SHIFT
#######		Difference between position stated in the statement and the party's perceived position before the treatment
	
	# Immigration Experiment. Conservative Party
		imm$shift_c <- imm$treat_c_pos - imm$imm_c_pre	
	# Immigration Experiment. Labour Party
		imm$shift_l <- imm$treat_l_pos - imm$imm_l_pre
	# Immigration Experiment. Conservative Party
		nhs$shift_c <- nhs$treat_c_pos - nhs$nhs_con
	# Immigration Experiment. Labour Party
		nhs$shift_l <- nhs$treat_l_pos - nhs$nhs_lab
	

####### INDICATOR: POPULAR dummy
######		The respondent interprets that the statement reflects the position that most citizens espouse
		
		## Immigration Experiment. Conservative Party
			imm$popular_statement_con <- ifelse( ( imm$most_pop == 1 & imm$treat_c_pos < 5) | ( imm$most_pop == 0 & imm$treat_c_pos > 5 ), 1, 0 )
		
		## Immigration Experiment. Labour Party
			imm$popular_statement_lab <- ifelse( ( imm$most_pop == 1 & imm$treat_l_pos < 5 ) | ( imm$most_pop == 0 & imm$treat_l_pos > 5 ), 1, 0 )
		
		## NHS Experiment. Conservative Party
			nhs$popular_statement_con <- ifelse( ( nhs$most_pop == 0 & nhs$treat_c_pos < 5 ) | ( nhs$most_pop == 1 & nhs$treat_c_pos > 5 ), 1, 0 )

		## NHS Experiment. Conservative Party
			nhs$popular_statement_lab <- ifelse( ( nhs$most_pop == 0 & nhs$treat_l_pos < 5 ) | ( nhs$most_pop == 1 & nhs$treat_l_pos > 5 ), 1, 0 )


	
####### INDICATOR: Statement is PRO-IMMIGATION, PRO-NHS
		
		## Pro-IMMIGRATION: Policy shift towards pro-NHS end of the scale. Conservative Party
			imm$proimm_shift_c <- NA
			imm$proimm_shift_c <- ifelse( imm$shift_c > 0, 1, imm$proimm_shift_c )
			imm$proimm_shift_c <- ifelse( imm$shift_c <= 0, 0, imm$proimm_shift_c )
			table( imm$proimm_shift_c )
	 
 		## Pro-IMMIGRATION: Policy shift towards pro-NHS end of the scale. Labour Party
			imm$proimm_shift_l <- NA
			imm$proimm_shift_l <- ifelse( imm$shift_l>0, 1, imm$proimm_shift_l )
			imm$proimm_shift_l <- ifelse( imm$shift_l<=0, 0, imm$proimm_shift_l )
			table( imm$proimm_shift_l )
        	
		## Pro-NHS: Policy shift towards pro-NHS end of the scale. Conservative Party
			nhs$pronhs_shift_c <- NA
			nhs$pronhs_shift_c <- ifelse(nhs$shift_c>0, 1, nhs$pronhs_shift_c)
			nhs$pronhs_shift_c <- ifelse(nhs$shift_c<=0, 0, nhs$pronhs_shift_c)
			table( nhs$pronhs_shift_c )
		
		## Pro-NHS: Policy shift towards pro-NHS end of the scale. Labour Party
			nhs$pronhs_shift_l <- NA
			nhs$pronhs_shift_l <- ifelse(nhs$shift_l>0, 1, nhs$pronhs_shift_l)
			nhs$pronhs_shift_l <- ifelse(nhs$shift_l<=0, 0, nhs$pronhs_shift_l)
			table( nhs$pronhs_shift_l )





####### INDICATOR: Which ISSUE ALTERNATIVE most British Citizens espouse
		
	## Immigration
		imm$most_pop <- ifelse( imm$most_pop == 9, NA, imm$most_pop )

	## NHS
		nhs$most_pop <- ifelse( nhs$most_pop == 9, NA, nhs$most_pop )
	
	## Transforming into FACTOR
		imm$popular_option <- factor( imm$most_pop, levels = c( 0, 1 ), labels = c( "pro-immigration", "anti-immigration" ) )
		nhs$popular_option <- factor( nhs$most_pop, levels = c( 1, 0 ), labels = c( "pro-nhs", "anti-nhs" ) )




####### ATTITUDINAL and DEMOGRAPHIC CONTROLS

# GENDER

	# Immigration
		table(imm$gender)
		imm <- imm %>% mutate ( genderec = gender - 1 ) 
		table(imm$genderec)
	
	# NHS
		table(nhs$gender)
		nhs <- nhs %>% mutate ( genderec = gender - 1 ) 
		table(nhs$genderec)


# AGE
	
	# Immigration
		table(imm$age)
		imm <- imm %>% mutate ( agerec = 2015 - age ) 
		table(imm$agerec)
	
	# NHS
		table(nhs$age)
		nhs <- nhs %>% mutate ( agerec = as.numeric( age ) )  %>% mutate ( agerec = ifelse( agerec < 1940, NA, agerec ) ) %>% mutate ( agerec = 2015 - agerec )  
		table(nhs$agerec)


# EDUCATION
	
	# Immigration
		table(imm$educ)
		imm <- imm %>% mutate ( educrec = ifelse( educ == 99 , NA, educ ) )  %>% mutate ( educrec = factor (educrec, levels = c( 1:7 ), labels = c("15 or younger", "16", "17", "18", "19 or older", "still at school", "still at university" ) ))
		table(imm$educrec)
		
	# NHS
		table(nhs$educ)
		nhs <- nhs %>% mutate ( educrec = ifelse( educ == 99 , NA, educ ) )  %>% mutate ( educrec = factor (educrec, levels = c( 1:7 ), labels = c("15 or younger", "16", "17", "18", "19 or older", "still at school", "still at university" ) ))
		table(nhs$educrec)



# PARTY ID
	
	# Immigration Experiment	
		imm$pid <- factor(imm$pid, levels=as.character(c(1:9, 98, 99)), labels=c("conservative", "labour", "libdem", "ukip", "snp", "plaidcymru", "green", "bnp", "other", "no party", "dk"))
		imm$pidcon <- ifelse( imm$pid=="conservative" , 1, NA)
		imm$pidcon <- ifelse( imm$pid!="conservative" , 0, imm$pidcon)
		imm$pidlab <- ifelse( imm$pid=="labour" , 1, NA)
		imm$pidlab <- ifelse( imm$pid!="labour" , 0, imm$pidlab)
	
	# NHS experiment
		nhs$pid <- factor(nhs$pid, levels=as.character(c(1:9, 98, 99)), labels=c("conservative", "labour", "libdem", "ukip", "snp", "plaidcymru", "green", "bnp", "other", "no party", "dk"))
		nhs$pidcon <- ifelse( nhs$pid=="conservative" , 1, NA)
		nhs$pidcon <- ifelse( nhs$pid!="conservative" , 0, nhs$pidcon)
		nhs$pidlab <- ifelse( nhs$pid=="labour" , 1, NA)
		nhs$pidlab <- ifelse( nhs$pid!="labour" , 0, nhs$pidlab)


# RELIGIOUS DENOMINATION

	# Immigration experiment
		imm$religion <- ifelse( imm$religion == 98 | imm$religion == 98, NA, imm$religion )
		imm$religion <- factor( imm$religion, levels = c( 0:8 ), labels = c( "no religion", "catholic", "anglican", "presbyterian", "methodist", "pentecostal", "hindu", "muslim", "jewish" ) )
	
	# NHS experiment
		nhs$religion <- ifelse( nhs$religion == 98 | nhs$religion == 98, NA, nhs$religion )
		nhs$religion <- factor( nhs$religion, levels = c( 0:8 ), labels = c( "no religion", "catholic", "anglican", "presbyterian", "methodist", "pentecostal", "hindu", "muslim", "jewish" ) )
	





## TABLE II - Regressions

	# DV: Post-treatment perception
	# IV: Statement ( as perceived by Respondent )
	# Control: Pre-treatment placement
	# Interacting with POPULAR STATEMENT

	# Immigration issue. Conservative Party
		bayesian_popular_IMM_con <- lm ( imm_c_postcomp ~ treat_c_pos * popular_statement_con + imm_c_pre * popular_statement_con, data = imm )
		summary( bayesian_popular_IMM_con )
	
	# Immigration issue. Labour Party
		bayesian_popular_IMM_lab <- lm ( imm_l_postcomp ~ treat_l_pos * popular_statement_lab + imm_l_pre * popular_statement_lab, data = imm )
		summary( bayesian_popular_IMM_lab )
		
	# NHS issue. Conservative Party
		bayesian_popular_NHS_con  <- lm( con_post ~ treat_c_pos * popular_statement_con + nhs_con * popular_statement_con, data = nhs )
		summary( bayesian_popular_NHS_con )
	
	# NHS issue. Labour Party
		bayesian_popular_NHS_lab <- lm( lab_post ~ treat_l_pos * popular_statement_lab + nhs_lab * popular_statement_lab, data = nhs )
		summary( bayesian_popular_NHS_lab )
	


## FIGURE I

	# IMMIGRATION, Conservative Party. top-left plot.
		bayesian_popular_IMM_con_MARG <- margins( bayesian_popular_IMM_con, at = list( popular_statement_con = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "treat_c_pos" )
		bayesian_popular_IMM_con_MARG$STATEMENT <- factor( bayesian_popular_IMM_con_MARG$popular_statement_con, levels = c( 1, 0 ), labels = c( "POPULAR", "UNPOPULAR" ) )
		
			margPLOT_bayesian_popular_IMM_con <- ggplot( data = bayesian_popular_IMM_con_MARG ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 1 ) ) + ylab("MARGINAL EFFECT of STATEMENT") + xlab(NULL) + ggtitle("IMMIGRATION - Conservative Party")
			margPLOT_bayesian_popular_IMM_con <- margPLOT_bayesian_popular_IMM_con + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = STATEMENT), size=1) + scale_colour_manual( values = c( "grey60", "grey0" ) )
			margPLOT_bayesian_popular_IMM_con <- margPLOT_bayesian_popular_IMM_con + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
			margPLOT_bayesian_popular_IMM_con <- margPLOT_bayesian_popular_IMM_con + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1))
			margPLOT_bayesian_popular_IMM_con
	
	# IMMIGRATION, Labour Party. top-right plot.
		bayesian_popular_IMM_lab_MARG <- margins( bayesian_popular_IMM_lab, at = list( popular_statement_lab = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "treat_l_pos" )
		bayesian_popular_IMM_lab_MARG$STATEMENT <- factor( bayesian_popular_IMM_lab_MARG$popular_statement_lab, levels = c( 1, 0 ), labels = c( "POPULAR", "UNPOPULAR" ) )
		
			margPLOT_bayesian_popular_IMM_lab <- ggplot( data = bayesian_popular_IMM_lab_MARG ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 1 ) ) + ylab("MARGINAL EFFECT of STATEMENT") + xlab(NULL) + ggtitle("IMMIGRATION - Labour Party")
			margPLOT_bayesian_popular_IMM_lab <- margPLOT_bayesian_popular_IMM_lab + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = STATEMENT), size=1) + scale_colour_manual( values = c( "grey60", "grey0" ) )
			margPLOT_bayesian_popular_IMM_lab <- margPLOT_bayesian_popular_IMM_lab + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
			margPLOT_bayesian_popular_IMM_lab <- margPLOT_bayesian_popular_IMM_lab + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1))
			margPLOT_bayesian_popular_IMM_lab

	# NHS, Conservative Party. bottom-left plot.
		bayesian_popular_NHS_con_MARG <- margins( bayesian_popular_NHS_con, data = nhs, at = list( popular_statement_con = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "treat_c_pos" )
		bayesian_popular_NHS_con_MARG$STATEMENT <- factor( bayesian_popular_NHS_con_MARG$popular_statement_con, levels = c( 1, 0 ), labels = c( "POPULAR", "UNPOPULAR" ) )
		
			margPLOT_bayesian_popular_NHS_con <- ggplot( data = bayesian_popular_NHS_con_MARG ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 1 ) ) + ylab("MARGINAL EFFECT of STATEMENT") + xlab(NULL) + ggtitle("NHS - Conservative Party")
			margPLOT_bayesian_popular_NHS_con <- margPLOT_bayesian_popular_NHS_con + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = STATEMENT), size=1) + scale_colour_manual( values = c( "grey60", "grey0" ) )
			margPLOT_bayesian_popular_NHS_con <- margPLOT_bayesian_popular_NHS_con + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
			margPLOT_bayesian_popular_NHS_con<- margPLOT_bayesian_popular_NHS_con + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1))
			margPLOT_bayesian_popular_NHS_con

	# NHS, Conservative Party. bottom-right plot.
		bayesian_popular_NHS_lab_MARG <- margins( bayesian_popular_NHS_lab, data = nhs, at = list( popular_statement_lab = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "treat_l_pos" )
		bayesian_popular_NHS_lab_MARG$STATEMENT <- factor( bayesian_popular_NHS_lab_MARG$popular_statement_lab, levels = c( 1, 0 ), labels = c( "POPULAR", "UNPOPULAR" ) )
		
			margPLOT_bayesian_popular_NHS_lab <- ggplot( data = bayesian_popular_NHS_lab_MARG ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 1 ) ) + ylab("MARGINAL EFFECT of STATEMENT") + xlab(NULL) + ggtitle("NHS - Labour Party")
			margPLOT_bayesian_popular_NHS_lab <- margPLOT_bayesian_popular_NHS_lab + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = STATEMENT), size=1) + scale_colour_manual( values = c( "grey60", "grey0" ) )
			margPLOT_bayesian_popular_NHS_lab <- margPLOT_bayesian_popular_NHS_lab + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
			margPLOT_bayesian_popular_NHS_lab <- margPLOT_bayesian_popular_NHS_lab + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1))
			margPLOT_bayesian_popular_NHS_lab

	# Saving plots
		ggsave(filename = "fig_popular_IMM_con.pdf",  plot = margPLOT_bayesian_popular_IMM_con, width = 6, height = 3 )
		ggsave(filename = "fig_popular_IMM_lab.pdf",  plot = margPLOT_bayesian_popular_IMM_lab, width = 6, height = 3 )
		ggsave(filename = "fig_popular_NHS_con.pdf",  plot = margPLOT_bayesian_popular_NHS_con, width = 6, height = 3 )
		ggsave(filename = "fig_popular_NHS_lab.pdf",  plot = margPLOT_bayesian_popular_NHS_lab, width = 6, height = 3 )








########################################################################################################################
#########									ONLINE APPENDIX												################
########################################################################################################################

#### TABLE A.2 - Manipulation check

	# 1) Placement meaning of statements, by treatment group

		# T test. Immigration Experiment. Conservative Party
			t.test( imm$treat_c_pos ~ imm$treat_c )
		# T test. Immigration Experiment. Labour Party
			t.test( imm$treat_l_pos ~ imm$treat_l )
		# T test. Immigration Experiment. Conservative Party
			t.test( nhs$treat_c_pos ~ nhs$treat_c )
		# T test. Immigration Experiment. Labour Party
			t.test( nhs$treat_l_pos ~ nhs$treat_l )


	# 2) party placements, pre-treatment

		# Immigration. Conservative Party
			mean( imm$imm_c_pre, na.rm = TRUE )
		# Immigration. Labour Party
			mean( imm$imm_l_pre, na.rm = TRUE )
		# NHS. Conservative Party
			mean( nhs$nhs_con, na.rm = TRUE )
		# NHS. Labour Party
			mean( nhs$nhs_lab, na.rm = TRUE )



#### FIGURE A.10 - Across-subjects effects

	### IMMIGRATION 
		treatment <- factor(c("anti-IMM","pro-IMM","anti-IMM","pro-IMM" ), levels=c("anti-IMM","pro-IMM"), labels = c( "ANTI-IMM", "PRO-IMM"  ))
		party <- factor(c("conservative","conservative","labour","labour"), levels=c("conservative","labour"))
		immdataforaverages <- list( imm$imm_c_postcomp[ imm$treat_c == "anti-immigration" ], imm$imm_c_postcomp[ imm$treat_c == "pro-immigration" ], imm$imm_l_postcomp[ imm$treat_l == "anti-immigration" ], imm$imm_l_postcomp[ imm$treat_l == "pro-immigration" ])
		
		imm_averages <- sapply(immdataforaverages, function(x) mean(x, na.rm=T))
		imm_stderror <- sapply(immdataforaverages, function(x) sqrt(var(x, na.rm=T)/sum(!is.na(x))))
		imm_min <- imm_averages - 1.96*imm_stderror
		imm_max <- imm_averages + 1.96*imm_stderror

		immcrossdata <- data.frame( party, treatment, imm_averages, imm_stderror,imm_min,imm_max) 
		
		simpleimmcross <- ggplot(immcrossdata) + scale_x_continuous(limits=c(0,.6), label=NULL, name="", breaks=NULL) + ggtitle("IMMIGRATION") + scale_y_continuous(limits=c(3.25,7.25), name="average POST-TREATMENT placement", breaks=c(3.25,  5, 7.25), label=c("CLOSE\nBORDERS", "5", "OPEN\nBORDERS"))
		simpleimmcross <- simpleimmcross + geom_linerange(aes(ymin=imm_min, ymax=imm_max,  x =c(.5,.4,.2,.1 ), color=party, linetype = treatment  ), size = 1.25 )
		simpleimmcross <- simpleimmcross + geom_point( aes( y = imm_averages, x =c(.5,.4,.2,.1 ), color = party ), size = 3 )
		simpleimmcross <- simpleimmcross + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0", size = 1.5 ), text = element_text(size=14), axis.text.y=element_text(size=14)) + scale_color_manual( values=c("#0087dc", "#d50000"), guide=FALSE ) 
		simpleimmcross 
	
		ggsave("across-immigration.pdf", plot=simpleimmcross,width=9, height=3.5)
	

	### NHS
		treatment <- factor(c("anti-NHS","pro-NHS","anti-NHS","pro-NHS" ), levels=c("anti-NHS","pro-NHS"), labels = c( "ANTI-NHS", "PRO-NHS" ))
		party <- factor(c("conservative","conservative","labour","labour"), levels=c("conservative","labour"))
		nhsdataforaverages <- list( nhs$con_post[nhs$treat_c=="anti-NHS"], nhs$con_post[nhs$treat_c=="pro-NHS"], nhs$lab_post[nhs$treat_l=="anti-NHS"], nhs$lab_post[nhs$treat_l=="pro-NHS"])

		nhs_averages <- sapply(nhsdataforaverages, function(x) mean(x, na.rm=T))
		nhs_stderror <- sapply(nhsdataforaverages, function(x) sqrt(var(x, na.rm=T)/sum(!is.na(x))))
		nhs_min <- nhs_averages - 1.96*nhs_stderror
		nhs_max <- nhs_averages + 1.96*nhs_stderror

		nhscrossdata <- data.frame(party, treatment, nhs_averages, nhs_stderror, nhs_min, nhs_max) 

		simplenhscross <- ggplot(nhscrossdata) + scale_x_continuous(limits=c(0,.6), label=NULL, name="", breaks=NULL) + ggtitle("NHS") + scale_y_continuous(limits=c(  min(nhs_min), max( nhs_max ) ), name="average POST-TREATMENT placement", breaks=c( min( nhs_min ),  5, max( nhs_max )), label=c("DECREASE\nFUNDS", "5", "INCREASE\nFUNDS"))
		simplenhscross <- simplenhscross + geom_linerange(aes(ymin=nhs_min, ymax=nhs_max,  x =c(.5,.4,.2,.1 ), color=party, linetype = treatment  ), size = 1.25 )
		simplenhscross <- simplenhscross + geom_point(aes(y=nhs_averages, x =c(.5,.4,.2,.1 ), color = party ), size = 3 )
		simplenhscross <- simplenhscross + coord_flip() + theme( panel.background = element_rect(fill = "white", colour = "grey0", size = 1.5 ), text = element_text(size=14), axis.text.y=element_text(size=14)) + scale_color_manual( values=c("#0087dc", "#d50000"), guide=FALSE ) 
		simplenhscross 
		ggsave("across-nhs.pdf", plot=simplenhscross,width=9, height=3.5)



#### TABLE A.03 - Perceptions of which issue alternative is most popular

		table( imm$popular_option )
		table( nhs$popular_option )

	# to obtain percentages
		
		# Immigration
		ggplot( subset( imm, is.na( popular_option ) == FALSE  ), aes( x = popular_option )) +  geom_bar( aes(y = (..count..)/sum(..count..) ) )
		# NHS
		ggplot( subset( nhs, is.na( popular_option ) == FALSE  ), aes( x = popular_option )) +  geom_bar( aes(y = (..count..)/sum(..count..) ) )



#### FIGURE A11

	# DV: Post-treatment perception
	# IV: Statement ( as perceived by Respondent )
	# Control: Pre-treatment placement
	# Interacting with Type of SHIFT : pro or anti imm

	# Immigration. Conservative Party
		direction_IMM_con <- lm ( imm_c_postcomp ~ treat_c_pos * proimm_shift_c + imm_c_pre * proimm_shift_c, data = imm )
		summary( direction_IMM_con )
	
	# Immigration. Labour Party
		direction_IMM_lab <- lm ( imm_l_postcomp ~ treat_l_pos * proimm_shift_l + imm_l_pre * proimm_shift_l, data = imm )
		summary( direction_IMM_lab )
	
	# NHS Conservative Party
		direction_NHS_con <- lm( con_post ~ treat_c_pos * pronhs_shift_c + nhs_con * pronhs_shift_c, data = nhs )
		summary( direction_NHS_con )

	# NHS Labour Party
		direction_NHS_lab <- lm( lab_post ~ treat_l_pos * pronhs_shift_l + nhs_lab * pronhs_shift_l, data = nhs )
		summary( direction_NHS_lab )


	# IMMIGRATION, Conservative Party. top-left plot.
		direction_IMM_con_MARG <- margins( direction_IMM_con, at = list( proimm_shift_c = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "treat_c_pos" )
		direction_IMM_con_MARG$STATEMENT <- factor( direction_IMM_con_MARG$proimm_shift_c, levels = c( 1, 0 ), labels = c(  "PRO-IMM", "ANTI-IMM" ) )
		
			margPLOT_direction_IMM_con <- ggplot( data = direction_IMM_con_MARG ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 1 ) ) + ylab("MARGINAL EFFECT OF STATEMENT") + xlab(NULL) + ggtitle("IMMIGRATION - Conservative Party")
			margPLOT_direction_IMM_con <- margPLOT_direction_IMM_con + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = STATEMENT), size=1)  + scale_colour_manual( values = c(  "grey0", "grey60" ) )
			margPLOT_direction_IMM_con <- margPLOT_direction_IMM_con + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
			margPLOT_direction_IMM_con<- margPLOT_direction_IMM_con + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0"))
			margPLOT_direction_IMM_con

	# IMMIGRATION, Labour Party. top-right plot.
		direction_IMM_lab_MARG <- margins( direction_IMM_lab, at = list( proimm_shift_l = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "treat_l_pos" )
		direction_IMM_lab_MARG$STATEMENT <- factor( direction_IMM_lab_MARG$proimm_shift_l, levels = c( 1, 0 ), labels = c(  "PRO-IMM", "ANTI-IMM" ) )
		
			margPLOT_direction_IMM_lab <- ggplot( data = direction_IMM_lab_MARG ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 1 ) ) + ylab("MARGINAL EFFECT OF STATEMENT") + xlab(NULL) + ggtitle("IMMIGRATION - Labour Party")
			margPLOT_direction_IMM_lab <- margPLOT_direction_IMM_lab + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = STATEMENT), size=1) + scale_colour_manual( values = c(  "grey0", "grey60" ) )
			margPLOT_direction_IMM_lab <- margPLOT_direction_IMM_lab + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
			margPLOT_direction_IMM_lab<- margPLOT_direction_IMM_lab + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0"))
			margPLOT_direction_IMM_lab

	# NHS, Conservative Party. bottom-left plot.
		direction_NHS_con_MARG <- margins( direction_NHS_con, at = list( pronhs_shift_c = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "treat_c_pos" )
		direction_NHS_con_MARG$STATEMENT <- factor( direction_NHS_con_MARG$pronhs_shift_c, levels = c( 1, 0 ), labels = c(  "PRO-NHS", "ANTI-NHS" ) )
		
			margPLOT_direction_NHS_con <- ggplot( data = direction_NHS_con_MARG ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 1 ) ) + ylab("MARGINAL EFFECT OF STATEMENT") + xlab(NULL) + ggtitle("NHS - Conservative Party")
			margPLOT_direction_NHS_con <- margPLOT_direction_NHS_con + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = STATEMENT), size=1) + scale_colour_manual( values = c(  "grey0", "grey60" ) )
			margPLOT_direction_NHS_con <- margPLOT_direction_NHS_con + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
			margPLOT_direction_NHS_con<- margPLOT_direction_NHS_con + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0"))
			margPLOT_direction_NHS_con

	# NHS, Conservative Party. bottom-left plot.
		direction_NHS_lab_MARG <- margins( direction_NHS_lab, at = list( pronhs_shift_l = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "treat_l_pos" )
		direction_NHS_lab_MARG$STATEMENT <- factor( direction_NHS_lab_MARG$pronhs_shift_l, levels = c( 1, 0 ), labels = c(  "PRO-NHS", "ANTI-NHS" ) )
		
			margPLOT_direction_NHS_lab <- ggplot( data = direction_NHS_lab_MARG ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 1 ) ) + ylab("MARGINAL EFFECT OF STATEMENT") + xlab(NULL) + ggtitle("NHS - Labour Party")
			margPLOT_direction_NHS_lab <- margPLOT_direction_NHS_lab + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = STATEMENT), size=1) + scale_colour_manual( values = c(  "grey0", "grey60" ) )
			margPLOT_direction_NHS_lab <- margPLOT_direction_NHS_lab + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
			margPLOT_direction_NHS_lab<- margPLOT_direction_NHS_lab + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0"))
			margPLOT_direction_NHS_lab

	# Saving plots
		ggsave(filename = "fig_direction_IMM_con.pdf",  plot = margPLOT_direction_IMM_con, width = 6.25, height = 3 )
		ggsave(filename = "fig_direction_IMM_lab.pdf",  plot = margPLOT_direction_IMM_lab, width = 6.25, height = 3 )
		ggsave(filename = "fig_direction_NHS_con.pdf",  plot = margPLOT_direction_NHS_con, width = 6, height = 3 )
		ggsave(filename = "fig_direction_NHS_lab.pdf",  plot = margPLOT_direction_NHS_lab, width = 6, height = 3 )



#### TABLE A.4 - Effect of PRO vs ANTI immigration statements, PRO vs ANTI NHS statements


	# DV: Post-treatment perception
	# IV: Statement ( as perceived by Respondent )
	# Control: Pre-treatment placement
	# Interacting with Type of SHIFT : pro or anti imm

	# Immigration. Conservative Party
		direction_IMM_con <- lm ( imm_c_postcomp ~ treat_c_pos * proimm_shift_c + imm_c_pre * proimm_shift_c, data = imm )
		summary( direction_IMM_con )
	
	# Immigration. Labour Party
		direction_IMM_lab <- lm ( imm_l_postcomp ~ treat_l_pos * proimm_shift_l + imm_l_pre * proimm_shift_l, data = imm )
		summary( direction_IMM_lab )
	
	# NHS Conservative Party
		direction_NHS_con <- lm( con_post ~ treat_c_pos * pronhs_shift_c + nhs_con * pronhs_shift_c, data = nhs )
		summary( direction_NHS_con )

	# NHS Labour Party
		direction_NHS_lab <- lm( lab_post ~ treat_l_pos * pronhs_shift_l + nhs_lab * pronhs_shift_l, data = nhs )
		summary( direction_NHS_lab )





#### TABLE A.7 - Replication of MAIN MODEL but including attitudinal and demographic controls

	# DV: Post-treatment perception
	# IV: Statement ( as perceived by Respondent )
	# Control: Pre-treatment placement
	# Interacting with POPULAR STATEMENT
	# WITH CONTROLS

	# Immigration Experiment. Conservative Party.
		controls_popular_IMM_con <- lm ( imm_c_postcomp ~ treat_c_pos * popular_statement_con + imm_c_pre * popular_statement_con + pid + genderec + agerec + religion, data = imm )
		summary( controls_popular_IMM_con )
		
	# Immigration Experiment. Labour Party.
		controls_popular_IMM_lab <- lm ( imm_l_postcomp ~ treat_l_pos * popular_statement_lab + imm_l_pre * popular_statement_lab + pid + genderec + agerec + religion, data = imm )
		summary( controls_popular_IMM_lab )
		
	# NHS Experiment. Conservative Party.
		controls_popular_NHS_con  <- lm( con_post ~ treat_c_pos * popular_statement_con + nhs_con * popular_statement_con + pid + genderec + agerec + religion, data = nhs )
		summary( controls_popular_NHS_con )
		
	# NHS Experiment. Labour Party.
		controls_popular_NHS_lab <- lm( lab_post ~ treat_l_pos * popular_statement_lab + nhs_lab * popular_statement_lab + pid + genderec + agerec + religion, data = nhs )
		summary( controls_popular_NHS_lab )



#### TABLE A.8 - Alternative Test of the Argument

	# DV: Absolute UPDATE in the respondent's perception
	# IV: Type of statement observed 
	# Interacting with maximum potential effect
	# Common intercept
	# Adding CONTROLS

	# Immigration, Conservative Party
		absUp_Int_IMM_con <- lm( abs ( update_c ) ~  max_effect_con + max_effect_con : antiimmtreat_con + agerec + genderec + pid + educrec + imm_slf, data = imm )
		summary ( absUp_Int_IMM_con )
	
	# Immigration, Labour Party
		absUp_Int_IMM_lab <- lm( abs ( update_l ) ~  max_effect_lab + max_effect_lab : antiimmtreat_lab + agerec + genderec + pid + educrec + imm_slf, data = imm )
		summary ( absUp_Int_IMM_lab )

	# NHS, Conservative Party
		absUp_Int_NHS_con <- lm( abs ( update_c ) ~  max_effect_con + max_effect_con : pronhs_con + agerec + genderec + pid + educrec + nhs_self, data = nhs )
		summary ( absUp_Int_NHS_con )

	# NHS, Labour Party
		absUp_Int_NHS_lab <- lm( abs ( update_l ) ~  max_effect_lab + max_effect_lab : pronhs_lab + agerec + genderec + pid + educrec + nhs_self, data = nhs )
		summary ( absUp_Int_NHS_lab )



#### FIGURE A.12

	# Immigration Experiment. Conservative Party. Top-left plot
		absUp_Int_IMM_con <- lm( abs ( update_c ) ~  max_effect_con + max_effect_con : proimmtreat_con + agerec + genderec + pid + educrec + imm_slf, data = imm )
		summary ( absUp_Int_IMM_con )

		alternative_IMM_con <- margins( absUp_Int_IMM_con, at = list( proimmtreat_con = c( 0,1 ) ) ) %>% summary() %>% as.data.frame() %>% filter( factor == "max_effect_con" )
		alternative_IMM_con$TREATMENT <- factor( alternative_IMM_con$proimmtreat_con, levels = c( 1, 0 ), labels = c(  "PRO-IMM ( UNPOPULAR )", " ANTI-IMM ( POPULAR )" ) )
		alternative_IMM_con <- alternative_IMM_con %>% mutate( lower = AME - 1.65 * SE, upper = AME + 1.65 * SE )

		margPLOT_alternative_IMM_con <- ggplot( data = alternative_IMM_con ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 0.5 ) ) + ylab("EFFECT as PROPORTION of MAXIMUM POTENTIAL EFFECT") + xlab(NULL) + ggtitle("IMMIGRATION - Conservative Party")
		margPLOT_alternative_IMM_con <- margPLOT_alternative_IMM_con + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = TREATMENT), size=1)
		margPLOT_alternative_IMM_con <- margPLOT_alternative_IMM_con + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
		margPLOT_alternative_IMM_con <- margPLOT_alternative_IMM_con + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1.5 ))
		margPLOT_alternative_IMM_con	


	# Immigration Experiment. Labour Party. Top-right plot
		absUp_Int_IMM_lab <- lm( abs ( update_l ) ~  max_effect_lab + max_effect_lab : proimmtreat_lab + agerec + genderec + pid + educrec + imm_slf, data = imm )
		summary ( absUp_Int_IMM_lab )

		alternative_IMM_lab <- margins( absUp_Int_IMM_lab, at = list( proimmtreat_lab = 0:1 ) )	%>% summary() %>% as.data.frame() %>% filter( factor == "max_effect_lab" )
		alternative_IMM_lab$TREATMENT <- factor( alternative_IMM_lab$proimmtreat_lab, levels = c( 1,0 ), labels = c(  "PRO-IMM ( UNPOPULAR )", " ANTI-IMM ( POPULAR )" ) )
		alternative_IMM_lab <- alternative_IMM_lab %>% mutate( lower = AME - 1.65 * SE, upper = AME + 1.65 * SE )
	
		margPLOT_alternative_IMM_lab <- ggplot( data = alternative_IMM_lab ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 0.5 ) ) + ylab("EFFECT as PROPORTION of MAXIMUM POTENTIAL EFFECT") + xlab(NULL) + ggtitle("IMMIGRATION - Labour Party")
		margPLOT_alternative_IMM_lab <- margPLOT_alternative_IMM_lab + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = TREATMENT), size=1)
		margPLOT_alternative_IMM_lab <- margPLOT_alternative_IMM_lab + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
		margPLOT_alternative_IMM_lab <- margPLOT_alternative_IMM_lab + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1.5 ))
		margPLOT_alternative_IMM_lab


	# NHS Experiment. Conservative Party. bottom-left plot
		absUp_Int_NHS_con <- lm( abs ( update_c ) ~  max_effect_con + max_effect_con : pronhs_con + agerec + genderec + pid + educrec + nhs_self, data = nhs )
		summary ( absUp_Int_NHS_con )

			# Need to eliminate plaidcymru as a possible level in PID in order to estimate MARGINS
			nhs_new <- nhs %>% mutate( pid = factor( pid, levels = c( "conservative", "labour", "libdem", "ukip", "snp", "green", "bnp", "other", "no party", "dk" ) ) )

		alternative_NHS_con <- margins( absUp_Int_NHS_con, data = nhs_new,  at = list( pronhs_con = 1:0 ) )%>% summary() %>% as.data.frame() %>% filter( factor == "max_effect_con" )
		alternative_NHS_con$TREATMENT <- factor( alternative_NHS_con$pronhs_con, levels = c( 1,0 ), labels = c( "PRO-NHS ( POPULAR )", "ANTI-NHS ( UNPOPULAR )" ) )
		alternative_NHS_con <- alternative_NHS_con %>% mutate( lower = AME - 1.65 * SE, upper = AME + 1.65 * SE )
	
		margPLOT_alternative_NHS_con <- ggplot( data = alternative_NHS_con ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 0.5 ) ) + ylab("EFFECT as PROPORTION of MAXIMUM POTENTIAL EFFECT") + xlab(NULL) + ggtitle("NHS - Conservative Party")
		margPLOT_alternative_NHS_con <- margPLOT_alternative_NHS_con + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = TREATMENT), size=1)
		margPLOT_alternative_NHS_con <- margPLOT_alternative_NHS_con + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
		margPLOT_alternative_NHS_con <- margPLOT_alternative_NHS_con + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1.5 ))
		margPLOT_alternative_NHS_con


	# NHS Experiment. Labour Party. bottom-right plot
		absUp_Int_NHS_lab <- lm( abs ( update_l ) ~  max_effect_lab + max_effect_lab : pronhs_lab + agerec + genderec + pid + educrec + nhs_self, data = nhs )
		summary ( absUp_Int_NHS_lab )

		alternative_NHS_lab <- margins( absUp_Int_NHS_lab, data = nhs_new,  at = list( pronhs_lab = 1:0 ) )%>% summary() %>% as.data.frame() %>% filter( factor == "max_effect_lab" )
		alternative_NHS_lab$TREATMENT <- factor( alternative_NHS_lab$pronhs_lab, levels = c( 1,0 ), labels = c( "PRO-NHS ( POPULAR )", "ANTI-NHS ( UNPOPULAR )" ) )
	
		margPLOT_alternative_NHS_lab <- ggplot( data = alternative_NHS_lab ) + scale_x_continuous(limits=c( 0, 0.75 ), name="", labels=NULL, breaks=NULL) + scale_y_continuous( limits=c( 0, 0.5 ) ) + ylab("EFFECT as PROPORTION of MAXIMUM POTENTIAL EFFECT") + xlab(NULL) + ggtitle("NHS - Labour Party")
		margPLOT_alternative_NHS_lab <- margPLOT_alternative_NHS_lab + geom_linerange( aes( ymin = lower, ymax = upper, x = c( 0.25 , 0.5), linetype = TREATMENT), size=1)
		margPLOT_alternative_NHS_lab <- margPLOT_alternative_NHS_lab + geom_point( aes( y = AME, x = c( 0.25 , 0.5) ), size=2)
		margPLOT_alternative_NHS_lab <- margPLOT_alternative_NHS_lab + coord_flip() + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1.5 ))
		margPLOT_alternative_NHS_lab


	# Saving plots
		ggsave(filename = "fig_alternative_IMM_con.pdf",  plot = margPLOT_alternative_IMM_con, width = 6.5, height = 3.5 )
		ggsave(filename = "fig_alternative_IMM_lab.pdf",  plot = margPLOT_alternative_IMM_lab, width = 6.5, height = 3.5 )
		ggsave(filename = "fig_alternative_NHS_con.pdf",  plot = margPLOT_alternative_NHS_con, width = 6.5, height = 3.5 )
		ggsave(filename = "fig_alternative_NHS_lab.pdf",  plot = margPLOT_alternative_NHS_lab, width = 6.5, height = 3.5 )

