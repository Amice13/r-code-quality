################################################
# Replication code 
# Article: The Credibility of Party Policy Rhetoric: Survey Experimental Evidence
# Author: Pablo Fernandez-Vazquez
# Purpose: Replicate results in the paper that rely apply WEIGHTS to have the convenience sample MATCH representative samples on key sociodemographic characteristics
# Source Data: 	1) immigration_SurvExp.csv
#				2) nhs_SurvExp.csv 
#				3) cipsdec2311.dta
# Software: R version 3.4.3
################################################


############ INSTALLING and LOADING PACKAGES
	install.packages("foreign", repos = "http://cran.rstudio.com")
	install.packages("Hmisc", repos = "http://cran.rstudio.com")

	library(foreign)
	library(Hmisc)


############ Set working directory
	setwd("/Users/pablofernandez/Dropbox/dissertation/survey_experiment/survey_experiment_paper/jop_short_submission/jop_short_submission_replication_files")


############# LOADING REPRESENTATIVE SAMPLE --- The (weighted) BES 2010 internet panel pre-election wave sample	################################################
# DATA SOURCE: http://bes2009-10.org/bes-data.php
# DOCUMENTATION: MEMOCIPS.pdf   AND		bes2010_precampaignquestionnaire.pdf
# DATA: cipsdec2311.dta
# WEIGHT: the weighting variable to make PRE wave representative is "w8" (see MEMOCIPS.pdf)
# NOTE: pre-campaign variables have "aa" prefix

	bes10pre <- read.dta("cipsdec2311.dta", convert.factors=F)

############# LOADING SURVEY EXPERIMENT DATA

	## Immigration
	imm <- read.csv("immigration_SurvExp.csv", header=TRUE, stringsAsFactors=FALSE)
	## NHS
	nhs <- read.csv("nhs_SurvExp.csv", header=TRUE, stringsAsFactors=FALSE)



############# RECODING SOCIODEMOGRAPHIC VARIABLES IN REPRESENTATIVE SAMPLE in order to be able to estimate PROPENSITY SCORES

	## GENDER aaq186
		bes10pre$gender <- factor(bes10pre$aaq186, levels=c(1,2), labels=c("male", "female"))

	## AGE aaq151 (needs to be rescaled to reflect actual age. See codebook)
		bes10pre$age <- 2010 - (1900 + bes10pre$aaq151 - 1) # from year of bith to AGE
	
	## AGE RECODED -- source: aaq151
		bes10pre$agerec <- NA
		bes10pre$agerec <- ifelse(bes10pre$age<=25, "18to25",bes10pre$agerec)
		bes10pre$agerec <- ifelse(bes10pre$age>25 & bes10pre$age<=35, "25to35",bes10pre$agerec)
		bes10pre$agerec <- ifelse(bes10pre$age>35 & bes10pre$age<=45, "35to45",bes10pre$agerec)
		bes10pre$agerec <- ifelse(bes10pre$age>45 & bes10pre$age<=55, "45to55",bes10pre$agerec)
		bes10pre$agerec <- ifelse(bes10pre$age>55 & bes10pre$age<=65, "55to65",bes10pre$agerec)
		bes10pre$agerec <- ifelse(bes10pre$age>65, "over65",bes10pre$agerec)
		bes10pre$agerec <- factor(bes10pre$agerec, levels=c("18to25","25to35","35to45","45to55","55to65","over65"))
	
	## EDUCATION -- source: aaq157
		table(bes10pre$age[bes10pre$aaq157==6])
		table(bes10pre$age[bes10pre$aaq157==7]) # The minimum age of those who are still in education is 18
		bes10pre$aaq157 <- ifelse((bes10pre$aaq157==6 | bes10pre$aaq157==7) & bes10pre$age<=15,1,bes10pre$aaq157)
		bes10pre$aaq157 <- ifelse((bes10pre$aaq157==6 | bes10pre$aaq157==7) & bes10pre$age==16,2,bes10pre$aaq157)
		bes10pre$aaq157 <- ifelse((bes10pre$aaq157==6 | bes10pre$aaq157==7) & bes10pre$age==17,3,bes10pre$aaq157)
		bes10pre$aaq157 <- ifelse((bes10pre$aaq157==6 | bes10pre$aaq157==7) & bes10pre$age==18,4,bes10pre$aaq157)
		bes10pre$aaq157 <- ifelse((bes10pre$aaq157==6 | bes10pre$aaq157==7) & bes10pre$age>18,5,bes10pre$aaq157)
		bes10pre$educ <- factor(bes10pre$aaq157, levels=c(1:5), labels=c("15 or younger", "16", "17", "18", "19 or older"))
	

	## MARITAL STATUS -- source: aaq160
		bes10pre$marital_st <- factor(bes10pre$aaq161, levels=c(1:6), labels=c("married", "living with partner", "separated", "divorced", "widowed", "single"))
	
	## ETHNICITY -- source: aaq177
		bes10pre$ethnicity <- factor(bes10pre$aaq177, levels=c(1:5), labels=c("white","mixed","asian/asian british","black/black british","other"))
	
	## REGION -- source aaq1
		bes10pre$aaq1 <- ifelse(bes10pre$aaq1>=6,(bes10pre$aaq1+1),bes10pre$aaq1)
		bes10pre$region <- factor(bes10pre$aaq1, levels=c(1:12), labels=c("east anglia","east midlands","greater london","north","north west", "northern ireland", "scotland","south east","south west","wales","west midlands","yorkshire and humber"))
	
	############ 	KEEPING only RELEVANT VARIABLES 
		bes4m <- data.frame(gender=bes10pre$gender, age=bes10pre$agerec, educ=bes10pre$educ, marital=bes10pre$marital_st, eth=bes10pre$ethnicity,region=bes10pre$region, repr=1, weight=bes10pre$w8_f) # REPR indicates whether the observation belongs to the representative sample // WEIGHT indicates the weight for the observation
	


############# RECODING SOCIODEMOGRAPHIC VARIABLES IN CONVENIENCE SAMPLES in order to be able to estimate PROPENSITY SCORES

	## GENDER 
		imm$gender <- factor(imm$gender, levels=c(1,2), labels=c("male", "female"))
		nhs$gender <- factor(nhs$gender, levels=c(1,2), labels=c("male", "female"))
	
	
	## AGE
		imm$age <- 2015 - imm$age # from year of birth to age  
		nhs$age <- 2015 - as.numeric(nhs$age) # from year of birth to age  
		 
	## AGE RECODED
		imm$agerec <- NA
		imm$agerec <- ifelse(imm$age<=25, "18to25",imm$agerec)
		imm$agerec <- ifelse(imm$age>25 & imm$age<=35, "25to35",imm$agerec)
		imm$agerec <- ifelse(imm$age>35 & imm$age<=45, "35to45",imm$agerec)
		imm$agerec <- ifelse(imm$age>45 & imm$age<=55, "45to55",imm$agerec)
		imm$agerec <- ifelse(imm$age>55 & imm$age<=65, "55to65",imm$agerec)
		imm$agerec <- ifelse(imm$age>65, "over65",imm$agerec)
		imm$agerec <- factor(imm$agerec, levels=c("18to25","25to35","35to45","45to55","55to65","over65"))
	
		nhs$agerec <- NA
		nhs$agerec <- ifelse(nhs$age<=25, "18to25",nhs$agerec)
		nhs$agerec <- ifelse(nhs$age>25 & nhs$age<=35, "25to35",nhs$agerec)
		nhs$agerec <- ifelse(nhs$age>35 & nhs$age<=45, "35to45",nhs$agerec)
		nhs$agerec <- ifelse(nhs$age>45 & nhs$age<=55, "45to55",nhs$agerec)
		nhs$agerec <- ifelse(nhs$age>55 & nhs$age<=65, "55to65",nhs$agerec)
		nhs$agerec <- ifelse(nhs$age>65, "over65",nhs$agerec)
		nhs$agerec <- factor(nhs$agerec, levels=c("18to25","25to35","35to45","45to55","55to65","over65"))
	
	## EDUCATION -- 
		table(imm$age[imm$educ==6])
		table(imm$age[imm$educ==7])
		imm$educ <- ifelse((imm$educ==6 | imm$educ==7) & imm$age<=15,1,imm$educ)
		imm$educ <- ifelse((imm$educ==6 | imm$educ==7) & imm$age==16,2,imm$educ)
		imm$educ <- ifelse((imm$educ==6 | imm$educ==7) & imm$age==17,3,imm$educ)
		imm$educ <- ifelse((imm$educ==6 | imm$educ==7) & imm$age==18,4,imm$educ)
		imm$educ <- ifelse((imm$educ==6 | imm$educ==7) & imm$age>18,5,imm$educ)
		imm$educ <- ifelse(imm$educ==99,NA,imm$educ)
		imm$educ <- factor(imm$educ, levels=c(1:5), labels=c("15 or younger", "16", "17", "18", "19 or older"))
	
		table(nhs$age[nhs$educ==6])
		table(nhs$age[nhs$educ==7])
		nhs$educ <- ifelse((nhs$educ==6 | nhs$educ==7) & nhs$age<=15,1,nhs$educ)
		nhs$educ <- ifelse((nhs$educ==6 | nhs$educ==7) & nhs$age==16,2,nhs$educ)
		nhs$educ <- ifelse((nhs$educ==6 | nhs$educ==7) & nhs$age==17,3,nhs$educ)
		nhs$educ <- ifelse((nhs$educ==6 | nhs$educ==7) & nhs$age==18,4,nhs$educ)
		nhs$educ <- ifelse((nhs$educ==6 | nhs$educ==7) & nhs$age>18,5,nhs$educ)
		nhs$educ <- ifelse(nhs$educ==99,NA,nhs$educ)
		nhs$educ <- factor(nhs$educ, levels=c(1:5), labels=c("15 or younger", "16", "17", "18", "19 or older"))
	
	
	## MARITAL STATUS -- source: aaq160
		imm$marital_st <- factor(imm$marital_st, levels=c(1:6), labels=c("married", "living with partner", "separated", "divorced", "widowed", "single"))
		nhs$marital_st <- factor(nhs$marital_st, levels=c(1:6), labels=c("married", "living with partner", "separated", "divorced", "widowed", "single"))
	
	## ETHNICITY
		imm$ethnicity <- ifelse(imm$ethnicity==99,NA,imm$ethnicity)
		imm$ethnicity <- factor(imm$ethnicity, levels=c(1:5), labels=c("white","mixed","asian/asian british","black/black british","other"))
		
		nhs$ethnicity <- ifelse(nhs$ethnicity==99,NA,nhs$ethnicity)
		nhs$ethnicity <- factor(nhs$ethnicity, levels=c(1:5), labels=c("white","mixed","asian/asian british","black/black british","other"))
	
	## REGION
		imm$region <- factor(imm$region, levels=c(1:12), labels=c("east anglia","east midlands","greater london","north","north west", "northern ireland", "scotland","south east","south west","wales","west midlands","yorkshire and humber"))
		nhs$region <- factor(nhs$region, levels=c(1:12), labels=c("east anglia","east midlands","greater london","north","north west", "northern ireland", "scotland","south east","south west","wales","west midlands","yorkshire and humber"))
	
	
	############ 	Keeping only relevant variable
	imm4m <- data.frame(gender=imm$gender, age=imm$agerec, educ=imm$educ, marital=imm$marital_st, eth=imm$ethnicity,region=imm$region, repr=0, weight=1) # REPR indicates whether the observation belongs to the representative sample // WEIGHT indicates the weight for the observation
	nhs4m <- data.frame(gender=nhs$gender, age=nhs$agerec, educ=nhs$educ, marital=nhs$marital_st, eth=nhs$ethnicity,region=nhs$region, repr=0, weight=1) # REPR indicates whether the observation belongs to the representative sample // WEIGHT indicates the weight for the observation




############## MERGING DATASETS
	besimm <- rbind(imm4m,bes4m)
	besnhs <- rbind(nhs4m,bes4m)


############# CALCULATING PROPENSITY SCORES
	propaltimm <- glm(repr ~ gender + age + educ + marital + eth + region, data=besimm, weights=weight ,family=binomial(link=logit))
	propaltnhs <- glm(repr ~ gender + age + educ + marital + eth + region, data=besnhs, weights=weight ,family=binomial(link=logit))


### Including prop scores in datasets

	## IMMiGRATION

	# First I need to identify for which observations the propensity score is computed
		besimm$psaltexist <- complete.cases(subset(besimm, select=c(repr,gender , age , educ , marital , eth , region,weight)))
	# adjudicating the propensity score
		besimm$psalt <- NA
		besimm$psalt[besimm$psaltexist==T] <- propaltimm$fitted.values
	# dropping cases in BES sample
		immps <- subset(besimm, repr==0)
		prop.table(table(immps$psaltexist)) # 5% of cases in IMMPS are dropped because I cannot compute prop scores
	# NORMALIZING WEIGHTS
		mean( immps$psalt, na.rm = TRUE )
		immps$psaltnorm <- immps$psalt / mean( immps$psalt, na.rm = TRUE )
	# merge with original IMMIGRATION dataset
		immwgt <- cbind(imm, psalt=immps$psalt, psaltnorm = immps$psaltnorm)

	## NHS

	# First I need to identify for which observations the propensity score is computed
		besnhs$psaltexist <- complete.cases(subset(besnhs, select=c(repr,gender , age , educ , marital , eth , region,weight)))
	# adjudicating the propensity score
		besnhs$psalt <- NA
		besnhs$psalt[besnhs$psaltexist==T] <- propaltnhs$fitted.values
	# dropping cases in BES sample
		nhsps <- subset(besnhs, repr==0)
		prop.table(table(nhsps$psaltexist)) # 4% of cases in IMMPS are dropped because I cannot compute prop scores
	# NORMALIZING WEIGHTS
		mean( nhsps$psalt, na.rm = TRUE )
		nhsps$psaltnorm <- nhsps$psalt / mean( nhsps$psalt, na.rm = TRUE )
	# merge with original IMMIGRATION dataset
		nhswgt <- cbind(nhs, psalt=nhsps$psalt, psaltnorm = nhsps$psaltnorm)




####################	TABLE A.5	- COMPARING THE DEMOGRAPHIC CHARACTERISTICS OF THE CROWDFLOWER CONTRIBUTORS AGAINST A REPRESENTATIVE SAMPLE OF BRITISH VOTERS.

	# GENDER
		nrow( subset ( imm, gender == "female" ) ) / sum( as.numeric( !is.na( imm$gender ) ) )
		nrow( subset ( nhs, gender == "female" ) ) / sum( as.numeric( !is.na( nhs$gender ) ) )
		nrow( subset ( bes10pre, gender == "female" ) ) / sum( as.numeric( !is.na( bes10pre$gender ) ) )

	# AGE
		nrow( subset ( imm, agerec == "18to25" ) ) / sum( as.numeric( !is.na( imm$agerec ) ) )
		nrow( subset ( nhs, agerec == "18to25" ) ) / sum( as.numeric( !is.na( nhs$agerec ) ) )
		nrow( subset ( bes10pre, agerec == "18to25" ) ) / sum( as.numeric( !is.na( bes10pre$agerec ) ) )

		nrow( subset ( imm, agerec == "25to35" ) ) / sum( as.numeric( !is.na( imm$agerec ) ) )
		nrow( subset ( nhs, agerec == "25to35" ) ) / sum( as.numeric( !is.na( nhs$agerec ) ) )
		nrow( subset ( bes10pre, agerec == "25to35" ) ) / sum( as.numeric( !is.na( bes10pre$agerec ) ) )

		nrow( subset ( imm, agerec == "35to45" ) ) / sum( as.numeric( !is.na( imm$agerec ) ) )
		nrow( subset ( nhs, agerec == "35to45" ) ) / sum( as.numeric( !is.na( nhs$agerec ) ) )
		nrow( subset ( bes10pre, agerec == "35to45" ) ) / sum( as.numeric( !is.na( bes10pre$agerec ) ) )

		nrow( subset ( imm, agerec == "45to55" ) ) / sum( as.numeric( !is.na( imm$agerec ) ) )
		nrow( subset ( nhs, agerec == "45to55" ) ) / sum( as.numeric( !is.na( nhs$agerec ) ) )
		nrow( subset ( bes10pre, agerec == "45to55" ) ) / sum( as.numeric( !is.na( bes10pre$agerec ) ) )

		nrow( subset ( imm, agerec == "55to65" ) ) / sum( as.numeric( !is.na( imm$agerec ) ) )
		nrow( subset ( nhs, agerec == "55to65" ) ) / sum( as.numeric( !is.na( nhs$agerec ) ) )
		nrow( subset ( bes10pre, agerec == "55to65" ) ) / sum( as.numeric( !is.na( bes10pre$agerec ) ) )

		nrow( subset ( imm, agerec == "over65" ) ) / sum( as.numeric( !is.na( imm$agerec ) ) )
		nrow( subset ( nhs, agerec == "over65" ) ) / sum( as.numeric( !is.na( nhs$agerec ) ) )
		nrow( subset ( bes10pre, agerec == "over65" ) ) / sum( as.numeric( !is.na( bes10pre$agerec ) ) )

	# EDUCATION
		nrow( subset ( imm, educ == "15 or younger" ) ) / sum( as.numeric( !is.na( imm$educ ) ) )
		nrow( subset ( nhs, educ == "15 or younger" ) ) / sum( as.numeric( !is.na( nhs$educ ) ) )
		nrow( subset ( bes10pre, educ == "15 or younger" ) ) / sum( as.numeric( !is.na( bes10pre$educ ) ) )

		nrow( subset ( imm, educ == "16" ) ) / sum( as.numeric( !is.na( imm$educ ) ) )
		nrow( subset ( nhs, educ == "16" ) ) / sum( as.numeric( !is.na( nhs$educ ) ) )
		nrow( subset ( bes10pre, educ == "16" ) ) / sum( as.numeric( !is.na( bes10pre$educ ) ) )

		nrow( subset ( imm, educ == "17" ) ) / sum( as.numeric( !is.na( imm$educ ) ) )
		nrow( subset ( nhs, educ == "17" ) ) / sum( as.numeric( !is.na( nhs$educ ) ) )
		nrow( subset ( bes10pre, educ == "17" ) ) / sum( as.numeric( !is.na( bes10pre$educ ) ) )

		nrow( subset ( imm, educ == "18" ) ) / sum( as.numeric( !is.na( imm$educ ) ) )
		nrow( subset ( nhs, educ == "18" ) ) / sum( as.numeric( !is.na( nhs$educ ) ) )
		nrow( subset ( bes10pre, educ == "18" ) ) / sum( as.numeric( !is.na( bes10pre$educ ) ) )

		nrow( subset ( imm, educ == "19 or older" ) ) / sum( as.numeric( !is.na( imm$educ ) ) )
		nrow( subset ( nhs, educ == "19 or older" ) ) / sum( as.numeric( !is.na( nhs$educ ) ) )
		nrow( subset ( bes10pre, educ == "19 or older" ) ) / sum( as.numeric( !is.na( bes10pre$educ ) ) )


	# MARITAL STATUS
		nrow( subset ( imm, marital_st == "married" ) ) / sum( as.numeric( !is.na( imm$marital_st ) ) )
		nrow( subset ( nhs, marital_st == "married" ) ) / sum( as.numeric( !is.na( nhs$marital_st ) ) )
		nrow( subset ( bes10pre, marital_st == "married" ) ) / sum( as.numeric( !is.na( bes10pre$marital_st ) ) )

		nrow( subset ( imm, marital_st == "living with partner" ) ) / sum( as.numeric( !is.na( imm$marital_st ) ) )
		nrow( subset ( nhs, marital_st == "living with partner" ) ) / sum( as.numeric( !is.na( nhs$marital_st ) ) )
		nrow( subset ( bes10pre, marital_st == "living with partner" ) ) / sum( as.numeric( !is.na( bes10pre$marital_st ) ) )

		nrow( subset ( imm, marital_st == "separated" ) ) / sum( as.numeric( !is.na( imm$marital_st ) ) )
		nrow( subset ( nhs, marital_st == "separated" ) ) / sum( as.numeric( !is.na( nhs$marital_st ) ) )
		nrow( subset ( bes10pre, marital_st == "separated" ) ) / sum( as.numeric( !is.na( bes10pre$marital_st ) ) )

		nrow( subset ( imm, marital_st == "divorced" ) ) / sum( as.numeric( !is.na( imm$marital_st ) ) )
		nrow( subset ( nhs, marital_st == "divorced" ) ) / sum( as.numeric( !is.na( nhs$marital_st ) ) )
		nrow( subset ( bes10pre, marital_st == "divorced" ) ) / sum( as.numeric( !is.na( bes10pre$marital_st ) ) )

		nrow( subset ( imm, marital_st == "widowed" ) ) / sum( as.numeric( !is.na( imm$marital_st ) ) )
		nrow( subset ( nhs, marital_st == "widowed" ) ) / sum( as.numeric( !is.na( nhs$marital_st ) ) )
		nrow( subset ( bes10pre, marital_st == "widowed" ) ) / sum( as.numeric( !is.na( bes10pre$marital_st ) ) )

		nrow( subset ( imm, marital_st == "single" ) ) / sum( as.numeric( !is.na( imm$marital_st ) ) )
		nrow( subset ( nhs, marital_st == "single" ) ) / sum( as.numeric( !is.na( nhs$marital_st ) ) )
		nrow( subset ( bes10pre, marital_st == "single" ) ) / sum( as.numeric( !is.na( bes10pre$marital_st ) ) )

	# RACE
		nrow( subset ( imm, ethnicity == "white" ) ) / sum( as.numeric( !is.na( imm$ethnicity ) ) )
		nrow( subset ( nhs, ethnicity == "white" ) ) / sum( as.numeric( !is.na( nhs$ethnicity ) ) )
		nrow( subset ( bes10pre, ethnicity == "white" ) ) / sum( as.numeric( !is.na( bes10pre$ethnicity ) ) )

		nrow( subset ( imm, ethnicity == "mixed" ) ) / sum( as.numeric( !is.na( imm$ethnicity ) ) )
		nrow( subset ( nhs, ethnicity == "mixed" ) ) / sum( as.numeric( !is.na( nhs$ethnicity ) ) )
		nrow( subset ( bes10pre, ethnicity == "mixed" ) ) / sum( as.numeric( !is.na( bes10pre$ethnicity ) ) )

		nrow( subset ( imm, ethnicity == "asian/asian british" ) ) / sum( as.numeric( !is.na( imm$ethnicity ) ) )
		nrow( subset ( nhs, ethnicity == "asian/asian british" ) ) / sum( as.numeric( !is.na( nhs$ethnicity ) ) )
		nrow( subset ( bes10pre, ethnicity == "asian/asian british" ) ) / sum( as.numeric( !is.na( bes10pre$ethnicity ) ) )

		nrow( subset ( imm, ethnicity == "black/black british" ) ) / sum( as.numeric( !is.na( imm$ethnicity ) ) )
		nrow( subset ( nhs, ethnicity == "black/black british" ) ) / sum( as.numeric( !is.na( nhs$ethnicity ) ) )
		nrow( subset ( bes10pre, ethnicity == "black/black british" ) ) / sum( as.numeric( !is.na( bes10pre$ethnicity ) ) )

		nrow( subset ( imm, ethnicity == "other" ) ) / sum( as.numeric( !is.na( imm$ethnicity ) ) )
		nrow( subset ( nhs, ethnicity == "other" ) ) / sum( as.numeric( !is.na( nhs$ethnicity ) ) )
		nrow( subset ( bes10pre, ethnicity == "other" ) ) / sum( as.numeric( !is.na( bes10pre$ethnicity ) ) )

	# REGION
		nrow( subset ( imm, region == "east anglia" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "east anglia" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "east anglia" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "east midlands" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "east midlands" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "east midlands" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "greater london" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "greater london" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "greater london	" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "north" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "north" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "north" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "north west" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "north west" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "north west" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "northern ireland" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "northern ireland" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "northern ireland" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "scotland" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "scotland" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "scotland" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "south east" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "south east" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "south east" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "south west" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "south west" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "south west" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "wales" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "wales" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "wales" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "west midlands" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "west midlands" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "west midlands" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )

		nrow( subset ( imm, region == "yorkshire and humber" ) ) / sum( as.numeric( !is.na( imm$region ) ) )
		nrow( subset ( nhs, region == "yorkshire and humber" ) ) / sum( as.numeric( !is.na( nhs$region ) ) )
		nrow( subset ( bes10pre, region == "yorkshire and humber" ) ) / sum( as.numeric( !is.na( bes10pre$region ) ) )




####################	TABLE A.6	- REPLICATING MODEL AFTER APPLYING WEIGHTS TO MAKE CONVENIENCE SAMPLE REPRESENTATIVE	

	# First - need to recode key outcome and predictor variables
	
	## Pos-treatment perception
		## Immigration Experiment. Conservative Party
			immwgt$imm_c_postcomp <- immwgt$imm_c_post
			immwgt$imm_c_postcomp <-ifelse(is.na(immwgt$imm_c_postcomp) & !is.na(immwgt$imm_c_post.1), immwgt$imm_c_post.1, immwgt$imm_c_postcomp)
			table( immwgt$imm_c_postcomp )
		## Immigration Experiment. Labour Party
			immwgt$imm_l_postcomp <- immwgt$imm_l_post
			immwgt$imm_l_postcomp <-ifelse(is.na(immwgt$imm_l_postcomp) & !is.na(immwgt$imm_l_post.1), immwgt$imm_l_post.1, immwgt$imm_l_postcomp)
			table( immwgt$imm_l_postcomp )
		## NHS Experiment. Conservative Party
			nhswgt$con_post <- nhswgt$con_post_d
			nhswgt$con_post <-ifelse(is.na(nhswgt$con_post) & !is.na(nhswgt$con_post_i), nhswgt$con_post_i, nhswgt$con_post)
			table( nhswgt$con_post )
		## NHS Experiment. Labour Party
			nhswgt$lab_post <- nhswgt$lab_post_d
			nhswgt$lab_post <-ifelse(is.na(nhswgt$lab_post) & !is.na(nhswgt$lab_post_i), nhswgt$lab_post_i, nhswgt$lab_post)
			table( nhswgt$lab_post )

	### POSITION EXPRESSED IN THE STATEMENT - as perceived by the respondent

		## Immigration Experiment. Conservative Party
			immwgt$treat_c_pos <- immwgt$c_restr_ch
			immwgt$treat_c_pos <- ifelse(is.na(immwgt$treat_c_pos) & !is.na(immwgt$c_open_ch), immwgt$c_open_ch, immwgt$treat_c_pos)
			table( immwgt$treat_c_pos )
		## Immigration Experiment. Labour Party
			immwgt$treat_l_pos <- immwgt$l_restr_ch
			immwgt$treat_l_pos <-ifelse(is.na(immwgt$treat_l_pos) & !is.na(immwgt$l_open_ch), immwgt$l_open_ch, immwgt$treat_l_pos)
			table( immwgt$treat_l_pos )
		## NHS Experiment. Conservative Party
			nhswgt$treat_c_pos <- nhswgt$c_dec_ch
			nhswgt$treat_c_pos <-ifelse(is.na(nhswgt$treat_c_pos) & !is.na(nhswgt$c_inc_ch), nhswgt$c_inc_ch, nhswgt$treat_c_pos)
			table( nhswgt$treat_c_pos )
		## NHS Experiment. Conservative Party
			nhswgt$treat_l_pos <- nhswgt$l_dec_ch
			nhswgt$treat_l_pos <-ifelse(is.na(nhswgt$treat_l_pos) & !is.na(nhswgt$l_inc_ch), nhswgt$l_inc_ch, nhswgt$treat_l_pos)
			table( nhswgt$treat_l_pos )
	
	## POPULAR STATEMENT - The respondent interprets that the statement reflects the position that most citizens espouse

		## Immigration Experiment. Conservative Party
			immwgt$popular_statement_con <- ifelse( ( immwgt$most_pop == 1 & immwgt$treat_c_pos < 5) | ( immwgt$most_pop == 0 & immwgt$treat_c_pos > 5 ), 1, 0 )
		## Immigration Experiment. Labour Party
			immwgt$popular_statement_lab <- ifelse( ( immwgt$most_pop == 1 & immwgt$treat_l_pos < 5 ) | ( immwgt$most_pop == 0 & immwgt$treat_l_pos > 5 ), 1, 0 )
		## NHS Experiment. Conservative Party
			nhswgt$popular_statement_con <- ifelse( ( nhswgt$most_pop == 0 & nhswgt$treat_c_pos < 5 ) | ( nhswgt$most_pop == 1 & nhswgt$treat_c_pos > 5 ), 1, 0 )
		## NHS Experiment. Conservative Party
			nhswgt$popular_statement_lab <- ifelse( ( nhswgt$most_pop == 0 & nhswgt$treat_l_pos < 5 ) | ( nhswgt$most_pop == 1 & nhswgt$treat_l_pos > 5 ), 1, 0 )



	#### MODEL - IMMIGRATION - Conservative Party
		bayesian_popular_weighted_IMM_con <- lm ( imm_c_postcomp ~ treat_c_pos * popular_statement_con + imm_c_pre * popular_statement_con, data = immwgt, weights = psaltnorm )
		summary( bayesian_popular_weighted_IMM_con )

	#### MODEL - IMMIGRATION - Labour Party
		bayesian_popular_weighted_IMM_lab <- lm ( imm_l_postcomp ~ treat_l_pos * popular_statement_lab + imm_l_pre * popular_statement_lab, data = immwgt, weights = psaltnorm )
		summary( bayesian_popular_weighted_IMM_lab )

	#### MODEL - NHS - Conservative Party
		bayesian_popular_weighted_NHS_con  <- lm( con_post ~ treat_c_pos * popular_statement_con + nhs_con * popular_statement_con, data = nhswgt, weights = psaltnorm )
		summary( bayesian_popular_weighted_NHS_con )

	#### MODEL - NHS - Labour Party
		bayesian_popular_weighted_NHS_lab <- lm( lab_post ~ treat_l_pos * popular_statement_lab + nhs_lab * popular_statement_lab, data = nhswgt, weights = psaltnorm )
		summary( bayesian_popular_weighted_NHS_lab )
