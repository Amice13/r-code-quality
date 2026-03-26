############################################
############################################
###                                      ###
### Loading and using basic PDBD Data 	 ###
###                                      ###
############################################
############################################


# Prepared by [masked name] as supplementary material for the
# manuscript "Parliaments Day-by-Day: A New Open Source Database to Answer the Question Who Was in Which Parliament, Party and Party-group When"

# Goals:
# 1) Users learn how to: 1) load PDBD data into R 2) how to generate basic parliamentary episode data that can be used in a statistical analysis


###############
# Preparation #
###############

# Change the language and date formatting to English if it is not already
	Sys.setenv(LANG = "EN")
	Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct date format does not work correctly
	Sys.getlocale(category = "LC_ALL")

# Set working directory
	setwd("......")
	getwd()

# Note: Users of this script need to use the folder called "csv_files\primary_data_frames" 
#       one level below their working directory. The PDBD data files that are 
#       provided via Dataverse need to be stored there.
# 		https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FPYGBDO&version=DRAFT

############
# Packages #
############

# Install packages if necessary
	if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
	if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
	if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
	if("httr" %in% rownames(installed.packages()) == FALSE) {install.packages("httr")}
	if("XML" %in% rownames(installed.packages()) == FALSE) {install.packages("XML")}
	if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
	if("stringi" %in% rownames(installed.packages()) == FALSE) {install.packages("stringi")}
	if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
	if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
	if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
	if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}

# Load packages
library(sqldf)
	library(rvest)
	library(gtools)
	library(httr)
	library(XML)
	library(stringr)
	library(stringi)
	library(dplyr)
	library(data.table)
	library(lubridate)
	library(ggplot2)

#################

# Load the PDBD data

#################

 ## about politicians

	# Load POLI - politician level data: basic not time varying biographical information like gender
	POLI = read.csv("./csv_files/primary_data_frames/POLI_2020-07-10_1004.csv", header = TRUE, sep = ";")
	
	# some fixes to make our lives easier below
	#	POLI <- POLI[,1:21] # temporary fix
		POLI$twitter_screen_name <- as.character(POLI$twitter_screen_name)
		POLI$twitter_screen_name[which(nchar(POLI$twitter_screen_name) == 0)] <- NA
		POLI$twitter_id <- as.character(POLI$twitter_id)
		POLI$twitter_id[which(nchar(POLI$twitter_id) == 0)] <- NA

#	summary(POLI)
	names(POLI)
	head(POLI)
	nrow(POLI)

	# Load RESE - resume entries
	RESE = read.csv("./csv_files/primary_data_frames/RESE_2020-07-10_1004.csv", header = TRUE, sep = ";")
#	summary(RESE)
	names(RESE)
	head(RESE)
	RESE$pf_position[which(RESE$pf_position == 1)] <- "01"

	# Load MEME - membership eppisodes; what parties where polticians a member of when
	MEME = read.csv("./csv_files/primary_data_frames/MEME_2020-07-10_1004.csv", header = TRUE, sep = ";")
#	summary(MEME)
	names(MEME)
	head(MEME)
	nrow(MEME)
	
	# Load PARE - parliamentary eppisodes; what parliaments where politicians in, with some other parliament level information
	PARE = read.csv("./csv_files/primary_data_frames/PARE_2020-07-10_1004.csv", header = TRUE, sep = ";")
#	summary(PARE)
	names(PARE)
	head(PARE)
	nrow(PARE)

 ## about institutions and other (institutional) contexts

	# Load PARL - all information on the level of parliaments, for example their first day in session
	PARL = read.csv("./csv_files/primary_data_frames/PARL_2020-07-10_1004.csv", header = TRUE, sep = ";")
#	summary(PARL)
	names(PARL)
	head(PARL)
	nrow(PARL)
	
	PARL$country_abb <- substr(PARL$parliament_id,0,2)

	# Load PART, all information on parties; connects to ParlGov when applicable
	PART = read.csv("./csv_files/primary_data_frames/PART_2020-07-10_1004.csv", header = TRUE, sep = ";")
#	summary(PART)
	names(PART)
	head(PART)
	nrow(PART)

	# Load FACT, information on factions
	FACT = read.csv("./csv_files/primary_data_frames/FACT_2020-07-10_1004.csv", header = TRUE, sep = ";")
#	summary(FACT)
	names(FACT)
	head(FACT)
	nrow(FACT)
	
#################

# Building up a parliamentary eppisode data-frame, with politicians as often as they occur in parliaments and their state (e.g. party membership, tenure e.t.c.) at the start of each parliament

#################
	
	########
	#### we start with PARL and select the parliaments we are interested in
	########
	
		# lets see how many parliaments there are currently in the data
			nrow(PARL)
	
		# we can choose between the following countries
			table(PARL$country_abb)
		# and the following 'levels' (e.g. national, local e.t.c): see codebook!
			table(PARL$level)
	
		# lets select german national parliaments
			PARLBU <- PARL[which(PARL$country_abb == "DE" & PARL$level == "NT"),]

			# how many where there of these?
			nrow(PARLBU)
			
			# lets inspect these, to see what variables we have for them
			PARLBU
			
		# right, we can see some dates..  let's use this to focus on parliaments within a certain date range
		
			# for this we first need to get the human readable dates into the internal R-date format, the most computational efficient way to do this is like this, from the package 'lubridate'
			
				PARLBU$leg_period_start_asdate <- as.Date(fast_strptime(as.character(PARLBU$leg_period_start),"%d%b%Y")) 
				PARLBU$leg_period_end_asdate <- as.Date(fast_strptime(as.character(PARLBU$leg_period_end),"%d%b%Y")) 
	
				# lets inspect if these dates where indeed added correctly
				head(PARLBU)

			# now, lets focus on parliaments that started after the start of 1950 and before the end of 1980
			
				# define the start-cutoff and the end cutoff
				startcutoff <- as.Date(fast_strptime("01jan1950","%d%b%Y"))
				endcutoff <- as.Date(fast_strptime("31dec1980","%d%b%Y"))
			
				# and do the reduction
				PARLBU <- PARLBU[which(	PARLBU$leg_period_start_asdate >= startcutoff & 
										PARLBU$leg_period_end_asdate <= endcutoff
										
										),]
			
				# lets inspect the result of this
				PARLBU
		
		
	########
	#### we know use the resume entries (RESE) to see who (POLI) where in these parliaments
	########
	
		# this is how many resume entries we currently have per country_abb
			table(RESE$country)
	
		# we again want to focus on Germany
			nrow(RESE)
			RESEBU <- RESE[which(RESE$country == "DE"),]
			nrow(RESEBU)
						
		# get the dates in RESE in the right format
		
			# the PDBD data contains markers of 'left censoring' [[lcen]] and right censoring [[rcen]] when we only know for sure that a date what 'at least from this date onward' or 'at least until this date' respectively, the script user on purpose needs to explicitly deal with this censoring, otherwise the conversion to R-dates will be unsuccessfull.
			
				RESEBU$res_entry_start <- gsub("[[lcen]]", "", RESEBU$res_entry_start, fixed = TRUE)
				RESEBU$res_entry_end <- gsub("[[rcen]]", "", RESEBU$res_entry_end, fixed = TRUE)
			
			# conversion to the internal R date format
				RESEBU$res_entry_start_asdate <- as.Date(fast_strptime(as.character(RESEBU$res_entry_start),"%d%b%Y")) 
				RESEBU$res_entry_end_asdate <- as.Date(fast_strptime(as.character(RESEBU$res_entry_end),"%d%b%Y")) 
	
		# as we could see above, RESE contains a lot of resume entries, we are however at this stage, only interested in the ones about membership to the german bundestag
			# these entries have the following 'political function codes' (see the codebook)
				# pf_geo_level = NT
				# pf_instdomain	= LE and 'LE-LH' if there is a lower and upper house
				# pf_orglevel = T3
				# pf_policy_area = NA
				# pf_position = 01
				
			# so lets select these
				nrow(RESEBU)
				head(RESEBU)
				RESEBU <- RESEBU[which(
									RESEBU$pf_geolevel == "NT" & 
									(RESEBU$pf_instdomain == "LE" | RESEBU$pf_instdomain == "LE-LH") &  # '|' signifies an OR here.
									RESEBU$pf_orglevel == "T3" & 
									is.na(RESEBU$pf_policy_area) & 
									RESEBU$pf_position == "01"
										),]
				# let's inspect the result
				nrow(RESEBU)
				head(RESEBU)
				table(RESEBU$country)			

				
		# now we use a SQL query with a so called 'left join' to add all of the German MPS that where in parliament 
			
			POPA <-	sqldf("SELECT PARLBU.parliament_id, PARLBU.leg_period_start_asdate, RESEBU.pers_id, RESEBU.res_entry_start_asdate, RESEBU.country
							FROM PARLBU LEFT JOIN RESEBU
							ON
								PARLBU.country_abb = RESEBU.country
								AND
								PARLBU.leg_period_start_asdate >= RESEBU.res_entry_start_asdate
								AND
								PARLBU.leg_period_start_asdate <= RESEBU.res_entry_end_asdate
						  ")
				  
			# 'unpacking' this SQL query
				# "SELECT PARLBU.parliament_id, RESEBU.pers_id" e.t.c. 
					 # meaning >>#  give me these two variables from these two data-frames - please not how SQL uses dots ('.') to seperate the variable (parliament_id) from its data-frame (PARLBU) and not '$' as is common in R.
					
				# "FROM PARLBU LEFT JOIN RESEBU"
					# meaning >># use the rows in PARLBU as the basis, and add as many rows from RESEBU to PARLBU as you can find matches.  Note: If you use WHERE instead of ON in the SQL query, rows in PARLBU get dropped if no matches in RESEBU can be found
					
				# "ON
				#				PARLBU.country_abb = RESEBU.country
				#				AND
				#				PARLBU.leg_period_start_asdate <= RESEBU.res_entry_start_asdate
				#				AND
				#				PARLBU.leg_period_start_asdate <= RESEBU.res_entry_end_asdate" > 
					# meaning >># match on country, and on dates: get all entries from RESEBU that started before or on the first day of each parliament and ended after (or on that day itself). So basically: pick a point on a time-line and give me all episodes that 'touch' this dot.
			
			# get rid of possible duplicates (pers_id parliament id combos) - in an ideal world these would not occur, but our experience thought us that in reality there is almost always a couple, no matter how hard we tried to avoid this. Please note that, for t
				
				# we make a string that contains person_parliament combinations
				POPA$fake_parl_episode_id <- paste(POPA$pers_id,POPA$parliament_id,sep="__")
				
				# how many duplicates are there? (number of TRUE indicates number of duplicates)
				table(duplicated(POPA$fake_parl_episode_id))
				
				# we run the reduction(nevertheless)
				nrow(POPA)
				POPABU <- POPA[which(!duplicated(POPA$fake_parl_episode_id)),]
				nrow(POPABU)
			
			# inspecting the result (which has the basic format we specified above: politicians at their first day in parliament).
			
				# looking at a snippit of the raw data - looking at the first twenty lines
				POPABU[1:20,]
				
				
				# some cross tabs to see if the numbers make sense (check the totals you see here with official sources!).
				table(POPABU$parliament_id)
	
	########
	#### having the basis structure of the data, let's enrich it with with some more information
	########	
	
		####
		## adding static individual level characteristics from POLI
		####
		
			# we again use LEFT JOIN, we write the results to a temporary data-frame so we can check if we do not accidentally have to many matches e.t.c.
			
				# first, lets have a look at the information we can found on politicians in POLI
					# look at top rows
					head(POLI)
				
					# how many from what country?
					
						# get country from primarty identifier (pers_id) by selecting its left two characters
						POLI$country <- substr(POLI$pers_id,0,2)
						
						# how many politicians are there in the data-frame from each country?, answer: plenty
						table(POLI$country)
						
			
			# running the query and checking what it does to our numbers of rows properly
			
				# checking the numbers before, at this point we have all of the people we need (not more, and not less!) in our data, so we don't want to loose or gain any rows in the data anymore!
					nrow(POPABU)
					
				# run the query
					TEMP <- sqldf("SELECT POPABU.pers_id, POPABU.parliament_id, POPABU.leg_period_start_asdate, POLI.first_name, POLI.last_name, POLI.gender, POLI.birth_date
								   FROM POPABU LEFT JOIN POLI
								   ON 
								   POPABU.pers_id = POLI.pers_id
								 ")
					nrow(TEMP)			
				
				# only continue / use this if we are happy with the numbers
					if(nrow(POPABU) == nrow(TEMP))
					{
						POPABU <- TEMP
					}
					
		
				# so, what does our data now look like?
					POPABU[0:10,]
			
					# inspect our result, lets see how many women their where in each parliament at this first day
					
						# first we need to aggregate (using 'tidyverse' solution)
							FORPLOT <- as.data.frame(POPABU %>% count(parliament_id,gender))
							
						# and we need the data when the parliament started
							# the .* below means 'everthing from FORPLOT')
							FORPLOT <- sqldf("SELECT FORPLOT.*, PARLBU.leg_period_start_asdate
											  FROM FORPLOT LEFT JOIN PARLBU
											  ON 
											  FORPLOT.parliament_id = PARLBU.parliament_id
											")
						# and country again as well (in case we need it at some point)
							FORPLOT$country <- substr(FORPLOT$parliament_id,0,2)
											
						# inspect the results (top 10 rows)
							FORPLOT[0:10,]
							
						# plot the result
							ggplot(data=FORPLOT, aes(x = leg_period_start_asdate, y = n, colour=gender)) +
							geom_line() +
							ylim(0,700) +
							ylab("Number of men and women") +
							facet_grid(country ~ .)
		
		####
		## adding party membership (MEME) and some party level information (PART)
		####		
		
		
			##
			# who was in what party when?
			##
		
			# what we are going to do here is very simular to what with did with POLI, with the one complication that we now need to take dates into account as well! (because people can switch parties!)
			
				# inspect MEME, look at somebody who switched
				MEME[which(MEME$pers_id == "NL_Wilders_Geert_1963"),]
				# we can see that he occurs as often in MEME as he was a member of different parties, and that these eppisodes are marked with dates, we thus needs these dates to know who was a member when
				
			# before proceeding, we need to again decide what to do with right and left censoring and transform things to the internal R format
				MEME$memep_startdate <- gsub("[[lcen]]", "", MEME$memep_startdate, fixed = TRUE)
				MEME$memep_enddate <- gsub("[[rcen]]", "", MEME$memep_enddate, fixed = TRUE)
				MEME$memep_startdate_asdate <- as.Date(fast_strptime(as.character(MEME$memep_startdate),"%d%b%Y")) 
				MEME$memep_enddate_asdate <- as.Date(fast_strptime(as.character(MEME$memep_enddate),"%d%b%Y")) 
			
				#inspect
				head(MEME)
			
			# now let's run the query, same setup we did above (pick a point on a time-line (entry date to parliament) and give me all episodes (in MEME) that 'touch' this dot, for this person(!)).
			
				nrow(POPABU)
			
				TEMP <- sqldf("SELECT POPABU.*, MEME.party_id
								   FROM POPABU LEFT JOIN MEME
								   ON 
								   POPABU.pers_id = MEME.pers_id
								   AND
									POPABU.leg_period_start_asdate >= MEME.memep_startdate_asdate
									AND
									POPABU.leg_period_start_asdate <= MEME.memep_enddate_asdate
								 ")
					nrow(TEMP)
				
				# only continue / use this if we are happy with the numbers
					if(nrow(POPABU) == nrow(TEMP))
					{
						POPABU <- TEMP
					}	
			
			# inspecting the results

				# top of data
				head(POPABU)
				
				# do the numbers make sense? (we use 'droplevels' here, because party_id is a factor and otherwise the table returns a lot of empty strings).
				table(droplevels(POPABU$party_id))
				
				# and graph them over time
				
						# aggreate to desired level using 'tidyverse' solution)
							FORPLOT2 <- as.data.frame(POPABU %>% count(parliament_id,party_id))
							
						# and we need the data when the parliament started
							# the .* below means 'everthing from FORPLOT2')
							FORPLOT2 <- sqldf("SELECT FORPLOT2.*, PARLBU.leg_period_start_asdate
											  FROM FORPLOT2 LEFT JOIN PARLBU
											  ON 
											  FORPLOT2.parliament_id = PARLBU.parliament_id
											")
						# and country again as well (in case we need it at some point)
							FORPLOT2$country <- substr(FORPLOT2$parliament_id,0,2)
											
						# inspect the results (top 10 rows)
							FORPLOT2[0:10,]
							
						# plot the result
							ggplot(data=FORPLOT2, aes(fill=party_id,x = leg_period_start_asdate, y = n)) +
							geom_bar(position="stack", stat="identity") +
							facet_grid(country ~ .) +
							ylab("Frequency")
		
			##
			# get in some additional party characteristics, like the party its party_id
			##
			
				# PART contains the party level information
			
				TEMP <- sqldf("SELECT POPABU.*, PART.party_abb, PART.party_parlgov_id
								   FROM POPABU LEFT JOIN PART
								   ON 
								   POPABU.party_id = PART.party_id
								 ")
				nrow(TEMP)
				nrow(POPABU)
				
				# only continue / use this if we are happy with the numbers
					if(nrow(POPABU) == nrow(TEMP))
					{
						POPABU <- TEMP
					}	
				
				# inspect
				POPABU[1:10,]	
				
				
#################

# Excercises 

#################
	
	# exercise A # adjust the script above (around line 180) to select a set of more recent parliaments
	
	# exercise B # adjust the script above to (around line 160 and line 210) select the desired data from both Switserland (CH), Germany (DE) and the Netherlands (NL)
		# Hint 1: you will need to add the correct country filter twice
		
		# Hint 2: adding additional data might bring to light new issues in the underlying data. Start with a fresh R instance to make sure that you are not incidentally working with 'old' data at some point. After adding new data, don't run the script as a whole but run it 'line by line' so you can deal with new issues if they come up. 
		
		# Hint 3: In Switserland party membership is in MEME is coded for the regional party. This means that the graph around line 500 has to many levels. If you have time you can add the 'mother_party_id' to the query on PART and move the graph down so you can use the national parties IDs as well in Switserland for the frequency counts.
		
	# exercise C # adjust the script above (around line 320) to also include the twitter screen name and twitter ID from POLI. Inspect for how many politicians we have this information and how this information is distributed accross countries. 
		# Hint 1: use table(is.na(...))
		
		# Hint 2: to be able to do this you will also need to generate or keep the country variable!
	
	# exercise D: adjust the script above to look for the last none-political function in the Netherlands MPS had before they entered parliament (use the codebook - page 12 or 14 - to find out what variable you need to filter on to look at none-political functions!) - if you have time-left, make an graphic that shows how much data currently already marked as 'prof' we actually have in what countries over time
	
	

	
