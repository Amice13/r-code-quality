#------------------------------------------------------------------------------
# this file clean microACS data 
#==============================================================================

clean_acs = function(microACS){

	# age
	setnames(microACS,'age','Age')
	
	#i) age group : note that there are people whose age = 0
	microACS[Age < 15, AgeGrp4 := 0]
	microACS[15<= Age & Age < 25, AgeGrp4 := 1]
	microACS[25<= Age & Age < 45, AgeGrp4 := 2]
	microACS[45<= Age & Age < 65, AgeGrp4 := 3]
	microACS[65<= Age , AgeGrp4 := 4]
	
	#ii) Born in usa
	#Citizenship status
	#1. Born in the U.S.
	#2. Born in Puerto Rico, Guam, the U.S. Virgin Islands, or the Northern Marianas
	#3. Born abroad of American parent(s)
	#4. U.S. citizen by naturalization
	#5. Not a citizen of the U.S.
	#microACS[,table(bpl)]
	microACS[bpl %in% 1:115, BornUSA := 1]
	microACS[bpl %in% 121:950, BornUSA := 0]

	#iii) Race
	# race code : RAC1P
	#1 .White alone
	#2 .Black or African American alone
	#3 .American Indian alone
	#4 .Alaska Native alone
	#5 .American Indian and Alaska Native tribes specified; or American
	# .Indian or Alaska Native, not specified and no other races
	#6 .Asian alone
	#7 .Native Hawaiian and Other Pacific Islander alone
	#8 .Some Other Race alone
	#9 .Two or More Races
	microACS[race %in% 1,Race5 := 1]
	microACS[race %in% 2,Race5 := 2]
	microACS[race %in% c(3),Race5 := 3]
	microACS[race %in% c(4,5,6),Race5 := 4]
	microACS[race %in% c(7,8,9) & racwht == 1,Race5 := 1]
	microACS[race %in% c(7,8,9) & racblk == 1,Race5 := 2]
	microACS[race %in% c(7,8,9) & (racamind == 1),Race5 := 3]
	microACS[race %in% c(7,8,9) & (racpacis == 1 | racasian == 1),Race5 := 4]
	microACS[race %in% c(7,8,9) & (racother == 1 ),Race5 := 5]

	#iv) Hispanic
	microACS[,Hisp := ifelse(hispan %in% 1:4, 1, 0)]
  
	#v) Marital status
	microACS[marst %in% c(1,2),MarStat5 := 1]
	microACS[marst %in% c(3),MarStat5 := 4]
	microACS[marst %in% c(4),MarStat5 := 3]
	microACS[marst %in% c(5),MarStat5 := 2]
	microACS[marst %in% c(6),MarStat5 := 5]
	#Marital status
	#1 .Married
	#2 .Widowed
	#3 .Divorced
	#4 .Separated
	#5 .Never married or under 15 years old

	#vi) Unemployed
  
	#  microACS[empstat==0,table(Age)]
  	# all 0 if age <= 15
	microACS[empstatd > 0,UnEmpl := ifelse(empstatd == 20, 1, 0)]
	microACS[empstatd > 0,NotInLabor := ifelse(empstatd == 30, 1, 0)]
	
	microACS[empstatd > 0,UnEmpl_c := ifelse(empstatd == 20, 1, 0)]
	microACS[empstatd > 0,NotInLabor_c := ifelse(empstatd == 30, 1, 0)]

	#vii) PhysProb
	vars <- c("ddrs", "dear", "deye", "dout", "dphy")
	
	#generate variable
	microACS[,PhysProb := 0]
	microACS[diffrem == 0 & diffphys == 0 & diffmob == 0 & diffcare == 0 & diffsens == 0,PhysProb := NA]
	microACS[diffrem == 2 | diffphys == 2 | diffmob == 2 | diffcare == 2 | diffsens == 2,PhysProb := 1]
	
	# years of education 
	microACS[is.na(educd) & Age < 3, EducYear := 0] # age < 3, zero years 
	microACS[educd == 1, EducYear := 0]
	microACS[educd == 2, EducYear := 0]
	microACS[educd == 10, EducYear := 4]
	microACS[educd == 11, EducYear := 4]
	microACS[educd == 12, EducYear := 4]
	microACS[educd == 13, EducYear := 4]
	microACS[educd == 14, EducYear := 1]
	microACS[educd == 15, EducYear := 2]
	microACS[educd == 16, EducYear := 3]
	microACS[educd == 17, EducYear := 4]
	microACS[educd == 20, EducYear := 8]
	microACS[educd == 21, EducYear := 6]
	microACS[educd == 22, EducYear := 5]
	microACS[educd == 23, EducYear := 6]
	microACS[educd == 24, EducYear := 8]
	microACS[educd == 25, EducYear := 7]
	microACS[educd == 26, EducYear := 8]
	microACS[educd == 30, EducYear := 9]
	microACS[educd == 40, EducYear := 10]
	microACS[educd == 50, EducYear := 11]
	microACS[educd == 61, EducYear := 11]
	microACS[educd == 62, EducYear := 12]
	microACS[educd == 63, EducYear := 12]
	microACS[educd == 64, EducYear := 12]
	microACS[educd == 65, EducYear := 12]
	microACS[educd == 71, EducYear := 13]
	microACS[educd == 81, EducYear := 14]
	microACS[educd == 101, EducYear := 16]
	microACS[educd == 114, EducYear := 18]
	microACS[educd == 115, EducYear := 18]
	microACS[educd == 116, EducYear := 20]

	# additional adjustment to match with NVDRS
	microACS[EducYear < 8, EducYear := 8]
	microACS[EducYear %in% 9:11, EducYear := 10.5]
	
	microACS[0 <= EducYear & EducYear <  12, EducGroup := 1]
	microACS[12 <= EducYear & EducYear < 13, EducGroup := 2]
	microACS[13 <= EducYear & EducYear < 16, EducGroup := 3]
	microACS[16 <= EducYear & EducYear < 17, EducGroup := 4]
	microACS[17 <= EducYear & EducYear < 21, EducGroup := 5]

# veteran status
	microACS[vetstat != 0, veteran := ifelse(vetstat == 2, 1, 0)]

	setnames(microACS,'sex','Sex')
	setnames(microACS,'year','Year')

	microACS[,DSID := 0]

	# identify puma
	setnames(microACS,'puma','Puma')
	microACS[, puma := paste0(str_pad(statefip, width=2, pad='0'),str_pad(Puma, width=5, pad='0'))]
	microACS[, statefip := str_pad(statefip, width=2, pad='0')]

	setnames(microACS,'perwt','PerWgt')
	setnames(microACS,'statefip','state')
		
	# assign unique IDs
	microACS[,uid := paste0(Year,'_',str_pad(serial,width=8,pad='0'),'_',str_pad(pernum, width=4,pad='0'))]
	
	#==============================================================================
	# adjust variables format to integer to save the space
	for (var in c('DSID','AgeGrp4','Age','Sex','Race5','Hisp','BornUSA','MarStat5',
		'UnEmpl','PhysProb','EducYear','veteran','EducGroup')) {
		acs[[var]] = as.integer(acs[[var]])
	}
	
	microACS[Year <  2012, merged_year := 2000L]
	microACS[Year >= 2012, merged_year := 2012L]
	
	# person weight is always integer
	microACS[, PerWgt := as.integer64(PerWgt)]

	# select only relevant variables
	sel_vars = c('Year','uid','merged_year','puma', 'state','PerWgt','DSID',
	  'density','metro','poverty','migrate1',
	  'Age','Sex','AgeGrp4','MarStat5','BornUSA','Race5','Hisp','EducGroup',
	  'empstat','UnEmpl','NotInLabor',
	  'PhysProb','EducYear','veteran')

	acs = microACS[,..sel_vars]

	# drop when data are missing fo ryear and state indicators
	acs = acs[!is.na(puma),]
	acs = acs[Year >= 2005,]
	acs = acs[!is.na(state),]

	# drop younger than 15
	#acs = acs[AgeGrp4 != 0,]

	# some variable specifications ...	
	acs[empstat == 0, empstat := NA]
	
	acs[, Race6 := Race5]
	acs[Hisp == 1, Race6 := 6L] # hispanic categories

	return(acs)
}



