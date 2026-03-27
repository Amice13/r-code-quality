#------------------------------------------------------------------------------
# this file clean NVDRS data 
# major variables (demographics + manner of death + unemployment)
# input 1. raw data (nvdrs)
# input 2. manually cleaned occupation data (occ_df)
#==============================================================================

clean_nvdrs = function(nvdrs){

	# indicator cleaning
	nvdrs[, IncID := stringr::str_pad(IncidentNumber, width=4, pad='0')]
	nvdrs[, Site := stringr::str_pad(SiteID, width=2, pad='0')]
	nvdrs[, PerID := stringr::str_pad(VictimNumber, width=4, pad='0')]
	
	# -- Key Vars
	nvdrs[, year := IncidentYear]
	nvdrs[,InjuryDate := as.Date(InjuryDate_myr, origin=c('1960-01-01'))]
	nvdrs[,DeathDate := as.Date(DeathDate_myr, origin=c('1960-01-01'))]
	
	nvdrs[, date := DeathDate]
	nvdrs[is.na(date),date := InjuryDate]
	nvdrs[, month := month(date)]

	nvdrs[ResidenceState < 88 ,State := str_pad(ResidenceState, 2, "left", "0")]
	nvdrs[,Cnty := str_pad(ResidenceFIPS, 5, "left", "0")]
	
	nvdrs[DeathState < 88 ,DthSt := str_pad(DeathState, 2, "left", "0")]
	
	# veteran status
	nvdrs[Military == 0,veteran := 0]      
	nvdrs[Military == 1,veteran := 1]
	#0	No
	#1	Yes
	#9	Unknown
	#On the death certificate form, the caption for the veteran status variable has been changed from “Ever a member of the U.S. armed forces” to “Ever served in U.S. armed forces” to match the wording on U.S. Standardized Death Certificate. Prior to 2010, the label for this data element was “Veteran status.” The label was changed in 2010 to reflect the actual wording on the standard death certificate. 

	# --  Manner of death
	nvdrs[AbstractorDeathmanner_c %in% c(1,8), DthTypN := 1] #suicide
	nvdrs[AbstractorDeathmanner_c %in% c(2,7), DthTypN := 2] #homicide
	nvdrs[AbstractorDeathmanner_c %in% c(9), DthTypN := 3] #undetermined
	nvdrs[AbstractorDeathmanner_c %in% c(3,4,5), DthTypN := 4] #accident / unintentional
	nvdrs[AbstractorDeathmanner_c %in% c(6), DthTypN := 5] #legal
	nvdrs[AbstractorDeathmanner_c %in% c(99), DthTypN := NA] #NA
    
	# -- other demographic variables
	nvdrs[Sex == 9, Sex := NA]

	nvdrs[Age == 999, Age := NA]	
	nvdrs[Age <  15 , AgeGrp4 := 0] 
	nvdrs[Age >= 15 & Age < 25, AgeGrp4 := 1] 
	nvdrs[Age >= 25 & Age < 45, AgeGrp4 := 2] 
	nvdrs[Age >= 45 & Age < 65, AgeGrp4 := 3] 
	nvdrs[Age >= 65, AgeGrp4 := 4]
	
	nvdrs[BirthPlace %in% 1:54, BornUSA := 1]
	nvdrs[BirthPlace %in% c(55, 56, 57, 59, 61, 62, 88), BornUSA := 0]
	nvdrs[BirthPlace %in% 99, BornUSA := NA]
	
	nvdrs[Race_c == 1, Race5 := 1] # white
	nvdrs[Race_c == 2, Race5 := 2] # black
	nvdrs[Race_c == 3, Race5 := 3] # native
	nvdrs[Race_c == 4, Race5 := 4] # asian
	nvdrs[Race_c %in% c(5,6), Race5 := 5] # other
	nvdrs[Race_c == 9, Race5 := NA] # missing / unknown
	
	nvdrs[Ethnicity != 9, Hisp := Ethnicity]

	# education 
	nvdrs[EducationLevel == 0, EducYear := 8]
	nvdrs[EducationLevel == 1, EducYear := 10.5]
	nvdrs[EducationLevel == 2, EducYear := 12]
	nvdrs[EducationLevel == 3, EducYear := 13]
	nvdrs[EducationLevel == 4, EducYear := 14]
	nvdrs[EducationLevel == 5, EducYear := 16]
	nvdrs[EducationLevel == 6, EducYear := 18]
	nvdrs[EducationLevel == 7, EducYear := 20]
	
	nvdrs[is.na(EducationLevel)|EducationLevel==9,EducYear := EducationYears]
	nvdrs[EducYear == 99, EducYear := NA]

	nvdrs[0 <= EducYear & EducYear <  12, EducGroup := 1]
	nvdrs[12 <= EducYear & EducYear < 13, EducGroup := 2]
	nvdrs[13 <= EducYear & EducYear < 16, EducGroup := 3]
	nvdrs[16 <= EducYear & EducYear < 17, EducGroup := 4]
	nvdrs[17 <= EducYear & EducYear < 21, EducGroup := 5]
	
	# marital status
	nvdrs[MaritalStatus == 1, MarStat5 := 1] # married
	nvdrs[MaritalStatus == 3, MarStat5 := 2] # widowed
	nvdrs[MaritalStatus == 4, MarStat5 := 3] # divorced
	nvdrs[MaritalStatus == 5, MarStat5 := 4] # separated
	nvdrs[MaritalStatus == 2, MarStat5 := 5] # never married 
	nvdrs[MaritalStatus == 6, MarStat5 := 5] # other single
		
	# physicial problem
	nvdrs[, PhysProb := PhysicalHealthProblem_c]
	nvdrs[CircumstancesKnown_c == 0 & PhysProb == 0,PhysProb := NA]
	
	# merge with  manually coded files ----
	nvdrs = merge(nvdrs, occ_df, by.x=c('OccupationText'), by.y=c('OccupationText'), all.x=TRUE)
	nvdrs[,c('current_employed_status','usual_employed_status') := NULL]
	nvdrs[is.na(employment_status),employment_status := 'missing']
	setnames(nvdrs,'employment_status','usual_employed_status')
	setnames(nvdrs,'employment_etc','usual_employed_status_etc')
		
	nvdrs = merge(nvdrs, occ_df, by.x=c('OccupationCurrentText'), by.y=c('OccupationText'), all.x=TRUE)
	nvdrs[is.na(employment_status),employment_status := 'missing']
	setnames(nvdrs,'employment_status','current_employed_status')
	setnames(nvdrs,'employment_etc','current_employed_status_etc')
	
	nvdrs[current_employed_status == 'undetermined', current_employed_status0 := 'missing']
	nvdrs[current_employed_status != 'undetermined',current_employed_status0 := current_employed_status]
	
	nvdrs[usual_employed_status == 'undetermined', usual_employed_status0 := 'missing']
	nvdrs[usual_employed_status != 'undetermined',usual_employed_status0 := usual_employed_status]
	
	nvdrs[,empstat := current_employed_status0]
	nvdrs[current_employed_status0 == 'missing' ,empstat := usual_employed_status0]
	
	nvdrs[, UnEmpl := ifelse(empstat == 'unemployed', 1, 0)]
	nvdrs[, NotInLabor := ifelse(empstat == 'notinlabor', 1, 0)]

	nvdrs[,empstat := as.integer(factor(empstat, levels=c('employed','unemployed','notinlabor')))]

	# remove unnecessary variables
	keep <- c("year", "month", "Site", "IncID", "PerID", "State", "Cnty",'ResidenceCityState',
		"DthTypN", "DthSt", "Sex", "Age", "AgeGrp4", "BornUSA", "Race5", "Hisp", "MarStat5", 
		'current_employed_status0','usual_employed_status0',
		'empstat',"UnEmpl","NotInLabor",
		"PhysProb",'EducGroup',
		"InjuryDate_myr","DeathDate_myr",
		"EducYear","veteran")
	
    nvdrs <- nvdrs[, c(keep), with=F]

	# add death indicator
	nvdrs$DSID <- 1

	# NVDRS uid and others
	nvdrs[, uid := paste0(year,
	      '_',str_pad(Site, width=2, pad='0'),
	      '_',str_pad(IncID, width=4, pad='0'),
	      '_',str_pad(PerID, width=4, pad='0'))]
	
	# only includes suicide cases
	nvdrs = nvdrs[DthTypN ==1, ]
	
	# include only matched cases
	setnames(nvdrs,'State','state')
	setnames(nvdrs,'Cnty','county')
	setnames(nvdrs,'year','Year')
	
	# include years >= 2005
	nvdrs = nvdrs[Year >= 2005,]

	# create weights
	nvdrs[, weight := 1]

	return(nvdrs)
}



