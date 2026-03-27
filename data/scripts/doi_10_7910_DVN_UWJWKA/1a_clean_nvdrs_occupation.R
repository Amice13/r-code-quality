#' --------------
# clean occupation data cleaning ----
#' ==============

clean_occupation = function(nvdrs){

	# some keyword we found through repeated searches
	words_notinlabor = c('Incarcerated','Jail','Prison','Inmate','Imprisonment',
		'Housekeep','Homemaker','Housewife', 'At Home','Mother','Houseman',
		'Teenager','Student','Grade',
		'Air Force','Navy','Army','Military','Soldier','Marine','Veteran',
		'Never Work','Never Emp','Not In Workforce',
		'Retired','Elderly','Unable To Work','Unabled','3rd Mate',
		'Disab','Disabled')
	
	words_notinlabor_inclusive = c('Incarcerated','Jail','Prison','Inmate','Imprisonment',
		'Housekeep','Homemaker','Housewife', 'At Home','Mother','Houseman',
		'Teenager','Student',
		'Air Force','Navy','Army','Military','Soldier','Marine','Veteran',
		'Never Work','Never Emp','Not In Workforce',
		'Not Emp',
		'Retired','Elderly','None','Not Wor','Not wor','No Wor','Null',
		'Unable To Work','Unabled',
		'Disab','Disabled')
	
	words_unemployed = c(
		'Laid Off','Uneployed','Umployed',
		'Not Emp','Unemp','Un Employeed','Umeployed','Umemp','None','Not Wor','Not wor','No Wor','Null')
	
	words_unemployed_strict = c(
		'Laid Off',
		'Unemp','Un Employeed','Umeployed','Umemp')
	
	words_NA = c('Not Reported','Unsure','Unvailable','Unspecified','Unrecorded','Unavailalble',
		'Unavailable','Unk','Unjnown','$Unknown^','Ukn','Not Available','Blank','Not Applicable','Missing','$NA^','N/A','Not Specified')
	words_NA_match = c('63w','68c','777','88m10','560588477','9','88888','99999','0','01-10-2006','02-05-2006',
		'02-22-2006','03/Lt','04-18-2004','043','05-16-2005','05-20-2005','06-03-2005','06-11-2005','077','088',
		'10-15-2004','10-16-2005','10-20-2005','10-21-2005','104','11-27-2005','11c','11b','12b','13d','13f',
		'147386772','147866084','148707776','153645445','157467161','157640012','158600272',
		'Yn1','Yn1-E6','Yn3 E4','Ync E-7','Unclear',
		"'",'*','-','---','----','-----','.','<<<>>>',
		'03-01-2900','06-09-2005','075349731','080','090','096707385','102580915',
		'10-29-2005','103922517','11-19-2005','137921800','140749751','141908373','141908485','149921676',
		'153600793','155420993','190708242','19d10','19k','89d30','91810','92a10','92g10','948b',
		'A1c','Abf3/E4','Abfan/E-3','Abhan/E3','Am2','Ao2/Po2','Ao3','Aoan-E3',
		'Bm2','L','`',
		'Ca','Co','Dj','E1','E4','Gm','Hr','Lp','Lt','Mc','Md','Me','Qa','Qc','St',
		'Aaa','Ac1','Aec','Aes','Afl','Am2','Ao3','Atk','Blm','Bm2','Bmg','Can','Cdl',
		'Cmi','Cmt','Cro','Crr','Cwo','Dcc','Dla','E03',
		'E-3','E-6','E-7','E-9','En3'
		)
	
	words_employed = c('Empolyed','Teacher','Tradesman','Self Employed','Union','Attendant','Guard',
		'Accountant','Generator','Production','Technician','Position','Job','Machinists',
		'Trainer','Educator','Employed, But Unknown','Repair','Squadron','Maintenance','Coordinator','Administrator',
		'Clinic','Retail','Operator','Labor','Trunking',
		'Rank','Junk','Officer','Empkoyed','Dunkin','Dispatcher')

	# merge two cleaned information 
	nvdrs[, usual_employed_status := 'employed']
	nvdrs[grepl(paste0(words_NA,collapse = '|'),OccupationText,ignore.case = TRUE), usual_employed_status := 'missing']
	nvdrs[grepl(paste0(words_notinlabor,collapse = '|'),OccupationText,ignore.case = TRUE), usual_employed_status := 'notinlabor']
	nvdrs[grepl(paste0(words_employed,collapse='|'), OccupationText), usual_employed_status := 'employed']
	nvdrs[grepl(paste0(words_unemployed,collapse = '|'),OccupationText,ignore.case = TRUE), usual_employed_status := 'unemployed']
	nvdrs[stringr::str_trim(OccupationText) %in% words_NA_match, usual_employed_status := 'missing']
	nvdrs[OccupationText=='',usual_employed_status := 'missing']

	nvdrs[, current_employed_status := 'employed']
	nvdrs[grepl(paste0(words_NA,collapse = '|'),OccupationCurrentText,ignore.case = TRUE), current_employed_status := 'missing']
	nvdrs[grepl(paste0(words_notinlabor,collapse = '|'),OccupationCurrentText,ignore.case = TRUE), current_employed_status := 'notinlabor']
	nvdrs[grepl(paste0(words_employed, collapse='|'), OccupationCurrentText) & current_employed_status == 'missing', current_employed_status := 'employed']
	nvdrs[grepl(paste0(words_unemployed,collapse = '|'),OccupationCurrentText,ignore.case = TRUE), current_employed_status := 'unemployed']
	nvdrs[OccupationCurrentText=='',current_employed_status := 'missing']
	
	tab_current = nvdrs[,.(n_case = .N), by = c('OccupationCurrentText','current_employed_status')]
	tab_current$occupation = 'current'
	tab_usual = nvdrs[,.(n_case = .N), by = c('OccupationText','usual_employed_status')]
	tab_usual$occupation = 'usual'

	setnames(tab_current,'OccupationCurrentText','OccupationText')
	setnames(tab_current,'current_employed_status','employed_status')
	setnames(tab_usual,'usual_employed_status','employed_status')

	tab_occupation = rbind(tab_current, tab_usual)
	return(tab_occupation)
}
