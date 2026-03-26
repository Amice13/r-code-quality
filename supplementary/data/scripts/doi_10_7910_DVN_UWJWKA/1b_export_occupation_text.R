
table_occupation_text = function(nvdrs){

	# include only suicide here  ----
	nvdrs = nvdrs[DthTypN == 1, ]
	
	nvdrs[current_employed_status == 'undetermined', current_employed_status0 := 'missing']
	nvdrs[current_employed_status != 'undetermined',current_employed_status0 := current_employed_status]
	
	nvdrs[usual_employed_status == 'undetermined', usual_employed_status0 := 'missing']
	nvdrs[usual_employed_status != 'undetermined',usual_employed_status0 := usual_employed_status]
		
	tab1a = nvdrs[usual_employed_status0 =='notinlabor',.N, by=c('OccupationText')]
	tab1b = nvdrs[current_employed_status0 =='notinlabor',.N, by=c('OccupationCurrentText')]
	setnames(tab1b,'OccupationCurrentText','OccupationText')
	tab1a[, type := 'notinlabor']
	tab1b[, type := 'notinlabor']
	
	tab2a = nvdrs[usual_employed_status0 =='missing',.N, by=c('OccupationText')]
	tab2b = nvdrs[current_employed_status0 =='missing',.N, by=c('OccupationCurrentText')]
	setnames(tab2b,'OccupationCurrentText','OccupationText')
	tab2a[, type := 'missing']
	tab2b[, type := 'missing']
	
	tab3a = nvdrs[usual_employed_status0 =='unemployed',.N, by=c('OccupationText')]
	tab3b = nvdrs[current_employed_status0 =='unemployed',.N, by=c('OccupationCurrentText')]
	setnames(tab3b,'OccupationCurrentText','OccupationText')
	tab3a[, type := 'unemployed']
	tab3b[, type := 'unemployed']
	
	tab4a = nvdrs[usual_employed_status0 =='employed',.N, by=c('OccupationText')]
	tab4b = nvdrs[current_employed_status0 =='employed',.N, by=c('OccupationCurrentText')]
	setnames(tab4b,'OccupationCurrentText','OccupationText')
	tab4a[, type := 'employed']
	tab4b[, type := 'employed']
	
	tab_occ = rbind(tab1a, tab1b, tab2a, tab2b, tab3a, tab3b, tab4a, tab4b)
	tab_occ = tab_occ[, .(N = sum(N)), by=c('OccupationText','type')]
	return(tab_occ)
}


