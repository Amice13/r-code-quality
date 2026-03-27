#------------------------------------------------------------------------------
# Combine NVDRS (processed) and ACS PUMS data (processed)
#==============================================================================

create_aggregate = function(acs, geo_unit = 'county'){
	
	# load other county data
	land_area = fread(file.path(rawdata_share_dir, 'county_2010_landarea.csv'))
	land_area = land_area[county_fips != 0,]
	land_area[, county_fips := stringr::str_pad(county_fips,pad='0',width=5)]
	
	pop_total = fread(file.path(rawdata_share_dir, 'county_2000_2019_population.csv'))
	pop_total = pop_total[year >= 2005 & year <= 2017,]
	pop_total[, county_fips := stringr::str_pad(county_fips,pad='0',width=5)]
	
	pop_den = merge(pop_total,land_area, by='county_fips',all.x=TRUE)
	setnames(pop_den,'value','pop_total')
	pop_den[,pop_density := pop_total / land_area]
	
	acs_macro = copy(pop_den)
	setnames(acs_macro, 'county_fips','county')

	if (geo_unit == 'cz') {
		# change it to CZ levels 
		acs_macro = merge(acs_macro, cz_county[,c('county','cz')], by='county', all.x=TRUE)
		acs_macro = acs_macro[, lapply(.SD, weighted.mean, w = pop_total, na.rm=TRUE), 
			.SDcols = c('pop_density'), by=c('cz','year')]

	}

	# create own aggregate level measures
	list_var = c('Sex','AgeGrp4','Race6','MarStat5','BornUSA','EducGroup','empstat','PhysProb','veteran')

	for (var in list_var) {
		var_value = sort(unique(acs[[var]]))
	
		for (i in var_value) {
			mean_data = acs[, weighted.mean(get(var) == i, weight, na.rm=TRUE), by=c(geo_unit,'Year')]
			mean_data[,V1 := 100 * V1]
			setnames(mean_data, 'V1',paste0('RAT_',var,'_',i))
			acs_macro = merge(x = acs_macro, y = mean_data, by.x=c(geo_unit,'year'),by.y=c(geo_unit,'Year'), all.x = TRUE, all.y = TRUE)
		}
	}
	
	# adjust some variable names 
	setnames(acs_macro,'RAT_Sex_2','RAT_Female')
	setnames(acs_macro,'RAT_BornUSA_1','RAT_BornUSA')
	setnames(acs_macro,'RAT_PhysProb_1','RAT_PhysProb')
	setnames(acs_macro,'RAT_veteran_1','RAT_veteran')
	
	setnames(acs_macro, c('RAT_empstat_1','RAT_empstat_2','RAT_empstat_3'),
		c('RAT_Empl','RAT_UnEmpl','RAT_NotInLabor'))

	# remove some variables
	var_del = c('RAT_Sex_1','RAT_BornUSA_0','RAT_veteran_0','RAT_PhysProb_0')
	acs_macro[,(var_del) := NULL]
	return(acs_macro)
}


combine_nvdrs_acs = function(nvdrs, acs, geo_unit = 'county') {

	# first select key variables 
	keep_var = c('uid','DSID','AgeGrp4','Age','Sex','Race5','Hisp','BornUSA',
		'empstat',
		'MarStat5','PhysProb','EducYear','EducGroup','veteran',
		'state',geo_unit,'Year','month','weight')

	# randomly assign months
	if (geo_unit == 'county'){

		acs[, month := sample.int(12, .N, replace=TRUE), 
			keyby = .(county, Year)]

	} else if (geo_unit == 'cz'){

		acs[, month := sample.int(12, .N, replace=TRUE), 
		keyby = .(cz, Year)]

	}

	# rename geo-mapping weight 
	setnames(acs,'weight','geo_weight')
	setnames(acs,paste0(geo_unit,'_weight'),'weight')
	acs[, weight := as.numeric(weight)]

	# remove 0 weight or missing weight 
	acs = acs[(is.na(weight) | weight == 0) == FALSE, ]

	acs_macro = create_aggregate(acs[AgeGrp4 != 0,], geo_unit = geo_unit)

	# select the relevant information
	sel_nvdrs = nvdrs[,c(keep_var), with=FALSE]

	# exclude nvdrs cases when they do not have counter-parts in ACS data
	sel_nvdrs = merge(
			sel_nvdrs, 
			acs[,.(n_acs = sum(weight)), by=c('Year',geo_unit)],
			by = c('Year', geo_unit), all.x=TRUE
		)
	sel_nvdrs = sel_nvdrs[!is.na(n_acs), ]
	sel_nvdrs[, n_acs := NULL]

	# check 
	acs = acs[, c(keep_var), with = FALSE]

	# bind two data sets
	reg_data = rbind(acs, sel_nvdrs, fill = TRUE)
	
	# reduce the data set size
	non_integer_var = c('uid','state',geo_unit,'weight')
	for (var in setdiff(keep_var, non_integer_var)) {
		reg_data[, (var) := as.integer(get(var))]
	}

	# define longitudinal vs cross-sectional samples
	select_state_code = nvdrs_coverage[rowSums(nvdrs_coverage[,paste0(c(2003:2017)),with=FALSE]) >= 13,`State (FIPS)`]
	
	reg_data[, sample_v1 := 0]
	reg_data[as.integer(state) %in% select_state_code, sample_v1 := 1]
	
	select_state_year = data.table::melt(nvdrs_coverage[, c('State (FIPS)','Name','2016','2017')], id.var=c('State (FIPS)','Name'))
	select_state_year = select_state_year[value == 1,]
	select_state_year[, `State (FIPS)` := stringr::str_pad(as.character(`State (FIPS)`), pad = '0', width = 2)]
	select_state_year[, variable := as.integer(as.character(variable))]
	
	reg_data[, sample_v2 := 0]
	reg_data = merge(reg_data, select_state_year[,c('State (FIPS)','variable','value')], by.x=c('state','Year'), by.y=c('State (FIPS)','variable'), all.x=TRUE)
	reg_data[value == 1, sample_v2 := 1]
	reg_data[, value := NULL]
	reg_data = reg_data[(sample_v1 == 1 | sample_v2 == 1), ]

	# -----
	# additional Data Pre-processing
	reg_data[,Female := as.integer(Sex == 2)]
	setnames(reg_data,'DSID','Suic')
	setnames(reg_data,'state','St')
	
	reg_data[, Race6 := Race5]
	reg_data[Hisp == 1, Race6 := 6L] # hispanic categories
	
	reg_data[, agegroup := ifelse(Age < 85, Age, 84)]
	reg_data[, agegroup := cut(agegroup, seq(from = 15, to = 85, by = 5), right = FALSE)]

	names(reg_data) = gsub('\\.','_',names(reg_data))
	reg_data[,weight := as.double(weight)]
	
	# merge with other data 
	reg_data = merge(reg_data, acs_macro, by.x = c('Year', geo_unit), by.y = c('year', geo_unit), all.x = TRUE)

	# generate sameness measures ----
	reg_data[empstat == 1, std_same_prop_empstat := scale(100 * RAT_Empl), by = 'Year']
	reg_data[empstat == 2, std_same_prop_empstat := scale(100 * RAT_UnEmpl), by = 'Year']
	reg_data[empstat == 3, std_same_prop_empstat := scale(100 * RAT_NotInLabor), by = 'Year']
	

	# add global monthly unemployment rates ----
	us_unemployment = import(file.path(rawdata_share_dir, 'monthly_unemployment_US_all_1948_2021.xlsx'),skip=11)
	setDT(us_unemployment)
	us_unemployment = melt(us_unemployment, id.var='Year')
	us_unemployment[,date := as.Date(paste0(Year,'-',variable,'-01'),format='%Y-%B-%d')]
	setnames(us_unemployment, 'value', 'monthly_unemployment')

	reg_data[,date := as.Date(paste0(Year,'-',month,'-01'),format='%Y-%m-%d')]
	reg_data = merge(reg_data, us_unemployment[,c('date','monthly_unemployment')], by=c('date'), all.x=TRUE)

	# add insurance benefits data
	state_ui_benefits = fread(file.path(rawdata_share_dir, 'state_ui_benefits.csv'),skip=1)
	state_ui_benefits[, date := zoo::as.yearqtr(paste0(Year,'Q',Quarter))]
	names(state_ui_benefits)[14] = c('ui_recipient')
	names(state_ui_benefits)[18] = c('ui_average_weekly')
	setnames(state_ui_benefits, 'Extended Benefits(000)', 'ui_benefits')


	us_benefits = state_ui_benefits[State == 'US', c('date','ui_benefits','ui_recipient','ui_average_weekly')]
	state_ui_benefits = state_ui_benefits[State != 'US',c('date','State','ui_benefits','ui_recipient','ui_average_weekly')]
	setnames(state_ui_benefits, 'ui_benefits','st_ui_benefits')
	setnames(state_ui_benefits, 'ui_recipient','st_ui_recipient')
	setnames(state_ui_benefits, 'ui_average_weekly','st_ui_average_weekly')

	tab = merge(nvdrs_coverage[,c('State (FIPS)', 'Name')], data.frame(State=c(state.name,'District of Columbia'), state.abb = c(state.abb,'DC')), by.x='Name', by.y='State')
	
	state_ui_benefits = merge(state_ui_benefits, tab, by.x='State', by.y='state.abb', all.x=TRUE)
	state_ui_benefits[, `State (FIPS)` := stringr::str_pad(`State (FIPS)`, pad = '0', width = 2)]

	reg_data[, date_quarter := zoo::as.yearqtr(date)]

	reg_data = merge(reg_data, us_benefits, by.x='date_quarter', by.y='date', all.x=TRUE)
	reg_data = merge(reg_data, state_ui_benefits[,c('State (FIPS)', 'date',
		'st_ui_benefits','st_ui_recipient','st_ui_average_weekly')], 
		by.x=c('date_quarter','St'), by.y=c('date','State (FIPS)'), all.x=TRUE)

	return(reg_data)
}

