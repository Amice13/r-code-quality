# ------------------------------
# ' create county indicators and weights for ACS data
# ------------------------------
clean_geo_mapping = function(dt1, dt2, geo_unit = 'county') {

	if (geo_unit == 'county'){
		# use county as geo weighting
		names(dt1) = gsub('\\(|\\)','',names(dt1))
		names(dt1) = gsub(' ','_',names(dt1))
		
		dt1[, county := str_pad(County_code, width=5, pad = '0')]
		dt1[, state := str_pad(State_code, width=2, pad = '0')]
		dt1[, PUMA := str_pad(PUMA_2000, width=5, pad = '0')]
		dt1[, puma := paste0(state,PUMA)]
		
		dt1[, weight := puma2k_to_county_allocation_factor]
		
		names(dt2) = gsub('\\(|\\)','',names(dt2))
		names(dt2) = gsub(' ','_',names(dt2))
		
		dt2[, county := str_pad(County_code, width=5, pad = '0')]
		dt2[, state := str_pad(State_code, width=2, pad = '0')]
		dt2[, PUMA := str_pad(puma12, width=5, pad = '0')]
		dt2[, puma := paste0(state,PUMA)]
		
		dt2[, weight := puma12_to_county_allocation_factor]
		
		geo_puma = rbind(
			data.table(dt1[,c('puma','county','weight')],merged_year = 2000),
			data.table(dt2[,c('puma','county','weight')], merged_year = 2012)
			)
	}
	if (geo_unit == 'cz') {

		names(dt1) = gsub('\\(|\\)','',names(dt1))
		names(dt1) = gsub(' ','_',names(dt1))
		
		dt1[, cz := str_pad(czone, width=5, pad = '0')]
		dt1[nchar(puma2000) == 5, state := substr(puma2000, 1, 1)]
		dt1[nchar(puma2000) == 6, state := substr(puma2000, 1, 2)]
		dt1[nchar(puma2000) == 5, puma := paste0(state, '0',substr(puma2000, 2, 5))]
		dt1[nchar(puma2000) == 6, puma := paste0(state, '0',substr(puma2000, 3, 6))]		
		dt1[, puma := str_pad(puma, width = 7, pad = '0')]
		dt1[, weight := afactor]
				
		names(dt2) = gsub('\\(|\\)','',names(dt2))
		names(dt2) = gsub(' ','_',names(dt2))
		
		dt2[, cz := str_pad(czone, width=5, pad = '0')]
		dt2[, puma := str_pad(puma2010, width = 7, pad = '0')]
		dt2[, weight := afactor]
		
		geo_puma = rbind(
			data.table(dt1[,c('puma','cz','weight')],merged_year = 2000),
			data.table(dt2[,c('puma','cz','weight')], merged_year = 2012)
			)
	}
	return(geo_puma)
}

create_matched_acs = function(acs, geo_puma, geo_unit){
	
	acs_geo = merge(x = acs, y = geo_puma, by =c('puma','merged_year'), 
		allow.cartesian = TRUE,
		all.x = TRUE,
		all.y = TRUE)
	
	acs_geo[, paste0(geo_unit,'_weight') := PerWgt * weight]
	
	return(acs_geo)	

}

