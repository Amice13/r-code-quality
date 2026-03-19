#------------------------------------------------------------------------------
# META file to run all analysis 
# to replicate Lee and Pescosolido (2024)'s paper
# "Misery Needs Company: Contextualizing the Geographic and Temporal Link between Unemployment and Suicide"
#==============================================================================

list_packages = c('rio', 'fst', 'data.table', 'bit64', 
	'refinr', 'diagis', 'stringr', 
	'ggplot2', 'gridExtra', 'ggsci','zoo','tidyverse',
	'wordcloud','tm','lmtest')

# you should install the the following packages 
#install.packages(list_packages)

# load all packages
invisible(lapply(list_packages,function(x) library(x, character.only = TRUE)))

# specify your home directory
home_dir = ''

# specify other directories
data_dir = file.path(home_dir, 'data')
rawdata_share_dir = file.path(data_dir, 'rawdata_sharable')
rawdata_notshare_dir = file.path(data_dir, 'rawdata_notshared')
processed_share_dir = file.path(data_dir, 'processed_sharable')
processed_notshare_dir = file.path(data_dir, 'processed_notshared')

output_dir = file.path(home_dir, 'output')
figure_dir = file.path(output_dir, 'figure')

##########################################
# data cleaning 
##########################################

# 1. clean NVDRS and ACS data ----
nvdrs <- read_fst(file.path(rawdata_notshare_dir, 'nvdrs.fst'), as.data.table = TRUE)

# clean occupation cleaning
source('code/1a_clean_nvdrs_occupation.R')
tab_occupation = clean_occupation(nvdrs)
export(tab_occupation, file.path(processed_notshare_dir,'nvdrs_occupation_coding.xlsx'))

# after manual cleaning of all fields; save the data 
occ_df = import(file.path(processed_share_dir,'nvdrs_occupation_coding_coded_final.xlsx'))
setDT(occ_df)

# clean the NVDRS data 
source('code/1_clean_nvdrs.R')
nvdrs_cleaned = clean_nvdrs(nvdrs)
write_fst(nvdrs_cleaned, file.path(processed_notshare_dir, "nvdrs_v2.fst"), 100)

# create a distribution for word-cloud figures
source('code/1b_export_occupation_text.R')
wordcloud_occupation = table_occupation_text(nvdrs)
fwrite(wordcloud_occupation, file.path(output_dir, 'suicide_occupation_text.csv'))

# Figure S2 Panels A-D in Appendix.
wordcloud_occupation = fread(file.path(output_dir,'suicide_occupation_text.csv'))
source('code/1c_figure_s2_wordcloud_by_occupation.R')

# clean ACS data
source('code/1d_clean_ACS_PUMS.R')
acs = read_fst(file.path(rawdata_share_dir,'microACS_ipums.fst'), as.data.table=TRUE)
acs = clean_acs(acs)

# 2. create matching weights for ACS data puma >> CZ and county ----
source('code/2a_create_ACS_county_weight.R')

# create weights for county mapping
county_puma2000 = fread(file.path(rawdata_share_dir, 'geocorr2014_puma2000_to_county.csv'), skip=1)
county_puma2012 = fread(file.path(rawdata_share_dir, 'geocorr2014_puma2012_to_county.csv'), skip=1)

county_puma = clean_geo_mapping(county_puma2000, county_puma2012, geo_unit = 'county')

# create weights for CZ mapping (commuting zone)
cz_puma2000 = import(file.path(rawdata_share_dir, 'cw_puma2000_czone.dta'))
cz_puma2012 = import(file.path(rawdata_share_dir, 'cw_puma2010_czone.dta'))
setDT(cz_puma2000)
setDT(cz_puma2012)

cz_puma = clean_geo_mapping(cz_puma2000, cz_puma2012, geo_unit = 'cz')

# create new geographically matched data sets 
acs_county = create_matched_acs(acs, county_puma, geo_unit = 'county')
acs_cz = create_matched_acs(acs, cz_puma, geo_unit = 'cz')

write_fst(acs_county, file.path(processed_share_dir, 'acs_matched_county_v2.fst'), 100)
write_fst(acs_cz, file.path(processed_share_dir, 'acs_matched_cz_v2.fst'), 100)

# 3. combine NVDRS and ACS data + add some measures ----
source('code/3a_aggregate_measures.R')

nvdrs_cleaned = read_fst(file.path(processed_notshare_dir, "nvdrs_v2.fst"), as.data.table = TRUE)
nvdrs_coverage = fread(file.path(rawdata_share_dir, 'nvdrs_coverage.csv'))

acs_county = read_fst(file.path(processed_share_dir, 'acs_matched_county_v2.fst'), as.data.table = TRUE)

reg_data = combine_nvdrs_acs(nvdrs_cleaned, acs_county, geo_unit = 'county')

write_fst(reg_data, file.path(processed_notshare_dir,'regtable_unemployment_county_v2.fst'), 100)
export(reg_data, file.path(processed_notshare_dir,'regtable_unemployment_county_v2.dta'))

# turn nvdrs county to cz using crosswalk
cz_county = import(file.path(rawdata_share_dir, 'cw_cty_czone.dta'))
setDT(cz_county)

cz_county[, county := stringr::str_pad(cty_fips, width = 5, pad = '0')]
cz_county[, cz := str_pad(czone, width=5, pad = '0')]

# reflect longitudinal changes on county boundary 
#Florida, 1997: Dade county (FIPS 12025) is renamed as Miami-Dade county (FIPS 12086).
#Action: replace FIPS code 12086 with the old code 12025.
cz_county[cty_fips == '12025', county := '12086']

nvdrs_cleaned_cz = merge(nvdrs_cleaned, cz_county, by='county')

acs_cz = read_fst(file.path(processed_share_dir, 'acs_matched_cz_v2.fst'), as.data.table = TRUE)
reg_data = combine_nvdrs_acs(nvdrs_cleaned_cz, acs_cz, geo_unit = 'cz')

write_fst(reg_data, file.path(processed_notshare_dir,'regtable_unemployment_cz_v2.fst'), 100)
export(reg_data, file.path(processed_notshare_dir,'regtable_unemployment_cz_v2.dta'))

# 3. clean it for regression analysis in stata ----
system('stata-mp code/3b_clean_regdata.do')

##########################################
# data validation
##########################################

# 4. compare NVDRS and CMF data sets: Figure S1, Tables S2 and S3.
source('code/4a_figure_s1_compare_cmf_nvdrs.R')

##########################################
# data analysis
##########################################

# 5. bivariate descriptive analysis using cross-section data: Figure S3 and Figure 1
system('stata-mp code/5a_descriptive_analysis.do')
source('code/5a_figure_s3_bivariate_association.R')
source('code/5a_figure1_overtime_association.R')

# 6. regression sameness effect using cross-section data Figure 2 and Fgure S4
system('stata-mp code/5b_sameness_main.do')
source('code/5b_figure2_figures4_sameness_main.R')

# 7. over-time descriptive analysis using longitudinal data: Figure 3 Panels A and B
system('stata-mp code/5c_sameness_main_overtime.do')
source('code/5c_figure3_sameness_overtime.R')

# 8. macro / micro regression sameness effect using longitudinal data
system('stata-mp code/5d_sameness_macrointeract.do')
source('code/5d_figure3_macro_micro.R')

# 9. robustness of effects
system('stata-mp code/6a_robustness_crosssection.do')
system('stata-mp code/6b_robustness_longitudinal.do')

# 10. examine nonlinearity 
system('stata-mp code/6c_nonlinearity_sameness.do')
source('code/6c_figure_s5_nonlinearity.R')



