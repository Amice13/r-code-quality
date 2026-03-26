#' --------------
# check county-level and CZ-level suicide rates ----
# Figure S1. NVDRS against CMF at county levels 
# Table S2. suicide rates by state
# Table S3. suicide rates by year
#' ==============

# load data
reg_data_county = read_fst(file.path(processed_notshare_dir,'regtable_unemployment_county_v2.fst'), as.data.table = TRUE,
	columns = c('Age','sample_v1','sample_v2','Suic','weight','Year','St','county'))
reg_data_county = reg_data_county[Age >= 15,]

# load compressed mortality file for comparison
cmf = read_fst(file.path(rawdata_notshare_dir, 'cmf_nchs_county_suicide_2005_2017.fst'), as.data.table=TRUE)

cmf[,state := substr(fips,1,2)]

# year by year comparison using V1 data ----
nvdrs_v1 = reg_data_county[sample_v1 == 1,]
nvdrs_v1 = nvdrs_v1[Year <= 2017,]

cmf_v1_matched = merge(cmf, unique(nvdrs_v1[,c('St','Year')]), by.x=c('state','Year'), by.y=c('St','Year'))

tab1_year_comparison = merge(
	cmf_v1_matched[, .(
		cmf_suicide = sum(deaths),
		cmf_suicide_rate = sum(deaths)/ sum(population) * 10^5), by=c('Year')],
	nvdrs_v1[, .(
		nvdrs_suicide = sum(Suic * weight),
		nvdrs_suicide_rate = weighted.mean(Suic,weight)  * 10^5), by=c('Year')], by=c('Year'))

tab1_year_comparison[, diff_suicide_count := cmf_suicide - nvdrs_suicide]
tab1_year_comparison[, diff_suicide_rate := cmf_suicide_rate - nvdrs_suicide_rate]

export(tab1_year_comparison, 
	file.path(output_dir,'tab_s3_comparison_year_cmf_nvdrs.xlsx'))

# state-levels using V2 data ----
nvdrs_coverage = fread(file.path(rawdata_share_dir, 'nvdrs_coverage.csv'))
setnames(nvdrs_coverage, 'State (FIPS)','stfips')
setnames(nvdrs_coverage, 'Name','state_name')

nvdrs_v2 = reg_data_county[sample_v2 == 1,]
nvdrs_v2[,St := stringr::str_pad(St, width=2, pad='0')]

cmf_v2_matched = merge(cmf, unique(nvdrs_v2[,c('St','Year')]), by.x = c('state','Year'), by.y=c('St','Year'))

tab2_state_comparison = merge(
	cmf_v2_matched[, .(
		cmf_suicide = sum(deaths),
		cmf_suicide_rate = sum(deaths)/ sum(population) * 10^5), by=c('state','Year')],
	nvdrs_v2[, .(
		nvdrs_suicide = sum(Suic * weight),
		nvdrs_suicide_rate = weighted.mean(Suic,weight) * 10^5), by=c('St','Year')], by.x=c('state','Year'),by.y=c('St','Year'))

tab2_state_comparison[, diff_suicide_rate := cmf_suicide_rate - nvdrs_suicide_rate]
tab2_state_comparison[, diff_suicide_count := cmf_suicide - nvdrs_suicide]
tab2_state_comparison[, state := as.integer(state)]
tab2_state_comparison = merge(tab2_state_comparison, nvdrs_coverage[,c('stfips','state_name')], by.x='state', by.y='stfips', all.x=TRUE)

export(tab2_state_comparison[Year == 2017 | (Year == 2016 & state_name == 'Hawaii'),] , 
	file.path(output_dir,'tab_s2_comparison_state_cmf_nvdrs.xlsx'))

# county level comparison using V2 ----
tab3_county_comparison = merge(
	cmf_v2_matched[, .(
		cmf_suicide = sum(deaths),
		cmf_suicide_rate = sum(deaths)/ sum(population) * 10^5), by=c('fips','Year')],
	nvdrs_v2[, .(
		nvdrs_suicide = sum(Suic * weight),
		nvdrs_suicide_rate = weighted.mean(Suic,weight) * 10^5), by=c('county','Year')], by.x=c('fips','Year'),by.y=c('county','Year'))

tab3_county_comparison[, diff_suicide_count := cmf_suicide - nvdrs_suicide]
tab3_county_comparison[, diff_suicide_rate := cmf_suicide_rate - nvdrs_suicide_rate]

tab3_county_comparison = melt(tab3_county_comparison[,c('fips','Year','cmf_suicide_rate','nvdrs_suicide_rate')], id.var = c('fips','Year'))
tab3_county_comparison[,variable := factor(variable, 
	levels = c('cmf_suicide_rate','nvdrs_suicide_rate'),
	labels = c('MCD','MSD-US'))]

p1 = ggplot(tab3_county_comparison, aes(x=value, group=variable, fill=variable))+
	geom_density(alpha = 0.5)+
	scale_fill_viridis_d(name='data source') +
	theme_bw() + 
	theme(legend.position = 'top')+
	labs(x='Suicide Rates per 100K', y = 'Density')

tab4_county_comparison = merge(
	cmf_v2_matched[, .(
		cmf_suicide = sum(deaths),
		cmf_suicide_rate = sum(deaths)/ sum(population) * 10^5), by=c('fips','Year')],
	nvdrs_v2[, .(
		nvdrs_suicide = sum(Suic * weight),
		nvdrs_suicide_rate = weighted.mean(Suic,weight) * 10^5), by=c('county','Year')], by.x=c('fips','Year'),by.y=c('county','Year'))

tab4_county_comparison[, diff_suicide_rate := cmf_suicide_rate - nvdrs_suicide_rate]
tab4_county_comparison[, diff_suicide_count := cmf_suicide - nvdrs_suicide]

p2 = ggplot(tab4_county_comparison, aes(x=cmf_suicide_rate, y=nvdrs_suicide_rate))+
	geom_point(size=0.7, alpha=0.5) +
	theme_bw() + 
	geom_abline(slope=1, intercept=0)+
	labs(x='Suicide Rate per 100K (from MCD)', y = 'Suicide Rate per 100K (from MSD-US)')

p = grid.arrange(p1,p2, nrow=1)
ggsave(file.path(figure_dir, 'figure_s1_comparison_county_suicide_rate.png'), p, width = 8, height=4)

tab5_county_comparison = merge(
	cmf_v1_matched[, .(
		cmf_suicide = sum(deaths),
		cmf_suicide_rate = sum(deaths)/ sum(population) * 10^5), by=c('fips','Year')],
	nvdrs_v1[, .(
		nvdrs_suicide = sum(Suic * weight),
		nvdrs_suicide_rate = weighted.mean(Suic,weight) * 10^5), by=c('county','Year')], by.x=c('fips','Year'),by.y=c('county','Year'))

tab5 = tab5_county_comparison[, .(corr = cor(cmf_suicide_rate, nvdrs_suicide_rate)), by='Year']
tab5[, Year := as.Date(paste0(Year,'-01-01'))]

p3 = ggplot(tab5, aes(x=Year,y=corr))+
	geom_point()+
	geom_line()+
	theme_bw() + 
	ylim(c(0,1))+
	scale_x_date(date_breaks = '1 year', date_label = '%Y')+
	labs(x='Year',y='Correlations between suicide rates of MCD and MSD-US')

ggsave(file.path(figure_dir, 'figure_s1_comparison_county_corr.png'), p3, width = 8, height=5)

tab4_county_comparison[,(cmf_suicide_rate - nvdrs_suicide_rate)] %>% mean
# 1.84 per 10^5k

tab4_county_comparison[,(abs(cmf_suicide_rate - nvdrs_suicide_rate))] %>% mean
# 3.2 per 10^5k

tab4_county_comparison[,cor.test(cmf_suicide_rate, nvdrs_suicide_rate)]
# correlation = 0.87

tab5_county_comparison[,cor.test(cmf_suicide_rate, nvdrs_suicide_rate)]
# correlation = 0.90
