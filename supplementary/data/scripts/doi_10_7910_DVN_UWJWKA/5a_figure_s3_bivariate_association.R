#' --------------
# bivariate associateion between employment status and suicide ----
# county-level / CZ-level
#' ==============

# load data
reg_data = read_fst(file.path(processed_notshare_dir,'regtable_unemployment_county_v2.fst'), as.data.table = TRUE,
	columns = c('county','Year','sample_v1','sample_v2','weight',
		'AgeGrp4','Suic','empstat'))

reg_data = reg_data[AgeGrp4 > 0, ]

county_dat = reg_data[ sample_v2 == 1 & !is.na(county), .(
		suicide_rate = weighted.mean(Suic, w = weight, na.rm=TRUE),
		RAT_empstat_1 = weighted.mean(empstat == 1, w = weight, na.rm=TRUE),
		RAT_empstat_2 = weighted.mean(empstat == 2, w = weight, na.rm=TRUE),
		RAT_empstat_3 = weighted.mean(empstat == 3, w = weight, na.rm=TRUE)
	), by = c('county')]

reg_data = read_fst(file.path(processed_notshare_dir,'regtable_unemployment_cz_v2.fst'), as.data.table = TRUE,
	columns = c('cz','Year','sample_v1','sample_v2','weight',
		'AgeGrp4','Suic','empstat'))
reg_data = reg_data[AgeGrp4 > 0, ]

cz_dat = reg_data[ sample_v2 == 1 & !is.na(cz), .(
		suicide_rate = weighted.mean(Suic, w = weight, na.rm=TRUE),
		RAT_empstat_1 = weighted.mean(empstat == 1, w = weight, na.rm=TRUE),
		RAT_empstat_2 = weighted.mean(empstat == 2, w = weight, na.rm=TRUE),
		RAT_empstat_3 = weighted.mean(empstat == 3, w = weight, na.rm=TRUE)
	), by = c('cz')]

tab_county = melt(county_dat, id.var = c('suicide_rate','county'))
tab_county[, geo_unit := 'county']
setnames(tab_county, 'county', 'geoid')

tab_cz = melt(cz_dat, id.var = c('suicide_rate','cz'))
tab_cz[, geo_unit := 'cz']
setnames(tab_cz, 'cz', 'geoid')

tab_geo = rbind(tab_county, tab_cz)

tab_geo[,variable := factor(variable, 
  levels = c('RAT_empstat_1','RAT_empstat_2','RAT_empstat_3'),
  labels = c('% Employed','% Unemployed','% Not in labor force'))]
tab_geo[, suicide_rate := suicide_rate * 10^5]

tab_geo[, value_cat := cut(value, include.lowest=TRUE,breaks = c(quantile(value, probs = seq(from=0, to=1, by = 0.05)))), by = c('variable','geo_unit')]

tab_geo[,value_cat_num1 := tstrsplit(gsub('\\[|\\]|\\(|\\)', '',value_cat),',',fixed=TRUE)[1]]
tab_geo[,value_cat_num2 := tstrsplit(gsub('\\[|\\]|\\(|\\)', '',value_cat),',',fixed=TRUE)[2]]
tab_geo[,value_cat_num1 := as.numeric(value_cat_num1)]
tab_geo[,value_cat_num2 := as.numeric(value_cat_num2)]
tab_geo[,value_cat_num := (value_cat_num1 + value_cat_num2)/2 ]

calculate_mean_ci = function(x){
  xt = t.test(x)  
  return(list(xt$estimate,xt$conf.int[1],xt$conf.int[2]))
}

mean_tab_geo = tab_geo[, calculate_mean_ci(suicide_rate), by = c('value_cat','value_cat_num','variable','geo_unit')]
setnames(mean_tab_geo, c('V1', 'V2', 'V3'), c('mean','lci','uci'))
mean_tab_geo[, geo_unit := factor(geo_unit, levels = c('county','cz'), labels = c('County-level','CZ-level'))]

colors_choice = pal_nejm("default")(3)

p = ggplot(mean_tab_geo, aes(x=value_cat_num, y = mean, ymin = lci, ymax = uci, fill=variable, color=variable)) +
  geom_point(size=4)+
  geom_errorbar(width = 0)+
  geom_smooth(method = 'gam')+
  geom_text(aes(label = round(mean)), size = 2, color='white')+
  scale_color_manual(name=NULL, values = c('% Employed'=colors_choice[2],'% Unemployed'=colors_choice[1],'% Not in labor force'=colors_choice[3]))+
  scale_fill_manual(name=NULL, values = c('% Employed'=colors_choice[2],'% Unemployed'=colors_choice[1],'% Not in labor force'=colors_choice[3]))+
  theme_bw()+
  facet_grid(geo_unit~variable, scale = 'free_x') +
  labs(y='P(Suicide) per 100k', x = 'aggregate-level employment status (%)')+
  theme(
    axis.title.x = element_text(size=10),
    axis.text.x = element_text(size=8),
    legend.position = 'top')

ggsave(file.path(figure_dir, 'figure_s3_bivariate_relationship.png'), p, width = 8, height = 6)

t.test(suicide_rate ~ as.factor(value_cat), 
  data=tab_geo[variable == '% Employed' & 
                  value_cat_num %in% tab_geo[variable == '% Employed',range(value_cat_num)],])

t.test(suicide_rate ~ as.factor(value_cat), 
  data=tab_geo[variable == '% Unemployed' & 
                  value_cat_num %in% tab_geo[variable == '% Unemployed',range(value_cat_num)],])

t.test(suicide_rate ~ as.factor(value_cat), 
  data=tab_geo[variable == '% Not in labor force' & 
                  value_cat_num %in% tab_geo[variable == '% Not in labor force',range(value_cat_num)],])

tab_geo[, cor.test(suicide_rate, value), by = c('geo_unit','variable')]
