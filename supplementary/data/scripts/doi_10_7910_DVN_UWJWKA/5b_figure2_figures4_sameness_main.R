#' --------------
# sameness main effects ----
# Figure 2
# Figure S4 in Appendix.
#' ==============

 read_margin = function(ff, atvar = NULL){
  tab = fread(ff)  
  tabnames = unlist(tab[1,])
  tab = tab[-1, ]
  colnames(tab) = tabnames
  VNAME = grep('V',names(tab), value = TRUE)
  tab[, (VNAME) := NULL]
  names(tab)[1] = 'stat'
  
  tab = melt(tab, id.var = 'stat')
  tab[, value := as.numeric(value)]
  tab = tab[!is.na(value),]

  if (!is.null(atvar)) {
    tab_at = fread(gsub('_margins_','_at_',ff))
    tab[, c('variable','variable_by') := tstrsplit(variable,'#')]

    tab  = merge(tab, tab_at[,c('V1',atvar), with=F], by.x = 'variable', by.y='V1', all.x=TRUE)
    setnames(tab, atvar, 'atvar')
  }

  return(tab)
}

ff_all = dir(file.path(output_dir, 'margins'), pattern = 'est1main')

list_tab = list() ; n = 1
for (geo_type in c('cz','county')){
	if (geo_type == 'cz') x_title <- 'CZ-level Employment Status (%)'
	if (geo_type == 'county') x_title <- 'County-level Employment Status (%)'
	
	ff_all_geo = grep(geo_type, ff_all, value = TRUE)
	ff_all_geo = grep('at', ff_all_geo, invert = TRUE, value = TRUE)
	
	ff_all_geo = file.path(output_dir, 'margins', ff_all_geo)
	tab_1 = read_margin(grep('RAT_Empl',ff_all_geo, value=TRUE), atvar = 'RAT_Empl') ; tab_1[, agg_emp := 'Employed']
	tab_2 = read_margin(grep('RAT_NotInLabor',ff_all_geo, value=TRUE), atvar = 'RAT_NotInLabor') ; tab_2[, agg_emp := 'NotInLabor']
	tab_3 = read_margin(grep('RAT_UnEmpl',ff_all_geo, value=TRUE), atvar = 'RAT_UnEmpl') ; tab_3[, agg_emp := 'UnEmployed']
	tab = rbind(tab_1, tab_2, tab_3)
	tab$geo_unit = geo_type
	list_tab[[n]] = tab
	n = n + 1
}

list_tab = rbindlist(list_tab)

tab_wide = dcast(list_tab[stat %in% c('b', 'll', 'ul','pvalue'), ], atvar + agg_emp +geo_unit~ stat, value.var = 'value')
setorder(tab_wide, 'agg_emp', 'atvar')

tab_wide[, mean := b * 10^5]
tab_wide[, UL := ul * 10^5]
tab_wide[, LL := ll * 10^5]

tab_wide[, agg_emp := factor(agg_emp, 
	levels = c('Employed','UnEmployed','NotInLabor'),
	labels = c('Employed', 'Unemployed','Not In Labor Force'))]
tab_wide[, geo_unit := factor(geo_unit, levels = c('county','cz'), labels = c('County-level','CZ-level'))]

colors_choice = pal_jama("default")(6)[c(1,4)]

p1 = ggplot(tab_wide, aes(x=atvar, y=mean, ymin=LL, ymax=UL,
	fill=geo_unit, group=geo_unit,color=geo_unit, shape = geo_unit)) + 
	geom_line() +
	geom_point() +
	geom_ribbon(alpha=0.5) +
	theme_bw()+
	scale_shape_manual(name=NULL, values = c('County-level'=4,'CZ-level'=1))+
	scale_color_manual(name=NULL, values = c('County-level'=colors_choice[2],'CZ-level'=colors_choice[1]))+
	scale_fill_manual(name=NULL, values = c('County-level'=colors_choice[2],'CZ-level'=colors_choice[1]))+
	theme(legend.position = 'top')+
	facet_grid(~agg_emp, scale= 'free_x')+
	labs(x='aggregate employment status (%)',y='Predicted P(suicide) per 100K')

ggsave(file.path(figure_dir, paste0('figure2_sameness_three_separate_all.pdf')), p1, width = 7, height = 3.5)

# by all
ff = file.path(output_dir, 'margins', paste0('v2_margins_interact_est1sameinter_std_same_prop_empstat_empstat_all_','county','.txt'))
tab_0a = read_margin(ff, atvar = 'std_same_prop_empstat') 
ff = file.path(output_dir, 'margins', paste0('v2_margins_interact_est1sameinter_std_same_prop_empstat_empstat_all_','cz','.txt'))
tab_0b = read_margin(ff, atvar = 'std_same_prop_empstat') 

tab_0a[, geo_unit := 'County Level']
tab_0b[, geo_unit := 'CZ level']
tab_0 = rbind(tab_0a, tab_0b)

tab_wide = dcast(tab_0[stat %in% c('b', 'll', 'ul','pvalue'), ], atvar + variable_by + geo_unit ~ stat, value.var = 'value')

tab_wide[, mean := b * 10^5]
tab_wide[, UL := ul * 10^5]
tab_wide[, LL := ll * 10^5]

tab_wide[, agg_emp := factor(variable_by, 
	levels = c('1.empstat', '2.empstat','3.empstat'),
	labels = c('Employed', 'Unemployed','Not In Labor Force'))]
colors_choice = pal_nejm("default")(3)

p2 = ggplot(tab_wide[abs(atvar) < 2, ], aes(x=atvar, y=mean, ymin=LL, ymax=UL,
	fill=agg_emp, group=agg_emp,color=agg_emp)) + 
	geom_line() +
	#geom_pointrange(size=0.2)+
	geom_point() +
	geom_ribbon(alpha=0.5) +
	theme_bw()+
	scale_color_manual(name=NULL, values = c('Employed'=colors_choice[2],'Unemployed'=colors_choice[1],'Not In Labor Force' = colors_choice[3]))+
	scale_fill_manual(name=NULL,  values = c('Employed'=colors_choice[2],'Unemployed'=colors_choice[1],'Not In Labor Force' = colors_choice[3]))+
	theme(legend.position = 'top') +
	facet_grid(.~geo_unit) +
	labs(x='% Same Employment Status(in SD units)',y='Predicted P(suicide) per 100K')

ggsave(file.path(figure_dir, paste0('figure_s4_sameness_alltogether.pdf')), p2, width = 7, height = 4)


