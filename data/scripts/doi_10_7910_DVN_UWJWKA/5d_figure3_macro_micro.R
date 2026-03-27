#' --------------
# Figure 3. Panels C and D
#' ==============

margin_path = file.path(output_dir, 'margins')

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
    if (grepl('margins',basename(ff))){
      tab_at = fread(gsub('_margins_','_margins_at_',ff))  
      tab  = merge(tab, tab_at[,c('V1',atvar), with=F], 
        by.x = 'variable', 
        by.y='V1', all.x=TRUE)
      
    } else {
      tab_at = fread(gsub('mchange_','mchange_at_',ff))  
      tab_at[, (atvar) := stringr::str_trim(get(atvar))]
      tab[, c('variable','variable_by') := tstrsplit(variable,'\\:')]      
      tab  = merge(tab, tab_at[,c('V1',atvar), with=F], 
        by.x = 'variable_by', 
        by.y='V1', all.x=TRUE)
      
    }
    
      setnames(tab, atvar, 'atvar')
    
  }

  return(tab)
}

ff_all = dir(file.path(output_dir, 'margins'), pattern = 'global')
ff_all = grep('mchange', ff_all, value = TRUE)

geo_type = 'county'  
ff_all_geo = grep('at', ff_all, invert = TRUE, value = TRUE)
ff_all_geo = grep(geo_type, ff_all_geo, invert = FALSE, value = TRUE)
ff_all_geo = file.path(output_dir, 'margins', ff_all_geo)

tab_1 = read_margin(grep('_unemployed',ff_all_geo, value=TRUE), atvar = 'monthly_unemployment') ; tab_1[, agg_emp := 'UnEmployed']
tab_2 = read_margin(grep('_nlf',ff_all_geo, value=TRUE), atvar = 'monthly_unemployment') ; tab_2[, agg_emp := 'NotInLabor']
tab_3 = read_margin(grep('_employed',ff_all_geo, value=TRUE), atvar = 'monthly_unemployment') ; tab_3[, agg_emp := 'Employed']

dt = rbind(tab_1, tab_2, tab_3)
dt$geo_unit = geo_type

setnames(dt, 'atvar', 'monthly_unemployment')

dt = dcast(dt, monthly_unemployment + agg_emp ~ stat, value.var = 'value')

dt[, outcome := b * 10^5]
dt[, lci := ll * 10^5]
dt[, uci := ul * 10^5]
dt[, monthly_unemployment := as.numeric(monthly_unemployment)]
dt[, sig := ifelse(pvalue < 0.05, 'sig', 'insig')]

p2 = ggplot(dt[agg_emp == 'UnEmployed',], 
  aes(x = monthly_unemployment, y = outcome, ymin = lci, ymax = uci, linetype = sig)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  scale_linetype_manual(name = NULL, values = c('sig'='solid','insig'='dashed'))+
  labs(y='Local Sameness Effects on Suicide', x = 'Monthly Unemployment Rates (%)')+
  theme(legend.position = 'none')

# add margins 
ff_all = dir(file.path(output_dir, 'margins'), pattern = 'global')
ff_all = grep('mchange', ff_all, value = TRUE, invert = TRUE)

ff_all_geo = grep('at', ff_all, invert = TRUE, value = TRUE)
ff_all_geo = grep(geo_type, ff_all_geo, invert = FALSE, value = TRUE)

ff_all_geo = file.path(output_dir, 'margins', ff_all_geo)
tab = read_margin(grep('_unemployed',ff_all_geo, value=TRUE), atvar = 'monthly_unemployment') ; tab_1[, agg_emp := 'UnEmployed']
tab$geo_unit = geo_type

setnames(tab, 'atvar', 'monthly_unemployment')
tab = dcast(tab, monthly_unemployment ~ stat, value.var = 'value')
tab[, outcome := b * 10^5]
tab[, lci := ll * 10^5]
tab[, uci := ul * 10^5]
tab[, monthly_unemployment := as.numeric(monthly_unemployment)]

p1 = ggplot(tab, aes(x = monthly_unemployment, y = outcome, ymin = ll, ymax = ul)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  labs(x='Monthly Unemployment Rate (%)', y='Suicide Rate (per 100k)') +
  theme_bw()+
  theme(legend.position = 'none')

ggsave(file=file.path(figure_dir, 'figure3_c_macro_margins.pdf'), 
  plot=p1, width=5,height=4)

ggsave(file=file.path(figure_dir, 'figure3_d_macro_mchange.pdf'),
  plot=p2, width=5,height=4)
