#' --------------
# Figure S5 Panels A and B ----
#' ==============
library(lmtest)

tab_all = read.csv(file.path(output_dir, 'AME_model_nonlinear_county.csv'))

extract_number = function(tab_all){
	x_unemp = as.numeric(gsub('\\=|\\-', '', tab_all[2:20,'X.']))
	y_mean = as.numeric(gsub('\\=', '', tab_all[2:20,'X..1.']))
	y_lci = as.numeric(unlist(lapply(strsplit(gsub('\\=|\\[|\\]', '', tab_all[2:20,'X..1']),','),function(x) x[1])))
	y_uci = as.numeric(unlist(lapply(strsplit(gsub('\\=|\\[|\\]', '', tab_all[2:20,'X..1']),','),function(x) x[2])))
	
	df_all = data.frame(x_unemp, y_mean, y_lci, y_uci)
	return(df_all)
}


df_all = extract_number(tab_all)

p = ggplot(df_all, aes(x=x_unemp, y=y_mean*10^5, ymin=y_lci*10^5, ymax=y_uci*10^5))+
	geom_pointrange()+
	theme_bw() +
	geom_smooth(se=FALSE)+
	geom_smooth(method='lm', color='red', se=TRUE)+
	labs(x='County-level unemployment rate across 20 intervals',
		y='Predicted Suicide Rate per 100k')
ggsave(file.path(figure_dir, 'figure_s5_a_linearity_all.png'), p, width=5, height=3)

df_year <- lapply(2005:2017, function(yy){
	tab_yy = read.csv(file.path(output_dir, paste0('AME_model_nonlinear_county_',yy,'.csv')))
	df_all = extract_number(tab_yy)
	linear_model = lm(y_mean ~ x_unemp, data=df_all)
	test = resettest(linear_model)
	cor = cor.test(df_all$x_unemp, df_all$y_mean)$estimate
	out = data.frame(
		year = yy, 
		r2 = summary(linear_model)$r.squared,
		retest_pvalue = test$p.value,
		retest_estimate = test$statistic)
	return(out)
}) |> rbindlist()

df_year[, sig := ifelse(retest_pvalue < 0.05, 'sig', 'insig')]

p = ggplot(df_year, aes(x=as.factor(year), y=retest_estimate, color=sig))+
	geom_point(size=3)+
	scale_color_manual(name=NULL, values = c('sig'='red', 'insig'='black'))+
	theme_bw() +
	theme(legend.position = 'top')+
	labs(y='RESET statistics', x='Year')

ggsave(file.path(figure_dir, 'figure_s5_b_reset_year.png'), p, width=5, height=3)

