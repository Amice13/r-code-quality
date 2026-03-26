#' --------------
# sameness main effects overtime ----
# figure 3, Panels A and B
#' ==============


ff_all = file.path(output_dir, 'margins', 
     paste0('v1_mchange_',2005:2017,'_unemployed_sameness_county.txt'))

coef_for_plot = rbindlist(lapply(ff_all, function(ff){
	dt = fread(ff)
	dt$year = strsplit(basename(ff),'_')[[1]][3]
	return(dt)
}))

coef_for_plot[,V6 := NULL]

coef_for_plot[,sig := ifelse(`p-value` < 0.05, 'sig', 'insig')]
coef_for_plot[,date := as.Date(paste0(year,'-01-01'))]
for (var in c('Change','LL','UL')) coef_for_plot[, (var) := get(var) * 10^5]

p = ggplot(coef_for_plot[grepl('Marginal',V1),], 
	aes(x = as.factor(year), y = Change, ymin = LL, ymax = UL, linetype = sig)) + 
  geom_pointrange(color='black', position=position_dodge2(width=0.3))+
  geom_hline(yintercept=0,lty=1,lwd=0.1)+
  labs(subtitle=NULL,
       y='sameness effects on suicide rates (per 100k)', x='') + 
  theme_bw() + 
  scale_linetype_manual(name = NULL, values = c('sig'='solid','insig'='dashed'))+
  theme(legend.position = 'none')

ggsave(file=file.path(figure_dir,'figure3_a_sameness_effects_overtime.pdf'),plot=p, width=6,height=4)


comparison_plot = fread(file.path(output_dir, 'sameness_overtime_comparison_against_unemployed.csv'))

comparison_plot[, sig := ifelse(ll > 0, 'sig','insig')]
for (var in c('diff','ll','ul')) comparison_plot[, (var) := (-1) * get(var) * 10^5]

colors_choice = pal_nejm("default")(3)[c(1,2,3)]

p = ggplot(comparison_plot, 
	aes(x = as.character(year), y = diff, ymin = ll, ymax = ul,
          linetype = sig, shape=comparison,
          color=comparison)) + 
  geom_pointrange(position=position_dodge2(width=0.3))+
  geom_hline(yintercept=0,lty=1,lwd=0.1)+
  labs(subtitle=NULL,
       y='diff in sameness effects on suicide rates (per 100k)', x='') + 
  theme_bw() + 
  scale_linetype_manual(name = NULL, values = c('sig'='solid','insig'='dashed'))+
  scale_shape_manual(name=NULL, values = c('Employed vs Unemployed'='square','NLF vs Unemployed'='triangle'))+
  scale_color_manual(name = NULL, values = c('Employed vs Unemployed'=colors_choice[1], 'NLF vs Unemployed'=colors_choice[3]))+
  theme(legend.position = 'top') 

ggsave(file=file.path(figure_dir,'figure3_b_sameness_effects_overtime_relative.pdf'),
     plot=p, width=6,height=4)

