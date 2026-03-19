#' --------------
# bivariate associations between employment status and suicide ----
# county-level / CZ-level
# individual level over-time
#' ==============

# add global monthly unemployment rates ----
us_unemployment = import(file.path(rawdata_share_dir, 'monthly_unemployment_US_all_1948_2021.xlsx'),skip=11)
setDT(us_unemployment)

us_unemployment = melt(us_unemployment, id.var='Year')
us_unemployment[,date := as.Date(paste0(Year,'-',variable,'-01'),format='%Y-%B-%d')]
setnames(us_unemployment, 'value', 'monthly_unemployment')
us_unemployment = us_unemployment[Year %in% 2005:2017, ]

p1 = ggplot(us_unemployment, aes(x=date, y=monthly_unemployment)) +
  geom_line(color='red') +
  theme_bw() +
  labs(y = 'Unemployment Rate', x = NULL, subtitle='Panel A.')+
  ylim(c(0, 11))+
  xlim(c(as.Date('2005-01-01'), as.Date('2017-10-31')))+
  ggplot2::annotate("rect", xmin = as.Date('2007-12-01'), xmax = as.Date('2009-06-30'), ymin = 0, ymax = 11, alpha = .2)

cmf = read_fst(file.path(rawdata_notshare_dir, 'cmf_nchs_county_suicide_2005_2017.fst'), as.data.table=TRUE)

cmf_year = cmf[, .(
  suicide_rate = sum(deaths) / sum(population) * 10^5
  ), by = 'Year']
setnames(cmf_year, 'Year', 'date')

cmf_year[, date := as.Date(paste0(date, '-07-01'))]

p2 = ggplot(cmf_year, aes(x=date, y=suicide_rate)) +
  geom_point(size = 3)+
  #geom_text(aes(label = round(suicide_rate)), size = 2, color='white')+
  geom_line(color='black') +
  theme_bw() +
  labs(y = 'Suicide Rate per 100k', x = NULL, subtitle = "Panel B.") +
  ylim(c(0, 20))+
  xlim(c(as.Date('2005-01-01'), as.Date('2017-10-31')))+
  theme(
    axis.title.y = element_text(vjust = 0.5),
    axis.text.y = element_text(size=7),)+
  ggplot2::annotate("rect", xmin = as.Date('2007-12-01'), xmax = as.Date('2009-06-30'), ymin = 0, ymax = 20, alpha = .2)

# load data
reg_data = read_fst(file.path(processed_notshare_dir,'regtable_unemployment_county_v2.fst'), as.data.table = TRUE,
  columns = c('county','Year','sample_v1','sample_v2','weight',
    'AgeGrp4','Suic','empstat'))
reg_data = reg_data[AgeGrp4 > 0, ]

mean_tab_county = reg_data[!is.na(empstat) & sample_v1 == 1, .(
  suicide_rate = weighted.mean(Suic, weight, na.rm=TRUE) * 10^5,
  suicide_rate_se = diagis::weighted_se(Suic, weight, na.rm=TRUE) * 10^5
  ), by = c('empstat','Year')]

mean_tab_county[, `:=`(
  lci = suicide_rate - 2 * suicide_rate_se,
  uci = suicide_rate + 2 * suicide_rate_se)
]

colors_choice = pal_nejm("default")(3)
mean_tab_county[, empstat_label := factor(empstat, levels = 1:3, labels = c('Employed','Unemployed','Not in labor force'))]

mean_tab_county[, date := as.Date(paste0(Year, '-07-01'))]

p3 = ggplot(mean_tab_county, aes(
  x = date, y = suicide_rate, ymin = lci, ymax = uci, 
  color=empstat_label, fill=empstat_label, shape=empstat_label)) +
  geom_point(size=4)+
  geom_line(aes(group=empstat_label))+
  geom_errorbar(width = 0)+
  geom_text(aes(label = round(suicide_rate)), size = 2, color='white')+
  ggplot2::annotate("text", x = as.Date('2006-01-01'), y = 17, color=colors_choice[2], label = 'Employed')+
  ggplot2::annotate("text", x = as.Date('2006-01-01'), y = 35, color=colors_choice[1], label = 'Unemployed')+
  ggplot2::annotate("text", x = as.Date('2006-01-01'), y = 10, color=colors_choice[3], label = 'Not in Labor-Force')+
  scale_color_manual(name=NULL, values = c('Employed'=colors_choice[2],'Unemployed'=colors_choice[1],'Not in labor force'=colors_choice[3]))+
  scale_fill_manual(name=NULL, values = c('Employed'=colors_choice[2],'Unemployed'=colors_choice[1],'Not in labor force'=colors_choice[3]))+
  scale_shape_manual(name=NULL, values = c('Employed'='square','Unemployed'='circle','Not in labor force'='triangle'))+
  theme_bw()+
  labs(y='P(Suicide) per 100k', x = 'Year', subtitle='Panel C.')+
  theme(
    axis.title.x = element_text(size=10),
    axis.text.x = element_text(size=8),
    legend.position = 'none') +
  ylim(c(0, 50)) +
  xlim(c(as.Date('2005-01-01'), as.Date('2017-10-31')))+
  ggplot2::annotate("rect", xmin = as.Date('2007-12-01'), xmax = as.Date('2009-06-30'), ymin = 0, ymax = 50, alpha = .2)

p = grid.arrange(p1, p2, p3, ncol = 1)

ggsave(file.path(figure_dir, 'figure1_overtime_combined.pdf'), p, width = 6.5, height = 8)

