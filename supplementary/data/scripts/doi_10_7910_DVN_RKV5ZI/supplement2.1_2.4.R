####figure S5####
#directly generated in yEd (see separate .graphml file in figures folder)

####table S3####
#directly generated in Word

####table S4####
cv_group6_full <- read.csv("./../data_set_up_script/03_dataframes_variables/a_group_unit_year/group_unit_year_basis_full.csv")
aut_corr <- subset(cv_group6_full, other == 0 & nopluralitysum >= 0.05)
aut_corr <- unique(aut_corr[c("sa_territory_t","sa_policy_territory_t","sa_fiscal_territory_t","sa_political_territory_t")])
tables4 <- cor(aut_corr, use ="pairwise.complete.obs")
write.csv(tables4, file='../tables/tables4.csv')

####figure S6####
cv_group6_full <- read.csv("./../data_set_up_script/03_dataframes_variables/a_group_unit_year/group_unit_year_basis_full.csv")
aut_plot_description <- subset(cv_group6_full, other == 0 & nopluralitysum >= 0.05)
aut_plot_description <- unique(aut_plot_description[c("cowcode","region","fips","year","adm_abs","sa_territory_t")])
aut_plot_description <- aut_plot_description %>% group_by(cowcode,year) %>% mutate(sa_territory_t_avg = sum(adm_abs * sa_territory_t) / sum(adm_abs))
aut_plot_description <- unique(aut_plot_description[c("cowcode","region","year","sa_territory_t_avg")])
aut_plot_description <- aut_plot_description %>% group_by(year) %>% mutate(autonomy_w_avg = mean(sa_territory_t_avg,na.rm=T))
aut_plot_description_w <- unique(aut_plot_description[c("year","autonomy_w_avg")])
figures6 <- ggplot(aut_plot_description_w, aes(x = year, y = autonomy_w_avg)) + geom_line() +
  xlab("year") + ylab("avg.\nterritorial autonomy") + theme_bw() + theme(text=element_text(family="Times"), legend.position="bottom", legend.title=element_text(size=10), axis.title=element_text(size=10)) + scale_x_continuous(breaks = c(1988, 1993, 1998, 2003, 2008, 2013, 2018))
ggsave(figures6, file='../figures/figures6.pdf', width = 17, height = 6, units="cm")
