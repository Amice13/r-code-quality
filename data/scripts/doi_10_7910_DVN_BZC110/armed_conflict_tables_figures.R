
cat("\n\n\n===================================\nCreating table 2 and figure 3-4\n\n")
library(tidyverse)
library(foreach)
library(doParallel)

summarize_sims_ac <- function(intervals){
	agg <- intervals %>% group_by(type) %>%
		summarize(hitrate = mean(hit), logwidth = mean(logwidth),
							width = mean(width)) %>%
		mutate(level = 'aggregate')

	bin2 <- intervals %>% group_by(type, bin2) %>%
		summarize(hitrate = mean(hit), logwidth = mean(logwidth),
							width = mean(width),.groups = 'drop') %>%
		mutate(level = 'bin2')

	bin4 <- intervals %>% group_by(type, bin4) %>%
		summarize(hitrate = mean(hit), logwidth = mean(logwidth),
							width = mean(width),.groups = 'drop') %>%
		mutate(level = 'bin4')

	bin7 <- intervals %>% group_by(type, bin7) %>%
		summarize(hitrate = mean(hit), logwidth = mean(logwidth),
							width = mean(width),.groups = 'drop') %>%
		mutate(level = 'bin7')

	return(bind_rows(agg, bin2, bin4, bin7))

}

registerDoParallel(cores = 10)

tictoc::tic()

ac_test <- foreach(i = 1:length(list.files("intervals_ac")),.final=bind_rows) %dopar% {
	summarize_sims_ac(readRDS(list.files("intervals_ac", full.names = T)[i]))
}

tictoc::toc()




t2_1 <- ac_test %>% filter(level == 'aggregate',
													 type != 'Negative Binomial',
													 type != 'Normal',
													 !str_detect(type, 'BCCPc')) %>%
	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
													type == 'BCCPd (7)' ~ 'BCCPd (7 bins)',
													type == 'Negative Binomial(2)' ~ 'Negative Binomial',
													type == 'Bootstrap' ~ 'Bootstrap (log)',
													type == 'Bootstrap (non-log)' ~ 'Bootstrap',
													T ~ type)) %>%
	group_by(type) %>%
	summarize(Aggregate = mean(hitrate)) %>%
	rename(Method = type) %>%
	ungroup()


t2_2 <- ac_test %>% filter(level == 'bin2',
													 type != 'Negative Binomial',
													 type != 'Normal',
													 !str_detect(type, 'BCCPc')) %>%
	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
													type == 'BCCPd (7)' ~ 'BCCPd (7 bins)',
													type == 'Negative Binomial(2)' ~ 'Negative Binomial',
													type == 'Bootstrap' ~ 'Bootstrap (log)',
													type == 'Bootstrap (non-log)' ~ 'Bootstrap',
													T ~ type)) %>%
	group_by(type,bin2) %>%
	summarize(hitrate = mean(hitrate)) %>%
	ungroup() %>%
	pivot_wider(names_from = bin2, values_from = hitrate) %>%
	rename(Method = type,
				 `Non-Zero` = `1`,
				 Zero = `0`)

## Table 2

if(!dir.exists('tables')){
	dir.create('tables')
}

left_join(t2_1, t2_2) %>%
	xtable::xtable() %>%
	xtable::print.xtable(include.rownames = F, file = 'tables/table2.tex')



### Plots
#group_by(truth,type,type2) %>%
#summarize(hit = mean(hit), logwidth = mean(logwidth)) %>% ungroup() %>%


bin_labs <- c("0" = '0', "1" = '1-2',
							"2" = '3-7', "3" = '8-20',
							"4" = '21-54', "5" = '55-148',
							"6" = '149+')


ac_test %>% filter(level == 'bin7',
									 type != 'Negative Binomial',
									 type != 'Normal',
									 !str_detect(type, 'BCCPc')) %>%
	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
													type == 'BCCPd (7)' ~ 'BCCPd (7 bins)',
													type == 'Bootstrap' ~ 'Bootstrap (log)',
													type == 'Bootstrap (non-log)' ~ 'Bootstrap',
													type == 'Negative Binomial(2)' ~ 'Negative Binomial',
													T ~ type)) %>%
	group_by(type,bin7) %>%
	summarize(hitrate = mean(hitrate)) %>%
	ungroup() %>%
	mutate(type2 = case_when(str_detect(type, 'CP') ~ 'CP',
													 T ~ 'xalternative')) %>%
	ggplot(aes(x = type, y = hitrate, fill = factor(bin7))) +
	scale_fill_manual(values = c(wesanderson::wes_palette('Darjeeling1',5),wesanderson::wes_palette('Darjeeling2',2)),name = 'Fatalities',
										labels = bin_labs)+
	geom_hline(yintercept = 0.9, linetype = 'dashed',lwd = 0.25)+
	geom_col(position = 'dodge')+
	facet_wrap(~type2, scales = 'free',nrow = 3)+
	xlab('Method')+ylab('Coverage')+
	theme_minimal()+
	theme(axis.text.x = element_text(size = 12, face = 'bold'),
				axis.text.y = element_text(size = 12, face = 'bold'),
				axis.title = element_text(size = 14, face = 'bold'),
				legend.title = element_text(size = 14, face = 'bold'),
				legend.text = element_text(size = 12, face = 'bold'),
				strip.text = element_blank())


if(!dir.exists('figures')){
	dir.create('figures')
}

ggsave("figures/figure3.pdf", width = 30, height = 20, dpi = 300,units = 'cm')

ac_test2 <- foreach(i = 1:length(list.files("intervals_ac")),.final=bind_rows) %dopar% {
	readRDS(list.files("intervals_ac", full.names = T)[i]) %>% filter(truth>0)
}

ac_test2 <- ac_test2 %>% filter(type != 'Negative Binomial',
																type != 'Bootstrap (non-log)',
																type != 'Poisson',
																type != 'Negative Binomial(2)',
									 type != 'Normal',
									 !str_detect(type, 'BCCPc')) %>%
	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
													type == 'BCCPd (7)' ~ 'BCCPd (7 bins)',
													type == 'Bootstrap' ~ 'Bootstrap (log)',
													type == 'Bootstrap (non-log)' ~ 'Bootstrap',
													type == 'Negative Binomial(2)' ~ 'Negative Binomial',
													T ~ type))



tictoc::tic()
ac_test2 %>%
	mutate(pred = case_when(type == 'Quantreg' ~ expm1(pred),
													T ~ pred)) %>%
	#filter(truth>0) %>%
	ggplot(aes(x = truth, y = width, color = type)) +
	geom_smooth(se = F, lwd = 1.25)+
	coord_cartesian(xlim = c(1,1100),ylim = c(0,8000)) +
	scale_x_continuous(trans = 'log1p', breaks = c(0,1,3,8,21,55,148,400, 1100))+
	scale_y_continuous(trans = 'log1p', breaks = c(1,3,8,21,55,148,400, 1100, 3000, 8000))+
	theme_minimal()+
	labs(x = 'True number of fatalities',
			 y = 'Smoothed average width',
			 color = 'Method')+
scale_color_manual(values = c(wesanderson:::wes_palette('Darjeeling1',5),
															wesanderson:::wes_palette('Darjeeling2',5)[c(2,5)]))+
	theme(legend.title = element_text(size = 14, face = 'bold'),
				legend.text = element_text(size = 12, face = 'bold'),
				axis.text.x = element_text(size = 12, face = 'bold'),
				axis.text.y = element_text(size = 12, face = 'bold'),
				axis.title = element_text(size = 14, face = 'bold'))
tictoc::toc()

ggsave("figures/figures4.pdf", width = 30, height = 10, dpi = 300,units = 'cm')

tictoc::toc()




#### Timesplit: Not in paper!

# ac_test_timesplit <- readRDS('data/timesplit_intervals.rds')
#
# ac_test_timesplit_summarized <- summarize_sims_ac(ac_test_timesplit)
#
# t3.1 <- ac_test_timesplit_summarized %>% filter(level == 'aggregate',
# 																								type != 'Normal',
# 																								!str_detect(type, 'BCCPc')) %>%
# 	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
# 													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
# 													type == 'BCCPd (7)' ~ 'BCCPd (7 bins)',
# 													#type == 'Negative Binomial(2)' ~ 'Negative Binomial',
# 													T ~ type)) %>%
# 	group_by(type) %>%
# 	summarize(hitrate = mean(hitrate)) %>%
# 	rename(Method = type,
# 					Aggregate = hitrate)
#
# t3.2 <- ac_test_timesplit_summarized %>% filter(level == 'bin2',
# 																						#type != 'Negative Binomial',
# 																						type != 'Normal',
# 																						!str_detect(type, 'BCCPc')) %>%
# 	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
# 													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
# 													type == 'BCCPd (7)' ~ 'BCCPd (7 bins)',
# 													#type == 'Negative Binomial(2)' ~ 'Negative Binomial',
# 													T ~ type)) %>%
# 	group_by(type,bin2) %>%
# 	summarize(hitrate = mean(hitrate)) %>%
# 	ungroup() %>%
# 	pivot_wider(names_from = bin2, values_from = hitrate) %>%
# 	rename(Method = type,
# 				 `Non-Zero` = `1`,
# 				 Zero = `0`)
#
# t3 <- left_join(t3.1,t3.2)
#
#
#
# bin_labs <- c("0" = '0', "1" = '1-2',
# 							"2" = '3-7', "3" = '8-20',
# 							"4" = '21-54', "5" = '55-148',
# 							"6" = '149+')
#
# ac_test_timesplit_summarized %>% filter(level == 'bin7',
# 									 type != 'Negative Binomial(2)',
# 									 type != 'Normal',
# 									 !str_detect(type, 'BCCPc')) %>%
# 	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
# 													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
# 													type == 'BCCPd (7)' ~ 'BCCPd (7 bins)',
# 													type == 'Bootstrap' ~ 'Bootstrap (log)',
# 													type == 'Bootstrap (non-log)' ~ 'Bootstrap',
# 													type == 'Negative Binomial(2)' ~ 'Negative Binomial',
# 													T ~ type)) %>%
# 	group_by(type,bin7) %>%
# 	summarize(hitrate = mean(hitrate)) %>%
# 	ungroup() %>%
# 	mutate(type2 = case_when(str_detect(type, 'CP') ~ 'CP',
# 													 T ~ 'xalternative')) %>%
# 	ggplot(aes(x = type, y = hitrate, fill = factor(bin7))) +
# 	scale_fill_manual(values = c(wesanderson::wes_palette('Darjeeling1',5),wesanderson::wes_palette('Darjeeling2',2)),name = 'Fatalities',
# 										labels = bin_labs)+
# 	geom_hline(yintercept = 0.9, linetype = 'dashed',lwd = 0.25)+
# 	geom_col(position = 'dodge')+
# 	facet_wrap(~type2, scales = 'free',nrow = 3)+
# 	xlab('Method')+ylab('Coverage')+
# 	theme_minimal()+
# 	theme(axis.text.x = element_text(size = 12, face = 'bold'),
# 				axis.text.y = element_text(size = 12, face = 'bold'),
# 				axis.title = element_text(size = 14, face = 'bold'),
# 				legend.title = element_text(size = 14, face = 'bold'),
# 				legend.text = element_text(size = 12, face = 'bold'),
# 				strip.text = element_blank())
#
# ggsave("", width = 30, height = 10, dpi = 300,units = 'cm')

cat("\n\n\n===================================\nReplication completed at: ", paste(Sys.time()), "\n")

#cat("Total time taken: ", difftime(Sys.time(), start_time, units = "mins"), " minutes\n")
