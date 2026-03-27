

### This script runs the analysis on the simulations, table 1 and figure 1-2.

cat("\n\n\n===================================\nCreating table 1 and figure 1-2\n\n")

library(tidyverse)
library(foreach)
library(doParallel)
## Uncomment the following line to run the simulations if they have not been run yet
#source('sums_run_save.R')

summarize_sims <- function(intervals){
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

	yrange <- intervals %>% mutate(yrange = case_when(truth<=1 ~ 0,
																										truth<=2 ~ 1,
																										truth<=3 ~ 2,
																										truth<=4 ~ 3,
																										truth<=5 ~ 4,
																										truth<=6 ~ 5,
																										truth<=7 ~ 6,
																										truth<=8 ~ 7,
																										truth<=9 ~ 8,
																										truth<=10 ~ 9,
																										T ~ 10)) %>%
		group_by(type, yrange) %>%
		summarize(hitrate = mean(hit), logwidth = mean(logwidth),
							width = mean(width),.groups = 'drop') %>%
		mutate(level = 'yrange')

	return(bind_rows(agg, bin2, bin4, yrange))

}

registerDoParallel(cores = parallel::detectCores() - 1)

tictoc::tic()

sims <- foreach(i = 1:length(list.files("intervals_sim")),.final=bind_rows) %dopar% {
	summarize_sims(readRDS(list.files("intervals_sim", full.names = T)[i]))
}

tictoc::toc()
#
# sims <- readRDS("sims/simulation_lnorm05.rds")
# sims <- foreach(i = 1:length(sims),.final=bind_rows) %do% {
# 	summarize_sims(sims[[i]])
# }
gc()


sims <- sims %>% filter(!(type %in% c('Normal','Negative Binomial','Negative Binomial(2)','Poisson')))

# Table 1

t1_1 <- sims %>%
	filter(level == 'aggregate') %>%
	group_by(type) %>%
	summarize(hitrate = mean(hitrate)) %>%
	ungroup() %>%
	pivot_longer(cols = c(hitrate), values_to = 'Aggregate') %>%
	select(-name) %>%
	arrange(type) %>%
	mutate(Aggregate = paste0(round(Aggregate,2)))

t1_2 <- sims %>% filter(level == 'bin4') %>%
	group_by(type, bin4) %>%
	summarize(hitrate = mean(hitrate)) %>%
	ungroup() %>%
	arrange(bin4,type) %>%
	pivot_longer(cols = c(hitrate)) %>%
	pivot_wider(names_from = bin4, values_from = value) %>%
	select(-name)


## Table 1

if(!dir.exists('tables')) dir.create('tables')

bind_cols(t1_1, t1_2 %>% select(-type)) %>%
	select(type, Aggregate, `1`, `2`, `3`, `4`) %>%
	xtable::xtable() %>%
	xtable::print.xtable(include.rownames = F, file = 'tables/table1.tex')




### Plots

bin_labs <- c(`0` = '0-1', `1` = '1-2', `2` = '2-3', `3` = '3-4', `4` = '4-5', `5` = '5-6', `6` = '6-7', `7` = '7-8', `8` = '8-9', `9` = '9-10', `10` = '10+')

# cp_labs2 <- c(`2` = 'BCCPd (2 bins)',
# 							`4` = 'BCCPd (4 bins)',
# 							`6` = 'BCCPd (6 bins)',
# 							`0` = 'SCP')

sims %>% filter(level == 'yrange') %>%
	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
													type == 'BCCPd (6)' ~ 'BCCPd (6 bins)',
													type == 'BCCPc (2)' ~ 'BCCPc (2 bins)',
													type == 'BCCPc (4)' ~ 'BCCPc (4 bins)',
													type == 'BCCPc (6)' ~ 'BCCPc (6 bins)',
													T ~ type)) %>%
	mutate(type2 = case_when(str_detect(type, 'BCCP') ~ 'BCCP',
													 T ~ 'xalternative')) %>%
	#filter(!str_detect(type, 'BCCPc')) %>%
	group_by(type,yrange) %>%
	summarize(hitrate = mean(hitrate),
						type2 = first(type2)) %>%
	ungroup() %>%
	ggplot(aes(x = type, y = hitrate, fill = factor(yrange))) +
	scale_fill_manual(values = c(wesanderson::wes_palette('Darjeeling1',5),wesanderson::wes_palette('Darjeeling2',5), wesanderson::wes_palette('BottleRocket1',1)),name = 'True value of y',
										labels = bin_labs)+
	facet_wrap(~type2, scales = 'free',nrow=2)+
	geom_hline(yintercept = 0.9, linetype = 'dashed',lwd = 0.25)+
	geom_col(position = 'dodge')+
	xlab('Method')+ylab('Coverage')+
	theme_minimal()+
	theme(axis.text.x = element_text(size = 12, face = 'bold'),
				axis.text.y = element_text(size = 12, face = 'bold'),
				axis.title = element_text(size = 14, face = 'bold'),
				legend.title = element_text(size = 14, face = 'bold'),
				legend.text = element_text(size = 12, face = 'bold'),
				strip.text = element_blank())

if(!dir.exists('figures')) dir.create('figures')

ggsave("figures/figure1.pdf", width = 30, height = 20, dpi = 300,units = 'cm')


sims2 <- foreach(i = 1:length(list.files("intervals_sim")),.final=bind_rows) %dopar% {
	readRDS(list.files("intervals_sim", full.names = T)[i])
}

sims2 %>%
	mutate(type = case_when(type == 'BCCPd (2)' ~ 'BCCPd (2 bins)',
													type == 'BCCPd (4)' ~ 'BCCPd (4 bins)',
													type == 'BCCPd (6)' ~ 'BCCPd (6 bins)',
													type == 'BCCPc (2)' ~ 'BCCPc (2 bins)',
													type == 'BCCPc (4)' ~ 'BCCPc (4 bins)',
													type == 'BCCPc (6)' ~ 'BCCPc (6 bins)',
													T ~ type)) %>%
	filter(!str_detect(type, 'c'),
				 type != 'Normal',
				 !str_detect(type, 'Bootstrap')) %>%
	#filter(truth>0) %>%
	#mutate(bins = case_when(type == 'SCP' ~ 0, T ~ bins),
	#			 bins2 = factor(bins, levels = c('0','2','4','6'))) %>%
	ggplot(aes(x = truth, y = width, color = type)) +
	geom_smooth(se = F, lwd = 1.25)+
	coord_cartesian(xlim = c(0,15),ylim = c(0,9)) +
	scale_x_continuous(trans = 'log1p', breaks = c(0,1,3,7,15))+
	#scale_y_continuous(trans = 'log1p', breaks = c(1,3,8,21,55,148,500, 5000))+
	theme_minimal()+
	labs(x = 'True value of y',
			 y = 'Smoothed average width',
			 color = 'Method')+
	scale_color_manual(values = c(wesanderson::wes_palette('Darjeeling1',5),wesanderson::wes_palette('Darjeeling2',2)[2]))+
	theme(legend.title = element_text(size = 14, face = 'bold'),
				legend.text = element_text(size = 12, face = 'bold'),
				axis.text.x = element_text(size = 12, face = 'bold'),
				axis.text.y = element_text(size = 12, face = 'bold'),
				axis.title = element_text(size = 14, face = 'bold'))


ggsave("figures/figure2.pdf", width = 30, height = 10, dpi = 300,units = 'cm')


cat("\n\n\n===================================\nFinished simulation replication at: ", paste(Sys.time()), "\n")

