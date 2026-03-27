# Versions to run:

# SCP
# BCCPd (2 bins)
# BCCPd (4 bins)
# BCCPd (7 bins)
# BCCPc (2 bins)
# BCCPc (4 bins)
# BCCPc (7 bins)
# Normal
# Lognormal
# Bootstrap
# Poisson
# Negative Binomial
cat("\n\n\n===================================\nRunning prediction intervals\n===================================\n\n")

library(tidyverse)
library(foreach)
library(doParallel)
library(doRNG)
source('functions/functions_armed_conflicts.R')

versions <- expand_grid(
	type = c('conformal',
					 'bccp',
					 'bootstrap',
					 'bootstrap_nonlog',
					 'parametric'
	),
	bins = c(NA,2,4,7),
	dist = c(NA,
					 'normal',
					 'lognormal',
					 'poisson',
					 'negbin',
					 'negbin2'),
	noncont = c(NA,
							'd',
							'c')
) %>%
	filter(!(type == 'bccp' & is.na(bins)),
				 !(type != 'bccp' & !is.na(bins)),
				 !(type == 'parametric' & is.na(dist)),
				 !(type != 'parametric' & !is.na(dist)),
				 !(type != 'bccp' & !is.na(noncont)),
				 !(type == 'bccp' & is.na(noncont)))


inner_function <- function(j, versions, append = FALSE){
	if(!append){
	tt <- foreach(i = 1:nrow(versions), .final = bind_rows) %do%
		run_single_version(type = versions$type[i],
											 bins = versions$bins[i],
											 dist = versions$dist[i],
											 noncont = versions$noncont[i],
											 i = j)

	# Check if the directory exists, if not create it
	if (!dir.exists('intervals_ac')) {
		dir.create('intervals_ac')
	}

	saveRDS(tt, file = paste0('intervals_ac/intervals_',j,'.rds'))
} else {
	tt <- foreach(i = 1:nrow(versions), .final = bind_rows) %do%
		run_single_version(type = versions$type[i],
											 bins = versions$bins[i],
											 dist = versions$dist[i],
											 noncont = versions$noncont[i],
											 i = j)

	# Check if the directory exists, if not create it
	if (!dir.exists('intervals_ac')) {
		dir.create('intervals_ac')
	}

	tmp_intervals <- readRDS(paste0('intervals_ac/intervals_',j,'.rds'))
	tmp_intervals <- tmp_intervals %>% filter(!(type %in% unique(tt$type)))
	saveRDS(bind_rows(tmp_intervals,tt), file = paste0('intervals_ac/intervals_',j,'.rds'))
}
	cat('Finished run ', j, ' of 1000', '\n')
}


registerDoParallel(cores = parallel::detectCores() - 1)

# Looping to prevent memory buildup by clearing out the R subprocesses regularly (possible bug in foreach?)
for(r in 1:10){


foreach(j = ((r-1)*100+1):(r*100), .options.RNG = 20250216+r) %dorng%
	inner_function(j, versions)

}

tmp <- foreach(i = 1:nrow(versions), .final = bind_rows) %do%
	run_single_version(type = versions$type[i],
										 bins = versions$bins[i],
										 dist = versions$dist[i],
										 noncont = versions$noncont[i],
										 i = i,
										 version = 'timesplit')

saveRDS(tmp, 'data/timesplit_intervals.rds')
