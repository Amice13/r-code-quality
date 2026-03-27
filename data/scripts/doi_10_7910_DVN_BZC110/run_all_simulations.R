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
source('functions/package_checker.R')
cat("\n\n\n===================================\nRunning simulation replication\n===================================\n\nStart time: ", paste(Sys.time()), "\n\n")
library(tidyverse)
library(foreach)
library(doParallel)
library(doRNG)
source('functions/functions_simulations.R')

versions <- expand_grid(
	type = c('conformal',
					 'bccp',
					 'bootstrap',
					 'bootstrap_log',
					 'parametric',
					 'quantreg'
	),
	bins = c(NA,2,4,6),
	dist = c(NA,
					 'normal',
					 'lognormal'),
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


inner_function <- function(version){
	filename <- ifelse(version$type == 'bccp', paste0(version$type,version$bins,version$noncont),
										 ifelse(version$type == 'parametric', paste0(version$dist),
														version$type))
	if(version$type != 'quantreg'){

		# Check if directory exists
		if(!dir.exists('intervals_sim')){
			dir.create('intervals_sim')
		}

		saveRDS(run_single_sim(dt = sim_generate_data_lnorm(),
													 type = version$type,
													 bins = version$bins,
													 dist = version$dist,
													 noncont = version$noncont,
													 j = version$j),
					 file = paste0('intervals_sim/',filename,version$j,'.rds'))



	} else {

		if(!dir.exists('intervals_sim')){
			dir.create('intervals_sim')
		}

	saveRDS(sim_generate_data_lnorm(quantreg = TRUE, j = version$j),file = paste0('intervals_sim/',filename,version$j,'.rds'))
	}
	if(version$k %% 100 == 0){
		cat('Simulation:',version$k,'of',nrow(versions),'done \n')
	}
	gc()
}

set.seed(20250216)
## Randomize order to balance load on cluster
versions <- expand_grid(versions,j = 1:1000) %>%
	slice_sample(prop=1)

## Add row number for progress tracking
versions <- versions %>%
	mutate(k = row_number())

registerDoParallel(cores = parallel::detectCores() - 1)

#Looping to prevent memory buildup by clearing out the R subprocesses regularly (possible bug in foreach?)

for(r in 1:12){
	foreach(i = ((r-1)*1000+1):(r*1000), .options.RNG = 20250216+r) %dorng%
		inner_function(versions[i,])
}
#
# foreach(i = 1:nrow(versions), .options.RNG = 20250216) %dorng%
# 	inner_function(versions[i,])

#Append quantreg intervals

# foreach(j = 1:1000, .options.RNG = 20250216) %dorng%
# 	sim_generate_data_lnorm(j = j, quantreg = TRUE, append_quantreg =TRUE, qr_alpha = 0.1)

