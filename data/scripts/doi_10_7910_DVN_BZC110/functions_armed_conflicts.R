

library(tidyverse)
library(pintervals)

run_single_version <- function(type, bins, dist, noncont,i, alpha = 0.1, version = c('randsplit','timesplit')){

	version <- match.arg(version, c('randsplit','timesplit'))


	# Load data
	if(version == 'randsplit'){
	dt <- readRDS(paste0('predictions_ac/ac_test_',i,'.rds'))

	dt <- dt %>% mutate(bin7 = case_when(truth == 0 ~ 0,
																truth <= 2 ~ 1,
																truth <= 7 ~ 2,
																truth <= 20 ~ 3,
																truth <= 54 ~ 4,
																truth <= 148 ~ 5,
																T~6),
							 bin4 = case_when(truth == 0 ~ 0,
							 								 truth <= 7 ~ 1,
							 								 truth <= 54 ~ 2,
							 								 T~3),
							 bin2 = case_when(truth == 0 ~ 0,
							 								 T~1))
	}else if(version == 'timesplit'){
		dt <- readRDS(paste0('data/predictions_timesplit.rds'))
		dt <- dt %>% mutate(bin7 = case_when(truth == 0 ~ 0,
																truth <= 2 ~ 1,
																truth <= 7 ~ 2,
																truth <= 20 ~ 3,
																truth <= 54 ~ 4,
																truth <= 148 ~ 5,
																T~6),
							 bin4 = case_when(truth == 0 ~ 0,
							 								 truth <= 7 ~ 1,
							 								 truth <= 54 ~ 2,
							 								 T~3),
							 bin2 = case_when(truth == 0 ~ 0,
							 								 T~1))
	}
	calib <- dt %>% filter(parition == 'calib')
	test <- dt %>% filter(parition == 'test')

	if(type == 'conformal'){
		prediction_intervals <- pintervals::pinterval_conformal(
			pred = test$pred,
			calib = calib$pred,
			calib_truth = log1p(calib$truth),
			lower_bound = 0,
			alpha = alpha
		) %>%
			mutate(type = 'SCP')
	}else if(type == 'bccp'){

		if(noncont == 'c'){
			contig <- TRUE
		}else{
			contig <- FALSE
		}


			prediction_intervals <- suppressWarnings(
			pintervals::pinterval_bccp(
			pred = test$pred,
			calib = calib$pred,
			calib_truth = log1p(calib$truth),
			calib_bins = pull(calib[,paste0('bin',bins)]),
			contiguize = contig,
			alpha = alpha
		)) %>%
			mutate(type = paste0('BCCP',noncont,' (',bins,')'))

	}else if(type == 'bootstrap'){
		prediction_intervals <- pintervals::pinterval_bootstrap(
			pred = test$pred,
			calib = calib$pred,
			calib_truth = log1p(calib$truth),
			lower_bound = 0,
			alpha = alpha
		) %>%
			mutate(type = 'Bootstrap')

	}else if(type == 'bootstrap_nonlog'){
		prediction_intervals <- pintervals::pinterval_bootstrap(
			pred = expm1(test$pred),
			calib = expm1(calib$pred),
			calib_truth = calib$truth,
			lower_bound = 0,
			alpha = alpha
		) %>%
			mutate(type = 'Bootstrap (non-log)')

	}else if(type == 'parametric'){
		if(dist == 'lognormal'){
			prediction_intervals <- pintervals::pinterval_parametric(
				pred = expm1(test$pred),
				dist = 'norm',
				pars = list(mean = test$pred,
										sd = sqrt(mean((calib$pred-log1p(calib$truth))^2))),
				alpha = alpha,
				lower_bound = 0
			) %>%
				mutate(lower_bound = expm1(lower_bound),
							 upper_bound = expm1(upper_bound),
					type = 'Log-normal')
		}else if(dist == 'normal'){
			prediction_intervals <- pintervals::pinterval_parametric(
				pred = expm1(test$pred),
				dist = 'norm',
				pars = list(mean = expm1(test$pred),
										sd = sqrt(mean((expm1(calib$pred)-calib$truth)^2))),
				alpha = alpha,
				lower_bound = 0
			) %>%
				mutate(type = 'Normal')
		}else if(dist == 'poisson'){
			prediction_intervals <- pintervals::pinterval_parametric(
				pred = expm1(test$pred),
				dist = 'pois',
				pars = list(lambda = expm1(test$pred)),
				alpha = alpha,
				lower_bound = 0
			) %>%
				mutate(type = 'Poisson')
		}else if(dist == 'negbin'){

			theta_est <- suppressWarnings(MASS::glm.nb(truth ~ pred, data = calib))$theta

			prediction_intervals <- pintervals::pinterval_parametric(
				pred = expm1(test$pred),
				dist = 'nbinom',
				pars = list(size = theta_est,
										mu = expm1(test$pred)),
				alpha = alpha,
				lower_bound = 0
			) %>%
				mutate(type = 'Negative Binomial')



	}else if(dist == 'negbin2'){

		theta_est <- suppressWarnings(MASS::glm.nb(truth ~ pred, data = calib))$theta

		prediction_intervals <- pintervals::pinterval_parametric(
			pred = expm1(test$pred),
			dist = 'nbinom',
			pars = list(size = 1/theta_est,
									mu = expm1(test$pred)),
			alpha = alpha,
			lower_bound = 0
		) %>%
			mutate(type = 'Negative Binomial(2)')


	}
}


if(is.na(noncont) || noncont == 'c'){

	if(type %in% c('conformal','bccp','bootstrap') | (!is.na(dist) && dist == 'normal')){

	prediction_intervals <- prediction_intervals	%>%
		mutate(truth = test$truth,
					 pred = expm1(test$pred),
					 bin7 = test$bin7,
					 bin4 = test$bin4,
					 bin2 = test$bin2,
					 logwidth = upper_bound - lower_bound,
					 lower_bound = round(expm1(lower_bound)),
					 upper_bound = round(expm1(upper_bound)),
					 width = upper_bound - lower_bound,
					 hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1,
					 								TRUE ~ 0))

	return(prediction_intervals)
	}else{
		prediction_intervals <- prediction_intervals	%>%
			mutate(truth = test$truth,
						 pred = expm1(test$pred),
						 bin7 = test$bin7,
						 bin4 = test$bin4,
						 bin2 = test$bin2,
						 logwidth = log1p(upper_bound) - log1p(lower_bound),
						 lower_bound = round(lower_bound),
						 upper_bound = round(upper_bound),
						 width = upper_bound - lower_bound,
						 hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1,
						 								TRUE ~ 0))

		return(prediction_intervals)
	}
}else{

	prediction_intervals <- prediction_intervals %>% mutate(truth = test$truth,
																		pred = expm1(test$pred),
																		bin7 = test$bin7,
																		bin4 = test$bin4,
																		bin2 = test$bin2)

	prediction_intervals2 <- 	foreach::foreach(i = 1:nrow(prediction_intervals),.final = bind_rows) %do%
		bind_cols(prediction_intervals[i,],
							tibble(width = sum(round(expm1(prediction_intervals[i,]$intervals[[1]]$upper_bound)) - round(expm1(prediction_intervals[i,]$intervals[[1]]$lower_bound))),
										 logwidth = sum(prediction_intervals[i,]$intervals[[1]]$upper_bound - prediction_intervals[i,]$intervals[[1]]$lower_bound),
										 hit = max(prediction_intervals[i,]$truth >= round(expm1(prediction_intervals[i,]$intervals[[1]]$lower_bound)) & prediction_intervals[i,]$truth <= round(expm1(prediction_intervals[i,]$intervals[[1]]$upper_bound)))))

	return(prediction_intervals2)

}

}





