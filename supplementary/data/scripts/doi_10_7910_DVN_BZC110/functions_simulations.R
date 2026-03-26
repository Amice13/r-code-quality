
library(tidyverse)
library(pintervals)
library(foreach)

sim_generate_data_lnorm <- function(n = 10000, train_p = 0.5, calib_p = 0.25, nbins = 4, sd_log = 0.5, quantreg = FALSE, qr_alpha = 0.1,j=NULL){

	x1 <- runif(n)
	x2 <- runif(n)

	y <- rlnorm(n, meanlog = x1 + x2, sdlog = sd_log)

	bin2 <- cut(y, breaks = c(0,quantile(y[1:round((train_p+calib_p)*n)], probs = seq(0, 1, 1/2))[2],Inf), include.lowest = T, labels =F)

	bin4 <- cut(y, breaks = c(0,quantile(y[1:round((train_p+calib_p)*n)], probs = seq(0, 1, 1/4))[2:4],Inf), include.lowest = T, labels =F)

	bin6 <- cut(y, breaks = c(0,quantile(y[1:round((train_p+calib_p)*n)], probs = seq(0, 1, 1/6))[2:6],Inf), include.lowest = T, labels =F)

	df <- tibble(x1, x2, y, bin2, bin4, bin6)
	df_train <- df %>% slice(1:(train_p*n))
	df_cal <- df %>% slice((train_p*n+1):(train_p*n + calib_p*n))
	df_test <- df %>% slice((train_p*n + calib_p*n + 1):n)

	mod <- lm(log(y) ~ x1 + x2, data=df_train)

	df_cal <- df_cal %>%
		mutate(pred = exp(predict(mod,newdata=df_cal)),
					 truth = y,
					 partition = 'calib')

	df_test <- df_test %>%
		mutate(pred = exp(predict(mod,newdata=df_test)),
					 truth = y,
					 partition = 'test')

	if(quantreg){

		qr_mod <- quantreg::rq(log(y) ~ x1 + x2, data=df_train, tau = c(qr_alpha/2, 1-qr_alpha/2))

		prediction_intervals <-
			tibble(pred = df_test$pred,
						 truth = df_test$y,
						 bin7 = df_test$bin6,
						 bin4 = df_test$bin4,
						 bin2 = df_test$bin2,
						 lower_bound = exp(predict(qr_mod,newdata=df_test)[,1]),
						 upper_bound = exp(predict(qr_mod,newdata=df_test)[,2])) %>%
			mutate(logwidth = log(upper_bound) - log(lower_bound),
						 width = upper_bound - lower_bound,
						 hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1,
						 								TRUE ~ 0),
						 type = 'Quantile Regression',
						 sim = j)

		return(prediction_intervals)


	}else{

	return(bind_rows(df_cal, df_test))

	}
}

run_single_sim <- function(dt, type, bins, dist, noncont,j, alpha = 0.1){
	# Load data
	calib <- dt %>% filter(partition == 'calib')
	test <- dt %>% filter(partition == 'test')

	if(type == 'conformal'){
		prediction_intervals <- pintervals::pinterval_conformal(
			pred = test$pred,
			calib = calib$pred,
			calib_truth = calib$truth,
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
				calib_truth = calib$truth,
				calib_bins = pull(calib[,paste0('bin',bins)]),
				contiguize =  contig,
				alpha = alpha
			)) %>%
			mutate(type = paste0('BCCP',noncont,' (',bins,')'))

	}else if(type == 'bootstrap'){
		prediction_intervals <- pintervals::pinterval_bootstrap(
			pred = test$pred,
			calib = calib$pred,
			calib_truth = calib$truth,
			lower_bound = 0,
			alpha = alpha
		) %>%
			mutate(type = 'Bootstrap')
	}else if(type == 'bootstrap_log'){
		prediction_intervals <- pintervals::pinterval_bootstrap(
			pred = log(test$pred),
			calib = log(calib$pred),
			calib_truth = log(calib$truth),
			lower_bound = 0,
			alpha = alpha
		) %>%
			mutate(upper_bound = exp(upper_bound),
						lower_bound = exp(lower_bound),
						type = 'Bootstrap (log)')

	}else if(type == 'parametric'){
		if(dist == 'lognormal'){
			prediction_intervals <- pintervals::pinterval_parametric(
				pred = test$pred,
				dist = 'lnorm',
				pars = list(meanlog = log(test$pred),
										sdlog = sqrt(mean((log(calib$pred)-log(calib$truth))^2))),
				alpha = alpha,
				lower_bound = 0
			) %>%
				mutate(type = 'Log-normal')
		}else if(dist == 'normal'){
			prediction_intervals <- pintervals::pinterval_parametric(
				pred = test$pred,
				dist = 'norm',
				pars = list(mean = test$pred,
										sd = sqrt(mean(calib$pred-calib$truth)^2)),
				alpha = alpha,
				lower_bound = 0
			) %>%
				mutate(type = 'Normal')
		}
	}

	if(is.na(noncont) || noncont == 'c'){

		prediction_intervals <- prediction_intervals	%>%
			mutate(truth = test$truth,
						 pred = test$pred,
						 bin7 = test$bin7,
						 bin4 = test$bin4,
						 bin2 = test$bin2,
						 logwidth = log(upper_bound) - log(lower_bound),
						 width = upper_bound - lower_bound,
						 hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1,
						 								TRUE ~ 0))

		return(prediction_intervals)
	}else{

		prediction_intervals <- prediction_intervals %>% mutate(truth = test$truth,
																														pred = test$pred,
																														bin7 = test$bin7,
																														bin4 = test$bin4,
																														bin2 = test$bin2,
																														sim = j)

		prediction_intervals2 <- 	foreach::foreach(i = 1:nrow(prediction_intervals),
																							 .final = bind_rows) %do%
			bind_cols(prediction_intervals[i,],
								tibble(width = sum(prediction_intervals[i,]$intervals[[1]]$upper_bound - prediction_intervals[i,]$intervals[[1]]$lower_bound),
											 logwidth = sum(log(prediction_intervals[i,]$intervals[[1]]$upper_bound) - log(prediction_intervals[i,]$intervals[[1]]$lower_bound)),
											 hit = max(prediction_intervals[i,]$truth >= prediction_intervals[i,]$intervals[[1]]$lower_bound & prediction_intervals[i,]$truth <= prediction_intervals[i,]$intervals[[1]]$upper_bound)))

		return(prediction_intervals2)

	}

}



