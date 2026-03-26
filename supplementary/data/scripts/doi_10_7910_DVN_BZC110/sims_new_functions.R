
library(tidyverse)
library(pintervals)
library(foreach)

sim_simple_lnorm <- function(n = 10000, train_p = 0.5, calib_p = 0.25, nbins = 4, sd_log = 1){
	x1 <- runif(n)
	x2 <- runif(n)

	y <- rlnorm(n, meanlog = x1 + x2, sdlog = sd_log)
	bin <- cut(y, breaks = c(0,quantile(y[1:round((train_p+calib_p)*n)], probs = seq(0, 1, 1/nbins))[2:nbins],Inf), include.lowest = T, labels =F)
	#bin <- ifelse(y < quantile(y, 0.75), 0, 1)
	bin2 <- cut(y, breaks = c(0,quantile(y[1:round((train_p+calib_p)*n)], probs = seq(0, 1, 1/2))[2],Inf), include.lowest = T, labels =F)
	bin6 <- cut(y, breaks = c(0,quantile(y[1:round((train_p+calib_p)*n)], probs = seq(0, 1, 1/6))[2:6],Inf), include.lowest = T, labels =F)

	df <- tibble(x1, x2, y, bin, bin2, bin6)
	df_train <- df %>% slice(1:(train_p*n))
	df_cal <- df %>% slice((train_p*n+1):(train_p*n + calib_p*n))
	df_test <- df %>% slice((train_p*n + calib_p*n + 1):n)

	mod <- lm(log(y) ~ x1 + x2, data=df_train)

	calib <- exp(predict(mod, newdata=df_cal))
	calib_truth <- df_cal$y
	calib_bins <- df_cal$bin
	calib_bins2 <- df_cal$bin2
	calib_bins6 <- df_cal$bin6

	pred_test <- exp(predict(mod, newdata=df_test))


	scp <- pintervals::pinterval_conformal(pred_test, calib, calib_truth, ncs_function = 'absolute_error', grid_size = 10000)

	scp <- scp %>%
		mutate(truth = df_test$y,
					 bin = df_test$bin,
					 hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1, T~0),
					 width = upper_bound - lower_bound,
					 type = 'SCP')

	bccpc_6 <- pintervals::pinterval_bccp(pred_test, calib, calib_truth, calib_bins6,  ncs_function = 'absolute_error', grid_size = 10000,
																					 contiguize = TRUE)

	bccpc_6 <- bccpc_6 %>%
		mutate(truth = df_test$y,
					 bin = df_test$bin,
					 hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1, T~0),
					 width = upper_bound - lower_bound,
					 type = 'BCCPc',
					 bins = 6)

	bccpc_4 <- pintervals::pinterval_bccp(pred_test, calib, calib_truth, calib_bins,  ncs_function = 'absolute_error', grid_size = 10000,
																				contiguize = TRUE)

	bccpc_4 <- bccpc_4 %>%
		mutate(truth = df_test$y,
					 bin = df_test$bin,
					 hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1, T~0),
					 width = upper_bound - lower_bound,
					 type = 'BCCPc',
					 bins = 4)

	bccpc_2 <- pintervals::pinterval_bccp(pred_test, calib, calib_truth, calib_bins2,  ncs_function = 'absolute_error', grid_size = 10000,
																				contiguize = TRUE)

	bccpc_2 <- bccpc_2 %>%
		mutate(truth = df_test$y,
					 bin = df_test$bin,
					 hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1, T~0),
					 width = upper_bound - lower_bound,
					 type = 'BCCPc',
					 bins = 2)

	bccpd_6 <- pintervals::pinterval_bccp(pred_test, calib, calib_truth, calib_bins6,  ncs_function = 'absolute_error', grid_size = 10000,
																				contiguize = FALSE)

	bccpd_6 <- bccpd_6 %>%
		mutate(truth = df_test$y,
					 bin = df_test$bin)

	bccpd_6 <- foreach::foreach(i = 1:nrow(bccpd_6),.final = bind_rows) %do%
		bind_cols(bccpd_6[i,],
							tibble(width = sum(bccpd_6[i,]$intervals[[1]]$upper_bound - bccpd_6[i,]$intervals[[1]]$lower_bound),
										 hit = max(bccpd_6[i,]$truth >= bccpd_6[i,]$intervals[[1]]$lower_bound & bccpd_6[i,]$truth <= bccpd_6[i,]$intervals[[1]]$upper_bound)),
							type = 'BCCPd',
							bins = 6)

	bccpd_4 <- pintervals::pinterval_bccp(pred_test, calib, calib_truth, calib_bins,  ncs_function = 'absolute_error', grid_size = 10000,
																				contiguize = FALSE)

	bccpd_4 <- bccpd_4 %>%
		mutate(truth = df_test$y,
					 bin = df_test$bin)

	bccpd_4 <- foreach::foreach(i = 1:nrow(bccpd_4),.final = bind_rows) %do%
		bind_cols(bccpd_4[i,],
							tibble(width = sum(bccpd_4[i,]$intervals[[1]]$upper_bound - bccpd_4[i,]$intervals[[1]]$lower_bound),
										 hit = max(bccpd_4[i,]$truth >= bccpd_4[i,]$intervals[[1]]$lower_bound & bccpd_4[i,]$truth <= bccpd_4[i,]$intervals[[1]]$upper_bound)),
							type = 'BCCPd',
							bins = 4)

	bccpd_2 <- pintervals::pinterval_bccp(pred_test, calib, calib_truth, calib_bins2,  ncs_function = 'absolute_error', grid_size = 10000,
																				contiguize = FALSE)

	bccpd_2 <- bccpd_2 %>%
		mutate(truth = df_test$y,
					 bin = df_test$bin)

	bccpd_2 <- foreach::foreach(i = 1:nrow(bccpd_2),.final = bind_rows) %do%
		bind_cols(bccpd_2[i,],
							tibble(width = sum(bccpd_2[i,]$intervals[[1]]$upper_bound - bccpd_2[i,]$intervals[[1]]$lower_bound),
										 hit = max(bccpd_2[i,]$truth >= bccpd_2[i,]$intervals[[1]]$lower_bound & bccpd_2[i,]$truth <= bccpd_2[i,]$intervals[[1]]$upper_bound)),
							type = 'BCCPd',
							bins = 2)


	all_intervals <- bind_rows(scp,bccpc_2,bccpc_4,bccpc_6,bccpd_2,bccpd_4,bccpd_6)

	return(all_intervals)

}

summarize_sims <- function(sims, bins=F,modid = NULL,i=NULL){
	if(!bins){
		return(sims %>% group_by(type) %>%
					 	summarize(hitrate = mean(hit, na.rm = T),
					 						width_mean = mean(width, na.rm = T),
					 						width_median = median(width, na.rm=T),
					 						.groups = 'drop') %>%
					 	mutate(model = modid,
					 				 i = i))
	} else {
		sum1 <-  sims %>% group_by(type, bin, bins) %>%
			summarize(hitrate = mean(hit, na.rm = T),
								width_mean = mean(width, na.rm = T),
								width_median = median(width, na.rm = T),
								.groups = 'drop') %>%
			ungroup() %>%
			#mutate(model = modid) %>%
			pivot_wider(names_from = bin, values_from = c(hitrate, width_mean, width_median))

		sum2 <- sims %>% group_by(type,bins) %>%
			summarize(hitrate = mean(hit, na.rm = T),
								width_mean = mean(width, na.rm = T),
								width_median = median(width, na.rm = T),
								.groups = 'drop') %>%
			mutate(i = i)

		return(left_join(sum2, sum1))

	}


}
