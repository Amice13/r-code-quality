cat("\n\n\n===================================\nAppending quantile intervals\n\n")
library(foreach)
library(doParallel)
library(ranger)
library(tidyverse)
library(doRNG)

make_quantile_intervals <- function(j, alpha = 0.1,append = TRUE, timesplit = F){
	### CP example VIEWS fatalities topics CM
	t <- Sys.time()
	# Load data, select features

	if(!timesplit){
	topics <- arrow::read_parquet('data/topics.parquet')
	topics2 <- topics %>% dplyr::filter(year>=2000, year<2023) %>% dplyr::select(1,69,68,4:67) %>% na.omit()


	train_ids <- sample(1:nrow(topics2),round(0.70*nrow(topics2)))

	calib_ids <- sample(setdiff(1:nrow(topics2),train_ids),round(0.20*nrow(topics2)))
	test_ids <- setdiff(setdiff(1:nrow(topics2),train_ids),calib_ids)

	# Divide into train, calib, test
	topics_train <- topics2[train_ids,]
	topics_calib <-  topics2[calib_ids,]
	topics_test <-  topics2[test_ids,]
	}else{
		topics <- arrow::read_parquet('data/topics.parquet')
		topics2 <- topics %>% dplyr::filter(year>=2000, year<2023) %>% dplyr::select(year,1,69,68,4:67) %>% na.omit()


		# Divide into train, calib, test
		topics_train <- topics2 %>% filter(year<2015) %>% select(-year)
		topics_calib <- topics2 %>% filter(year>=2015, year<2020) %>% select(-year)
		topics_test <-  topics2 %>% filter(year>=2020) %>% select(-year)

}
	# Train rf
	f1 <- as.formula(paste0('log1p(ged_sb_target)~',paste(colnames(topics2)[-c(1:3)],collapse='+')))
	ranger1 <- invisible(ranger(f1,data=topics_train, quantreg = TRUE))

	quants <- predict(ranger1,data=topics_test, type = "quantiles", quantiles = c(alpha/2,1-alpha/2))


	test <- tibble(pred = predict(ranger1,data=topics_test)$p, truth = topics_test$ged_sb_target,
								 parition = "test") %>%
		mutate(bin7 = case_when(truth == 0 ~ 0,
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

	prediction_intervals <- tibble(pred = test$pred,
																 lower_bound = quants$p[,1],
																 upper_bound = quants$p[,2],
																 type = 'Quantreg',
																 truth = test$truth,
																 bin7 = test$bin7,
																 bin4 = test$bin4,
																 bin2 = test$bin2) %>% mutate(
																 	logwidth = upper_bound - lower_bound,
																 	lower_bound = round(expm1(lower_bound)),
																 	upper_bound = round(expm1(upper_bound)),
																 	width = upper_bound - lower_bound,
																 	hit = case_when(truth >= lower_bound & truth <= upper_bound ~ 1,
																 									TRUE ~ 0))

	if(timesplit){
		tmp_intervals <- readRDS(paste0('data/timesplit_intervals.rds')) %>% filter(type != 'Quantreg')
		saveRDS(bind_rows(tmp_intervals,prediction_intervals), paste0('data/timesplit_intervals.rds'))
	}else if(!append){

		if(!dir.exists('quantile_intervals')) dir.create('quantile_intervals')

	saveRDS(prediction_intervals, paste0('quantile_intervals/intervals_',j,'.rds'))
} else {
	tmp_intervals <- readRDS(paste0('intervals_ac/intervals_',j,'.rds')) %>% filter(type != 'Quantreg')

	saveRDS(bind_rows(tmp_intervals,prediction_intervals), paste0('intervals_ac/intervals_',j,'.rds'))



}
	cat("Simulation ", j, " done", "in ", difftime(Sys.time(),t,unit="secs"), "\n")
}

registerDoParallel(cores=round(parallel::detectCores()/2)-1)

# Looping to prevent memory buildup by clearing out the R subprocesses regularly (possible bug in foreach?)

for(r in 1:10){


foreach(j=((r-1)*100+1):(r*100), .options.RNG = 20250216+r) %dorng% make_quantile_intervals(j)

}

make_quantile_intervals(1001, timesplit = T)

