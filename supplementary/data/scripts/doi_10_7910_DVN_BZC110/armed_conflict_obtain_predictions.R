
cat("\n\n\n===================================\nRunning armed conflict replication\n===================================\n\nStarting at: ", paste(Sys.time()), "\n\n")


cat("Making predictions\n===================================\n\n")

library(dplyr)
library(ranger)
library(foreach)
library(doParallel)
library(doRNG)
library(pintervals)

## Total runtime using 5 cores parallel with 2 threads each (ranger default): ~2 hours on a Mac M2

#remotes::install_github("doktorandahl/pintervals")


## Function to obtain predictions in the calibration and test period for the armed conflict data, used later to compute the coverage and with of different prediction intervals

obtain_calib_pred <- function(j){
	### CP example VIEWS fatalities topics CM
	t <- Sys.time()
	# Load data, select features
	topics <- arrow::read_parquet('data/topics.parquet')
	topics2 <- topics %>% dplyr::filter(year>=2000, year<2023) %>% dplyr::select(1,69,68,4:67) %>% na.omit()


	train_ids <- sample(1:nrow(topics2),round(0.70*nrow(topics2)))

	calib_ids <- sample(setdiff(1:nrow(topics2),train_ids),round(0.20*nrow(topics2)))
	test_ids <- setdiff(setdiff(1:nrow(topics2),train_ids),calib_ids)

	# Divide into train, calib, test
	topics_train <- topics2[train_ids,]
	topics_calib <-  topics2[calib_ids,]
	topics_test <-  topics2[test_ids,]

	# Train rf
	f1 <- as.formula(paste0('log1p(ged_sb_target)~',paste(colnames(topics2)[-c(1:3)],collapse='+')))
	ranger1 <- invisible(ranger(f1,data=topics_train))

	calib <- tibble(pred = predict(ranger1,data=topics_calib)$p, truth = topics_calib$ged_sb_target,
									parition = "calib",
									ids = calib_ids,
									sim = j)

	test <- tibble(pred = predict(ranger1,data=topics_test)$p, truth = topics_test$ged_sb_target,
									parition = "test",
									ids = test_ids,
									sim = j)

	if(!dir.exists("predictions_ac")){
		dir.create("predictions_ac/")
	}

	file_name <- paste0("predictions_ac/ac_test_", j, ".rds")

	saveRDS(bind_rows(calib,test), file_name)

	cat("Simulation ", j, "of 1000 done", "in ", difftime(Sys.time(),t,unit="secs"), "\n")

}

obtain_calib_pred_timesplit <- function(){
	### CP example VIEWS fatalities topics CM
	t <- Sys.time()
	# Load data, select features
	topics <- arrow::read_parquet('data/topics.parquet')
	topics2 <- topics %>% dplyr::filter(year>=2000, year<2023) %>% dplyr::select(year,1,69,68,4:67) %>% na.omit()

	# Divide into train, calib, test
	topics_train <- topics2 %>% filter(year<2015) %>% select(-year)
	topics_calib <- topics2 %>% filter(year>=2015, year<2020) %>% select(-year)
	topics_test <-  topics2 %>% filter(year>=2020) %>% select(-year)

	# Train rf
	f1 <- as.formula(paste0('log1p(ged_sb_target)~',paste(colnames(topics2)[-c(1:3)],collapse='+')))
	ranger1 <- invisible(ranger(f1,data=topics_train))

	calib <- tibble(pred = predict(ranger1,data=topics_calib)$p, truth = topics_calib$ged_sb_target,
									parition = "calib")

	test <- tibble(pred = predict(ranger1,data=topics_test)$p, truth = topics_test$ged_sb_target,
								 parition = "test")

	file_name <- paste0("data/predictions_timesplit.rds")

	saveRDS(bind_rows(calib,test), file_name)
}

registerDoParallel(cores=round(parallel::detectCores()/2)-1)

# Looping to prevent memory buildup by clearing out the R subprocesses regularly (possible bug in foreach?)

for(r in 1:10){


foreach(j=((r-1)*100+1):(r*100), .options.RNG = 20250216+r) %dorng% obtain_calib_pred(j)

}

obtain_calib_pred_timesplit()

cat("\n\n\n===================================\nFinished predictions at: ", paste(Sys.time()), "\n")
