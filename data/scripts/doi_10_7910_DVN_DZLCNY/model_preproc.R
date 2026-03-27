# Aim: preprocess the data for the modeling process

library(purrr)
library(dplyr)

# Read in the data
d <- read.csv(paste0('results/featextract/terror_feature_extraction',mm,'.csv')) %>%
  dplyr::filter(!is.na(country))

# Remove unused variables
d$country <- d$iso_a3
d$iso_a3 <- NULL
d$regnb <- NULL
d$continent <- NULL
d$col <- d$row <- NULL
d$gid <- d$admin <- NULL
d$myregion <- NULL
d$pop <- NULL
# to check correlation
# library(corrplot)
# myvar <- c(6,8,9,12:18,20:23)
# corvar <- d[,myvar]
# corvar <- corvar[complete.cases(corvar),]
# mycor <-cor(corvar)
# corrplot(mycor,method=c("number"))

# Create matrix of predictors.
d <- d %>%
  dplyr::mutate(terror = 1L * (terror > 0))

# Convert country to a dummy variable
d$country <- as.factor(d$country)

# remove first year (NAs in lagged variables)
d <- d %>%
  dplyr:: filter(year != min(year)) %>%
  dplyr::mutate(.row_ind = row_number())

# save data for plots
idvar<-c("x","y","id","country")
voronid<-d[idvar]
voronid<-voronid[!duplicated(voronid), ]
write.csv(voronid,file=paste0("results/voronoi/voronid",mm,".csv"),row.names=FALSE)
rm(voronid)

# create temporal cross-validation indices
# test indices for year 2013 trough 2016 (year 2017 will remain untouched)
ind_test <- purrr::map(2012:2016, ~ dplyr::filter(d, year == .x)$.row_ind)
# train indices
# first 9 years always used for training
ind_train <-  purrr::map(2012:2016, ~ dplyr::filter(d, year < .x)$.row_ind)
# list object for caret::train
temporal_csv <- list(train = ind_train, test = ind_test)

## indices for ML algorithms
eval_years <- 2012:2016
setup_cv <- purrr::map(seq_along(eval_years),
                function(z) {
                  list(
                    outer_train = purrr::map(
                      (2008 + (z-1)):(eval_years[z]-1),
                      ~ list(
                        train =  dplyr::filter(d, year < .x)$.row_ind,
                        test = dplyr::filter(d, year == .x)$.row_ind)),
                    outer_test = purrr::map(z, ~ dplyr::filter(d, year == eval_years[.x])$.row_ind))
                })

## create model matrix + outcome for ML methods
X <- d
X_country <- model.matrix(~country, X)[, -1]
myreg <- X %>%
  dplyr::select(-one_of(c("id", ".row_ind", "year", "week", "week_year", "country",
                          "terror"))) %>%
  as.matrix()
myreg <- cbind(X_country, myreg)
# outcome as factor
y <- X %>%
  dplyr::mutate(terror = factor(terror > 0, levels = c(FALSE, TRUE), labels = c("no", "yes"))) %>%
  dplyr::pull(terror)

stopifnot(nrow(myreg) == length(y))


## Define custom summary class to use Brier Score for evaluation

brierSummary <- function (data, lev = NULL, model = NULL) { # for training on a next-period return
  #browser() #essential for debugging
  dat = dim(data)
  
  # get observed dummy
  Y_obs = as.numeric(model.matrix( ~ data[, "obs"] - 1)) # create dummy - for each level of the outcome
  # get predicted probabilities
  Y_pred = data[["yes"]]
  
  brier =  sum((Y_obs - Y_pred)^2)/length(Y_obs)
  
  names(brier) = "brier"
  
  return(brier)
  
}
#END