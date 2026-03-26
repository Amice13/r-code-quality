## create discretized study area and prepare data
source("subcode/data_and_map.R", echo = TRUE,local = TRUE)
# feature engineering
source("subcode/create_features.R", echo = TRUE,local = TRUE)

# ##run individual models
# ## preprocessing
source("subcode/model_preproc.R", echo = TRUE,local = TRUE)
##GBT (via xgboost)
source("subcode/xgb.R", echo = TRUE,local = TRUE)
