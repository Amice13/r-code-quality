# 1) KS forecasts

assign(stringr::str_c(freq.component,start.year.is,"kitchensink"),
  kitchensink(forecast.data$premium,
              predictor.set.ks,
              start = start.oos,
              end = end.oos,
              freq = frequency,
              h = horizon,
              train = training.years,
              k = variable.count.ks,
              CT = FALSE))

assign(stringr::str_c(freq.component,start.year.is,"util.kitchensink"),
       kitchensink(forecast.data$simple_premium,
              predictor.set.ks,
              start = start.oos,
              end = end.oos,
              freq = frequency,
              h = horizon,
              train = training.years,
              k = variable.count.ks,
              CT = FALSE))

assign(stringr::str_c(freq.component,start.year.is,"ct.kitchensink"),
       kitchensink(forecast.data$premium,
              predictor.set.ks,
              start = start.oos,
              end = end.oos,
              freq = frequency,
              h = horizon,
              train = training.years,
              k = variable.count.ks,
              CT = TRUE))

assign(stringr::str_c(freq.component,start.year.is,"ct.util.kitchensink"),
       kitchensink(forecast.data$simple_premium,
              predictor.set.ks,
              start = start.oos,
              end = end.oos,
              freq = frequency,
              h = horizon,
              train = training.years,
              k = variable.count.ks,
              CT = TRUE))
print("KS forecasts: finished")

# Combination forecasts ------------------
assign(stringr::str_c(freq.component,start.year.is,"combination"),
       combination.forecast(
    erp = forecast.data$premium,
    predictor = predictor.set,
    start = start.oos,
    end = end.oos,
    train = training.years,
    k = variable.count,
    freq = frequency,
    h = horizon,
    CT = FALSE
  ))

assign(stringr::str_c(freq.component,start.year.is,"util.combination"),
       combination.forecast(
    erp = forecast.data$simple_premium,
    predictor = predictor.set,
    start = start.oos,
    end = end.oos,
    train = training.years,
    k = variable.count,
    freq = frequency,
    h = horizon,
    CT = FALSE
  ))

assign(stringr::str_c(freq.component,start.year.is,"ct.combination"),
       combination.forecast(
    erp = forecast.data$premium,
    predictor = predictor.set,
    start = start.oos,
    end = end.oos,
    train = training.years,
    k = variable.count,
    freq = frequency,
    h = horizon,
    CT = TRUE
  ))

assign(stringr::str_c(freq.component,start.year.is,"ct.util.combination"),
       combination.forecast(
    erp = forecast.data$simple_premium,
    predictor = predictor.set,
    start = start.oos,
    end = end.oos,
    train = training.years,
    k = variable.count,
    freq = frequency,
    h = horizon,
    CT = TRUE
  ))

print("Combination forecasts: finished")
##DMSPE forecasts ---------------------------------

assign(stringr::str_c(freq.component,start.year.is,"dmspe"),
       dmspe.forecast(
    forecast.data$premium,
    predictor.set,
    start = start.oos,
    end = end.oos,
    train = training.years,
    k = variable.count,
    freq = frequency,
    h = horizon,
    CT = FALSE
  ))

assign(stringr::str_c(freq.component,start.year.is,"util.dmspe"),
       dmspe.forecast(
    forecast.data$simple_premium,
    predictor.set,
    start = start.oos,
    end = end.oos,
    train = training.years,
    k = variable.count,
    freq = frequency,
    h = horizon,
    CT = FALSE
  ))

assign(stringr::str_c(freq.component,start.year.is,"ct.dmspe"),
       dmspe.forecast(
    forecast.data$premium,
    predictor.set,
    start = start.oos,
    end = end.oos,
    train = training.years,
    k = variable.count,
    freq = frequency,
    h = horizon,
    CT = TRUE
  ))

assign(stringr::str_c(freq.component,start.year.is,"ct.util.dmspe"),
       dmspe.forecast(
    forecast.data$simple_premium,
    predictor.set,
    start = start.oos,
    end = end.oos,
    train = training.years,
    k = variable.count,
    freq = frequency,
    h = horizon,
    CT = TRUE
  ))
print("DMSPE forecasts: finished")

# C-ENET forecasts

assign(stringr::str_c(freq.component,start.year.is,"cenet"),
       cenet.forecast(erp = forecast.data$premium,
                 predictor.set,
                 start = start.oos,
                 end = end.oos,
                 k =variable.count,
                 train = training.years,
                 freq = frequency,
                 h = horizon,
                 CT = FALSE))

assign(stringr::str_c(freq.component,start.year.is,"util.cenet"),
       cenet.forecast(erp = forecast.data$simple_premium,
                 predictor.set,
                 start = start.oos,
                 end = end.oos,
                 k =variable.count,
                 train = training.years,
                 freq = frequency,
                 h = horizon,
                 CT = FALSE))

assign(stringr::str_c(freq.component,start.year.is,"ct.cenet"),
       cenet.forecast(erp = forecast.data$premium,
                 predictor.set,
                 start = start.oos,
                 end = end.oos,
                 k =variable.count,
                 train = training.years,
                 freq = frequency,
                 h = horizon,
                 CT = TRUE))

assign(stringr::str_c(freq.component,start.year.is,"ct.util.cenet"),
       cenet.forecast(erp = forecast.data$simple_premium,
                 predictor.set,
                 start = start.oos,
                 end = end.oos,
                 k =variable.count,
                 train = training.years,
                 freq = frequency,
                 h = horizon,
                 CT = TRUE))

print("C-ENET forecasts: finished")

#E-NET, Ridge, LASSO

for (v in 1:3){
  
  assign(stringr::str_c(freq.component,start.year.is, method[v]),
         enet.forecast(
           erp= forecast.data$premium,
           predictor = predictor.set,
           alpha = vec[v],
           start = start.oos,
           end=end.oos,
           train=training.years,
           k=variable.count,
           freq=frequency,
           h = horizon,
           CT=FALSE))
  
}


for (v in 1:3){
  
  assign(stringr::str_c(freq.component,start.year.is, "util.",method[v]),
         enet.forecast(
           erp= forecast.data$simple_premium,
           predictor = predictor.set,
           alpha = vec[v],
           start = start.oos,
           end=end.oos,
           train=training.years,
           k=variable.count,
           freq=frequency,
           h = horizon,
           CT=FALSE))
  
}


# CT restrictions

for (v in 1:3){
  
  assign(stringr::str_c(freq.component,start.year.is, "ct.",method[v]),
         enet.forecast(
           erp= forecast.data$premium,
           predictor = predictor.set,
           alpha = vec[v],
           start = start.oos,
           end=end.oos,
           train=training.years,
           k=variable.count,
           freq=frequency,
           h = horizon,
           CT=TRUE))
  
}


for (v in 1:3){
  
  assign(stringr::str_c(freq.component,start.year.is, "ct.util.",method[v]),
         enet.forecast(
           erp= forecast.data$simple_premium,
           predictor = predictor.set,
           alpha = vec[v],
           start = start.oos,
           end=end.oos,
           train=training.years,
           k=variable.count,
           freq=frequency,
           h = horizon,
           CT=TRUE))
  
}
print("E-NET forecasts: finished")

## PCR forecasts --------------------------------------------

assign(stringr::str_c(freq.component,start.year.is,"pcr"), 
       pcr.forecast(forecast.data$premium,
               predictor.set,
               start = start.oos,
               end = end.oos,
               freq = frequency,
               h = horizon,
               train = training.years,
               CT = FALSE))

assign(stringr::str_c(freq.component,start.year.is,"util.pcr"), 
       pcr.forecast(forecast.data$simple_premium,
               predictor.set,
               start = start.oos,
               end = end.oos,
               freq = frequency,
               h = horizon,
               train = training.years,
               CT = FALSE))

assign(stringr::str_c(freq.component,start.year.is,"ct.pcr"), 
       pcr.forecast(forecast.data$premium,
               predictor.set,
               start = start.oos,
               end = end.oos,
               freq = frequency,
               h = horizon,
               train = training.years,
               CT = TRUE))

assign(stringr::str_c(freq.component,start.year.is,"ct.util.pcr"), 
       pcr.forecast(forecast.data$simple_premium,
               predictor.set,
               start = start.oos,
               end = end.oos,
               freq = frequency,
               h = horizon,
               train = training.years,
               CT = TRUE))
print("PCR forecasts: finished")

# 3PRF forecasts --------------------------------------------

assign(stringr::str_c(freq.component,start.year.is,"threepass"), 
       threepass.forecast(forecast.data$premium,
                     predictor.set,
                     start = start.oos,
                     end = end.oos,
                     freq = frequency,
                     h = horizon,
                     train = training.years,
                     k = variable.count,
                     CT = FALSE))

assign(stringr::str_c(freq.component,start.year.is,"util.threepass"), 
       threepass.forecast(forecast.data$simple_premium,
                     predictor.set,
                     start = start.oos,
                     end = end.oos,
                     freq = frequency,
                     h = horizon,
                     train = training.years,
                     k = variable.count,
                     CT = FALSE))

assign(stringr::str_c(freq.component,start.year.is,"ct.threepass"), 
       threepass.forecast(forecast.data$premium,
                     predictor.set,
                     start = start.oos,
                     end = end.oos,
                     freq = frequency,
                     h = horizon,
                     train = training.years,
                     k = variable.count,
                     CT = TRUE))

assign(stringr::str_c(freq.component,start.year.is,"ct.util.threepass"), 
       threepass.forecast(forecast.data$simple_premium,
                     predictor.set,
                     start = start.oos,
                     end = end.oos,
                     freq = frequency,
                     h = horizon,
                     train = training.years,
                     k = variable.count,
                     CT = TRUE))
print("3PRF forecasts: finished")

# 3PRFm forecasts ----------------------

assign(stringr::str_c(freq.component,start.year.is,"3prfm"), 
       modified.threepass(forecast.data$premium,
                     predictor.set,
                     start = start.oos,
                     end = end.oos,
                     freq = frequency,
                     h = horizon,
                     train = training.years,
                     k = variable.count,
                     CT=FALSE))

assign(stringr::str_c(freq.component,start.year.is,"util.3prfm"), 
       modified.threepass(forecast.data$simple_premium,
                     predictor.set,
                     start = start.oos,
                     end = end.oos,
                     freq = frequency,
                     h = horizon,
                     train = training.years,
                     k = variable.count,
                     CT=FALSE))

assign(stringr::str_c(freq.component,start.year.is,"ct.3prfm"), 
       modified.threepass(forecast.data$premium,
                     predictor.set,
                     start = start.oos,
                     end = end.oos,
                     freq = frequency,
                     h = horizon,
                     train = training.years,
                     k = variable.count,
                     CT=TRUE))

assign(stringr::str_c(freq.component,start.year.is,"ct.util.3prfm"), 
       modified.threepass(forecast.data$simple_premium,
                     predictor.set,
                     start = start.oos,
                     end = end.oos,
                     freq = frequency,
                     h = horizon,
                     train = training.years,
                     k = variable.count,
                     CT=TRUE))
print("3PRFm forecasts: finished")
