###  Code to run Fama-French version of model
###  for Christensen and OHara 


## Import Fama French Data 


FF_Data <- read_csv("../ImportableData/FamaFrenchImport.csv")


##  Cut it to the same dates as the Returns data
FF_Estimation <- FF_Data %>%
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"),
         MFRF = Mkt-RF) %>%
  filter(Date >= Dates[1] & Date <= Dates[length(Dates)])
  


###
