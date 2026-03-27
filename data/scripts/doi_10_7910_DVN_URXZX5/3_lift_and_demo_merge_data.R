# Merge LiFt data with Demographics data

library(plyr)
library(reshape2)
library(data.table)

save_data_file <- "data/Lift_And_Demo_Data_All"

load("data/Demographics_Data_Clean.Rda")
demographic_data <- data

load("data/Lift_Data_Clean.Rda")
lift_data <- data

# remove duplicate UUIDs
lift_data <- lift_data[!duplicated(lift_data[ , 7:9 ]),]

# lift data from long to wide format
lift_data_dcast <- dcast(setDT(lift_data), UUID ~ scenario, value.var=c("x","t","A","choice","y","x0","experiment"))

# join lift and demographic 
joined_data <- join(demographic_data,lift_data_dcast , by='UUID', type='left', match='first')

# Save DataFrame
save(joined_data,file = paste0(save_data_file,".Rda"))

# Save CSV 
write.csv(joined_data, file = paste0(save_data_file,".csv"))

# remove superfluous dataframes
rm(data,lift_data,lift_data_dcast,demographic_data,save_data_file)