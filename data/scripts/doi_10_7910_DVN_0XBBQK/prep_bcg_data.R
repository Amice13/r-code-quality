library(readr)
library(reshape2)

### Reformats BCG vaccination coverage data

# import WHO BCG vaccination data
bcg <- read_csv("bcg_data_2021.csv")

# reshape data
bcg_melt <- melt(bcg, id=c("iso3", "country"))

# rename columns
colnames(bcg_melt)[3:4] <- c("year", "bcg")

# convert vaccination rates to percentages
bcg_melt$bcg <- bcg_melt$bcg / 100

# cast year column as numeric
bcg_melt$year <- as.numeric(as.character(bcg_melt$year))

# save data file
saveRDS(bcg_melt, file="bcg_data_melt.rds")
