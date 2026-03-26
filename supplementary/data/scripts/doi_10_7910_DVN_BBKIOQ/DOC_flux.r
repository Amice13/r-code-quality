
#R
#chamber analysis guatavita
#from 2021 to 2024

library(ggplot2)

std <- function(x) sd(x)/sqrt(length(x))


pcdir<-"C:/Users/jubenavides/Dropbox/papers/Guatavita fluxes/stats"
macdir<-"/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Guatavita fluxes/stats"

setwd(macdir)



# Load data
doc_data <- read.csv("DOC transport.csv")

# Convert the date column to Date format and extract year
doc_data$date <- as.Date(doc_data$date, format = "%d/%m/%y")
doc_data$Year <- format(doc_data$date, "%Y")

# Constants
seconds_per_year <- 365 * 24 * 3600        # seconds in a year
mg_C_to_g <- 1e-3                          # mg to g
g_C_to_g_CO2 <- 44 / 12                    # C to CO2 equivalent
area_m2 <- 10000                           # 1 hectare in m²

# Calculate carbon discharge in g/year
doc_data$g_C_discharge_y <- doc_data$Q * doc_data$TOC.mg.C.L.dif * seconds_per_year * mg_C_to_g

# Convert to g CO2 m-2 y-1
doc_data$g_CO2_m2_y <- doc_data$g_C_discharge_y * g_C_to_g_CO2 / area_m2

# Summarize mean per Site, Year, and Season
summary_DOC <- aggregate(g_CO2_m2_y ~ Site + Year + Season, data = doc_data, FUN = mean)
summary_DOCsd <- aggregate(g_CO2_m2_y ~ Site + Year + Season, data = doc_data, FUN = sd)

summary_DOC$DOCsd<-summary_DOCsd$g_CO2_m2_y

# View result
print(summary_DOC)

write.csv(summary_DOC,"summary_DOC.csv")


