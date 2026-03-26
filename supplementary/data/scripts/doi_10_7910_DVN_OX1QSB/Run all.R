source("0.1. Packages.R")
source("0.2. Preliminary definitions.R")
if (frequency == 12){source("1.1. Data preparation - monthly.R")}
if (frequency == 4) {source("1.1. Data preparation - quarterly.R")}
source("2.1. Combination forecasts.R")
source("2.2. DMSPE forecasts.R")
source("2.3. C-ENET forecasts.R")
source("2.4. KS forecasts.R")
source("2.5. ENET forecasts.R")
source("2.6. PCR forecasts.R")
source("2.7. 3PRF forecasts.R")
source("2.8. 3PRFm forecasts.R")
source("3.1. Allocation.R")
source("4.1. Data and Definitions.R")
source("4.2. Execute forecasts.R")
source("5.1. Output preparation.R")
source("5.2. Output.R")

if (frequency == 4 & predictors == "welch.goyal.quarterly" & start.year.is == 1947) {source("5.3. Plots.R")}
if (frequency == 12 & predictors == "welch.goyal.monthly" & start.year.is == 1947) {source("5.3. Plots monthly.R")}
if (frequency == 12 & predictors == "welch.goyal.monthly" & start.year.is == 1927) {source("5.3. Plots monthly1927.R")}
if (frequency == 12 & predictors == "li.tsiakas" & start.year.is == 1947) {source("5.3. Plots litsiakas.R")}
if (frequency == 12 & predictors == "rapach.zhou" & start.year.is == 1947) {source("5.3. Plots rapachzhou.R")}
