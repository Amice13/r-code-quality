library(rio)
library(haven)
library(readr)
library(timeDate)
library(timeSeries)
library(stargazer)
library(fUnitRoots)

getOption("scipen")
opt <- options("scipen" = 20) # changes notation of y axis to absolutes instead of scientific notation
getOption("scipen")

####################################################
##### Test of Dataset's Mathematical Validity ######
####################################################

# Prior to running analysis, comprehensive test of internal mathematical validity of entire 
# dataset should be undertaken
# The mathematical Validity (mathval) tests are coded in scripts/X_mathval_internal_validity_test.r

# Refer to Thesis Paper Figure X [add correct number later] for graphical representation of variable structure

####################################################

# Test following statements for all Groups x = A,B,C,D to ensure mathematic validity ##
#
#
# 1) Total Traffic x_TotTraffic should equal x_PaxTerminalTot + x_PaxTransit:
#         x_TotTraffic = x_PaxTerminalTot + x_PaxTransit
#
# 2) Total Terminal Traffic should equal sum of total scheduled and other Commercial Traffic: 
#         x_PaxTerminalTot = x_PAXScheduledTot + x_OtherCommercTraffic
#
# 3) Total Scheduled Traffic should equal sum of destinations: 
#         x_PAXScheduledTot = x_PAXScheduledDE + x_PAXScheduledEurope + x_PAXScheduled NonEurope
#
# If all three statements hold across all observations and all variables, then dataset is mathematically valid!

options(tibble.print_max = 60)

source("Scripts/stepbystep/00_mathval/A_mathval_internal_validity_tests.R", echo = TRUE)
source("Scripts/stepbystep/00_mathval/B_mathval_internal_validity_tests.R", echo = TRUE)
source("Scripts/stepbystep/00_mathval/C_mathval_internal_validity_tests.R", echo = TRUE)
source("Scripts/stepbystep/00_mathval/D_mathval_internal_validity_tests.R", echo = TRUE)

# Prerequisite Validity Test: Statements 1-3 must result in TRUE for all observations i = [1:60] 
# and must be tested on each Variable X = A,B,C,D
# If above prerequisite holds, dataset is internally mathematically valid, and no errors were 
# caused during data import and transformation
#
# ANALYSIS MAY CONTINUE ONLY IF PREREQUISITE IS MET

rm(advtrafficde,trafficDE_simplified)

####################################################
#### Conclusion of Mathematical Validity Tests  ####
####################################################

####################################################
###### Importing Data & creating Time Series  ######
####################################################


advtrafficde <- read_csv("data/import/advtrafficde.csv")

class(advtrafficde)
class(advtrafficde$Date)
summary(advtrafficde$Date)

# numeric var i =[1:60] number all observation months sequentially
advtrafficde$month <- seq(from = 1, to = 60, by = 1) 

class(advtrafficde$month)

# Check proper atrribution of month numeration
print(advtrafficde[1:60,c("month","Date")])

## Turn Variable Date into Date-Format ##
advtrafficde$tsDate <- seq.Date(as.Date("2016-01-01"), as.Date("2020-12-01"), by = "month")


## Reduce Number of Variables by removing  "Schengen" & "EU" across all groups A-D
## Analysis will only focus on domestic (DE), Continental (Europe), and intercont (non-Europe) Traffic
## Following Data-Frame therefore omits EU and Schengen
trafficDE_simplified <- advtrafficde[, c("month", "tsDate","A_PaxTerminalTot","A_PAXScheduledTot","A_PAXScheduledDE",
                                 "A_PAXScheduledEurope","A_PAXScheduled NonEurope","A_OtherCommercTraffic","A_PAXTransit",
                                 "A_TotTraffic","B_PaxTerminalTot","B_PAXScheduledTot","B_PAXScheduledDE","B_PAXScheduledEurope",
                                 "B_PAXScheduled NonEurope","B_OtherCommercTraffic","B_PAXTransit","B_TotTraffic",
                                 "C_PaxTerminalTot","C_PAXScheduledTot","C_PAXScheduledDE","C_PAXScheduledEurope","C_PAXScheduled NonEurope",
                                 "C_OtherCommercTraffic","C_PAXTransit","C_TotTraffic","D_PaxTerminalTot",
                                 "D_PAXScheduledTot","D_PAXScheduledDE","D_PAXScheduledEurope","D_PAXScheduled NonEurope",
                                 "D_OtherCommercTraffic","D_PAXTransit","D_TotTraffic")]

# Remove any unnecessary Objects from Workspace
rm(advtrafficde, opt)


####################################################
#### Preparing Time Series Variables for ARIMA #####
####################################################

# (1) First create timeseries for all 8 Variable types and across all 4 Groups!
source("Scripts/stepbystep/01_timeseriesconstruct/01_timeseriesconstruct.R", echo = TRUE)

# (2) Second, Run descriptive Statistics on all variables to understand TS Structure better (note the plots!)
source("Scripts/stepbystep/02_descriptivestats/x_TotTraffic_descriptstats.R", echo = TRUE) #1
source("Scripts/stepbystep/02_descriptivestats/x_PaxTransit_descriptstats.R", echo = TRUE) #2
source("Scripts/stepbystep/02_descriptivestats/x_OtherCommercTraffic_descriptstats.R", echo = TRUE) #3
source("Scripts/stepbystep/02_descriptivestats/x_PaxScheduledNonEurope_descriptstats.R", echo = TRUE) #4
source("Scripts/stepbystep/02_descriptivestats/x_PaxScheduledEurope_descriptstats.R", echo = TRUE) #5
source("Scripts/stepbystep/02_descriptivestats/x_PaxScheduledDE_descriptstats.R", echo = TRUE) #6
source("Scripts/stepbystep/02_descriptivestats/x_PaxScheduledTot_descriptstats.R", echo = TRUE) #7
source("Scripts/stepbystep/02_descriptivestats/x_PaxTerminalTot_descriptstats.R", echo = TRUE) #8


####################################################
###### Exporting Descriptive Statistics Plots ######
####################################################

# NOTE: Change directory to save plots 
# Plots as used by LK are already saved and numbered in the Directory "plots"
# If no changes were made, the following lines of code are redundant


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/01_descriptive_stats/")

plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, "C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/01_descriptive_stats/", row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0("C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/01_descriptive_stats/01_", 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)

# Remove All Plots to create space!
dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()

####################################################

# (3) Third, Homogenize all Timeseries (Variance & Mean) & Note the Plots!
source("Scripts/stepbystep/03_data_reprocessing/x_TotTraffic_reprocess.R", echo = TRUE) #1
source("Scripts/stepbystep/03_data_reprocessing/x_PaxTransit_reprocess.R", echo = TRUE) #2
source("Scripts/stepbystep/03_data_reprocessing/x_OtherCommercTraffic_reprocess.R", echo = TRUE) #3
source("Scripts/stepbystep/03_data_reprocessing/x_PaxScheduledNonEurope_reprocess.R", echo = TRUE) #4
source("Scripts/stepbystep/03_data_reprocessing/x_PaxScheduledEurope_reprocess.R", echo = TRUE) #5
source("Scripts/stepbystep/03_data_reprocessing/x_PaxScheduledDE_reprocess.R", echo = TRUE) #6
source("Scripts/stepbystep/03_data_reprocessing/x_PaxScheduledTot_reprocess.R", echo = TRUE) #7
source("Scripts/stepbystep/03_data_reprocessing/x_PaxTerminalTot_reprocess.R", echo = TRUE) #8

####################################################
########## Exporting Homogenization Plots ##########
####################################################

# NOTE: Change directory to save plots 
# Plots as used by LK are already saved and numbered in the Directory "plots"
# If no changes were made, the following lines of code are redundant


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/02_homogenize/")

plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, "C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/02_homogenize/", row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0("C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/02_homogenize/02_", 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)

# Remove All Plots to create space!
dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()

####################################################

# (4) Fourth, Determine p & q based on ACF and PACF & Run AIC Tests to verify

## This section may result in various Warnings, which is unfortunate but not of great concern, as this section
## only serves to verify the previous decisions made to fit ARIMA Models
## Warnings created, as multiple ARIMA variations are tested simoulatneaously
## Some of these variations may not be possible and therefore produce NaNs 
## These variations are discarded, either due to a high AIC of NaN, which shows they are unviable

source("Scripts/stepbystep/04_AIC_Tests/x_TotTraffic_AIC_Tests.R", echo = TRUE) #1
source("Scripts/stepbystep/04_AIC_Tests/x_PaxTransit_AIC_Tests.R", echo = TRUE) #2
source("Scripts/stepbystep/04_AIC_Tests/x_OtherCommercTraffic_AIC_Tests.R", echo = TRUE) #3
source("Scripts/stepbystep/04_AIC_Tests/x_PaxScheduledNonEurope_AIC_Tests.R", echo = TRUE) #4
source("Scripts/stepbystep/04_AIC_Tests/x_PaxScheduledEurope_AIC_Tests.R", echo = TRUE) #5
source("Scripts/stepbystep/04_AIC_Tests/x_PaxScheduledDE_AIC_Tests.R", echo = TRUE) #6
source("Scripts/stepbystep/04_AIC_Tests/x_PaxScheduledTot_AIC_Tests.R", echo = TRUE) #7
source("Scripts/stepbystep/04_AIC_Tests/x_PaxTerminalTot_AIC_Tests.R", echo = TRUE) #8

# Clean up & remove no longer used data objects
rm(aic_A_OtherCommercTraffic,aic_A_PaxScheduledDE,aic_A_PaxScheduledEurope,
   aic_A_PaxScheduledNonEurope,aic_A_PaxScheduledTot,aic_A_PaxTerminalTot,
   aic_A_PaxTransit,aic_A_TotTraffic,aic_B_PaxScheduledDE,aic_B_PaxScheduledEurope,
   aic_B_PaxScheduledNonEurope,aic_B_PaxScheduledTot,aic_B_PaxTerminalTot,
   aic_B_PaxTransit,aic_B_TotTraffic,aic_C_OtherCommercTraffic,aic_C_PaxScheduledDE,
   aic_C_PaxScheduledEurope,aic_C_PaxScheduledNonEurope,aic_C_PaxScheduledTot,aic_C_PaxTerminalTot,
   aic_C_PaxTransit,aic_C_TotTraffic,aic_D_OtherCommercTraffic,aic_D_PaxScheduledDE,
   aic_D_PaxScheduledEurope,aic_D_PaxScheduledNonEurope,aic_D_PaxScheduledTot,
   aic_D_PaxTransit,aic_D_TotTraffic,aic_D_PaxTerminalTot)


# (5) Fifth, Fit model based on ACF & PACF analysis and AIC Tests
source("Scripts/stepbystep/05_ARIMA_fitting/X_TotTraffic_ARIMA_Fit.R", echo = TRUE) #1
source("Scripts/stepbystep/05_ARIMA_fitting/x_PaxTransit_ARIMA_Fit.R", echo = TRUE) #2
source("Scripts/stepbystep/05_ARIMA_fitting/x_OtherCommercTraffic_ARIMA_Fit.R", echo = TRUE) #3
source("Scripts/stepbystep/05_ARIMA_fitting/x_PaxScheduledNonEurope_ARIMA_Fit.R", echo = TRUE) #4
source("Scripts/stepbystep/05_ARIMA_fitting/x_PaxScheduledEurope_ARIMA_Fit.R", echo = TRUE) #5
source("Scripts/stepbystep/05_ARIMA_fitting/x_PaxScheduledDE_ARIMA_Fit.R", echo = TRUE) #6
source("Scripts/stepbystep/05_ARIMA_fitting/x_PaxScheduledTot_ARIMA_Fit.R", echo = TRUE) #7
source("Scripts/stepbystep/05_ARIMA_fitting/x_PaxTerminalTot_ARIMA_Fit.R", echo = TRUE) #8

# Produces the Regression Table presented in Methods chapter, which illustrates why no seasonal
# differentiation was feasible in the context of this Thesis.
library(stargazer)
source("Scripts/stepbystep/05_ARIMA_fitting/seasonal_argument/seasonal_argument.R", echo = TRUE)

####################################################
########## Exporting ARIMA Forecast Plots ##########
####################################################

# NOTE: Change directory to save plots 
# Plots as used by LK are already saved and numbered in the Directory "plots"
# If no changes were made, the following lines of code are redundant


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/03_ARIMA_forecast/")

plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, "C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/03_ARIMA_forecast/", row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0("C:/Users/Laurenz/OneDrive - bwedu/WiSe 2020-2021/Data/R_BA_DATA/BA_Airlines_Corona/plots/03_ARIMA_forecast/03_", 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)

# Remove All Plots to create space!
dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()

####################################################


####################################################
## Summarizing & Exporting the final ARIMA Model ##
####################################################

library(stargazer)

dep.varnames <- c("Total Traffic (incl. Transit)<br>TotTraffic",
                  "Total Terminal Traffic<br>PaxTerminalTot",
                  "Total Scheduled Traffic<br>PaxScheduledTot",
                  "Total Domestic Traffic (DE)<br>PaxScheduledDE",
                  "Total Continental Traffic (Europe)<br>PaxScheduledEurope",
                  "Total Intercontinental Traffic (Non Europe)<br>PaxScheduledNonEurope",
                  "Total Transit<br>PaxTransit",
                  "Other Commercial Traffic<br>OtherCommercTraffic")

stargazer(fit_A_TotTraffic,
          fit_A_PaxTerminalTot,
          fit_A_PaxScheduledTot,
          fit_A_PaxScheduledDE,
          fit_A_PaxScheduledEurope,
          fit_A_PaxScheduledNonEurope,
          fit_A_PaxTransit,
          fit_A_OtherCommercTraffic,
          type = "html", 
          out = "results/arima_Group_A.html", 
          title = "Group A ''Large Community Airports'' - All Traffic Types",
          dep.var.labels.include = FALSE,
          column.labels = dep.varnames,
          model.numbers = FALSE,
          initial.zero = FALSE,
          style = "aer")

stargazer(fit_B_TotTraffic,
          fit_B_PaxTerminalTot,
          fit_B_PaxScheduledTot,
          fit_B_PaxScheduledDE,
          fit_B_PaxScheduledEurope,
          fit_B_PaxScheduledNonEurope,
          fit_B_PaxTransit,
          fit_B_OtherCommercTraffic,
          type = "html", 
          out = "results/arima_Group_B.html", 
          title = "Group B ''National Airports'' - All Traffic Types",
          dep.var.labels.include = FALSE,
          column.labels = dep.varnames,
          model.numbers = FALSE,
          initial.zero = FALSE,
          style = "aer")

stargazer(fit_C_TotTraffic,
          fit_C_PaxTerminalTot,
          fit_C_PaxScheduledTot,
          fit_C_PaxScheduledDE,
          fit_C_PaxScheduledEurope,
          fit_C_PaxScheduledNonEurope,
          fit_C_PaxTransit,
          fit_C_OtherCommercTraffic,
          type = "html", 
          out = "results/arima_Group_C.html", 
          title = "Group C ''Large Regional Airports'' - All Traffic Types",
          dep.var.labels.include = FALSE,
          column.labels = dep.varnames,
          model.numbers = FALSE,
          initial.zero = FALSE,
          style = "aer")

stargazer(fit_D_TotTraffic,
          fit_D_PaxTerminalTot,
          fit_D_PaxScheduledTot,
          fit_D_PaxScheduledDE,
          fit_D_PaxScheduledEurope,
          fit_D_PaxScheduledNonEurope,
          fit_D_PaxTransit,
          fit_D_OtherCommercTraffic,
          type = "html", 
          out = "results/arima_Group_D.html", 
          title = "Group D ''Small Regional Airports'' - All Traffic Types",
          dep.var.labels.include = FALSE,
          column.labels = dep.varnames,
          model.numbers = FALSE,
          initial.zero = FALSE,
          style = "aer")



####################################################
################ Analysis Complete! ################
####################################################
