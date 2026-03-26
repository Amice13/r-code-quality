# please run this code to get the input data for the regression in STATA 
# This was done to mask the column names in the original data.

library(readstata13)
library(data.table)
dt   = fread("/Users/Mani/Dropbox/Panama S2/Draft Manuscripts/Natural and Physical Capital Complementarity/submitted files/Accepted version/Submissions codes and data/regression_data/regression_data.csv")
cols = fread("/Users/Mani/Dropbox/Panama S2/Draft Manuscripts/Natural and Physical Capital Complementarity/submitted files/Accepted version/Submissions codes and data/regression_data/column_names.csv")
setnames(dt, old = cols$old_cols, new = cols$new_cols)
save.dta13(dt, "/Users/Mani/Dropbox/Panama S2/Draft Manuscripts/Natural and Physical Capital Complementarity/submitted files/Accepted version/Submissions codes and data/regression_data/regression_data.dta")
