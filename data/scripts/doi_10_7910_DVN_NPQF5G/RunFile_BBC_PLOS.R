###################################
# Baumgartner, Box-Steffensmeier,
#   Campbell Replication Run File
# Instructions: 
#   1) Change local working directory
#      currently its for Campbell
#   2) Run the entire script which 
#      will call the run file for 
#      the analysis and produce the 
#      relevant figures and tables
#
#   Note: This will take approximately
#         12 hours to complete
###################################

### Set local working directory
setwd("~/ReplicationArchive")

### Install all needed packages
needed <- setdiff(c("readstata13", "rio", "grid", "poweRlaw", "xtable", "RColorBrewer", "data.table", "foreign", "car", "texreg", "Rmisc"), 
                  installed.packages()[,"Package"])

if(length(needed) > 0){
  install.packages(needed, repos = "https://cloud.r-project.org/", dependencies = TRUE)
}

### Source run file which will replicate the entire analyses and output  tables and figures into the working directory.  
source("Replication_BBC_PLOS.R")