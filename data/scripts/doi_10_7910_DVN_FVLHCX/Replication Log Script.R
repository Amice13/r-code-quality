# Master Script to Create Log File

# Start logging
log_file <- "replication_log.txt"
sink(log_file, append = TRUE)

cat("Replication started at:", Sys.time(), "\n")

# List of scripts to run
scripts <- c('1_Figure 1.R', '2_Figure A1.R', '3_Figure 2.R', '4_Table A2.R',
             '5_Figure A2.R', '6_Figure A3.R', '7_Figure A4.R', '8_Figure A5.R',
             '9_Figure 3.R', '10_Table A3.R', '11_Figure A6.R', '12_Figure A7.R',
             '13_Table 2.R')



# Run each script
for (script in scripts) {
  cat("\nRunning:", script, "at", Sys.time(), "\n")
  tryCatch(
    {
      source(script)
      cat("Completed:", script, "successfully.\n")
    },
    error = function(e) {
      cat("Error in", script, ":", conditionMessage(e), "\n")
    }
  )
}

cat("Replication completed at:", Sys.time(), "\n")

# Stop logging
sink()


