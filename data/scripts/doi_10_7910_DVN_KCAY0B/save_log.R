# Working directory - change as necessary
setwd("~/Dropbox/Auth Backsliding/REPLICATION MARCH 2024/")

# Save file for code of main text
sink("logs/main_text_log.txt")
source('main_text.r', echo = TRUE)
sink()

# Save file for code of appendix
sink("logs/appendix_log.txt")
source('appendix.r', echo = TRUE)
sink()