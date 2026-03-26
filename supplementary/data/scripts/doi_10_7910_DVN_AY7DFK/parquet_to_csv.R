args = commandArgs(trailingOnly = TRUE)
library(arrow)
library(readr)

df = read_parquet(args[1])
write_csv(df, args[2])