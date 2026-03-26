rm(list = ls())
library(dplyr)
library(stats)
library(spatstat)
library(data.table)

datadir <- '../data/'
outdir  <- '../output/'

line.col1 <- 'black'
line.col2 <- 'lightskyblue'

load(file = paste0(datadir,'intermediate_file_1.RData'))

data <- data.frame(data)


data <- data %>% mutate(
                     contract_weight = contract_obligations/sum(contract_obligations),
                     dpd             = contract_obligations/duration
                 )

## Keep only contracts with positive values (> 99% of data)
data <- data %>% filter(contract_obligations > 0)

##### Generate ECDFs
ecdf      <- ecdf(data$duration) 
ecdf.wtd  <- ewcdf(data$duration, weights = data$contract_obligations)


pdf(paste0(outdir,'pooled_duration_ecdfs.pdf'), height = 5, width = 5)
plot(ecdf, lwd = 3, col = line.col1, do.points = FALSE,
     main = '', xlab = 'Duration (Days)', ylab = 'Cumulative Share',
     lty = 1, verticals = TRUE, col.01line = 'gray1', bty = 'l')
lines(ecdf.wtd, lwd = 3, col = line.col2, col.01line = 'gray1', do.points = FALSE, verticals = TRUE)
abline(v = 365, lwd = 0.7, col = 'gray1', lty = 5)
abline(v = 365*2, lwd = 0.7, col = 'gray1', lty = 5)
legend('right', legend = c("Unweighted", "Dollar-Weighted"),
       col = c(line.col1, line.col2), lwd = 3, lty = 1, bty = 'n')
dev.off()

