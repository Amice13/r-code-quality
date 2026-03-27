# balance function to get the mean, p-value, CI, difference from the t-test
bal <- function(y,data){
    x1 <- data[y][data$treat == 1,] 
    x2 <- data[y][data$treat == 0,]
    meanx <- unname(t.test(x = x1, y = x2)$estimate[1])
    meany <- unname(t.test(x = x1, y = x2)$estimate[2])
    diff <- meanx - meany
    min <- t.test(x = x1, y = x2)$conf.int[1]
    max <- t.test(x = x1, y = x2)$conf.int[2]
    p <- t.test(x = x1, y = x2)$p.value
    return(c(meanx,meany,diff,min,max,p))
}
