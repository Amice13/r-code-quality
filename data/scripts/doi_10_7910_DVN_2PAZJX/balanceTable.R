balance_table <- function(DT){
    ## This is a helper function to calculate and print a check for covariate
    ## balance on the data. It proceeds in two steps: 
    ## 
    ##   1. Calculates a mean and standard deviation of Age, Reported Gender,
    ##      Years in the US, and Years of Education.
    ##   2. Reports the p-values of a t-test for differences in mean between
    ##      the baseline category (not shown any images of candidates) and the
    ##      treatment conditions.


    balance.table <- DT[ , .(
        "Mean.Age"     = mean(age, na.rm = TRUE),
        "SD.Age"       = sd(age, na.rm = TRUE),
        "PctWoman"     = mean(I(sex == 2), na.rm = TRUE),
        "SD.PctWoman"  = sd(I(sex == 2), na.rm = TRUE),
        "MeanYrsUS"    = mean(yearsinus, na.rm = TRUE),
        "SD.MeanYrsUS" = sd(yearsinus, na.rm = TRUE),
        "Mean.Ed"      = mean(educ, na.rm = T),
        "SD.Ed"        = sd(educ, na.rm = T)) ,
        by = conditions]

    balance.table <- na.omit(balance.table)
    balance.table <- balance.table[order(balance.table$conditions), ]
    balance.table <- data.frame(balance.table)

    ##
    ## 1. add results of t-test
    ## 2. capture the result of this loop as a separate table,
    ##     then append to the data table
    ##

    balance.table$t.age       <- NA
    balance.table$t.sex       <- NA
    balance.table$t.yearsinus <- NA
    balance.table$t.educ      <- NA

    ##
    ## subset the data to compare conditions against the baseline.
    ## - conditions are indexed in `j` in this table;
    ## - condition 2 is compared to condition 1; then condition 3 againt 1 and so on.
    ##

    ## First, compare for reported gender. 

    for(j in 2:4){
        temp <- t.test(I(sex == 2) ~ conditions,
                       data = subset(DT, conditions %in% c(1,j)))
        balance.table[
            balance.table$conditions == j,
            paste('t', 'sex', sep = '.')] <- as.numeric(temp[[3]])
    }


    ## Now, compare for `age`, `yearsinus`, and `educ`

    vars <- c('age', 'yearsinus', 'educ')

    for(i in vars){
        for(j in 2:4){
            temp <- t.test(get(i) ~ conditions,
                           data = subset(DT, conditions %in% c(1,j)))
            balance.table[balance.table$conditions == j,
                          paste('t', i, sep = '.')] <- as.numeric(temp[[3]])
        }
    }

    ## Rename table for printing

    varname            <- row.names(t(balance.table))
    newbaltable        <- data.frame(varname, as.data.frame(t(balance.table)))
    names(newbaltable) <- c('varname', 'control', 'white', 'mestizo', 'indigenous') 
    row.names(newbaltable) <- 1:length(row.names(newbaltable))

    return(newbaltable)
}
