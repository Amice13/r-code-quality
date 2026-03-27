  # READ DATA
    d <- read.table("cache/d.csv", header=TRUE, sep=",",
                    stringsAsFactors=F, encoding = "UTF-8")

    d$max.left.factor <- as.factor(d$max.left)
    d$max.right.factor <- as.factor(d$max.right)

########################################
## DESCRIPTIVE STATISTICS FOR VARIABLES
########################################


    # DESCRIPTIVE STATISTICS - ALL VARIABLES
    # SUMMARY TABLE FOR NUMERIC VARIABLES
    variables <- psych::describe(d[,c("education", "age", "east", "male", "pol.interest", "income",
                                 "Ltopic1", "Ltopic2", "Ltopic3", "Ltopic4",
                                 "Rtopic1", "Rtopic2", "Rtopic3", "Rtopic4")])
    variables <- round(data.frame(variables[c(2,3,4,8,9,10)]),2)
    variables$min <- round(variables$min, 0)
    variables$max <- round(variables$max, 0)
    variables$range <- round(variables$range, 0)
    # SUMMARY TABLE For CAtegoricAL VARIABLES
    varnames <- c("factor.party.preference", "max.left.factor", "max.right.factor")
    n.categories <- NULL
    n <- NULL
    shares.categories <- NULL
    for (i in varnames){
      n.cat <- length(table(d[c(i)]))
      # n.categories <- c(n.categories, n.cat)
      n.sum <- sum(table(d[c(i)]))
      n <- c(n, n.sum)
      share.names.values <- rbind(names(table(d[c(i)])), as.numeric(table(d[c(i)])))
      shares <- NULL
      for (z in 1:n.cat){
        single.shares <- paste(share.names.values[1,z], " = ", share.names.values[2,z], sep="")
        shares <- c(shares,single.shares)
      }
      all.shares <- paste(shares, collapse = ", ")
      shares.categories <- c(shares.categories, all.shares)
    }
    variables.categorical <- data.frame(cbind(n , shares.categories, NA, NA, NA, NA)) # n.categories,
    rownames(variables.categorical) <- varnames
    names(variables.categorical) <- names(variables)
    # COMBINE NUMERIC AND CATEGORICAL VARIABLES
    summary <- rbind(variables, variables.categorical)

    # GENERATE LATEX TABLE
    Hmisc::latex(summary, file="")

    
    
