# INT.: Card 28 has been displayed!
# Political parties are often regarded as being on the "left" or on the "right".
# Please tell me where you would place the following parties on this scale.
# Let me read the parties out to you.
# INT: Please read out the names of the political parties!
# The Christian Democratic Union V107
# The Christian Social Union V108
# The Social Democratic Party V109
# The Free Democratic Party V110
# The Left V111
# Alliance '90/The Greens V112
# National Democratic Party of Germany V113
# Left wing o o o o o o o o o o Right wing



    
    
# IMPORT ALLBUS 2008 DATA (sociodemographics etc.)
    d2 <- read.csv("ZA4600_A08.txt", sep="#")
    names(d2)[2] <- "id"
    
    # party left-right placements
    #   The Christian Democratic Union V107
    # The Christian Social Union V108
    # The Social Democratic Party V109
    # The Free Democratic Party V110
    # The Left V111
    # Alliance '90/The Greens V112
    # National Democratic Party of Germany V113
    
    vars <- c("v107", "v108", "v109", "v110", "v111", "v112", "v113")
    new.vars <- c("cdu.leftright", "csu.leftright", "spd.leftright", "fdp.leftright", 
                  "linke.leftright", "greens.leftright", "npd.leftright")
    for(i in 1:length(vars)){
      d2[,new.vars[i]] <- mapvalues(d2[,vars[i]], from = c(99, 98), to = c(NA, NA)) 
    }
    table(d2$cdu.leftright,d2$v107)
    
    d2 <- dplyr::select(d2, id, cdu.leftright, csu.leftright, spd.leftright, fdp.leftright, 
                        linke.leftright, greens.leftright, npd.leftright)
    

    
    # MERGE PROBING DATA WITH ALLBUS 2008
    d <- left_join(d, d2, by="id") # c
    
    
    levels(d$max.left.factor) <- c("Values.L", "Ideologies.L", "Parties.L", "Policies.L")
    levels(d$max.right.factor) <- c("Right-wing.extremists.R", "Parties.R", "Ideologies.R", "Xenophobia.R")
    
    # paste(names(d)[49:55],  collapse="', '")
    d.vars <- c('cdu.leftright', 'csu.leftright', 'spd.leftright', 'fdp.leftright', 'linke.leftright', 'greens.leftright', 'npd.leftright')
    d.vars <- rep(d.vars,2)
    i.vars <- c(rep('max.left.factor', 7), rep('max.right.factor', 7))
    results <- NULL
    for (i in 1:length(d.vars)){
    fit.i <- lm(d[,d.vars[i]] ~ d[,i.vars[i]])
    # fit.i <- summary(fit.i)
    names(fit.i$coefficients) <- gsub("d\\[, i\\.vars\\[i\\]\\]", "", names(fit.i$coefficients))  
        results[[i]] <- fit.i
    }
    
    stargazer(results, type = "html", out = "plots/pred.party.placement.html",
              column.labels = toupper(gsub(".leftright", " LR", d.vars)), dep.var.labels.include = FALSE)
    
    

    

   