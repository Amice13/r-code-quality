################################################################
##################### Asset allocation #########################
################################################################


## Function for asset allocation ----
utility.gains <- 
  function(gamma=3,
           varwidth=10,
           lower.bound=0,
           upper.bound=1.5,
           cbp=0,
           start,
           end,
           freq,
           benchmark,
           forecast,
           actual){
    
    simple <- forecast.data$simple_premium
    riskfree <- forecast.data$Rfree
   
    if (freq == 4){
      annualize <- 400
    }
    
    if (freq == 12){
      annualize <- 1200
    }
    
    variance <- 
      stats::lag(rollapply(
        simple,
        width = (freq*varwidth),
        FUN = var,
        align = "right",
        fill = NA),-1)

    forecast.average <- benchmark[start:end]
    forecast.competing <- forecast[start:end]
    
    portfolio.average <- 
      cbind(forecast.average,variance)
    portfolio.competing <- 
      cbind(forecast.competing,variance)
    
    ### weight calculation ----
    weight.average <- 
      (1/gamma)*(portfolio.average$forecast.average/(portfolio.average$variance))
    weight.competing <- 
      (1/gamma)*(portfolio.competing$forecast.competing/(portfolio.competing$variance))
    
    #### Weight constraints ----      
    weight.average <- 
      ifelse(weight.average<lower.bound,
             lower.bound,
             weight.average)
    weight.competing <- 
      ifelse(weight.competing<lower.bound,
             lower.bound,
             weight.competing)
    weight.competing <- 
      ifelse(weight.competing>upper.bound,
             upper.bound,
             weight.competing)
    weight.average <- 
      ifelse(weight.average>upper.bound,
             upper.bound,
             weight.average)

    actual <- actual[start:end]
    
    ### Wealth calculations for TC ---- 
    ## Total wealth
    total.wealth.average <- 
      1+riskfree+weight.average*(actual)
    total.wealth.competing <- 
      1+riskfree+weight.competing*(actual)
    
    #- Total wealth from risky asset in period t
      
    risky.wealth.average <- 
      (weight.average)*(1+riskfree+actual)
    risky.wealth.competing <- 
      (weight.competing)*(1+riskfree+actual)
    
    ## targeted weight on the risky asset in period t+1
      
    lagged.weight.average <- 
      stats::lag(weight.average,1)
    lagged.weight.competing <- 
      stats::lag(weight.competing,1)
    
    target.risky.average <- 
      lagged.weight.average*total.wealth.average
    target.risky.competing <- 
      lagged.weight.competing*total.wealth.competing
    
    ### Turnover ----      
    turnover.average <- 
      (abs(target.risky.average-risky.wealth.average))/total.wealth.average
    turnover.competing <- 
      (abs(target.risky.competing-risky.wealth.competing))/total.wealth.competing
    
    ### TC calculation ----
    
    c <- cbp/10000
      
    transaction.cost.average <-
      c*turnover.average
    transaction.cost.competing <- 
      c*turnover.competing
    
  # return on portfolio after transaction costs
      
    return.average.tc <- 
      (1+riskfree+weight.average*actual)*(1-transaction.cost.average)-1
    return.competing.tc <- 
      (1+riskfree+weight.competing*actual)*(1-transaction.cost.competing)-1
    
    return.average.without.tc <- 
      riskfree+(weight.average)*(actual)
    return.competing.without.tc <- 
      riskfree+weight.competing*actual
    
    t <- length(return.average.without.tc)
    
    last.period.average <- 
      return.average.without.tc[t]
    last.period.competing <- 
      return.competing.without.tc[t]
    
    # Need to deal with missing values at end because of no next target allocation
    # We assume that the portfolio at the end is valued, not liquidated, hence no transaction cost
    # average
    if(length(return.average.tc) == length(return.average.without.tc)){
      
      return.average.tc <- 
        ifelse(!is.na(return.average.tc),
               return.average.tc,
               last.period.average)
    }
    else{
      return.average.tc <- c(return.average.tc, last.period.average)
    }
    
    # competing    
    if(length(return.competing.tc) == length(return.competing.without.tc)){
      
      return.competing.tc <- 
        ifelse(!is.na(return.competing.tc),
               return.competing.tc,
               last.period.competing)
    }
    else{
      return.competing.tc <- c(return.competing.tc, last.period.competing)
    }
    
    
    
    
    
    ## average turnover of return forecast
      
    average.turnover.average <- 
      mean(turnover.average,na.rm = T)
    average.turnover.competing <- 
      mean(turnover.competing,na.rm=T)
    
    ### Relative average turnover ----
    
    rel.avg.to <- 
      average.turnover.competing/average.turnover.average
    
    ### CER ---- 
      
    mu.average <- 
      mean(return.average.without.tc,na.rm = T)
    var.average <- 
      var(return.average.without.tc,na.rm = T)
    nu.average <- 
      mu.average-0.5*gamma*var.average
      
    mu.competing <- 
      mean(return.competing.without.tc,na.rm = T)
    var.competing <- 
      var(return.competing.without.tc,na.rm = T)
    nu.competing <- 
      mu.competing-0.5*gamma*var.competing
    
    mu.average.tc <- 
      mean(return.average.tc,na.rm = T)
    var.average.tc <- 
      var(return.average.tc,na.rm = T)
    nu.average.tc <- 
      mu.average.tc-0.5*gamma*var.average.tc
    
    mu.competing.tc <- 
      mean(return.competing.tc,na.rm = T)
    var.competing.tc <- 
      var(return.competing.tc,na.rm = T)
    nu.competing.tc <- 
      mu.competing.tc-0.5*gamma*var.competing.tc
  
    ### CER difference ----      
    delta <- 
      (nu.competing-nu.average)*(annualize)
    delta.tc <- 
      (nu.competing.tc-nu.average.tc)*(annualize)
      
    ### Output ----
    return(list(
        delta = delta,delta.tc = delta.tc,
        rel.avg.to = rel.avg.to,
        weight.competing = weight.competing,
        weight.average = weight.average))
      
}



## Function for utility gain of buy-and-hold relative to historical average
## This simply modifies the general function from above
## by setting the competing strategy returns equal to the buy-and-hold returns

bh.utility.gains <- 
  function(gamma=3,
           varwidth=10,
           lower.bound=0,
           upper.bound=1.5,
           cbp=0,
           start,
           end,
           freq,
           benchmark,
           forecast,
           actual){
    
    simple <- forecast.data$simple_premium
    riskfree <- forecast.data$Rfree
    
    if (freq == 4){
      annualize <- 400
    }
    
    if (freq == 12){
      annualize <- 1200
    }
    
    variance <- 
      stats::lag(rollapply(
        simple,
        width = (freq*varwidth),
        FUN = var,
        align = "right",
        fill = NA),-1)
    
    forecast.average <- benchmark[start:end]
    forecast.competing <- forecast[start:end]
    
    portfolio.average <- 
      cbind(forecast.average,variance)
    portfolio.competing <- 
      cbind(forecast.competing,variance)
    
    ### weight calculation ----
    weight.average <- 
      (1/gamma)*(portfolio.average$forecast.average/(portfolio.average$variance))
    weight.competing <- 
      (1/gamma)*(portfolio.competing$forecast.competing/(portfolio.competing$variance))
    
    #### Weight constraints ----      
    weight.average <- 
      ifelse(weight.average<lower.bound,
             lower.bound,
             weight.average)
    weight.competing <- 
      ifelse(weight.competing<lower.bound,
             lower.bound,
             weight.competing)
    weight.competing <- 
      ifelse(weight.competing>upper.bound,
             upper.bound,
             weight.competing)
    weight.average <- 
      ifelse(weight.average>upper.bound,
             upper.bound,
             weight.average)
    
    actual <- actual[start:end]
    
    ### Wealth calculations for TC ---- 
    ## Total wealth
    total.wealth.average <- 
      1+riskfree+weight.average*(actual)
    total.wealth.competing <- 
      1+riskfree+weight.competing*(actual)
    
    #- Total wealth from risky asset in period t
    
    risky.wealth.average <- 
      (weight.average)*(1+riskfree+actual)
    risky.wealth.competing <- 
      (weight.competing)*(1+riskfree+actual)
    
    ## targeted weight on the risky asset in period t+1
    
    lagged.weight.average <- 
      stats::lag(weight.average,1)
    lagged.weight.competing <- 
      stats::lag(weight.competing,1)
    
    target.risky.average <- 
      lagged.weight.average*total.wealth.average
    target.risky.competing <- 
      lagged.weight.competing*total.wealth.competing
    
    ### Turnover ----      
    turnover.average <- 
      (abs(target.risky.average-risky.wealth.average))/total.wealth.average
    turnover.competing <- 
      (abs(target.risky.competing-risky.wealth.competing))/total.wealth.competing
    
    ### TC calculation ----
    
    c <- cbp/10000
    
    transaction.cost.average <-
      c*turnover.average
    transaction.cost.competing <- 
      c*turnover.competing
    
    # return on portfolio after transaction costs
    
    return.average.tc <- 
      (1+riskfree+weight.average*actual)*(1-transaction.cost.average)-1
    return.competing.tc <- riskfree+actual
    
    
    return.average.without.tc <- 
      riskfree+(weight.average)*(actual)
    return.competing.without.tc <- riskfree+actual
    
    t <- length(return.average.without.tc)
    
    last.period.average <- 
      return.average.without.tc[t]
    last.period.competing <- 
      return.competing.without.tc[t]
    
    # Need to deal with missing values at end because of no next target allocation
    # We assume that the portfolio at the end is valued, not liquidated, hence no transaction cost
    # average
    if(length(return.average.tc) == length(return.average.without.tc)){
      
      return.average.tc <- 
        ifelse(!is.na(return.average.tc),
               return.average.tc,
               last.period.average)
    }
    else{
      return.average.tc <- c(return.average.tc, last.period.average)
    }
    
    # competing    
    if(length(return.competing.tc) == length(return.competing.without.tc)){
      
      return.competing.tc <- 
        ifelse(!is.na(return.competing.tc),
               return.competing.tc,
               last.period.competing)
    }
    else{
      return.competing.tc <- c(return.competing.tc, last.period.competing)
    }
    
    
    
    
    
    ## average turnover of return forecast
    
    average.turnover.average <- 
      mean(turnover.average,na.rm = T)
    average.turnover.competing <- 
      mean(turnover.competing,na.rm=T)
    
    ### Relative average turnover ----
    
    rel.avg.to <- 
      average.turnover.competing/average.turnover.average
    
    ### CER ---- 
    
    mu.average <- 
      mean(return.average.without.tc,na.rm = T)
    var.average <- 
      var(return.average.without.tc,na.rm = T)
    nu.average <- 
      mu.average-0.5*gamma*var.average
    
    mu.competing <- 
      mean(return.competing.without.tc,na.rm = T)
    var.competing <- 
      var(return.competing.without.tc,na.rm = T)
    nu.competing <- 
      mu.competing-0.5*gamma*var.competing
    
    mu.average.tc <- 
      mean(return.average.tc,na.rm = T)
    var.average.tc <- 
      var(return.average.tc,na.rm = T)
    nu.average.tc <- 
      mu.average.tc-0.5*gamma*var.average.tc
    
    mu.competing.tc <- 
      mean(return.competing.tc,na.rm = T)
    var.competing.tc <- 
      var(return.competing.tc,na.rm = T)
    nu.competing.tc <- 
      mu.competing.tc-0.5*gamma*var.competing.tc
    
    ### CER difference ----      
    delta <- 
      (nu.competing-nu.average)*(annualize)
    delta.tc <- 
      (nu.competing.tc-nu.average.tc)*(annualize)
    
    ### Output ----
    return(list(
      delta = delta,delta.tc = delta.tc,
      rel.avg.to = rel.avg.to,
      weight.competing = weight.competing,
      weight.average = weight.average))
    
  }
