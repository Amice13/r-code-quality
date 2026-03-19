# export statistics to latex


forecastnames <-
  c(
    "KS",
    "MC",
    "MD",
    "TC",
    "IMC",
    "IMD",
    "ITC",
    "D(1)",
    "D(.9)",
    "D(.5)",
    "C-EN",
    "EN",
    "LA",
    "Ri",
    "PC1",
    "PC3",
    "3PRF",
    "3PRFm",
    "Buyhold"
  )
method.count <- length(forecastnames)
method.count_m1 <- length(forecastnames) - 1
statnames <-
  c(
    "{$R^2_{OS}$}",
    "{delta}",
    "{$R^2_{OS}$}",
    "{delta}",
    "",
    "{$R^2_{OS}$}",
    "{delta}",
    "{$R^2_{OS}$}",
    "{delta}",
    "",
    "{$R^2_{OS}$}",
    "{delta}",
    "{$R^2_{OS}$}",
    "{delta}"
  )


column.count <- length(statnames) # number of columns for latex table

output <- matrix(0, method.count, column.count)

output.function <- function(start, end, pred, freq, half = T) {
  for (i in 1:method.count_m1) {
    pval <- matrix(0, method.count, column.count)

    # OLS results, full sample
    
    predicted <- pred[[i]][start:end]
    spe.pred <- (pred[[74]][start:end] - predicted) ^ 2
    cw.third <- (pred[[73]][start:end] - predicted) ^ 2
    test <- pred[[75]][start:end] - spe.pred + cw.third
    
    
    reg.cw <- lm(test ~ 1)
    if (horizon == 1){
      pval[i, 1] <-
        pt(summary(reg.cw)$coefficients[, 3], reg.cw$df, lower = FALSE)
    }
    
    if (horizon > 1){
      nw_se <- NeweyWest(reg.cw, lag = horizon - 1, prewhite = FALSE)
      t.val <- coef(reg.cw) / sqrt(diag(nw_se))
      pval[i, 1] <- pt(t.val, df = df.residual(reg.cw), lower.tail = FALSE)
    }    
    
    
    cse.pred <- sum(spe.pred)
    
    cse.mean <- sum(pred[[75]][start:end])
    
    output[i, 1] <-
      format(round((1 - cse.pred / cse.mean) * 100, digits = 2), nsmall = 2)
    
    # WLS results, full sample
    predicted <- pred[[36 + i]][start:end]
    spe.pred <- (pred[[74]][start:end] - predicted) ^ 2
    cw.third <- (pred[[73]][start:end] - predicted) ^ 2
    test <- pred[[75]][start:end] - spe.pred + cw.third
    
    reg.cw <- lm(test ~ 1)
    if (horizon == 1){
      pval[i, 3] <-
        pt(summary(reg.cw)$coefficients[, 3], reg.cw$df, lower = FALSE)
    }
    
    if (horizon > 1){
      nw_se <- NeweyWest(reg.cw, lag = horizon - 1, prewhite = FALSE)
      t.val <- coef(reg.cw) / sqrt(diag(nw_se))
      pval[i, 3] <- pt(t.val, df = df.residual(reg.cw), lower.tail = FALSE)
    }  
    
    
    cse.pred <- sum(spe.pred)
    
    output[i, 3] <-
      format(round((1 - cse.pred / cse.mean) * 100, digits = 2), nsmall = 2)
    
    allocation <-
      utility.gains(
        benchmark = pred[[76]],
        forecast = pred[[18 + i]],
        actual = pred[[77]],
        start = start,
        end = end,
        freq = freq,
        varwidth = variance.years,
        gamma = gamma.allocation,
        cbp = transaction.cost
      )
    output[i, 2] <-
      format(round(allocation$delta.tc, digits = 2), nsmall = 2)
    
    allocation <-
      utility.gains(
        benchmark = pred[[76]],
        forecast = pred[[54 + i]],
        actual = pred[[77]],
        start = start,
        end = end,
        freq = freq,
        varwidth = variance.years,
        gamma = gamma.allocation,
        cbp = transaction.cost
      )
    output[i, 4] <-
      format(round(allocation$delta.tc, digits = 2), nsmall = 2)
    
    if (half == FALSE) {
      rowvector <- c(1, 3)
      
      for (k in rowvector) {
        output[i, k] <- case_when(
          pval [i, k] > 0.1 ~ paste0(output[i, k]),
          pval [i, k] <= 0.1 &
            pval [i, k] > 0.05 ~ paste0(output[i, k], "$^{*}$"),
          pval [i, k] <= 0.05 &
            pval [i, k] > 0.01 ~ paste0(output[i, k], "$^{**}$"),
          pval [i, k] <= 0.01 ~ paste0(output[i, k], "$^{***}$")
        )
      }
    }
    
    if (half == TRUE) {
      # OLS results , first sample half
      
      predicted <- pred[[i]][start:(end / 2)]
      spe.pred <- (pred[[74]][start:(end / 2)] - predicted) ^ 2
      cw.third <- (pred[[73]][start:(end / 2)] - predicted) ^ 2
      test <- pred[[75]][start:(end / 2)] - spe.pred + cw.third

      reg.cw <- lm(test ~ 1)
      if (horizon == 1){
        pval[i, 6] <-
          pt(summary(reg.cw)$coefficients[, 3], reg.cw$df, lower = FALSE)
      }
      
      if (horizon > 1){
        nw_se <- NeweyWest(reg.cw, lag = horizon - 1, prewhite = FALSE)
        t.val <- coef(reg.cw) / sqrt(diag(nw_se))
        pval[i, 6] <- pt(t.val, df = df.residual(reg.cw), lower.tail = FALSE)
      }  
      
      
      cse.pred <- sum(spe.pred)
      
      cse.mean <- sum(pred[[75]][start:(end / 2)])
      
      output[i, 6] <-
        format(round((1 - cse.pred / cse.mean) * 100, digits = 2), nsmall = 2)
      
      # WLS results, first sample half
      
      predicted <- pred[[36 + i]][start:(end / 2)]
      spe.pred <- (pred[[74]][start:(end / 2)] - predicted) ^ 2
      cw.third <- (pred[[73]][start:(end / 2)] - predicted) ^ 2
      test <- pred[[75]][start:(end / 2)] - spe.pred + cw.third
     
      reg.cw <- lm(test ~ 1)
      if (horizon == 1){
        pval[i, 8] <-
          pt(summary(reg.cw)$coefficients[, 3], reg.cw$df, lower = FALSE)
      }
      
      if (horizon > 1){
        nw_se <- NeweyWest(reg.cw, lag = horizon - 1, prewhite = FALSE)
        t.val <- coef(reg.cw) / sqrt(diag(nw_se))
        pval[i, 8] <- pt(t.val, df = df.residual(reg.cw), lower.tail = FALSE)
      }  
      
      
      
      cse.pred <- sum(spe.pred)
      
      output[i, 8] <-
        format(round((1 - cse.pred / cse.mean) * 100, digits = 2), nsmall = 2)
      
      # OLS results, second sample half
      
      predicted <- pred[[i]][(end / 2 + 1):end]
      spe.pred <- (pred[[74]][(end / 2 + 1):end] - predicted) ^ 2
      cw.third <- (pred[[73]][(end / 2 + 1):end] - predicted) ^ 2
      test <- pred[[75]][(end / 2 + 1):end] - spe.pred + cw.third

      
      reg.cw <- lm(test ~ 1)
      if (horizon == 1){
        pval[i, 11] <-
          pt(summary(reg.cw)$coefficients[, 3], reg.cw$df, lower = FALSE)
      }
      
      if (horizon > 1){
        nw_se <- NeweyWest(reg.cw, lag = horizon - 1, prewhite = FALSE)
        t.val <- coef(reg.cw) / sqrt(diag(nw_se))
        pval[i, 11] <- pt(t.val, df = df.residual(reg.cw), lower.tail = FALSE)
      }  
      
      
      
      cse.pred <- sum(spe.pred)
      
      cse.mean <- sum(pred[[75]][(end / 2 + 1):end])
      
      output[i, 11] <-
        format(round((1 - cse.pred / cse.mean) * 100, digits = 2), nsmall = 2)
      
      # WLS results, second sample half
      
      predicted <- pred[[36 + i]][(end / 2 + 1):end]
      spe.pred <- (pred[[74]][(end / 2 + 1):end] - predicted) ^ 2
      cw.third <- (pred[[73]][(end / 2 + 1):end] - predicted) ^ 2
      test <- pred[[75]][(end / 2 + 1):end] - spe.pred + cw.third
   
      reg.cw <- lm(test ~ 1)
      if (horizon == 1){
        pval[i, 13] <-
          pt(summary(reg.cw)$coefficients[, 3], reg.cw$df, lower = FALSE)
      }
      
      if (horizon > 1){
        nw_se <- NeweyWest(reg.cw, lag = horizon - 1, prewhite = FALSE)
        t.val <- coef(reg.cw) / sqrt(diag(nw_se))
        pval[i, 13] <- pt(t.val, df = df.residual(reg.cw), lower.tail = FALSE)
      }  
      cse.pred <- sum(spe.pred)
      
      output[i, 13] <-
        format(round((1 - cse.pred / cse.mean) * 100, digits = 2), nsmall = 2)
      
      rowvector <- c(1, 3, 6, 8, 11, 13)
      
      for (k in rowvector) {
        output[i, k] <- case_when(
          pval [i, k] > 0.1 ~ paste0(output[i, k]),
          pval [i, k] <= 0.1 &
            pval [i, k] > 0.05 ~ paste0(output[i, k], "$^{*}$"),
          pval [i, k] <= 0.05 &
            pval [i, k] > 0.01 ~ paste0(output[i, k], "$^{**}$"),
          pval [i, k] <= 0.01 ~ paste0(output[i, k], "$^{***}$")
        )
        
      }
      
      
      allocation <-
        utility.gains(
          benchmark = pred[[76]],
          forecast = pred[[18 + i]],
          actual = pred[[77]],
          start = start,
          end = end / 2,
          freq = freq,
          varwidth = variance.years,
          gamma = gamma.allocation,
          cbp = transaction.cost
        )
      output[i, 7] <-
        format(round(allocation$delta.tc, digits = 2), nsmall = 2)
      
      allocation <-
        utility.gains(
          benchmark = pred[[76]],
          forecast = pred[[18 + i]],
          actual = pred[[77]],
          start = (end / 2 + 1),
          end = end,
          freq = freq,
          varwidth = variance.years,
          gamma = gamma.allocation,
          cbp = transaction.cost
        )
      output[i, 12] <-
        format(round(allocation$delta.tc, digits = 2), nsmall = 2)
      
      allocation <-
        utility.gains(
          benchmark = pred[[76]],
          forecast = pred[[54 + i]],
          actual = pred[[77]],
          start = start,
          end = end / 2,
          freq = freq,
          varwidth = variance.years,
          gamma = gamma.allocation,
          cbp = transaction.cost
        )
      output[i, 9] <-
        format(round(allocation$delta.tc, digits = 2), nsmall = 2)
      
      allocation <-
        utility.gains(
          benchmark = pred[[76]],
          forecast = pred[[54 + i]],
          actual = pred[[77]],
          start = (end / 2 + 1),
          end = end,
          freq = freq,
          varwidth = variance.years,
          gamma = gamma.allocation,
          cbp = transaction.cost
        )
      output[i, 14] <-
        format(round(allocation$delta.tc, digits = 2), nsmall = 2)
    }
    
  }
  
  allocation <-
    bh.utility.gains(
      benchmark = pred[[76]],
      forecast = pred[[18 + 1]],
      actual = pred[[77]],
      start = start,
      end = end,
      freq = freq,
      varwidth = variance.years,
      gamma = gamma.allocation,
      cbp = transaction.cost
    )
  
  output[19, 2] <-
    format(round(allocation$delta.tc, digits = 2), nsmall = 2)
  output[19, 4] <-
    format(round(allocation$delta.tc, digits = 2), nsmall = 2)
  
  
  allocation <-
    bh.utility.gains(
      benchmark = pred[[76]],
      forecast = pred[[18 + 1]],
      actual = pred[[77]],
      start = start,
      end = end / 2,
      freq = freq,
      varwidth = variance.years,
      gamma = gamma.allocation,
      cbp = transaction.cost
    )
  output[19, 7] <-
    format(round(allocation$delta.tc, digits = 2), nsmall = 2)
  output[19, 9] <-
    format(round(allocation$delta.tc, digits = 2), nsmall = 2)
  
  allocation <-
    bh.utility.gains(
      benchmark = pred[[76]],
      forecast = pred[[18 + 1]],
      actual = pred[[77]],
      start = (end / 2 + 1),
      end = end,
      freq = freq,
      varwidth = variance.years,
      gamma = gamma.allocation,
      cbp = transaction.cost
    )
  output[19, 12] <-
    format(round(allocation$delta.tc, digits = 2), nsmall = 2)
  output[19, 14] <-
    format(round(allocation$delta.tc, digits = 2), nsmall = 2)
  
  
  rownames(output) <- forecastnames
  colnames(output) <- statnames
  
  return(output)
  
}
# Without restrictions
oos.length <- length(forecast.list[[1]])
results.table <- output.function(
  start = 1,
  end = oos.length,
  pred = forecast.list,
  freq = frequency,
  half = TRUE
)
# convert utility gains to numeric for excel output
results.table <- as.data.frame(results.table)

# empty columns (spaces)
empty <- c(5, 10)
for (k in empty) {
  results.table[, k] <- ""
}

colvector <- c(2, 4, 7, 9, 12, 14)

for (k in colvector) {
  results.table[, k] <- as.numeric(results.table[, k])
  
}

# print latex table
print(
  xtable(results.table, type = "latex"),
  sanitize.text.function = identity,
  size = "\\fontsize{9pt}{10pt}\\selectfont",
  file = stringr::str_c(filename.component,".tex"),
  floating.environment = "sidewaystable"
)

# With CT restrictions

oos.length.ct <- length(forecast.ct.list[[1]])
results.ct.table <- output.function(
  start = 1,
  end = oos.length.ct,
  pred = forecast.ct.list,
  freq = frequency,
  half = TRUE
)
# convert utility gains to numeric for excel output
results.ct.table <- as.data.frame(results.ct.table)

# empty columns (spaces)
empty <- c(5, 10)
for (k in empty) {
  results.ct.table[, k] <- ""
}

colvector <- c(2, 4, 7, 9, 12, 14)

for (k in colvector) {
  results.ct.table[, k] <- as.numeric(results.ct.table[, k])
  
}

# print latex table
print(
  xtable(results.ct.table, type = "latex"),
  sanitize.text.function = identity,
  size = "\\fontsize{9pt}{10pt}\\selectfont",
  file = stringr::str_c(filename.component,".ct.tex"),
  floating.environment = "sidewaystable"
)

