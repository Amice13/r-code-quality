#' Test Covariate Balance Using Equivalence Tests
#'
#' @param varname Character. Covariate variable name
#' @param treat Character. Treatment variable name  
#' @param w Character. Weight variable name
#' @return Data.frame. Equivalence test results for weighted vs unweighted
est_equiv <- function(varname, treat, w) {
  
  require(parameters)
  require(stats)
  
  # define empirical model
  f <- as.formula(paste0(treat, " ~ ", varname)) #, "+globalreg"
  # estimate model without weights
  m_uw <- lm(f, data = g)
  # estimate model with CBPS
  m_wt <- lm(f, data = g, weights = g[[w]])
  # conduct equivalence tests
  equiv_uw <- bayestestR::equivalence_test(m_uw, rule = "classic")
  equiv_wt <- bayestestR::equivalence_test(m_wt, rule = "classic")
  # extract model information
  df_uw <- data.frame(equiv_uw)
  df_uw$sample <- "Unadjusted"
  df_uw$estimate <- coef(m_uw) # get OLS estimate
  ## weighted data
  df_wt <- data.frame(equiv_wt)
  df_wt$sample <- "Adjusted"
  df_wt$estimate <- coef(m_wt) # get OLS estimate

  df_out <- rbind(df_uw, df_wt)

  df_out <- subset(df_out, Parameter != "(Intercept)")

  return(df_out)
}
