#' Estimate Placebo Tests for Treatment Effects
#'
#' @param shockvar Character. Name of shock variable
#' @param fe Character. Fixed effects specification (default: "globalreg")
#' @param outcome Character. Outcome variable name (default: "salient")
#' @param df Data.frame. Analysis dataset (default: global 'g')
#' @return Data.frame. Average marginal effects for placebo test
est_placebo <- function(shockvar, fe = "globalreg", outcome = "salient", df = g) {
  wt <- gsub("_z", "", paste0("w_treat_", shockvar))
  f_in <- as.formula(paste0(outcome, "~ reg_loser_50 * (", shockvar, "*", hetvars, covars, ")", "|", fe))
  m_both <- feols(fml = f_in, data = df, vcov = ~cty_reg, weights = df[[wt]])
  ame <- avg_slopes(m_both, variables = shockvar, by = "reg_loser_50") |> data.frame()
  
  return(ame)
}
