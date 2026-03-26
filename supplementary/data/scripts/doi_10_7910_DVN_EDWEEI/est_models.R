#' Estimate Treatment Effect Models Using Multiple Approaches
#'
#' Estimates climate shock treatment effects using three different approaches:
#' covariate adjustment, CBPS weighting, and doubly robust (both).
#'
#' @param shockvar Character. Name of the climate shock variable
#' @param fe Character. Fixed effects specification (default: "globalreg")
#' @param outcome Character. Name of outcome variable
#' @param df Data.frame. Analysis dataset (default: global 'g')
#' @param hetvars Character. Formula string for heterogeneity variables (must be defined in global environment)
#' @param covars Character. Formula string for covariates (must be defined in global environment)
#'
#' @return List with three elements:
#'   \item{Covariates}{List with AME estimates and hypothesis tests for covariate-adjusted model}
#'   \item{CBPS}{List with AME estimates and hypothesis tests for CBPS-weighted model}
#'   \item{CBPS + Covariates}{List with AME estimates and hypothesis tests for doubly robust model}
#'
#' @details
#' Function constructs weight variable name by removing "_z" suffix and adding "w_treat_" prefix.
#' Uses clustered standard errors at administrative zone level (cty_reg).
#' Estimates average marginal effects by vulnerability status (reg_loser_50).
#'
#' @examples
#' \dontrun{
#' # Requires global variables: hetvars, covars
#' results <- est_models("best_tanom_7d_z", outcome = "salient", df = analysis_data)
#' }
est_models <- function(shockvar, fe = "globalreg", outcome, df = g, hetvars, covars) {
  
  # Input validation
  stopifnot("shockvar must be character" = is.character(shockvar) && length(shockvar) == 1)
  stopifnot("fe must be character" = is.character(fe) && length(fe) == 1)
  stopifnot("outcome must be character" = is.character(outcome) && length(outcome) == 1)
  stopifnot("df must be a data.frame" = is.data.frame(df))
  stopifnot("hetvars must be character" = is.character(hetvars) && length(hetvars) == 1)
  stopifnot("covars must be character" = is.character(covars) && length(covars) == 1)
  
  # Check required variables exist in data
  required_vars <- c(shockvar, outcome, "reg_loser_50", "cty_reg")
  missing_vars <- required_vars[!required_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    stop("Missing required variables in data: ", paste(missing_vars, collapse = ", "))
  }
    
  # Construct and validate weight variable
  wt <- gsub("_z", "", paste0("w_treat_", shockvar))
  if (!wt %in% names(df)) {
    stop("Weight variable '", wt, "' not found in data. Check shockvar naming convention.")
  }
    
  # Specify models
  f_in <- as.formula(paste0(outcome, "~ reg_loser_50 * (", shockvar, "*", hetvars, covars, ")", "|", fe))
  f_wt <- as.formula(paste0(outcome, "~ reg_loser_50 *", shockvar, "*", hetvars, "|", fe))
  
  # Covariate-adjusted
  m <- feols(fml = f_in, data = df, vcov = ~cty_reg)
  ame <- avg_slopes(m, variables = shockvar, by = "reg_loser_50") |> data.frame()
  ame_h <- slopes(m, variables = shockvar, by = "reg_loser_50", hypothesis = "b2 - b1 = 0") |> data.frame()
  
  # Only CBPS
  m_wt <- feols(fml = f_wt, data = df, vcov = ~cty_reg, weights = df[[wt]])
  ame_wt <- avg_slopes(m_wt, variables = shockvar, by = "reg_loser_50") |> data.frame()
  ame_wt.h <- slopes(m_wt, variables = shockvar, by = "reg_loser_50", hypothesis = "b2 - b1 = 0") |> data.frame()
  
  # Doubly robust
  m_both <- feols(fml = f_in, data = df, vcov = ~cty_reg, weights = df[[wt]])
  ame_both <- avg_slopes(m_both, variables = shockvar, by = "reg_loser_50") |> data.frame()
  ame_both.h <- slopes(m_both, variables = shockvar, by = "reg_loser_50", hypothesis = "b2 - b1 = 0") |> data.frame()
  
  l_covar <- list(ame, ame_h)
  l_cbps <- list(ame_wt, ame_wt.h)
  l_dr <- list(ame_both, ame_both.h)
  
  return(list(`Covariates` = l_covar, `CBPS` = l_cbps, `CBPS + Covariates` = l_dr))
}
