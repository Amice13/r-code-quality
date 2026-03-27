#' Conduct Sensitivity Analysis for Unobserved Confounding
#'
#' Performs sensitivity analysis using sensemakr package to assess robustness
#' to potential unobserved confounders.
#'
#' @param estimate_in Numeric. Treatment effect estimate
#' @param se_in Numeric. Standard error of treatment effect
#' @param benchmark_fml Character. Formula name for benchmark covariate
#' @param benchmark_label Character. Label for benchmark covariate in plot
#' @param senseof Character. Identifier for sensitivity analysis type
#' @param fig_label Character. Figure label
#' @param D.X Model object. Auxiliary regression of treatment on covariates
#' @param D.X.dof Numeric. Degrees of freedom for D.X model
#' @param ols_cl Coefficient table from main OLS model
#' @param ols_df Numeric. Degrees of freedom for main OLS model
#' @param shockvar Character. Shock variable name for file naming (default: "")
#'
#' @return Invisible. Function called to create sensitivity plots
#'
#' @details
#' Function iterates through kd values (0 to 100 by 0.1) to find the point
#' where adjusted t-statistic equals 1.96 (critical value).
#' Saves PNG plots showing sensitivity of t-value and estimate.
#'
#' @note Creates files in Output/figures/ directory
est_sensitivity <- function(estimate_in, se_in, benchmark_fml, benchmark_label, senseof, fig_label, D.X, D.X.dof, ols_cl, ols_df, shockvar = "") {
  
  r2dxj.x <- partial_r2(t_statistic = D.X$coeftable[benchmark_fml, 3], dof = D.X.dof)
  r2yxj.dx <- partial_r2(t_statistic = ols_cl[benchmark_fml, 3], dof = ols_df)
  
  kd <- seq(0, 100, by = 0.01)
  target_t <- 1.96
  tolerance <- 0.001
  adjusted_t <- Inf
  selected_kd <- NA
  
  for (k in kd) {
    sense.out <- sensemakr(
      estimate = estimate_in,
      se = se_in,
      r2dxj.x = r2dxj.x,
      r2yxj.dx = r2yxj.dx,
      dof = ols_df,
      kd = k
    )
    
    # Extract adjusted t-statistic from sensemakr object
    # The adjusted estimate and SE are in the sensitivity_stats component
    adjusted_estimate <- sense.out$bounds$adjusted_estimate
    adjusted_se <- sense.out$bounds$adjusted_se
    
    if (!is.null(adjusted_estimate) && !is.null(adjusted_se) && adjusted_se != 0) {
      adjusted_t <- adjusted_estimate / adjusted_se
      
      if (abs(adjusted_t - target_t) < tolerance) {
        selected_kd <- k
        break
      }
    }
  }
  sense.out$bounds$bound_label <- paste0(round(selected_kd, 1), "x ", benchmark_label)
  pdf(here("Output", "figures", paste0("fig_", fig_label, "_wrp_sense_", senseof, "_", tolower(benchmark_label), "_", shockvar, ".pdf")),
      width = 6.5, height = 3, pointsize = 8
  )
  par(mfrow = c(1, 2))
  plot(sense.out, sensitivity.of = "t-value", main = "Sensitivity of t-value", lim = .008, lim.y = .008, label.bump.x = 0.001, label.bump.y = 0.0005, round = 3)
  plot(sense.out, main = "Sensitivity of Estimate", lim = .008, lim.y = .008, label.bump.x = 0.001, label.bump.y = 0.0005, round = 3)
  dev.off()
}
