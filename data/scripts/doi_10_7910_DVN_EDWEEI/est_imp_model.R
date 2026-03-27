#' Estimate Panel Models on Multiple Imputed Data
#'
#' Estimates fixed-effects panel data models across multiple imputed datasets
#' and combines results using multiple imputation inference rules. Designed for
#' analyzing climate shock effects on survey outcomes with missing data.
#'
#' @param formula A formula object specifying the model to estimate
#' @param coefvector Character vector of coefficient names to extract from results
#' @param lagged Logical. If TRUE, creates lagged treatment variables (default: FALSE)
#' @param subset Character. Subset data by respondent type: "all", "skeptics", 
#'   "believers", or "undecideds" (default: "all")
#' @param ame Logical. If TRUE, calculates average marginal effects for 
#'   interaction terms (default: FALSE)
#' @param treatvar Character. Name of treatment variable for AME calculation 
#'   (required if ame = TRUE)
#' @param modvar Character. Name of moderator variable for AME calculation 
#'   (required if ame = TRUE)
#' @param a_out Amelia imputation object containing multiple imputed datasets.
#'   Defaults to global 'a_out' variable from parent environment
#'
#' @return If ame = FALSE: A list with class "modelsummary_list" containing:
#'   \describe{
#'     \item{tidy}{Data frame with coefficients, standard errors, t-statistics, p-values}
#'     \item{glance}{Data frame with model fit statistics (N, adj. R-squared, DOF)}
#'   }
#'   If ame = TRUE: A list containing the above model results plus a data frame
#'   of average marginal effects by damage level.
#'
#' @details 
#' The function:
#' \itemize{
#'   \item Estimates two-way fixed effects models (individual + time) using plm
#'   \item Uses Newey-West heteroskedasticity and autocorrelation robust standard errors
#'   \item Combines results across imputations using Rubin's rules via mi.meld()
#'   \item Creates interaction terms for fire*damage and heat*damage
#'   \item Optionally calculates average marginal effects for moderated relationships
#' }
#'
#' @note 
#' \itemize{
#'   \item Requires 'caseid' and 'year' variables for panel structure
#'   \item Expects binary damage_bin and climate shock variables (fire, tanom)
#'   \item For subset analysis, expects skeptic/undecided indicator variables
#'   \item AME calculation assumes interaction between treatvar and modvar exists
#' }
#'
#' @examples
#' \dontrun{
#' # Basic model estimation
#' formula <- gw_binary ~ fire + damage_bin + covariates
#' coefs <- c("fire", "damage_bin", "edu_2", "dem")
#' results <- est_imp_model(formula, coefs)
#' 
#' # With interaction and AME
#' formula_int <- gw_binary ~ fire * damage_bin + covariates  
#' coefs_int <- c("fire", "damage_bin", "fire:damage_bin", "edu_2")
#' results_ame <- est_imp_model(formula_int, coefs_int, ame = TRUE, 
#'                              treatvar = "fire", modvar = "damage_bin")
#' }
#'
est_imp_model <- function(formula, coefvector, lagged = FALSE, subset = "all", ame = FALSE, treatvar, modvar, a_out = get("a_out", envir = parent.frame())) {
  require(plm)
  require(lmtest)
  require(broom)
  require(marginaleffects)
  
  # Initialize containers for results across imputations
  b_out <- NULL
  se_out <- NULL
  r2_out <- NULL
  ame_out <- list()
  
  # Loop through each imputed dataset
  for(i in 1:a_out$m) {
    
    # Extract and prepare imputed dataset
    df_ordered <- a_out$imputations[[i]] %>%
      arrange(caseid, year)
    
    # Create interaction terms for climate shock analysis
    df_ordered$interact_fire <- df_ordered$fire * df_ordered$damage_bin
    df_ordered$interact_heat <- df_ordered$tanom * df_ordered$damage_bin
    
    # Apply respondent subset filters based on climate beliefs
    if (subset == "skeptics") {
      df_ordered <- subset(df_ordered, skeptic == 1)
    }
    if (subset == "believers") {
      df_ordered <- subset(df_ordered, skeptic == 0 & undecided == 0)
      
    }
    if (subset == "undecideds") {
      df_ordered <- subset(df_ordered, undecided == 1)
      
    }
    
    # Create lagged treatment variables
    if (isTRUE(lagged)) {
      df_ordered <- df_ordered %>%
        group_by(caseid) %>%
        mutate(
          fire_lead = dplyr::lead(fire),
          heat_lead = dplyr::lead(tanom)
        )
    }
    plm_out <- plm::plm(formula = formula, data = df_ordered, index = c("caseid", "year"), model = "within", effect = "twoways")
    plm_vcov <- plm::vcovNW(plm_out, type = "HC1")
    if (isTRUE(ame)) {
      beta <- coef(plm_out)
      ame_fire <- beta[treatvar]
      se_fire <- sqrt(plm_vcov[treatvar, treatvar])
      interacted <- paste0(treatvar, ":", modvar)
      ame_fire_damage <- beta[treatvar] + beta[interacted]
      se_fire_damage <- sqrt(plm_vcov[treatvar, treatvar] + plm_vcov[interacted, interacted] + 2 * plm_vcov[interacted, treatvar])
      ame_out[[i]] <- data.frame(
        damage_bin = c(1, 0),
        estimate = c(ame_fire_damage, ame_fire),
        std.error = c(se_fire_damage, se_fire)
      )
      }
    
    # Extract coefficients and standard errors with robust variance
    plm_clustered <- coeftest(plm_out, vcov. = plm_vcov)
    plm_clustered <- tidy(plm_clustered)
    b_out <- rbind(b_out, plm_clustered$estimate)
    se_out <- rbind(se_out, plm_clustered$std.error)
    #Get adjusted R2
    tss <- sum((df_ordered$gw_binary - mean(df_ordered$gw_binary))^2)
    r2_out <- 1 - ((sum(plm_out$residuals ^ 2) / tss) * (nrow(df_ordered) - 1)) / (plm_out$df.residual) 
  }
  combined_results <- mi.meld(q = b_out, se = se_out)
  climate_dof <- plm_out$df.residual
  if (isTRUE(ame)) {
    ame_df <- do.call(rbind, ame_out)
    ame_df <- ame_df %>%
      data.frame() %>%
      group_by(damage_bin) %>%
      summarize(estimate = mean(estimate), std.error = mean(std.error))
    }
  imp_climate <- data.frame(
    term = coefvector,
    estimate = as.vector(combined_results$q.mi),
    std.error = as.vector(combined_results$se.mi)
  )
  imp_climate$statistic <- with(imp_climate, estimate / std.error)
  imp_climate$p.value <- with(imp_climate, pt(abs(statistic), df = plm_out$df.residual, lower.tail = FALSE) * 2)
  imp_climate
  climate_stat <- data.frame("N" = nobs(plm_out), "adj.r.squared" = mean(r2_out), "dof" = climate_dof)
  imp_climate <- list(tidy = imp_climate, glance = climate_stat)
  class(imp_climate) <- "modelsummary_list"
  
  if (isTRUE(ame)) {
    return(list(imp_climate, ame_df))
  } else{
    return(imp_climate)
  }
}
