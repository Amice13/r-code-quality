#######
#######
####### Replication Data for: The Differential Impact of the Hong Kong National Security Law 
####### on Political Sensitivity Bias in Opinion Polls.
####### This file modifies the original plotting functions in the synthdid package to manually include CIs.
####### Last Updated: August. 2023
#######
#######

source("custom_fuctions/solver.r")

#' Plots unit by unit difference-in-differences. Dot size indicates the weights omega_i
#' used in the average that yields our treatment effect estimate. 
#' This estimate and endpoints of a 95% CI are plotted as horizontal lines.
#' Requires ggplot2
#' @param estimates as output by synthdid_estimate. Can be a single one or a list of them.
#' @param negligible.threshold Unit weight threshold below which units are plotted as small, transparent xs instead of circles. Defaults to .001.
#' @param negligible.alpha Determines transparency of those xs.
#' @param se.method the method used to calculate standard errors for the CI. See vcov.synthdid_estimate. 
#'        Defaults to 'jackknife' for speed. If 'none', don't plot a CI.
#' @param units a list of control units --- elements of rownames(Y) --- to plot differences for. Defaults to NULL, meaning all of them.
#' @export synthdid_units_plot
synthdid_units_plot = function(estimates, negligible.threshold = .001, negligible.alpha = .3, se.method='jackknife', 
                               se.manual=NULL, units=NULL) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    .ignore <- tryCatch(attachNamespace("ggplot2"), error = function(e) e)
  } else {
    stop("Plotting requires the package `ggplot2`. Install it to use this function.")
  }
  if (class(estimates) == 'synthdid_estimate') { estimates = list(estimates) }
  if (is.null(names(estimates))) { names(estimates) = sprintf('estimate %d', 1:length(estimates)) }
  plot.data = do.call(rbind, lapply(1:length(estimates), function(ee) {
    estimate = estimates[[ee]]
    setup = attr(estimate, 'setup')
    weights = attr(estimate, 'weights')
    Y = setup$Y - contract3(setup$X, weights$beta)
    N0 = setup$N0; N1 = nrow(Y) - N0
    T0 = setup$T0; T1 = ncol(Y) - T0
    
    lambda.pre = c(weights$lambda, rep(0, T1))
    lambda.post = c(rep(0, T0), rep(1 / T1, T1))
    omega.control = c(weights$omega, rep(0, N1))
    omega.treat = c(rep(0, N0), rep(1 / N1, N1))
    difs = as.vector(t(omega.treat) %*% Y %*% (lambda.post - lambda.pre)) - as.vector(Y[1:N0, ] %*% (lambda.post - lambda.pre))
    se = if (se.method == 'none') { NA } else { if (!is.null(se.manual)){
      se.manual[[ee]]
    } else{
      sqrt(vcov(estimate, method = se.method))
    }
      }
    include.units = if(is.null(units)) { 1:N0 } else { which(rownames(Y)[1:N0] %in% units) }
    data.frame(y = difs[include.units], unit = rownames(Y)[include.units], weight = omega.control[include.units],
               estimate = c(estimate), se = se, estimator = names(estimates)[[ee]])
  }))
  p = ggplot(plot.data) +
    geom_point(aes(x = unit, y = y, size = weight), data = plot.data[plot.data$weight > negligible.threshold, ]) +
    geom_point(aes(x = unit, y = y, size = weight), data = plot.data[plot.data$weight <= negligible.threshold, ], alpha = negligible.alpha, shape = 4, show.legend = FALSE) +
    geom_hline(aes(yintercept = estimate), size = .75)
  if (!all(is.na(plot.data$se))) {
    p = p + geom_hline(aes(yintercept = estimate - 1.96 * se), size = .5, alpha = .5) +
      geom_hline(aes(yintercept = estimate + 1.96 * se), size = .5, alpha = .5)
  }
  p + facet_grid(. ~ estimator) + xlab('') + ylab('') + guides(shape = 'none') +
    theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


