# Self-written functions
########################
mrln_to_num <- function(x) {
  # This function turns Likert scales into z-standardized numeric variables.
  x = case_when(
    str_detect(x, "1|Aldrig") ~ 1,
    str_detect(x, "2|Sjældent") ~ 2,
    str_detect(x, "3|En gang i mellem") ~ 3,
    str_detect(x, "4|Ofte") ~ 4,
    str_detect(x, "5|Meget ofte") ~ 5,
    x == "Ved ikke" ~ as.numeric(NA),
    TRUE ~ as.numeric(NA))
  
  return(x)
}

mrln_std <- function(x, gewicht) {
  # z-standardize variable according to 
  # mean and SD in the whole sample.
  
  x <- (x - wtd.mean(x, weights = gewicht)) / sqrt(wtd.var(x, weights = gewicht))
  return(x)
}

mrln_cntr_std <- function(x, gewicht, cntr) {
  # z-standardize variable according to
  # mean and SD in the control group
  
  cntr_mean <- wtd.mean(x[cntr], 
                        weights = gewicht[cntr])
  cntr_sd <- sqrt(wtd.var(x[cntr], 
                          weights = gewicht[cntr]))
  
  x <- (x - cntr_mean) / cntr_sd
  return(x)
}

mrln_estimtr <- function(y, pred, add_cntr = "", daten) {
  # This function implements the two versions of 
  # the standard OLS model for all analyses.
  
  ## Generate formula object and define controls
  f <- as.formula(
    paste(y, "~", pred, 
          " + Alder + Region + koen + educ + hh_size + empl", 
          add_cntr, sep = " "))
  
  ## Estimate model
  mod <- lm_robust(formula = f, data = daten, weights = wgt)
  
  return(mod)
}

mrln_estimtr_noWeights <- function(y, pred, add_cntr = "", daten) {
  # This function implements the two versions of 
  # the un-weighted OLS model as robustness check.
  
  ## Generate formula object and define controls
  f <- as.formula(
    paste(y, "~", pred, 
          " + Alder + Region + koen + educ + hh_size + empl", 
          add_cntr, sep = " "))
  
  ## Estimate model
  mod <- lm_robust(formula = f, data = daten, se_type = "classical")
  
  return(mod)
}

mrln_Likert <- function(x) {
  # This function plots Likert scales based on:
  # https://scisley.github.io/articles/ggplot-likert-plot/
  
  plotdaten <- x %>% mutate(
    text = paste0(formatC(100 * perc, format = "f", digits = 0), "%"),
    cs = cumsum(perc),
    offset = sum(perc[1:(floor(n()/2))]) + (n() %% 2)*0.5*(perc[ceiling(n()/2)]),
    xmax = -offset + cs,
    xmin = xmax - perc) %>%
    ungroup()
  plotdaten <- plotdaten %>%
    left_join(plotdaten %>%
                group_by(Variable) %>%
                dplyr::summarize(max.xmax = max(xmax)) %>%
                mutate(r = row_number(max.xmax)),
              by = "Variable") %>%
    arrange(desc(r)) %>%
    mutate(ymin = r - (1 - 0.2) / 2,
           ymax = r + (1 - 0.2) / 2)
  
  ggplot(plotdaten) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Answer)) +
    geom_vline(xintercept = 0, color = "#9a9a9a") +
    geom_text(aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=text), size = 3) +
    scale_y_continuous("", breaks = 1:n_distinct(plotdaten$Variable),
                       labels=rev(plotdaten %>% distinct(Variable) %>% .$Variable)) +
    scale_fill_brewer("", palette = "RdBu", 
                      breaks=c('Strongly disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly agree')) +
    theme_classic() +
    labs(x = "", y = "") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text = element_text(colour = "black"))
}

mrln_effects <- function(x, wer = TRUE, NoAxis = FALSE, fxscale = TRUE) {
  # This function creates an effects plot after having used lm_robust()
  
  x <- x %>% mutate( # Get 90% CI
    min90 = estimate - qt(0.95, df = df) * std.error,
    max90 = estimate + qt(0.95, df = df) * std.error)
  
  if (wer) {
    plot <- ggplot(data = x, aes(y = estimate, x = term, 
                                 ymin = conf.low, ymax = conf.high, shape = Who)) +
      geom_hline(yintercept = 0, color = "#901A1E") +
      geom_linerange(size = 1.3, position = position_dodge(width = 0.5)) +
      geom_linerange(aes(ymin = min90, ymax = max90), size = 1.3, color = "#808080",
                     position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5)) +
      scale_shape_manual("", values = c(21, 19)) + 
      coord_flip() +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(colour = "black"))
  } else {
    plot <- ggplot(data = x, aes(y = estimate, x = term, 
                                 ymin = conf.low, ymax = conf.high)) +
      geom_hline(yintercept = 0, color = "#901A1E") +
      geom_linerange(size = 1.3, position = position_dodge(width = 0.5)) +
      geom_linerange(aes(ymin = min90, ymax = max90), size = 1.3, color = "#808080",
                     position = position_dodge(width = 0.5)) +
      geom_point(size = 4, position = position_dodge(width = 0.5)) +
      coord_flip() +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(colour = "black"))
  }
  if (NoAxis) {
    plot <- plot + 
      theme(axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(colour = "black"))
  }
  if (fxscale) {
    plot <- plot + scale_y_continuous(limits = c(-0.5, 0.5))
  }
  
  return(plot)
}

#' Compute empirical cumulative distribution
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualisation of distribution. Compared to other visualisations that rely on
#' density (like [geom_histogram()]), the ECDF doesn't require any
#' tuning parameters and handles both continuous and categorical variables.
#' The downside is that it requires more training to accurately interpret,
#' and the underlying visual tasks are somewhat more challenging.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If `TRUE`, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @section Computed variables:
#' \describe{
#'   \item{x}{x in data}
#'   \item{y}{cumulative density corresponding x}
#' }
#' @export
#' @examples
#' df <- data.frame(
#'   x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
#'   g = gl(2, 100)
#' )
#' ggplot(df, aes(x)) + stat_ecdf(geom = "step")
#'
#' # Don't go to positive/negative infinity
#' ggplot(df, aes(x)) + stat_ecdf(geom = "step", pad = FALSE)
#'
#' # Multiple ECDFs
#' ggplot(df, aes(x, colour = g)) + stat_ecdf()
stat_ecdf <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      weight =  NULL, 
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      weight = weight,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' 

StatEcdf <- ggproto("StatEcdf", Stat,
                    compute_group = function(data, scales, weight, n = NULL, pad = TRUE) {
                      # If n is NULL, use raw values; otherwise interpolate
                      if (is.null(n)) {
                        x <- unique(data$x)
                      } else {
                        x <- seq(min(data$x), max(data$x), length.out = n)
                      }
                      
                      if (pad) {
                        x <- c(-Inf, x, Inf)
                      }
                      y <- ewcdf(data$x, weights=data$weight/sum(data$weight))(x)
                      
                      data.frame(x = x, y = y)
                    },
                    
                    default_aes = aes(y = stat(y)),
                    
                    required_aes = c("x")
)


# Two functions to post-process rdrobust() objects to make a table using modelsummary
# taken from https://stackoverflow.com/questions/67823100/print-tables-with-3-regression-output-models-from-rdrobust
tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1],
    std.error = model$se[, 1],
    p.value = model$pv[, 1]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Bandwidth = round(model$bws[1,1], digits = 2),
    "Effective-cases" = paste(model$N_h[1], " vs ", model$N_h[2], sep = " "),
    "Overall-cases" = paste(model$N[1], " vs ", model$N[2], sep = " ") 
  )
  ret
}