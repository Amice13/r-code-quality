source("https://raw.githubusercontent.com/cran/cjoint/master/R/cjoint.R")

amce2 <- function (formula, data, design = "uniform", respondent.varying = NULL, 
                   subset = NULL, respondent.id = NULL, cluster = TRUE, na.ignore = FALSE, 
                   weights = NULL, baselines = NULL) 
{
  formula_user <- formula
  formula_char_user <- all.vars(formula)
  user_names <- list()
  user_levels <- list()
  for (char in formula_char_user) {
    user_names[[clean.names(char)]] <- char
    if (class(data[[char]]) == "factor") {
      old_names <- names(user_levels)
      user_levels <- c(user_levels, levels(data[[char]]))
      new_names <- sapply(clean.names(levels(data[[char]])), 
                          function(x) paste(clean.names(char), x, sep = ""))
      names(user_levels) <- c(old_names, new_names)
    }
  }
  formula_char <- clean.names(formula_char_user)
  if (length(unique(formula_char)) != length(formula_char)) {
    stop("Error: Variable names must be unique when whitespace and meta-characters are removed. Please rename.")
  }
  y_var <- clean.names(formula_char_user[1])
  orig_effects <- clean.names(attr(terms(formula_user), "term.labels"))
  orig_effects <- c(sort(orig_effects[!grepl(":", orig_effects)]), 
                    orig_effects[grepl(":", orig_effects)])
  vars_plus <- paste(orig_effects, collapse = " + ")
  form <- formula(paste(c(y_var, vars_plus), collapse = " ~ "))
  orig_effects <- attr(terms(form), "term.labels")
  full_terms <- attr(terms(formula(paste(y_var, paste(sapply(orig_effects, 
                                                             function(x) gsub(":", "*", x)), collapse = " + "), sep = " ~ "))), 
                     "term.labels")
  missing_terms <- full_terms[!is.element(full_terms, orig_effects)]
  if (length(missing_terms > 0)) {
    orig_effects <- c(orig_effects, missing_terms)
    warning("Missing base terms for interactions added to formula")
  }
  orig_effects <- c(sort(orig_effects[!grepl(":", orig_effects)]), 
                    orig_effects[grepl(":", orig_effects)])
  vars_plus <- paste(orig_effects, collapse = " + ")
  form <- formula(paste(c(y_var, vars_plus), collapse = "~"))
  orig_effects <- clean.names(attr(terms(form), "term.labels"))
  unique_vars <- clean.names(rownames(attr(terms(form), "factor"))[-1])
  respondent_vars <- clean.names(respondent.varying)
  profile_vars <- unique_vars[!is.element(unique_vars, respondent_vars)]
  if (length(respondent_vars) > 0) {
    profile_effects <- unlist(sapply(orig_effects, USE.NAMES = F, 
                                     function(x) {
                                       y <- strsplit(x, ":")[[1]]
                                       if (!any(is.element(y, respondent_vars))) 
                                         x
                                     }))
    resp_only <- unlist(sapply(orig_effects, USE.NAMES = F, 
                               function(x) {
                                 y <- strsplit(x, ":")[[1]]
                                 if (any(is.element(y, respondent_vars))) 
                                   x
                               }))
    resp_mod <- unlist(sapply(resp_only, USE.NAMES = F, function(x) {
      y <- strsplit(x, ":")[[1]]
      vars <- y[!is.element(y, respondent_vars)]
      if (length(vars) > 0) 
        paste(vars, collapse = ":")
    }))
    resp_effects <- c(resp_mod, resp_only)
  }
  else {
    profile_effects <- orig_effects
    resp_effects <- NULL
  }
  if (!is.null(respondent.id)) 
    respondent.id <- clean.names(respondent.id)
  if (!is.null(weights)) 
    weights <- clean.names(weights)
  if (!is.null(baselines)) {
    names(baselines) <- clean.names(names(baselines))
    baselines <- lapply(baselines, function(x) clean.names(x))
  }
  colnames(data) <- clean.names(colnames(data))
  var_to_check_levels <- unique(c(formula_char_user, respondent_vars, 
                                  profile_vars, profile_effects, orig_effects))
  for (var in var_to_check_levels) {
    if (class(data[[var]]) == "factor") {
      clean.labels <- clean.names(levels(data[[var]]))
      if (length(unique(clean.labels)) != length(clean.labels)) {
        stop(paste("Error: levels of variable", var, 
                   "are not unique when whitespace and meta-characters are removed. Please rename."))
      }
      data[[var]] <- factor(data[[var]], levels = levels(data[[var]]), 
                            labels = clean.names(levels(data[[var]])))
    }
  }
  for (var in formula_char) {
    if (!(var %in% colnames(data))) {
      stop(paste("Error:", var, "not in 'data'"))
    }
  }
  for (var in profile_vars) {
    if (class(data[[var]]) != "factor") {
      data[[var]] <- as.factor(data[[var]])
      warning(paste(c("Warning: ", var, " changed to factor"), 
                    collapse = ""))
    }
  }
  if (na.ignore == FALSE) {
    for (variab in formula_char) {
      if (sum(is.na(data[[variab]])) != 0) {
        stop(paste("Error:", variab, "has missing values in 'data'"))
      }
    }
  }
  if (!is.null(respondent_vars)) {
    for (var in respondent_vars) {
      found <- 0
      for (formulavars in formula_char) {
        if (var == formulavars) {
          found <- 1
        }
      }
      if (found == 0) {
        stop(paste("Error:", var, "is specified in respondent.varying, but is not in the formula"))
      }
    }
  }
  if (!is.numeric(data[[y_var]]) & !is.integer(data[[y_var]])) {
    stop(paste("Error:", y_var, "is not numeric or integer"))
  }
  if (!is.null(baselines)) {
    for (var in names(baselines)) {
      if (!(baselines[[var]] %in% data[[var]])) {
        stop(paste("Error: user supplied baseline", baselines[[var]], 
                   "is not a level of", var))
      }
    }
  }
  if (is.null(respondent.id) & cluster == TRUE) {
    warning("respondent.id is NULL - setting cluster to FALSE. Please specify a respondent.id variable if you want to estimate clustered standard errors")
    cluster <- FALSE
  }
  if (cluster == TRUE) {
    if (is.null(respondent.id)) {
      stop("Error: Must specify a respondent.id if cluster = TRUE")
    }
    else if (!(respondent.id %in% colnames(data))) {
      stop("Error: respondent.id not in data")
    }
  }
  if (!is.null(weights)) {
    if (!weights %in% colnames(data)) {
      stop("Error: weights not in data")
    }
    if (length(unique(data[[weights]])) == 1) {
      weights <- NULL
    }
  }
  if (class(design) == "conjointDesign") {
    names(dimnames(design$J)) <- clean.names(names(dimnames(design$J)))
    dimnames(design$J) <- lapply(dimnames(design$J), function(x) clean.names(x))
    names(design$depend) <- clean.names(names(design$depend))
    design$depend <- lapply(design$depend, function(x) clean.names(x))
    for (eff in profile_vars) {
      if (!(eff %in% names(dimnames(design$J)))) {
        stop(paste("Error:", eff, "not in 'design' object"))
      }
    }
    for (eff in names(dimnames(design$J))) {
      if (!(eff %in% colnames(data))) {
        stop(paste("Error: attribute", eff, "in 'design' object is not in 'data'"))
      }
      else {
        for (lev in clean.names(levels(as.factor(data[[eff]])))) {
          if (!(lev %in% dimnames(design$J)[[eff]])) {
            stop(paste("Error: factor level", lev, "of attribute", 
                       eff, "not in 'design' object"))
          }
        }
      }
    }
    depend_to_check <- NULL
    for (var in names(design$depend)) {
      depend_to_check <- c(depend_to_check, design$depend[[var]])
    }
    depend_to_check <- depend_to_check[!depend_to_check %in% 
                                         var_to_check_levels]
    for (var in depend_to_check) {
      if (class(data[[var]]) == "factor") {
        clean.labels <- clean.names(levels(data[[var]]))
        if (length(unique(clean.labels)) != length(clean.labels)) {
          stop(paste("Error: levels of variable", var, 
                     "used as a dependency, are not unique when whitespace and meta-characters are removed. Please rename."))
        }
        data[[var]] <- factor(data[[var]], levels = levels(data[[var]]), 
                              labels = clean.names(levels(data[[var]])))
      }
    }
  }
  else if (design == "uniform") {
    design <- list()
    design.dim <- vector(length = length(profile_vars))
    dim_list <- list()
    for (i in 1:length(profile_vars)) {
      dim_list[[i]] <- levels(factor(data[[profile_vars[i]]]))
      design.dim[i] <- length(dim_list[[i]])
    }
    names(dim_list) <- profile_vars
    design$J <- array(1/prod(design.dim), dim = design.dim, 
                      dimnames = dim_list)
    design$depend <- compute_dependencies(design$J)
  }
  else {
    stop("Design object must be a valid character string 'uniform' or a conjointDesign object")
  }
  if (is.null(subset)) {
    data <- data
  }
  else {
    if (class(subset) == "logical") {
      if (length(subset) == nrow(data)) {
        data <- subset(data, subset)
      }
      else {
        warning("Warning: invalid argument to 'subset' - must be the same length as the number of rows in data")
      }
    }
    else {
      warning("Warning: invalid argument to 'subset' - must be a logical")
    }
  }
  if (!is.null(baselines)) {
    for (var in names(baselines)) {
      data[[var]] <- factor(data[[var]])
      data[[var]] <- relevel(data[[var]], baselines[[var]])
    }
  }
  if (any(profile_vars %in% names(design$depend))) {
    depend_vars <- c()
    for (eff in profile_vars[profile_vars %in% names(design$depend)]) {
      inter <- c()
      eff_all <- grep(paste(c(":", eff, "(?!_)", "|", eff, 
                              ":(?<!_)"), collapse = ""), orig_effects, value = T, 
                      perl = T)
      if (length(eff_all) > 0) {
        eff_all <- sapply(strsplit(eff_all, ":"), function(x) paste(sort(x), 
                                                                    collapse = "*"))
      }
      eff_all <- c(eff, eff_all)
      inter <- sapply(eff_all, USE.NAMES = F, function(x) {
        T_r <- design$depend[[eff]]
        for (t in T_r) {
          data[[t]] <- as.factor(data[[t]])
        }
        T_r_d <- c(x, T_r)
        paste(T_r_d, collapse = "*")
      })
      depend_vars <- c(depend_vars, inter)
    }
    depend_vars <- unique(depend_vars)
    form_full <- formula(paste(c(form, depend_vars), collapse = " + "))
  }
  else {
    form_full <- form
  }
  all_run_vars <- attr(terms(form_full), "term.labels")
  all_run_vars <- c(sort(all_run_vars[!grepl(":", all_run_vars)]), 
                    all_run_vars[grepl(":", all_run_vars)])
  vars_plus <- paste(all_run_vars, collapse = " + ")
  form_full <- formula(paste(c(y_var, vars_plus), collapse = "~"))
  all_run_vars <- attr(terms(form_full), "term.labels")
  if (length(respondent_vars) > 0) {
    prof_only <- unlist(sapply(all_run_vars, function(x) {
      y <- clean.names(strsplit(x, ":")[[1]])
      if (!any(is.element(y, respondent_vars))) 
        x
    }))
    prof_only_plus <- paste(prof_only, collapse = " + ")
    form_prof <- paste(all.vars(form_full)[1], prof_only_plus, 
                       sep = " ~ ")
    form_prof <- formula(form_prof)
  }
  else {
    form_prof <- form_full
  }
  all_prof <- clean.names(attr(terms(form_prof), "term.labels"))
  all_run_vars <- clean.names(all_run_vars)
  if (any(!is.element(all_prof, all_run_vars))) {
    warning("Warning: mismatch of term names between full formula and profile formula")
  }
  if (is.null(weights)) {
    lin.mod.prof <- lm(form_prof, data = data)
    if (length(respondent_vars) > 0) {
      lin.mod.full <- lm(form_full, data = data)
    }
    else {
      lin.mod.full <- NULL
    }
  }
  else {
    if (cluster) {
      out.design <- svydesign(ids = data[[respondent.id]], 
                              weights = data[[weights]], data = data)
    }
    else {
      out.design <- svydesign(ids = ~0, weights = data[[weights]], 
                              data = data)
    }
    lin.mod.prof <- svyglm(form_prof, data = data, design = out.design)
    if (length(respondent_vars) > 0) {
      lin.mod.full <- svyglm(form_full, data = data, design = out.design)
    }
    else {
      lin.mod.full <- NULL
    }
  }
  sample_size_prof <- length(lin.mod.prof$residuals)
  if (length(respondent.varying) > 0) {
    sample_size_full <- length(lin.mod.full$residuals)
  }
  else {
    sample_size_full <- NULL
  }
  if (is.null(weights) & cluster == TRUE) {
    vcov_mat_prof <- cluster_se_glm(lin.mod.prof, data[[respondent.id]])
    if (length(respondent.varying) > 0) {
      vcov_mat_full <- cluster_se_glm(lin.mod.full, data[[respondent.id]])
    }
    else {
      vcov_mat_full <- NULL
    }
  }
  else if (!is.null(weights)) {
    vcov_mat_prof <- vcov(lin.mod.prof)
    if (length(respondent_vars) > 0) {
      vcov_mat_full <- vcov(lin.mod.full)
    }
    else {
      vcov_mat_full <- NULL
    }
  }
  else {
    vcov_mat_prof <- vcovHC(lin.mod.prof, type = "HC2")
    if (length(respondent.varying) > 0) {
      vcov_mat_full <- vcovHC(lin.mod.full, type = "HC2")
    }
    else {
      vcov_mat_full <- NULL
    }
  }
  fix.vcov <- function(varprob, vcov) {
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop("Matrix package needed for this function to work. Please install it.", 
           call. = FALSE)
    }
    varprob2 <- Matrix(varprob, sparse = TRUE)
    vcov2 <- Matrix(vcov, sparse = TRUE)
    fix1 <- varprob2 %*% vcov2 + t(varprob2 %*% vcov2) + 
      vcov2
    vcov_vector <- matrix(as.vector(vcov2), ncol = 1, nrow = length(as.vector(vcov2)))
    weighted_covs <- kronecker(varprob2, varprob2) %*% vcov_vector
    weighted_covs <- matrix(weighted_covs, nrow = nrow(vcov), 
                            ncol = ncol(vcov))
    fix2 <- fix1 + weighted_covs
    out <- matrix(fix2, ncol = ncol(vcov), nrow = nrow(vcov))
    colnames(out) <- rownames(out) <- rownames(vcov)
    return(out)
  }
  J_baseline <- NULL
  J_effect <- NULL
  J_call <- Quote(design$J[])
  J_call <- J_call[c(1, 2, rep(3, length(dim(design$J))))]
  warn_i <- 0
  estimates <- list()
  profile_effects <- c(sort(profile_effects[!grepl(":", profile_effects)]), 
                       profile_effects[grepl(":", profile_effects)])
  covariance_list <- list()
  varprob_mat <- matrix(0, nrow(vcov_mat_prof), ncol(vcov_mat_prof))
  colnames(varprob_mat) <- rownames(varprob_mat) <- colnames(vcov_mat_prof)
  for (i in 1:length(profile_effects)) {
    substrings <- strsplit(profile_effects[i], "[:*]", perl = TRUE)[[1]]
    all_levels <- list()
    all_levels_coefs <- list()
    for (effect in substrings) {
      all_levels[[effect]] <- levels(data[[effect]])[-1]
      all_levels_coefs[[effect]] <- sapply(all_levels[[effect]], 
                                           function(x) {
                                             paste(c(effect, x), collapse = "")
                                           })
    }
    levels <- expand.grid(all_levels, stringsAsFactors = FALSE)
    levels <- cbind(apply(levels, 1, function(x) paste(x, 
                                                       collapse = ":")), levels)
    colnames(levels) <- c("name", substrings)
    coefs <- expand.grid(all_levels_coefs, stringsAsFactors = FALSE)
    coefs <- apply(coefs, 1, function(x) paste(x, collapse = ":"))
    results <- matrix(nrow = 2, ncol = nrow(levels))
    if (length(substrings) > 1) {
      rownames(results) <- c("ACIE", "Std. Error")
    }
    else {
      rownames(results) <- c("AMCE", "Std. Error")
    }
    colnames(results) <- coefs
    results[2, ] <- NA
    all_depends <- unlist(sapply(all_prof, USE.NAMES = F, 
                                 function(x) {
                                   y <- strsplit(x, ":")[[1]]
                                   if (all(is.element(substrings, y))) 
                                     x
                                 }))
    all_depends <- all_depends[-is.element(all_depends, profile_effects[i])]
    for (j in 1:nrow(levels)) {
      effect_level_coef <- coefs[j]
      initial_beta <- coefficients(lin.mod.prof)[effect_level_coef]
      if (!is.na(initial_beta) & length(substrings) > 1) {
        for (effect1 in substrings) {
          effect_base1 <- levels(data[[effect1]])[1]
          base.subset <- data[which(data[[effect1]] == 
                                      effect_base1), ]
          for (effect in substrings[!(substrings %in% 
                                      effect1)]) {
            base.subset <- base.subset[which(base.subset[[effect]] == 
                                               as.character(levels[j, effect])), ]
          }
          if (nrow(base.subset) == 0) {
            initial_beta <- NA
            warn_i <- warn_i + 1
          }
        }
      }
      if (!is.na(initial_beta) & length(all_depends) > 
          0) {
        J_effect_call <- J_base_call <- J_call
        for (effect in substrings) {
          base <- levels(data[[effect]])[1]
          effect_index <- which(names(dimnames(design$J)) == 
                                  effect)
          J_base_call[effect_index + 2] <- base
          level <- levels[j, effect]
          J_effect_call[effect_index + 2] <- level
        }
        eval(call("<-", Quote(J_baseline), J_base_call))
        eval(call("<-", Quote(J_effect), J_effect_call))
        for (k in 1:length(all_depends)) {
          depend <- all_depends[[k]]
          substrings_d <- strsplit(depend, ":")[[1]]
          substrings_d <- substrings_d[!is.element(substrings_d, 
                                                   substrings)]
          all_depend_coefs <- list()
          for (sub in substrings_d) {
            all_depend_coefs[[sub]] <- sapply(levels(data[[sub]]), 
                                              function(x) paste(c(sub, x), collapse = ""))
          }
          all_depend_levels <- expand.grid(all_depend_coefs)
          substrings_l <- strsplit(effect_level_coef, 
                                   ":")[[1]]
          for (l in length(substrings_l):1) {
            all_depend_levels <- cbind(rep(substrings_l[l], 
                                           nrow(all_depend_levels)), all_depend_levels)
          }
          colnames(all_depend_levels)[1:length(substrings_l)] <- substrings
          all_depend_levels <- all_depend_levels[sort(colnames(all_depend_levels))]
          all_depend_level_coefs <- apply(all_depend_levels, 
                                          1, function(x) paste(x, collapse = ":"))
          if (!(is.null(dim(J_baseline)))) {
            baseline_support <- apply(J_baseline, substrings_d, 
                                      sum)
          }
          else {
            baseline_support <- J_baseline
          }
          baseline_support[baseline_support != 0] <- 1
          if (!is.null(dim(J_effect))) {
            joint_prob <- apply(J_effect, substrings_d, 
                                sum) * baseline_support
          }
          else {
            joint_prob <- J_effect * baseline_support
          }
          joint_prob <- as.vector(joint_prob)
          names(joint_prob) <- all_depend_level_coefs
          all_depend_level_coefs <- all_depend_level_coefs[!is.na(lin.mod.prof$coefficients[all_depend_level_coefs])]
          varprob_mat[effect_level_coef, all_depend_level_coefs] <- as.numeric(joint_prob[all_depend_level_coefs])/as.numeric(sum(joint_prob))
          if (length(all_depend_level_coefs)) {
            var_prob <- joint_prob[all_depend_level_coefs]
            var_prob <- as.numeric(var_prob)/as.numeric(sum(joint_prob))
            depend_betas <- lin.mod.prof$coefficients[all_depend_level_coefs]
            initial_beta <- sum(initial_beta, var_prob * 
                                  depend_betas, na.rm = T)
          }
        }
      }
      results[1, j] <- initial_beta
    }
    estimates[[profile_effects[i]]] <- results
  }
  vcov_prof <- suppressMessages(fix.vcov(varprob_mat, vcov_mat_prof))
  for (i in 1:length(estimates)) {
    coef_names <- colnames(estimates[[i]])
    variances <- sqrt(diag(vcov_prof)[coef_names])
    estimates[[i]][2, ] <- ifelse(is.na(estimates[[i]][1, 
                                                       ]), NA, variances)
  }
  profile_effects_plus <- paste(profile_effects, collapse = " + ")
  profile_effects_form <- formula(paste(c(y_var, profile_effects_plus), 
                                        collapse = " ~ "))
  profile_effects_terms <- colnames(model.matrix(profile_effects_form, 
                                                 data))
  profile_effects_terms <- profile_effects_terms[profile_effects_terms %in% 
                                                   colnames(vcov_mat_prof)]
  vcov_prof <- vcov_prof[profile_effects_terms, profile_effects_terms]
  if (length(respondent_vars) > 0) {
    conditional.estimates <- list()
    resp_effects <- c(sort(resp_effects[!grepl(":", resp_effects)]), 
                      resp_effects[grepl(":", resp_effects)])
    covariance_list <- list()
    varprob_mat <- matrix(0, nrow(vcov_mat_full), ncol(vcov_mat_full))
    colnames(varprob_mat) <- rownames(varprob_mat) <- colnames(vcov_mat_full)
    for (i in 1:length(resp_effects)) {
      substrings <- strsplit(resp_effects[i], "[:*]", perl = TRUE)[[1]]
      all_levels <- list()
      all_levels_coefs <- list()
      for (effect in substrings) {
        if (class(data[[effect]]) != "factor") {
          all_levels[[effect]] <- effect
          all_levels_coefs[[effect]] <- effect
        }
        else {
          all_levels[[effect]] <- levels(data[[effect]])[-1]
          all_levels_coefs[[effect]] <- sapply(all_levels[[effect]], 
                                               function(x) {
                                                 paste(c(effect, x), collapse = "")
                                               })
        }
      }
      levels <- expand.grid(all_levels, stringsAsFactors = FALSE)
      levels <- cbind(apply(levels, 1, function(x) paste(x, 
                                                         collapse = ":")), levels)
      colnames(levels) <- c("name", substrings)
      coefs <- expand.grid(all_levels_coefs, stringsAsFactors = FALSE)
      coefs <- apply(coefs, 1, function(x) paste(x, collapse = ":"))
      results <- matrix(nrow = 2, ncol = nrow(levels))
      rownames(results) <- c("Conditional Estimate", "Std. Error")
      colnames(results) <- coefs
      results[2, ] <- NA
      if (any(substrings %in% profile_vars)) {
        all_depends <- unlist(sapply(all_run_vars, USE.NAMES = F, 
                                     function(x) {
                                       y <- strsplit(x, ":")[[1]]
                                       if (all(is.element(substrings, y))) 
                                         x
                                     }))
        all_depends <- all_depends[!is.element(all_depends, 
                                               resp_effects[i])]
        resp.other <- respondent_vars[!is.element(respondent_vars, 
                                                  substrings)]
        all_depends <- unlist(sapply(all_depends, function(x) {
          sub_depends <- strsplit(x, ":")[[1]]
          if (all(!is.element(sub_depends, resp.other))) 
            x
        }))
      }
      else {
        all_depends <- c()
      }
      for (j in 1:nrow(levels)) {
        effect_level_coef <- coefs[j]
        initial_beta <- coefficients(lin.mod.full)[effect_level_coef]
        if (!is.na(initial_beta)) {
          for (effect1 in substrings[substrings %in% 
                                     profile_vars]) {
            effect_base1 <- levels(data[[effect1]])[1]
            base.subset <- data[which(data[[effect1]] == 
                                        effect_base1), ]
            for (effect in substrings[substrings %in% 
                                      profile_vars][!(substrings[substrings %in% 
                                                                 profile_vars] %in% effect1)]) {
              base.subset <- base.subset[which(base.subset[[effect]] == 
                                                 as.character(levels[j, effect])), ]
            }
            if (nrow(base.subset) == 0) {
              initial_beta <- NA
              warn_i <- warn_i + 1
            }
          }
        }
        if (!is.na(initial_beta) & length(all_depends) > 
            0) {
          J_effect_call <- J_base_call <- J_call
          for (effect in substrings[substrings %in% profile_vars]) {
            base <- levels(data[[effect]])[1]
            effect_index <- which(names(dimnames(design$J)) == 
                                    effect)
            J_base_call[effect_index + 2] <- base
            level <- levels[j, effect]
            J_effect_call[effect_index + 2] <- level
          }
          eval(call("<-", Quote(J_baseline), J_base_call))
          eval(call("<-", Quote(J_effect), J_effect_call))
          for (k in 1:length(all_depends)) {
            depend <- all_depends[[k]]
            substrings_d <- strsplit(depend, ":")[[1]]
            substrings_d <- substrings_d[!is.element(substrings_d, 
                                                     substrings)]
            all_depend_coefs <- list()
            for (sub in substrings_d) {
              all_depend_coefs[[sub]] <- sapply(levels(data[[sub]]), 
                                                function(x) paste(c(sub, x), collapse = ""))
            }
            all_depend_levels <- expand.grid(all_depend_coefs)
            substrings_l <- strsplit(effect_level_coef, 
                                     ":")[[1]]
            for (l in length(substrings_l):1) {
              all_depend_levels <- cbind(rep(substrings_l[l], 
                                             nrow(all_depend_levels)), all_depend_levels)
            }
            colnames(all_depend_levels)[1:length(substrings_l)] <- substrings
            all_depend_levels <- all_depend_levels[sort(colnames(all_depend_levels))]
            all_depend_level_coefs <- apply(all_depend_levels, 
                                            1, function(x) paste(x, collapse = ":"))
            if (!(is.null(dim(J_baseline)))) {
              baseline_support <- apply(J_baseline, substrings_d, 
                                        sum)
            }
            else {
              baseline_support <- J_baseline
            }
            baseline_support[baseline_support != 0] <- 1
            if (!is.null(dim(J_effect))) {
              joint_prob <- apply(J_effect, substrings_d, 
                                  sum) * baseline_support
            }
            else {
              joint_prob <- J_effect * baseline_support
            }
            joint_prob <- as.vector(joint_prob)
            names(joint_prob) <- all_depend_level_coefs
            all_depend_level_coefs <- all_depend_level_coefs[!is.na(lin.mod.full$coefficients[all_depend_level_coefs])]
            varprob_mat[effect_level_coef, all_depend_level_coefs] <- as.numeric(joint_prob[all_depend_level_coefs])/as.numeric(sum(joint_prob))
            if (length(all_depend_level_coefs)) {
              var_prob <- joint_prob[all_depend_level_coefs]
              var_prob <- as.numeric(var_prob)/as.numeric(sum(joint_prob))
              depend_betas <- lin.mod.full$coefficients[all_depend_level_coefs]
              initial_beta <- sum(initial_beta, var_prob * 
                                    depend_betas, na.rm = T)
            }
          }
        }
        results[1, j] <- initial_beta
      }
      conditional.estimates[[resp_effects[i]]] <- results
    }
    vcov_resp <- suppressMessages(fix.vcov(varprob_mat, vcov_mat_full))
    for (i in 1:length(conditional.estimates)) {
      coef_names <- colnames(conditional.estimates[[i]])
      variances <- sqrt(diag(vcov_resp)[coef_names])
      conditional.estimates[[i]][2, ] <- ifelse(is.na(conditional.estimates[[i]][1, 
                                                                                 ]), NA, variances)
    }
    resp_effects_plus <- paste(resp_effects, collapse = " + ")
    resp_effects_form <- formula(paste(c(y_var, resp_effects_plus), 
                                       collapse = " ~ "))
    resp_effects_terms <- colnames(model.matrix(resp_effects_form, 
                                                data))
    resp_effects_terms <- resp_effects_terms[resp_effects_terms %in% 
                                               colnames(vcov_mat_full)]
    vcov_resp <- vcov_resp[resp_effects_terms, resp_effects_terms]
  }
  output <- list()
  class(output) <- c("amce")
  output$estimates <- estimates
  output$attributes <- dimnames(design$J)
  output$vcov.prof <- vcov_prof
  output$samplesize_prof <- sample_size_prof
  output$formula <- form
  if (warn_i > 0) {
    warning(paste("Warning: ", warn_i, " interaction levels lacked support at baseline, effects undefined unless alternative baseline is provided."))
  }
  if (length(respondent.varying) > 0) {
    output$cond.estimates <- conditional.estimates
    output$vcov.resp <- vcov_resp
    output$samplesize_full <- sample_size_full
    output$cond.formula <- resp_effects_form
  }
  output$baselines <- list()
  output$continuous <- list()
  for (k in unique_vars) {
    if (class(data[[k]]) == "factor") {
      output$baselines[[k]] <- levels(data[[k]])[1]
    }
    else if (class(data[[k]]) == "numeric") {
      output$continuous[[k]] <- quantile(model.matrix(form, 
                                                      data)[, k], probs = c(0.25, 0.5, 0.75), na.rm = T)
    }
  }
  if (!is.null(respondent.id)) {
    output$numrespondents <- length(unique(data[[respondent.id]]))
  }
  else {
    output$numrespondents <- NULL
  }
  if (!is.null(respondent.varying)) {
    output$respondent.varying <- respondent_vars
  }
  else {
    output$respondent.varying <- NULL
  }
  if (!is.null(weights)) {
    output$weights <- subset(data, select = weights)
  }
  else {
    output$weights <- NULL
  }
  output$user.names <- user_names
  output$user.levels <- user_levels
  output$data <- data
  output$lmer.full <- lin.mod.full
  output$lmer.prof <- lin.mod.prof
  return(output)
}