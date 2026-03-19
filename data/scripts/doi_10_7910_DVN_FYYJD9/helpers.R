draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}


het_mm_p <- function(var_list = NULL, x, long_data) {
  est_mm_out_x <- list()
  est_mm_out_nx <- list()
  est_mm_out_diff <- list()
  
  if (is.null(var_list)) {
    var_list <- c("sex_",
                  "age_",
                  "harm_",
                  "exposure_",
                  "country_",
                  "occupation_",
                  "work_")
  }
  
  for (i in 1:length(var_list)) {
    col <- sym(var_list[i])
    est_mm_out_x[[i]] <-
      long_data %>%
      filter(!!sym(x) == 1) %>%
      group_by(!!col) %>%
      do(tidy(
        lm_robust(conjoint_chosen ~ 1, data = .,
                  clusters = admin_ResponseId)
      )) %>%
      mutate(Z = paste0(var_list[i], !!col),
             X = "Yes")
    
    est_mm_out_nx[[i]] <-
      long_data %>%
      filter(!!sym(x) == 0) %>%
      group_by(!!col) %>%
      do(tidy(
        lm_robust(conjoint_chosen ~ 1, data = .,
                  clusters = admin_ResponseId)
      )) %>%
      mutate(Z = paste0(var_list[i], !!col),
             X = "No")
    
    est_mm_out_diff[[i]] <-
      long_data %>%
      filter(!is.na(!!sym(x))) %>%
      group_by(!!col) %>%
      do(tidy(
        lm_robust(
          conjoint_chosen ~ !!sym(x),
          data = .,
          clusters = admin_ResponseId
        )
      )) %>%
      filter(term == x) %>%
      mutate(Z = paste0(var_list[i], !!col),
             X = "Difference")
    
  }
  
  het_mm_out <-
    bind_rows(est_mm_out_x, est_mm_out_nx, est_mm_out_diff) %>%
    ungroup() %>%
    select(Z,
           X,
           estimate,
           std.error,
           statistic,
           p.value,
           conf.low,
           conf.high) %>%
    mutate(
      attribute = str_to_title(word(Z, 1, sep = "_")),
      level = word(Z, 2, sep = "_"),
      level = str_replace(level, " \\(e.g., police/fire\\)", ""),
      level = case_when(
        level == "Non-essential worker" ~ "Non-essential workers",
        TRUE ~ paste(level)
      ),
      level = factor(
        level,
        levels = c(
          "Male",
          "Female",
          "18-24",
          "25-34",
          "35-44",
          "45-54",
          "55-64",
          "65-74",
          "75+",
          "Low",
          "Moderate",
          "High",
          "China",
          "Pakistan",
          "Australia",
          "India",
          "Nigeria",
          "South Africa",
          "Brazil",
          "Canada",
          "United States",
          "Non-essential workers",
          "Public transit",
          "Education and childcare",
          "First responders",
          "Healthcare workers",
          "Yes",
          "No"
        )
      ),
      attribute = factor(
        attribute,
        levels = c(
          "Country",
          "Exposure",
          "Harm",
          "Occupation",
          "Work",
          "Age",
          "Sex"
        ),
        labels = c(
          "Country of origin:",
          #"Risk of exposure to COVID-19:",
          "Risk of exposure:",
          #"Risk of serious illness from COVID-19:",
          "Risk of serious illness:",
          "Occupation group:",
          "Can work from home:",
          "Age group:",
          "Sex:"
        )
      )
    )
  return(het_mm_out)
}


het_mm_a <- function(var_list = NULL, attr_lvls = NULL, attr_labs = NULL, 
                     feat_lvls = NULL, feat_labs = NULL, x, long_data,
                     cluster) {
  est_mm_out_x <- list()
  est_mm_out_nx <- list()
  est_mm_out_diff <- list()
  
  if (is.null(var_list)) {
    var_list <-
      c(
        "price_",
        "participants_",
        "costs_",
        "benefits_",
        "supply_",
        "sharing_",
        "monitoring_"
      )
  }
  
  if (is.null(feat_lvls) | is.null(feat_labs) | is.null(attr_lvls) | 
      is.null(attr_labs) ) {
    feat_lvls <- c(
      "$1",
      "$5",
      "$10",
      "$15",
      "$20",
      "20 of 192",
      "80 of 192",
      "100 of 192",
      "170 of 192",
      "Only rich countries contribute",
      "Countries that need more vaccines pay more",
      "Rich countries contribute more than poor countries",
      "Countries that pay more get more in return",
      "Only poor countries",
      "Poor countries get more than rich countries",
      "Proportional to a country's population size",
      "Countries with more people at risk of severe illness get more",
      "No",
      "Yes",
      "Compulsory",
      "Voluntary",
      "U.S. government",
      "World Health Organization",
      "Independent commission",
      "United Nations"
    )
    
    feat_labs <- c(
      "$1",
      "$5",
      "$10",
      "$15",
      "$20",
      "20 of 192",
      "80 of 192",
      "100 of 192",
      "170 of 192",
      "Only rich countries contribute",
      "Countries that need more vaccines pay more",
      "Rich countries contribute more than poor",
      "Proportional to a country's contribution",
      "Only poor countries benefit",
      "Poor countries benefit more than rich",
      "Proportional to population size",
      "Proportional to size of at-risk population",
      "No",
      "Yes",
      "Compulsory",
      "Voluntary",
      "U.S. government",
      "World Health Organization",
      "Independent commission",
      "United Nations"
    )
    
    attr_lvls <- c(
      "Price",
      "Participants",
      "Benefits",
      "Costs",
      "Monitoring",
      "Sharing",
      "Supply"
    )
    
    attr_labs <- c(
      "Costs to average household",
      "Number of participating countries",
      "Distribution of benefits",
      "Distribution of costs",
      "Monitoring for non-compliance",
      "Sharing of vaccine technology",
      "External supply agreements allowed"
    )
  }
  
  
  for (i in 1:length(var_list)) {
    col <- sym(var_list[i])
    est_mm_out_x[[i]] <-
      long_data %>%
      filter(!!sym(x) == 1) %>%
      group_by(!!col) %>%
      do(tidy(
        lm_robust(conjoint_chosen ~ 1, data = .,
                  clusters = !!sym(cluster))
      )) %>%
      mutate(Z = paste0(var_list[i], !!col),
             X = "Yes")
    
    est_mm_out_nx[[i]] <-
      long_data %>%
      filter(!!sym(x) == 0) %>%
      group_by(!!col) %>%
      do(tidy(
        lm_robust(conjoint_chosen ~ 1, data = .,
                  clusters = !!sym(cluster))
      )) %>%
      mutate(Z = paste0(var_list[i], !!col),
             X = "No")
    
    est_mm_out_diff[[i]] <-
      long_data %>%
      filter(!is.na(!!sym(x))) %>%
      group_by(!!col) %>%
      do(tidy(
        lm_robust(
          conjoint_chosen ~ !!sym(x),
          data = .,
          clusters = !!sym(cluster)
        )
      )) %>%
      filter(term == x) %>%
      mutate(Z = paste0(var_list[i], !!col),
             X = "Difference")
    
  }
  
  het_mm_out <-
    bind_rows(est_mm_out_x, est_mm_out_nx, est_mm_out_diff) %>%
    ungroup() %>%
    select(Z,
           X,
           estimate,
           std.error,
           statistic,
           p.value,
           conf.low,
           conf.high) %>%
    mutate(
      attribute = str_to_title(word(Z, 1, sep = "_")),
      level = word(Z, 2, sep = "_"),
      level = factor(
        level,
        levels = feat_lvls, 
        labels = feat_labs
      ),
      attribute = factor(
        attribute,
        levels = attr_lvls, 
        labels = attr_labs
      )
    )
  
  
  return(het_mm_out)
}

het_amce_a <- function(x,
                       long_data,
                       formula,
                       cluster,
                       attr_labs = NULL,
                       attr_lvls = NULL,
                       feat_labs = NULL,
                       feat_lvls = NULL) {
  
  if (is.null(feat_lvls) | is.null(feat_labs) | is.null(attr_lvls) |
      is.null(attr_labs)) {
    
    attr_lvls <- c("price",
                   "participants",
                   "benefits",
                   "costs",
                   "monitoring",
                   "sharing",
                   "supply")
    
    attr_labs <- c(
      "Costs to average household (reference: $1)",
      "Number of participating countries (reference: 20 of 192)",
      "Distribution of benefits (reference: Only poor countries benefit)",
      "Distribution of costs (reference: Only rich countries contribute)",
      "Monitoring for non-compliance (reference: U.S. government)",
      "Sharing of vaccine technology (reference: Compulsory)",
      "External supply agreements allowed (reference: No)"
    )
    
    feat_lvls <- c(
      "$5",
      "$10",
      "$15",
      "$20",
      "80 of 192",
      "100 of 192",
      "170 of 192",
      "Countries that pay more get more in return",
      "Poor countries get more than rich countries",
      "Proportional to a country's population size",
      "Countries with more people at risk of severe illness get more",
      "Countries that need more vaccines pay more",
      "Rich countries contribute more than poor countries",
      "Yes",
      "Voluntary",
      "World Health Organization",
      "United Nations",
      "Independent commission"
    )
    
    feat_labs <- c(
      "$5",
      "$10",
      "$15",
      "$20",
      "80 of 192",
      "100 of 192",
      "170 of 192",
      "Proportional to a country's contribution",
      "Poor countries benefit more than rich",
      "Proportional to population size",
      "Proportional to size of at-risk population",
      "Countries that need more contribute more",
      "Rich countries contribute more than poor",
      "Yes",
      "Voluntary",
      "World Health Organization",
      "United Nations",
      "Independent commission"
    )
  }
  
  # get outcome variable
  outcome <- all.vars(stats::update(formula, . ~ 0))
  if (!length(outcome) || outcome == ".") {
    stop("'formula' is missing an outcome variable")
  }
  
  # get RHS variables, variable labels, and factor levels
  RHS <- all.vars(stats::update(formula, 0 ~ .))
  
  ## Linear model
  f <- as.formula(paste(outcome, paste(RHS, collapse = " + "), sep = " ~ "))
  
  ## Interactions
  interactions <- outer(x, RHS, function(x, y) {
    paste0(x, '*', y)
  })
  
  f_x <- as.formula(paste(outcome, paste(interactions, collapse = " + "), sep = " ~ "))
  
  est_amce_out_x <-
    lm_robust(f,
              clusters = !!sym(cluster),
              data = long_data %>% filter(!!sym(x) == 1)) %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(X = "Yes")
  
  est_amce_out_nx <-
    lm_robust(f,
              clusters = !!sym(cluster),
              data = long_data %>% filter(!!sym(x) == 0)) %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(X = "No")
  
  est_amce_out_diff <-
    lm_robust(f_x, clusters = !!sym(cluster),
              data = long_data) %>%
    tidy() %>%
    filter(str_detect(term, ":")) %>%
    mutate(X = "Difference",
           term = str_remove(term, ".*:"))
  
  het_amce_out <-
    bind_rows(est_amce_out_x, est_amce_out_nx, est_amce_out_diff) %>%
    mutate(
      tmp_attr = str_remove(term, "_.*"),
      tmp_feat = str_remove(term, ".*_"),
      attribute_ref =
        factor(tmp_attr,
               levels = attr_lvls,
               labels = attr_labs,),
      level = factor(tmp_feat,
                     levels = feat_lvls,
                     labels = feat_labs,)
    )
  
  return(het_amce_out)
}


het_amce_p <- function(x, long_data){
  
  est_amce_out_x <-
    lm_robust(
      conjoint_chosen ~ sex_ + age_ + harm_ + exposure_ + country_ +
        occupation_ + work_,
      clusters = admin_ResponseId,
      data = long_data %>% filter(!!sym(x) == 1)
    ) %>%
    tidy() %>% 
    filter(term != "(Intercept)") %>% 
    mutate(X = "Yes")
  
  est_amce_out_nx <- 
    lm_robust(
      conjoint_chosen ~ sex_ + age_ + harm_ + exposure_ + country_ +
        occupation_ + work_,
      clusters = admin_ResponseId,
      data = long_data %>% filter(!!sym(x) == 0)
    ) %>%
    tidy() %>% 
    filter(term != "(Intercept)") %>% 
    mutate(X = "No") 
  
  est_amce_out_diff <-
    lm_robust(
      conjoint_chosen ~ !!sym(x) * sex_+!!sym(x) * age_+!!sym(x) * harm_+!!sym(x) *
        exposure_+!!sym(x) * country_+!!sym(x) * occupation_+!!sym(x) * work_,
      clusters = admin_ResponseId,
      data = long_data
    ) %>%
    tidy() %>%
    filter(str_detect(term, ":")) %>%
    mutate(X = "Difference",
           term = str_remove(term, ".*:"))
  
  het_amce_out <-
    bind_rows(est_amce_out_x, est_amce_out_nx, est_amce_out_diff) %>%
    mutate(
      attribute_ref =
        case_when(
          str_detect(term, "sex_") ~ "Sex (reference: Female)",
          str_detect(term, "age_") ~ "Age group (reference: 18-24)",
          str_detect(term, "harm_") ~ "Risk of serious illness (reference: Low)",
          str_detect(term, "exposure_") ~ "Risk of exposure (reference: Low)",
          str_detect(term, "country_") ~  "Country of origin (reference: United States)",
          str_detect(term, "occupation_") ~ "Occupation group (reference: Non-essential workers)",
          str_detect(term, "work_") ~ "Can work from home (reference: Yes)"
        ),
      attribute_ref = factor(
        attribute_ref,
        levels = c(
          "Country of origin (reference: United States)",
          "Risk of exposure (reference: Low)",
          "Risk of serious illness (reference: Low)",
          "Occupation group (reference: Non-essential workers)",
          "Can work from home (reference: Yes)",
          "Age group (reference: 18-24)",
          "Sex (reference: Female)"
        )
      ),
      level = str_replace(
        term,
        "sex_|age_|harm_|exposure_|country_|occupation_|work_",
        ""
      ),
      level = str_replace(level, " \\(e.g., police/fire\\)", ""),
      level = case_when(
        level == "Non-essential worker" ~ "Non-essential workers", 
        TRUE ~ paste(level)
      ), 
      level = factor(
        level,
        levels = c(
          "Male",
          "25-34",
          "35-44",
          "45-54",
          "55-64",
          "65-74",
          "75+",
          "Moderate",
          "High",
          "China",
          "Pakistan",
          "Australia",
          "India",
          "Nigeria",
          "South Africa",
          "Brazil",
          "Canada",
          "Public transit",
          "Education and childcare",
          "First responders",
          "Healthcare workers",
          "No"
        )
      )
    )
  return(het_amce_out)
}


bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")

glass_delta <- function(Y, Z, reference){
  Y/sd(Y[Z == reference], na.rm = TRUE)
}

## Function to make inverse covariance weighted indices
invcov <-
  function(Z, outcome_mat, to_reorient, reorient = FALSE) {
    if (length(Z) != nrow(outcome_mat))
      stop("Error: Treatment assignment, outcome matrix require same n!")
    if (reorient == TRUE) {
      outcome_mat[, c(to_reorient)] <- -outcome_mat[, c(to_reorient)]
    }
    c_mean <-
      apply(
        X = outcome_mat[Z == 0, ],
        MARGIN = 2,
        FUN = mean,
        na.rm = T
      )
    c_sd <-
      apply(
        X = outcome_mat[Z == 0, ],
        MARGIN = 2,
        FUN = sd,
        na.rm = T
      )
    z_score <- t(t(sweep(outcome_mat, 2, c_mean)) / c_sd)
    Sigma_hat <-
      solve(cov(z_score, y = z_score, use = "complete.obs"))
    one_vec <- as.vector(rep(1, ncol(outcome_mat)))
    if (sum(is.na(outcome_mat)) > 0) {
      z_score[is.na(z_score)] <- 0
    }
    w_ij <-
      t(solve(t(one_vec) %*% Sigma_hat %*% one_vec) %*% (t(one_vec) %*% Sigma_hat))
    if (sum(w_ij < 0) > 0) {
      warning('Warning, at least one weight is negative!')
    }
    s_ij <-
      t(solve(t(one_vec) %*% Sigma_hat %*% one_vec) %*% (t(one_vec) %*% Sigma_hat %*% t(z_score)))
    index <-
      (s_ij - mean(s_ij[Z == 0], na.rm = T)) / sd(s_ij[Z == 0], na.rm = T)
    return(s_ij)
  }


tidy.rma.uni <- function(fit) {
  
  tibble(estimate = as.numeric(fit$beta), 
         std.error = fit$se, 
         conf.low = fit$ci.lb, 
         conf.high = fit$ci.ub,
         p.value = fit$pval)
}

add_parens <- function(x, digits = 2) {
  x <- as.numeric(x)
  return(paste0("(", sprintf(paste0("%.", digits, "f"), x), ")"))
}

format_num <- function(x, digits = 2) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}


make_entry <- function(est, se, p, digits = 2) {
  entry <- paste0(format_num(est, digits = digits), " ", 
                  add_parens(se, digits = digits))
  entry[p < 0.05] <- paste0(entry[p < 0.05], "*")
  return(entry)
}

table_entry <- function(est, se, digits = 2) {
  entry <- paste0(format_num(est, digits = digits), " ", 
                  add_parens(se, digits = digits))
  return(entry)
}

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}
