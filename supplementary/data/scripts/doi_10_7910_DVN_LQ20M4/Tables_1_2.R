#############################################################################################################
## Philip Warncke & Ryan E. Carlin                                                                         ##
## Replication script for: "Bad Mood Rising: Bad Mood Rising? Assessing Scalar Invariance Violations       ##
## with Comparative Democratic Support Data (cond. accepted in: Public Opinion Quarterly, Sp. issue 2026:  ##
## Advancing Public Opinion Research Across Countries and Contexts)                                        ##
## Please reach out to Philip Warncke (https://philip-warncke.net/) for queries or bug reports.            ##
#############################################################################################################

#### SCRIPT 5 OUT OF 5: Chile-Costa Rica comparison (Table 1) & directional DIF assessment (Table 2)  ####

########## Loading required packages ##########
rm(list = setdiff(ls(), lsf.str()))

if(!require("pacman")) install.packages('pacman')
library(pacman)

p_load(haven)
p_load(mirt)
p_load(missForest)
p_load(future)
p_load(future.apply)
p_load(progressr)
p_load(tidyverse)

set.seed(42)

########## Data import, cleaning, and FR missing imputation ##########

## Negator function helper
neg <- function(x) { 
  temp <- -1*x
  temp <- temp+abs(min(temp, na.rm = TRUE))
  return(temp)}

Latinobarometro2013Eng <- read_dta("Latinobarometro2013Eng.dta") %>%
  zap_labels() %>% zap_label()  %>% zap_formats() %>% zap_labels()

Latinobaro_sub <- Latinobarometro2013Eng %>% dplyr::select(idenpa, P12STGBS, P43ST_A, P43TGB_B, P43GBS_C,
                                                           P43GBS_E, P43TGB_D, P50GBS_F)

Latinobaro_sub[Latinobaro_sub<0] <- NA ## Recoding missing
Latinobaro_sub$P43ST_A <- Latinobaro_sub$P43ST_A %>% neg()

Latinobaro_sub <- Latinobaro_sub %>% 
  mutate(country_name = recode(idenpa, `32`  = "Argentina", `68`  = "Bolivia", `76`  = "Brazil", `152` = "Chile",
                               `170` = "Colombia", `188` = "Costa Rica", `214` = "Dominican Rep.", `218` = "Ecuador",
                               `222` = "El Salvador", `320` = "Guatemala", `340` = "Honduras", `484` = "Mexico",
                               `558` = "Nicaragua", `591` = "Panama", `600` = "Paraguay", `604` = "Peru", `724` = "Spain",
                               `858` = "Uruguay", `862` = "Venezuela"))

Latinobaro_mie <- Latinobaro_sub %>% dplyr::select(idenpa, P12STGBS, P43ST_A, P43TGB_B, P43GBS_C, P43GBS_E,
                                            P43TGB_D, P50GBS_F)

Latinobaro_red <- Latinobaro_mie %>% dplyr::filter(idenpa %in% c(188, 152))

## Select items
items <- c("P43GBS_E", "P43TGB_B", "P43ST_A", "P43GBS_C")
groupvar <- "idenpa"

# Subset to items + grouping variable
dat <- Latinobaro_red[, c(items, groupvar)]

# Ensure grouping variable is a factor
dat[[groupvar]] <- factor(dat[[groupvar]])

## Setting up item block
item_data <- dat[, items]
group     <- dat[[groupvar]]

item_data[] <- lapply(item_data, function(x) {
  if (is.numeric(x)) x else as.numeric(as.factor(x))
})

item_data <- missForest(as.matrix(item_data))$ximp
item_data <-  round(item_data, 0) %>% as.data.frame() ## Flooring back to discrete likert choices

########### Chile - Costa Rica DIF assessment (Table 1) ###########
item_order <- c("P43ST_A", "P43GBS_C", "P43TGB_B", "P43GBS_E")
item_labels <- c(
  P43ST_A  = "Churchillian sup.",
  P43GBS_C = "Single-party rule",
  P43TGB_B = "Strongman rule",
  P43GBS_E = "Expert rule"
)

## Mapping requested items to their positions in the model
item_idx <- match(item_order, colnames(item_data))

## Grabbing item-level thresholds in correct oder (lowest first)
cats_first <- sort(unique(na.omit(item_data[[ item_order[1] ]])))
n_thr <- length(cats_first) - 1L 

which_par_thresh <- paste0("d", seq_len(n_thr))

mg_metric <- multipleGroup(
  data     = item_data,
  model    = 1,
  group    = group,
  itemtype = "graded",
  invariance = c("slopes"),                # Imposing metric invariance
  technical = list(NCYCLES = 6000),
  SE       = TRUE)

mg_scalar <- multipleGroup(
  data     = item_data,
  model    = 1,
  group    = group,
  itemtype = "graded",
  invariance = c("slopes", "intercepts"), # Imposing scalar invariance
  technical = list(NCYCLES = 6000),
  SE       = TRUE)

cf_scalar <- coef(mg_scalar, IRTpars = TRUE, simplify = TRUE)

# Group-wise latent means
group_means <- sapply(cf_scalar, function(x) x$means)
group_cov <- sapply(cf_scalar, function(x) x$cov) 

group_means/group_cov ## Mean is -0.582 std. units lower in Costa Rica than in Chile

## Global differential item functioning stats extracted via canned mirt function:
dif_lr <- mirt::DIF(
  mg_scalar,
  which.par  = which_par_thresh,
  items2test = item_idx,
  scheme     = "drop",
  verbose    = FALSE,
  technical  = list(NCYCLES = 20000)
)


## Coefficient standard errors
cf_se <- coef(mg_metric, IRTpars = TRUE, printSE = TRUE)


## Global LR test + uncertainty: scalar (constrained) vs metric
lr_tab <- anova(mg_scalar, mg_metric)

chi2_lr <- lr_tab$X2[2]
df_lr   <- lr_tab$df[2]
p_lr    <- lr_tab$p[2]

###### Aggregating results and assembling output table ######

## Order for table overview


## Preparing statistical summaries with helper functions
fmt2 <- function(x) ifelse(is.na(x), "", sprintf("%.2f", x))
fmt3 <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))

n_items <- length(item_order)
n_cols  <- 1 + 3 * n_items   # 1 country col + 3 thresholds per item

## Global LR test
p_lr_num <- suppressWarnings(as.numeric(p_lr))

global_lr_line <- if (!is.na(p_lr_num) && p_lr_num < 0.001) {
  sprintf(
    "    \\multicolumn{%d}{l}{Global LR test (scalar vs. metric): $\\chi^2(%d) = %s$, $p < .001$.}\\\\",
    n_cols, df_lr, fmt2(chi2_lr)
  )
} else {
  sprintf(
    "    \\multicolumn{%d}{l}{Global LR test (scalar vs. metric): $\\chi^2(%d) = %s$, $p = %s$.}\\\\",
    n_cols, df_lr, fmt2(chi2_lr), fmt3(p_lr_num)
  )
}

## Helper function to retreive item codes from DIF table
get_dif_item_codes <- function(dif_obj) {
  if ("item" %in% names(dif_obj)) dif_obj$item else rownames(dif_obj)
}

dif_items_codes <- get_dif_item_codes(dif_lr)

X2_num <- suppressWarnings(as.numeric(dif_lr$X2))
p_num  <- suppressWarnings(as.numeric(dif_lr$p))
df_num <- suppressWarnings(as.numeric(dif_lr$df))

valid <- !is.na(X2_num) & !is.na(p_num) & !is.na(df_num)
X2_num <- X2_num[valid]
p_num  <- p_num[valid]
df_num <- df_num[valid]
dif_items_codes <- dif_items_codes[valid]

if (length(X2_num) == 0L) {
  item_lr_max_line <- sprintf(
    "    \\multicolumn{%d}{l}{Max item-level LR DIF on thresholds: not available (estimation failed).}\\\\",
    n_cols
  )
  item_lr_min_line <- sprintf(
    "    \\multicolumn{%d}{l}{Min item-level LR DIF on thresholds: not available (estimation failed).}\\\\",
    n_cols
  )
} else {
  ## Strongest versus weakest item-level DIF
  idx_max <- which.max(X2_num)
  idx_min <- which.min(X2_num)
  
  max_X2 <- X2_num[idx_max]
  df_max <- df_num[idx_max]
  p_min  <- p_num[idx_max]
  
  min_X2 <- X2_num[idx_min]
  df_min <- df_num[idx_min]
  p_max  <- p_num[idx_min]
  
  item_code_max <- dif_items_codes[idx_max]
  item_code_min <- dif_items_codes[idx_min]
  
  get_label <- function(code) {
    lbl <- item_labels[code]
    if (is.na(lbl) || length(lbl) == 0L) code else unname(lbl)
  }
  
  item_label_max <- get_label(item_code_max)
  item_label_min <- get_label(item_code_min)
  
  item_lr_max_line <- if (!is.na(p_min) && p_min < 0.001) {
    sprintf(
      "    \\multicolumn{%d}{l}{Max item-level LR DIF on thresholds (%s): $X^2(%d) = %s$, $p < .001$.}\\\\",
      n_cols, item_label_max, df_max, fmt2(max_X2)
    )
  } else {
    sprintf(
      "    \\multicolumn{%d}{l}{Max item-level LR DIF on thresholds (%s): $X^2(%d) = %s$, $p = %s$.}\\\\",
      n_cols, item_label_max, df_max, fmt2(max_X2), fmt3(p_min)
    )
  }
  
  item_lr_min_line <- if (!is.na(p_max) && p_max < 0.001) {
    sprintf(
      "    \\multicolumn{%d}{l}{Min item-level LR DIF on thresholds (%s): $X^2(%d) = %s$, $p < .001$.}\\\\",
      n_cols, item_label_min, df_min, fmt2(min_X2)
    )
  } else {
    sprintf(
      "    \\multicolumn{%d}{l}{Min item-level LR DIF on thresholds (%s): $X^2(%d) = %s$, $p = %s$.}\\\\",
      n_cols, item_label_min, df_min, fmt2(min_X2), fmt3(p_max)
    )
  }
}

diag_block <- c(
  "    \\midrule\\midrule",
  sprintf(
    "    \\multicolumn{%d}{l}{\\textbf{Scalar invariance diagnostics (MG graded IRT)}}\\\\",
    n_cols
  ),
  global_lr_line,
  item_lr_max_line,
  item_lr_min_line
)

## Extract thresholds & build rows
cf <- coef(mg_metric, IRTpars = TRUE, simplify = TRUE)
D_const <- 1.702  ## Translating between logistic and probit scales for consistency

extract_probit_thresholds <- function(cf_group, group_label, item_names = NULL,
                                      D = D_const) {
  item_pars <- cf_group$items
  if (is.null(item_names)) item_names <- rownames(item_pars)
  
  item_pars <- item_pars[item_names, , drop = FALSE]
  pars <- as.data.frame(item_pars)
  
  a      <- pars[["a"]]
  b_cols <- grep("^b", colnames(pars), value = TRUE)
  b_mat  <- as.matrix(pars[, b_cols, drop = FALSE])
  
  ## Probit scale thresholds
  alpha   <- a / D
  tau_mat <- b_mat * alpha
  
  out <- as.data.frame(tau_mat)
  out$item  <- rownames(pars)
  out$group <- group_label
  
  out |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(colnames(tau_mat)),
      names_to  = "threshold",
      values_to = "tau_probit"
    )
}

item_raw    <- rownames(cf[[1]]$items)
group_names <- names(cf)   # "152", "188"

probit_thr_list <- lapply(group_names, function(g) {
  extract_probit_thresholds(cf[[g]], group_label = g, item_names = item_raw)
})
probit_thresholds <- bind_rows(probit_thr_list)

probit_thresholds_wide <- probit_thresholds |>
  tidyr::pivot_wider(
    id_cols     = c(item, group),
    names_from  = threshold,
    values_from = tau_probit
  )

## Probit discriminations
disc_logistic <- cf[["152"]]$items[item_order, "a"]
disc_probit   <- disc_logistic / D_const
names(disc_probit) <- item_order

## Chile is country (152), Costa Rica is (188)
tau_chile <- probit_thresholds_wide |>
  dplyr::filter(group == "152") |>
  dplyr::arrange(match(item, item_order))

tau_cr <- probit_thresholds_wide |>
  dplyr::filter(group == "188") |>
  dplyr::arrange(match(item, item_order))

stopifnot(identical(tau_chile$item, item_order))
stopifnot(identical(tau_cr$item,   item_order))

tau_diff <- tau_cr
tau_diff[, c("b1","b2","b3")] <-
  tau_cr[, c("b1","b2","b3")] - tau_chile[, c("b1","b2","b3")]

## Flattening vectors (τ1, τ2, τ3 for each item)
row_chile_vals_num <- unlist(lapply(seq_along(item_order), function(i) {
  c(tau_chile$b1[i], tau_chile$b2[i], tau_chile$b3[i])}))

row_cr_vals_num <- unlist(lapply(seq_along(item_order), function(i) {
  c(tau_cr$b1[i], tau_cr$b2[i], tau_cr$b3[i])}))

row_diff_vals_num <- unlist(lapply(seq_along(item_order), function(i) {
  c(tau_diff$b1[i], tau_diff$b2[i], tau_diff$b3[i])}))

## Adding the average DIF per item
avg_diff_by_item <- vapply(
  seq_along(item_order),
  function(i) mean(c(tau_diff$b1[i], tau_diff$b2[i], tau_diff$b3[i])),
  numeric(1)
)

row_avg_vals_num <- unlist(lapply(avg_diff_by_item, function(m) c(NA, m, NA)))

row_chile_vals_chr <- fmt2(row_chile_vals_num)
row_cr_vals_chr    <- fmt2(row_cr_vals_num)
row_diff_vals_chr  <- fmt2(row_diff_vals_num)
row_avg_vals_chr   <- fmt2(row_avg_vals_num)

## Building final LaTeX table
col_spec <- paste0("l", paste(rep("rrr", n_items), collapse = ""))

## Header row 1: item labels in bold
header_items <- paste(
  sapply(item_order, function(code) {
    lbl <- item_labels[[code]]
    sprintf("\\multicolumn{3}{c}{\\textbf{%s}}", lbl)
  }),
  collapse = " & "
)

header1 <- paste0("\\textbf{Country} & ", header_items, " \\\\")

## Header row 2: τ1, τ2, τ3 (not bolded per your request)
subcols <- rep(c("$\\tau_{1}$", "$\\tau_{2}$", "$\\tau_{3}$"),
               times = n_items)
header2 <- paste(c("", subcols), collapse = " & ")
header2 <- paste0(header2, " \\\\")

## Body rows
row_chile_tex <- paste(c("Chile",      row_chile_vals_chr), collapse = " & ")
row_chile_tex <- paste0(row_chile_tex, " \\\\")

row_cr_tex <- paste(c("Costa Rica",   row_cr_vals_chr), collapse = " & ")
row_cr_tex <- paste0(row_cr_tex, " \\\\")

row_diff_tex <- paste(c("Difference", row_diff_vals_chr), collapse = " & ")
row_diff_tex <- paste0(row_diff_tex, " \\\\")

row_avg_tex <- paste(c("Average diff.", row_avg_vals_chr), collapse = " & ")
row_avg_tex <- paste0(row_avg_tex, " \\\\")

## Sample sizes per country for the table note

tab_group <- table(group)
# assumes 'group' coded 152 = Chile, 188 = Costa Rica
n_chile <- as.integer(tab_group[["152"]])
n_cr    <- as.integer(tab_group[["188"]])

disc_note_parts <- sapply(item_order, function(code) {
  lbl <- item_labels[[code]]
  sprintf("%s = %s", lbl, fmt2(disc_probit[[code]]))
})
disc_note <- paste(disc_note_parts, collapse = "; ")

note_line <- paste0(
  "  \\footnotesize\\emph{Note}: Entries are probit-scale difficulty thresholds ",
  "($\\tau_1$--$\\tau_3$) from a graded response model with metric invariance ",
  "across Chile and Costa Rica, where larger $\\tau$ indicates a more demanding ",
  "category step at a fixed latent trait level. Probit-scale discriminations ",
  "($\\alpha$) by item: ", disc_note, ". ",
  "Sample sizes: Chile $N = ", n_chile, "$, Costa Rica $N = ", n_cr, "$."
)

tab_lines <- c(
  "\\begin{table}[ht]",
  "  \\centering",
  "  \\caption{Probit-scale thresholds for democracy support items in Chile and Costa Rica.}",
  "  \\label{tab:probit_thresholds_cr_ch}",
  paste0("  \\begin{tabular}{", col_spec, "}"),
  "    \\toprule",
  paste0("    ", header1),
  "    ",
  paste0("    ", header2),
  "    \\midrule",
  paste0("    ", row_chile_tex),
  paste0("    ", row_cr_tex),
  "    \\midrule",
  paste0("    ", row_diff_tex),
  paste0("    ", row_avg_tex),
  diag_block,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.5em}",
  note_line,
  "\\end{table}"
)

mg_scalar_m <- multipleGroup(
  data     = item_data,
  model    = 1,
  group    = group,
  itemtype = "graded",
  invariance = c("slopes", "intercepts", "free_means", "free_var"), # Imposing scalar invariance
  technical = list(NCYCLES = 6000),
  SE       = TRUE)

cf_scalar <- coef(mg_scalar_m, IRTpars = TRUE, simplify = TRUE)

# Group-wise latent means
group_means <- sapply(cf_scalar, function(x) x$means)
group_cov <- sapply(cf_scalar, function(x) x$cov) 

group_means/group_cov ## Mean is -0.568 std. units lower in Costa Rica than in Chile

writeLines(tab_lines, "probit_thresholds_democracy_CR_CH.tex")

########### Bootrapping IRT threshold patterns (Table 2) ###########

bootstrap_threshold_sums_centered <- function(
    data,
    items,
    groupvar,
    n_boot   = 1000,
    n_cores  = max(1, future::availableCores() - 2),
    technical = list(NCYCLES = 3000)) {
  
  ## Cleaning and prepping Latinobarometer data
  dat <- data[, c(items, groupvar)]
  dat[[groupvar]] <- factor(dat[[groupvar]])
  groups <- levels(dat[[groupvar]])
  G <- length(groups)
  
  ## Per-country sample sizes and weights (fixed across bootstraps)
  n_by_group <- table(dat[[groupvar]])
  w <- as.numeric(n_by_group / sum(n_by_group))  # weights in same order as 'groups'
  
  ## Helper: convert items to numeric (if not already)
  make_item_numeric <- function(x) {
    if (is.numeric(x)) x else as.numeric(as.factor(x))
  }
  
  ## Constant for logistic -> probit
  D_const <- 1.702
  
  ## Setting up worker function to run in parallel later (one bootstrap per replicate)
  worker_fun <- function(b) {
    ## Resampling with replacement within each country
    idx_boot <- unlist(lapply(groups, function(g) {
      idx_g <- which(dat[[groupvar]] == g)
      sample(idx_g, size = length(idx_g), replace = TRUE)
    }))
    dat_b <- dat[idx_boot, , drop = FALSE]
    
    group_b     <- factor(dat_b[[groupvar]], levels = groups)
    item_data_b <- dat_b[, items, drop = FALSE]
    item_data_b[] <- lapply(item_data_b, make_item_numeric)
    
    ## Fit metric MG graded IRT
    fit <- try(
      multipleGroup(
        data      = item_data_b,
        model     = 1,
        group     = group_b,
        itemtype  = "graded",
        invariance = c("slopes"),  # Assumping equal slopes (metric invariance) here
        SE        = FALSE,
        technical = technical
      ),
      silent = TRUE
    )
    if (inherits(fit, "try-error")) {
      ## return NAs for failed bootstraps
      return(rep(NA_real_, G))
    }
    
    ## Extract IRT parameters per group (IRTpars=TRUE to get 'a' and 'b*')
    cf <- try(coef(fit, IRTpars = TRUE, simplify = TRUE), silent = TRUE)
    if (inherits(cf, "try-error")) {
      return(rep(NA_real_, G))
    }
    
    ## Summing probit difficulty thresholds across items & thresholds for each group 
    sums_tau <- numeric(G)
    names(sums_tau) <- groups
    
    for (g in groups) {
      if (!g %in% names(cf)) {
        sums_tau[g] <- NA_real_
        next
      }
      
      item_pars <- cf[[g]]$items
      
      ## Restrict to target items (and drop any non-item rows)
      item_pars <- item_pars[items, , drop = FALSE]
      
      if (!("a" %in% colnames(item_pars))) {
        sums_tau[g] <- NA_real_
        next
      }
      a <- item_pars[, "a"]
      
      b_cols <- grep("^b", colnames(item_pars), value = TRUE)
      if (length(b_cols) == 0L) {
        sums_tau[g] <- NA_real_
        next
      }
      b_mat <- as.matrix(item_pars[, b_cols, drop = FALSE])
      
      ## Obtaining probit discriminations and difficulty thresholds
      alpha <- a / D_const
      
      ## Multiplying alpha_j across thresholds for each item
      tau_mat <- b_mat * alpha
      
      # Sum over items and thresholds
      sums_tau[g] <- sum(tau_mat)
    }
    
    as.numeric(sums_tau)
  }
  
  ## Running bootstraps in parallel via planed multisession
  plan(multisession, workers = n_cores)
  
  boot_mat <- future.apply::future_sapply(
    X = seq_len(n_boot),
    FUN = function(b) worker_fun(b),
    future.seed = TRUE
  )
  
  boot_mat <- t(boot_mat)
  colnames(boot_mat) <- groups
  
  ## Dropping bootstraps where model failed within group 
  valid_rows <- apply(boot_mat, 1, function(z) all(!is.na(z)))
  boot_mat <- boot_mat[valid_rows, , drop = FALSE]
  
  ## Centering on estimated global (sample-weighted) mean
  global_mean <- as.numeric(boot_mat %*% w)
  
  ## Centered sums:
  boot_centered <- boot_mat - global_mean
  
  ## Exporting per-country summaries (raw & centered)
  summary_df <- data.frame(
    country          = groups,
    n                = as.numeric(n_by_group[groups]),
    median_raw       = NA_real_,
    lo_raw           = NA_real_,
    hi_raw           = NA_real_,
    median_centered  = NA_real_,
    lo_centered      = NA_real_,
    hi_centered      = NA_real_
  )
  
  for (i in seq_along(groups)) {
    s_vals <- boot_mat[, i]
    c_vals <- boot_centered[, i]
    
    summary_df$median_raw[i]      <- median(s_vals)
    summary_df$lo_raw[i]          <- quantile(s_vals, 0.025)
    summary_df$hi_raw[i]          <- quantile(s_vals, 0.975)
    
    summary_df$median_centered[i] <- median(c_vals)
    summary_df$lo_centered[i]     <- quantile(c_vals, 0.025)
    summary_df$hi_centered[i]     <- quantile(c_vals, 0.975)
  }
  
  ## Between-country standard deviation of sums
  sd_between <- apply(boot_mat, 1, sd)
  
  dispersion_summary <- c(
    median_sd = median(sd_between),
    lo_sd     = quantile(sd_between, 0.025),
    hi_sd     = quantile(sd_between, 0.975)
  )
  
  list(
    boot_raw           = boot_mat,
    boot_centered      = boot_centered,
    global_mean        = global_mean,
    per_country        = summary_df,
    between_country_sd = dispersion_summary
  )
}


## Subsetting and imputing missing with RF
# Latinobaro_mie <- Latinobaro_sub %>% select(country_name, P12STGBS, P43ST_A, P43TGB_B, P43GBS_C, 
#                                             P43GBS_E, P50GBS_F) %>% filter(!country_name %in% c("Spain"))
# Latinobaro_miec <- Latinobaro_mie
# Latinobaro_miec$country_name <- NULL
# Latinobaro_miec <- missForest(as.matrix(Latinobaro_miec))$ximp
# Latinobaro_miec <-  round(Latinobaro_miec, 0) %>% as.data.frame()
# 
# Latinobaro_miec$country_name <- Latinobaro_mie$country_name
# 
# write.csv(Latinobaro_miec, "Latinobaro_miec.csv",row.names = FALSE)
# 
Latinobaro_miec <- read.csv("Latinobaro_miec.csv")

## Running bootstrapp in parallel

# items    <- c("P43GBS_E", "P43TGB_B", "P43ST_A", "P43GBS_C")
# groupvar <- "country_name"
# 
# boot_res <- bootstrap_threshold_sums_centered(
#   data      = Latinobaro_miec,
#   items     = items,
#   groupvar  = groupvar,
#   n_boot    = 2000,
#   n_cores   = max(1, future::availableCores() - 2),
#   technical = list(NCYCLES = 3000)
# )
# 
# inv_emp_sim <-  boot_res$per_country %>%
#   mutate(sig_vs_global = ifelse(lo_centered > 0 | hi_centered < 0, TRUE, FALSE))
# 
# saveRDS(inv_emp_sim, "inv_emp_sim.rds")
inv_emp_sim <- readRDS("inv_emp_sim.rds")

##### Generating bootstrap output table (Table 2) #####
tbl <- inv_emp_sim %>%
  dplyr::select(country, n, median_raw, median_centered, lo_centered, hi_centered)

## Formatting to two decimal places
fmt2 <- function(x) sprintf("%.2f", x)

## Column specifications: one for text and next five numeric
col_spec <- "lrrrrr"

## Header row
header <- paste(
  "Country",
  "$n$",
  "$\\sum_k \\tau_{kg}$",
  "$\\sum_k \\tau_{kg} - \\bar{\\tau}$",
  "$\\text{CI}_{0.025}$",
  "$\\text{CI}_{0.975}$",
  sep = " & "
)
header <- paste0("    ", header, " \\\\")

# Build body rows, bolding significant rows
rows_tex <- vector("character", nrow(tbl))

for (i in seq_len(nrow(tbl))) {
  row_i <- tbl[i, ]
  country    <- as.character(row_i$country)
  n_i        <- row_i$n
  med_raw    <- fmt2(row_i$median_raw)
  med_cent   <- fmt2(row_i$median_centered)
  lo_cent    <- fmt2(row_i$lo_centered)
  hi_cent    <- fmt2(row_i$hi_centered)
  
  ## Adding rule that if not significant vs global diplay in bold face
  if (!inv_emp_sim$sig_vs_global[i]) {
    country  <- sprintf("\\textbf{%s}", country)
    n_i      <- sprintf("\\textbf{%d}", n_i)
    med_raw  <- sprintf("\\textbf{%s}", med_raw)
    med_cent <- sprintf("\\textbf{%s}", med_cent)
    lo_cent  <- sprintf("\\textbf{%s}", lo_cent)
    hi_cent  <- sprintf("\\textbf{%s}", hi_cent)
  } else {
    n_i <- sprintf("%d", n_i)
  }
  
  row_tex <- paste(
    country,
    n_i,
    med_raw,
    med_cent,
    lo_cent,
    hi_cent,
    sep = " & "
  )
  rows_tex[i] <- paste0("    ", row_tex, " \\\\")
}

# Put everything together into a LaTeX table
tab_lines <- c(
  "\\begin{table}[ht]",
  "  \\centering",
  "  \\caption{Bootstrap distribution of summed probit difficulty thresholds by country.}",
  "  \\label{tab:inv_empirical_sums}",
  paste0("  \\begin{tabular}{", col_spec, "}"),
  "    \\toprule",
  header,
  "    \\midrule",
  rows_tex,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{0.5em}",
  "  \\footnotesize\\emph{Note}: Entries are medians and 95\\% bootstrap intervals of the sum of probit-scale difficulty thresholds per country.",
  "  Rows in \\textbf{bold} indicate countries where the 95\\% interval for the centered sum excludes zero,",
  "  i.e., the summed thresholds differ significantly from the cross-national average under the metric-invariant model.",
  "\\end{table}"
)

writeLines(tab_lines, "inv_empirical_sums.tex")

###### Run measurement invariance explorer (online supplement) ########
# runMIE("dem  =~ P12STGBS + P43ST_A + P43TGB_B + P43GBS_C + P43GBS_E + P50GBS_F",
#        data = Latinobaro_miec, group = "country_name")