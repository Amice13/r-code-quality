library(dplyr)
library(texreg)
library(Amelia)
library(broom)
library(sandwich)
library(lmtest)
library(rlang) 
library(mitools)

raw <- read.csv("yourfilepath.csv")

res <- raw %>%
  # convert 9s to NA across DV columns
  mutate(across(c(supp_group_radgen, supp_group_rad, supp_group_tradgen,
                  supp_protest_radgen, supp_protest_rad, supp_protest_tradgen,
                  succeed_radgen, succeed_rad, succeed_tradgen), ~ na_if(., 9))) %>%
  # assign treatment based on which DV column is non-missing
  mutate(treatment = case_when(
    !is.na(supp_group_tradgen)  ~ 1,
    !is.na(supp_group_rad)  ~ 2,
    !is.na(supp_group_radgen) ~ 3
  )) %>%
  # aggregate into single support variable
  mutate(support_group = coalesce(supp_group_radgen, supp_group_rad, supp_group_tradgen)) %>%
  mutate(support_protest = coalesce(supp_protest_radgen, supp_protest_rad, supp_protest_tradgen)) %>%
  mutate(succeed = coalesce(succeed_radgen, succeed_rad, succeed_tradgen)) %>%
  select(-starttime, -endtime, -consent)

# Reverse a 1–5 scale. Make 5 = high support/success and 1 = low support/success
res$support_group <- 6 - res$support_group
res$support_protest <- 6 - res$support_protest
res$succeed <- 6 - res$succeed
res$appropriate <- 6 - res$appropriate

# covariates
res$age        <- 2025 - res$birthyr
res$college    <- ifelse(res$educ >= 3, 1, 0)                 # 3 = some college+
res$race       <- ifelse(res$race == 1, 0, 1)                 # white=0, nonwhite=1
res$employed   <- ifelse(res$employ %in% c(1,2), 1, 0)        # employed=1, else 0
res$faminc_new <- ifelse(res$faminc_new == 97, NA, res$faminc_new)
res$children   <- ifelse(res$child18 == 1, 1, 0)
res$gender<- res$gender - 1
# yougov: pid3 1=D, 2=R, 3=I  -> my code: 1=D, 2=I, 3=R
# Collapse pid7 and mark only "Not sure" as missing (impute these only)
res <- res %>%
  mutate(
    # treat 9 ("Don't know") as NA
    pid7 = ifelse(pid7 == 9L, NA_integer_, pid7),
    
    # 3-category factor: Dem / Ind / Rep
    pid_cat = case_when(
      pid7 %in% 1:3 ~ "Democrat",
      pid7 == 4L    ~ "Independent",
      pid7 %in% 5:7 ~ "Republican",
      pid7 == 8L    ~ NA_character_   # only 'Not sure' becomes NA
    ) |> factor(levels = c("Democrat","Independent","Republican")),
    
    #  Numeric version
    #  1 = Dem, 0 = Ind, -1 = Rep; NA for 'Not sure'
    pid = case_when(
      pid7 %in% 1:3 ~  1L,
      pid7 == 4L    ~  0L,
      pid7 %in% 5:7 ~ -1L,
      pid7 == 8L    ~ NA_integer_
    )
  )


######################################################################

res <- res %>%
  mutate(
    other_norm = tolower(trimws(protest_past_other)),
    protest_past = {
      any_high  <- (protest_past_1 == 1) | (protest_past_3 == 1)
      any_light <- (protest_past_2 == 1) | (protest_past_4 == 1)
      any_other <- (protest_past_5 == 1)
      
      base <- case_when(
        any_high  ~ 2L,
        any_light ~ 1L,
        protest_past_6 == 1 & !any_high & !any_light & !any_other ~ 0L,
        TRUE ~ NA_integer_
      )
      
      other_code <- case_when(
        # None (0)
        other_norm %in% tolower(c(
          "Are you for real expect ppl writing they did vandalism etc?",
          "I choose not to identify what sorts of protest I have engaged in",
          "Look protesting is for people who don't understand life and don't care about anybody else's feelings they only hear what they want to hear and what they want to do with their life and trying to control people so I don't do protesting to me it should be against the law",
          "Red Pilling online","events","Protest","Phone zaps"
        )) ~ 0L,
        
        # Some (1)
        other_norm %in% tolower(c(
          "Protesting on a Sidewalk with a sign.","voting against certain politicians who support violence",
          "envelope stuffing","Marching but never blocked traffic",
          "March protest not blocking buildings or traffic","Social Media posting",
          "anti-war Vietnam","ralllies","Boycott woke companies",
          "Buying merch that supports a cause","anti abortion","pot protest 1970s",
          "I run stste and local aniti criminal immagration","Supporting organizations",
          "Calls to Reps","Signing petitions","Letters to the editor.","Peaceful protest",
          "60 years ago, I marched against the Vietnam War","social media campaigns",
          "Walk outs","Peaceful rallies, not blocking traffic",
          "Peacefully expressing my distain of communism/socialism",
          "I simply avoid buying certain products and avoid doing business with certain businesses.",
          "social media","Attend rallies","Appearance before a government board.",
          "Non-violent counter-protest","Peaceful","Petition,phone calls","Petition",
          "Sharing protest material on social media","voting","Emails","internet comments",
          "Picketing, silent protest","Marching","Peaceful rally",
          "Shareholder advocacy, donating to advocacy groups (both for-profit groups and non-profit)",
          "Public prayer vigils","Gathering signatures for petitions.",
          "Holding signs and shouting but not blocking streets or access to buildings",
          "showing up and marching in a group with signs, but without breaking the law (like blocking traffic or causing property damage) -- only peaceful protest",
          "Social Media. What else?","Writing opinions that were published to local newspaper",
          "Prayer gatherings","Signing petitions and letters","Honking for roadside protests",
          "Peaceful Trump protest","marching but not blocking traffic or pedestrian traffic",
          "Dr. King's 1963 march and subsequent nonviolent civil rights and antiwar protests",
          "public gathering","standing on corner praying",
          "Protesting against the war in Vietnamese. Did NOT block traffic or buildings.",
          "Peaceful educational protests against a federal agencies abuse of federal injured workers",
          "marched against drugs with the police","Attended a demonstration",
          "Going to legislators offices to air my concerns en masse","Online petitions",
          "Signing a petition","Calling representatives",
          "Marching but never blocking traffic or buildings, I do not support violence or illegal activities",
          "Not use products"
        )) ~ 1L,
        
        # High (2)
        other_norm %in% tolower(c(
          "Direct action",
          "Marched, picketed, supported civil disobedience",
          "Revolution",
          "strikes, neighborhood assemblies, blockades, treesits, occupations",
          "beaten by police for protesting",
          "Doxxing",
          "Marching and picketing only"  
        )) ~ 2L,
        
        TRUE ~ NA_integer_
      )
      
      out <- pmax(base, other_code, na.rm = TRUE)
      out[is.infinite(out)] <- NA_integer_
      out
    }
  ) %>%
  select(-other_norm)



####################################
# Imputation 
####################################
### Keep only analysis-ready variables for MI
vars_keep <- c(
  "caseid","weight",                      # idvars (not imputed)
  "treatment",                            # nominal
  "support_group", "support_protest", "succeed", "appropriate", # Likert-ish
  "age","gender","race","faminc_new","children","college","employed","pid", "protest_past",
  "ideo5"                                 
)

surv_for_imp <- res %>%
  select(any_of(vars_keep)) %>%
  mutate(
    # nominal
    across(c(treatment, gender, race, children, college, employed, pid), ~ as.integer(.)),
    # ordinal
    across(c(support_group, support_protest, succeed, faminc_new, ideo5, protest_past), ~ as.integer(.)),
    # continuous
    age = as.numeric(age)
  )

### drop constant columns
is_const <- vapply(surv_for_imp, function(x) length(unique(na.omit(x))) <= 1, logical(1))
if (any(is_const)) {
  message("Dropping constant columns: ", paste(names(surv_for_imp)[is_const], collapse = ", "))
  surv_for_imp <- surv_for_imp[, !is_const, drop = FALSE]
}

### Amelia lists
ords <- intersect(c("support_group", "support_protest", "succeed","faminc_new","ideo5", "protest_past"), names(surv_for_imp))
noms <- intersect(c("treatment","gender","race","children","college","employed","pid"), names(surv_for_imp))

set.seed(12345)
N.chain <- 10
imputeData <- amelia(
  x = surv_for_imp,
  m = N.chain,
  idvars = intersect(c("caseid","weight"), names(surv_for_imp)),
  noms   = noms,
  ords   = ords,
  incheck = TRUE
)

####################################
# Build treatment-pair objects
####################################
imp12 <- imputeData
imp13 <- imputeData
imp23 <- imputeData
for(i in 1:N.chain){
  imp12$imputations[[i]] <- subset(imp12$imputations[[i]], treatment %in% c(1,2))
  imp13$imputations[[i]] <- subset(imp13$imputations[[i]], treatment %in% c(1,3))
  imp23$imputations[[i]] <- subset(imp23$imputations[[i]], treatment %in% c(2,3))
}


### pooling helper
pool_fit <- function(imp_obj, fml, digits = 3) {
  fits <- lapply(imp_obj$imputations, function(d) lm(formula = fml, data = d, weights = weight))
  tab <- Amelia::mi.combine(fits)
  Ns  <- sapply(imp_obj$imputations, function(d) nrow(model.frame(fml, data = d)))
  N   <- as.integer(median(Ns))
  z   <- ifelse(is.finite(tab$df), qt(0.975, df = tab$df), 1.96)
  tab$ci_low  <- tab$estimate - z * tab$std.error
  tab$ci_high <- tab$estimate + z * tab$std.error
  tab$N <- N
  # overwrite pvals with z-test approx
  tab$p.value <- 2 * (1 - pnorm(abs(tab$statistic)))
  if (!is.null(digits)) {
    num_cols <- vapply(tab, is.numeric, logical(1))
    tab[num_cols] <- lapply(tab[num_cols], function(x) round(x, digits))
  }
  print(tab)
  invisible(tab)
}


####################################
# ATEs
####################################

### support group
out_13_u <- pool_fit(imp13, support_group ~ I(treatment==3))
out_23_u <- pool_fit(imp23, support_group ~ I(treatment==3))

print(out_13_u); print(out_23_u)


### support protest
out_13_p <- pool_fit(imp13, support_protest ~ I(treatment==3))
out_23_p <- pool_fit(imp23, support_protest ~ I(treatment==3))

print(out_13_p); print(out_23_p)

### protest success
out_13_s <- pool_fit(imp13, succeed ~ I(treatment==3))
out_23_s <- pool_fit(imp23, succeed ~ I(treatment==3))

print(out_13_s); print(out_23_s)


####################################################################
### Controls
####################################################################

### Adjusted (age + gender + race + college + pid + protest past)
adj_rhs <- ~ I(treatment==3) + age + gender + race + college + pid + protest_past
out_13_a <- pool_fit(imp13, update.formula(support_group ~ 1, adj_rhs))
out_23_a <- pool_fit(imp23, support_group ~ I(treatment==3) + age + gender + race + college + pid +protest_past)

print(out_13_a); print(out_23_a)

### Adjusted (age + gender + race + college + protest past + pid)
adj_rhs <- ~ I(treatment==3) + age + gender + race + college + pid + protest_past
out_13_ap <- pool_fit(imp13, update.formula(support_protest ~ 1, adj_rhs))
out_23_ap <- pool_fit(imp23, support_protest ~ I(treatment==3) + age + gender + race + college + pid + protest_past)

print(out_13_ap); print(out_23_ap)

### Adjusted (age + gender + race + college + protest past + pid)
adj_rhs <- ~ I(treatment==3) + age + gender + race + college + pid + protest_past
out_13_as <- pool_fit(imp13, update.formula(succeed ~ 1, adj_rhs))
out_23_as <- pool_fit(imp23, succeed ~ I(treatment==3) + age + gender + race + college + pid + protest_past)

print(out_13_as); print(out_23_as)


#################################################################

####################################
# Mediation outcomes 
####################################

### not violent = 1, violent = 5
appropriate_13 <- pool_fit(imp13, appropriate ~ I(treatment==3))
appropriate_23 <- pool_fit(imp23, appropriate ~ I(treatment==3))
print(appropriate_13); print(appropriate_23)


#################################################################

####################################
# Heterogeneity outcomes 
####################################
### helper to subset every completed dataset inside an Amelia object
filter_imp_base <- function(imp_obj, expr) {
  for (i in seq_along(imp_obj$imputations)) {
    d <- imp_obj$imputations[[i]]
    imp_obj$imputations[[i]] <- d[with(d, eval(expr)), , drop = FALSE]
  }
  imp_obj
}

### men (0) and women (1)
imp23_men    <- filter_imp_base(imp23, quote(!is.na(gender) & gender == 0))
imp23_women  <- filter_imp_base(imp23, quote(!is.na(gender) & gender == 1))

###  run existing pooled regressions
out_23_u_m <- pool_fit(imp23_men,   support_group ~ I(treatment==3))
out_23_u_w <- pool_fit(imp23_women, support_group ~ I(treatment==3))
out_23_u_mc <- pool_fit(imp23_men,   support_group ~ I(treatment==3)+ age + race + college + pid + protest_past)
out_23_u_wc <- pool_fit(imp23_women, support_group ~ I(treatment==3)+ age + race + college + pid + protest_past)


out_23_p_m <- pool_fit(imp23_men,   support_protest ~ I(treatment==3))
out_23_p_w <- pool_fit(imp23_women, support_protest ~ I(treatment==3))
out_23_p_mc <- pool_fit(imp23_men,   support_protest ~ I(treatment==3)+ age + race + college + pid + protest_past)
out_23_p_wc <- pool_fit(imp23_women, support_protest ~ I(treatment==3)+ age + race + college + pid + protest_past)


out_23_s_m <- pool_fit(imp23_men,   succeed ~ I(treatment==3))
out_23_s_w <- pool_fit(imp23_women, succeed ~ I(treatment==3))
out_23_s_mc <- pool_fit(imp23_men,   succeed ~ I(treatment==3)+ age + race + college + pid + protest_past)
out_23_s_wc <- pool_fit(imp23_women, succeed ~ I(treatment==3)+ age + race + college + pid + protest_past)


# mediation
appropriate_23_m <- pool_fit(imp23_men, appropriate ~ I(treatment==3))
appropriate_23_w <- pool_fit(imp23_women, appropriate ~ I(treatment==3))


### Covariate balance 
covars <- c("age", "gender", "race", "college", "protest_past", "pid")

### Original
balance_raw <- lapply(covars, function(v) {
  m <- res %>%
    group_by(treatment) %>%
    summarize(mean = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
  p <- anova(lm(res[[v]] ~ factor(res$treatment)))$`Pr(>F)`[1]
  tibble::tibble(
    variable = v,
    source = "Original",
    treat1 = m$mean[m$treatment == 1],
    treat2 = m$mean[m$treatment == 2],
    treat3 = m$mean[m$treatment == 3],
    p = round(p, 3)
  )
}) %>%
  bind_rows()

### Average across imputations
N.chain <- length(imputeData$imputations)

balance_imp_list <- lapply(seq_len(N.chain), function(i) {
  df <- imputeData$imputations[[i]]
  lapply(covars, function(v) {
    m <- df %>%
      group_by(treatment) %>%
      summarize(mean = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
    p <- anova(lm(df[[v]] ~ factor(df$treatment)))$`Pr(>F)`[1]
    tibble::tibble(
      variable = v,
      treat1 = m$mean[m$treatment == 1],
      treat2 = m$mean[m$treatment == 2],
      treat3 = m$mean[m$treatment == 3],
      p = p
    )
  }) %>%
    bind_rows() %>%
    mutate(.imp = i)
})

balance_imp <- bind_rows(balance_imp_list)

### Pool by taking simple averages across imputations
balance_imp_avg <- balance_imp %>%
  group_by(variable) %>%
  summarize(
    source = "Imputed (avg)",
    treat1 = mean(treat1, na.rm = TRUE),
    treat2 = mean(treat2, na.rm = TRUE),
    treat3 = mean(treat3, na.rm = TRUE),
    p = round(mean(p, na.rm = TRUE), 3),
    .groups = "drop"
  )

### combine tables
balance_summary <- bind_rows(balance_raw, balance_imp_avg) %>%
  select(variable, source, treat1, treat2, treat3, p)

balance_summary

