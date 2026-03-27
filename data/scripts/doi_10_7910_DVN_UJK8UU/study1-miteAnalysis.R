library(plyr)
library(tidyr)
library(dplyr) 
library(knitr)
library(texreg)
library(mma)
library(effsize)
library(xtable)


# Helper functions

# returns a subset of rows where all relevant vars are not na
get_only_complete_cases <- function(df, relevant_vars) {
  filtered_df <- df %>%
    filter(!if_any(all_of(relevant_vars), is.na))
  filtered_df
}

get_col_values_with_at_least_n_rows <- function(df, col_name, min_n_rows) {
  # Use dplyr to count and filter
  result <- df %>%
    group_by(!!sym(col_name)) %>%         # Group data by the 'race' column
    summarise(count = n()) %>% # Count the number of occurrences for each race
    filter(count >= min_n_rows) %>%   # Filter to keep races with at least 500 occurrences
    pull(!!sym(col_name))  
  result
}

prepare_data <- function(raw_data_file, additional_data_file, ggi_file) {
  raw_gender_data = read.csv(paste(raw_data_file, ".csv", sep=""))
  # Load additional demographic data
  additional_data = read.csv(paste(additional_data_file, ".csv", sep=""))
  # GGI data
  ggi = read.csv(ggi_file)
  # replace - with _ in measure names
  additional_data$key = gsub("-", "_", additional_data$key)
  # Create a single column containing collected values
  additional_data$value = paste(additional_data$int_value, additional_data$str_value, sep="")
  additional_data$value <- gsub("NA", "", additional_data$value)
  # Pivot to have one row per participant
  additional_data_wide <- pivot_wider(additional_data, names_from = key, values_from = value, id_cols = participant_id)
  additional_data_wide$race = paste(additional_data_wide$str_ethnicity_asian, additional_data_wide$str_ethnicity_black, additional_data_wide$str_ethnicity_latino, additional_data_wide$str_ethnicity_native_american, additional_data_wide$str_ethnicity_pacific, additional_data_wide$str_ethnicity_white, sep=",")
  additional_data_wide$race <- gsub(",NA", "", additional_data_wide$race)
  additional_data_wide$race <- gsub("NA", "", additional_data_wide$race)
  additional_data_wide$race <- gsub("^,", "", additional_data_wide$race)
  # save the transformed file
  write.csv(additional_data_wide, paste(additional_data_file, "_wide.csv", sep=""))
  
  # merge raw data with additional demographic data
  combined_data <- merge(raw_gender_data, additional_data_wide, by.x = "participant_id", by.y = "participant_id", all.x = TRUE)
  ## log age
  combined_data$log10_age = log10(combined_data$age)
  # adding squared log age as an explicit column
  combined_data$log10_age_sq <- combined_data$log10_age^2
  
  ## Merge with GGI data
  combined_data <- merge(combined_data, ggi, by.x = "country_young", by.y = "country", all.x = TRUE)
  combined_data <- rename(combined_data, "GGI_young" = "GGI")
  combined_data <- merge(combined_data, ggi, by.x = "country_now", by.y = "country", all.x = TRUE)
  combined_data <- rename(combined_data, "GGI_now" = "GGI")
  
  ## Making a nominal version of the gender, comprehension and web_usage variables:
  combined_data$gender.flipped = 1 - combined_data$gender # now 1 = male; 0 = female
  combined_data$gender.f = factor(combined_data$gender.flipped)
  combined_data$comprehension.f = factor(combined_data$comprehension)
  write.csv(combined_data, paste(raw_data_file, "-combined.csv", sep=""))
  
  # filter data for analyses
  relevant_vars = c("gender.f", "cmni27", "cmni43", "cmni41", "cmni24", "cmni8", "cmni25", "cmni36", "education", "comprehension.f", "log10_age", "score")
  filtered_combined_data <- data.frame(combined_data)
  # keep only men, women, non-binary
  filtered_combined_data <- subset(filtered_combined_data, gender.f == 0 | gender.f == 1 | gender.f == -1)
  # only participants aged 13 or older
  filtered_combined_data <- subset(filtered_combined_data, age >= 13)
  # Only keep rows where none of the relevant_vars are na
  filtered_combined_data <- get_only_complete_cases(filtered_combined_data, relevant_vars)
  filtered_combined_data <- subset(filtered_combined_data, education != "")
  
  write.csv(filtered_combined_data, paste(raw_data_file, "-combined-filtered.csv", sep=""))
  
  list(combined_data, filtered_combined_data)
}
  
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  print(paste("N = ", nrow(x)))
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
}

effect_sizes <- function(df) {
  print("********* EFFECT SIZES (binary gender on CMNI) ***********")
  extended_binary_gender_data_aux <- subset(df, gender.f == 0 | gender.f == 1)
  extended_binary_gender_data_aux$gender.n <- as.numeric(extended_binary_gender_data_aux$gender.f)
  print("score")
  print(cohen.d(score ~ gender.n, data=extended_binary_gender_data_aux))
  print("CMNI 27")
  print(cohen.d(cmni27 ~ gender.n, data=extended_binary_gender_data_aux))
  print("CMNI 43")
  print(cohen.d(cmni43 ~ gender.n, data=extended_binary_gender_data_aux))
  print("CMNI 41")
  print(cohen.d(cmni41 ~ gender.n, data=extended_binary_gender_data_aux))
  print("CMNI 24")
  print(cohen.d(cmni24 ~ gender.n, data=extended_binary_gender_data_aux))
  print("CMNI 8")
  print(cohen.d(cmni8 ~ gender.n, data=extended_binary_gender_data_aux))
  print("CMNI 25")
  print(cohen.d(cmni25 ~ gender.n, data=extended_binary_gender_data_aux))
  print("CMNI 36")
  print(cohen.d(cmni36 ~ gender.n, data=extended_binary_gender_data_aux))
}

# Start of the actual analyses

# names of the files containing data; exclude the extension
raw_data_file = "2024.11.03 - mite results"
additional_data_file = "2024.11.02 - mite results - additional_data"
ggi_file = "2020_ggi.csv"

# Prepare the data
prepared_data <- prepare_data(raw_data_file, additional_data_file, ggi_file)
combined_data = prepared_data[[1]]
filtered_combined_data = prepared_data[[2]]
filtered_combined_data_binary_only <- subset(filtered_combined_data, gender.f == 0 | gender.f == 1)

# Effect sizes for Fig 3
print("Effect sizes for Fig 3")
effect_sizes(filtered_combined_data)

# Table 2
print("Table 2")
correlations_binary_gender<-corstars(filtered_combined_data[c("cmni27", "cmni43", "cmni41", "cmni24", "cmni8", "cmni25", "cmni36")], method="pearson", removeTriangle="upper", result="latex")
# print(correlations_binary_gender)

# Data for Table 3 -- this takes a long time
print("Table 3")
mediation_analysis <- function(df, name="", n2=20) {
  # Only keeping binary genders for this analysis
  df <- subset(df, gender.f == 0 | gender.f == 1)
  # Slicing the data for the mma analysis
  df.x <- df[c("cmni27", "cmni43", "cmni41", "cmni24", "cmni8", "cmni25", "cmni36", "education", "comprehension.f", "log10_age", "log10_age_sq")]
  df.y <- df[c("score")]
  df.pred <- df[c("gender.f")]
  # set n2 to a higher number for the final analyses
  res <- mma(df.x, df.y, n2=n2, pred=df.pred, contmed=1:7, jointm=list(n=1, cmni=1:7), predref=0, alpha=0.05, alpha2 = 0.05)
  s <- summary(res)
  list(s = s, df = df, name = name)
}

process_mediation_analysis_results <- function(res) {
  s <- res$s
  df <- res$df
  name <- res$name
  
  df_main <- data.frame(
    "index" = c("N", "Total", "Direct"),
    "est" = c(nrow(df), s$bin.result$results$total.effect['est',], s$bin.result$results$direct.effect['est',]),
    "lwbd" = c(NA, s$bin.result$results$total.effect['lwbd',], s$bin.result$results$direct.effect['lwbd',]),
    "upbd" = c(NA, s$bin.result$results$total.effect['upbd',], s$bin.result$results$direct.effect['upbd',])
  )
  rownames(df_main) <- df_main$index
  df_main$index = NULL
  est_df_ind <- as.data.frame(s$bin.result$results$indirect.effect$gender.f['est',], nm="est")
  lwbd_df_ind <- as.data.frame(s$bin.result$results$indirect.effect$gender.f['lwbd',], nm="lwbd")
  upbd_df_ind <- as.data.frame(s$bin.result$results$indirect.effect$gender.f['upbd',], nm="upbd")
  
  df_ind <- cbind(est_df_ind, lwbd_df_ind, upbd_df_ind, deparse.level = 1)
  
  est_df <- rbind(df_main, df_ind)
  est_df$est_str = sprintf("%.3f", est_df$est)
  est_df$CI_str = paste("[", sprintf("%.3f", est_df$lwbd), ", ", sprintf("%.3f", est_df$upbd), "]", sep="")
  est_df["N", "CI_str"] = ""
  est_df_str = est_df[c("est_str", "CI_str")]
  est_df_num = est_df[c("est", "lwbd", "upbd")]
  
  colnames(est_df_num)[colnames(est_df_num) == "est"] <- paste("est (", name, ")", sep="")
  colnames(est_df_num)[colnames(est_df_num) == "lwbd"] <- paste("lwbd (", name, ")", sep="")
  colnames(est_df_num)[colnames(est_df_num) == "upbd"] <- paste("upbd (", name, ")", sep="")
  
  colnames(est_df_str)[colnames(est_df_str) == "est_str"] <- paste("est (", name, ")", sep="")
  colnames(est_df_str)[colnames(est_df_str) == "CI_str"] <- paste("CI (", name, ")", sep="")
  
  list(est_df_num = est_df_num, est_df_str = est_df_str)
}

compare_mediation_analyses <- function(df, category_col, category_values, n2=20) {
  res_df = NULL
  res_raw_df = NULL
  for(category_val in category_values) {
    df_subset = df
    if (category_val != "")
      df_subset <- subset(df, eval(as.name(category_col)) == category_val)
    cat("****** " , category_val, " (N =", nrow(df_subset), ") *********")
    analysis_res = mediation_analysis(df_subset, name=category_val, n2=n2)
    res = process_mediation_analysis_results(analysis_res)
    if (is.null(res_df)) {
      res_df = res$est_df_str
      res_raw_df = res$est_df_num
    } else {
      res_df = cbind(res_df, res$est_df_str)
      res_raw_df = cbind(res_raw_df, res$est_df_num)      
    }
    write.csv(res_raw_df, paste("mediation_analyses_raw-", category_col, "-", n2, ".csv", sep = ""))
    write.csv(res_df, paste("mediation_analyses-", category_col, "-", n2, ".csv", sep = ""))
  }
  list(res_df, res_raw_df)
}
mediation_analyses_all_results = compare_mediation_analyses(filtered_combined_data, "all", c(""), n2=100)

# Data for Table 4
print("Table 4 and the accompanying diagram")
main_regression_analyses <- function(df, print_results = TRUE, name="", include_nb = FALSE) {
  if (include_nb) 
    extended_binary_gender_data <- subset(df, gender.f == 0 | gender.f == 1 | gender.f == -1)
  else
    extended_binary_gender_data <- subset(df, gender.f == 0 | gender.f == 1)
  relevant_vars = c("gender.f", "cmni27", "cmni43", "cmni41", "cmni24", "cmni8", "cmni25", "cmni36", "GGI_young", "GGI_now", "education", "comprehension.f", "log10_age", "score", "country_young", "country_now")
  extended_binary_gender_data <- get_only_complete_cases(extended_binary_gender_data, relevant_vars)
  
  m0 <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2), data=extended_binary_gender_data)
  m_gender <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + gender.f, data=extended_binary_gender_data)
  m_cmni <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 +cmni36, data=extended_binary_gender_data)
  m_ggi <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + GGI_young + GGI_now, data=extended_binary_gender_data)
  m_gender_cmni <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + gender.f + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36, data=extended_binary_gender_data)
  m_gender_ggi <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + gender.f + GGI_young + GGI_now, data=extended_binary_gender_data)
  m_gender_cmni_ggi <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + gender.f + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36 + GGI_young + GGI_now, data=extended_binary_gender_data)
  m_gender_cmni_interactions <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + gender.f + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36 + gender.f:cmni27 + gender.f:cmni43 + gender.f:cmni41 + gender.f:cmni24 + gender.f:cmni8 + gender.f:cmni25 + gender.f:cmni36, data=extended_binary_gender_data)
  m_gender_ggi_interactions <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + gender.f + GGI_young + GGI_now + gender.f:GGI_young + gender.f:GGI_now, data=extended_binary_gender_data)
  
  m0_summary <- summary(m0)
  m_gender_summary <- summary(m_gender)
  m_cmni_summary <- summary(m_cmni)
  m_gender_cmni_summary <- summary(m_gender_cmni)
  
  deltaR2a_gender_0 = m_gender_summary$adj.r.squared - m0_summary$adj.r.squared
  deltaR2a_cmni_0 = m_cmni_summary$adj.r.squared - m0_summary$adj.r.squared
  deltaR2a_genderCMNI_gender = m_gender_cmni_summary$adj.r.squared - m_gender_summary$adj.r.squared
  
  if (print_results) {
    print(paste)
    print(texreg( list( m_gender, m_gender_cmni, m_gender_ggi, m_gender_cmni_ggi ), single.row = TRUE, dcolumn = TRUE ))
    print(texreg( list( m_gender, m_gender_cmni, m_gender_ggi, m_gender_cmni_ggi ), single.row = TRUE, digits=3, dcolumn = TRUE ))
    
    print(texreg::screenreg( list( m0, m_gender, m_cmni, m_ggi, m_gender_cmni, m_gender_ggi, m_gender_cmni_ggi, m_gender_cmni_interactions, m_gender_ggi_interactions ), single.row = TRUE, digits = 3 ))
    
    print("Gender to m0")
    print(anova(m0, m_gender))
    print("CMNI to m0")
    print(anova(m0, m_cmni))
    print("GGI to m0")
    print(anova(m0, m_ggi))
    print("Gender+CMNI to gender only")
    print(anova(m_gender, m_gender_cmni))
    print("Gender+CMNI to CMNI only")
    print(anova(m_cmni, m_gender_cmni))
    print("Gender+GGI to gender only")
    print(anova(m_gender, m_gender_ggi))
    print("Gender+CMNI+GGI to gender+CMNI")
    print(anova(m_gender_cmni, m_gender_cmni_ggi))
    print("Gender+CMNI+GGI to gender+GGI")
    print(anova(m_gender_ggi, m_gender_cmni_ggi))
    print("Gender+CMNI with interactions to Gender+CMNI")
    print(anova(m_gender_cmni, m_gender_cmni_interactions))
    print("Gender+GGI with interactions to Gender+GGI")
    print(anova(m_gender_ggi, m_gender_ggi_interactions))
  }
  list(name = name,
       n = nrow(extended_binary_gender_data),
       m1R2a = m0_summary$adj.r.squared, 
       m2aR2a = m_gender_summary$adj.r.squared,
       m2bR2a = m_cmni_summary$adj.r.squared,
       m3aR2a = m_gender_cmni_summary$adj.r.squared,
       delta_m2am1 = deltaR2a_gender_0,
       delta_m2bm1 = deltaR2a_cmni_0,
       delta_m3am2a = deltaR2a_genderCMNI_gender
  ) 
}

main_regression_analyses(filtered_combined_data)

# Data for Figure 5 -- a random subsample is created each time so the results will differ a little from run to run
print("Data for Figure 5")
balanced_main_regression_analyses <- function(df) {
  relevant_vars = c("gender.f", "cmni27", "cmni43", "cmni41", "cmni24", "cmni8", "cmni25", "cmni36", "GGI_young", "GGI_now", "education", "comprehension.f", "log10_age", "score", "country_young", "country_now")
  df <- get_only_complete_cases(df, relevant_vars)
  min_size <- nrow(subset(df, gender.f == -1))
  df$gender.f <- droplevels(df$gender.f)
  # Create the balanced subsample
  balanced_df <- do.call(rbind, lapply(split(df, df$gender.f), function(x) {
    x[sample(nrow(x), min_size), ]
  }))
  print(table(balanced_df$gender.f))
  main_regression_analyses(balanced_df, include_nb = TRUE)
}
balanced_main_regression_analyses(filtered_combined_data)

# Table 5
print("Table 5")
men_gender_data <- subset(filtered_combined_data, gender.f == 1)
women_gender_data <- subset(filtered_combined_data, gender.f == 0)
nb_gender_data <- subset(filtered_combined_data, gender.f == -1)

men_cmni <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36, data=men_gender_data)
women_cmni <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36, data=women_gender_data)
nb_cmni <- lm (score ~ education + comprehension.f + log10_age + I(log10_age^2) + cmni27 + cmni43 + cmni41 + cmni24 + cmni8 + cmni25 + cmni36, data=nb_gender_data)

print(texreg::screenreg( list( men_cmni, women_cmni, nb_cmni ), single.row = TRUE ))

# Tables 10 and 11
print("Tables 10 and 11")

do_compare_mediation_analyses <- function(df, n2=100) {
  # only binary genders for this analysis
  df <- subset(df, gender.f == 0 | gender.f == 1)
  races <- get_col_values_with_at_least_n_rows(subset(df, int_num_ethnicity_options_selected > 0), "race", 2000)
  df_countries <- subset(df, country_now == country_young & country_young != "")
  countries <- get_col_values_with_at_least_n_rows(df_countries, "country_young", 2000)
  races_res = compare_mediation_analyses(df, "race", races, n2=n2)
  countries_res = compare_mediation_analyses(df_countries, "country_young", countries, n2=n2)
  list(races_res, countries_res)
}
mediation_analyses_intersectionality_results = do_compare_mediation_analyses(filtered_combined_data, n2=100)

# Data for Tables 12 and 13
print("Tables 12 and 13")

compare_main_regression_models <- function(df, category_col, category_values) {
  res_df = NULL
  for(category_val in category_values) {
    df_subset = df
    if (category_val != "")
      df_subset <- subset(df, eval(as.name(category_col)) == category_val)
    cat("****** " , category_val, " (N =", nrow(df_subset), ") *********\n")
    res = main_regression_analyses(df_subset, print_results = FALSE, name=category_val)
    if (is.null(res_df)) {
      res_df = data.frame(res)
    } else {
      res_df = rbind(res_df, data.frame(res))
    }
  }
  # transpose res_df
  res_df <- as.data.frame(t(res_df))
  write.csv(res_df, paste("main_regression_analyses-", category_col, ".csv", sep = ""))
  res_df
}

do_compare_main_regression_analyses <- function(df) {
  # only binary genders for this analysis
  df <- subset(df, gender.f == 0 | gender.f == 1)
  races <- get_col_values_with_at_least_n_rows(subset(df, int_num_ethnicity_options_selected > 0), "race", 2000)
  df_countries <- subset(filtered_combined_data, country_now == country_young & country_young != "")
  countries <- get_col_values_with_at_least_n_rows(df_countries, "country_young", 2000)
  races_res = compare_main_regression_models(df, "race", races)
  countries_res = compare_main_regression_models(df_countries, "country_young", countries)
  list(races_res, countries_res)
}

main_regression_analyses_intersectionality_results = do_compare_main_regression_analyses(filtered_combined_data)


