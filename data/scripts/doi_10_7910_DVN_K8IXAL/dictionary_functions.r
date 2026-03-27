#' ---
#' title: "Catalysts for Change? Exploring Policy Relevance in Abstracts of Leading Energy Journals (ERSS)"
#' subtitle: "02_validate_dictionaries.R"
#' author: "Authors: Brian Boyle, Yen-Chieh Liao, Sarah King, Robin Rauner, and Stefan Müller"
#' ---

# load required R packages
library(quanteda) # CRAN v4.1.0
library(tidyverse) # CRAN v2.0.0
library(mltest) # CRAN v1.0.1
library(tidycomm) # CRAN v0.4.1


# Function to evaluate dictionary performance
evaluate_dictionary <- function(dictionary, corpus) {
  # Tokenize and process
  toks <- tokens(corpus,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    split_hyphens = FALSE,
    padding = TRUE
  ) |>
    tokens_tolower()

  # Apply dictionary
  dfmat <- tokens_lookup(toks, dictionary = dictionary) |> dfm()
  dfmat_weighted <- dfm_weight(dfmat, scheme = c("boolean"))
  dat_dict <- quanteda::convert(dfmat_weighted, to = "data.frame") |>
    cbind(docvars(dfmat_weighted))

  # Create cross-table (confusion matrix)
  cross_table <- table(
    dict = dat_dict$policy_statement_dict,
    true = dat_dict$true_policy_statement
  )

  # Get individual cell values from cross table
  tt <- cross_table["1", "1"] # true positive
  ff <- cross_table["0", "0"] # true negative
  tf <- cross_table["1", "0"] # false positive
  ft <- cross_table["0", "1"] # false negative

  # Calculate performance metrics
  ml_results <- ml_test(
    predicted = dat_dict$policy_statement_dict,
    true = dat_dict$true_policy_statement,
    output.as.table = TRUE
  )

  # Calculate average metrics
  avg_f1 <- mean(ml_results$F1)
  avg_precision <- mean(ml_results$precision)
  avg_recall <- mean(ml_results$recall)

  # Transform data to long format
  dat_merged_long <- dat_dict |>
    pivot_longer(
      cols = c("policy_statement_dict", "true_policy_statement"),
      names_to = "coder",
      values_to = "coded_policy"
    ) |>
    mutate(coder = dplyr::recode(coder,
      "policy_statement_dict" = "dict",
      "true_policy_statement" = "true"
    ))

  # Calculate inter-coder reliability
  icr_metrics <- dat_merged_long |>
    test_icr(article_scopus_id, coder, coded_policy)


  # Return performance metrics
  return(list(
    avg_f1 = avg_f1,
    avg_precision = avg_precision,
    avg_recall = avg_recall,
    f1_0 = ml_results$F1[1],
    f1_1 = ml_results$F1[2],
    k_alpha = icr_metrics$Krippendorffs_Alpha,
    true_true = as.numeric(tt),
    false_false = as.numeric(ff),
    true_false = as.numeric(tf),
    false_true = as.numeric(ft)
  ))
}

# Function to re-order terms in a dictionary and evaluate it
bootstrap_dictionary <- function(terms, corpus) {
  # Shuffle the terms
  shuffled_terms <- sample(terms)

  # Create a dictionary with the shuffled terms
  dict_policy <- dictionary(list(
    policy_statement_dict = shuffled_terms
  ))

  # Evaluate the dictionary
  evaluation_result <- evaluate_dictionary(dict_policy, corpus)

  return(list(terms = shuffled_terms, results = evaluation_result))
}

# Function to optimize dictionary terms using F1 Score
optimize_dictionary <- function(initial_terms, candidate_terms, corpus) {
  # Create the initial dictionary with the provided terms
  initial_dict <- dictionary(list(policy_statement_dict = initial_terms))
  initial_eval <- evaluate_dictionary(initial_dict, corpus)
  initial_f1 <- initial_eval$f1_1
  current_dict <- initial_dict
  current_f1 <- initial_f1

  # Create data frame to store results
  results_df <- data.frame(
    term = character(),
    k_alpha = numeric(),
    f1_score = numeric(),
    improvement = numeric(),
    stringsAsFactors = FALSE
  )

  # Iterate through all candidate terms
  for (term in candidate_terms) {
    # Add candidate term to the current dictionary
    test_dict <- dictionary(list(
      policy_statement_dict = c(current_dict$policy_statement_dict, term)
    ))

    # Evaluate the new dictionary
    new_eval <- evaluate_dictionary(test_dict, corpus)

    # Record results
    results_df <- rbind(results_df, data.frame(
      term = term,
      k_alpha = new_eval$k_alpha,
      f1_score = new_eval$f1_1,
      improvement = new_eval$f1_1 - initial_f1,
      stringsAsFactors = FALSE
    ))

    # Update if f1_1 improved
    if (new_eval$f1_1 > current_f1) {
      current_dict <- test_dict
      current_f1 <- new_eval$f1_1
    }
  }

  # Sort results by improvement
  results_df <- results_df[order(-results_df$improvement), ]

  return(list(
    optimized_dict = current_dict,
    final_f1 = current_f1,
    results = results_df
  ))
}

# Function to create a data frame with performannce metrics
make_table <- function(dict_policy, result_relevant, result_key, corpus) {
  # Create reference dictionary
  dict_ref <- dictionary(list(policy_statement_dict = c("policy", "policies")))

  # Evaluate all dictionaries
  ref <- evaluate_dictionary(dict_ref, corpus)
  dict1 <- evaluate_dictionary(dict_policy, corpus)
  dict2 <- evaluate_dictionary(result_relevant$optimized_dict, corpus)
  dict3 <- evaluate_dictionary(result_key$optimized_dict, corpus)

  # Combine into a single data frame
  combined_table <- data.frame(
    "Policy/Policies" = unlist(ref),
    "Dict 1" = unlist(dict1),
    "Dict 2" = unlist(dict2),
    "Dict 3" = unlist(dict3)
  )
  # Convert to matrix
  combined_table <- as.matrix(combined_table)

  # format numeric values
  combined_table[1:6, ] <- apply(
    combined_table[1:6, ], 2, function(x) round(as.numeric(x), 2)
  )

  combined_table[7:10, ] <- apply(
    combined_table[7:10, ], 2, function(x) format(as.integer(x), nsmall = 0)
  )

  # Rename columns
  colnames(combined_table) <- c(
    "Policy/Policies", "Dict 1",
    "Dict 2", "Dict 3"
  )

  # Rename rows
  rownames(combined_table) <- c(
    "F1 Avg", "Precision Avg", "Recall Avg",
    "F1 = 0", "F1 = 1", "Krippendorff's Alpha",
    "True Positives", "True Negatives",
    "False Positives", "False Negatives"
  )

  return(combined_table)
}
