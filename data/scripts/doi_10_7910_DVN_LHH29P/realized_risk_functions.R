
getRisk <- function(df, outcome_var) {

  mod <- lm(as.formula(glue("{outcome_var} ~ g_pathway_ul + g_pathway_cr")), df)

  ftest <- linearHypothesis(mod, names(coef(mod))[-1], vcov = vcovHC(mod, "HC1"))
  fp <- ftest[2, "Pr(>F)"]

  means_and_pathway_ftest = tibble(
    outcome = outcome_var,
    measure = "mean",
    all_pathways = df %>% pull(glue("{outcome_var}")) %>% mean(., na.rm = T),
    cr = df %>% filter(g_pathway_cr==1) %>% pull(glue("{outcome_var}")) %>% mean(., na.rm = T),
    ul = df %>% filter(g_pathway_ul==1) %>% pull(glue("{outcome_var}")) %>% mean(., na.rm = T),
    re = df %>% filter(g_pathway_re==1) %>% pull(glue("{outcome_var}")) %>% mean(., na.rm = T),
    pval = fp
  )

  return(means_and_pathway_ftest)
}


getN <- function(df) {

  n_row = tibble(
    outcome = "n",
    measure = "n",
    all_pathways = df %>% nrow,
    cr = df %>% filter(g_pathway_cr==1) %>% nrow,
    ul = df %>% filter(g_pathway_ul==1) %>% nrow,
    re = df %>% filter(g_pathway_re==1) %>% nrow
  )

  return(n_row)
}
