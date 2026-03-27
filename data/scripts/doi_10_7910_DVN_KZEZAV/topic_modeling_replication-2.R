# ISSUE TOPIC MODELLING
# v 1.0 per 2021-08-25
# by Johan Lindholm
#--------------------------
# This script identifies topics in CJEU judgments. It was developed for the 
# paper 'From One to Many' but can be adapted for general classification. 
#--------------------------

library(tidyverse)
library(tidytext)
library(topicmodels)
library(textmineR)
library(wordcloud)
library(reshape2)
library(textstem)
library(Rcpp)

rm(list = ls())

path <- "Your file directory"
path <- "C:/Users/phili/Dropbox/CJEU dataset/Papers/Issue Splitting/Replication material/"

# LOAD CORPUS

load(paste0(path, "tm_corpus.RData"))
   
  
# CREATE TRAINING AND TEST DOCUMENT-TERM MATRICES
  
  # function for casting corpus as processed tokens
   
    processedTokensFromCorpus <- function(corpus) {   
      
      custom_stopwords <-c("cn", "note", "head", "subhead", "chapter",
                           "paragraph", "articl", "code", "convent", 
                           "agreement", "tfeu", "act", "regul", 
                           "direct", "treati", "h", "ec", "ecr", "law",
                           "legisl", "offici", "journal", "provis", "preambl",
                           "commiss", "council", "ag", "court",
                           "section", "european", "union", "question", "refer")
      # common and non-specific terms in corpus
      
      tokens <- unnest_tokens(corpus, word, text) %>%   # cast corpus as tokens
        anti_join(stop_words) %>%   # remove standard stopwords
        mutate(lemma = lemmatize_words(word)) %>%   # lemmatize tokens
        mutate(stem = stem_words(lemma)) %>%   # stem tokens
        filter(!stem %in% custom_stopwords)   # remove custom stopwords
      
      return(tokens)
    }
    
  # split corpus in training and test sets
  
    set.seed(54321)
    test_ecli <- sample(unique(crps$ecli), 
                        round(length(unique(crps$ecli)) * 0.2, digits = 0)) # select 20% test sample
    test_crps <- crps[crps$ecli %in% test_ecli,]   # create test corpus
    crps <- crps[!crps$ecli %in% test_ecli,]   # create training corpus
  
  # create training dtm
    
    train_tokens <- processedTokensFromCorpus(crps)
  
    combined_tokens <- bind_rows(tibble(document = train_tokens$issue_id,
                                        word = train_tokens$stem),
                                 tibble(document = train_tokens$ecli,
                                        word = train_tokens$stem))
    
    dtm_combined <- count(combined_tokens, document, word, sort = TRUE) %>%
      cast_dtm(document, word, n)
    
  # create test dtm
    
    test_tokens <- processedTokensFromCorpus(test_crps)

    combined_test_tokens <- bind_rows(tibble(document = test_tokens$issue_id,
                                        word = test_tokens$stem),
                                 tibble(document = test_tokens$ecli,
                                        word = test_tokens$stem))
    
    dtm_test_combined <- count(combined_test_tokens, document, word, sort = TRUE) %>%
      cast_dtm(document, word, n)
    
        
# TRAIN TOPIC MODEL
  
  # topic model
    
    # train new model
    
      #m_combined <- LDA(dtm_combined, k = 10)
    
    # or load main model
      
      load(paste0(path, "tm_topic_model.RData"))

  # present model as word cloud  
  
    topics <- tidy(m_combined, matrix = "beta")
    
    topic_terms <- topics %>%
      group_by(topic) %>%
      top_n(200, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
    
    cloud_data <- topic_terms %>%
      mutate(topic = paste("topic", topic)) %>%
      acast(term ~ topic, value.var = "beta", fill = 0)
    
    setEPS()
    postscript("topic_wordcloud.eps", width = 7, height = 7) # Figure 14 in manuscript appendix
    comparison.cloud(cloud_data, 
                     max.words = 300, 
                     length=c(0.1,1),
                     title.size = 1,
                     match.colors = TRUE) 
    dev.off()

  
# APPLY MODEL TO TEST DATA
  
  # classify test data
  
    test_topics <- topicmodels::posterior(m_combined, dtm_test_combined)
  
  # calculate max topic probability by docuent
    
    calculate_rel_prob <- function(document) {
      document_prob <- test_gamma$max_prob[test_gamma$document == document]
      document_ecli <- test_gamma$ecli[test_gamma$document == document]
      judgment_prob <- test_gamma$max_prob[test_gamma$document == document_ecli]
      doc_norm_prob <- document_prob/judgment_prob
      return(doc_norm_prob)
    }
    
    df <- as.data.frame(test_topics$topics)
    test_gamma <- tibble(document = rownames(df),
                         max_prob = apply(df, 1, max)) %>%
      mutate(is_issue = str_detect(document, ":I[0-9]{1,2}$")) %>%
      mutate(ecli = str_extract(document, ".*(?=:I)"))
    
    test_gamma$ecli[is.na(test_gamma$ecli)] <- test_gamma$document[is.na(test_gamma$ecli)]
    test_gamma$norm_max_prob <- unlist(lapply(test_gamma$document, calculate_rel_prob))
    
  # process complete (non-split and non-filtered) judgment
    
    # create complete test judgment corpus and dtm
      load(paste0(path, "para_data.Rdata"))
      entire_test_crps <- filter(para_data, ecli %in% test_ecli)
      entire_test_crps$text <- str_to_lower(entire_test_crps$text) %>%
        str_replace_all("[[:digit:]]", "") %>%
        str_replace_all("[[:punct:]]", "")
  
      entire_test_tokens <- processedTokensFromCorpus(entire_test_crps)
      
      dtm_entire_test <- count(entire_test_tokens, ecli, stem, sort = TRUE) %>%
        cast_dtm(ecli, stem, n)
    
    # classify complete judgment
      
      entire_test_topics <- topicmodels::posterior(m_combined, dtm_entire_test)
    
      df <- as.data.frame(entire_test_topics$topics)
      test_entire_gamma <- tibble(document = rownames(df),
                                  max_prob = apply(df, 1, max),
                                  is_issue = FALSE,
                                  ecli = document,
                                  norm_max_prob = 1)
    
    
  # calculate issue-to-judgment probability  
    
    ix <- match(test_gamma$ecli, test_entire_gamma$document)
    test_gamma$max_entire_prob <- test_entire_gamma$max_prob[ix]
    test_gamma$norm_max_entire_prob <- test_gamma$max_prob / test_gamma$max_entire_prob    
    test_gamma <- filter(test_gamma, is_issue == TRUE) %>%   # keep only issues
      filter(norm_max_prob != 1) %>%  # remove single-issue judgment
      arrange(norm_max_prob)    # order for graph
    x <- filter(test_gamma, is_issue == TRUE) %>%
      filter(norm_max_prob != 1) %>%
      arrange(norm_max_prob)
    
# GENERATE GRAPH
    
    setEPS()
    postscript("tm_issue_split_ordered.eps", width = 10, height = 7) # Figure 5 in main manuscript
    ggplot(x, aes(x = 1:nrow(x), y = norm_max_prob)) +
      geom_hline(aes(yintercept = 1, lty = "Judgment base"), color = "black") +
      geom_hline(aes(yintercept = mean(norm_max_prob), lty = "Mean filtered"), color = "grey50")  +
      geom_hline(aes(yintercept = mean(norm_max_entire_prob), lty = "Mean complete"), color = "grey50") +
      geom_errorbar(aes(ymin = norm_max_prob, ymax = norm_max_entire_prob)) +
      geom_point(aes(y = norm_max_entire_prob, shape = "complete"), alpha = 1, size = 2.5) +
      geom_point(aes(shape = "filtered"), alpha = 1, size = 2.5) +
      theme(panel.background = element_blank()) +
      labs(title = "",
           subtitle = "",
           y = "Issue max topic probability\nrelative to judgment max probability",
           x = "Issues",
           shape = "Relative to\njudgments\nthat are...",
           lty = "")# +
    dev.off()  
    
# EXAMPLE
    
  # Prepare data
    ecli <- "ECLI:EU:C:2001:114"
    topics_dist <- as_tibble(test_topics$topics) %>%
      mutate(name = rownames(test_topics$topics)) %>%
      pivot_longer(1:10, names_to = "topic")
    judgment_topics <- filter(topics_dist, str_remove(name, ":I[0-9]") == ecli)
      
  # Plot
    setEPS()
    postscript("topic_example.eps", width = 10, height = 5) # Figure 4 in main manuscript
    par(mfrow = c(1,3))
    barplot(judgment_topics[1:10,]$value,
            names.arg = c("Topic 1","Topic 2","Topic 3","Topic 4","Topic 5",
                     "Topic 6","Topic 7","Topic 8","Topic 9","Topic 10"),
            ylim = c(0,0.5),
            xlab = "", ylab = "Topic probability", las = 2,
            main = "Complete Judgment (Case C-187/99)")
    abline(h = seq(0,0.5,0.1), lty = 2, lwd = .5, col = "grey")
    barplot(judgment_topics[21:30,]$value,
            names.arg = c("Topic 1","Topic 2","Topic 3","Topic 4","Topic 5",
                          "Topic 6","Topic 7","Topic 8","Topic 9","Topic 10"),
            ylim = c(0,0.5),
            xlab = "", ylab = "Topic probability", las = 2,
            main = "Issue 1 (Case C-187/99)")
    abline(h = seq(0,0.5,0.1), lty = 2, lwd = .5, col = "grey")
    barplot(judgment_topics[11:20,]$value,
            names.arg = c("Topic 1","Topic 2","Topic 3","Topic 4","Topic 5",
                          "Topic 6","Topic 7","Topic 8","Topic 9","Topic 10"),
            ylim = c(0,0.5),
            xlab = "", ylab = "Topic probability", las = 2,
            main = "Issue 2 (Case C-187/99)")
    abline(h = seq(0,0.5,0.1), lty = 2, lwd = .5, col = "grey")
    dev.off()

    
  # issue prob rel entire judgment prob
    
    calculate_entire_rel_prob <- function(document) {
      document_prob <- test_entire_gamma$max_prob[test_entire_gamma$document == document]
      document_ecli <- test_entire_gamma$ecli[test_entire_gamma$document == document]
      judgment_prob <- test_entire_gamma$max_prob[test_entire_gamma$document == document_ecli]
      doc_norm_prob <- document_prob/judgment_prob
      return(doc_norm_prob)
    }
    
    # add issues
      test_entire_gamma <- bind_rows(test_entire_gamma,
                                     test_gamma[test_gamma$is_issue == TRUE,])
      # test_gamma$ecli[is.na(test_gamma$ecli)] <- test_gamma$document[is.na(test_gamma$ecli)]
      test_entire_gamma$norm_max_prob <- unlist(lapply(test_entire_gamma$document, calculate_entire_rel_prob))
    
    