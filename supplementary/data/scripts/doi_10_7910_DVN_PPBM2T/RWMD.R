
# --- Libraries --- #
if (!require("quanteda")) devtools::install_github("kbenoit/quanteda")
if (!require("readtext")) devtools::install_github("kbenoit/readtext")
library(stringr)
library(stringi)
library(text2vec)
library(readr)
library(countrycode)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(xtable)
library(cccd)
set.seed(1912)




# --- UNGD Speeches --- #
DATA_DIR <- "data/TXT/"  # set directory to location of downloaded UNGA corpus

ungd_files <- readtext(DATA_DIR, 
                       docvarsfrom = "filenames", 
                       dvsep="_", 
                       docvarnames = c("Country", "Session", "Year")) # import, setting document variables in the process

ungd_corpus <- corpus(ungd_files, text_field = "text") # construct a corpus object 




# --- Fit GloVe Model --- #

# --- Text preprocessing and vectorization:

tok <- function(x) {word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x, language="en"))} # create word tokenizer function

tokens.un <- ungd_files$text  %>% tolower %>% tok   # word tokenization and conversion to lowercase

it <- itoken(tokens.un, progressbar = F) # create vocabulary of simple unigrams

vocab.un <- create_vocabulary(it) %>% prune_vocabulary(term_count_min = 5L, doc_proportion_min = .05)

vectorizer <- vocab_vectorizer(vocab.un) # create vocabulary vectorizer function

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L) # create term co-occurrence matrix





# --- Hyperparameters:

hyper_p <- expand.grid(wvs = c(200),
                       x_max = c(15))


for(i in 1:nrow(hyper_p)){

  hyper_p_i <- hyper_p[i,]

  glove <- GlobalVectors$new(word_vectors_size = hyper_p_i$wvs,
                             vocabulary = vocab.un,
                             x_max = hyper_p_i$x_max)

  wv_main <- glove$fit_transform(tcm, n_iter = 15, convergence_tol = 0.001) # provides "main" vectors

  wv_context <- glove$components # provides "context" vectors

  word_vectors = wv_main + t(wv_context) # original GloVe paper says to average or sum main and context vectors

  saveRDS(word_vectors,
          file = paste("data/word_vectors_",hyper_p_i$wvs,"d_",hyper_p_i$x_max,"x.rds", sep = ""))

}










# --- Explore the space(s) --- #

# --- Addition/subtraction
# try out vector addition/subtraction and analogies for the different parameter settings

word_vectors <- readRDS("data/word_vectors_200d_15x.rds")

test1 <- word_vectors["environment", , drop = FALSE] + word_vectors["pollut", , drop = FALSE]
cos_sim <- sim2(x = word_vectors, y = test1, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

test2 <- word_vectors["peac", , drop = FALSE] - word_vectors["agreement", , drop = FALSE] + word_vectors["weapon", , drop = FALSE]
cos_sim <- sim2(x = word_vectors, y = test2, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

test3 <- word_vectors["west", , drop = FALSE] - word_vectors["nato", , drop = FALSE] + word_vectors["russia", , drop = FALSE]
cos_sim <- sim2(x = word_vectors, y = test3, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

test4 <- word_vectors["terrorist", , drop = FALSE] + word_vectors["bomb", , drop = FALSE] 
cos_sim <- sim2(x = word_vectors, y = test4, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)











# --- Use embeddings to find dyadwise speech distances within each year --- #

  
rwmd_model <- RWMD$new(word_vectors)
un.rwmd.l <- list()
  for(i in 1970:2014){
    print(i)
    sub.i <- corpus_subset(ungd_corpus, Year == i)
    tokens <- word_tokenizer(tolower(sub.i))
    it <- itoken(tokens)
    v <- create_vocabulary(it)
    vectorizer <- vocab_vectorizer(v)
    ir_dtm <- create_dtm(it, vectorizer)
    
    rwmd_dist <- dist2(ir_dtm, method = rwmd_model, norm = "none")
    rwmd_norm <- (rwmd_dist-min(rwmd_dist))/(max(rwmd_dist)-min(rwmd_dist))
    rwmd_norm_sims <- 1 - rwmd_norm
    
    diag(rwmd_norm_sims) <- 0
    colnames(rwmd_norm_sims) <- docvars(sub.i, "Country")
    rownames(rwmd_norm_sims) <- docvars(sub.i, "Country")
    
    un.rwmd.l[[i]] <- rwmd_norm_sims
    
  }
  
saveRDS(un.rwmd.l, file = "data/un_rwmd_200d_15x.rds")
  



  
  
  
  


avg_vector_mat_list <- list()
for(i in 1970:2014){
  sub.i <- corpus_subset(ungd_corpus, Year == i)
  
  avg_vector_mat <- matrix(, nrow = 0, ncol = 200) # gives warning of missing arg, but actually correctly makes an empty matrix
  
  for(j in unique(docvars(sub.i)$Country)){
    
    sub.c.i <- corpus_subset(sub.i, Country == j)
    tokens <- word_tokenizer(tolower(sub.c.i))
    
    word_vectors_sub <- word_vectors[rownames(word_vectors) %in%  tokens[[1]],]
    wv.i <- colSums(word_vectors_sub)/nrow(word_vectors_sub)
    
    avg_vector_mat <- rbind(avg_vector_mat, wv.i)
    
  }
  
  rownames(avg_vector_mat) <- docvars(sub.i)$Country
  avg_vector_mat_list[[i]] <- avg_vector_mat
}
  

un_cosine_200d_15x <- list()
for(i in 1970:2014){
  mat.i <- lsa::cosine(t(avg_vector_mat_list[[i]]))
  cos_norm <- (mat.i-min(mat.i))/(max(mat.i)-min(mat.i))
  diag(cos_norm) <- 0
  un_cosine_200d_15x[[i]] <- cos_norm
  
}
  
saveRDS(un_cosine_200d_15x, file = "data/un_cosine_200d_15x.rds")




