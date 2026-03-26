
# Arthur Spirling
# 10/15/24


# PURPOSE: compare"His Apologeticall Narration" (0260) (HAN), to other Lilburne 
# texts and other Leveller texts

# DESCRIPTION: 
# This code does 3 main things: 
# 1. gets NNs of peers terms for Levellers v non-Levellers but with HAN removed
# 2. gets NNS of peers terms for Lilburne v Leveller but with HAN removed
# 3. compares HAN v all other documents in terms of peer use

# before that, it prepares the corpus and loads the packages needed. 
#sink("logs/log3.txt")
rm(list=ls())
set.seed(1649)

###############################################################################
# Prepare the Corpus, Load the relevant packages we will need. Load the       #
# embeddings.                                                                 #
###############################################################################

# First, load the packages and corpus
require(quanteda)
require(conText)


#grab the tar files and unpack
untar("useful_objects.tar")

# load the corpus -- 
load("useful_objects/pamphlet_corpus.rdata")
pamphlet_corpus <- rtL_corp

# load the embeddings, and local transform
load("useful_objects/embeddings.rdata")
the_embeddings <- all_embed
local_transform <- all_transform

# we need to remove some specific texts that aren't really comparable here
# "Several Hands"  
# "Anon"
# "Signed by Several"
#  "Six Soldiers"
# "Gerrard Winstanley" (i.e. diggers)

drop_authors <- c( "Several Hands","Anon","Signed by Several","Six Soldiers",
                   "Gerrard Winstanley")
pamphlet_corpus_A <- subset(pamphlet_corpus, 
                            !docvars(pamphlet_corpus)$FirstAuthor%in%drop_authors)

# and drop the one where Lilburne is a second author, but the first author 
# is not a Leveller
pamphlet_corpus_B <- subset(pamphlet_corpus_A, 
                            !docvars(pamphlet_corpus_A)$FirstAuthor%in%c("Robert Lokier"))


# we need a Lilburne dummy: 1 if Lilburne, 0 otherwise
# use first author as demarcating variable
author1 <- docvars(pamphlet_corpus_B)$FirstAuthor
author_lil <- grep("Lilburne", author1)
Lilburne_dummy <- rep(0, times = length(docvars(pamphlet_corpus_B)$leveller_main) )
Lilburne_dummy[author_lil] <- 1
docvars(pamphlet_corpus_B)$Lilburne_Author <- Lilburne_dummy


# We need to do a set of comparisons inline with the DESCRIPTION above. For this
# we will need: 
# pamphlet_corpus_C: our usual general corpus but does not include HAN (includes a Lilburne dummy)
# --> we don't report results directly from corpus_C in the paper, 
# but it's useful to have

# pamphlet_corpus_D: our usual LEVELLER corpus but does not include HAN (includes Lilburne dummy)
# pamphlet_corpus_E: our usual LEVELLER corpus, includes HAN, and dummy for that

# drop the relevant 'peers and equals' Lilburne document (HAN)
pamphlet_corpus_C <- subset(pamphlet_corpus_B, 
                            !docvars(pamphlet_corpus_B)$DocCode%in%c("0260.txt"))

# drop everyone who isn't a Leveller (this won't include HAN)
pamphlet_corpus_D <- subset(pamphlet_corpus_C, 
                            docvars(pamphlet_corpus_C)$leveller_expansive == 1)

# go back and include HAN, and drop everyone who isn't a Leveller
pamphlet_corpus_E <- subset(pamphlet_corpus_B, 
                            docvars(pamphlet_corpus_B)$leveller_expansive == 1)

# add a dummy for HAN
dc <- docvars(pamphlet_corpus_E)$DocCode
which260 <- which(dc=="0260.txt")
document_dummy <- rep(0, times = length(docvars(pamphlet_corpus_E)$leveller_main) )
document_dummy[which260] <- 1
docvars(pamphlet_corpus_E)$document_dum <- document_dummy

# Note that this:
#> docvars(pamphlet_corpus_E)$Title[docvars(pamphlet_corpus_E)$document_dum==1]
# returns "His Apologeticall Narration", as it should.

###############################################################################
# Operation 1: look at the nearest neighbors of "peer"  terms                 #
# for Levellers and non-Levellers, respectively (not including HAN)           #
#                                                                             #      
# Note that we don't report this in the paper                                 #
###############################################################################
# get group-specific (i.e. Leveller v non-Leveller) embeddings, average within period
# tokenize etc
toks <- tokens(pamphlet_corpus_C, 
               remove_punct=T, remove_symbols=T, 
               remove_numbers=T, remove_separators=T)
# clean out stopwords and words with 2 or fewer characters
toks_nostop <- tokens_select(tokens_tolower(toks), 
                             pattern = stopwords("en"), 
                             selection = "remove", min_nchar=3)
# only use features that appear at least 5 times in the corpus
feats <- dfm(toks_nostop, tolower=T, 
             verbose = FALSE) %>% dfm_trim(min_termfreq = 5) %>% featnames()
# leave the pads so that non-adjacent words will not become adjacent
toks_nostop_feats <- tokens_select(toks_nostop, feats, padding = TRUE)
# make the 'peers' (avoid "peerage")
peers_toks <- tokens_context(x = toks_nostop_feats, pattern = "peer\\b|peere|peers|peeres",
                             valuetype = c("regex"), window = 12L)
# make dfm object
peers_dfm <- dfm(peers_toks)
#make dem object
peers_dem <- dem(x = peers_dfm, 
                 pre_trained = the_embeddings, 
                 transform = TRUE, 
                 transform_matrix = local_transform, verbose = TRUE)
peers_lev_nonlev <- dem_group(peers_dem, groups = peers_dem@docvars$leveller_expansive)

# NNs by covariate (here, Leveller status)
peers_nns1 <- nns(peers_lev_nonlev, 
                 pre_trained = the_embeddings, 
                 N = 20, candidates = peers_lev_nonlev@features, 
                 as_list = TRUE)

#print(peers_nns1)
cat("\n inspect peers_nns1 to see the Leveller v non-Leveller results 
(not reported in the paper)  \n")



###############################################################################
# Operation 2: look at the nearest neighbors of "peer"  terms                 #
# for Lilburne v Levellers, respectively  (not including HAN)                  #
###############################################################################


toks2 <- tokens(pamphlet_corpus_D, 
               remove_punct=T, remove_symbols=T, 
               remove_numbers=T, remove_separators=T)
toks_nostop2 <- tokens_select(tokens_tolower(toks2), 
                             pattern = stopwords("en"), 
                             selection = "remove", min_nchar=3)
feats2 <- dfm(toks_nostop2, tolower=T, 
             verbose = FALSE) %>% dfm_trim(min_termfreq = 5) %>% featnames()
toks_nostop_feats2 <- tokens_select(toks_nostop2, feats2, padding = TRUE)
peers_toks2 <- tokens_context(x = toks_nostop_feats2, pattern = "peer\\b|peere|peers|peeres",
                             valuetype = c("regex"), window = 12L)
peers_dfm2 <- dfm(peers_toks2)
peers_dem2 <- dem(x = peers_dfm2, 
                 pre_trained = the_embeddings, 
                 transform = TRUE, 
                 transform_matrix = local_transform, verbose = TRUE)
peers_lil <- dem_group(peers_dem2, groups = peers_dem2@docvars$Lilburne_Author)

# NNs by covariate (here, Lilburne author)
peers_nns2 <- nns(peers_lil, 
                 pre_trained = the_embeddings, 
                 N = 20, candidates = peers_lil@features, 
                 as_list = TRUE)

#print(peers_nns2)
cat("\n inspect peers_nns2 to see the Lilburne v Leveller results 
(mentioned in passing in paper, but no table reported)  \n")


###############################################################################
# Operation 3: on Leveller corpus, show "effect" of HAN dummy                 #
#                                                                             #  
# We report these results in TABLE 3, TABLE 4, FIGURE 3                       #
###############################################################################

toks3 <- tokens(pamphlet_corpus_E, 
               remove_punct=T, remove_symbols=T, 
               remove_numbers=T, remove_separators=T)

# clean out stopwords and words with 2 or fewer characters
toks_nostop3 <- tokens_select(tokens_tolower(toks3), 
                             pattern = stopwords("en"), 
                             selection = "remove", min_nchar=3)
# only use features that appear at least 5 times in the corpus
feats3 <- dfm(toks_nostop3, tolower=T, 
             verbose = FALSE) %>% dfm_trim(min_termfreq = 5) %>% featnames()
# leave the pads so that non-adjacent words will not become adjacent
toks_nostop_feats3 <- tokens_select(toks_nostop3, feats3, padding = TRUE)

# can we show the nns differ?
peers_toks3 <- tokens_context(x = toks_nostop_feats3, pattern = "peer\\b|peere|peers|peeres",
                              valuetype = c("regex"), window = 12L)
peers_dfm3 <- dfm(peers_toks3)
peers_dem3 <- dem(x = peers_dfm3, 
                  pre_trained = the_embeddings, 
                  transform = TRUE, 
                  transform_matrix = local_transform, verbose = TRUE)
peers_dum <- dem_group(peers_dem3, groups = peers_dem3@docvars$document_dum)

# NNs by covariate (here, Lilburne author)
peers_dum_nns2 <- nns(peers_dum, 
                  pre_trained = the_embeddings, 
                  N = 20, candidates = peers_dum@features, 
                  as_list = TRUE)

cat("\n TABLE 3: \n")
print(peers_dum_nns2)

# do a dummy regression
cat("\n TABLE 4 \n")
peer_model_document <- conText(formula = c("peers","peer","peeres","peere")~ document_dum,
                      data = toks_nostop_feats3,
                      pre_trained = the_embeddings,
                      transform = TRUE, transform_matrix = local_transform,
                     permute = TRUE, num_permutations = 1000,
                      window = 12, case_insensitive = TRUE,
                      verbose = FALSE)




# plot NNS
# This is FIGURE 3
# we limit candidates to features in our corpus
feats4 <- featnames(dfm(peers_toks3))

# compute ratio
peers_nns_ratio3 <- get_nns_ratio(x = peers_toks3, 
                                 N = 10,
                                 groups = docvars(peers_toks3, 'document_dum'),
                                 numerator = "1",
                                 candidates = feats4,
                                 pre_trained = the_embeddings,
                                 transform = TRUE,
                                 transform_matrix = local_transform,
                                 bootstrap = TRUE,
                                 num_bootstraps = 1000,
                                 permute = TRUE,
                                 num_permutations = 1000,
                                 verbose = FALSE)

x11()
source("useful_objects/nns_plot_function.R")
plot_nns2(nns_df = peers_nns_ratio3, label_lo = "Other", label_hi = "Document dummy")

#sink()
