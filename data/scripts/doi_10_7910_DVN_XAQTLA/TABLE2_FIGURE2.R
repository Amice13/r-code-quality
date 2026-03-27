# Arthur Spirling
# 10/9/24
#sink("logs/log2.txt")
rm(list=ls())
set.seed(1649)

# PURPOSE: compare LILBURNE to other Levellers wrt "peers" and understandings 
# of same.

# DESCRIPTION: 
# This code does 2 main things: 
# 1. gets NNs of peers terms for Lilburne v  other Levellers
# 2. gets most characteristic NNs for those groups
#
# before that, it prepares the corpus and loads the packages needed. 


###############################################################################
# Prepare the Corpus, Load the relevant packages we will need. Load the       #
# embeddings.                                                                 #
###############################################################################

# First, load the packages and corpus
require(quanteda) #quanteda_4.1.0
require(conText)  #V 1.4.3

#grab the tar files and unpack
untar("useful_objects.tar")

# load the corpus -- 
load("useful_objects/pamphlet_corpus.rdata")
pamphlet_corpus <- rtL_corp

# load the embeddings, and local transform
load("useful_objects/embeddings.rdata")
the_embeddings <- all_embed
local_transform <- all_transform


# we need a Lilburne dummy: 1 if Lilburne, 0 otherwise
# we say Lilburne is the author if he is the First Author listed
author1 <- docvars(pamphlet_corpus)$FirstAuthor
author_lil <- grep("Lilburne", author1)
Lilburne_dummy <- rep(0, times = length(docvars(pamphlet_corpus)$leveller_main) )
Lilburne_dummy[author_lil] <- 1
docvars(pamphlet_corpus)$Lilburne_Author <- Lilburne_dummy

levs <- which(docvars(pamphlet_corpus)$leveller_expansive == 1)
Leveller_corpus <- pamphlet_corpus[levs]

###############################################################################
# Operation 1: look at the nearest neighbors of "peer" terms                  # 
# for Lilburne v others, respectively                                         #
###############################################################################

# get group-specific (i.e. Lilburne v other Levellers) embeddings, average within period
# tokenize etc

toks <- tokens(Leveller_corpus, 
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

peers_Lilburne_not <- dem_group(peers_dem, groups = peers_dem@docvars$Lilburne_Author)

# NNs by covariate (here, Leveller status)
peer_nns <- nns(peers_Lilburne_not, 
                 pre_trained = the_embeddings, 
                 N = 20, candidates = peers_Lilburne_not@features, 
                 as_list = TRUE)

print(peer_nns)
# This is Table 2 in paper: Lilburne (1) vs others (0)

###############################################################################
# Operation 2: look at the nearest neighbors that are most characteristic of  #
# Lilburne v Levellers   (i.e. in ratio terms)                                #
###############################################################################


# we limit candidates to features in our corpus
feats <- featnames(dfm(peers_toks))

# compute ratio
peer_nns_ratio <- get_nns_ratio(x = peers_toks, 
                              N = 10,
                              groups = docvars(peers_toks, 'Lilburne_Author'),
                              numerator = "1",
                              candidates = feats,
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
plot_nns2(nns_df = peer_nns_ratio, label_lo = "Others", label_hi = "Lilburne")
# this is Figure 2 in paper.


#sink()








