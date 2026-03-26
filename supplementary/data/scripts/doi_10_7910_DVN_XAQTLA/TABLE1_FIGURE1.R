# Arthur Spirling
# 10/09/24

#sink("logs/log1.txt")

rm(list=ls())
set.seed(1649)

# install packages if needed...
#- quanteda _4.1.0 
#- readtext _0.91
#- stringr _1.5.1
#- text2vec _ 0.6.4
#- conText  _1.4.3
#- wordcloud _2.6
# -lsa _0.73.3

install.packages(c("quanteda", "readtext", "stringr", "text2vec"))
install.packages("conText")
install.packages("wordcloud")
install.packages("lsa")


# PURPOSE: compare Levellers to others wrt "peers" and understandings of same.

# DESCRIPTION: 
# This code does 2 main things: 
# 1. gets NNs of peers terms for Levellers v non-Levellers
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


###############################################################################
# Operation 1: look at the nearest neighbors of "peer"  terms                 #
# for Levellers and non-Levellers, respectively                               #
###############################################################################

# get group-specific (i.e. Leveller v non-Leveller) embeddings, 
# average within period, tokenize etc

toks <- tokens(pamphlet_corpus_B, 
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
peers_nns <- nns(peers_lev_nonlev, 
                 pre_trained = the_embeddings, 
                 N = 20, candidates = peers_lev_nonlev@features, 
                 as_list = TRUE)

print(peers_nns)
# this is Table 1 in paper -- Levellers are (1), non-Levellers are (0)

###############################################################################
# Operation 2: look at the nearest neighbors that are most characteristic of  #
# Levellers v non-Levellers (i.e. in ratio terms)                             #
###############################################################################


# we limit candidates to features in our corpus
feats <- featnames(dfm(peers_toks))

# compute ratio
peers_nns_ratio <- get_nns_ratio(x = peers_toks, 
                              N = 10,
                              groups = docvars(peers_toks, 'leveller_expansive'),
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
plot_nns2(nns_df = peers_nns_ratio, label_lo = "Non-Levellers", label_hi = "Levellers")
# Figure 1 in paper

#sink()
