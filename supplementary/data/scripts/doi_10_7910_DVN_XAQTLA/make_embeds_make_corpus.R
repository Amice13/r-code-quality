# Arthur Spirling
# 10/09/24

rm(list=ls())
set.seed(1649)

# This code...
# 1. pulls together the parliamentary data (the background corpus from Rodon & 
# Pashkalis)
# 2. grabs the pamphlet collection, and pulls it all together
# 3. put the parliamentary data and pamphlet data together
# 4. produces the embeddings, and the local ALC transform matrix
# 5. saves relevant stuff

# It requires the packages as described in the README.  It also needs access to
# <useful_objects> which contains the data overview
# <leveller_texts> which contains the Leveller pamphlets
# <parl_text_data> which contains the parliament data courtesy of Rodon and Paskhalis

# This line checks that the set.seed is set correctly: 
cat("\nchecking set.seed is correct; you should see 0.1066428 -1.1294294 -0.7114150 \n")
print(rnorm(3))

# load some packages
require(quanteda)
require(readtext)
require(stringr)
require(text2vec)
require(RcppParallel)
require(conText)

#grab the tar files and unpack
untar("useful_objects.tar") # just to make sure we can write to the dir
untar("parl_text_data.tar")
untar("leveller_texts.tar")

###############################################################################
# Part I: Pull the 17th C parliamentary corpus together                       #
###############################################################################

# get the parliament data
setwd("parl_text_data")


# make the parliamentary corpus
rout_1 <- readRDS("acts_interregnum_df_cleaned.rds")
data_frame_site1 <- data.frame(
  txt = rout_1$text,
  meta = "collection1"
)

rout_2 <- readRDS("commons_df.rds")
data_frame_site2 <- data.frame(
  txt = rout_2$text,
  meta = "collection2"
)

rout_3 <- readRDS("lords_df.rds")
data_frame_site3 <- data.frame(
  txt = rout_3$text,
  meta = "collection3"
)

rout_4 <- readRDS( "statutes_df_cleaned.rds" )
data_frame_site4 <- data.frame(
  txt = rout_4$text,
  meta = "collection4"
)

# pull it all together to make 'parl_corp'
all_df <- do.call(rbind, lapply(ls(pattern = "^data_frame_"), get))
parl_corp <- corpus(all_df, text_field = "txt")

###############################################################################
# Part 2: Pull the Leveller pamphlet collection together                      #
###############################################################################


# get the Leveller pamphlet collection
setwd("../leveller_texts")
rtL <- readtext("*.txt")

# get the variables
overview <- read.csv("../useful_objects/overview_files_data.csv")


# set the variables up
m <-  match(rtL$doc_id, overview$DocCode)
variables <- overview[m,]

rtL_corp <- corpus(rtL)
docvars(rtL_corp) <- variables
# this is a bit roundabout -- we won't use this corpus as is, it's just here to
# make the docvars assignment easier later...

# drop the repeated documents
repeats <- which(docvars(rtL_corp)$Comments=="repeated doc")
rtL_corp <- rtL_corp[-c(repeats)]
docvars(rtL_corp)$Comments 

# rtL_corp is the corpus of the pamphlets 


###############################################################################
# Part 3: Make the full parliament corpus + pamphlet corpus into one corpus   #
###############################################################################


full_corpus <- parl_corp + rtL_corp


###############################################################################
# Part 4: Clean up the corpus, and make the embeddings                        #
###############################################################################


# tokenize corpus removing unnecessary (i.e. semantically uninformative) elements
toks <- tokens_tolower( tokens(full_corpus, remove_punct=T, remove_symbols=T, 
                              remove_numbers=T, remove_separators=T) )

# clean out stopwords and words with 2 or fewer characters
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove", min_nchar=3)

# only use features that appear at least 5 times in the corpus
feats <- dfm(toks_nostop, tolower=T, verbose = FALSE) %>% dfm_trim(min_termfreq = 5) %>% featnames()

# leave the pads so that non-adjacent words will not become adjacent
toks2 <- tokens_select(toks_nostop, feats, padding = TRUE)

#######################################
## Make embeddings on the "big" corpus
#######################################

# construct the feature co-occurrence matrix
all_fcm <- fcm(toks2, context = "window", window = 12, count = "frequency", 
               tri = FALSE)

# estimate glove model using text2vec
# rank is size of embeddings 

# set *one* thread here.  This is slow, but should replicate *exactly*
RcppParallel::setThreadOptions(1)
glove <- GlobalVectors$new(rank = 300, x_max = 100, learning_rate = 0.05)
wv_main <- glove$fit_transform(all_fcm, n_iter = 200, convergence_tol = 0.001, 
                               n_threads = 1)


wv_context <- glove$components
all_embed <- wv_main + t(wv_context)  # word vectors 


# Now produce a local transform matrix: i.e. make the ALC 'A' matrix
all_transform <- compute_transform(x= all_fcm, pre_trained = all_embed, 
                                  weighting = "log")
# (use log weighting -- seems reasonable choice)

###############################################################################
# Part 5: Save the embeddings, save the pamphlet corpus for future use        #
###############################################################################

# save the embeddings + local transform for future use.
save(all_embed, all_transform, 
     file="../useful_objects/embeddings.rdata")

# save the pamphlet corpus
save(rtL_corp, 
     file="../useful_objects/pamphlet_corpus.rdata")

# final check on the set.seed.
cat("\n Checking set.seed is correct; you should see 0.3450810 0.1487775 0.8947996. Run complete. \n")
print(runif(3))
