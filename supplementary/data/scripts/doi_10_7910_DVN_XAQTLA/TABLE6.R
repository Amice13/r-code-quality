# Arthur Spirling
# 10/9/24

rm(list=ls())
set.seed(1649)

#grab the tar files and unpack
untar("useful_objects.tar")

# PURPOSE: compare our embeddings to those of "off the shelf" GloVe

cat("\n\n  This code pulls down a large (500MB) object for the Glove comparisons.\n  
        This may take some time on your machine\n")
#############################
# load the packages we need #
#############################

require(lsa) # version 0.73.3 
require(conText)


# for GloVe we need a function to pull closest neighbors out (in cosine terms)

make.cosine <-function(a="jury",b="vote"){
  cosine(glove[a,], glove[b,])
}

####################
# GloVe embeddings #
####################

df <- readRDS(
  url("https://www.dropbox.com/scl/fi/wiie9o7pf9dxobno7omnn/glove.rds?rlkey=5xi6eraza3x1rvkn1prz9i0ic&st=n4sxkpzc&dl=1")
  )

glove <- df


# look at NNs of a given term

cat("\n*** modern GLOVE EMBEDDINGS *** \n")

# sovereign
cat("\n sovereign \n")
dput( noquote( find_nns(glove["sovereign",], glove, N = 10, norm = "l2") )[-1] )


# parliament
cat("\n parliament \n")
dput(find_nns(glove["parliament",], glove, N = 10, norm = "l2")[-1])

# cromwell
cat("\n cromwell \n")
dput(find_nns(glove["cromwell",], glove, N = 10, norm = "l2")[-1])

# rights
cat("\n rights \n")
dput( find_nns(glove["rights",], glove, N = 10, norm = "l2")[-1])



# alms
cat("\n alms \n")
dput(find_nns(glove["alms",], glove, N = 10, norm = "l2")[-1])


# god
cat("\n god \n")
dput(find_nns(glove["god",], glove, N = 10, norm = "l2")[-1])



##############################
# Now do OUR embeddings
##############################

cat("\n*** LEVELLER EMBEDDINGS *** \n")


# load the embedings
load("useful_objects/embeddings.rdata")
em <- all_embed
local_transform <- all_transform
LEm <- em%*%local_transform



# look at NNs of a given term, drop term itself [-1] for 
# display reasons

# sovereign
cat("\n sovereign \n")
dput( find_nns(em["sovereign",], em, N = 10, norm = "l2")[-1] )


# parliament
cat("\n parliament \n")
dput(find_nns(em["parliament",], em, N = 10, norm = "l2")[-1])

# cromwell
cat("\n cromwell \n")
dput(find_nns(em["cromwell",], em, N = 10, norm = "l2")[-1])

# rights
cat("\n rights \n")
dput( find_nns(em["rights",], em, N = 10, norm = "l2")[-1])


# alms
cat("\n alms \n")
dput(find_nns(em["alms",], em, N = 10, norm = "l2")[-1])


# god
cat("\n god \n")
dput(find_nns(em["god",], em, N = 10, norm = "l2")[-1])

