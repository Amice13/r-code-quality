# set up python virtual environment (useg in Figure 1 and Figure 3)
library(reticulate)
virtualenv_create("python/alacarte") # create a new environment 
virtualenv_install("python/alacarte", "sentence_transformers") # sentence_transformers
virtualenv_install("python/alacarte", "gensim") # sentence_transformers

# Firgure 2
source("code/fg02_fg03_corpus.R")
source("code/fg02.R")
fg2
rm(list = ls())

# Figure 3
source("code/fg03.R")
fg3
rm(list = ls())

# Figure 1, Table A1 
source("code/fg01_tbA1.R")
fg1 # figure 1
targets_nns_era[['1855']] # table A1
targets_nns_era[['2005']] # table A1
rm(list = ls())

# Table 3
source("code/tb03.R")
cbind("alc" = nns_alc$trump$feature, "unadjusted" = nns_emb$trump$feature) # table 3
cbind("alc" = nns_alc$Trump$feature, "unadjusted" = nns_emb$Trump$feature) # table 3
rm(list = ls())

# Figure 4
source("code/fg04.R")
fg4
rm(list = ls())

# Figure 5, Table 4, Table 5
source("code/fg05_tb04_tb05.R")
fg5
cbind("Democrats" = nns_D$feature, "Republicans" = nns_R$feature) # table 4
ncs_D$context # table 5
ncs_R$context # table 5
rm(list = ls())

# Figure 6
source("code/fg06_corpus.R")
source("code/fg06.R")
fg6
rm(list = ls())

# Figures 7, 8
source("code/fg07_corpus.R")
source("code/fg07-08.R")
fg7 # figure 7
fg8a # figure 8a
fg8b # figure 8b
rm(list = ls())

# Figures 9, 10, E5
source("code/fg09_corpus.R")
source("code/fg09-10-E5.R")
fg9
fg10
fgE5
rm(list = ls())

# ONLY SUPPLEMENT FIGURES BELOW

# Figure B1, Table B2
source("code/fgB1_tbB2.R")
fgB1 # figure B1
cbind("celebrity" = nns_trump$celebrity$feature, "president" = nns_trump$president$feature) # table B2
rm(list = ls())

# Figure C2
source("code/fgC2.R")
fgC2a # figure C2a
fgC2b # figure C2b
rm(list = ls())

# Figure D3, D4
source("code/fgD3-D4.R")
fgD3a # figure D3a
fgD3b # figure D3b
fgD4a # figure D4a
fgD4b # figure D4b
rm(list = ls())

# Figure F6
source("code/fgF6.R")
fgF6 # figure F6
rm(list = ls())

# Figure F7, F8, F9, G10
source("code/fgF7-F8-F9-G10.R")
fgF7a # figure F7a
fgF7b # figure F7b
fgF7c # figure F7c
fgF7d # figure F7d
fgF7e # figure F7e
fgF7f # figure F7f
fgF8a # figure F8a
fgF8b # figure F8b
fgF8c # figure F8c
fgF8d # figure F8d
fgF9 # figure F9
fgG10a # figure G10a
fgG10b # figure G10b
fgG10c # figure G10c
fgG10d # figure G10d
rm(list = ls())

# Figure H11
source("code/fgH11.R")
fgH11a # figure H11a
fgH11b # figure H11b
fgH11c # figure H11c
fgH11d # figure H11d
fgH11e # figure H11e
rm(list = ls())





