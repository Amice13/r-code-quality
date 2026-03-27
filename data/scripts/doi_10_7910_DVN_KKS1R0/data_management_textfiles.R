######################################################################
## Read in TFM and assign names                                     ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################




# =====================
# = 1 Create the data =
# =====================


# load the word frequency matrices


tdmat_arg <- read.csv("data/intermediates/wfm_matrices/wfm_arg.csv", row.names = 1)
tdmat_bra <- read.csv("data/intermediates/wfm_matrices/wfm_bra.csv", row.names = 1)
tdmat_chi <- read.csv("data/intermediates/wfm_matrices/wfm_chi.csv", row.names = 1)
tdmat_col <- read.csv("data/intermediates/wfm_matrices/wfm_col.csv", row.names = 1)
tdmat_cri <- read.csv("data/intermediates/wfm_matrices/wfm_cri.csv", row.names = 1)
tdmat_ecu <- read.csv("data/intermediates/wfm_matrices/wfm_ecu.csv", row.names = 1)
tdmat_gtm <- read.csv("data/intermediates/wfm_matrices/wfm_gtm.csv", row.names = 1)
tdmat_mex <- read.csv("data/intermediates/wfm_matrices/wfm_mex.csv", row.names = 1)
tdmat_per <- read.csv("data/intermediates/wfm_matrices/wfm_per.csv", row.names = 1)
tdmat_pry <- read.csv("data/intermediates/wfm_matrices/wfm_pry.csv", row.names = 1)
tdmat_slv <- read.csv("data/intermediates/wfm_matrices/wfm_slv.csv", row.names = 1)
tdmat_ury <- read.csv("data/intermediates/wfm_matrices/wfm_ury.csv", row.names = 1)
tdmat_ven <- read.csv("data/intermediates/wfm_matrices/wfm_ven.csv", row.names = 1)


names.arg <- colnames(tdmat_arg)
names.bra <- colnames(tdmat_bra)
names.chi <- colnames(tdmat_chi)
names.col <- colnames(tdmat_col)
names.cri <- colnames(tdmat_cri)
names.ecu <- colnames(tdmat_ecu)
names.gtm <- colnames(tdmat_gtm)
names.mex <- colnames(tdmat_mex)
names.per <- colnames(tdmat_per)
names.pry <- colnames(tdmat_pry)
names.slv <- colnames(tdmat_slv)
names.ury <- colnames(tdmat_ury)
names.ven <- colnames(tdmat_ven)





			