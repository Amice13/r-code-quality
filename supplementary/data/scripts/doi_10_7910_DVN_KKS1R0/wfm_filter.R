######################################################################
##  Filtering data and remain with x% most frequent words           ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################


# To counter time trends in the topics, 
# use a threshold for words 
# identify the number of mentions per document

wfm.filter <- function(dat.wfm, keep){
	wfm.ones <- dat.wfm
	wfm.ones[dat.wfm>0] <- 1
	rowsums.wfm <- apply(wfm.ones, 1,sum)
	wfm.filtered <- dat.wfm[rowsums.wfm>(1-keep)*length(dat.wfm[1,]),]	# all those words that appear in at least 1-keep documents
	return(wfm.filtered)
}

# 60%
tdmat_arg60 <- wfm.filter(tdmat_arg, 0.6)
tdmat_bra60 <- wfm.filter(tdmat_bra, 0.6)
tdmat_chi60 <- wfm.filter(tdmat_chi, 0.6)
tdmat_col60 <- wfm.filter(tdmat_col, 0.6)
tdmat_cri60 <- wfm.filter(tdmat_cri, 0.6)
tdmat_ecu60 <- wfm.filter(tdmat_ecu, 0.6)
tdmat_gtm60 <- wfm.filter(tdmat_gtm, 0.6)
tdmat_mex60 <- wfm.filter(tdmat_mex, 0.6)
tdmat_per60 <- wfm.filter(tdmat_per, 0.6)
tdmat_pry60 <- wfm.filter(tdmat_pry, 0.6)
tdmat_slv60 <- wfm.filter(tdmat_slv, 0.6)
tdmat_ury60 <- wfm.filter(tdmat_ury, 0.6)
tdmat_ven60 <- wfm.filter(tdmat_ven, 0.6)

# 70%
tdmat_arg70 <- wfm.filter(tdmat_arg, 0.7)
tdmat_bra70 <- wfm.filter(tdmat_bra, 0.7)
tdmat_chi70 <- wfm.filter(tdmat_chi, 0.7)
tdmat_col70 <- wfm.filter(tdmat_col, 0.7)
tdmat_cri70 <- wfm.filter(tdmat_cri, 0.7)
tdmat_ecu70 <- wfm.filter(tdmat_ecu, 0.7)
tdmat_gtm70 <- wfm.filter(tdmat_gtm, 0.7)
tdmat_mex70 <- wfm.filter(tdmat_mex, 0.7)
tdmat_per70 <- wfm.filter(tdmat_per, 0.7)
tdmat_pry70 <- wfm.filter(tdmat_pry, 0.7)
tdmat_slv70 <- wfm.filter(tdmat_slv, 0.7)
tdmat_ury70 <- wfm.filter(tdmat_ury, 0.7)
tdmat_ven70 <- wfm.filter(tdmat_ven, 0.7)

# Paper uses this filter
# 80%
tdmat_arg80 <- wfm.filter(tdmat_arg, 0.8)
tdmat_bra80 <- wfm.filter(tdmat_bra, 0.8)
tdmat_chi80 <- wfm.filter(tdmat_chi, 0.8)
tdmat_col80 <- wfm.filter(tdmat_col, 0.8)
tdmat_cri80 <- wfm.filter(tdmat_cri, 0.8)
tdmat_ecu80 <- wfm.filter(tdmat_ecu, 0.8)
tdmat_gtm80 <- wfm.filter(tdmat_gtm, 0.8)
tdmat_mex80 <- wfm.filter(tdmat_mex, 0.8)
tdmat_per80 <- wfm.filter(tdmat_per, 0.8)
tdmat_pry80 <- wfm.filter(tdmat_pry, 0.8)
tdmat_slv80 <- wfm.filter(tdmat_slv, 0.8)
tdmat_ury80 <- wfm.filter(tdmat_ury, 0.8)
tdmat_ven80 <- wfm.filter(tdmat_ven, 0.8)