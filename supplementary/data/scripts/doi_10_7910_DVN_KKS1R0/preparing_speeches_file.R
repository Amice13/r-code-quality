######################################################################
## Implement the Functions that take speeches and make TFM          ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################



# ====================================================================
# Now: hit the speeches. Input is source folder and then output file =   
# ====================================================================
                                            
wfm.maker("data/input/speeches/arg", "data/intermediates/wfm_matrices/wfm_arg.csv")
wfm.maker("data/input/speeches/chi", "data/intermediates/wfm_matrices/wfm_chi.csv")
wfm.maker("data/input/speeches/col", "data/intermediates/wfm_matrices/wfm_col.csv")
wfm.maker("data/input/speeches/cri", "data/intermediates/wfm_matrices/wfm_cri.csv")
wfm.maker("data/input/speeches/ecu", "data/intermediates/wfm_matrices/wfm_ecu.csv")
wfm.maker("data/input/speeches/gtm", "data/intermediates/wfm_matrices/wfm_gtm.csv")
wfm.maker("data/input/speeches/mex", "data/intermediates/wfm_matrices/wfm_mex.csv")
wfm.maker("data/input/speeches/per", "data/intermediates/wfm_matrices/wfm_per.csv")
wfm.maker("data/input/speeches/pry", "data/intermediates/wfm_matrices/wfm_pry.csv")
wfm.maker("data/input/speeches/slv", "data/intermediates/wfm_matrices/wfm_slv.csv")
wfm.maker("data/input/speeches/ury", "data/intermediates/wfm_matrices/wfm_ury.csv")
wfm.maker("data/input/speeches/ven", "data/intermediates/wfm_matrices/wfm_ven.csv")
                                                     
# Brazil is special, since portuguese
wfm.maker("data/input/speeches/bra", "data/intermediates/wfm_matrices/wfm_bra.csv", language = 'portuguese')
