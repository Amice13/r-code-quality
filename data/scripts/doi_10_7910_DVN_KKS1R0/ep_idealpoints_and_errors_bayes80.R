######################################################################
##  Implementation as bayesian model - parallelised                 ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################

cat("I started Arg","\n")
# We ran arg.nb.b.80 <- ep.wf.negbin.b(tdmat_arg80, nr.chains = 5, nr.iter = 15000)
arg.nb.b.80 <- ep.wf.negbin.b(tdmat_arg80)
save(arg.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_arg_negbin_b_80.RData')

cat("I started Bra","\n")
# We ran bra.nb.b.80 <- ep.wf.negbin.b(tdmat_bra80, nr.chains = 5, nr.iter = 20000)
bra.nb.b.80 <- ep.wf.negbin.b(tdmat_bra80)
save(bra.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_bra_negbin_b_80.RData')

cat("I started Chi ","\n")
# We ran chi.nb.b.80 <- ep.wf.negbin.b(tdmat_chi80, nr.chains = 5, nr.iter = 45000)
chi.nb.b.80 <- ep.wf.negbin.b(tdmat_chi80)
save(chi.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_chi_negbin_b_80.RData')

cat("I started Col ","\n")
# We ran col.nb.b.80 <- ep.wf.negbin.b(tdmat_col80, nr.chains = 3, nr.iter = 25000)
col.nb.b.80 <- ep.wf.negbin.b(tdmat_col80)
save(col.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_col_negbin_b_80.RData')

cat("I started Cri ","\n")
# We ran cri.nb.b.80 <- ep.wf.negbin.b(tdmat_cri80, nr.chains = 5, nr.iter = 40000)
cri.nb.b.80 <- ep.wf.negbin.b(tdmat_cri80)
save(cri.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_cri_negbin_b_80.RData')

cat("I started Ecu ","\n")
# We ran ecu.nb.b.80 <- ep.wf.negbin.b(tdmat_ecu80, nr.chains = 5, nr.iter = 25000)
ecu.nb.b.80 <- ep.wf.negbin.b(tdmat_ecu80)
save(ecu.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_ecu_negbin_b_80.RData')

cat("I started Gtm ","\n")
# We ran gtm.nb.b.80 <- ep.wf.negbin.b(tdmat_gtm80,nr.chains = 3, nr.iter = 35000)
gtm.nb.b.80 <- ep.wf.negbin.b(tdmat_gtm80)
save(gtm.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_gtm_negbin_b_80.RData')

cat("I started Mex ","\n")
# We ran mex.nb.b.80 <- ep.wf.negbin.b(tdmat_mex80, nr.chains = 3, nr.iter = 35000)
mex.nb.b.80 <- ep.wf.negbin.b(tdmat_mex80)
save(mex.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_mex_negbin_b_80.RData')

cat("I started Per ","\n")
# We ran per.nb.b.80 <- ep.wf.negbin.b(tdmat_per80, nr.chains = 3, nr.iter = 35000)
per.nb.b.80 <- ep.wf.negbin.b(tdmat_per80)
save(per.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_per_negbin_b_80.RData')

cat("I started Pry ","\n")
# We ran pry.nb.b.80 <- ep.wf.negbin.b(tdmat_pry80, nr.chains = 5, nr.iter = 30000)
pry.nb.b.80 <- ep.wf.negbin.b(tdmat_pry80)
save(pry.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_pry_negbin_b_80.RData')

cat("I started Slv ","\n")
# We ran slv.nb.b.80 <- ep.wf.negbin.b(tdmat_slv80, nr.chains = 5, nr.iter = 30000)
slv.nb.b.80 <- ep.wf.negbin.b(tdmat_slv80)
save(slv.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_slv_negbin_b_80.RData')

cat("I started Ury ","\n")
# We ran ury.nb.b.80 <- ep.wf.negbin.b(tdmat_ury80, nr.chains = 3, nr.iter = 25000)
ury.nb.b.80 <- ep.wf.negbin.b(tdmat_ury80)
save(ury.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_ury_negbin_b_80.RData')

cat("I started Ven ","\n")
# We ran ven.nb.b.80 <- ep.wf.negbin.b(tdmat_ven80, nr.chains = 3, nr.iter = 25000)
ven.nb.b.80 <- ep.wf.negbin.b(tdmat_ven80)
save(ven.nb.b.80, file = 'data/intermediates/bayesdraws/pres_pos_ven_negbin_b_80.RData')