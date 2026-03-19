######################################################################
## Save data                                                        ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################


save(dat.pres.pos.arg80, file = 'data/output/positions/pres_pos_arg80.RData')
save(dat.pres.pos.bra80, file = 'data/output/positions/pres_pos_bra80.RData')
save(dat.pres.pos.chi80, file = 'data/output/positions/pres_pos_chi80.RData')
save(dat.pres.pos.col80, file = 'data/output/positions/pres_pos_col80.RData')
save(dat.pres.pos.cri80, file = 'data/output/positions/pres_pos_cri80.RData')
save(dat.pres.pos.ecu80, file = 'data/output/positions/pres_pos_ecu80.RData')
save(dat.pres.pos.gtm80, file = 'data/output/positions/pres_pos_gtm80.RData')
save(dat.pres.pos.mex80, file = 'data/output/positions/pres_pos_mex80.RData')
save(dat.pres.pos.per80, file = 'data/output/positions/pres_pos_per80.RData')
save(dat.pres.pos.pry80, file = 'data/output/positions/pres_pos_pry80.RData')
save(dat.pres.pos.slv80, file = 'data/output/positions/pres_pos_slv80.RData')
save(dat.pres.pos.ury80, file = 'data/output/positions/pres_pos_ury80.RData')
save(dat.pres.pos.ven80, file = 'data/output/positions/pres_pos_ven80.RData')

# once all data sets are created, merge them to one large masterfile
load(file = 'data/output/positions/pres_pos_arg80.RData')
load(file = 'data/output/positions/pres_pos_bra80.RData')
load(file = 'data/output/positions/pres_pos_chi80.RData')
load(file = 'data/output/positions/pres_pos_col80.RData')
load(file = 'data/output/positions/pres_pos_cri80.RData')
load(file = 'data/output/positions/pres_pos_ecu80.RData')
load(file = 'data/output/positions/pres_pos_gtm80.RData')
load(file = 'data/output/positions/pres_pos_mex80.RData')
load(file = 'data/output/positions/pres_pos_per80.RData')
load(file = 'data/output/positions/pres_pos_pry80.RData')
load(file = 'data/output/positions/pres_pos_slv80.RData')
load(file = 'data/output/positions/pres_pos_ury80.RData')
load(file = 'data/output/positions/pres_pos_ven80.RData')

# and save
dat.pres.pos.all80 <- rbind(dat.pres.pos.arg80
                           ,dat.pres.pos.bra80
                           ,dat.pres.pos.chi80
                           ,dat.pres.pos.col80
                           ,dat.pres.pos.cri80
                           ,dat.pres.pos.ecu80
                           ,dat.pres.pos.gtm80
                           ,dat.pres.pos.mex80
                           ,dat.pres.pos.per80
                           ,dat.pres.pos.pry80
                           ,dat.pres.pos.slv80
                           ,dat.pres.pos.ury80
                           ,dat.pres.pos.ven80)

# there you go: the master data 
save(dat.pres.pos.all80, file = 'data/output/positions/pres_pos_all80.RData')
write.csv(dat.pres.pos.all80, file = "data/output/positions/ADW_positions.csv")
write.dta(dat.pres.pos.all80, file = "data/output/ADW_MasterData.dta")

