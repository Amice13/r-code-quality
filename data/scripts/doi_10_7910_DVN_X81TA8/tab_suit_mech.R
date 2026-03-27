
#### define mediators
data.g25$crop_val_2000_sum[is.na(data.g25$crop_val_2000_sum)] <- 0
data.g25$crop_val_sqkm <- 1000*data.g25$crop_val_2000_sum/data.g25$area_land_sqkm
data.g25$crop_val_sqkm_log <- log(1+data.g25$crop_val_sqkm)

data.g25$road_dens_1960_log <- log(1+1000*data.g25$roads_imp_1960_km/data.g25$area_land_sqkm)

data.g25$rail_dens_1960_log <- log(1+1000*data.g25$rails_1960_km/data.g25$area_land_sqkm)
data.g25$rail_dens_1960_other_log <- log(1+1000*data.g25$rails_other_km/data.g25$area_land_sqkm)




latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
The dependent variables in Columns 1, 2, and 4 are defined as the natural logarithm of 1 plus (i) paved or improved road length in km per 1000 sqkm land area around 1960,
(ii) rails built for other than military or mining-related purposes in km per 1000 sqkm land area in 1960, and (iii) a cell-level estimate of crop production value in 1'000 USD per sqkm as of 2000. 
The outcome in column 3 is a cell-level dummy for power stations in 1972.
The main independent variable is the cell mean of agro-climatic suitability scores for nine cash crops from the FAO GAEZ database.
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")}



### First 1: Predict Mediators with cash crop suit and geographic controls
med.vars <- c("road_dens_1960_log","rail_dens_1960_other_log","plants_1972_yn","crop_val_sqkm_log")
m.list.med <- lapply(med.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=controls.geo, fe="iso3c_col",cl="iso3c_col", dat = data.g25)
  return(out25)
})

mean.dv <- sapply(med.vars,function(var){
  round(mean(data.g25[,var],na.rm=T),3)
})

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list.med))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list.med))),
                  latex.controls.hist(rep("\\xmark",length(m.list.med))),
                  latex.mean.dv(mean.dv))
# define which coefficients are shown in the output table

fileConn<-file(paste0(tab.path,"suit_mediators.tex"))
writeLines(stargazer(m.list.med,
                     title="Cash Crop Suitability \\& Mediators",
                     keep="suit_std",
                     column.sep.width = "30pt",
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Road Dens. 1960", "Rail Dens. 1960", "Power Plant 1972", "Crop Value 2000"),
                     covariate.labels=c("Cash Crop Suitability"),
                     font.size = "scriptsize",
                     notes.align = "c",label="suit_mediators",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)


### Do standard mediation with suit only: Predict Mediators with cash crop suit and geographic controls
out.vars <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")
m.list1 <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=c(controls.geo), fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})

m.list2 <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=c(controls.geo,"crop_val_sqkm_log"), fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})

m.list3 <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=c(controls.geo,"road_dens_1960_log","rail_dens_1960_other_log","plants_1972_yn"), fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})


m.list.med <- list(m.list3[[1]],m.list2[[1]],
                   m.list3[[2]],m.list2[[2]],
                   m.list3[[3]],m.list2[[3]],
                   m.list3[[4]],m.list2[[4]])

share.med <- sapply(c(1:8),function(i){
  shr <- 1- m.list.med[[i]]$coefficients["suit_std",]/m.list1[[round(i/2 + 0.1)]]$coefficients["suit_std",]
  return(paste0(round(100*shr,1)," \\%"))
})





latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The dependent variables in Columns 1-6  are defined as the natural logarithm of 1 plus (i,ii) paved or improved road length in km per 1000 sqkm land area in 1998, 
(iii,iv) the urban population per sqkm land area in 2015, (v,vi) total night lights per 100'000 capita in 2015.
The dependent variable in Columns 7 and 8 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped.
The main independent variable is the cell mean of agro-climatic suitability scores for nine cash crops from the FAO GAEZ database.
Mediators are (a) the logged paved or improved road length in km per 1000 sqkm land area around 1960,
(b) rails built for other than military or mining-related purposes in km per 1000 sqkm land area in 1960, 
(c) a cell-level dummy for power stations in 1972,
and (d) a FAO estimate of total crop production value in 1'000 USD per sqkm as of 2000. 
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}



keep.lines <- c("suit_std","road_dens_1960_log","rail_dens_1960_other_log","plants_1972_yn","crop_val_sqkm_log")

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list.med))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list.med))),
                  latex.controls.hist(rep("\\xmark",length(m.list.med))),
                  latex.share.mediated(share.med))

tab <-  stargazer(m.list.med,
                     title="Mechanisms: Path Dependence vs. Serial Correlation",
                     keep=keep.lines,
                     column.sep.width = "5pt",
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     column.labels = c("Roads","Cities","Lights","Wealth"),
                     column.separate = c(2,2,2,2),
                     dep.var.caption = "Outcome",dep.var.labels.include = F,
                     covariate.labels=c("Cash Crop Suitability",
                                        "Road Dens. 1960 (log)",
                                        "Rail Dens. 1960 (log)",
                                        "Power Plant 1972 (Y/N)",
                                        "Crop Value 2000 (log)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="mechanisms_suit",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F)
tab <- c(tab[c(1:12)],paste0("\\\\[-1.8ex] ",tab[13]),tab[c(14:length(tab))])
fileConn<-file(paste0(tab.path,"mechanisms_suit.tex"))
writeLines(tab,fileConn)
close(fileConn)


### Appendix: mediated by both PD and SC simultaneously

m.list4 <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=c(controls.geo,"road_dens_1960_log","rail_dens_1960_other_log","plants_1972_yn","crop_val_sqkm_log"), fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})


share.med.both <- sapply(c(1:4),function(i){
  shr <- 1- m.list4[[i]]$coefficients["suit_std",]/m.list1[[i]]$coefficients["suit_std",]
  return(paste0(round(100*shr,1)," \\%"))
})


latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The dependent variables in Columns 1-3  are defined as the natural logarithm of 1 plus (i) paved or improved road length in km per 1000 sqkm land area in 1998, 
(ii) the urban population per sqkm land area in 2015, (iii) total night lights per 100'000 capita in 2015.
The dependent variable in Columns 4 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped.
The main independent variable is the cell mean of agro-climatic suitability scores for nine cash crops from the FAO GAEZ database.
Mediators are (a) the logged paved or improved road length in km per 1000 sqkm land area around 1960,
(b) rails built for other than military or mining-related purposes in km per 1000 sqkm land area in 1960, 
(c) a cell-level dummy for power stations in 1972,
and (d) a FAO estimate of total crop production value in 1'000 USD per sqkm as of 2000. 
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}



add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list4))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list4))),
                  latex.controls.hist(rep("\\xmark",length(m.list4))),
                  latex.share.mediated(share.med.both))

fileConn<-file(paste0(tab.path,"mechanisms_suit_both.tex"))
writeLines(stargazer(m.list4,
                     title="Mechanisms: Path Dependence \\& Serial Correlation",
                     keep=keep.lines,
                     column.sep.width = "30pt",
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Road Dens. 1998", "Urban Pop. Dens. 2015", "Lights p.c. 2015", "HH Wealth"),
                     covariate.labels=c("Cash Crop Suitability",
                                        "Road Dens. 1960 (log)",
                                        "Rail Dens. 1960 (log)",
                                        "Power Plant 1972 (Y/N)",
                                        "Crop Value 2000 (log)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="mechanisms_suit_both",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)



#### do sequential G meidation with intermediate confounders
m.list.base <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=controls.geo, fe="iso3c_col", cl="mrdk_id", dat = data.g25)
  return(out25)
})

m.list.cropval <- lapply(out.vars,function(y){
  out25 <- mdlr.acde(y=y, x="suit_std",mediators = "crop_val_sqkm_log", conf.pre = controls.geo, conf.int = controls.std[c(7:17)],
                     fe="iso3c_col", cl="mrdk_id", dat = data.g25)
  return(out25)
})

m.list.infra <- lapply(out.vars,function(y){
  out25 <- mdlr.acde(y=y, x="suit_std",mediators = c("road_dens_1960_log","rail_dens_1960_other_log","plants_1972_yn"), conf.pre = controls.geo, conf.int = controls.std[c(7:17)], 
                     fe="iso3c_col", cl="mrdk_id", dat = data.g25)
  return(out25)
})



m.list.acde <- list(m.list.infra[[1]],m.list.cropval[[1]],
                    m.list.infra[[2]],m.list.cropval[[2]],
                    m.list.infra[[3]],m.list.cropval[[3]],
                    m.list.infra[[4]],m.list.cropval[[4]])
summary(m.list.acde[[1]])


share.med.acde <- sapply(c(1:8),function(i){
  shr <- 1- m.list.acde[[i]]$coefficients["suit_std",]/m.list.base[[round(i/2 + 0.1)]]$coefficients["suit_std",]
  return(paste0(round(100*shr,1)," \\%"))
})

latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The dependent variables in Columns 1-6  are defined as the natural logarithm of 1 plus (i,ii) paved or improved road length in km per 1000 sqkm land area in 1998, 
(iii,iv) the urban population per sqkm land area in 2015, (v,vi) total night lights per 100'000 capita in 2015.
The dependent variable in Columns 7 and 8 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped.
The main independent variable is the cell mean of agro-climatic suitability scores for nine cash crops from the FAO GAEZ database.
Mediators are (a) the logged paved or improved road length in km per 1000 sqkm land area around 1960,
(b) rails built for other than military or mining-related purposes in km per 1000 sqkm land area in 1960, 
(c) a cell-level dummy for power stations in 1972,
and (d) a FAO estimate of total crop production value in 1'000 USD per sqkm as of 2000. 
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}



keep.lines <- c("suit_std")

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list.acde))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list.acde))),
                  latex.controls.hist(rep("\\checkmark",length(m.list.acde))),
                  latex.share.mediated(share.med.acde))

tab <-  stargazer(m.list.acde,
                  title="Mechanisms: Path Dependence vs. Serial Correlation (ACDE)",
                  keep=keep.lines,
                  column.sep.width = "5pt",
                  model.numbers = F,
                  multicolumn=F,# se = se,
                  column.labels = c("Roads","Cities","Lights","Wealth"),
                  column.separate = c(2,2,2,2),
                  dep.var.caption = "Outcome", dep.var.labels = rep(c("PD","SC"),4),
                  dep.var.labels.include = T,
                  covariate.labels=c("Cash Crop Suit."),
                  font.size = "scriptsize",
                  notes.align = "c",label="mechanisms_suit_acde",align =F,
                  add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                  omit.stat = c("rsq","res.dev","ser"),
                  notes = latex.notes.long(1), notes.label = "", notes.append = F)
tab <- c(tab[c(1:12)],paste0("\\\\[-1.8ex] ",tab[14]),tab[13],tab[c(15:length(tab))])
fileConn<-file(paste0(tab.path,"mechanisms_suit_acde.tex"))
writeLines(tab,fileConn)
close(fileConn)



######### do sequential g with both PD and SC simultaneously
m.list.both <- lapply(out.vars,function(y){
  out25 <- mdlr.acde(y=y, x="suit_std",mediators = c("road_dens_1960_log","rail_dens_1960_other_log","plants_1972_yn","crop_val_sqkm_log"), conf.pre = controls.geo, conf.int = controls.std[c(7:17)],
                     fe="iso3c_col", cl="mrdk_id", dat = data.g25)
  return(out25)
})

share.med.both <- sapply(c(1:4),function(i){
  shr <- 1- m.list.both[[i]]$coefficients["suit_std",]/m.list.base[[i]]$coefficients["suit_std",]
  return(paste0(round(100*shr,1)," \\%"))
})

latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The dependent variables in Columns 1-3  are defined as the natural logarithm of 1 plus (i) paved or improved road length in km per 1000 sqkm land area in 1998, 
(ii) the urban population per sqkm land area in 2015, (iii) total night lights per 100'000 capita in 2015.
The dependent variable in Columns 4 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped.
The main independent variable is the cell mean of agro-climatic suitability scores for nine cash crops from the FAO GAEZ database.
Mediators are (a) the logged paved or improved road length in km per 1000 sqkm land area around 1960,
(b) rails built for other than military or mining-related purposes in km per 1000 sqkm land area in 1960, 
(c) a cell-level dummy for power stations in 1972,
and (d) a FAO estimate of total crop production value in 1'000 USD per sqkm as of 2000. 
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}


add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list.both))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list.both))),
                  latex.controls.hist(rep("\\checkmark",length(m.list.both))),
                  latex.share.mediated(share.med.both))

fileConn<-file(paste0(tab.path,"mechanisms_suit_acde_both.tex"))
writeLines(stargazer(m.list.both,
                     title="Mechanisms: Path Dependence \\& Serial Correlation (ACDE)",
                     keep=keep.lines,
                     column.sep.width = "40pt",
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Roads","Cities","Lights","Wealth"),
                     covariate.labels=c("Cash Crop Suit. (ACDE)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="mechanisms_suit_acde_both",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)


