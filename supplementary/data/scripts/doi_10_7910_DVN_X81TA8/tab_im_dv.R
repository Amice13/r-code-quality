### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### R2: Continuous predictor: int. margin DV, cont & binary predictors
out1 <- mdlr(y="road_dens_2015_log", x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std, fe="iso3c",  cl="mrdk_id", dat = data.g25[data.g25$road_dens_2015_log!=0,])
out2 <- mdlr(y="road_dens_2015_log", x=c("crops_sqkm_log","minerals_sqkm_log"), cntr=controls.std, fe="iso3c",  cl="mrdk_id", dat = data.g25[data.g25$road_dens_2015_log!=0,])
out3 <- mdlr(y="urb_dens_2015_log", x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std, fe="iso3c",  cl="mrdk_id", dat = data.g25[data.g25$urb_dens_2015_log!=0,])
out4 <- mdlr(y="urb_dens_2015_log", x=c("crops_sqkm_log","minerals_sqkm_log"), cntr=controls.std, fe="iso3c",  cl="mrdk_id", dat = data.g25[data.g25$urb_dens_2015_log!=0,])
out5 <- mdlr(y="nl_2015_pc_log", x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std, fe="iso3c",  cl="mrdk_id", dat = data.g25[data.g25$nl_2015_pc_log!=0,])
out6 <- mdlr(y="nl_2015_pc_log", x=c("crops_sqkm_log","minerals_sqkm_log"), cntr=controls.std, fe="iso3c", cl="mrdk_id", dat = data.g25[data.g25$nl_2015_pc_log!=0,])
m.list <- list(out1,out2,out3,out4,out5,out6)



latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  Sample restricted to cells with non-zero values on road length (Columns 1 and 2), cities (Columns 3 and 4), and night lights (Columns 5 and 6).
  The dependent variables in Columns 1-6 are defined as the natural logarithm of 1 plus (i,ii) paved or improved road length in km per 1000 sqkm land area in 1998, 
  (iii,iv) the urban population per sqkm land area in 2015, (v,vi) total night lights per 100'000 capita in 2015. 
Predictor variables in odd-numbered columns are binary indicators of colonial cash crop or mineral production.  
Predictor variables in even-numbered columns are the logged cell-level production values of cash crops and minerals in 1'000 USD (nominal, as of 1957) per sqkm land area.
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Historical controls are logged minimum distances to trade routes in 1900, cities in 1900, and the first colonial capital, as well as
ethnic group level proxies for precolonial reliance on agriculture, political centralization, and exposure to the slave trades. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}



add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep("\\checkmark",length(m.list))))
# define which coefficients are shown in the output table
keep.lines <- c("hance_crops_dummy","hance_minerals_dummy","crops_sqkm_log","minerals_sqkm_log")

# prepare and save output table


fileConn<-file(paste0(tab.path,"base_int_margin_dv.tex"))
writeLines(stargazer(m.list,
                     title="Intensive Margin: Development Outcomes",
                     keep=keep.lines,
                     column.sep.width = "22pt",
                     column.labels = c("Road Dens.", "Urban Pop. Dens.", "Night Lights p.c."),
                     column.separate =c(2,2,2),
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels.include = F,
                     covariate.labels=c("Cash Crops (Y/N)",
                                        "Minerals (Y/N)",
                                        "Cash Crops per sqkm (log)", 
                                        "Minerals per sqkm (log)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="base_int_margin_dv",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)
