### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### R1: Continuous predictor: continuous DV, cont predictor
out.vars.cont <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")


m.list <- lapply(out.vars.cont,function(y){
  out25 <- mdlr(y=y, x=c("crops_sqkm_log","minerals_sqkm_log"), cntr=controls.std, fe="iso3c", cl="mrdk_id", dat = data.g25)
  return(out25)
})
 
latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The dependent variables in Columns 1-3 are defined as the natural logarithm of 1 plus (i) paved or improved road length in km per 1000 sqkm land area in 1998, 
  (ii) the urban population per sqkm land area in 2015, (iii) total night lights per 100'000 capita in 2015. 
The dependent variable in Column 4 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped. Predictor variables are the logged cell-level production values of cash crops and minerals in 1'000 USD (nominal, as of 1957) per sqkm land area.
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Historical controls are logged minimum distances to trade routes in 1900, cities in 1900, and the first colonial capital, as well as
ethnic group level proxies for precolonial reliance on agriculture, political centralization, and exposure to the slave trades. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

mean.dv <- sapply(out.vars.cont,function(var){
  round(mean(data.g25[,var],na.rm=T),3)
})

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep("\\checkmark",length(m.list))),
                  latex.mean.dv(mean.dv))
# define which coefficients are shown in the output table
keep.lines <- which(grepl("crops|minerals",rownames(m.list[[2]]$coefficients)))

# prepare and save output table


fileConn<-file(paste0(tab.path,"base_cont_pred.tex"))
writeLines(stargazer(m.list,
                     title="Continuous Predictors",
                     keep=keep.lines,
                     column.sep.width = "30pt",
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Road Dens.", "Urban Pop. Dens.", "Lights p.c.", "HH Wealth"),
                     covariate.labels=c("Cash Crops per sqkm (log)", 
                                        "Minerals per sqkm (log)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="base_cont_pred",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)
