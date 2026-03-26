######## combined table with binary and cont outcomes, 7 columns
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Baseline OLS: extensive margin DV, dummy predictor

out.vars <- c("roads_imp_1998_yn","road_dens_2015_log","cities_2015_yn","urb_dens_2015_log","nl_2015_dummy","nl_2015_pc_log","hh_wealth")
m.list <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})



latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
The dependent variables in Columns 1, 3, and 5 are dummies indicating cells that (i) intersect with a paved or improved road in 1998, 
(iii) host a city with more than 10'000 inhabitants in 2015, (v) emit non-zero night lights in 2015. 
The dependent variables in Columns 2, 4, and 6 are defined as the natural logarithm of 1 plus (ii) paved or improved road length in km per 1000 sqkm land area in 1998, 
(iv) the urban population per sqkm land area in 2015, (vi) total night lights per 100'000 capita in 2015.
The dependent variable in Column 4 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped. 
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Historical controls are logged minimum distances to trade routes in 1900, cities in 1900, and the first colonial capital, as well as
ethnic group level proxies for precolonial reliance on agriculture, political centralization, and exposure to the slave trades. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

mean.dv <- sapply(out.vars,function(var){
  round(mean(data.g25[,var],na.rm=T),3)
})

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep("\\checkmark",length(m.list))),
                  latex.mean.dv(mean.dv))
# define which coefficients are shown in the output table
keep.lines <- which(grepl("hance",rownames(m.list[[1]]$coefficients)))

# prepare and save output table

tab <- stargazer(m.list,
                 title="Colonial Resources \\& Contemporary Development",
                 keep=keep.lines,
                 column.sep.width = "+10pt",
                 model.numbers = F,
                 multicolumn=F,# se = se,
                 column.labels = c("Roads","Cities","Lights","Wealth"),
                 column.separate = c(2,2,2,2),
                 dep.var.caption = "Outcome",dep.var.labels = c("(Y/N)", "log", "(Y/N)", "log","(Y/N)", "log","cell mean"),
                 covariate.labels=c("Cash Crops (Y/N)", 
                                    "Minerals (Y/N)"),
                 font.size = "scriptsize",
                 notes.align = "c",label="baseline",align =F,
                 add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                 omit.stat = c("rsq","res.dev","ser"),
                 digit.separator = "'",
                 notes = latex.notes.long(1.0), notes.label = "", notes.append = F)


tab <- c(tab[c(1:12)],paste0("\\\\[-1.8ex] ",tab[14]),tab[13],tab[c(15:length(tab))])
fileConn<-file(paste0(tab.path,"base_7columns.tex"))
writeLines(tab,fileConn)
close(fileConn)






tab <- stargazer(m.list,
                 title="Colonial Resources",
                 keep=keep.lines,
                 column.sep.width = "+10pt",
                 model.numbers = F,
                 multicolumn=F,# se = se,
                 column.labels = c("Roads","Cities","Lights","Wealth"),
                 column.separate = c(2,2,2,2),
                 dep.var.caption = "Outcome",dep.var.labels = c("(YN)", "log", "(YN)", "log","(YN)", "log","cell mean"),
                 covariate.labels=c("Cash Crops", 
                                    "Minerals"),
                 #font.size = "scriptsize",
                 notes.align = "c",label="baseline",align =F,
                 add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                 omit.stat = c("rsq","res.dev","ser"),
                 digit.separator = "'",
                 notes = latex.notes.long(1.0), notes.label = "", notes.append = F,
                 type="html")


tab <- c(tab[c(1:12)],paste0("\\\\[-1.8ex] ",tab[14]),tab[13],tab[c(15:length(tab))])
fileConn<-file(paste0(tab.path,"base_7columns.txt"))
writeLines(tab,fileConn)
close(fileConn)

?stargazer
