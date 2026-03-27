
#### load global data and prepare variables
data.glob <- readRDS("data/data_global.rds")
names(data.glob)


### define logged 2015nightlights per land area
data.glob$nl_2015_sum[data.glob$nl_2015_sum<0]<-0
data.glob$nl_2015_sqkm_log <- log(1+1000*data.glob$nl_2015_sum/data.glob$area_land_hyde)

### standardize suitability scores and spatial lags
data.glob$suit8_std <- scale(data.glob$suit_low_mean8)[,1]
data.glob$cal.avgNo0_std <- scale(data.glob$cal.avgNo0)[,1]

lags <- names(data.glob)[grep("lag",names(data.glob))]
for(var in lags){
  data.glob[,paste0(var,"_std")] <- scale(data.glob[,var])[,1]
}

#### code lake and natural harbor dummies as in Henderson et al.
data.glob$onlake <- ifelse(data.glob$lakebigdist<25,1,0)
data.glob$onharb <- ifelse(data.glob$harbordist<25,1,0)


#### define control variables based on Henderson et al.
controls.hend <- c("rugged","elv","distc","malaria_suit_max","cal.avgNo0_std",
                  "lat","coastal","onriv","onlake","onharb",
                  "factor(ecosystems)","growday","tmp","precip")

#### Code Sub-Saharan African indicator
data.glob$ssa_dummy <- ifelse(data.glob$region_pol6=="SSA",1,0)

#### identify countries used in our baseline analysis
hance_countries <- as.character(unique(data.glob$natlarge[data.glob$region_pol=="SSA" & data.glob$region_wb23!="Southern Africa"]))
hance_countries <- hance_countries[!hance_countries%in%c(NA,"Sao Tome and Principe","Comoros","Madagascar","Mauritius", "Cape Verde")]


### Run interaction models
m1 <- mdlr(y="nl_2015_sqkm_log", x="suit8_std*ssa_dummy" , 
           cntr=paste0(controls.hend,"*ssa_dummy",collapse=" + "), fe="cowcode",cl="cowcode", 
           dat = data.glob[data.glob$natlarge%in%hance_countries | data.glob$region_pol%in%c("EA","SEA","SA","LA"),])
summary(m1)


m2 <- mdlr(y="nl_2015_sqkm_log", x="suit8_std*ssa_dummy + suit8_lag_max1_std*ssa_dummy" , 
           cntr=paste0(controls.hend,"*ssa_dummy",collapse=" + "), fe="cowcode",cl="cowcode", 
           dat = data.glob[data.glob$natlarge%in%hance_countries | data.glob$region_pol%in%c("EA","SEA","SA","LA"),])
summary(m2)

m3 <- mdlr(y="nl_2015_sqkm_log", x="suit8_std*ssa_dummy + suit8_lag_max2_std*ssa_dummy" , 
           cntr=paste0(controls.hend,"*ssa_dummy",collapse=" + "), fe="cowcode",cl="cowcode", 
           dat = data.glob[data.glob$natlarge%in%hance_countries | data.glob$region_pol%in%c("EA","SEA","SA","LA"),])
summary(m3)

m4 <- mdlr(y="nl_2015_sqkm_log", x="suit8_std*ssa_dummy + suit8_lag_max3_std*ssa_dummy" , 
           cntr=paste0(controls.hend,"*ssa_dummy",collapse=" + "), fe="cowcode",cl="cowcode", 
           dat = data.glob[data.glob$natlarge%in%hance_countries | data.glob$region_pol%in%c("EA","SEA","SA","LA"),])
summary(m4)

m5 <- mdlr(y="nl_2015_sqkm_log", x="suit8_std*ssa_dummy + suit8_lag_max4_std*ssa_dummy" , 
           cntr=paste0(controls.hend,"*ssa_dummy",collapse=" + "), fe="cowcode",cl="cowcode", 
           dat = data.glob[data.glob$natlarge%in%hance_countries | data.glob$region_pol%in%c("EA","SEA","SA","LA"),])
summary(m5)

##### Prepare plot data
row01  <- deltaMethod(m1,"suit8_std")
row02  <- deltaMethod(m1,"suit8_std + `suit8_std:ssa_dummy`")
row03  <- deltaMethod(m2,"suit8_std")
row04  <- deltaMethod(m2,"suit8_std + `suit8_std:ssa_dummy`")
row05  <- deltaMethod(m2,"suit8_lag_max1_std")
row06  <- deltaMethod(m2,"suit8_lag_max1_std + `ssa_dummy:suit8_lag_max1_std`")
row07  <- deltaMethod(m3,"suit8_std")
row08  <- deltaMethod(m3,"suit8_std + `suit8_std:ssa_dummy`")
row09  <- deltaMethod(m3,"suit8_lag_max2_std")
row10 <- deltaMethod(m3,"suit8_lag_max2_std + `ssa_dummy:suit8_lag_max2_std`")
row11 <- deltaMethod(m4,"suit8_std")
row12 <- deltaMethod(m4,"suit8_std + `suit8_std:ssa_dummy`")
row13 <- deltaMethod(m4,"suit8_lag_max3_std")
row14 <- deltaMethod(m4,"suit8_lag_max3_std + `ssa_dummy:suit8_lag_max3_std`")
row15 <- deltaMethod(m5,"suit8_std")
row16 <- deltaMethod(m5,"suit8_std + `suit8_std:ssa_dummy`")
row17 <- deltaMethod(m5,"suit8_lag_max4_std")
row18 <- deltaMethod(m5,"suit8_lag_max4_std + `ssa_dummy:suit8_lag_max4_std`")


plot.df <- data.frame(rbind(row01,
                            row02,
                            row03,
                            row04,
                            row05,
                            row06,
                            row07,
                            row08,
                            row09,
                            row10,
                            row11,
                            row12,
                            row13,
                            row14,
                            row15,
                            row16,
                            row17,
                            row18))
names(plot.df) <- c("beta","se","lb","ub")
plot.df$order <- c(22,21,19,18,17,16,14,13,12,11,9,8,7,6,4,3,2,1)
plot.df$what <- c("Cash Crop Suit. (non-Afr.)",
                  "Cash Crop Suit. (Afr.)",
                  "Cash Crop Suit. (non-Afr.)",
                  "Cash Crop Suit. (Afr.)",
                  "Max. Suit. NB1 (non-Afr.)",
                  "Max. Suit. NB1 (Afr.)",
                  "Cash Crop Suit. (non-Afr.)",
                  "Cash Crop Suit. (Afr.)",
                  "Max. Suit. NB2 (non-Afr.)",
                  "Max. Suit. NB2 (Afr.)",
                  "Cash Crop Suit. (non-Afr.)",
                  "Cash Crop Suit. (Afr.)",
                  "Max. Suit. NB3 (non-Afr.)",
                  "Max. Suit. NB3 (Afr.)",
                  "Cash Crop Suit. (non-Afr.)",
                  "Cash Crop Suit. (Afr.)",
                  "Max. Suit. NB4 (non-Afr.)",
                  "Max. Suit. NB4 (Afr.)")


### make plot
p <- ggplot(plot.df)
p <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=16) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  labs(title="Beyond Africa: Suitability & Spatial Spillovers", subtitle = "Sub-Saharan Africa vs. Latin America and Asia (SEA, EA, SA)", x="Marginal Effect and 95% Confidence Interval", caption="Based on interaction models from Table A12") +
  scale_y_continuous(name="Suitability & Spatial Lag", breaks = plot.df$order, labels = plot.df$what, limits=c(0.5,22.5)) +
  #scale_x_continuous(limits=c(-0.2,0.9)) +
  theme_minimal(base_size=12) +  
  theme(axis.text.y = element_text(size=10), plot.title = element_text(size=16)) 
p
p + ggsave("output/tables_wd/afr_rest_interactions.pdf",width=7.5, height=7.5)



###### Prepare and output regression table
m.list <- list(m1,m2,m3,m4,m5)

latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The sample includes all cells within the Sub-Saharan African countries covered by our baseline analysis as well as all Latin American, South, East, and Southeast Asian cells. 
  The dependent variable is defined as the natural logarithm of 1 plus total night lights per 1000 sqkm in 2015.
Geographic control variables are based on Henderson et al. (2018) and include: ruggedness, elevation, distance to coast, 
malaria suitability, caloric suitability, absolute latitude, temperature, precipitation, length of the growing season,
14 biome indicators, and four dummies indicating whether a cell is located within 25 km of a coastline, navigable river, big lake, and natural harbor.
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}


add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep("\\xmark",length(m.list))))
# define which coefficients are shown in the output table

coefs <- c(rownames(m.list[[1]]$coefficients),
           rownames(m.list[[2]]$coefficients),
           rownames(m.list[[3]]$coefficients),
           rownames(m.list[[4]]$coefficients),
           rownames(m.list[[5]]$coefficients))
coefs <- unique(coefs)
suit.coefs <- which(grepl("suit8",coefs))
coefs[suit.coefs]


# prepare and save output table
summary(m1)

?stargazer

fileConn<-file(paste0(tab.path,"beyond_africa.tex"))
writeLines(stargazer(m.list,
                     title="Africa vs. Latin America and Asia",
                     keep=c("suit8","lag"),
                     column.sep.width = "25pt",
                     model.numbers = T,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Nightlights per sqkm (log)",
                     covariate.labels=c("Cash Crop Suit. (Std.)", 
                                        "Max. Cash Crop Suit. NB1 (Std.)", 
                                        "Max. Cash Crop Suit. NB2 (Std.)", 
                                        "Max. Cash Crop Suit. NB3 (Std.)", 
                                        "Max. Cash Crop Suit. NB4 (Std.)", 
                                        "Cash Crop Suit. $\\times$ SSA",
                                        "Max. Suit. NB1  $\\times$ SSA",
                                        "Max. Suit. NB2  $\\times$ SSA",
                                        "Max. Suit. NB3  $\\times$ SSA",
                                        "Max. Suit. NB4  $\\times$ SSA"),
                     font.size = "scriptsize",
                     notes.align = "c",label="beyond_africa",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)








