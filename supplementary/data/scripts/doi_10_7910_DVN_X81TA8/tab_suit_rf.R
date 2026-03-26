
#install.packages("patchwork")

library(patchwork)


## add suitability 
# add.suit <- readRDS(here("data","add_suit_g25.rds"))
# names(add.suit)
# data.g25 <- left_join(data.g25,add.suit[,c(1,27:35)])
# data.g25$suit_std7 <- scale(apply(data.g25[,c("suit_coc","suit_cof","suit_cot","suit_grd","suit_plm","suit_tea","suit_tob")],1,mean,na.rm=T))[,1]
# data.g25$suit_std8 <- scale(apply(data.g25[,c("suit_coc","suit_cof","suit_cot","suit_grd","suit_plm","suit_tea","suit_tob","suit_sug")],1,mean,na.rm=T))[,1]

### Exog 1: suitability RF, continuous DV, Murdock placebo, horse race with control vars
out.vars <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")
m.list <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=controls.std, fe="iso3c_col",cl="iso3c_col", dat = data.g25)
  return(out25)
})
sapply(m.list,function(x){exp(coef(x)[1])-1})


m.list.geo <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=controls.geo, fe="iso3c_col",cl="iso3c_col", dat = data.g25)
  return(out25)
})

m.list <- list(m.list.geo[[1]],m.list[[1]],
               m.list.geo[[2]],m.list[[2]],
               m.list.geo[[3]],m.list[[3]],
               m.list.geo[[4]],m.list[[4]])

latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
The dependent variables in Columns 1-6  are defined as the natural logarithm of 1 plus (i,ii) paved or improved road length in km per 1000 sqkm land area in 1998, 
(iii,iv) the urban population per sqkm land area in 2015, (v,vi) total night lights per 100'000 capita in 2015.
The dependent variable in Columns 7 and 8 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped.
The main independent variable is the cell mean of agro-climatic suitability scores for nine cash crops from the FAO GAEZ database.
Geographic control variables not shown: Longitude, latitude, and their squares. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

mean.dv <- sapply(out.vars,function(var){
  round(mean(data.g25[,var],na.rm=T),3)
})

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep(c("\\xmark","\\checkmark"),length(m.list)/2)),
                  latex.mean.dv(rep(mean.dv,each=2)))
# define which coefficients are shown in the output table
keep.lines <- rownames(m.list[[2]]$coefficients)[which(!grepl("lon|lat",rownames(m.list[[2]]$coefficients),ignore.case = T))]


# prepare and save output table

tab <- stargazer(m.list,
                 title="Cash Crop Suitability \\& Contemporary Development",
                 keep=keep.lines,
                 order=c(1:18),
                 column.sep.width = "0pt",
                 model.numbers = F,
                 multicolumn=F,# se = se,
                 column.labels = c("Roads","Cities","Lights","Wealth"),
                 column.separate = c(2,2,2,2),
                 dep.var.caption = "Outcome",dep.var.labels = c("Roads","Cities","Lights","Wealth"),
                 dep.var.labels.include = F,
                 covariate.labels=c("Cash Crop Suit.", 
                                    "Caloric Suit.",
                                    "TseTse Suit.",
                                    "Malaria Suit.",
                                    "Ruggedness",
                                    "Elevation",
                                    "Dist. River Nav.",
                                    "Dist. Coast",
                                    #"Dist. Expl.",
                                    "Dist. City",
                                    "Dist. Capital",
                                    "Dist. Trade",
                                    "Slaves (Med.)",
                                    "Slaves (High)",
                                    "Agric. (Med.)",
                                    "Agric. (High)",
                                    "Precol. State",
                                    "Precol. Chiefdom",
                                    "Precol Centr. NA"),
                 font.size = "scriptsize",
                 notes.align = "c",label="suit_rf_benchmark",align =F,
                 add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                 omit.stat = c("rsq","res.dev","ser"),
                 digit.separator = "'",
                 notes = latex.notes.long(1.0), notes.label = "", notes.append = F, no.space = T)


tab <- c(tab[c(1:12)],paste0("\\\\[-1.8ex] ",tab[13]),tab[c(14:length(tab))])
fileConn<-file(paste0(tab.path,"suit_rf_benchmark.tex"))
writeLines(tab,fileConn)
close(fileConn)


##########################################################################
############
#### do plots

plot.ls <- lapply(m.list[c(2,4,6,8)],function(i){
  get_plot_data(i,c(1:18))
})

summary(m.list[[8]])

controls.std2

plot.df <- do.call(rbind,plot.ls)
out.labs <- c("Road Density 1998 (log)","Urb. Pop. Dens. 2015 (log)","Night Lights 2015 pc (log)","DHS Household Wealth")
plot.df$outcome <- rep(out.labs,each=18)
plot.df$outcome <- factor(plot.df$outcome,levels=out.labs)
plot.df$what <- rep(c("Cash Crop Suit.", 
                      "Caloric Suit.",
                      "TseTse Suit.",
                      "Malaria Suit.",
                      "Ruggedness",
                      "Elevation",
                      "Dist. River Nav.",
                      "Dist. Coast",
                      #"Dist. Expl.",
                      "Dist. City",
                      "Dist. Capital",
                      "Dist. Trade",
                      "Slaves (Med.)",
                      "Slaves (High)",
                      "Agric. (Med.)",
                      "Agric. (High)",
                      "Precol. State",
                      "Precol. Chiefdom",
                      "Precol Centr. NA"),4)
plot.df$order <- rep(c(18:1),4)

plot.df <- plot.df[order(plot.df$outcome,plot.df$beta,decreasing=T),]
plot.df$order2 <- rep(c(18:1),4)


plot.ls <- lapply(c(1:4),function(i){
  
  p <- ggplot(plot.df[plot.df$outcome==levels(plot.df$outcome)[i],])
  p <- p + geom_point(size=2.75,aes(x=beta,y=order2),shape=16) + 
    geom_errorbarh(aes(x=beta,y=order2, xmin=lb, xmax=ub), size=1, height=0.0) +
    geom_vline(xintercept=0,linetype="dotted", size=0.6) +
    geom_vline(xintercept=plot.df$beta[plot.df$what=="Cash Crop Suit." & plot.df$outcome==levels(plot.df$outcome)[i]],linetype="dotted", size=0.6,col="red") +
    geom_vline(xintercept=-plot.df$beta[plot.df$what=="Cash Crop Suit." & plot.df$outcome==levels(plot.df$outcome)[i]],linetype="dotted", size=0.6,col="red") +
    scale_y_continuous(breaks=plot.df$order2[plot.df$outcome==levels(plot.df$outcome)[i]],minor_breaks = NULL, 
                       labels=plot.df$what[plot.df$outcome==levels(plot.df$outcome)[i]],limits = c(0.5,19.5)) + 
    labs(x = "Coefficients and 95% Confidence Intervals", y = "Predictor",
         title=levels(plot.df$outcome)[i]) +
    #facet_wrap(~ outcome,nrow=3,scales="free_x")  +
    theme_minimal(base_size=12) +  
    theme(axis.text.y = element_text(size=10)) 
  return(p)
  
})

print(plot.ls[[1]])
library(patchwork)
p.out <- (plot.ls[[1]] | plot.ls[[2]])/ 
  (plot.ls[[3]] | plot.ls[[4]])
p.out
p.out + ggsave(paste0(fig.path,"suit_rf_horserace.pdf"),width=10,height=10)


### bring new state history in
statehist <- read.csv("data/statehiste.csv",stringsAsFactors = F)
names(statehist)
statehist <- statehist[,c(1,2,113:143,156:161)]
library(countrycode)
statehist$iso3c_col <- countrycode(statehist$wbname,"country.name","iso3c")

data.g25 <- left_join(data.g25,statehist)



data.g25$mrdk_state_chiefdom <- ifelse(data.g25$mrdk_precol_centr>1,1,0)
data.g25$statehist_log <- log(data.g25$statehiste00)



placebo.vars <- c("hance_crops_dummy","cities_log","trade_log","mrdk_state_chiefdom","slaves_area_ln","statehist_log")

m.list.placebo <- lapply(placebo.vars,function(y){
  out25 <- mdlr(y=y, x="suit_std", cntr=controls.geo, fe="0",cl="iso3c_col", dat = data.g25)
  return(out25)
})
summary(m.list.placebo[[4]])


latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
The dependent variables are (i) a dummy for cash crop production in 1957, (ii) logged distance to cities in 1900, 
(iii) logged distance to trade routes in 1900, (iv) a dummy if the cell falls within a Murdock group polygon with a high level of political centralization (chiefdom or higher),
(v) the logged number of slaves per ethnic polygon area as provided by Nunn and Wantchekon (2011), 
and (vi) the logged number of years since the onset of centralized statehood as measured on the country-level by Borcan, Olsson and Putterman (2018).
The main predictor is the cell mean of agroclimatic suitability for cocoa, coffee, cotton, groundnuts, oil palm, tea, tobacco, sugarcane, and bananas.
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Standard errors clustered on countries in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

mean.dv <- sapply(placebo.vars,function(var){
  round(mean(data.g25[,var],na.rm=T),3)
})

add.lines <- list(latex.cntr.fe(rep("\\xmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep("\\xmark",length(m.list))),
                  latex.mean.dv(mean.dv))
# define which coefficients are shown in the output table
keep.lines <- which(grepl("suit",rownames(m.list.placebo[[1]]$coefficients)))

# prepare and save output table

tab <- stargazer(m.list.placebo,
                 title="Cash Crop Suitability \\& Precolonial Development",
                 keep=keep.lines,
                 column.sep.width = "+10pt",
                 model.numbers = F,
                 multicolumn=F,# se = se,
                 dep.var.caption = "Outcome",dep.var.labels = c("Cash Crops 1957", "Dist. Cities", "Dist. Trade Route", "Murdock Centr.","Slaves", "State Hsitory"),
                 covariate.labels=c("Cash Crop Suitability"),
                 font.size = "scriptsize",
                 notes.align = "c",label="suit_placebo",align =F,
                 add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                 omit.stat = c("rsq","res.dev","ser"),
                 digit.separator = "'",
                 notes = latex.notes.long(1.0), notes.label = "", notes.append = F)

fileConn<-file(paste0(tab.path,"suit_placebo.tex"))
writeLines(tab,fileConn)
close(fileConn)
