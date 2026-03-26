#############################
# MAIN PLOTS AND TABLES
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/analysis/analysis_all.R
#
#############################

# LOAD GLOBALS AND DATA ##
if(!exists("LOADED_GLOBALS")){
  source("scripts/analysis/analysis_globals.R")
}
load_data(reload = F)


# MAIN RESULTS ###

# Load models 

## Pooled
pool.model.ls <- readRDS(file.path(tem_res.path, "pool_model.ls.rds"))
pool.se.ls <- readRDS(file.path(tem_res.path, "pool_se.ls.rds"))

## Fragments
cut.model.ls <- readRDS(file.path(tem_res.path, "cut_model.ls.rds"))
cut.se.ls <- readRDS(file.path(tem_res.path, "cut_se.ls.rds"))

cutxsec.model.ls <- readRDS(file.path(tem_res.path, "cutxsec_model.ls.rds"))
cutxsec.se.ls <- readRDS(file.path(tem_res.path, "cutxsec_se.ls.rds"))




# Pooled models: All variables, table ####

# Stub
stub <- "pool"

# Var labs
var.labs <- c("Constant",
              "Edge length","River", "Watershed", "Elevation mean",
              "Ethnic boundary$_t$", "Deep lag" , 
              "State border$_{t-1}$","Ethnic boundary$_{t-1}$")

# Notes
latex.notes <- function(width = .95, add = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textit{Notes:} 
  Each period $t$ has a length of 25 years. ", add,
  "95\\% confidence intervals from parametric bootstrap in parenthesis. 
  $^*$ Statistically significant at the 95\\% level.
           }  ")
}

# Output
pspm2table(list(pool.model.ls[[2]], pool.model.ls[[1]]), type = "latex",
            file = file.path(tab.path, paste0(stub,".tex")),
            label = stub,
            caption = "Determinants of state borders in Europe, 1886--2011",
            stars = c(.01, .05, .1), 
            custom.coef.names = var.labs,
            custom.model.names = c("1: Baseline","2: Lagged Dep. Var."),
            dcolumn = T, use.packages = F,
            add.stats = c(`No. of periods` = "N_instances",
                          `No. of vertices` =  "N",
                          `No. of edges` =  "N_edges",
                          `No. of states` = "N_groups"),
            custom.gof.rows = list(`Controls` = c("yes", "yes")), 
            include.nobs = F,
            bootci = lapply(list(pool.se.ls[[2]], pool.se.ls[[1]]), function(s){s$ci_mat}),
            include.loglik = F,
            custom.note = latex.notes(.8),
            reorder.coef = c(1, 2,5, 4,3), 
            omit.coef = "water|elev|river|length"
)
readLines(file.path(tab.path, "pool.tex"))



# Magnitude: Interpretation for bridge edges

## Median covariate values
med.val <- lapply(seq_along(pool.model.ls), function(i){
  g_ls <- pool.model.ls[[i]]$g_ls
  m <- sapply(names(pool.model.ls[[i]]$estimate)[-1], function(n){
    median(unlist(sapply(g_ls, function(g){edge_attr(g, n)})),
           na.rm = T)
  })
  names(m) <- names(pool.model.ls[[i]]$estimate)[-1]
  m
})

## Hazard ratio

### Estimate
haz.rat <- sapply(c(1,2), function(i){
  e <- pool.model.ls[[i]]$estimate
  exp(e[length(e)])
})

### CI
haz.rat.ci <- lapply(c(1,2), function(i){
  e <- pool.model.ls[[i]]$estimate
  c(exp(pool.se.ls[[i]]$ci_mat[3,length(e)]),
    exp(pool.se.ls[[i]]$ci_mat[4,length(e)]))
})

### Out
labs <- c("ldv", "base")
for(l in seq_along(labs)){
  writeLines(effect_text(coef = haz.rat[l],
                         ci = haz.rat.ci[[l]],
                         accuracy = 1),
             file.path(num.path, paste0("hr_", labs[l],".tex")))
}



## Effect at median values

### Function
meff_fun <- function(e, val){

  ## Ethnic border
  val[length(e)] <- 1
  o1 <- exp(sum(e * val))
  p1 <- o1/(1+o1)
  
  ## Ethnic border
  val[length(e)] <- 0
  o2 <- exp(sum(e * val))
  p2 <- o2/(1+o2)
  
  return(c(p1 = p1, p2 = p2, diff = p1 - p2))
}

### Estimate
mef.rat <- lapply(c(1,2), function(i){
  e <- pool.model.ls[[i]]$estimate
  val <- c(1, med.val[[i]])
  meff_fun(e, val)
})

### CI
mef.rat.ci <- lapply(c(1,2), function(i){
  
  ## Bootstrapped estimates
  mef.mat <- do.call(rbind, lapply(1:nrow(pool.se.ls[[i]]$beta_boot), function(j){
    val <- c(1, med.val[[i]])
    meff_fun(pool.se.ls[[i]]$beta_boot[j,], val)
  }))
  
  ## 95 percent CI
  lapply(c(.025, .975), function(p){
    as.vector(apply(mef.mat, 2, quantile, p = p))
  })
})

### Out
labs <- list(ldv = c("eth", "noeth", "diff"),
             base = c("eth", "noeth", "diff"))
for(l in seq_along(labs)){
  for(i in seq_along(labs[[l]])){
    writeLines(effect_text(coef = mef.rat[[l]][i],
                           ci = c(mef.rat.ci[[l]][[1]][i], mef.rat.ci[[l]][[2]][[i]]),
                           accuracy = 1, trans_fun = function(x){x*100}),
               file.path(num.path, paste0("meff_", names(labs)[l], "_", labs[[l]][i], "_", ".tex")))
  }
}


# Main coefficient plots

## Prepare main
plot.df <- rbind(do.call(rbind, lapply(1:2, function(i){
  p <- length(pool.model.ls[[i]]$estimate)
  data.frame(beta = pool.model.ls[[i]]$estimate[p],
             se = stdEr(pool.model.ls[[i]])[p],
             bcilow = pool.se.ls[[i]]$ci_mat[3,p],
             bciup = pool.se.ls[[i]]$ci_mat[4,p],
             variable = names(pool.model.ls[[i]]$estimate)[p],
             model = c("Lagged Dependent Variable Model", "Baseline Model")[i],
             year = "Pooled", stringsAsFactors = F)
})),
do.call(rbind, lapply(1:length(cut.model.ls), function(i){
  p <- length(cut.model.ls[[i]]$estimate)
  data.frame(beta = cut.model.ls[[i]]$estimate[p],
             se = stdEr(cut.model.ls[[i]])[p],
             bcilow = cut.se.ls[[i]]$ci_mat[3,p],
             bciup = cut.se.ls[[i]]$ci_mat[4,p],
             variable = names(cut.model.ls[[i]]$estimate)[p],
             model = "Lagged Dependent Variable Model",
             year = as.character(cut_yrs[i+1]), stringsAsFactors = F)
})),
do.call(rbind, lapply(1:length(cutxsec.model.ls), function(i){
  p <- length(cutxsec.model.ls[[i]]$estimate)
  data.frame(beta = cutxsec.model.ls[[i]]$estimate[p],
             se = stdEr(cutxsec.model.ls[[i]])[p],
             bcilow = cutxsec.se.ls[[i]]$ci_mat[3,p],
             bciup = cutxsec.se.ls[[i]]$ci_mat[4,p],
             variable = names(cutxsec.model.ls[[i]]$estimate)[p],
             model = "Baseline Model",
             year = as.character(cut_yrs[i]), stringsAsFactors = F)
})))

plot.df$year <- factor(plot.df$year, 
                          levels = unique(c("Pooled",unique(sort(plot.df$year)))), 
                          ordered = T)
plot.df$year.num <- ifelse(plot.df$year == "Pooled", 0, as.numeric(plot.df$year))

## Bootstrapped estimates
bs.plot.df <- rbind(do.call(rbind, lapply(1:2, function(i){
  p <- length(pool.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(pool.se.ls[[i]]$beta_boot[,p]),
             variable = names(pool.model.ls[[i]]$estimate)[p],
             model = c("Lagged Dependent Variable Model", "Baseline Model")[i],
             year = "Pooled", stringsAsFactors = F)
})),
do.call(rbind, lapply(1:length(cut.model.ls), function(i){
  p <- length(cut.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(cut.se.ls[[i]]$beta_boot[,p]),
             variable = names(cut.model.ls[[i]]$estimate)[p],
             model = "Lagged Dependent Variable Model",
             year = as.character(cut_yrs[i+1]), stringsAsFactors = F)
})),
do.call(rbind, lapply(1:length(cutxsec.model.ls), function(i){
  p <- length(cutxsec.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(cutxsec.se.ls[[i]]$beta_boot[,p]),
             variable = names(cutxsec.model.ls[[i]]$estimate)[p],
             model = "Baseline Model",
             year = as.character(cut_yrs[i]), stringsAsFactors = F)
})))
  

bs.plot.df <- bs.plot.df[bs.plot.df$variable != "Constant",]
bs.plot.df$variable <- gsub(" ", "\n", bs.plot.df$variable, fixed = T)
bs.plot.df$variable <- factor(bs.plot.df$variable, 
                              levels = unique(bs.plot.df$variable), 
                              ordered = T)
bs.plot.df$year <- factor(bs.plot.df$year, 
                              levels = unique(c("Pooled",unique(sort(bs.plot.df$year)))), 
                              ordered = T)
bs.plot.df$year.num <- ifelse(bs.plot.df$year == "Pooled", 0, as.numeric(bs.plot.df$year))

## Drop 1986 from poled model // no variation!
plot.df <- plot.df[!(plot.df$year == 1986 & 
                       plot.df$model == "Lagged Dependent Variable Model"),]
bs.plot.df <- bs.plot.df[!(bs.plot.df$year == 1986 & 
                             bs.plot.df$model == "Lagged Dependent Variable Model"),]

## Plot
g <- ggplot(plot.df, aes(x = year.num, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(year)), fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_wrap(~ model, nrow = 1) + theme_minimal() +
  scale_x_continuous(breaks = c(1, unique(plot.df$year.num)), 
                     labels = c("", as.character(unique(unique(plot.df$year))))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        panel.spacing = unit(2, "lines"),
        panel.grid.minor.x = element_blank()) +
  xlab("Sample") + ylab("Effect of ethnic boundaries") +
  geom_vline(xintercept = 1, col = "darkgrey", lty = 2)

png(file.path(fig.path, "main_coefficients.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()


# Robustness check: 1886 ethnicity ##########

## Fragments
cut.model.ls <- readRDS(file.path(tem_res.path, "long_model.ls.rds"))
cut.se.ls <- readRDS(file.path(tem_res.path, "long_se.ls.rds"))

cutxsec.model.ls <- readRDS(file.path(tem_res.path, "longxsec_model.ls.rds"))
cutxsec.se.ls <- readRDS(file.path(tem_res.path, "longxsec_se.ls.rds"))


## Prepare main
plot.df <- rbind(do.call(rbind, lapply(3:4, function(i){
  p <- length(pool.model.ls[[i]]$estimate)
  data.frame(beta = pool.model.ls[[i]]$estimate[p],
             se = stdEr(pool.model.ls[[i]])[p],
             bcilow = pool.se.ls[[i]]$ci_mat[3,p],
             bciup = pool.se.ls[[i]]$ci_mat[4,p],
             variable = names(pool.model.ls[[i]]$estimate)[p],
             model = c("Lagged Dependent Variable Model", "Baseline Model")[i-2],
             year = "Pooled", stringsAsFactors = F)
})),
do.call(rbind, lapply(1:length(cut.model.ls), function(i){
  p <- length(cut.model.ls[[i]]$estimate)
  data.frame(beta = cut.model.ls[[i]]$estimate[p],
             se = stdEr(cut.model.ls[[i]])[p],
             bcilow = cut.se.ls[[i]]$ci_mat[3,p],
             bciup = cut.se.ls[[i]]$ci_mat[4,p],
             variable = names(cut.model.ls[[i]]$estimate)[p],
             model = "Lagged Dependent Variable Model",
             year = as.character(cut_yrs[i+1]), stringsAsFactors = F)
})),
do.call(rbind, lapply(1:length(cutxsec.model.ls), function(i){
  p <- length(cutxsec.model.ls[[i]]$estimate)
  data.frame(beta = cutxsec.model.ls[[i]]$estimate[p],
             se = stdEr(cutxsec.model.ls[[i]])[p],
             bcilow = cutxsec.se.ls[[i]]$ci_mat[3,p],
             bciup = cutxsec.se.ls[[i]]$ci_mat[4,p],
             variable = names(cutxsec.model.ls[[i]]$estimate)[p],
             model = "Baseline Model",
             year = as.character(cut_yrs[i]), stringsAsFactors = F)
})))

plot.df$year <- factor(plot.df$year, 
                       levels = unique(c("Pooled",unique(sort(plot.df$year)))), 
                       ordered = T)
plot.df$year.num <- ifelse(plot.df$year == "Pooled", 0, as.numeric(plot.df$year))

## Bootstrapped estimates
bs.plot.df <- rbind(do.call(rbind, lapply(3:4, function(i){
  p <- length(pool.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(pool.se.ls[[i]]$beta_boot[,p]),
             variable = names(pool.model.ls[[i]]$estimate)[p],
             model = c("Lagged Dependent Variable Model", "Baseline Model")[i-2],
             year = "Pooled", stringsAsFactors = F)
})),
do.call(rbind, lapply(1:length(cut.model.ls), function(i){
  p <- length(cut.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(cut.se.ls[[i]]$beta_boot[,p]),
             variable = names(cut.model.ls[[i]]$estimate)[p],
             model = "Lagged Dependent Variable Model",
             year = as.character(cut_yrs[i+1]), stringsAsFactors = F)
})),
do.call(rbind, lapply(1:length(cutxsec.model.ls), function(i){
  p <- length(cutxsec.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(cutxsec.se.ls[[i]]$beta_boot[,p]),
             variable = names(cutxsec.model.ls[[i]]$estimate)[p],
             model = "Baseline Model",
             year = as.character(cut_yrs[i]), stringsAsFactors = F)
})))


bs.plot.df <- bs.plot.df[bs.plot.df$variable != "Constant",]
bs.plot.df$variable <- gsub(" ", "\n", bs.plot.df$variable, fixed = T)
bs.plot.df$variable <- factor(bs.plot.df$variable, 
                              levels = unique(bs.plot.df$variable), 
                              ordered = T)
bs.plot.df$year <- factor(bs.plot.df$year, 
                          levels = unique(c("Pooled",unique(sort(bs.plot.df$year)))), 
                          ordered = T)
bs.plot.df$year.num <- ifelse(bs.plot.df$year == "Pooled", 0, as.numeric(bs.plot.df$year))


## Drop 1986 from poled model // no variation!
plot.df <- plot.df[!(plot.df$year == 1986 & 
                       plot.df$model == "Lagged Dependent Variable Model"),]
bs.plot.df <- bs.plot.df[!(bs.plot.df$year == 1986 & 
                             bs.plot.df$model == "Lagged Dependent Variable Model"),]

## Plot
g <- ggplot(plot.df, aes(x = year.num, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(year)), fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_wrap(~ model, nrow = 1) + theme_minimal() +
  scale_x_continuous(breaks = c(1, unique(plot.df$year.num)), 
                     labels = c("", as.character(unique(unique(plot.df$year))))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        panel.spacing = unit(2, "lines"),
        panel.grid.minor.x = element_blank()) +
  xlab("Sample") + ylab("Effect of 1886 ethnic boundaries") +
  geom_vline(xintercept = 1, col = "darkgrey", lty = 2)

png(file.path(fig.path, "eth1886_coefficients.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()


# ROBUSTNESS CHECK: CONTROL VARIABLES ###


# Stub
stub <- "controls"

# Load Models
controls.model.ls <- c(pool.model.ls[c(2,1)], 
                       readRDS(file.path(tem_res.path, "controls_model.ls.rds")),
                       readRDS(file.path(tem_res.path, "adm1886_model.ls.rds")))
controls.se.ls <- c(pool.se.ls[c(2,1)], 
                    readRDS(file.path(tem_res.path, "controls_se.ls.rds")), 
                    readRDS(file.path(tem_res.path, "adm1886_se.ls.rds")))


# Model vector
model.vec <- c(1,2, 4,3,6,5, 8,7)

# Var labs
var.labs <- c("Constant","Edge length", "Largest river",  "Largest watershed","Elevation mean",
              "Ethnic boundary$_t$","Deep lag","State border$_{t-1}$" ,"Ethnic boundary$_{t-1}$", 
              "Largest river$^2$","Any river",
              "$\\Delta$ Longitude", "$\\Delta$ Latitude", "Pop. dens. 1880",
              "Cum. $\\Delta$ altitude", "Std. dev. altitude",
              "Adm. bord.$_{1800}$", "Adm. bord.$_{1900}$", "State border$_{pre-1886}$"
              )

# Output
pspm2table(lapply(model.vec, function(i){controls.model.ls[[i]]}), type = "latex",
            file = file.path(tab.path, paste0(stub,".tex")),
            label = stub,
            caption = "Determinants of state borders in Europe, 1886--2011: Varying control variables",
            stars = c(.01, .05, .1), 
            custom.header = list("Main results" = 1:2, "No controls" = 3:4,
                              "Add. controls" = 5:6, "Admin. borders" = 7:8),
            custom.coef.names = var.labs,
            custom.model.names = paste0(1:8, ": ", rep(c("Baseline","LDV"), 4)),
            dcolumn = T, use.packages = F,
            add.stats = c(`No. of periods` = "N_instances",
                          `No. of vertices` =  "N",
                          `No. of edges` =  "N_edges",
                          `No. of states` = "N_groups"),
            custom.gof.rows = NULL, 
            include.nobs = F,
            bootci = lapply(model.vec, function(i){controls.se.ls[[i]]$ci_mat}),
            include.loglik = F,
            reorder.coef = c(1, 6, 9, 19, 8, 7, 2, 5, 4, 3, 10:18),
            custom.note = latex.notes(.8)
)
readLines(file.path(tab.path, paste0(stub,".tex")))


# ROBUSTNESS CHECK: FULL INTERACTION MODELS ###

# Stub
stub <- "fullint"

# Load Models
fullint.model.ls <- readRDS(file.path(tem_res.path, "fullint_model.ls.rds"))
fullint.se.ls <- readRDS(file.path(tem_res.path, "fullint_se.ls.rds"))



# # Var labs
var.labs <- c("Constant",
              "State border$_{t-1}$ (SB)",
              "Ethnic boundary$_{t-1}$ (EB)",
              "EB$_{t-1}$ $\\times$ SB$_{t-1}$"
             )

# Notes
latex.notes <- function(width = .95){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textit{Notes:} 
  Each period $t$ has a length of 25 years. 
  95\\% confidence intervals from parametric bootstrap in parenthesis. 
  $^*$ Statistically significant at the 95\\% level.
           }  ")
}

# Output
pspm2table(fullint.model.ls, type = "latex",
            file = file.path(tab.path, paste0(stub,".tex")),
            label = stub,
            caption = "Border emergence, stability, and duration",
            stars = c(.01, .05, .1), 
            custom.coef.names = var.labs,
            omit.coef = "length|water|river|elev|duration|abram",
            # custom.header = list("Baseline" = c(1),"Full interaction models" = 2:4),
            custom.model.names = paste0("(",1:5, ")"),
            dcolumn = T, use.packages = F,
            add.stats = c(`No. of periods` = "N_instances",
                          `No. of vertices` =  "N",
                          `No. of edges` =  "N_edges",
                          `No. of states` = "N_groups"),
            include.nobs = F,
            bootci = lapply(fullint.se.ls, function(s){s$ci_mat}),
            include.loglik = F,
            custom.note = latex.notes(.8),
            custom.gof.rows = list("Controls" = rep("yes", 5),
                                   "SB$_{t-1}$ $\\times$ controls" = c("yes", "no", "yes", "no", "yes"),
                                   "Cubic duration"= c("no", "no", "no", "yes", "yes") , 
                                   "SB$_{t-1}$ $\\times$ cub. dur." = c("no", "no", "no", "yes", "yes")) ,
            reorder.coef = c(1,2, 3,4 )
)
readLines(file.path(tab.path, "fullint.tex"))

# SE of sum of ethnic boundary coefficient
# CI overlaps w/ 0 in 2 but not 3

## Old borders
for(i in c(2:3, 5)){
  writeLines(effect_text(coef = sum(fullint.model.ls[[i]]$estimate[grepl("cuteth_lag", names(fullint.model.ls[[i]]$estimate))]),
                         ci = quantile(rowSums(fullint.se.ls[[i]]$beta_boot[,grepl("cuteth_lag", names(fullint.model.ls[[i]]$estimate))]),
                                       prob = c(.025, .975)),
                         accuracy = 2),
             file.path(num.path, paste0("fullint_", i,"_old.tex")))
}

## New borders
for(i in c(2:3, 5)){
  writeLines(effect_text(coef = fullint.model.ls[[i]]$estimate["cuteth_lag"],
                         ci = quantile(fullint.se.ls[[i]]$beta_boot[,names(fullint.model.ls[[i]]$estimate) == "cuteth_lag"],
                                       prob = c(.025, .975)),
                         accuracy = 2),
             file.path(num.path, paste0("fullint_", i,"_new.tex")))
}

## Difference
for(i in c(2:3, 5)){
  writeLines(effect_text(coef = fullint.model.ls[[i]]$estimate[length(fullint.model.ls[[i]]$estimate)],
                         ci = quantile(fullint.se.ls[[i]]$beta_boot[,length(fullint.model.ls[[i]]$estimate)],
                                       prob = c(.025, .975)),
                         accuracy = 2),
             file.path(num.path, paste0("fullint_", i,"_diff.tex")))
}


# ROBUSTNESS CHECK: PERIODICITY ####

# Load models
period.se.ls <- readRDS(file.path(tem_res.path, "periodcheck_se.ls.rds"))
period.model.ls <- readRDS(file.path(tem_res.path, "periodcheck_model.ls.rds"))


## Prepare main
plot.df <- do.call(rbind, lapply(seq_along(period.model.ls), function(i){
  p <- length(period.model.ls[[i]]$estimate)
  data.frame(beta = period.model.ls[[i]]$estimate[p],
             se = stdEr(period.model.ls[[i]])[p],
             bcilow = period.se.ls[[i]]$ci_mat[3,p],
             bciup = period.se.ls[[i]]$ci_mat[4,p],
             variable = names(period.model.ls[[i]]$estimate)[p],
             model = i, N = max(period.model.ls[[i]]$N),
             NE = max(period.model.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F)
}))
plot.df$periodicity <- c(5, 15, 25, 35, 45, 55, 65)
plot.df$model <- rep(c("Lagged Dependent Variable Model", "Baseline Model"), 
                     each = length(unique(plot.df$periodicity)))



## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(seq_along(period.model.ls), function(i){
  p <- length(period.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(period.se.ls[[i]]$beta_boot[,p]),
             variable = names(period.model.ls[[i]]$estimate)[p],
             model = rep(c("Lagged Dependent Variable Model", "Baseline Model"), each = 7)[i],
             periodicity = rep(c(5, 15, 25, 35, 45, 55, 65), 2)[i],
             stringsAsFactors = F)
}))


## Plot
g <- ggplot(plot.df, aes(x = periodicity, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(periodicity)), fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_wrap(~ model, nrow = 1) + theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_x_continuous(breaks = c(5, 15, 25, 35, 45, 55, 65)) +
  theme( # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank()) +
  xlab("Periodicity (in years)") + ylab("Effect of ethnic\nboundaries") +
  # scale_y_continuous(sec.axis = sec_axis(~ . * 10000)) +
  # geom_line(aes(y = N / 10000)) +
  NULL

png(file.path(fig.path, "periodicity_coefficients.png"), width = 6, height = 1.8, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()



# ROBUSTNESS CHECK: SHIFT NETWORK ###

# Load models
shift.model.ls <- readRDS(file.path(tem_res.path, "shiftcheck_model.ls.rds"))

# Prepare plot
plot.df <- do.call(rbind, lapply(seq_along(shift.model.ls), function(i){
  do.call(rbind, lapply(seq_along(shift.model.ls[[i]]), function(j){
    data.frame(iter = i, 
               model = j,
               variable = names(shift.model.ls[[i]][[j]]$estimate),
               do.call(cbind, shift.model.ls[[i]][[j]]),
               stringsAsFactors = F)
  }))
}))
plot.df$model <- c("Lagged Dependent Variable Model", "Baseline Model")[plot.df$model]


# Mean
mean.df <- cbind(do.call(rbind, apply(unique(plot.df[, c("model", "variable"), drop = F]), 1, function(x){
  dens_at(plot.df[plot.df$model == x["model"] & 
                    plot.df$variable == x["variable"], "estimate"],
          fun = mean)
})), unique(plot.df[,c("model", "variable")]))

# Main results
pool.df <- data.frame(estimate = sapply(1:2, function(i){
  pool.model.ls[[i]]$estimate[length(pool.model.ls[[i]]$estimate)]
  }),
  lb =  sapply(1:2, function(i){
    pool.se.ls[[i]]$ci_mat[3, ncol(pool.se.ls[[i]]$ci_mat)]
  }),
  ub =  sapply(1:2, function(i){
    pool.se.ls[[i]]$ci_mat[4, ncol(pool.se.ls[[i]]$ci_mat)]
  }),
  model = c("Lagged Dependent Variable Model", "Baseline Model")
  )


# Get percentiles
writeLines(as.character(signif(mean(plot.df$estimate[grepl("Lagged", plot.df$model) &
                                                       plot.df$variable %in% c("cuteth_lag", "cuteth")] < 
                                      pool.df$estimate[grepl("Lagged", pool.df$model)]) * 100, 2)),
           file.path(num.path, "shift_percentile_ldv.tex"))
writeLines(as.character(signif(mean(plot.df$estimate[grepl("Baseline", plot.df$model) &
                                                       plot.df$variable %in% c("cuteth_lag", "cuteth")] < 
                                      pool.df$estimate[grepl("Baseline", pool.df$model)]) * 100, 2)),
           file.path(num.path, "shift_percentile_base.tex"))

# Plot
g <- ggplot(plot.df[plot.df$variable %in% c("cuteth_lag", "cuteth"),],
            aes(x = estimate)) +
  geom_line(stat = "density") +
  geom_segment(data = mean.df[mean.df$variable %in% c("cuteth_lag", "cuteth"),], 
               aes(x = mean, xend = mean, y = 0, yend = est.dens)) +
  theme_minimal() +
  facet_wrap(~ model) +
  geom_point(data = pool.df, aes(y = 4), 
             col = "red") +
  geom_segment(data = pool.df, 
               aes(x = lb, xend = ub, y = 4, yend = 4), 
               col = "red") +
  ylab("Density") + xlab("Point estimate") +
  xlim(c(0,1.5)) +
  theme(panel.spacing = unit(2, "lines")) +
  geom_vline(xintercept = 0, col = "darkgrey", lty = 2)

## Save
png(file.path(fig.path, "robcheck_shiftnetwork.png"), 
    width = 6, height = 1.5, res = 400, units = "in")
par(mar = c(0,0,0,0))
plot(g)
dev.off()


# ROBUSTNESS CHECK: VARY RESOLUTION ####

# Load models
resol.se.ls <- readRDS(file.path(tem_res.path, "rescheck_se.ls.rds"))
resol.model.ls <- readRDS(file.path(tem_res.path, "rescheck_model.ls.rds"))


## Prepare main
plot.df <- do.call(rbind, lapply(seq_along(resol.model.ls), function(i){
  p <- length(resol.model.ls[[i]]$estimate)
  data.frame(beta = resol.model.ls[[i]]$estimate[p],
             se = stdEr(resol.model.ls[[i]])[p],
             bcilow = resol.se.ls[[i]]$ci_mat[3,p],
             bciup = resol.se.ls[[i]]$ci_mat[4,p],
             variable = names(resol.model.ls[[i]]$estimate)[p],
             model = i, N = max(resol.model.ls[[i]]$N),
             NE = max(resol.model.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F)
}))
plot.df$resolution <- rep(seq(50, 200, by = 25), 2)
plot.df$model <- rep(c("Lagged Dependent Variable Model", "Baseline Model"), each = 7)

## Numbers
writeLines(as.character(plot.df$N[plot.df$resolution == 50][1]),
           file.path(num.path, "robresolution_Nhigh.tex"))
writeLines(as.character(plot.df$N[plot.df$resolution == 100][1]),
           file.path(num.path, "robresolution_Nbase.tex"))
writeLines(as.character(plot.df$NE[plot.df$resolution == 100][1]),
           file.path(num.path, "robresolution_Ebase.tex"))
writeLines(as.character(plot.df$N[plot.df$resolution == 200][1]),
           file.path(num.path, "robresolution_Nlow.tex"))

## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(seq_along(resol.model.ls), function(i){
  p <- length(resol.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(resol.se.ls[[i]]$beta_boot[,p]),
             variable = names(resol.model.ls[[i]]$estimate)[p],
             model = rep(c("Lagged Dependent Variable Model", "Baseline Model"), each = 7)[i],
             resolution = rep(seq(50, 200, by = 25), 2)[i],
             stringsAsFactors = F)
}))


## Plot
g <- ggplot(plot.df, aes(x = resolution, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(resolution)), fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_wrap(~ model, nrow = 1) + theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank()) +
  xlab("Resolution (edge length in km)") + ylab("Effect of ethnic\nboundaries") +
  NULL

png(file.path(fig.path, "resolution_coefficients.png"), width = 6, height = 1.8, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()



# ROBUSTNESS CHECK: VARY STRUCTURE ####

# Load models
struct.se.ls <- readRDS(file.path(tem_res.path, "structcheck_se.ls.rds"))
struct.model.ls <- readRDS(file.path(tem_res.path, "structcheck_model.ls.rds"))

struc.labs <- c("hex.", "quad.", "triang.", "rand.")
## Prepare main
plot.df <- do.call(rbind, lapply(seq_along(struct.model.ls), function(i){
  p <- length(struct.model.ls[[i]]$estimate)
  data.frame(beta = struct.model.ls[[i]]$estimate[p],
             se = stdEr(struct.model.ls[[i]])[p],
             bcilow = struct.se.ls[[i]]$ci_mat[3,p],
             bciup = struct.se.ls[[i]]$ci_mat[4,p],
             variable = names(struct.model.ls[[i]]$estimate)[p],
             model = i, N = max(struct.model.ls[[i]]$N),
             NE = max(struct.model.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F)
}))
plot.df$structure <- rep(struc.labs, 2)
plot.df$model <- rep(c("Lagged Dependent Variable Model", "Baseline Model"), each = 4)


## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(seq_along(struct.model.ls), function(i){
  p <- length(struct.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(struct.se.ls[[i]]$beta_boot[,p]),
             variable = names(struct.model.ls[[i]]$estimate)[p],
             model = rep(c("Lagged Dependent Variable Model", "Baseline Model"), each = 4)[i],
             structure = rep(struc.labs, 2)[i],
             stringsAsFactors = F)
}))
bs.plot.df$structure <- factor(bs.plot.df$structure,
                               levels = unique(plot.df$structure), 
                               ordered = T)
plot.df$structure <- factor(plot.df$structure,
                            levels = unique(plot.df$structure), 
                            ordered = T)

## Plot
g <- ggplot(plot.df, aes(x = structure, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(structure)),
              fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_wrap(~ model, nrow = 1) + theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        panel.spacing = unit(2, "lines"),
        panel.grid.minor.x = element_blank()) +
  xlab("Network structure") + ylab("Effect of ethnic\nboundaries") +
  NULL

png(file.path(fig.path, "structure_coefficients.png"), width = 6, height = 1.8, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()

# Plot graphs
for(g in c("quad4", "triangular", "random")){
  # Load Graph
  this.graph.path <- file.path("data/analysis_data",
                               paste0("graph_", g, ".rds"))
  this.g <- readRDS(this.graph.path)
  
  # Plot
  png(file.path(fig.path, paste0("map_",g,".png")), 
      width = 6, height = 3, res = 400, units = "in")
  par(mar = c(0,0,2,0))
  plot(plot.europe.shp, border = "lightgrey",
       lwd = .4, asp = 1)
  plot_spatial_graph(this.g, vertex.size = 0.025, 
                     edge.color = "grey", 
                     edge.width = .75,
                     add = T)
  dev.off()
}

# LINGUISTIC DISTANCE ############


# Load models
lingdse.ls <- readRDS(file.path(tem_res.path, "lingdist_se.ls.rds"))
lingdmodel.ls <- readRDS(file.path(tem_res.path, "lingdist_model.ls.rds"))


## Prepare main

types <- c("Linear", "Factors", "1886\nLinear", "1886\nFactors")
types <- factor(types, levels = types, ordered = T)
var.labs <- c("", "(0,.25]", "(.25,.5]", "(.5,1]", "(0,.25]", "(.25,.5]", "(.5,1]",
                  "", "(0,.25]", "(.25,.5]", "(.5,1]")
plot.df <- do.call(rbind, lapply(seq_along(lingdmodel.ls), function(i){
  p <- seq_along(lingdmodel.ls[[i]]$estimate)[!grepl("cshp|river|watershed|elev|abram|Constant|length", 
                                                     names(lingdmodel.ls[[i]]$estimate))]
  data.frame(beta = lingdmodel.ls[[i]]$estimate[p],
             se = stdEr(lingdmodel.ls[[i]])[p],
             bcilow = lingdse.ls[[i]]$ci_mat[3,p],
             bciup = lingdse.ls[[i]]$ci_mat[4,p],
             variable = names(lingdmodel.ls[[i]]$estimate)[p],
             type = rep(types, each = 2)[i], 
             N = max(lingdmodel.ls[[i]]$N),
             NE = max(lingdmodel.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F,
             model = rep(c("Lagged Dependent\nVariable Model", "Baseline Model"), length(lingdmodel.ls)/2)[i])
}))

## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(seq_along(lingdmodel.ls), function(i){
  p <- seq_along(lingdmodel.ls[[i]]$estimate)[!grepl("cshp|river|watershed|elev|abram|Constant|length", 
                                                     names(lingdmodel.ls[[i]]$estimate))]
  data.frame(beta = as.vector(lingdse.ls[[i]]$beta_boot[,p]),
             type = rep(types, each = 2)[i],
             variable = rep(names(lingdmodel.ls[[i]]$estimate)[p], each = nrow(lingdse.ls[[i]]$beta_boot)),
             model = rep(c("Lagged Dependent\nVariable Model", "Baseline Model"), length(lingdmodel.ls)/2)[i],
             stringsAsFactors = F)
}))
plot.df$variable <- factor(plot.df$variable, 
                           levels = unique(plot.df$variable), labels = var.labs, ordered = T)
bs.plot.df$variable <- factor(bs.plot.df$variable, 
                           levels = unique(bs.plot.df$variable), labels = var.labs, ordered = T)


## Plot
g <- ggplot(plot.df, aes(x = variable, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(variable)), fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_grid(model ~ type,
             scales = "free", space = "free_x") + 
  theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme( 
    panel.spacing = unit(1.5, "lines"),
    panel.grid.minor.x = element_blank()) +
  xlab(NULL) + ylab("Effect of edge-level linguistic distance") +
  NULL

# Save
png(file.path(fig.path, "plot_lingdist.png"), width = 6, height = 3.5, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()




# NATURAL GEOGRAPHY VARIABLES ####

# Load models
resol.se.ls <- readRDS(file.path(tem_res.path, "rescheck_se.ls.rds"))
resol.model.ls <- readRDS(file.path(tem_res.path, "rescheck_model.ls.rds"))

## Prepare main
plot.df <- do.call(rbind, lapply(seq_along(resol.model.ls), function(i){
  p <- seq_along(resol.model.ls[[i]]$estimate)[!grepl("\\_lag|cuteth|abram|Constant|length", 
                                                      names(resol.model.ls[[i]]$estimate))]
  data.frame(beta = resol.model.ls[[i]]$estimate[p],
             se = stdEr(resol.model.ls[[i]])[p],
             bcilow = resol.se.ls[[i]]$ci_mat[3,p],
             bciup = resol.se.ls[[i]]$ci_mat[4,p],
             variable = names(resol.model.ls[[i]]$estimate)[p],
             N = max(resol.model.ls[[i]]$N),
             NE = max(resol.model.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F,
             resolution = rep(seq(50, 200, by = 25), 2)[i],
             model = rep(c("Lagged Dependent Variable Model", "Baseline Model"), each = 7)[i])
}))

## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(seq_along(resol.model.ls), function(i){
  p <- seq_along(resol.model.ls[[i]]$estimate)[!grepl("\\_lag|cuteth|abram|Constant|length", 
                                                      names(resol.model.ls[[i]]$estimate))]
  data.frame(beta = as.vector(resol.se.ls[[i]]$beta_boot[,p]),
             variable = rep(names(resol.model.ls[[i]]$estimate)[p], each = nrow(resol.se.ls[[i]]$beta_boot)),
             model = rep(c("Lagged Dependent Variable Model", "Baseline Model"), each = 7)[i],
             resolution = rep(seq(50, 200, by = 25), 2)[i],
             stringsAsFactors = F)
}))


## Plot
fac.labs <- c("Elevation", "River\nsize", "Watershed\nsize")
names(fac.labs) <- sort(unique(plot.df$variable))
g <- ggplot(plot.df, aes(x = resolution, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(resolution)), fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_grid(variable ~ model, labeller = labeller(variable = fac.labs),
             scales = "free_y") + 
  theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme( panel.spacing = unit(1, "lines"),
    panel.grid.minor.x = element_blank()) +
  xlab("Resolution (edge length in km)") + ylab("Effect") +
  NULL

# Save
png(file.path(fig.path, "resolution_coefficients_natvars.png"), width = 6, height = 3.5, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()





# AUSTRIA HUNGARY #######


# Load models
ah.se.ls <- readRDS(file.path(tem_res.path, "austhung_se.ls.rds"))
ah.model.ls <- readRDS(file.path(tem_res.path, "austhung_model.ls.rds"))


## Prepare main
plot.df <- do.call(rbind, lapply(seq_along(ah.model.ls), function(i){
  p <- length(ah.model.ls[[i]]$estimate)
  data.frame(beta = ah.model.ls[[i]]$estimate[p],
             se = stdEr(ah.model.ls[[i]])[p],
             bcilow = ah.se.ls[[i]]$ci_mat[3,p],
             bciup = ah.se.ls[[i]]$ci_mat[4,p],
             variable = names(ah.model.ls[[i]]$estimate)[p],
             model = i, N = max(ah.model.ls[[i]]$N),
             NE = max(ah.model.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F)
}))

plot.df$model <- rep(c("Lagged Dependent Variable Model", "Baseline Model"), 
                     length(unique(plot.df$variable)) + 1)
plot.df$variable <- factor(plot.df$variable, 
                           levels = c("cuteth1911" ,  "largest.diff.1900",   
                                      "largest.diff.1910",   "hell.dist.1900",    "hell.dist.1910"),
                           ordered = T,
                           labels = c( "Ethnic\nboundary", 
                                       paste0(c(1900, 1910), " Plurality\ndifference"), 
                                       paste0(c(1900, 1910), " Hellinger\ndistance")))


## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(seq_along(ah.model.ls), function(i){
  p <- length(ah.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(ah.se.ls[[i]]$beta_boot[,p]),
             variable = names(ah.model.ls[[i]]$estimate)[p],
             model = rep(c("Lagged Dependent Variable Model", "Baseline Model"), 
                         length(unique(plot.df$variable))+1)[i],
             stringsAsFactors = F)
}))
bs.plot.df$variable <- factor(bs.plot.df$variable, 
                           levels = c("cuteth1911" ,  "largest.diff.1900",   
                                      "largest.diff.1910",   "hell.dist.1900",    "hell.dist.1910"),
                           ordered = T,
                           labels = c( "Ethnic\nboundary", 
                                       paste0(c(1900, 1910), " Plurality\ndifference"), 
                                       paste0(c(1900, 1910), " Hellinger\ndistance")))

## Plot
g <- ggplot(plot.df, aes(x = variable, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(variable)), 
              fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_wrap(~ model, nrow = 1) + theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank()) +
  xlab(NULL) + ylab("Effect of ethnic\nboundary measure") +
  NULL

png(file.path(fig.path, "austhung_coefficients.png"), width = 6, height = 2, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()



# ALL ETHNIC MAPS ####

# Load models
bymap.se.ls <- readRDS(file.path(tem_res.path, "bymap_se.ls.rds"))
bymap.model.ls <- readRDS(file.path(tem_res.path, "bymap_model.ls.rds"))


## Prepare main
plot.df <- do.call(rbind, lapply(which(!sapply(bymap.model.ls, is.null)), function(i){
  p <- length(bymap.model.ls[[i]]$estimate)
  data.frame(beta = bymap.model.ls[[i]]$estimate[p],
             se = stdEr(bymap.model.ls[[i]])[p],
             bcilow = bymap.se.ls[[i]]$ci_mat[3,p],
             bciup = bymap.se.ls[[i]]$ci_mat[4,p],
             variable = names(bymap.model.ls[[i]]$estimate)[p],
             model = i, N = max(bymap.model.ls[[i]]$N),
             name = names(bymap.model.ls)[i],
             NE = max(bymap.model.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F)
}))
plot.df$model <- rep(c("Lagged Dependent Variable Model", "Baseline Model"), 
                     each = length(bymap.model.ls)/2)[!sapply(bymap.model.ls, is.null)]


## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(which(!sapply(bymap.model.ls, is.null)), function(i){
  p <- length(bymap.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(bymap.se.ls[[i]]$beta_boot[,p]),
             variable = names(bymap.model.ls[[i]]$estimate)[p],
             N = max(bymap.model.ls[[i]]$N),
             model = rep(c("Lagged Dependent Variable Model", "Baseline Model"), 
                         each = length(bymap.model.ls)/2)[
                           !sapply(bymap.model.ls, is.null)][i],
             resolution = rep(seq(50, 200, by = 25), 2)[i],
             stringsAsFactors = F)
}))


## Order
plot.df <- do.call(rbind, lapply(unique(plot.df$model), function(m){
  this.df <- plot.df[plot.df$model == m,]
  this.df <- this.df[this.df$beta != 0,]
  this.df$rank <- rank(this.df$beta)
  this.df
}))
bs.plot.df <- join(bs.plot.df, 
                   plot.df[, c("variable", "model", "rank")],
                   by = c("model", "variable"), match = "all", type = "inner")

## Across maps
map.sum <- data.frame(do.call(rbind, lapply(c("Lagged Dependent Variable Model", "Baseline Model"), function(m){
  this.df <- plot.df[plot.df$model == m, ]
  this.df <- this.df[order(this.df$beta),]
  this.df$Ncum <- cumsum(this.df$N)
  data.frame(model = m, 
    bcilow = (this.df$beta[which(this.df$Ncum > sum(this.df$N)*.025)[1]-1] +
                this.df$beta[which(this.df$Ncum > sum(this.df$N)*.025)[1]]) / 2, 
    bciup = this.df$beta[this.df$Ncum > sum(this.df$N)*.975][1], 
    beta = sum(this.df$beta * (this.df$N / sum(this.df$N))))
})))

## Plot
g <- ggplot(plot.df, aes(x = rank, y = beta, alpha = N)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = plot.df, aes(x = -10, weight = N, alpha = NULL), width = 10,
              fill = "transparent", color = "darkgrey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  geom_point(data = map.sum, aes(x = -10, alpha = NULL), size = 3) +
  geom_errorbar(data = map.sum, 
                aes(ymin = bcilow, ymax = bciup, x = -10, alpha = NULL), 
                width = 0, size = 1) +
  facet_wrap(~ model, nrow = 1) + theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_alpha_continuous() +
  labs(alpha = "Map\nsize\n(vertices)") +
  theme(panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  xlab(NULL) + 
  ylab("Effect of ethnic\nboundaries") +
  NULL

png(file.path(fig.path, "bymap_coefficients.png"), width = 6, height = 2.5, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()



# CONTINENT ANALYSIS ####

# Load models
continents.se.ls <- readRDS(file.path(tem_res.path, "continents_se.ls.rds"))
continents.model.ls <- readRDS(file.path(tem_res.path, "continents_model.ls.rds"))


## Names
cont.vec <- c("eu","as","af","na","sa")
cont.long.vec <- c("Europe","Asia","Africa","North America","South America")

## Prepare main
plot.df <- do.call(rbind, lapply(seq_along(continents.model.ls), function(i){
  p <- length(continents.model.ls[[i]]$estimate)
  data.frame(beta = continents.model.ls[[i]]$estimate[p],
             se = stdEr(continents.model.ls[[i]])[p],
             bcilow = continents.se.ls[[i]]$ci_mat[3,p],
             bciup = continents.se.ls[[i]]$ci_mat[4,p],
             variable = names(continents.model.ls[[i]]$estimate)[p],
             model.id = i, N = max(continents.model.ls[[i]]$N),
             NE = max(continents.model.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F)
}))
plot.df$continent <- rep(cont.long.vec, 2)
plot.df$model <- rep(c("Lag. Dep.\nVariable", "Baseline"), 
                     each = length(cont.long.vec))



## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(seq_along(continents.model.ls), function(i){
  p <- length(continents.model.ls[[i]]$estimate)
  data.frame(beta = as.vector(continents.se.ls[[i]]$beta_boot[,p]),
             variable = names(continents.model.ls[[i]]$estimate)[p],
             model.id = i,
             model = rep(c("Lag. Dep.\nVariable", "Baseline"), 
                         each = length(cont.long.vec))[i],
             continent = rep(cont.long.vec, 2)[i],
             stringsAsFactors = F)
}))


## Drop LDV Americas: No change
plot.df <- plot.df[!plot.df$model.id %in% c(4,5),]
bs.plot.df <- bs.plot.df[!bs.plot.df$model.id %in% c(4,5),]

## Plot
g <- ggplot(plot.df, aes(x = model, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = factor(model)), fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_wrap(~ continent, nrow = 1) + theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme( # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
    panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  xlab(NULL) + ylab("Effect of ethnic boundaries") +
  NULL

png(file.path(fig.path, "continent_coefficients.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()


# POPULATION INTERACTION MODELS ###

# Stub
stub <- "popint"

# Load Models
popint.model.ls <- readRDS(file.path(tem_res.path, "popint_model.ls.rds"))
popint.se.ls <- readRDS(file.path(tem_res.path, "popint_se.ls.rds"))

# Marginal effects plot

## Models
these.m <- c(World = 4, Africa = 3, Asia = 2, Europe = 1)
popvals <- unlist(sapply(popint.model.ls[[4]]$pspm_ls, function(s){
  c(min(s$Z$pop1880l[s$A == 1], na.rm = T), 
    max(s$Z$pop1880l[s$A == 1], na.rm = T))
}))
popvals <- seq(min(popvals[1,]), max(popvals[2,]), length.out = 10)


## Prepare main
plot.df <- do.call(rbind, lapply(names(these.m), function(n){
  i = these.m[n]
  p.eth <- which(names(popint.model.ls[[i]]$estimate) == "mean_greg")
  p.int <- which(names(popint.model.ls[[i]]$estimate) == "I(pop1880l * mean_greg)")
  
  data.frame(beta = popint.model.ls[[i]]$estimate[p.eth] + 
               popint.model.ls[[i]]$estimate[p.int] * popvals,
             continent = n,
             pop = popvals,
             model.id = i, 
             N = sum(popint.model.ls[[i]]$N),
             NE = sum(popint.model.ls[[i]]$N_edges), 
             year = "Pooled", stringsAsFactors = F)
}))


## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(names(these.m), function(n){
  i = these.m[n]
  p.eth <- which(names(popint.model.ls[[i]]$estimate) == "mean_greg")
  p.int <- which(names(popint.model.ls[[i]]$estimate) == "I(pop1880l * mean_greg)")
  
  d <- data.frame(beta = rep(as.vector(popint.se.ls[[i]]$beta_boot[,p.eth]), each = length(popvals)) + 
                          rep(as.vector(popint.se.ls[[i]]$beta_boot[,p.int]), each = length(popvals)) *
                                rep(popvals, nrow(popint.se.ls[[i]]$beta_boot)),
             pop = rep(popvals, nrow(popint.se.ls[[i]]$beta_boot)),
             continent = n,
             model.id = i, stringsAsFactors = F)
  d <- join(aggregate.data.frame(list(lb = d$beta),
                                 d[, colnames(d) != "beta"],
                                 FUN = quantile, prob = .025),
            aggregate.data.frame(list(ub = d$beta),
                                 d[, colnames(d) != "beta"],
                                 FUN = quantile, prob = .975),
            by = colnames(d)[colnames(d) != "beta"], match = "first", type = "left")
  d
}))

## Distributions
dist.df <- do.call(rbind, lapply(names(these.m), function(n){
  i = these.m[n]
  set.seed(i)
  d <- data.frame(continent = n ,
             pop = unlist(sapply(popint.model.ls[[i]]$pspm_ls, function(s){
    na.omit(as.vector(s$Z$pop1880l[s$A == 1]))
  })))
  d
}))

bs.plot.df$continent <- factor(bs.plot.df$continent, 
                               levels = unique(bs.plot.df$continent),
                               ordered = T)
plot.df$continent <- factor(plot.df$continent, 
                            levels = unique(plot.df$continent),
                            ordered = T)
dist.df$continent <- factor(dist.df$continent, 
                            levels = unique(dist.df$continent),
                            ordered = T)

## Plot
g <- ggplot(plot.df, aes(x = pop, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_ribbon(data = bs.plot.df, 
            aes(ymin=lb,ymax=ub, y = NULL), 
            color = "grey", fill = "grey", alpha = .3, lty = 3) +
  geom_line() +
  geom_density(data = dist.df, aes(y = ..scaled.. - 2),  col = "darkgrey") +
  facet_wrap(~ continent, nrow = 1) + theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_x_continuous(breaks = log(1 + c(0, 10, 100, 1000, 10000, 100000)),
                     labels = c(0, 10, 100, 1000, 10000, 100000)) +
  theme(panel.spacing = unit(2, "lines"),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  xlab("Population density") + ylab("Effect of ethnic boundaries") +
  NULL

png(file.path(fig.path, "popint_coefficients.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()
