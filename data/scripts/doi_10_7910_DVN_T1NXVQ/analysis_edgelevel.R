#############################
# SIMPLE EDGE-LEVEL LOGISTIC REGRESSIONS
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

# Extra libraries
library(glmmML)
library(lfe)
library(stargazer)
library(sandwich)

# Functions

## Robust VCV
robustvcv <- function(object, ...) sandwich(object) *
  nobs(object) / (nobs(object) - 1)

## Stargazer helpers
collab_w_ruler <- function(column.labels, column.separate, trim = 10, add.below = NULL){
  cmidrule <- paste(paste0("\\cmidrule(lr{",trim,"pt}){", c(2, 2 + cumsum(column.separate[-length(column.separate)])), "-",
                           c(1+ cumsum(column.separate)), "}"), collapse = " ")
  if(is.null(add.below)){
    result <-  c(column.labels[-length(column.labels)], 
                 paste0(column.labels[length(column.labels)], "} \\\\ ", cmidrule, " \\\\[-6ex] {"))
  } else {
    add.b <- paste(paste0(" & \\multicolumn{1}{c}{", add.below, "}"), collapse = "")
    result <-  c(column.labels[-length(column.labels)], 
                 paste0(column.labels[length(column.labels)], "} \\\\ ", cmidrule, add.b, " {"))
  }
  
  return(result)
}


# ESTIMATE MODEL #########

# Models
form.ls <- list(pool_ldv = as.formula(paste("cshp",
                                            " ~ cuteth_lag+ cshp_lag + abram_sum + ", main_cov_spec, "  ",
                                            "")),
                pool_xsec = as.formula(paste("cshp",
                                             " ~ cuteth + ", main_cov_spec, "  ",
                                             "")))


# Data
edge.df <- do.call(rbind, lapply(cut_yrs, function(y){
  this.g <- graph
  ## rename treatment and outcome
  E(this.g)$cshp <- edge_attr(this.g, paste0("cshp_", y))
  E(this.g)$cuteth <- edge_attr(this.g, paste0("cuteth", cut_yrs[which(cut_yrs == y)]))
  if(y != cut_yrs[1]){
    E(this.g)$cshp_lag <- edge_attr(this.g, paste0("cshp_", cut_yrs[which(cut_yrs == y) -1]))
    E(this.g)$cuteth_lag <- edge_attr(this.g, paste0("cuteth", cut_yrs[which(cut_yrs == y) -1]))
  } else {
    E(this.g)$cshp_lag <- NA
    E(this.g)$cuteth_lag <- NA
  }
  
  ## Make data
  vars <- unique(unlist(sapply(form.ls, all.vars)))
  this.df <- data.frame(do.call(cbind, lapply(vars, function(v){
    edge_attr(this.g, v)
  })))
  colnames(this.df) <- vars 
  
  ## Return
  this.df
}))


# Logit
glm.ls <- rev(lapply(form.ls, function(f){
  m <- glm(f, data = edge.df, family = binomial)
  m$robustvcv <- robustvcv(m)
  m
}))



# Var labs and notes
var.labs <- c("Constant",
              "Ethnic boundary$_t$","Ethnic boundary$_{t-1}$", "State border$_{t-1}$",
              "Deep lag" , 
              "Edge length","River", "Watershed", "Elevation mean"
              )

latex.notes <- function(width = .75){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textit{Notes:} 
  Each period $t$ has a length of 25 years. 
  Robust standard errors in parenthesis. 
  Significance codes: $*$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}. ")
}
star.cutoffs <- c(.1,.05,.01)
star.char <- c("*", "**", "***")

# Table
fileConn <- file(file.path(tab.path,"models_edgelevel.tex"))
writeLines(stargazer(glm.ls,title="Edge level modeling: Logit results",
                     se = lapply(glm.ls, function(x){diag(x$robustvcv)^.5}),
                     keep = 1:5,
                     column.labels = c("Baseline model", "Lagged dependent variable"),
                     dep.var.labels= rep("", length(glm.ls)),
                     multicolumn=F,# se = se,
                     covariate.labels = var.labs,
                     omit = c(),
                     notes.align = "l",label="models_edgelevel",align =T,
                     add.lines = list(Controls = c("Controls", "yes", "yes")),
                     digits = 2, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("adj.rsq","res.dev","ser"),
                     notes = latex.notes(), 
                     notes.append = F,
                     notes.label = "",  star.char = star.char, 
                     star.cutoffs = star.cutoffs,
                     font.size = "scriptsize", column.sep.width ="10pt"), 
           fileConn)
close(fileConn)
