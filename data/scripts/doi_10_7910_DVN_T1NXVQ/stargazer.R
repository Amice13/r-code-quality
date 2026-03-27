#############################
# HELPER FUNCTIONS FOR TABLES AND PLOTS
# particular to work with stargazer. 
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

# Draw vertical line into density plot
dens_at <- function(d, fun = mean, ...){
  # Copied from a StackOverflow solution by whuber:
  #http://stats.stackexchange.com/questions/32093/
  #how-to-draw-mean-median-and-mode-lines-in-r-that-end-at-density
  dens <- density(d)
  n <- length(dens$y)
  dx <- mean(diff(dens$x))                  # Typical spacing in x
  y.unit <- sum(dens$y) * dx                # Check: this should integrate to 1
  dx <- dx / y.unit                         # Make a minor adjustment
  x.mean <- fun(d, ...) 
  y.mean <- dens$y[length(dens$x[dens$x < x.mean])]
  data.frame(mean = x.mean, est.dens = y.mean)
}


# (function) Latex entry for FStat with multiple instruments
latex.fstat.multi <- function(model.ls, f.id, name, condfstat.type){
  latex.addline(name, unlist(lapply(model.ls, function(m){
    f <- try(condfstat(m, type = condfstat.type)[,f.id])
    if(class(f) == "try-error"){ "" } else { as.character(round_any(f, 0.01))}
  })))
} 

# (function) Add line to latex table
latex.addline <- function(title,entries){c(title,paste0("\\multicolumn{1}{c}{",entries,"}"))}
latex.addline_nomc <- function(title,entries){c(title,entries)}

# (function) Mean of dependent variable for latex table
latex.mean.dv <- function(model.ls){
  c("Mean DV:",paste0("\\multicolumn{1}{c}{",signif(unlist(lapply(model.ls, function(m){
    if(!is.null(m$weights)){
      sum(m$response*m$weights) / sum(m$weights)
    } else {
      mean(m$response)
    }
    
  })), 2),"}"))
}

# (function) SD of dependent variable for latex table
latex.mean.sd <- function(model.ls){
  c("Std.-dev. DV:",paste0("\\multicolumn{1}{c}{",signif(unlist(lapply(model.ls, function(m){sd(m$response)})), 2),"}"))
}

# (function) Extract all coefficients from a list of felm models
extract_coef <- function(m.list){
  lapply(m.list, function(m){
    list(dv = colnames(m$coefficients),
         coef = m$coefficients,
         clustervcv = m$clustervcv)
  })
}

# (function) helper function to combine
make_form <- function(dv, expl, fe, iv = "0", se = "0" ){
  as.formula(paste(dv, "~", paste(expl, collapse = "+"), "|",
                   paste(fe, collapse = "+"), "|", iv, "|", se))
}

# (function) Generate Regression weights
gen_weights <- function(vec){
  tab <- table(vec)
  weights <- data.frame(cbind(weight = as.numeric(tab), unit = as.character(names(tab))),
                        stringsAsFactors = F)
  weight.vec <- 1/as.numeric(join(data.frame(unit = as.character(vec), stringsAsFactors = F),
                                  weights, type = "left", by = "unit")[,2])
  return(weight.vec)
  
}

# (function) Make coefficients with SE to nice text
effect_text <- function(coef, se = NULL, ci = NULL , trans_fun = NULL, accuracy = 1){
  if(!is.null(se)){
    if(is.null(trans_fun)){
      paste0(sprintf(paste0("%.", accuracy, "f"),coef),
             " [", sprintf(paste0("%.", accuracy, "f"), 
                           (coef - se*1.95)),", ", 
             sprintf(paste0("%.", accuracy, "f"), (coef + se*1.95)), "]")
    } else {
      paste0(sprintf(paste0("%.", accuracy, "f"),trans_fun(coef)),
             " [", sprintf(paste0("%.", accuracy, "f"), 
                           (trans_fun(coef - se*1.95))),", ", 
             sprintf(paste0("%.", accuracy, "f"), 
                     (trans_fun(coef + se*1.95))), "]")
    }
  } else if(!is.null(ci)){
    if(is.null(trans_fun)){
      paste0(sprintf(paste0("%.", accuracy, "f"), (coef)),
             " [", sprintf(paste0("%.", accuracy, "f"), (ci[1])),", ", 
             sprintf(paste0("%.", accuracy, "f"), (ci[2])), "]")
    } else {
      paste0(sprintf(paste0("%.", accuracy, "f"), (trans_fun(coef))),
             " [", sprintf(paste0("%.", accuracy, "f"), (trans_fun(ci[1]))),", ", 
             sprintf(paste0("%.", accuracy, "f"), (trans_fun(ci[2]))), "]")
    }
  } else {
    if(is.null(trans_fun)){
      paste0(sprintf(paste0("%.", accuracy, "f"), (coef)))
    } else {
      paste0(sprintf(paste0("%.", accuracy, "f"), (trans_fun(coef))))
    }
  }
  
}


# (function) Make stargazer columnlabels with one extra layer and rulers
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
