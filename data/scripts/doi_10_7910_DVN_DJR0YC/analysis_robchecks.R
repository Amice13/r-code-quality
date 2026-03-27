################################
# ANALYSIS: ADDITIONAL ROBUSTNESS CHECKS
################################


################
# Table A6: no control variables
################

m1.2cl.weights.nc <- list()
for(fe in fe.spec1){
  form.str <- paste("dead ~  epr.incl.ind_l1*epr.incl.dist_l1",
                    fe, clust.var.a)
  this.data <- data[,unlist(lapply(colnames(data), 
                                   function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                      !colnames(data) %in% c("age")] 
  this.data <- na.omit(this.data)    
  this.data$weights <- gen_weights(this.data$cowcode)
  m <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
  print(summary(m))
  m1.2cl.weights.nc <- c(m1.2cl.weights.nc, list(m))
}




m.list <- m1.2cl.weights.nc



add.lines <- list(latex.ethnic.id.fe(c("yes","--","yes","--")), 
                  latex.dist.fe(c("yes","yes","--","--")),
                  latex.reg.year.fe(c("yes","yes","--","--")),
                  latex.ethnic.year.fe(c("no","yes","no","yes")),
                  latex.dist.year.fe(c("no","no","yes","yes")),
                  latex.controls(c("no","no","no","no")))
keep.lines <- which(grepl("epr.",rownames(m.list[[1]]$coefficients)))

latex.notes.nc <- "\\parbox[t]{.95\\textwidth}{\\textit{Notes:} OLS linear probability models. 
The sample mean of the dependent variable is 10.77 infant deaths per 100 live births.   
Observations are weighted to ensure equal weights for each country.
Two-way clustered standard errors in parentheses (survey-ethnic group and survey-district clusters).
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}"

fileConn<-file(paste0(tab.path,"robustness_nc.tex"))
writeLines(stargazer(m.list,
                     title="No Control Variables",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="nocontrols",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.nc, notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)



################
# Table A3: different temporal lags
################


form.str <- paste("dead ~  epr.incl.ind*epr.incl.dist +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m0 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m0)

form.str <- paste("dead ~  epr.incl.ind_l1*epr.incl.dist_l1 +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m1 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m1)

form.str <- paste("dead ~  epr.incl.ind_l2*epr.incl.dist_l2 +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m2 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m2)

form.str <- paste("dead ~  epr.incl.ind_l3*epr.incl.dist_l3 +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m3 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m3)



m.list <- list(m0,m1,m2,m3)
add.lines <- list(latex.ethnic.id.fe(c("yes","yes","yes","yes")), 
                  latex.dist.fe(c("yes","yes","yes","yes")),
                  latex.reg.year.fe(c("yes","yes","yes","yes")),
                  latex.controls(c("yes","yes","yes","yes")))
keep.lines <- c("epr.incl.ind","epr.incl.dist","epr.incl.ind:epr.incl.dist",
                "epr.incl.ind_l1","epr.incl.dist_l1","epr.incl.ind_l1:epr.incl.dist_l1",
                "epr.incl.ind_l2","epr.incl.dist_l2","epr.incl.ind_l2:epr.incl.dist_l2",
                "epr.incl.ind_l3","epr.incl.dist_l3", "epr.incl.ind_l3:epr.incl.dist_l3")

fileConn<-file(paste0(tab.path,"robustness_lags.tex"))
writeLines(stargazer(m.list,
                     title="Different Temporal Lags",
                     keep=keep.lines, 
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     column.labels = c("t","t-1","t-2","t-3"), model.numbers = F,
                     #column.separate = c(2,2,1),
                     column.sep.width = "5pt",
                     order = paste0("^", keep.lines , "$"),
                     covariate.labels= paste0(rep(c("Government Co-Ethnic",
                                                    "Dist. Share Government Co-Ethnics",
                                                    "Co-Ethnic $\\times$ Dist. Share Co-Ethnics"), 4),
                                              rep(c(" (t)", " (t-1)", " (t-2)", " (t-3)"), each = 3)),
                     font.size = "scriptsize",
                     notes.align = "c",label="robustness_lags",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(.95), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)



################
# Table A4: clustering standard errors at different spatial units and year
################

clusters <- c("| 0 | pts.round + year","| 0 |dist.round + year","| 0 |reg.round + year",
              "| 0 | cow.round + year")

#### different SEs
m1.clusters.weights <- list()
for(cl in clusters){
  form.str <- paste("dead ~  epr.incl.ind_l1*epr.incl.dist_l1 +", 
                    controls.str,
                    "|dist.round + ethnic.id + birth.reg.year", cl)
  this.data <- data[,unlist(lapply(colnames(data), 
                                   function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                      !colnames(data) %in% c("age")] 
  this.data <- na.omit(this.data)    
  this.data$weights <- gen_weights(this.data$cowcode)
  m <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
  print(summary(m))
  m1.clusters.weights <- c(m1.clusters.weights, list(m))
}



m.list <- m1.clusters.weights

add.lines <- list(latex.clusters(c("EA \\& Year","Dist. \\& Year","Region \\& Year","Country \\& Year")),
                  latex.ethnic.id.fe(c("yes","yes","yes","yes")), 
                  latex.dist.fe(c("yes","yes","yes","yes")),
                  latex.reg.year.fe(c("yes","yes","yes","yes")),
                  latex.controls(c("yes","yes","yes","yes")))
latex.notes.3 <- "\\parbox[t]{.95\\textwidth}{\\textit{Notes:} OLS linear probability models. 
The sample mean of the dependent variable is 10.78 infant deaths per 100 live births.   
Observations are weighted to ensure equal weights for each country.
Control variables include mothers' age and age squared, as well as infants' sex, a twin dummy, 
birth rank, and birth rank squared.
Differently clustered standard errors in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}"
keep.lines <- which(grepl("epr.",rownames(m.list[[1]]$coefficients)))

fileConn<-file(paste0(tab.path,"robustness_clusters.tex"))
writeLines(stargazer(m.list,
                     title="Differently Clustered Standard Errors",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     column.sep.width = "5pt",
                     dep.var.caption = "Infant Mortality U1",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="robustness_clusters",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.3, notes.label = "", notes.append = F
), 
fileConn)
close(fileConn)




################
# Table A5: No regression weights
################

m1.2cl <- list()
for(fe in fe.spec1){
  form.str <- paste("dead ~  epr.incl.ind_l1*epr.incl.dist_l1 +", 
                    controls.str, 
                    fe, clust.var.a)
  m <- felm(as.formula(form.str),data = data, exactDOF=T)
  print(summary(m))
  m1.2cl <- c(m1.2cl, list(m))
}


m.list <- m1.2cl

add.lines <- list(latex.ethnic.id.fe(c("yes","--","yes","--")), 
                  latex.dist.fe(c("yes","yes","--","--")),
                  latex.reg.year.fe(c("yes","yes","--","--")),
                  latex.ethnic.year.fe(c("no","yes","no","yes")),
                  latex.dist.year.fe(c("no","no","yes","yes")),
                  latex.controls(c("no","no","no","no")))
keep.lines <- which(grepl("epr.",rownames(m.list[[1]]$coefficients)))
latex.notes.4 <- "\\parbox[t]{0.95\\textwidth}{\\textit{Notes:} OLS linear probability models. 
The sample mean of the dependent variable is 10.77 infant deaths per 100 live births.   
Control variables include mothers' age and age squared, as well as infants' sex, a twin dummy, 
birth rank, and birth rank squared.
Two-way clustered standard errors in parentheses (survey-ethnic group and survey-district clusters).
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}"


fileConn<-file(paste0(tab.path,"unweighted.tex"))
writeLines(stargazer(m.list,
                     title="Infant Mortality: Unweighted Regressions",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="m1.2cl",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.4, notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)






################
# Table A7: EPR Senior Partners instead of all included groups
################

m1.2cl.weights.senior <- list()
for(fe in fe.spec1){
  form.str <- paste("dead ~  epr.senior.ind_l1*epr.senior.dist_l1 +", 
                    controls.str, 
                    fe, clust.var.a)
  this.data <- data[,unlist(lapply(colnames(data), 
                                   function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                      !colnames(data) %in% c("age")] 
  this.data <- na.omit(this.data)    
  this.data$weights <- gen_weights(this.data$cowcode)
  m <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
  print(summary(m))
  m1.2cl.weights.senior <- c(m1.2cl.weights.senior, list(m))
}


m.list <- m1.2cl.weights.senior

add.lines <- list(latex.ethnic.id.fe(c("yes","--","yes","--")), 
                  latex.dist.fe(c("yes","yes","--","--")),
                  latex.reg.year.fe(c("yes","yes","--","--")),
                  latex.ethnic.year.fe(c("no","yes","no","yes")),
                  latex.dist.year.fe(c("no","no","yes","yes")),
                  latex.controls(c("yes","yes","yes","yes")))
keep.lines <- which(grepl("epr.",rownames(m.list[[1]]$coefficients)))

fileConn<-file(paste0(tab.path,"m1.2cl.weights_senior.tex"))
writeLines(stargazer(m.list,
                     title="Government Senior Partners",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Senior Government Co-Ethnic (t-1)", 
                                        "Dist. Share Senior Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="m1.2cl.weights_senior",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(".95"), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)




################
# Table A8: Separate DiDs for individual and district-level government co-ethnicity
################

## ethnic, no controls
form.str <- paste("dead ~ epr.incl.ind_l1", 
                  "| ethnic.id + birth.reg.year | 0 | ethnic.id")
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m1.nc <- felm(as.formula(form.str),data = this.data, weights=this.data$weights, exactDOF=T, keepCX = F)
summary(m1.nc)

## ethnic, controls
form.str <- paste("dead ~ epr.incl.ind_l1 +", 
                  controls.str, 
                  "| ethnic.id + birth.reg.year | 0 | ethnic.id")
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m1 <- felm(as.formula(form.str),data = this.data, weights=this.data$weights, exactDOF=T)
summary(m1)


## districts, nc
form.str <- paste("dead ~ epr.incl.dist_l1",
                  "| dist.round + birth.reg.year | 0 | dist.round")
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m2.nc <- felm(as.formula(form.str),data = this.data, weights=this.data$weights, exactDOF=T, keepCX = F)
summary(m2.nc)



## districts
form.str <- paste("dead ~ epr.incl.dist_l1 +", 
                  controls.str, 
                  "| dist.round + birth.reg.year | 0 | dist.round")
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m2 <- felm(as.formula(form.str),data = this.data, weights=this.data$weights, exactDOF=T, keepCX = F)
summary(m2)



m.list <- list(m1.nc,m1,m2.nc,m2)
add.lines <- list(latex.ethnic.id.fe(c("yes","yes","no","no")), 
                  latex.dist.fe(c("no","no","yes","yes")),
                  latex.reg.year.fe(c("yes","yes","yes","yes")),
                  latex.controls(c("no","yes","no","yes")))
keep.lines <- c("epr.incl.ind_l1","epr.incl.dist_l1")

fileConn<-file(paste0(tab.path,"simple_dd.tex"))
writeLines(stargazer(m.list,
                     title="Infant Mortality: Ethnic vs. District-level Diff-in-Diff",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="simple_dd",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(".95"), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)


################
# table A9: Pre-Switch dummies and trends
################

# Model with dummies for the year before an ethnic group's switch to/from political inclusion 
form.str <- paste("dead ~  epr.incl.ind_l1*epr.incl.dist_l1 + upgrade + downgrade +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m.pre.dummies1 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m.pre.dummies1)

form.str <- paste("dead ~  epr.incl.ind_l1*epr.incl.dist_l1 + upgrade + upgrade_f1 + upgrade_f2 +
                  downgrade + downgrade_f1 + downgrade_f2 +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m.pre.dummies3 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.pre.dummies3)


form.str <- paste("dead ~  epr.incl.ind_l1*epr.incl.dist_l1 + pre_trend3_up_l1 + pre_trend3_down_l1 +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m.pre.trend.3 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m.pre.trend.3)


form.str <- paste("dead ~  epr.incl.ind_l1*epr.incl.dist_l1 + pre_trend5_up_l1 + pre_trend5_down_l1 +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m.pre.trend.5 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m.pre.trend.5)


m.list <- list(m.pre.dummies1,m.pre.dummies3, m.pre.trend.3,m.pre.trend.5)
add.lines <- list(latex.ethnic.id.fe(c("yes","yes","yes","yes")), 
                  latex.dist.fe(c("yes","yes","yes","yes")),
                  latex.reg.year.fe(c("yes","yes","yes","yes")),
                  latex.controls(c("yes","yes","yes","yes")))
keep.lines <- c("epr.incl.ind_l1","epr.incl.dist_l1", "epr.incl.ind_l1:epr.incl.dist_l1",
                "upgrade","upgrade_f1","upgrade_f2", "downgrade","downgrade_f1","downgrade_f2",
                "pre_trend3_up_l1","pre_trend3_down_l1",
                "pre_trend5_up_l1","pre_trend5_down_l1")

fileConn<-file(paste0(tab.path,"pre_trends.tex"))
writeLines(stargazer(m.list,
                     title="Robustness: Pre-Trends",
                     keep=keep.lines,
                     order = paste0("^", keep.lines , "$"),
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)",
                                        "Upgrade$_{t}$",
                                        "Upgrade$_{t+1}$",
                                        "Upgrade$_{t+2}$",
                                        "Downgrade$_{t}$",
                                        "Downgrade$_{t+1}$",
                                        "Downgrade$_{t+2}$",
                                        "Pre-Trend Upgrade$_{t+2 \\; \\text{to} \\; t}$",
                                        "Pre-Trend Downgrade$_{t+2 \\; \\text{to} \\; t}$",
                                        "Pre-Trend Upgrade$_{t+4 \\;  \\text{to} \\; t}$",
                                        "Pre-Trend Downgrade$_{t+4 \\; \\text{to} \\; t}$"),
                     font.size = "scriptsize",
                     notes.align = "c",label="pre_trends",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(".95"), notes.label = "", notes.append = F), fileConn)
close(fileConn)


