################################
# ANALYSIS: TABLE
################################



######### Table 2 in main text


### Model with linear ethnic-group-specific time trends
form.str <- paste("dead ~ epr.incl.ind_l1 + epr.incl.dist_l1 + I(epr.incl.ind_l1*epr.incl.dist_l1) + factor(ethnic.id):year +", 
                  controls.str, 
                  fe.spec1[1], clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m1.2cl.weights.tt <- felm(as.formula(form.str),data = this.data, 
                          weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m1.2cl.weights.tt)




##### Pre-Trends: 
# Model with dummies for the three years before an ethnic group's switch to/from political inclusion 
form.str <- paste("dead ~  epr.incl.ind_l1 + epr.incl.dist_l1 + I(epr.incl.ind_l1*epr.incl.dist_l1) + upgrade_f0_f2 + downgrade_f0_f2 +", 
                  controls.str, 
                  "| factor(ethnic.id) + factor(birth.reg.year) + factor(dist.round)", clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m.pre.3 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m.pre.3)


# Survey Cluster FE to address public vs private good distinction
form.str <- paste("dead ~  epr.incl.ind_l1 + I(epr.incl.ind_l1*epr.incl.dist_l1) + ", 
                  controls.str, 
                  "| factor(ethnic.id) + factor(birth.pts.year)", clust.var.a)
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m.cluster.fe <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m.cluster.fe)


# Selective Migration & Sorting: Subset of children born to mothers with no or only primary education
form.str <- paste("dead ~  epr.incl.ind_l1 + epr.incl.dist_l1 + I(epr.incl.ind_l1*epr.incl.dist_l1) +", 
                  controls.str, 
                  "| factor(ethnic.id) + factor(birth.reg.year) + factor(dist.round)", clust.var.a)
this.data <- data[data$mother.educ.fac%in%c(0,1),unlist(lapply(colnames(data), 
                                                               function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)
m.educ <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T, keepCX = F)
summary(m.educ)





####### Output Table
mean(data$dead, na.rm=T)
mean(data$dead[data$mother.educ.fac%in%c(0,1)], na.rm=T)
latex.notes.trend <- "\\parbox[t]{\\textwidth}{\\textit{Notes:} OLS linear probability models. 
Column 4 restricts the sample to children born to mothers with less than secondary education.
The sample mean of the dependent variable is 10.77 in columns 1--3 and 11.37 in column 4.   
Observations are weighted to ensure equal weights for each country.
Control variables include mothers' age and age squared, as well as childrens' sex, a twin dummy, 
birth rank, and birth rank squared. 
Two-way clustered standard errors in parentheses (survey-ethnic group and survey-district clusters).  
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}"


m.list <- list(m.cluster.fe,m1.2cl.weights.tt,m.pre.3,m.educ)
add.lines <- list(latex.ethnic.id.fe(c("yes","yes","yes","yes")), 
                  latex.dist.fe(c("--","yes","yes","yes")),
                  latex.reg.year.fe(c("--","yes","yes","yes")),
                  latex.pts.year.fe(c("yes","no","no","no")),
                  latex.ethnic.tt(c("no","yes","no","no")),
                  latex.controls(c("yes","yes","yes","yes")))
keep.lines <- c("epr.incl.ind_l1","epr.incl.dist_l1", "epr.incl.ind_l1 * epr.incl.dist_l1",
                "upgrade_f0_f2","downgrade_f0_f2")

fileConn<-file(paste0(tab.path,"robustness_trends.tex"))
writeLines(stargazer(m.list,
                     title="Robustness: Cluster-Year Fixed Effects, Trends, and Subsample Analysis",
                     keep=keep.lines, order = c(1:5),
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     column.labels = c("Cluster-YoB FE","Time Trend", "Pre-Trends", "Less Educated"), model.numbers = T,
                     #column.separate = c(2,2,1),
                     column.sep.width = "+5pt",
                     # order = paste0("^", keep.lines , "$"),
                     covariate.labels=c("Government Co-Ethnic (t-1)",
                                        "Dist. Share Government Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)",
                                        "Upgrade$_{t \\; \\text{to} \\; t+2}$",
                                        "Downgrade$_{t \\; \\text{to} \\; t+2}$"),
                     font.size = "scriptsize",
                     notes.align = "c",label="trends",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.trend, notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)
