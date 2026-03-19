################################
# ANALYSIS: HOUSEHOLD AND MOTHER Fixed effects
################################

# Effective number of observations and fixed effects

## HH
obs.var <- na.omit(unique(data[!is.na(data$dead), c("hh.id", "epr.incl.ind_l1", "epr.incl.dist_l1")]))

### Number of fixed effects: 314909
length(unique(obs.var$hh.id))

### Number of hh with variation: 123176
effect.hh <- unique(obs.var$hh.id[duplicated(obs.var$hh.id)])
length(effect.hh)

### Number of infants in these HH: 807260
sum(data$hh.id %in% effect.hh & !is.na(data$dead) & !is.na(data$epr.incl.ind_l1))


## Mothers
obs.var <- na.omit(unique(data[!is.na(data$dead), c("mother.id", "epr.incl.ind_l1", "epr.incl.dist_l1")]))

### Number of fixed effects: 379818
length(unique(obs.var$mother.id))

### Number of mothers with variation: 130334
effect.moth <- unique(obs.var$mother.id[duplicated(obs.var$mother.id)])
length(effect.moth)

### Number of infants from these mothers: 736095
sum(data$mother.id %in% effect.moth & !is.na(data$dead) & !is.na(data$epr.incl.ind_l1))



# Estimation ####

####household and mother FE
form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.dist_l1 +",
                  controls.str,
                  "| factor(hh.id) + factor(year)", "| 0 | ethnic.id + dist.round")
this.data <- data[,unlist(lapply(colnames(data),
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.hh.fe <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.hh.fe)

form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.dist_l1 +",
                  controls.str,
                  "| factor(hh.id) + factor(birth.reg.year)", "| 0 | ethnic.id + dist.round")
this.data <- data[,unlist(lapply(colnames(data),function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.hh.fe2 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.hh.fe2)



form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.dist_l1 +",
                  controls.str,
                  "| factor(mother.id) + factor(year)", "| 0 | ethnic.id + dist.round")
this.data <- data[,unlist(lapply(colnames(data),
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.mo.fe <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.mo.fe)

form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.dist_l1 +",
                  controls.str,
                  "| factor(mother.id) + factor(birth.reg.year)", "| 0 | ethnic.id + dist.round")
this.data <- data[,unlist(lapply(colnames(data),function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.mo.fe2 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.mo.fe2)


m.list <- list(m.hh.fe,m.hh.fe2,m.mo.fe,m.mo.fe2)

add.lines <- list(latex.hh.fe(c("yes","yes","--","--")), 
                  latex.mother.fe(c("no","no","yes","yes")),
                  latex.year.fe(c("yes","no","yes","no")),
                  latex.reg.year.fe(c("no","yes","no","yes")),
                  latex.controls(c("yes","yes","yes","yes")))
keep.lines <- which(grepl("epr.",rownames(m.list[[1]]$coefficients)))

fileConn<-file(paste0(tab.path,"hh_mo_fe.tex"))
writeLines(stargazer(m.list,
                     title="Household and Mother Fixed Effects",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="rob_hhmothfe",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(.95), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)






