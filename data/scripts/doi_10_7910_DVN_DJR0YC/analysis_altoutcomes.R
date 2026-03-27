################################
# ANALYSIS: ALTERNATIVE OUTCOMES
################################




########## Alternative outcomes
## prenatal_trained, prenatal_doctor, inst. births, assist_trained, assist_doctor, birthweight_low
outcomes1 <- c("prenatal_min_healthprof","prenatal_doctor","institutional.birth","assist_min_healthprof","assist_doctor","birthweight_low","dead")



## Plot distribution of birth years
g <- ggplot(data = data, aes(x = year)) +
  geom_line(stat = "density", aes(color = "1. Baseline",
                                lty = "1. Baseline")) +
  geom_line(data = data[(data$side.year - data$year) <= 6,], 
            stat = "density", aes(color = "2. Age 5 and below at survey",
                                lty = "2. Age 5 and below at survey")) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(color = "Sample", lty = "Sample") +
  ylab("Density") + xlab("Birthyear") +  
  ggsave(paste0(fig.path,"birthyr_dist_altoutcomes.pdf"),width=5,height=3)
  



## Estimate
new.dv.no.fe <- list()
for(y in outcomes1){
  form.str <- paste(y, "~ epr.incl.ind_l1*epr.incl.dist_l1 + ",
                    controls.str,
                    "| birth.reg.year", "| 0 | ethnic.id + dist.round")
  this.data <- data[(data$side.year - data$year) <= 6, ]
  this.data <- this.data[,unlist(lapply(colnames(this.data),
                                   function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                      !colnames(this.data) %in% c("age")]
  this.data <- na.omit(this.data)
  this.data$weights <- gen_weights(this.data$cowcode)
  m <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
  print(summary(m))
  new.dv.no.fe <- c(new.dv.no.fe, list(m))
}


new.dv.both.fe <- list()
for(y in outcomes1){
  form.str <- paste(y, "~ epr.incl.ind_l1*epr.incl.dist_l1 + ",
                    controls.str,
                    "| birth.reg.year + ethnic.id + dist.round", "| 0 | ethnic.id + dist.round")
  this.data <- data[(data$side.year - data$year) <= 6, ]
  this.data <- this.data[,unlist(lapply(colnames(this.data),
                                        function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                           !colnames(this.data) %in% c("age")]
  this.data <- na.omit(this.data)
  this.data$weights <- gen_weights(this.data$cowcode)
  m <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
  print(summary(m))
  new.dv.both.fe <- c(new.dv.both.fe, list(m))
}




## These latex notes
these.latex.notes <- "\\parbox[t]{.95\\textwidth}{\\textit{Notes:} OLS linear probability models. 
Observations are weighted to ensure equal weights for each country.
Control variables include mothers' age and age squared as well as infants' sex, a twin dummy, 
birth rank, and birth rank squared.
Two-way clustered standard errors in parentheses (survey-ethnic group and survey-district clusters).
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}"


# new.dv.no.fe
dv.means1 <- round(apply(data[,outcomes1],2,mean,na.rm=T),2)
m.list <- new.dv.no.fe


add.lines <- list(latex.ethnic.id.fe(rep("no",7)), 
                  latex.dist.fe(rep("no",7)),
                  latex.reg.year.fe(rep("yes",7)),
                  latex.controls(rep("yes",7)),
                  latex.dv.means(dv.means1))
keep.lines <- which(grepl("epr.",rownames(m.list[[1]]$coefficients)))

fileConn<-file(paste0(tab.path,"new_outcomes7_no_fe.tex"))
writeLines(stargazer(m.list,
                     title="Alternative Outcomes, Children Age 5 and Below",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Prenatal Asst.", "Prenatal Doc.","Inst. Birth" ,
                                                                    "Asst. Birth", "Asst. Doctor","Low Weight","Dead"),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="new_outcomes_no_fe",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = these.latex.notes, notes.label = "", notes.append = F,float.env = "sidewaystable"), 
           fileConn)
close(fileConn)


# new.dv.both.fe
m.list <- new.dv.both.fe

add.lines <- list(latex.ethnic.id.fe(rep("yes",7)), 
                  latex.dist.fe(rep("yes",7)),
                  latex.reg.year.fe(rep("yes",7)),
                  latex.controls(rep("yes",7)),
                  latex.dv.means(dv.means1))
keep.lines <- which(grepl("epr.",rownames(m.list[[1]]$coefficients)))

fileConn<-file(paste0(tab.path,"new_outcomes7_both_fe.tex"))
writeLines(stargazer(m.list,
                     title="Alternative Outcomes, Children Age 5 and Below",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Prenatal Asst.", "Prenatal Doc.","Inst. Birth" ,
                                                                    "Asst. Birth", "Asst. Doctor","Low Weight","Dead"),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="new_outcomes_both_fe",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = these.latex.notes, notes.label = "", notes.append = F,float.env = "sidewaystable"), 
           fileConn)
close(fileConn)

