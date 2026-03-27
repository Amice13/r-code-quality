################################
# ANALYSIS: ENUMERATION AREA LEVEL
################################

#### aggregate up to ea-level 
print("Aggregate")
data.pts <- aggregate.data.frame(data[,c("dead","birthorder.num","female","twin_dummy","mother.b.age",
                                         "epr.incl.ind_l1","epr.incl.pts_l1",
                                         "epr.incl.dist_l1")],
                                 by= data[ ,c("cowcode","dhs_round","dhs_subround","ADM1_CODE","id.new","v001","year")],
                                 FUN = mean,na.rm=T)
dim(data.pts)

cor(data.pts$epr.incl.pts,data.pts$epr.incl.ind,use="complete.obs")

data.pts$pts.round <- as.factor(paste0(data.pts$cowcode, ".",data.pts$dhs_round,".",data.pts$dhs_subround,".",data.pts$v001))
data.pts$dist.round <- as.factor(paste0(data.pts$cowcode, ".",data.pts$dhs_round,".",data.pts$dhs_subround,".",data.pts$id.new))
data.pts$cow.round <- as.factor(paste0(data.pts$cowcode, ".",data.pts$dhs_round,".",data.pts$dhs_subround))
data.pts$birth.reg.year <- as.factor(paste0(data.pts$cowcode, ".",data.pts$dhs_round,".",data.pts$dhs_subround,".",data.pts$ADM1_CODE,".",data.pts$year))
data.pts$birth.dist.year <- as.factor(paste0(data.pts$cowcode, ".",data.pts$dhs_round,".",data.pts$dhs_subround,".",data.pts$id.new,".",data.pts$year))
data.pts$birth.pts.year <- as.factor(paste0(data.pts$cowcode, ".",data.pts$dhs_round,".",data.pts$dhs_subround,".",data.pts$v001,".",data.pts$year))
data.pts$birth.cow.year <- as.factor(paste0(data.pts$cowcode, ".",data.pts$dhs_round,".",data.pts$dhs_subround,".",data.pts$year))

names(data.pts)[which(names(data.pts)=="epr.incl.ind_l1")] <- "epr.incl.pts.raw_l1"

## Estimate
print("Estimate")


# check ea-level fixed effects and ea-level share of included

## ea-level fixed effects to account for unobserved geographic determinants of health outcomes within districts
form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.dist_l1 + ",
                  controls.str,
                  "| factor(ethnic.id) + factor(pts.round) + factor(birth.reg.year)", clust.var.a)
this.data <- data[,unlist(lapply(colnames(data),
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.fe.ea <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.fe.ea)


## ea-level inclusion to check whether results hold at finer spatial resolution
form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.pts_l1 +",
                  controls.str,
                  "| factor(ethnic.id) + factor(pts.round) + factor(birth.reg.year)", "| 0 | ethnic.id + pts.round")
this.data <- data[,unlist(lapply(colnames(data),
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.pts.incl <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.pts.incl)

# Point level to check whether entirely non-co-ethnic EAs in co-ethnic districts benefit
form.str <- paste("dead ~ epr.incl.pts.raw_l1*epr.incl.dist_l1",
                  "| pts.round + year | 0 | pts.round ")
this.data <- data.pts[,unlist(lapply(colnames(data.pts),
                                     function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                        !colnames(data.pts) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.pts <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.pts)




m.list <- list(m.fe.ea,m.pts.incl,m.pts)


add.lines <- list(latex.uoa(c("Ind.","Ind.","EA-Year")), 
                  latex.ethnic.id.fe(c("yes","yes","--")), 
                  latex.dist.fe(c("--","--","--")),
                  latex.pts.fe(c("yes","yes","yes")),
                  latex.year.fe(c("--","--","yes")),
                  latex.reg.year.fe(c("yes","yes","no")),
                  latex.controls(c("yes","yes","no")),
                  latex.clusters(c("Dist. \\& Ethn.", "DHS EA \\& Ethn.","EA \\& Country-YoB")))

keep.lines <- c("epr.incl.ind_l1","epr.incl.dist_l1","epr.incl.ind_l1:epr.incl.dist_l1",
                "epr.incl.pts_l1","epr.incl.ind_l1:epr.incl.pts_l1",
                "epr.incl.pts.raw_l1","epr.incl.pts.raw_l1:epr.incl.dist_l1")

fileConn<-file(paste0(tab.path,"robustness_ea.tex"))
writeLines(stargazer(m.list,
                     title="Robustness: Enumeration Areas \\& Spatial Segregation",
                     keep=keep.lines,
                     order = paste0("^", keep.lines , "$"),
                     multicolumn=F,# se = se,
                     column.sep.width = "11pt",
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)",
                                        "EA Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ EA Share Co-Ethnics (t-1)",
                                        "EA Share Co-Ethnics (t-1)", 
                                        "EA Share $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="robustness_ea",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(.95), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)

