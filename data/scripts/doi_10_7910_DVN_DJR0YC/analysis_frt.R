################################
# ANALYSIS: FRANCOIS, RAINER & TREBBI
################################





######################### FRT Robustness Check #################################
frt.sample <- unique(data$cowcode[!is.na(data$leadergroup.ind)])
countrycode(frt.sample,"cown","country.name")

## baseline model with leader co-ethnicity
form.str <- paste("dead ~ leadergroup.ind_l1*leadergroup.dist_l1 + ",
                  controls.str,
                  fe.spec.frt[1], clust.var.frt)
this.data <- data[data$cowcode%in%frt.sample,unlist(lapply(colnames(data),
                                                           function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.frt1 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.frt1)

## baseline model with co-ethnicity to "top ministers"
form.str <- paste("dead ~ topgovpositions.ind_l1*topgovpositions.dist_l1 + ",
                  controls.str,
                  fe.spec.frt[1], clust.var.frt)
this.data <- data[data$cowcode%in%frt.sample,unlist(lapply(colnames(data),
                                                           function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.frt2 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.frt2)

## baseline model with co-ethnicity to any ministers
form.str <- paste("dead ~ govpositions.ind_l1*govpositions.dist_l1 + ",
                  controls.str,
                  fe.spec.frt[1], clust.var.frt)
this.data <- data[data$cowcode%in%frt.sample,unlist(lapply(colnames(data),
                                                           function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")]
this.data <- na.omit(this.data)
this.data$weights <- gen_weights(this.data$cowcode)
m.frt3 <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, exactDOF=T)
summary(m.frt3)


m.list <- list(m.frt1,m.frt2,m.frt3)

add.lines <- list(latex.ethnic.id.fe(c("yes","yes","yes")), 
                  latex.dist.fe(c("yes","yes","yes")),
                  latex.reg.year.fe(c("yes","yes","yes")),
                  latex.controls(c("yes","yes","yes")))

keep.lines <- c("leadergroup.ind_l1","leadergroup.dist_l1","leadergroup.ind_l1:leadergroup.dist_l1",
                "topgovpositions.ind_l1","topgovpositions.dist_l1","topgovpositions.ind_l1:topgovpositions.dist_l1",
                "govpositions.ind_l1","govpositions.dist_l1","govpositions.ind_l1:govpositions.dist_l1")

fileConn<-file(paste0(tab.path,"robustness_frt.tex"))
writeLines(stargazer(m.list,
                     title="Robustness: FRT Data",
                     keep=keep.lines,
                     order = paste0("^", keep.lines , "$"),
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Leader Co-Ethnic (t-1)", 
                                        "Dist. Share Leader Co-Ethnics (t-1)",
                                        "Leader Co-Ethnic $\\times$ Dist. Share Leader Co-Ethnics (t-1)",
                                        "Top Gov. Co-Ethnic (t-1)", 
                                        "Dist. Share Top Gov. Co-Ethnics (t-1)",
                                        "Top Gov. Co-Ethnic $\\times$ Dist. Share Top Gov. Co-Ethnics (t-1)",
                                        "Gov. Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="robustness_frt",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long.frt, notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)
mean(data$dead[data$cowcode%in%frt.sample],na.rm=T)


##### Plot FRT TopGov Results
m <- m.frt2
beta <- as.vector(m$beta)
names(beta) <- paste0("b",c(1:length(coef(m))))
rownames(m$beta)
beta


pred1<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
#fill in predictions and standard errors
for (i in 1:nrow(pred1)) {
  a <- pred1$z[i] 
  pred1$b[i] <- deltaMethod(beta,"b1 + a*b2 + a*b9",vcov.=m$clustervcv)$Estimate
  pred1$se[i] <- deltaMethod(beta,"b1 + a*b2 + a*b9",vcov.=m$clustervcv)$SE
  pred1$lb[i] <- pred1$b[i]-1.96*pred1$se[i]
  pred1$ub[i] <- pred1$b[i]+1.96*pred1$se[i]
}

pred2<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
#fill in predictions and standard errors
for (i in 1:nrow(pred2)) {
  a <- pred2$z[i] 
  pred2$b[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$Estimate
  pred2$se[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$SE
  pred2$lb[i] <- pred2$b[i]-1.96*pred2$se[i]
  pred2$ub[i] <- pred2$b[i]+1.96*pred2$se[i]
}

pred1$co_ethnic <- "yes"
pred2$co_ethnic <- "no"
pred <- rbind(pred1,pred2)


#### ggplot solution
dens <- density(data$epr.incl.dist[data$cowcode%in%frt.sample], na.rm=T)
dens=data.frame(x=dens$x,y=dens$y)
dens$scaled <- (dens$y/2.5) - 4
describe(dens)

p <- ggplot(data = pred)

main  <- p + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                         fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b, fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline") +
  scale_x_continuous(name="District-level Share of Top Cabinet Co-Ethnics", limits=c(-0.01,1.01)) +
  labs(linetype="Top Cabinet Co-Ethnic",
       title = "Ethnic Favoritism at the Individual and District Level (FRT Analysis)",
       subtitle = "Linear predictions relative to top cabinet non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Model 1 in Table 1.\nDot-dashed line represents density of district co-ethnicity shares.") + 
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash") 

out <- main + theme_grey(base_size = 18) +  
  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) +
  scale_linetype_manual(values=c(1,2),name="Government Co-Ethnic") 
out 
out + ggsave(paste0(fig.path,"pred_baseline_frt.pdf"),width=12,height=9)

