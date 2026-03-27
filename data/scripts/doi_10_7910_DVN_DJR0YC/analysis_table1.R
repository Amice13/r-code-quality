################################
# ANALYSIS: TABLE 1
################################


## empty container
m1.2cl.weights <- list()

## Independent variables
indep.var.spec <- c("epr.incl.ind_l1 + epr.incl.dist_l1 + I(epr.incl.ind_l1*epr.incl.dist_l1)", # constitutive terms and 2-way IA
                    "epr.incl.dist_l1 + I(epr.incl.ind_l1*epr.incl.dist_l1)", # individual co-ethnicity drops  out due to group-birthyear FE
                    "epr.incl.ind_l1 + I(epr.incl.ind_l1*epr.incl.dist_l1)", # district co-ethnicity drops out due to district-birthyear FE
                    "I(epr.incl.ind_l1*epr.incl.dist_l1)") # both constitutive terms drop out

### Estimate
# loops through 4 specifications with increasingly tight spatio-temporal fixed effects
## In each iteration:
# prepares model formula for felm function
# subsets data to relevant variables and complete observations
# weighs observations to ensure equal weight for each country
# estimates model and stores result in m1.2cl.weights

for(i in seq_along(fe.spec1)){
  form.str <- paste("dead ~  ", indep.var.spec[i], "+",
                    controls.str, 
                    fe.spec1[i], clust.var.a) 
  this.data <- data[,unlist(lapply(colnames(data), 
                                   function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                      !colnames(data) %in% c("age")] # 
  this.data <- na.omit(this.data)    
  this.data$weights <- gen_weights(this.data$cowcode)
  m <- felm(as.formula(form.str),data = this.data, weights = this.data$weights, 
            exactDOF=T, keepCX = F)
  print(summary(m))
  m1.2cl.weights <- c(m1.2cl.weights, list(m))
}


# prepare additional lines for regression table
add.lines <- list(latex.ethnic.id.fe(c("yes","--","yes","--")), 
                  latex.dist.fe(c("yes","yes","--","--")),
                  latex.reg.year.fe(c("yes","yes","--","--")),
                  latex.ethnic.year.fe(c("no","yes","no","yes")),
                  latex.dist.year.fe(c("no","no","yes","yes")),
                  latex.controls(c("yes","yes","yes","yes")))
# define which coefficients are shown in the output table
keep.lines <- which(grepl("epr.",rownames(m1.2cl.weights[[1]]$coefficients)))

# prepare and save output table
fileConn<-file(paste0(tab.path,"baseline.tex"))
writeLines(stargazer(m1.2cl.weights,
                     title="Main Specifications",
                     keep=keep.lines,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m1.2cl.weights)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="baseline",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(.95), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)


####### ####### ####### ####### ####### ####### ####### ####### 
####### Prepare Figures 3, A6, A7, and A8
# Continuous prediction plot from main model (Figure 3)
# Conventional marginal effects plot (Figure A6)
# Prediction plot with binning estimates (Figure A7)
# Marginal effects plots with binning estimates (Figure A8)

# get coefficient vector from baseline model and rename 
m <- m1.2cl.weights[[1]]
beta <- as.vector(m$beta)
names(beta) <- paste0("b",c(1:length(coef(m))))
rownames(m$beta)
beta

# calculate marginal effects of individual-level coethnicity along the range of district-level coethnicity
mfx<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
#fill in marginal effects and standard errors
for (i in 1:nrow(mfx)) {
  a <- mfx$z[i] 
  mfx$b[i] <- deltaMethod(beta,"b1 +  a*b3",vcov.=m$clustervcv)$Estimate
  mfx$se[i] <- deltaMethod(beta,"b1 +  a*b3",vcov.=m$clustervcv)$SE
  mfx$lb[i] <- mfx$b[i]-1.96*mfx$se[i]
  mfx$ub[i] <- mfx$b[i]+1.96*mfx$se[i]
}

# calculate predictions for government co-ethnics along the range of district-level co-ethnicity
pred1<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
#fill in predictions and standard errors
for (i in 1:nrow(pred1)) {
  a <- pred1$z[i] 
  pred1$b[i] <- deltaMethod(beta,"b1 + a*b2 + a*b3",vcov.=m$clustervcv)$Estimate
  pred1$se[i] <- deltaMethod(beta,"b1 + a*b2 + a*b3",vcov.=m$clustervcv)$SE
  pred1$lb[i] <- pred1$b[i]-1.96*pred1$se[i]
  pred1$ub[i] <- pred1$b[i]+1.96*pred1$se[i]
}

# calculate predictions for government non-co-ethnics along the range of district-level co-ethnicity
pred2<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
#fill in predictions effects and standard errors
for (i in 1:nrow(pred2)) {
  a <- pred2$z[i] 
  pred2$b[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$Estimate
  pred2$se[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$SE
  pred2$lb[i] <- pred2$b[i]-1.96*pred2$se[i]
  pred2$ub[i] <- pred2$b[i]+1.96*pred2$se[i]
}

# define co-ethnic variable and put together in one dataframe
pred1$co_ethnic <- "yes"
pred2$co_ethnic <- "no"
pred <- rbind(pred1,pred2)


# prepare density of district-level co-ethnicity
dens <- density(data$epr.incl.dist, na.rm=T)
dens=data.frame(x=dens$x,y=dens$y)
# scale to ensure it fits in the bottom half of the plot
dens$scaled <- (dens$y/2.5) - 4

## Plot Figure 3
p <- ggplot(data = pred)

main  <- p + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                         fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b, fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(-0.01,1.01)) +
  labs(linetype="Government Co-Ethnic",
       title = "Ethnic Favoritism at the Individual and District Level",
       subtitle = "Linear predictions relative to government non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Model 1 in Table 1.\nDot-dashed line represents density of district co-ethnicity shares.") + 
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash") 

out <- main + theme_grey(base_size = 18) +  
  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) +
  scale_linetype_manual(values=c(1,2),name="Government Co-Ethnic") 
out 
out + ggsave(paste0(fig.path,"pred_baseline.pdf"),width=12,height=9)


### add binning estimates
## adapted from the inter.binning function from Hainmueller, Mummolo and Xu's interflex package
# set outcome, treatment, moderator, controls, fixed effects, standard errors, data, and weights
Y <- "dead"
D <- "epr.incl.ind_l1"
X <- "epr.incl.dist_l1"
Z=controls.cm
FE= c("ethnic.id","dist.round","birth.reg.year")
cl=c("ethnic.id","dist.round")
this.data.bin <- data[,c(Y,D,X,Z,FE,"cowcode")]
names(this.data.bin)
this.data.bin <- na.omit(this.data.bin)
this.data.bin$wgt <- gen_weights(this.data.bin$cowcode)

# define number of bins and observations
nbins <- 3
n <- nrow(this.data.bin)

# define cut points and split moderator in three groups
cuts.X <- c(-0.1,1/3,2/3,1.1)
groupX <- cut(this.data.bin[,X],breaks=cuts.X,labels=F)
describe(groupX)

## mid points as group median
x0<-rep(NA,nbins)
for (i in 1:nbins) x0[i]<-median(this.data.bin[which(groupX==i),X], na.rm=TRUE)
#  mid points specified
#x0 <- c(0.25,3/6,0.75)

# build ias
G<-DG<-GX<-DGX<-matrix(0,n,nbins)
for (i in 1:nbins) {
  G[which(groupX==i),i]<-1
  DG[,i]<-this.data.bin[,D]*G[,i]
  GX[,i]<-G[,i]*(this.data.bin[,X]-x0[i])
  DGX[,i]<-DG[,i]*(this.data.bin[,X]-x0[i])
}

## formula and estimation
Gs<-GXs<-DGs<-DGXs<-c()
for (i in 1:nbins)  {
  Gs<-c(Gs,paste0("G[,",i,"]"))
  GXs<-c(GXs,paste0("GX[,",i,"]"))
  DGs<-c(DGs,paste0("DG[,",i,"]"))
  DGXs<-c(DGXs,paste0("DGX[,",i,"]"))
}
Xf<-paste0(Y,"~ -1+",paste0(DGs,collapse="+"),"+",paste0(DGXs,collapse="+"),
           "+",paste0(Gs[-1],collapse="+"),"+",paste0(GXs,collapse="+"))
if (is.null(Z)==FALSE) {
  Xf<- paste0(Xf,"+",paste0(Z,collapse="+"))
}
if (is.null(FE)==FALSE) {
  Xf <- paste0(Xf, "|",paste0(FE, collapse = "+"))
  if (is.null(cl)==FALSE)  {
    Xf <- paste0(Xf, "| 0 |",paste0(cl,collapse = "+"))
  }
}
mod.Xf<-as.formula(Xf)   

mod.X <- felm(mod.Xf,data=this.data.bin,weights=this.data.bin[,"wgt"])
summary(mod.X)

# get coefficient vector from binning estimation
m <- mod.X
beta <- as.vector(m$beta)
names(beta) <- paste0("b",c(1:length(coef(m))))


# calculate predicted values for government co-ethnics 
co1 <- deltaMethod(beta,"b1",vcov.=m$clustervcv)
co2 <- deltaMethod(beta,"b2 + b7",vcov.=m$clustervcv)
co3  <- deltaMethod(beta,"b3 + b8",vcov.=m$clustervcv)

# calculate predicted values for government non-co-ethnics 
no1 <- rep(0,4)
no2 <- deltaMethod(beta,"b7",vcov.=m$clustervcv)
no3 <- deltaMethod(beta,"b8",vcov.=m$clustervcv)

# put together in one dataframe
pred_bin <- data.frame(rbind(co1,co2,co3,no1,no2,no3))
# code co-ethnicity variable
pred_bin$co_ethnic <- rep(c("yes","no"),each=3)
# slightly move evaluation points to enable plotting estimates for co-ethnics and non-co-ethnics next to each other
pred_bin$z <- c(x0+0.01,x0-0.01)
# rename variables
names(pred_bin) <- c("beta","se","lb","ub","co_ethnic","z")

# Specify positions of text labels in the plot
eval.p <- data.frame(x=x0,y=c(-2.6,-3,-0.15),lab=paste(round(x0,2)))
groups.p <- data.frame(x=c(1/6,3/6,5/6),y=-3.5,lab=c("Low","Medium","High"))



# Prepare Stacked Histogram
yrange <- na.omit(c(pred$lb,pred$ub))
maxdiff<-(max(yrange)-min(yrange))
pos<-max(yrange)-maxdiff/20

hist.out<-hist(this.data[,"epr.incl.dist_l1"],this.data[,"weights"],
               breaks=80,plot=FALSE)

n.hist<-length(hist.out$mids)
dist<-hist.out$mids[2]-hist.out$mids[1]
hist.max<-max(hist.out$counts)
## count the number of treated
count1<-rep(0,n.hist)
treat<-which(this.data[,"epr.incl.ind_l1"]==max(this.data[,"epr.incl.ind_l1"]))
for (i in 1:n.hist) {
  count1[i]<-sum(this.data[treat,"epr.incl.dist_l1"]>=hist.out$breaks[i] &
                   this.data[treat,"epr.incl.dist_l1"]<hist.out$breaks[(i+1)])
}
count1[n.hist]<-sum(this.data[treat,"epr.incl.dist_l1"]>=hist.out$breaks[n.hist] &
                      this.data[treat,"epr.incl.dist_l1"]<=hist.out$breaks[n.hist+1])
## put in a data frame
histX<-data.frame(ymin=rep(min(yrange)-maxdiff/1.2,n.hist),
                  ymax=hist.out$counts/hist.max*maxdiff/1.2+min(yrange)-maxdiff/1.2,
                  xmin=hist.out$mids-dist/2,
                  xmax=hist.out$mids+dist/2,
                  count1=count1/hist.max*maxdiff/1.2+min(yrange)-maxdiff/1.2)


######### prepare prediction plot and add binning estimates (Figure A7)
main  <- p + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                         fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b, fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(-0.01,1.01)) +
  labs(linetype="Government Co-Ethnic",
       title = "Ethnic Favoritism at the Individual and District Level",
       subtitle = "Linear predictions relative to government non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Model 1 in Table 1 and Binning Estimates.\nStacked histogram shows distribution of district co-ethnicity shares.") + 
  geom_rect(data=histX,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            colour="gray50",alpha=0,size=0.5) + # control
  geom_rect(data=histX,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=count1),
            fill="black",colour="grey50",alpha=1,size=0.5)

out <- main + theme_grey(base_size = 18) +  
  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) +
  scale_linetype_manual(values=c(1,2),name="Government Co-Ethnic") 

# add binning estimates
out2 <- out + geom_point(data=pred_bin,aes(x=z,y=beta,group=co_ethnic,shape=co_ethnic),size=4) +
  geom_errorbar(data=pred_bin,aes(x=z,y=beta,ymax=ub,ymin=lb),size=1,width=0) +
  scale_shape_manual(values=c(16,17),name="Government Co-Ethnic") +
  geom_text(data=eval.p,aes(x=x,y=y,label=lab),size=5) +
  geom_vline(xintercept = c(0.33,0.67),linetype="dotted",size=0.7) +
  geom_text(data=groups.p,aes(x=x,y=y,label=lab),size=7.5) 
out2
out2 + ggsave(paste0(fig.path,"pred_baseline_binning.pdf"),width=12,height=9)

####################################################
## ggplot marginal effects (Figure A6)
p <- ggplot(data = mfx)
main  <- p + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                         fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b, fill=NULL,color=NULL),linetype="dashed") + 
  scale_y_continuous(name="MFX of Individual Co-Ethnicity") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(-0.01,1.01)) +
  theme(legend.position = "bottom") + 
  labs(
    title = "Individual-level Co-Ethnicity Advantage within Districts",
    subtitle = "Marginal Effect and 95% Confidence Intervals.",
    caption = "Based on Model 1 in Table 1.\nDot-dashed line represents density of district co-ethnicity shares.") +
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash") 

out <- main + theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 
out 
out + ggsave(paste0(fig.path,"mfx_baseline.pdf"),width=12,height=9)

## add binning estimates
mfx1 <- deltaMethod(beta,"b1",vcov.=m$clustervcv)
mfx2 <- deltaMethod(beta,"b2",vcov.=m$clustervcv)
mfx3  <- deltaMethod(beta,"b3",vcov.=m$clustervcv)
mfx_bin <- rbind(mfx1,mfx2,mfx3)
mfx_bin$z <- x0
names(mfx_bin) <- c("beta","se","lb","ub","z")

# set label positions 
eval.p.mfx <- data.frame(x=x0,y=c(-2.9,-2.2,-1.5),lab=paste(round(x0,2)))

# prepare marginal effects plot and add binning estimates
p <- ggplot(data = mfx)
main  <- p + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                         fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b, fill=NULL,color=NULL),linetype="dashed") + 
  scale_y_continuous(name="MFX of Individual Co-Ethnicity") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(-0.01,1.01)) +
  theme(legend.position = "bottom") + 
  labs(
    title = "Individual-level Co-Ethnicity Advantage within Districts",
    subtitle = "Marginal Effect and 95% Confidence Intervals.",
    caption = "Based on Model 1 in Table 1 and Binning Estimates.\nStacked histogram shows distribution of district co-ethnicity shares.") +
  geom_rect(data=histX,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            colour="gray50",alpha=0,size=0.5) + # control
  geom_rect(data=histX,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=count1),
            fill="black",colour="grey50",alpha=1,size=0.5)

out <- main + theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 
out 

# add binning estimates
out2 <- out +
  geom_point(data=mfx_bin,aes(x=z,y=beta),size=4,shape=17) +
  geom_errorbar(data=mfx_bin,aes(x=z,y=beta,ymax=ub,ymin=lb),size=1,width=0) +
  geom_text(data=eval.p.mfx,aes(x=x,y=y,label=lab),size=5)  +
  geom_hline(yintercept = 0,linetype="dotted",size=0.7) +
  geom_vline(xintercept = c(0.33,0.67),linetype="dotted",size=0.7) +
  geom_text(data=groups.p,aes(x=x,y=y,label=lab),size=7.5) 
out2
out2 +  ggsave(paste0(fig.path,"mfx_baseline_binning.pdf"),width=12,height=9)




