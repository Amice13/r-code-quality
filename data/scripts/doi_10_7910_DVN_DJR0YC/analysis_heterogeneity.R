################################
# ANALYSIS: HETEROGENITIES
################################


##################### ##################### ##################### ##################### 
############## Heterogeneity Analysis: Triple Interaction Models with Binary Moderators


############# ############# ############# ############# ############# 
#### Heterogeneity: VDEM High vs Low
form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.dist_l1*vdem_polyarchy_index_high_l1 +",controls.str,
                  "| factor(ethnic.id) + factor(dist.round)  + factor(birth.reg.year) | 0 | ethnic.id + dist.round")
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)

m.vdem.high <- felm(as.formula(form.str),data = this.data, weights = this.data$weights)
summary(m.vdem.high)

m <- m.vdem.high
beta <- as.vector(m$beta)
names(beta) <- paste0("b",c(1:length(coef(m))))
rownames(m$beta)
beta


# fill in predictions and standard errors for co-ethnics in non-democ regimes
pred1<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred1)) {
  a <- pred1$z[i] 
  pred1$b[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10",vcov.=m$clustervcv)$Estimate
  pred1$se[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10",vcov.=m$clustervcv)$SE
  pred1$lb[i] <- pred1$b[i]-1.96*pred1$se[i]
  pred1$ub[i] <- pred1$b[i]+1.96*pred1$se[i]
}

# fill in predictions and standard errors for non-co-ethnics in non-democ regimes
pred2<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred2)) {
  a <- pred2$z[i] 
  pred2$b[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$Estimate
  pred2$se[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$SE
  pred2$lb[i] <- pred2$b[i]-1.96*pred2$se[i]
  pred2$ub[i] <- pred2$b[i]+1.96*pred2$se[i]
}

# put together in one dataset for plot
pred1$co_ethnic <- "yes"
pred2$co_ethnic <- "no"
pred_vdem_no <- rbind(pred1,pred2)


# fill in predictions and standard errors for co-ethnics in democ regimes
pred3<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred3)) {
  a <- pred3$z[i] 
  pred3$b[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10 + b11 + a*b12 + a*b13",vcov.=m$clustervcv)$Estimate
  pred3$se[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10 + b11 + a*b12 + a*b13",vcov.=m$clustervcv)$SE
  pred3$lb[i] <- pred3$b[i]-1.96*pred3$se[i]
  pred3$ub[i] <- pred3$b[i]+1.96*pred3$se[i]
}

# fill in predictions and standard errors for non-co-ethnics in democ regimes
pred4<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred4)) {
  a <- pred4$z[i] 
  pred4$b[i] <- deltaMethod(beta,"a*b2 + a*b12",vcov.=m$clustervcv)$Estimate
  pred4$se[i] <- deltaMethod(beta,"a*b2 + a*b12",vcov.=m$clustervcv)$SE
  pred4$lb[i] <- pred4$b[i]-1.96*pred4$se[i]
  pred4$ub[i] <- pred4$b[i]+1.96*pred4$se[i]
}

pred3$co_ethnic <- "yes"
pred4$co_ethnic <- "no"
pred_vdem_yes <- rbind(pred3,pred4)


## differences democ-non.democ for co-ethnics and non-co-ethnics
# fill in estimates and standard errors for co-ethnic diff
pred5<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred5)) {
  a <- pred5$z[i] 
  pred5$b[i] <- deltaMethod(beta,"b11 + a*b12 + a*b13",vcov.=m$clustervcv)$Estimate
  pred5$se[i] <- deltaMethod(beta,"b11 + a*b12 + a*b13",vcov.=m$clustervcv)$SE
  pred5$lb[i] <- pred5$b[i]-1.96*pred5$se[i]
  pred5$ub[i] <- pred5$b[i]+1.96*pred5$se[i]
}

# fill in estimates and standard errors for non-co-ethnic diff
pred6<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred6)) {
  a <- pred6$z[i] 
  pred6$b[i] <- deltaMethod(beta,"a*b12",vcov.=m$clustervcv)$Estimate
  pred6$se[i] <- deltaMethod(beta,"a*b12",vcov.=m$clustervcv)$SE
  pred6$lb[i] <- pred6$b[i]-1.96*pred6$se[i]
  pred6$ub[i] <- pred6$b[i]+1.96*pred6$se[i]
}

pred5$co_ethnic <- "yes"
pred6$co_ethnic <- "no"
pred_vdem_diff<- rbind(pred5,pred6)



##### real mfx in non-democ
mfx1 <-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(mfx1)) {
  a <- mfx1$z[i] 
  mfx1$b[i] <- deltaMethod(beta,"b1 + a*b10 ",vcov.=m$clustervcv)$Estimate
  mfx1$se[i] <- deltaMethod(beta,"b1 + a*b10",vcov.=m$clustervcv)$SE
  mfx1$lb[i] <- mfx1$b[i]-1.96*mfx1$se[i]
  mfx1$ub[i] <- mfx1$b[i]+1.96*mfx1$se[i]
}

##### real mfx in democ
mfx2 <-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(mfx2)) {
  a <- mfx2$z[i] 
  mfx2$b[i] <- deltaMethod(beta,"b1 + a*b10 + b11 + a*b13",vcov.=m$clustervcv)$Estimate
  mfx2$se[i] <- deltaMethod(beta,"b1 + a*b10 + b11 + a*b13",vcov.=m$clustervcv)$SE
  mfx2$lb[i] <- mfx2$b[i]-1.96*mfx2$se[i]
  mfx2$ub[i] <- mfx2$b[i]+1.96*mfx2$se[i]
}


#### is the difference between mfx at highest and lowest district proportion significant (democ)? 
dif1<-data.frame(b=NA, se=NA)
dif1$b <- deltaMethod(beta,"(b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13)",vcov.=m$clustervcv)$Estimate
dif1$se <- deltaMethod(beta,"(b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13)",vcov.=m$clustervcv)$SE
dif1$lb <- dif1$b-1.96*dif1$se
dif1$ub <- dif1$b+1.96*dif1$se

#### is the difference between mfx at highest and lowest district proportion significant (nondemoc)? 
dif2<-data.frame(b=NA, se=NA)
dif2$b <- deltaMethod(beta,"(b1  + 1*b10)-(b1  + 0*b10)",vcov.=m$clustervcv)$Estimate
dif2$se <- deltaMethod(beta,"(b1  + 1*b10)-(b1  + 0*b10)",vcov.=m$clustervcv)$SE
dif2$lb <- dif2$b-1.96*dif2$se
dif2$ub <- dif2$b+1.96*dif2$se

#### is the moderating effect of district proportion sig different between regime types? 
dif3<-data.frame(b=NA, se=NA)
dif3$b <- deltaMethod(beta,"((b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13))-((b1  + 1*b10)-(b1  + 0*b10))",vcov.=m$clustervcv)$Estimate
dif3$se <- deltaMethod(beta,"((b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13))-((b1  + 1*b10)-(b1  + 0*b10))",vcov.=m$clustervcv)$SE
dif3$lb <- dif3$b-1.96*dif3$se
dif3$ub <- dif3$b+1.96*dif3$se



##### ggplot non-democ
dens <- density(data$epr.incl.dist[data$vdem_polyarchy_index_high_l1==0], na.rm=T)
dens=data.frame(x=dens$x,y=dens$y)
dens$scaled <- (dens$y/2.5) - 5
describe(dens)

plot_vdem_no  <- ggplot(data = pred_vdem_no) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                           fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline", limits=c(-5,0.5)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(0,1)) +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Predictions for VDEM Polyarchy < Median",
       subtitle = "Linear predictions relative to government non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Triple Interaction Model.\nDot-dashed line represents density of district co-ethnicity shares.") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line"))  + 
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash")  

plot_vdem_no
# save
plot_vdem_no + ggsave(paste0(fig.path,"pred_3IA_vdem_low.pdf"),width=12,height=9)


##### ggplot democ
dens <- density(data$epr.incl.dist[data$vdem_polyarchy_index_high_l1==1], na.rm=T)
dens=data.frame(x=dens$x,y=dens$y)
dens$scaled <- (dens$y/2.5) - 5
describe(dens)
max(pred_vdem_yes$ub)

plot_v_dem_yes  <- ggplot(data = pred_vdem_yes) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                              fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline", limits=c(-5,0.52)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(0,1)) +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Predictions for VDEM Polyarchy > Median",
       subtitle = "Linear predictions relative to government non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) + 
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash")  

plot_v_dem_yes
# save
plot_v_dem_yes + ggsave(paste0(fig.path,"pred_3IA_vdem_high.pdf"),width=12,height=9)





##### ggplot diff co-ethnics
plot_vdem_diff  <- ggplot(data = pred5) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                      fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=2) + 
  scale_y_continuous(name="Predicted Difference in Infant Mortality", limits=c(-0.5,4)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Differences VDEM (High) - VDEM (Low) (Co-Ethnics)",
       subtitle = "Predictions and 95% Confidence Intervals.",
       caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_vdem_diff
# save
plot_vdem_diff + ggsave(paste0(fig.path,"pred_3IA_vdem_diff_co.pdf"),width=12,height=9)

##### ggplot diff non-co-ethnics
plot_vdem_diff  <- ggplot(data = pred6) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                      fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="Predicted Difference in Infant Mortality", limits=c(-0.5,4)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Differences VDEM (High) - VDEM (Low) (Non-Co-Ethnics)",
       subtitle = "Predictions and 95% Confidence Intervals.",
       caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_vdem_diff
# save
plot_vdem_diff + ggsave(paste0(fig.path,"pred_3IA_vdem_diff_nonco.pdf"),width=12,height=9)



##### mfx: non-democ
plot_mfx_vdem_low <- ggplot(data = mfx1) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                       fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="MFX of Individual Co-Ethnicity ") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(
    title = "Marginal Effect of Individual-Level Co-Ethnicity (VDEM < Median)",
    subtitle = "Predictions and 95% Confidence Intervals.",
    caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_mfx_vdem_low
# save
plot_mfx_vdem_low + ggsave(paste0(fig.path,"mfx_3IA_vdem_low.pdf"),width=12,height=9)


##### mfx: democ
plot_mfx_vdem_high <- ggplot(data = mfx2) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                        fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="Marginal Effect of Individual Co-Ethnicity") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(
    title = "Marginal Effect of Individual-Level Co-Ethnicity (VDEM > Median)",
    subtitle = "Predictions and 95% Confidence Intervals.",
    caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_mfx_vdem_high
# save
plot_mfx_vdem_high + ggsave(paste0(fig.path,"mfx_3IA_vdem_high.pdf"),width=12,height=9)









############# ############# ############# ############# ############# 
############ Polity IV High vs Low (Median Split at -1)
form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.dist_l1*polity_iv_high_l1 +",controls.str,
                  "| factor(ethnic.id) + factor(dist.round)  + factor(birth.reg.year) | 0 | ethnic.id + dist.round")
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)

m.polity.high <- felm(as.formula(form.str),data = this.data, weights = this.data$weights)
summary(m.polity.high)

m <- m.polity.high
beta <- as.vector(m$beta)
names(beta) <- paste0("b",c(1:length(coef(m))))
rownames(m$beta)
beta


# fill in predictions and standard errors for co-ethnics in non_democ regimes
pred1<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred1)) {
  a <- pred1$z[i] 
  pred1$b[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10",vcov.=m$clustervcv)$Estimate
  pred1$se[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10",vcov.=m$clustervcv)$SE
  pred1$lb[i] <- pred1$b[i]-1.96*pred1$se[i]
  pred1$ub[i] <- pred1$b[i]+1.96*pred1$se[i]
}

# fill in predictions and standard errors for non-co-ethnics in non-democ regimes
pred2<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
#fill in maerginal effects and standard errors
for (i in 1:nrow(pred2)) {
  a <- pred2$z[i] 
  pred2$b[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$Estimate
  pred2$se[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$SE
  pred2$lb[i] <- pred2$b[i]-1.96*pred2$se[i]
  pred2$ub[i] <- pred2$b[i]+1.96*pred2$se[i]
}

# put together in one dataset for plot
pred1$co_ethnic <- "yes"
pred2$co_ethnic <- "no"
pred_polity_no <- rbind(pred1,pred2)


# fill in predictions and standard errors for co-ethnics in democ regimes
pred3<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred3)) {
  a <- pred3$z[i] 
  pred3$b[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10 + b11 + a*b12 + a*b13",vcov.=m$clustervcv)$Estimate
  pred3$se[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10 + b11 + a*b12 + a*b13",vcov.=m$clustervcv)$SE
  pred3$lb[i] <- pred3$b[i]-1.96*pred3$se[i]
  pred3$ub[i] <- pred3$b[i]+1.96*pred3$se[i]
}

# fill in predictions and standard errors for non-co-ethnics in democ regimes
pred4<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred4)) {
  a <- pred4$z[i] 
  pred4$b[i] <- deltaMethod(beta,"a*b2 + a*b12",vcov.=m$clustervcv)$Estimate
  pred4$se[i] <- deltaMethod(beta,"a*b2 + a*b12",vcov.=m$clustervcv)$SE
  pred4$lb[i] <- pred4$b[i]-1.96*pred4$se[i]
  pred4$ub[i] <- pred4$b[i]+1.96*pred4$se[i]
}

pred3$co_ethnic <- "yes"
pred4$co_ethnic <- "no"
pred_polity_yes <- rbind(pred3,pred4)


## differences democ-non.democ for co-ethnics and non-co-ethnics
# fill in estimates and standard errors for co-ethnic diff
pred5<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred5)) {
  a <- pred5$z[i] 
  pred5$b[i] <- deltaMethod(beta,"b11 + a*b12 + a*b13",vcov.=m$clustervcv)$Estimate
  pred5$se[i] <- deltaMethod(beta,"b11 + a*b12 + a*b13",vcov.=m$clustervcv)$SE
  pred5$lb[i] <- pred5$b[i]-1.96*pred5$se[i]
  pred5$ub[i] <- pred5$b[i]+1.96*pred5$se[i]
}

# fill in estimates and standard errors for non-co-ethnic diff
pred6<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred6)) {
  a <- pred6$z[i] 
  pred6$b[i] <- deltaMethod(beta,"a*b12",vcov.=m$clustervcv)$Estimate
  pred6$se[i] <- deltaMethod(beta,"a*b12",vcov.=m$clustervcv)$SE
  pred6$lb[i] <- pred6$b[i]-1.96*pred6$se[i]
  pred6$ub[i] <- pred6$b[i]+1.96*pred6$se[i]
}

pred5$co_ethnic <- "yes"
pred6$co_ethnic <- "no"
pred_polity_diff<- rbind(pred5,pred6)


##### real mfx in non-democ
mfx1 <-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(mfx1)) {
  a <- mfx1$z[i] 
  mfx1$b[i] <- deltaMethod(beta,"b1 + a*b10 ",vcov.=m$clustervcv)$Estimate
  mfx1$se[i] <- deltaMethod(beta,"b1 + a*b10",vcov.=m$clustervcv)$SE
  mfx1$lb[i] <- mfx1$b[i]-1.96*mfx1$se[i]
  mfx1$ub[i] <- mfx1$b[i]+1.96*mfx1$se[i]
}

##### real mfx in democ
mfx2 <-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(mfx2)) {
  a <- mfx2$z[i] 
  mfx2$b[i] <- deltaMethod(beta,"b1 + a*b10 + b11 + a*b13",vcov.=m$clustervcv)$Estimate
  mfx2$se[i] <- deltaMethod(beta,"b1 + a*b10 + b11 + a*b13",vcov.=m$clustervcv)$SE
  mfx2$lb[i] <- mfx2$b[i]-1.96*mfx2$se[i]
  mfx2$ub[i] <- mfx2$b[i]+1.96*mfx2$se[i]
}

#### is the difference between mfx at highest and lowest district proportion significant (democ)? 
dif1<-data.frame(b=NA, se=NA)
dif1$b <- deltaMethod(beta,"(b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13)",vcov.=m$clustervcv)$Estimate
dif1$se <- deltaMethod(beta,"(b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13)",vcov.=m$clustervcv)$SE
dif1$lb <- dif1$b-1.96*dif1$se
dif1$ub <- dif1$b+1.96*dif1$se

#### is the difference between mfx at highest and lowest district proportion significant (nondemoc)? 
dif2<-data.frame(b=NA, se=NA)
dif2$b <- deltaMethod(beta,"(b1  + 1*b10)-(b1  + 0*b10)",vcov.=m$clustervcv)$Estimate
dif2$se <- deltaMethod(beta,"(b1  + 1*b10)-(b1  + 0*b10)",vcov.=m$clustervcv)$SE
dif2$lb <- dif2$b-1.96*dif2$se
dif2$ub <- dif2$b+1.96*dif2$se

#### is the moderating effect of district proportion sig different between regime types? 
dif3<-data.frame(b=NA, se=NA)
dif3$b <- deltaMethod(beta,"((b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13))-((b1  + 1*b10)-(b1  + 0*b10))",vcov.=m$clustervcv)$Estimate
dif3$se <- deltaMethod(beta,"((b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13))-((b1  + 1*b10)-(b1  + 0*b10))",vcov.=m$clustervcv)$SE
dif3$lb <- dif3$b-1.96*dif3$se
dif3$ub <- dif3$b+1.96*dif3$se

##### ggplot non-democ
dens <- density(data$epr.incl.dist[data$polity_iv_high_l1==0], na.rm=T)
dens=data.frame(x=dens$x,y=dens$y)
dens$scaled <- (dens$y/2.5) - 4.5
describe(dens)


plot_polity_no  <- ggplot(data = pred_polity_no) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                               fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline", limits=c(-4.5,0.5)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(0,1)) +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Predictions for Polity IV < Median",
       subtitle = "Linear predictions relative to government non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Triple Interaction Model") +
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash")  +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_polity_no
# save
plot_polity_no + ggsave(paste0(fig.path,"pred_3IA_polity_low.pdf"),width=12,height=9)


##### ggplot democ
dens <- density(data$epr.incl.dist[data$polity_iv_high_l1==1], na.rm=T)
dens=data.frame(x=dens$x,y=dens$y)
dens$scaled <- (dens$y/2.5) - 4.5
describe(dens)


plot_polity_yes  <- ggplot(data = pred_polity_yes) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                                 fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline", limits=c(-4.5,0.5)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(0,1)) +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Predictions for Polity IV > Median",
       subtitle = "Linear predictions relative to government non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Triple Interaction Model") +
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash")  +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_polity_yes

# save
plot_polity_yes + ggsave(paste0(fig.path,"pred_3IA_polity_high.pdf"),width=12,height=9)





##### ggplot diff co-ethnics
plot_polity_diff  <- ggplot(data = pred5) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                        fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=2) + 
  scale_y_continuous(name="Predicted Difference in Infant Mortality", limits=c(-1,3.5)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Differences Polity (High) - Polity (Low) (Co-Ethnics)",
       subtitle = "Predictions and 95% Confidence Intervals.",
       caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_polity_diff
# save
plot_polity_diff + ggsave(paste0(fig.path,"pred_3IA_polity_diff_co.pdf"),width=12,height=9)

##### ggplot diff non-co-ethnics
plot_polity_diff  <- ggplot(data = pred6) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                        fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="Predicted Difference in Infant Mortality", limits=c(-1,3.7)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Differences Polity (High) - Polity (Low) (Non-Co-Ethnics)",
       subtitle = "Predictions and 95% Confidence Intervals.",
       caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_polity_diff
# save
plot_polity_diff + ggsave(paste0(fig.path,"pred_3IA_polity_diff_nonco.pdf"),width=12,height=9)



##### mfx: non-democ
plot_mfx_polity_low <- ggplot(data = mfx1) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                         fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="MFX of Individual Co-Ethnicity ") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(
    title = "Marginal Effect of Individual-Level Co-Ethnicity (Polity IV < Median)",
    subtitle = "Predictions and 95% Confidence Intervals.",
    caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_mfx_polity_low
# save
plot_mfx_polity_low + ggsave(paste0(fig.path,"mfx_3IA_polity_low.pdf"),width=12,height=9)


##### mfx: democ
plot_mfx_polity_high <- ggplot(data = mfx2) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                          fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="Marginal Effect of Individual Co-Ethnicity") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(
    title = "Marginal Effect of Individual-Level Co-Ethnicity (Polity IV > Median)",
    subtitle = "Predictions and 95% Confidence Intervals.",
    caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_mfx_polity_high
# save
plot_mfx_polity_high + ggsave(paste0(fig.path,"mfx_3IA_polity_high.pdf"),width=12,height=9)





############# ############# ############# ############# ############# 
############ Majoritarian vs PR and Mixed systems
form.str <- paste("dead ~ epr.incl.ind_l1*epr.incl.dist_l1*fptp_most_l1 +",controls.str,
                  "| factor(ethnic.id) + factor(dist.round) + factor(birth.reg.year) | 0 | ethnic.id + dist.round")
this.data <- data[,unlist(lapply(colnames(data), 
                                 function(x){grepl(x,paste0(form.str,"cowcode"))})) &
                    !colnames(data) %in% c("age")] 
this.data <- na.omit(this.data)    
this.data$weights <- gen_weights(this.data$cowcode)

m.fptp <- felm(as.formula(form.str),data = this.data, weights = this.data$weights)
summary(m.fptp)

m <- m.fptp
beta <- as.vector(m$beta)
names(beta) <- paste0("b",c(1:length(coef(m))))
rownames(m$beta)
beta


# fill in predictions and standard errors for co-ethnics in PR regimes
pred1<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred1)) {
  a <- pred1$z[i] 
  pred1$b[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10",vcov.=m$clustervcv)$Estimate
  pred1$se[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10",vcov.=m$clustervcv)$SE
  pred1$lb[i] <- pred1$b[i]-1.96*pred1$se[i]
  pred1$ub[i] <- pred1$b[i]+1.96*pred1$se[i]
}

# fill in predictions and standard errors for non-co-ethnics in PR regimes
pred2<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred2)) {
  a <- pred2$z[i] 
  pred2$b[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$Estimate
  pred2$se[i] <- deltaMethod(beta,"a*b2",vcov.=m$clustervcv)$SE
  pred2$lb[i] <- pred2$b[i]-1.96*pred2$se[i]
  pred2$ub[i] <- pred2$b[i]+1.96*pred2$se[i]
}

# put together in one dataset for plot
pred1$co_ethnic <- "yes"
pred2$co_ethnic <- "no"
pred_fptp_no <- rbind(pred1,pred2)


# fill in predictions and standard errors for co-ethnics in FPTP regimes
pred3<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred3)) {
  a <- pred3$z[i] 
  pred3$b[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10 + b11 + a*b12 + a*b13",vcov.=m$clustervcv)$Estimate
  pred3$se[i] <- deltaMethod(beta,"b1 + a*b2 + a*b10 + b11 + a*b12 + a*b13",vcov.=m$clustervcv)$SE
  pred3$lb[i] <- pred3$b[i]-1.96*pred3$se[i]
  pred3$ub[i] <- pred3$b[i]+1.96*pred3$se[i]
}

# fill in predictions and standard errors for non-co-ethnics in FPTP regimes
pred4<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred4)) {
  a <- pred4$z[i] 
  pred4$b[i] <- deltaMethod(beta,"a*b2 + a*b12",vcov.=m$clustervcv)$Estimate
  pred4$se[i] <- deltaMethod(beta,"a*b2 + a*b12",vcov.=m$clustervcv)$SE
  pred4$lb[i] <- pred4$b[i]-1.96*pred4$se[i]
  pred4$ub[i] <- pred4$b[i]+1.96*pred4$se[i]
}

pred3$co_ethnic <- "yes"
pred4$co_ethnic <- "no"
pred_fptp_yes <- rbind(pred3,pred4)


## differences fptp-PR for co-ethnics and non-co-ethnics
# fill in estimates and standard errors for co-ethnic diff
pred5<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred5)) {
  a <- pred5$z[i] 
  pred5$b[i] <- deltaMethod(beta,"b11 + a*b12 + a*b13",vcov.=m$clustervcv)$Estimate
  pred5$se[i] <- deltaMethod(beta,"b11 + a*b12 + a*b13",vcov.=m$clustervcv)$SE
  pred5$lb[i] <- pred5$b[i]-1.96*pred5$se[i]
  pred5$ub[i] <- pred5$b[i]+1.96*pred5$se[i]
}

# fill in estimates and standard errors for non-co-ethnic diff
pred6<-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(pred6)) {
  a <- pred6$z[i] 
  pred6$b[i] <- deltaMethod(beta,"a*b12",vcov.=m$clustervcv)$Estimate
  pred6$se[i] <- deltaMethod(beta,"a*b12",vcov.=m$clustervcv)$SE
  pred6$lb[i] <- pred6$b[i]-1.96*pred6$se[i]
  pred6$ub[i] <- pred6$b[i]+1.96*pred6$se[i]
}

pred5$co_ethnic <- "yes"
pred6$co_ethnic <- "no"
pred_fptp_diff<- rbind(pred5,pred6)


##### real mfx in PR
mfx1 <-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(mfx1)) {
  a <- mfx1$z[i] 
  mfx1$b[i] <- deltaMethod(beta,"b1 + a*b10 ",vcov.=m$clustervcv)$Estimate
  mfx1$se[i] <- deltaMethod(beta,"b1 + a*b10",vcov.=m$clustervcv)$SE
  mfx1$lb[i] <- mfx1$b[i]-1.96*mfx1$se[i]
  mfx1$ub[i] <- mfx1$b[i]+1.96*mfx1$se[i]
}

##### real mfx in FPTP
mfx2 <-data.frame(z=seq(0,1,0.05),b=NA, se=NA)
for (i in 1:nrow(mfx2)) {
  a <- mfx2$z[i] 
  mfx2$b[i] <- deltaMethod(beta,"b1 + a*b10 + b11 + a*b13",vcov.=m$clustervcv)$Estimate
  mfx2$se[i] <- deltaMethod(beta,"b1 + a*b10 + b11 + a*b13",vcov.=m$clustervcv)$SE
  mfx2$lb[i] <- mfx2$b[i]-1.96*mfx2$se[i]
  mfx2$ub[i] <- mfx2$b[i]+1.96*mfx2$se[i]
}

#### is the difference between mfx at highest and lowest district proportion significant (maj)? 
dif1<-data.frame(b=NA, se=NA)
dif1$b <- deltaMethod(beta,"(b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13)",vcov.=m$clustervcv)$Estimate
dif1$se <- deltaMethod(beta,"(b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13)",vcov.=m$clustervcv)$SE
dif1$lb <- dif1$b-1.96*dif1$se
dif1$ub <- dif1$b+1.96*dif1$se

#### is the difference between mfx at highest and lowest district proportion significant (nonmaj)? 
dif2<-data.frame(b=NA, se=NA)
dif2$b <- deltaMethod(beta,"(b1  + 1*b10)-(b1  + 0*b10)",vcov.=m$clustervcv)$Estimate
dif2$se <- deltaMethod(beta,"(b1  + 1*b10)-(b1  + 0*b10)",vcov.=m$clustervcv)$SE
dif2$lb <- dif2$b-1.96*dif2$se
dif2$ub <- dif2$b+1.96*dif2$se

#### is the moderating effect of district proportion sig different between systems? 

dif3<-data.frame(b=NA, se=NA)
dif3$b <- deltaMethod(beta,"((b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13))-((b1  + 1*b10)-(b1  + 0*b10))",vcov.=m$clustervcv)$Estimate
dif3$se <- deltaMethod(beta,"((b1 + 1*b10 + b11 + 1*b13)-(b1 + 0*b10 + b11 + 0*b13))-((b1  + 1*b10)-(b1  + 0*b10))",vcov.=m$clustervcv)$SE
dif3$lb <- dif3$b-1.96*dif3$se
dif3$ub <- dif3$b+1.96*dif3$se

##### ggplot PR
dens <- density(data$epr.incl.dist[data$fptp_most_l1==0], na.rm=T)
dens=data.frame(x=dens$x,y=dens$y)
dens$scaled <- (dens$y/2.5) - 6.1
describe(dens)


plot_fptp_no  <- ggplot(data = pred_fptp_no) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                           fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline", limits=c(-6.1,2.2)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(0,1)) +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Predictions for Non-FPTP Electoral Systems",
       subtitle = "Linear predictions relative to government non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Triple Interaction Model") +
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash")  +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_fptp_no
# save
plot_fptp_no + ggsave(paste0(fig.path,"pred_3IA_fptp_no.pdf"),width=12,height=9)


##### ggplot FPTP
dens <- density(data$epr.incl.dist[data$fptp_most_l1==1], na.rm=T)
dens=data.frame(x=dens$x,y=dens$y)
dens$scaled <- (dens$y/2.5) - 6.1



plot_fptp_yes  <- ggplot(data = pred_fptp_yes) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                             fill=NULL,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL,group=co_ethnic, linetype=co_ethnic)) + 
  scale_y_continuous(name="Predicted Infant Mortality Compared to Baseline", limits=c(-6.1,2)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics", limits=c(0,1)) +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Predictions for FPTP Electoral Systems",
       subtitle = "Linear predictions relative to government non-co-ethnic in entirely non-co-ethnic district.",
       caption = "Based on Triple Interaction Model") +
  geom_line(data = dens, aes(x = x, y = scaled), linetype="dotdash")  +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_fptp_yes

# save
plot_fptp_yes + ggsave(paste0(fig.path,"pred_3IA_fptp_yes.pdf"),width=12,height=9)



##### ggplot diff co-ethnics
plot_fptp_diff  <- ggplot(data = pred5) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                      fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=2) + 
  scale_y_continuous(name="Predicted Difference in Infant Mortality", limits=c(-4.8,4.5)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Differences FPTP - Non-FPTP (Co-Ethnics)",
       subtitle = "Predictions and 95% Confidence Intervals.",
       caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_fptp_diff
# save
plot_fptp_diff + ggsave(paste0(fig.path,"pred_3IA_fptp_diff_co.pdf"),width=12,height=9)

##### ggplot diff non-co-ethnics
plot_fptp_diff  <- ggplot(data = pred6) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                      fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="Predicted Difference in Infant Mortality", limits=c(-4.9,4.5)) +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(linetype="Government Co-Ethnic",
       title = "Differences FPTP - Non-FPTP (Non-Co-Ethnics)",
       subtitle = "Predictions and 95% Confidence Intervals.",
       caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_fptp_diff
# save
plot_fptp_diff + ggsave(paste0(fig.path,"pred_3IA_fptp_diff_nonco.pdf"),width=12,height=9)


##### mfx: PR
plot_mfx_pr <- ggplot(data = mfx1) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                 fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="MFX of Individual Co-Ethnicity ") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(
    title = "Marginal Effect of Individual-Level Co-Ethnicity (Mainly PR)",
    subtitle = "Predictions and 95% Confidence Intervals.",
    caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_mfx_pr
# save
plot_mfx_pr + ggsave(paste0(fig.path,"mfx_3IA_pr.pdf"),width=12,height=9)


##### mfx: mostly FPTP
plot_mfx_fptp <- ggplot(data = mfx2) + geom_ribbon(alpha=0.25, aes(ymin=lb, ymax=ub, x = z,
                                                                   fill=NULL,color=NULL)) + 
  geom_line(mapping = aes(x=z ,y=b,color=NULL),linetype=1) + 
  scale_y_continuous(name="Marginal Effect of Individual Co-Ethnicity") +
  scale_x_continuous(name="District-level Share of Government Co-Ethnics") +
  theme(legend.position = "bottom") + 
  labs(
    title = "Marginal Effect of Individual-Level Co-Ethnicity (Mainly FPTP)",
    subtitle = "Predictions and 95% Confidence Intervals.",
    caption = "Based on Triple Interaction Model") +
  theme_grey(base_size = 18) +  theme(legend.position = "bottom", legend.key.size = unit(2.5,"line")) 

plot_mfx_fptp
# save
plot_mfx_fptp + ggsave(paste0(fig.path,"mfx_3IA_fptp.pdf"),width=12,height=9)




####################################################################
####################################################################
################# TABLE ###################################################


# Interaction Table
m.list <- list(m.polity.high,m.vdem.high,m.fptp)
add.lines <- list(latex.ethnic.id.fe(c("yes","yes","yes")), 
                  latex.dist.fe(c("yes","yes","yes")),
                  latex.reg.year.fe(c("yes","yes","yes")),
                  latex.controls(c("yes","yes","yes")))

keep.lines <- c("epr.incl.ind_l1","epr.incl.dist_l1","epr.incl.ind_l1:epr.incl.dist_l1",
                "epr.incl.ind_l1:polity_iv_high_l1", "epr.incl.dist_l1:polity_iv_high_l1", "epr.incl.ind_l1:epr.incl.dist_l1:polity_iv_high_l1",
                "epr.incl.ind_l1:vdem_polyarchy_index_high_l1", "epr.incl.dist_l1:vdem_polyarchy_index_high_l1", "epr.incl.ind_l1:epr.incl.dist_l1:vdem_polyarchy_index_high_l1",
                "epr.incl.ind_l1:fptp_most_l1", "epr.incl.dist_l1:fptp_most_l1", "epr.incl.ind_l1:epr.incl.dist_l1:fptp_most_l1")


fileConn<-file(paste0(tab.path,"heterogeneity.tex"))
writeLines(stargazer(m.list,
                     title="Heterogeneity: Regime Type \& Electoral System",
                     keep=keep.lines,
                     order = paste0("^", keep.lines , "$"),
                     multicolumn=F,# se = se,
                     dep.var.caption = "Infant Mortality",dep.var.labels = rep("",length(m.list)),
                     covariate.labels=c("Government Co-Ethnic (t-1)", 
                                        "Dist. Share Gov. Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share Co-Ethnics (t-1)",
                                        "Co-Ethnic $\\times$ High Polity IV (t-1)",
                                        "Dist. Share $\\times$ High Polity IV (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share $\\times$  High Polity IV (t-1)",
                                        "Co-Ethnic $\\times$  High VDEM (t-1)",
                                        "Dist. Share $\\times$  High VDEM (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share $\\times$  High VDEM (t-1)",
                                        "Co-Ethnic $\\times$  Mostly FPTP (t-1)",
                                        "Dist. Share $\\times$  Mostly FPTP (t-1)",
                                        "Co-Ethnic $\\times$ Dist. Share $\\times$  Mostly FPTP (t-1)"),
                     font.size = "scriptsize",
                     notes.align = "c",label="heterogeneity",align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(.85), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)

