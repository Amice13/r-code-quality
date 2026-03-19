require(ggplot2)
require(arm)
require(MASS)

load('election-2020-survey.Rda')

# Survey Results and Estimation

# run models
t.bin=glm(topic.approve.w1.bin~topic.w1+partisanship+sex+age+income+education+ethnicity+president.w1+condition.w1,data=dt,family='binomial')
p.bin=glm(pres.approve.w1.bin~topic.w1+partisanship+sex+age+income+education+ethnicity+president.w1+condition.w1,data=dt,family='binomial')
c.bin=glm(manip.cong.w1.bin~topic.w1+partisanship+sex+age+income+education+ethnicity+president.w1+condition.w1,data=dt,family='binomial')
l.bin=glm(manip.law.w1.bin~topic.w1+partisanship+sex+age+income+education+ethnicity+president.w1+condition.w1,data=dt,family='binomial')

# simulate and store effects
outcomes=matrix(data=NA, ncol=6,nrow=24)

public_comparisons=function(model,data,simulations=1000,seed=6658) {
  set.seed(seed)
  betas=slot(sim(model,simulations),'coef')
  covariates=names(model$model)
  results=matrix(data=NA, ncol=6,nrow=6)
  
  # add columns for factor coefs
  D=data[complete.cases(data[,covariates]),]
  D=cbind(D,model.matrix(~topic.w1-1,data=D))
  D=cbind(D,model.matrix(~president.w1-1,data=D))
  if ('partisanship' %in% covariates) {D=cbind(D,model.matrix(~partisanship-1,data=D))}
  if ('sex' %in% covariates) {D=cbind(D,model.matrix(~sex-1,data=D))}
  if ('income' %in% covariates) {D=cbind(D,model.matrix(~income-1,data=D))}
  if ('education' %in% covariates) {D=cbind(D,model.matrix(~education-1,data=D))}
  if ('ethnicity' %in% covariates) {D=cbind(D,model.matrix(~ethnicity-1,data=D))}
  D=cbind(D,model.matrix(~condition.w1-1,data=D))
  
  # intercept + covariates 
  controls=cbind(1,D$`topic.w1contractor pay`,D$`topic.w1farm payments`,D$`topic.w1foreign aid and abortion`,D$`topic.w1foreign workers`,D$`topic.w1gun violence research`,D$topic.w1LGBT,D$`topic.w1policing and military surplus`,D$`topic.w1public lands`,D$`topic.w1Russian sanctions`,D$`topic.w1student loans`,D$topic.w1trade,D$`topic.w1water rules`,D$`topic.w1weapon sales to Saudi Arabia`,D$topic.w1wildlife)
  if ('partisanship' %in% covariates) {controls=cbind(controls,D$partisanshipDemocrat,D$partisanshipRepublican)}
  if ('sex' %in% covariates) {controls=cbind(controls,D$sexFemale)}
  if ('age' %in% covariates) {controls=cbind(controls,D$age)}
  if ('income' %in% covariates) {controls=cbind(controls,D$`income2: $25,000 to $50,000`,D$`income3: $50,001 to $75,000`,D$`income4: $75,001 to $100,000`,D$`income5: More than $100,001`)}
  if ('education' %in% covariates) {controls=cbind(controls,D$`educationSome College or Vocational`,D$`educationB.A. or B.S.`,D$`educationPost-graduate or Higher`)}
  if ('ethnicity' %in% covariates) {controls=cbind(controls,D$ethnicityBlack,D$`ethnicityNative American`,D$`ethnicityOther/Decline to State`,D$ethnicityWhite)}
  
  # new data to simulate over
  d.position.obama=cbind(controls,0,0,0)
  d.order.obama=cbind(controls,0,0,1)
  d.congress.obama=cbind(controls,0,1,0)
  
  d.position.trump=cbind(controls,1,0,0)
  d.order.trump=cbind(controls,1,0,1)
  d.congress.trump=cbind(controls,1,1,0)
  
  # simulations
  p.pos.obama=apply((1/(1+exp(-as.matrix(d.position.obama)%*% t(betas)))),1,as.vector) 
  p_pos.obama=apply(p.pos.obama,1,mean)
  p.ord.obama=apply((1/(1+exp(-as.matrix(d.order.obama)%*% t(betas)))),1,as.vector) 
  p_ord.obama=apply(p.ord.obama,1,mean)
  p.cong.obama=apply((1/(1+exp(-as.matrix(d.congress.obama)%*% t(betas)))),1,as.vector) 
  p_cong.obama=apply(p.cong.obama,1,mean)
  
  p.pos.trump=apply((1/(1+exp(-as.matrix(d.position.trump)%*% t(betas)))),1,as.vector) 
  p_pos.trump=apply(p.pos.trump,1,mean)
  p.ord.trump=apply((1/(1+exp(-as.matrix(d.order.trump)%*% t(betas)))),1,as.vector) 
  p_ord.trump=apply(p.ord.trump,1,mean)
  p.cong.trump=apply((1/(1+exp(-as.matrix(d.congress.trump)%*% t(betas)))),1,as.vector) 
  p_cong.trump=apply(p.cong.trump,1,mean)
  
  # store
  results[1,1]<- quantile(p_pos.obama,.025)
  results[1,2]<- mean(p_pos.obama)
  results[1,3]<- quantile(p_pos.obama,.975)
  results[1,4]<- 'Position-taking'
  results[1,5]<- 'Obama'
  results[1,6]<- covariates[1]
  results[2,1]<- quantile(p_cong.obama,.025)
  results[2,2]<- mean(p_cong.obama)
  results[2,3]<- quantile(p_cong.obama,.975)
  results[2,4]<- 'Lobby Congress'
  results[2,5]<- 'Obama'
  results[2,6]<- covariates[1]
  results[3,1]<- quantile(p_ord.obama,.025)
  results[3,2]<- mean(p_ord.obama)
  results[3,3]<- quantile(p_ord.obama,.975)
  results[3,4]<- 'Executive Action'
  results[3,5]<- 'Obama'
  results[3,6]<- covariates[1]
  results[4,1]<- quantile(p_pos.trump,.025)
  results[4,2]<- mean(p_pos.trump)
  results[4,3]<- quantile(p_pos.trump,.975)
  results[4,4]<- 'Position-taking'
  results[4,5]<- 'Trump'
  results[4,6]<- covariates[1]
  results[5,1]<- quantile(p_cong.trump,.025)
  results[5,2]<- mean(p_cong.trump)
  results[5,3]<- quantile(p_cong.trump,.975)
  results[5,4]<- 'Lobby Congress'
  results[5,5]<- 'Trump'
  results[5,6]<- covariates[1]
  results[6,1]<- quantile(p_ord.trump,.025)
  results[6,2]<- mean(p_ord.trump)
  results[6,3]<- quantile(p_ord.trump,.975)
  results[6,4]<- 'Executive Action'
  results[6,5]<- 'Trump'
  results[6,6]<- covariates[1]
  
  
  return(results)
}

outcomes=data.frame(lb.95=numeric(24),me=numeric(24),ub.95=numeric(24),condition=character(24),president=character(24),dv=character(24))
outcomes[1:6,1:6]=public_comparisons(p.bin,dt)
outcomes[7:12,1:6]=public_comparisons(t.bin,dt)
outcomes[13:18,1:6]=public_comparisons(c.bin,dt)
outcomes[19:24,1:6]=public_comparisons(l.bin,dt)
outcomes$lb.95=as.numeric(outcomes$lb.95)
outcomes$me=as.numeric(outcomes$me)
outcomes$ub.95=as.numeric(outcomes$ub.95)
outcomes$condition=factor(outcomes$condition,levels=c('Position-taking','Lobby Congress','Executive Action'))
# --------------------------------------------- 

# Figure 5.2: There is little or no penalty for executive action. 
done.a = ggplot(outcomes[outcomes$dv=='pres.approve.w1.bin',], aes(x=president, y=me, fill=condition)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) +
  geom_errorbar(aes(ymin=lb.95, ymax=ub.95), width=.2,position=position_dodge(.9)) +
  labs(x="President", y = "Job Approval")+ 
  theme_classic() + 
  scale_fill_manual(values=c('#ffffff','#BDBCBC','#424242')) +
  theme(legend.title=element_blank(),legend.position='none',axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))

done.b = ggplot(outcomes[outcomes$dv=='topic.approve.w1.bin',], aes(x=president, y=me, fill=condition)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) +
  geom_errorbar(aes(ymin=lb.95, ymax=ub.95), width=.2,position=position_dodge(.9)) +
  labs(x="President", y = "Policy Approval")+ 
  theme_classic() + 
  scale_fill_manual(values=c('#ffffff','#BDBCBC','#424242')) +
  theme(legend.title=element_blank(),legend.position='none',axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))

done.c = ggplot(outcomes[outcomes$dv=='manip.cong.w1.bin',], aes(x=president, y=me, fill=condition)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) +
  geom_errorbar(aes(ymin=lb.95, ymax=ub.95), width=.2,position=position_dodge(.9)) +
  labs(x="President", y = "Works with Congress")+ 
  theme_classic() + 
  scale_fill_manual(values=c('#ffffff','#BDBCBC','#424242')) +
  theme(legend.title=element_blank(),legend.position='none',axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))

done.d = ggplot(outcomes[outcomes$dv=='manip.law.w1.bin',], aes(x=president, y=me, fill=condition)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) +
  geom_errorbar(aes(ymin=lb.95, ymax=ub.95), width=.2,position=position_dodge(.9)) +
  labs(x="President", y = "Respects Law")+ 
  theme_classic() + theme(axis.text.x=element_text(colour="black"),
                          axis.text.y=element_text(colour="black")) +
  scale_fill_manual(values=c('#ffffff','#BDBCBC','#424242'),name = "Action Type") 
# -----------------------------------------------

# Arrange and export commands
#leg <- as_ggplot(get_legend(done.d))

#done.d = done.d + theme(legend.title=element_blank(),legend.position='none')

#done = grid.arrange(
#  done.a,done.b,leg,done.c,done.d,
#  widths = c(2, 2, 1),nrow=2,  layout_matrix = rbind(c(1, 2, 3),
#                                                     c(4, 5, NA)))

#pdf(file='/Users/lowande/Dropbox (University of Michigan)/projects/book/manuscript/figures/f-done.pdf',width=6.5,height=4)
#grid.arrange(
#  done.a,done.b,leg,done.c,done.d,
#  widths = c(2, 2, 1),nrow=2,  layout_matrix = rbind(c(1, 2, 3),
#                                                     c(4, 5, NA)))
#dev.off()
# -----------------------------------------------
