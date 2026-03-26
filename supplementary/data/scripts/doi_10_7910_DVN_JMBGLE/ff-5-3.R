require(ggplot2)
require(arm)
require(MASS)

load('election-2020-survey.Rda')

# Survey Results and Estimation

# run models
t.bin=glm(topic.approve.w1.bin~topic.w1+partisanship+sex+age+income+education+ethnicity+president.w1+condition.w1,data=dt,family='binomial')
o.bin=glm(topic.approve.w2.bin~topic.w1+partisanship+sex+age+income+education+ethnicity+president.w1+condition.w1 + outcome,data=dt,family='binomial')

# simulate and store effects
outcomes=matrix(data=NA, ncol=6,nrow=6)

# announcement results 
set.seed(6658)
betas=slot(sim(t.bin,1000),'coef')
covariates=names(t.bin$model)
results=matrix(data=NA, ncol=6,nrow=2)
data=dt

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
d.obama=cbind(controls,0,D$condition.w1congress,D$condition.w1order)
d.trump=cbind(controls,1,D$condition.w1congress,D$condition.w1order)

# simulations
p.obama=apply((1/(1+exp(-as.matrix(d.obama)%*% t(betas)))),1,as.vector) 
p_obama=apply(p.obama,1,mean)

p.trump=apply((1/(1+exp(-as.matrix(d.trump)%*% t(betas)))),1,as.vector) 
p_trump=apply(p.trump,1,mean)

# store
results[1,1]<- quantile(p_obama,.025)
results[1,2]<- mean(p_obama)
results[1,3]<- quantile(p_obama,.975)
results[1,4]<- 'Announcement'
results[1,5]<- 'Obama'
results[1,6]<- covariates[1]
results[2,1]<- quantile(p_trump,.025)
results[2,2]<- mean(p_trump)
results[2,3]<- quantile(p_trump,.975)
results[2,4]<- 'Announcement'
results[2,5]<- 'Trump'
results[2,6]<- covariates[1]

outcomes[1:2,1:6]=results

# failure and success results

set.seed(6658)
betas=slot(sim(o.bin,1000),'coef')
covariates=names(o.bin$model)
results=matrix(data=NA, ncol=6,nrow=4)
data=dt

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
d.obama.win=cbind(controls,0,D$condition.w1congress,D$condition.w1order,0)
d.obama.fail=cbind(controls,0,D$condition.w1congress,D$condition.w1order,1)
d.trump.win=cbind(controls,1,D$condition.w1congress,D$condition.w1order,0)
d.trump.fail=cbind(controls,1,D$condition.w1congress,D$condition.w1order,1)

# simulations
p.obama.win=apply((1/(1+exp(-as.matrix(d.obama.win)%*% t(betas)))),1,as.vector) 
p_obama.win=apply(p.obama.win,1,mean)

p.trump.win=apply((1/(1+exp(-as.matrix(d.trump.win)%*% t(betas)))),1,as.vector) 
p_trump.win=apply(p.trump.win,1,mean)

p.obama.fail=apply((1/(1+exp(-as.matrix(d.obama.fail)%*% t(betas)))),1,as.vector) 
p_obama.fail=apply(p.obama.fail,1,mean)

p.trump.fail=apply((1/(1+exp(-as.matrix(d.trump.fail)%*% t(betas)))),1,as.vector) 
p_trump.fail=apply(p.trump.fail,1,mean)

# store
results[1,1]<- quantile(p_obama.win,.025)
results[1,2]<- mean(p_obama.win)
results[1,3]<- quantile(p_obama.win,.975)
results[1,4]<- 'Success'
results[1,5]<- 'Obama'
results[1,6]<- covariates[1]
results[2,1]<- quantile(p_trump.win,.025)
results[2,2]<- mean(p_trump.win)
results[2,3]<- quantile(p_trump.win,.975)
results[2,4]<- 'Success'
results[2,5]<- 'Trump'
results[2,6]<- covariates[1]
results[3,1]<- quantile(p_obama.fail,.025)
results[3,2]<- mean(p_obama.fail)
results[3,3]<- quantile(p_obama.fail,.975)
results[3,4]<- 'Failure'
results[3,5]<- 'Obama'
results[3,6]<- covariates[1]
results[4,1]<- quantile(p_trump.fail,.025)
results[4,2]<- mean(p_trump.fail)
results[4,3]<- quantile(p_trump.fail,.975)
results[4,4]<- 'Failure'
results[4,5]<- 'Trump'
results[4,6]<- covariates[1]

outcomes[3:6,1:6]=results

outcomes = data.frame(outcomes)
names(outcomes) = c('lb.95','me','ub.95','condition','president')
outcomes$lb.95 = as.numeric(outcomes$lb.95 )
outcomes$me = as.numeric(outcomes$me)
outcomes$ub.95 = as.numeric(outcomes$ub.95 )
# -----------------------------------------------

# Figure 5.3: Public opinion about presidents’ handling of the topic at announcement is indistinguishable from success. 
default = ggplot(outcomes, aes(x=president, y=me, fill=condition)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) +
  geom_errorbar(aes(ymin=lb.95, ymax=ub.95), width=.2,position=position_dodge(.9)) +
  labs(x="President", y = "Handling of Policy")+ 
  theme_classic() + theme(axis.text.x=element_text(colour="black"),
                          axis.text.y=element_text(colour="black")) + 
  scale_fill_manual(values=c('#ffffff','#BDBCBC','#424242'),name = "Information Type") 
# -----------------------------------------------
