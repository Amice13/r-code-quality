
##define function to estimate mean predictions using any of three modeling strategies

pred<-function(data,  groups, topics, model.type=c("ols")){

results<-list(NA)


	for(i in 1:length(groups)){
		
		dd<-na.omit(data[data[,groups[i]]==1,c("respid","response","unfriend.source","friend.source",groups[i],topics[i],"dem","rep")])
		
		

dd$relevant<-NA
dd$relevant[dd[,topics[i]]==1]<-1
dd$relevant[dd[,topics[i]]==0]<-0

if(model.type=="ols"){
model<-ols(response~friend.source+unfriend.source +relevant+friend.source*relevant + unfriend.source*relevant, data=dd[ dd[,groups[i]]==1,],model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, dd[ dd[,groups[i]]==1,"respid"])##respondent-clustered SEs
coef.c<-clust$coefficients
se.c<-sqrt(diag(vcov(clust)))
vc<-vcov(clust)
vars<-names(clust$coefficients)

##conventional ses
se.c2<-sqrt(diag(vcov(model)))

##which produced a larger se on relevant?
if(se.c[4]>se.c2[4]){ clust<- clust  }##Use cluster robust SEs
if(se.c[4]<se.c2[4]){ clust<- model  }##use standard OLS SEs



##get predicted values
irr.neutral<-predict(clust, newdata=cbind.data.frame(unfriend.source=0, friend.source=0, relevant=F), se.fit=T, interval="confidence")
irr.friend<-predict(clust, newdata=cbind.data.frame(unfriend.source=0, friend.source=1, relevant=F), se.fit=T, interval="confidence")
irr.unfriend<-predict(clust, newdata=cbind.data.frame(unfriend.source=1, friend.source=0, relevant=F), se.fit=T, interval="confidence")

rel.neutral<-predict(clust, newdata=cbind.data.frame(unfriend.source=0, friend.source=0, relevant=T), se.fit=T, interval="confidence")
rel.friend<-predict(clust, newdata=cbind.data.frame(unfriend.source=0, friend.source=1, relevant=T), se.fit=T, interval="confidence")
rel.unfriend<-predict(clust, newdata=cbind.data.frame(unfriend.source=1, friend.source=0, relevant=T), se.fit=T, interval="confidence")


est.irr<-c( irr.unfriend$linear.predictors, irr.neutral$linear.predictors , irr.friend$linear.predictors       )
names(est.irr)<-c("irr.unfriend","irr.neutral","irr.friend")
lb.irr<-c( est.irr[1] - 1.96*irr.unfriend$se.fit, est.irr[2] - 1.96*irr.neutral$se.fit , est.irr[3] - 1.96*irr.friend$se.fit   )
ub.irr<-c( est.irr[1] + 1.96*irr.unfriend$se.fit, est.irr[2] + 1.96*irr.neutral$se.fit , est.irr[3] + 1.96*irr.friend$se.fit   )



est.rel<-c( rel.unfriend$linear.predictors, rel.neutral$linear.predictors , rel.friend$linear.predictors       )
names(est.rel)<-c("rel.unfriend","rel.neutral","rel.friend")
lb.rel<-c( est.rel[1] - 1.96*rel.unfriend$se.fit, est.rel[2] - 1.96*rel.neutral$se.fit , est.rel[3] - 1.96*rel.friend$se.fit   )
ub.rel<-c( est.rel[1] + 1.96*rel.unfriend$se.fit, est.rel[2] + 1.96*rel.neutral$se.fit , est.rel[3] + 1.96*rel.friend$se.fit   )

	
}





if(model.type=="probit"){
model <- glm(response~friend.source+unfriend.source +relevant+friend.source*relevant + unfriend.source*relevant, data=dd[ dd[,groups[i]]==1,],  family = binomial(link = "probit"))


coef.c<-model$coefficients
se.c<-sqrt(diag(vcov(model)))
vc<-vcov(model)
vars<-names(model$coefficients)




##get predicted values
irr.neutral<-predict(model, newdata=cbind.data.frame(unfriend.source=0, friend.source=0, relevant=F), se.fit=T, type="response")
irr.friend<-predict(model, newdata=cbind.data.frame(unfriend.source=0, friend.source=1, relevant=F), se.fit=T, type="response")
irr.unfriend<-predict(model, newdata=cbind.data.frame(unfriend.source=1, friend.source=0, relevant=F), se.fit=T, type="response")

rel.neutral<-predict(model, newdata=cbind.data.frame(unfriend.source=0, friend.source=0, relevant=T), se.fit=T, type="response")
rel.friend<-predict(model, newdata=cbind.data.frame(unfriend.source=0, friend.source=1, relevant=T), se.fit=T, type="response")
rel.unfriend<-predict(model, newdata=cbind.data.frame(unfriend.source=1, friend.source=0, relevant=T), se.fit=T, type="response")

est.irr<-c( irr.unfriend$fit, irr.neutral$fit , irr.friend$fit       )
names(est.irr)<-c("irr.unfriend","irr.neutral","irr.friend")
lb.irr<-c( est.irr[1] - 1.96*irr.unfriend$se.fit, est.irr[2] - 1.96*irr.neutral$se.fit , est.irr[3] - 1.96*irr.friend$se.fit   )
ub.irr<-c( est.irr[1] + 1.96*irr.unfriend$se.fit, est.irr[2] + 1.96*irr.neutral$se.fit , est.irr[3] + 1.96*irr.friend$se.fit   )


est.rel<-c( rel.unfriend$fit, rel.neutral$fit , rel.friend$fit       )
names(est.rel)<-c("rel.unfriend","rel.neutral","rel.friend")
lb.rel<-c( est.rel[1] - 1.96*rel.unfriend$se.fit, est.rel[2] - 1.96*rel.neutral$se.fit , est.rel[3] - 1.96*rel.friend$se.fit   )
ub.rel<-c( est.rel[1] + 1.96*rel.unfriend$se.fit, est.rel[2] + 1.96*rel.neutral$se.fit , est.rel[3] + 1.96*rel.friend$se.fit   )

}



if(model.type=="logit"){
model <- glm(response~friend.source+unfriend.source +relevant+friend.source*relevant + unfriend.source*relevant, data=dd[ dd[,groups[i]]==1,],  family = binomial(link = "logit"))


coef.c<-model$coefficients
se.c<-sqrt(diag(vcov(model)))
vc<-vcov(model)
vars<-names(model$coefficients)




##get predicted values
irr.neutral<-predict(model, newdata=cbind.data.frame(unfriend.source=0, friend.source=0, relevant=F), se.fit=T, type="response")
irr.friend<-predict(model, newdata=cbind.data.frame(unfriend.source=0, friend.source=1, relevant=F), se.fit=T, type="response")
irr.unfriend<-predict(model, newdata=cbind.data.frame(unfriend.source=1, friend.source=0, relevant=F), se.fit=T, type="response")

rel.neutral<-predict(model, newdata=cbind.data.frame(unfriend.source=0, friend.source=0, relevant=T), se.fit=T, type="response")
rel.friend<-predict(model, newdata=cbind.data.frame(unfriend.source=0, friend.source=1, relevant=T), se.fit=T, type="response")
rel.unfriend<-predict(model, newdata=cbind.data.frame(unfriend.source=1, friend.source=0, relevant=T), se.fit=T, type="response")

est.irr<-c( irr.unfriend$fit, irr.neutral$fit , irr.friend$fit       )
names(est.irr)<-c("irr.unfriend","irr.neutral","irr.friend")
lb.irr<-c( est.irr[1] - 1.96*irr.unfriend$se.fit, est.irr[2] - 1.96*irr.neutral$se.fit , est.irr[3] - 1.96*irr.friend$se.fit   )
ub.irr<-c( est.irr[1] + 1.96*irr.unfriend$se.fit, est.irr[2] + 1.96*irr.neutral$se.fit , est.irr[3] + 1.96*irr.friend$se.fit   )


est.rel<-c( rel.unfriend$fit, rel.neutral$fit , rel.friend$fit       )
names(est.rel)<-c("rel.unfriend","rel.neutral","rel.friend")
lb.rel<-c( est.rel[1] - 1.96*rel.unfriend$se.fit, est.rel[2] - 1.96*rel.neutral$se.fit , est.rel[3] - 1.96*rel.friend$se.fit   )
ub.rel<-c( est.rel[1] + 1.96*rel.unfriend$se.fit, est.rel[2] + 1.96*rel.neutral$se.fit , est.rel[3] + 1.96*rel.friend$se.fit   )

}




irr<-cbind.data.frame(est=est.irr, lb=lb.irr, ub=ub.irr,group="irr")
rel<-cbind.data.frame(est=est.rel, lb=lb.rel, ub=ub.rel,group="rel")
results.d<-rbind.data.frame(irr, rel)
results.d$party="dem"
results.d$order=seq(1,nrow(results.d)*2, by=2)
res<-results.d

res<-res[order(res$order),]
res


results[[i]]<-res

}
names(results)<-groups
return(results)
}##END FUNCTION
