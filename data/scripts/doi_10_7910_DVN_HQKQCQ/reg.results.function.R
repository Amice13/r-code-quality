


reg.results<-function( data, dv, covars, groups=NULL, group.names=NULL, topics=NULL, interaction=NULL, coef.names=NULL ) {

##make storage object for coefficients and standard errors plus one row for number of observations

if(interaction==T){covars<-c(covars,"relevant * friend.source","relevant * unfriend.source")}

results<-as.data.frame(matrix(nrow=(length(covars)*2)+3,ncol=length(topics) ))
colnames(results)<-groups
rows<-c("Intercept","S.E.0")
for(i in 1:length(covars)){rows<-c(rows, covars[i], paste("S.E.", i, sep="")) }
rows[length(rows)+1]<-"N"
rownames(results)<-rows


##make storage object for indicator of stat significane at 5% level
sig.list<-list(NA)

##work with a subset of the data
for(j in 1:length(groups)){

##define estimation data
est<-na.omit(data[  data[,groups[j] ]==1   ,c("response","respid","friend.source","unfriend.source","dem","rep",topics[j],groups[j])])

##generate variables
est$relevant<-NA
est$relevant[est[,topics[j]]==1]<-1
est$relevant[est[,topics[j]]==0]<-0

##store number of observations
results["N",j]<- nrow(est)

##build model formula
form1<-dv
form2<-covars[1]
for(g in 2:length(covars)){form2<-paste(form2,covars[g],sep="+" )}
form2<-as.formula(paste(form1,"~",form2, sep=""))


##estimate OLS model
model<-ols(form2, data=est,model=TRUE, x=TRUE, y=TRUE)

##store coefficients
results["Intercept",j]<-model$coefficients["Intercept"]

##get indices in results for covariate coefficients
ind<-seq(3, (length(covars)*2 +1), by=2 )

for(k in 1:length(ind)){
results[ind[k],j]<-model$coefficients[k+1]	
}

##get homosekdastic SEs
v<-vcov(model)
se.standard<-sqrt(diag(v))


##Get clustered SEs
clust<-robcov(model, est$respid)
v2<-vcov(clust)
se.clust<-sqrt(diag(v2))

se.final<-NA

##choose whichever SE is larger
for(f in 1:length(se.standard)){
	if(se.standard[f]>se.clust[f]){se.final[f]<-se.standard[f]}
	if(se.standard[f]<se.clust[f]){se.final[f]<-se.clust[f]}
}


results["S.E.0",j]<-se.final[1]

ind2<-ind+1 ##make new index for standard errors in results matrix

for(k in 1:length(ind2)){
results[ind2[k],j]<-se.final[k+1]	
}



##flag stat sig results
sig<-NA

ind.sig<-c(1,ind) ##add intercept row to indices
ind2.sig<-c(2,ind2) ##add intercept SE row to indices

for(s in 1:( length(covars)+1) ){
	
	lb<-results[ind.sig[s],j]-1.96*results[ind2.sig[s],j] ##compute lower bound of 95% CI
	ub<-results[ind.sig[s],j]+1.96*results[ind2.sig[s],j] ##compute upper bound of 95% CI
	
	if((lb*ub)>0){sig[s]<-1}##if product of upper and lower bound is positive, they are on one side of zero and the result is stat. sig...
	if((lb*ub)<0){sig[s]<-0} ##...if not, result is not stat. sig
	
	
}

sig.list[[j]]<-sig


} ##close group-level loop

###make ready for xtable in paper
results.num<-results ##rename results, keep this as a numeric object

results[1:(nrow(results)-1),]<-round(results[1:(nrow(results)-1),], digits=3)

##add parens around SEs
for(w in 1:ncol(results)){ ##loop over groups
for(h in 1:length(ind2.sig)){ ##loop over SE rows

results[ind2.sig[h],w]<-paste( "(" , results[ind2.sig[h],w] , ")",sep="" )

if(sig.list[[w]][h]==1){results[ind2.sig[h],w]<-paste( results[ind2.sig[h],w],"*"  ,sep="" )}
}
}##close parens loop

colnames(results)<-group.names

##label rows correctly
se.rows<-grep("S.E.",rownames(results))
blank<-" "
for(d in 1:length(se.rows)){
	
	rownames(results)[se.rows[d]]<-paste(blank, " ",sep="")
	blank<-paste(blank, rownames(results)[se.rows[d]], sep="")
}

coef.rows<-seq(1, (nrow(results)-2), by=2 )

rownames(results)[coef.rows]<-coef.names

res<-list(results, results.num)
return(res)

}##close function
