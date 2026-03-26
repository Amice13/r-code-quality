



topics<-c("fem","smoking","socialsec","studentdebt","obamacare","weight")
groups<-c("female","smoker","senior","student","hcip", "lose.weight")


data<-conj.dta
j<-1


did<-function(data, groups, topics){

	dds<-NA
	ses<-NA
	dfs<-NA
	
for(j in 1:length(groups)){##loop over groups
		
temp<-na.omit(conj.dta[ ,c("response","respid",topics[j],groups[j]) ])

form<-"response~"
form1<-paste(topics[j],groups[j],sep="+")
form2<-paste("+",topics[j],"*",groups[j] )
form3<-as.formula(paste(form, form1, form2))

model<-ols(form3 , data=temp,model=TRUE, x=TRUE, y=TRUE)
clust<-robcov(model, temp$respid)


##save larger SE
v<-vcov(model)
se1<-as.n(sqrt(diag(v)))[4]
se.clust<-as.n(sqrt(clust$var[4,4]))

if(se1>se.clust){se<-se1}
if(se1<se.clust){se<-se.clust}

dds[j]<-model$coefficients[4]
ses[j]<-se.clust

		
	}
	
	names(dds)<-groups
	names(ses)<-groups
	res<-cbind.data.frame(did=dds, se=ses)
	res$lb<-res$did-1.96*res$se
	res$ub<-res$did+1.96*res$se
	return(res)
}
