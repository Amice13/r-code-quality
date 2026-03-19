###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
##### FEASIBILITY ANALYSIS
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################

cat("Begin FEASIBILITY ANALYSIS\n")

load("cces_2012_income.Rdata") ##cces.income
load("zip_apsa_imputed22.Rdata")
load("dd.final.Rdata")



dim(dd.final)
dd.final$faminc.house<-dd.final$income.num*3*1000
table(dd.final$faminc.house)

dd.final$zipnew<-dd.final$zip
dd.final$zipnew[dd.final$zipnew==""]<-NA
table(nchar(dd.final$zipnew))


head(dd.final)


class(dd.final$ownedvaluek)
table(dd.final$ownedvaluek)
dd.final$house.val<-1000*as.numeric(dd.final$ownedvaluek)
table(dd.final$house.val)


##did they follow the rule of thumb for three times the price of the house?
dd.final$rule.house<-NA
dd.final$rule.house[dd.final$faminc.house>=dd.final$house.val]<-1
dd.final$rule.house[dd.final$faminc.house<dd.final$house.val]<-0
table(dd.final$rule.house, exclude=NULL)



##########

zip$in.dd.final<-0
zip$in.dd.final[zip$zipnew%in%dd.final$zipnew]<-1
table(zip$in.dd.final)

zip.dd.final<-zip[zip$in.dd.final==1,]
dim(zip.dd.final)
names(zip.dd.final)

names(zip)
zip.dd.final<-cbind.data.frame(pctr082=zip.dd.final$pctr082, zipnew=zip.dd.final$zipnew, ba=zip.dd.final$ba, own25=zip.dd.final$ownocc.val.q252, rentmed=zip.dd.final$mediangrossrent2, top25=zip.dd.final$top25,pctown=zip.dd.final$pctowned.5yr.20122, rent=zip.dd.final$mediangrossrent2, popdens.5yr.20122=zip.dd.final$popdens.5yr.20122, msa=zip.dd.final$msa)
dim(zip.dd.final)
head(zip.dd.final)




##### merge zip code data to ssi data by current zip code of respondent
dim(dd.final)
dd.final.zip<-merge(dd.final, zip.dd.final, by="zipnew",all.x=TRUE)
dim(dd.final.zip)
names(dd.final.zip)

##merge in current and former msa for movers
msas.old<-cbind.data.frame(msa.old=zip$msa, zipnew=zip$zipnew)##using complete zip code data file
msas.new<-cbind.data.frame(msa.new=zip$msa, zipnew=zip$zipnew)

table(nchar(dd.final$lastzip.text))##previous zip code
table(nchar(dd.final.zip$zipnew))##current zip code
table(nchar(zip$zipnew))
names(zip)

dim(dd.final.zip)
dd.final.zip<-merge(dd.final.zip, msas.new, by="zipnew", all.x=T)##merge by current zip code
dd.final.zip<-merge(dd.final.zip, msas.old, by.x="lastzip.text",by.y="zipnew", all.x=T)##merge by current zip code
dim(dd.final.zip)



##what percentage of movers stayed in the same MSA
table(!is.na(dd.final.zip$lastzip.text))
table(is.na(dd.final.zip$lastzip.text))
table(dd.final.zip$same.msa, exclude=NULL)
mean(dd.final.zip$same.msa, na.rm=T)
mean(dd.final.zip$same.msa[dd.final.zip$lastzip.text!=dd.final.zip$zipnew], na.rm=T)


##are people who move to different MSAs different than those who move within MSA?
vars<-c("income.num", "white2","single","educ2")
var.names<-c("HH Income ($)","Non-Hispanic White","Single","B.A.")
r<-as.data.frame(matrix(nrow=length(vars), ncol=3))
rownames(r)<-var.names
colnames(r)<-c("Mean (Same MSA)","Mean (Different MSA)","p.value of diff.")

for(i in 1:length(vars)){
t<-t.test(dd.final.zip[dd.final.zip$same.msa==1,vars[i]],dd.final.zip[dd.final.zip$same.msa==0,vars[i]], na.rm=T )
r[i,1]<-t$estimate[1]
r[i,2]<-t$estimate[2]
r[i,3]<-t$p.value
}
r<-signif(r, digits=2)
r
library(xtable)
print(xtable(r,digits=c(0,2,2,4)), row.names=F)



##What percentage of people stayed in the same zip?
table(!is.na(dd.final$lastzip.text))
class(dd.final$lastzip.text)
class(dd.final$zipnew)
table(dd.final$lastzip.text==dd.final$zipnew)

dd.final$same.zip<-NA
dd.final$same.zip[!is.na(dd.final$lastzip.text) & dd.final$lastzip.text==dd.final$zipnew]<-1
dd.final$same.zip[!is.na(dd.final$lastzip.text) & dd.final$lastzip.text!=dd.final$zipnew]<-0
table(dd.final$same.zip, exclude=NULL)
mean(dd.final$same.zip, na.rm=T)


dim(dd.final)
dim(dd.final.zip)



dd.final.zip$dem<-0
dd.final.zip$dem[dd.final.zip$partywlean=="dem"]<-1
table(dd.final.zip$dem)

dd.final.zip$rep<-0
dd.final.zip$rep[dd.final.zip$partywlean=="rep"]<-1
table(dd.final.zip$rep)


dim(dd.final.zip) ###final data to create histograms





##make storage objects
dd.final.zip$zip.poss.dems<-NA
dd.final.zip$zip.poss.dems.ed<-NA
dd.final.zip$zip.poss.dems.msa<-NA
dd.final.zip$zip.poss.dems.south<-NA
dd.final.zip$zip.poss.dems.northeast<-NA
dd.final.zip$zip.poss.dems.west<-NA
dd.final.zip$zip.poss.dems.midwest<-NA
dd.final.zip$zip.poss.dems.aff<-NA
dd.final.zip$zip.poss.dems.owner<-NA
dd.final.zip$zip.poss.dems.density<-NA
dd.final.zip$zip.poss.dems.owner.aff<-NA
dd.final.zip$zip.poss.dems.owner.aff.west<-NA
dd.final.zip$zip.poss.dems.owner.aff.midwest<-NA
dd.final.zip$zip.poss.dems.owner.aff.south<-NA
dd.final.zip$zip.poss.dems.owner.aff.northeast<-NA

dd.final.zip$zip.poss.dems.ed.aff<-NA
dd.final.zip$zip.poss.dems.ed.aff.msa<-NA
dd.final.zip$zip.poss.dems.ed.aff.south<-NA
dd.final.zip$zip.poss.dems.ed.aff.west<-NA
dd.final.zip$zip.poss.dems.ed.aff.midwest<-NA
dd.final.zip$zip.poss.dems.ed.aff.northeast<-NA


dd.final.zip$zip.poss.reps<-NA
dd.final.zip$zip.poss.reps.ed<-NA
dd.final.zip$zip.poss.reps.msa<-NA
dd.final.zip$zip.poss.reps.south<-NA
dd.final.zip$zip.poss.reps.northeast<-NA
dd.final.zip$zip.poss.reps.west<-NA
dd.final.zip$zip.poss.reps.midwest<-NA
dd.final.zip$zip.poss.reps.aff<-NA
dd.final.zip$zip.poss.reps.owner<-NA
dd.final.zip$zip.poss.reps.owner.aff<-NA

dd.final.zip$zip.poss.reps.owner.aff.west<-NA
dd.final.zip$zip.poss.reps.owner.aff.midwest<-NA
dd.final.zip$zip.poss.reps.owner.aff.south<-NA
dd.final.zip$zip.poss.reps.owner.aff.northeast<-NA

dd.final.zip$zip.poss.reps.owner.west<-NA
dd.final.zip$zip.poss.reps.density<-NA
dd.final.zip$zip.poss.reps.ed.aff<-NA
dd.final.zip$zip.poss.reps.ed.aff.msa<-NA
dd.final.zip$zip.poss.reps.ed.aff.south<-NA
dd.final.zip$zip.poss.reps.ed.aff.west<-NA
dd.final.zip$zip.poss.reps.ed.aff.midwest<-NA
dd.final.zip$zip.poss.reps.ed.aff.northeast<-NA


cat("Produce Figure 5 in main text ... \n")


##remove missings on zip code variables

##survey respondents
dd.final.zip<-dd.final.zip[ !is.na(dd.final.zip$zipnew) & !is.na(dd.final.zip$faminc.house) & !is.na(dd.final.zip$pctr082) & !is.na(dd.final.zip$popdens.5yr.20122) & !is.na(dd.final.zip$pctown) &  !is.na(dd.final.zip$top25), ]

dim(dd.final.zip)

##all zips in US
zip<-zip[ !is.na(zip$zipnew)  &  !is.na(zip$ownocc.val.q252) & !is.na(zip$popdens.5yr.20122 ) & !is.na(zip$pctowned.5yr.20122) &  !is.na(zip$top25) & !is.na(zip$south) & !is.na(zip$northeast) & !is.na(zip$west) & !is.na(zip$midwest) , ]

dim(zip)

###loop to compute housing options under various constraints
for(i in 1:nrow(dd.final.zip)){

##DEMS	

if(dd.final.zip$dem[i]==1  ) {
	
##more dem
dd.final.zip$zip.poss.dems[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current zip)


##more dem + in top 25 msa
dd.final.zip$zip.poss.dems.msa[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 & zip$top25==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in top25 MSA)

##more dem + in south
dd.final.zip$zip.poss.dems.south[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$south==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in south)

##more dem + in northeast
dd.final.zip$zip.poss.dems.northeast[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$northeast==1  ] , na.rm=TRUE)##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in northeast)


##more dem + in west
dd.final.zip$zip.poss.dems.west[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$west==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in west)

##more dem + in midwest
dd.final.zip$zip.poss.dems.midwest[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 & zip$midwest==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in midwest)

##more dem + affordable
dd.final.zip$zip.poss.dems.aff[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile house affordable)


##more dem + at least as high on pct owner occupied
dd.final.zip$zip.poss.dems.owner[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable
dd.final.zip$zip.poss.dems.owner.aff[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$pctowned.5yr.20122>=dd.final.zip$pctown[i] & dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; %owner occupied equal or higher in destination)

##more dem + at least as high on pct owner occupied & affordable in the West
dd.final.zip$zip.poss.dems.owner.aff.west[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$west==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the midwest
dd.final.zip$zip.poss.dems.owner.aff.midwest[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$midwest==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the south
dd.final.zip$zip.poss.dems.owner.aff.south[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$south==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the northeast
dd.final.zip$zip.poss.dems.owner.aff.northeast[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$northeast==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


##more dem + higher density than current (for dems)
dd.final.zip$zip.poss.dems.density[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$popdens.5yr.20122[i]<zip$popdens.5yr.20122  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


}

##REPS	
	
if(dd.final.zip$rep[i]==1){	

##more rep
dd.final.zip$zip.poss.reps[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current)

##more rep + in top 25 msa
dd.final.zip$zip.poss.reps.msa[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 & zip$top25==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in top25 MSA)

##more rep + in south
dd.final.zip$zip.poss.reps.south[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 & zip$south==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in south)

##more rep + in northeast
dd.final.zip$zip.poss.reps.northeast[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 & zip$northeast==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in northeast)


##more rep + in west
dd.final.zip$zip.poss.reps.west[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 & zip$west==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in west)

##more rep + in midwest
dd.final.zip$zip.poss.reps.midwest[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 & zip$midwest==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in midwest)

##more rep + affordable
dd.final.zip$zip.poss.reps.aff[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)


##more rep + at least as high on pct owner occupied
dd.final.zip$zip.poss.reps.owner[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable
dd.final.zip$zip.poss.reps.owner.aff[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the West
dd.final.zip$zip.poss.reps.owner.aff.west[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$west==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


##more rep + at least as high on pct owner occupied & affordable in the midest
dd.final.zip$zip.poss.reps.owner.aff.midwest[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$midwest==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the south
dd.final.zip$zip.poss.reps.owner.aff.south[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$south==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the northeast
dd.final.zip$zip.poss.reps.owner.aff.northeast[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$northeast==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)




##more rep + lower density than current (for reps)
dd.final.zip$zip.poss.reps.density[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$popdens.5yr.20122[i]>zip$popdens.5yr.20122  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)

}


}

	

table(nchar(dd.final.zip$zipnew))

dd.final.zip2<-dd.final.zip

dim(dd.final.zip2)
head(dd.final.zip2)

save(dd.final.zip2, file=paste(output,"move_poss_ssi2.Rdata",sep="") )
load(paste(output,"move_poss_ssi2.Rdata",sep=""))


##check for missingness on zipcode covariates
table(!is.na(zip$pctr082))

table(!is.na(zip$zipnew))

table(!is.na(zip$ba))

table(!is.na(zip$ownocc.val.q252))

table(!is.na(zip$popdens.5yr.20122 ))

table(!is.na(zip$pctowned.5yr.20122))

table(!is.na(zip$top25))

table(!is.na(zip$south))

table(!is.na(zip$northeast))

table(!is.na(zip$west))

table(!is.na(zip$midwest))


##about as many missings as members of other party
table(dd.final.zip2$dem)
table(is.na(dd.final.zip2$zip.poss.reps.density))

##about as many missings as members of other party
table(dd.final.zip2$rep)
table(is.na(dd.final.zip2$zip.poss.dems.northeast))


var.names<-c("zip.poss.dems", "zip.poss.dems.msa", "zip.poss.dems.south", "zip.poss.dems.northeast", "zip.poss.dems.west", "zip.poss.dems.midwest", "zip.poss.dems.aff", "zip.poss.dems.owner", "zip.poss.dems.owner.aff", "zip.poss.dems.owner.aff.west", "zip.poss.dems.owner.aff.midwest", "zip.poss.dems.owner.aff.south", "zip.poss.dems.owner.aff.northeast",   
"zip.poss.dems.density","zip.poss.reps",  "zip.poss.reps.msa", "zip.poss.reps.south", "zip.poss.reps.northeast", "zip.poss.reps.west", "zip.poss.reps.midwest", "zip.poss.reps.aff", "zip.poss.reps.owner", "zip.poss.reps.owner.aff", "zip.poss.reps.owner.aff.west", "zip.poss.reps.owner.aff.midwest", "zip.poss.reps.owner.aff.south", "zip.poss.reps.owner.aff.northeast","zip.poss.reps.density")
	
	
for(i in 1:length(var.names)){
	
	print(summary(dd.final.zip2[,var.names[i]]))
	
}



# # # # 

# # # ##analyze
 # # # ##spot check

test<-NA

##take 1000 draws, see if all are successful
for(i in 1:1000){
draw<-sample(1:nrow(dd.final.zip2), size=1)

if(dd.final.zip2$rep[i]==1){
test[i]<-I(sum(zip$housing[ dd.final.zip2$pctr082[draw]<zip$pctr082  ] , na.rm=TRUE)==dd.final.zip2$zip.poss.reps[draw])
}

if(dd.final.zip2$dem[i]==1){
test[i]<-I(sum(zip$housing[ dd.final.zip2$pctr082[draw]>zip$pctr082  ] , na.rm=TRUE)==dd.final.zip2$zip.poss.dems[draw])
}


}
table(test) ##perfect.




# # # ###end spot check



	
	total.housing<-as.numeric(sum(zip$housing[!is.na(zip$pctr082) & !is.na(zip$zipnew) & !is.na(zip$ba) &  !is.na(zip$ownocc.val.q252) & !is.na(zip$popdens.5yr.20122 ) & !is.na(zip$pctowned.5yr.20122) &  !is.na(zip$top25) & !is.na(zip$south) & !is.na(zip$northeast) & !is.na(zip$west) & !is.na(zip$midwest)]))

	
	

	##compute proportion of total housing units represented by each sum
	for(i in 1:length(var.names)){
		
		dd.final.zip2[,paste(var.names[i],".prop",sep="")]<-dd.final.zip2[,var.names[i]] / total.housing
		
	}
	
	
plot.vars<-c("zip.poss.dems.prop"  ,
 "zip.poss.dems.msa.prop"      ,          "zip.poss.dems.south.prop"      ,        "zip.poss.dems.northeast.prop"         ,
 "zip.poss.dems.west.prop"   ,            "zip.poss.dems.midwest.prop"       ,     "zip.poss.dems.aff.prop"            ,   
"zip.poss.dems.owner.prop"         ,   "zip.poss.dems.owner.aff.prop",  "zip.poss.dems.owner.aff.west.prop"  ,"zip.poss.dems.owner.aff.midwest.prop"  ,"zip.poss.dems.owner.aff.south.prop"  ,"zip.poss.dems.owner.aff.northeast.prop"  ,

 "zip.poss.dems.density.prop"       ,    
 
 "zip.poss.reps.prop"     ,     "zip.poss.reps.msa.prop"        ,        "zip.poss.reps.south.prop"     ,        
 "zip.poss.reps.northeast.prop"      ,    "zip.poss.reps.west.prop"        ,       "zip.poss.reps.midwest.prop"          , 
"zip.poss.reps.aff.prop"         ,       "zip.poss.reps.owner.prop"     ,   "zip.poss.reps.owner.aff.prop",  "zip.poss.reps.owner.aff.west.prop"  ,"zip.poss.reps.owner.aff.midwest.prop"  ,"zip.poss.reps.owner.aff.south.prop"  ,"zip.poss.reps.owner.aff.northeast.prop"  ,"zip.poss.reps.density.prop"          )


##get total number of partisans in sample
total.dems<-dim(dd.final.zip2[dd.final.zip2$dem==1 & !is.na(dd.final.zip2$zipnew)& !is.na(dd.final.zip2$faminc.house) & !is.na(dd.final.zip2$pctr082) & !is.na(dd.final.zip2$popdens.5yr.20122) & !is.na(dd.final.zip2$pctown) &  !is.na(dd.final.zip2$top25),])[1]
total.dems

total.dems==table(dd.final.zip2$dem[!is.na(dd.final.zip2$zipnew)& !is.na(dd.final.zip2$faminc.house) & !is.na(dd.final.zip2$pctr082) & !is.na(dd.final.zip2$popdens.5yr.20122) & !is.na(dd.final.zip2$pctown) &  !is.na(dd.final.zip2$top25)])[2]


total.reps<-dim(dd.final.zip2[dd.final.zip2$rep==1 & !is.na(dd.final.zip2$zipnew)& !is.na(dd.final.zip2$faminc.house) & !is.na(dd.final.zip2$pctr082)  & !is.na(dd.final.zip2$popdens.5yr.20122) & !is.na(dd.final.zip2$pctown) &  !is.na(dd.final.zip2$top25),])[1]

total.reps==table(dd.final.zip2$rep[!is.na(dd.final.zip2$zipnew)& !is.na(dd.final.zip2$faminc.house) & !is.na(dd.final.zip2$pctr082) & !is.na(dd.final.zip2$popdens.5yr.20122) & !is.na(dd.final.zip2$pctown) &  !is.na(dd.final.zip2$top25)])[2]


bars<-paste("bar.",var.names,sep="")##make names for bar charts
denoms<-c(rep(total.dems, 16), rep(total.reps, 16)) ##make vector of denominators

plot.obs<-list(NA)


bins<-seq(0,1,.05)

##store proportions in a list for each variable

#dems
for(i in 1:(length(bars)/2)  )  {


	plot.obs[[i]]<-assign(bars[i], hist(dd.final.zip2[dd.final.zip2$dem==1,plot.vars[i]], breaks=bins, plot=F)$counts / total.dems)
}


#reps
for(i in ((length(bars)/2)+1): length(bars) )  {
	plot.obs[[i]]<-assign(bars[i], hist(dd.final.zip2[dd.final.zip2$rep==1,plot.vars[i]], breaks=bins, plot=F)$counts / total.reps)
}

names(plot.obs)<-bars



##GENERATE FIGURE 5 in manuscript
file.name<-paste(output, "dr.panel",".pdf",sep="")
pdf(file.name, paper='special', height=5, width=7)

par(mfrow=c(2,4) , oma=c(1.2,1.2,0,0))

barplot(plot.obs$bar.zip.poss.dems, main="Sort as Dem."
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
  
barplot(plot.obs$bar.zip.poss.dems.aff, main= "Sort as Dem., Affordable"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
   
barplot(plot.obs$bar.zip.poss.dems.owner.aff, main= "Sort as Dem., Affordable
Preserve Quality"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)

barplot(plot.obs$bar.zip.poss.dems.owner.aff.west, main=  "Sort as Dem., Affordable, 
Preserve Quality, in West"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
  
  
   barplot(plot.obs$bar.zip.poss.reps, main="Sort as Rep."
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)

barplot(plot.obs$bar.zip.poss.reps.aff, main= "Sort as Rep., Affordable"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
   
barplot(plot.obs$bar.zip.poss.reps.owner.aff, main= "Sort as Rep., Affordable
Preserve Quality"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)


barplot(plot.obs$bar.zip.poss.reps.owner.aff.west, main=  "Sort as Rep., Affordable, 
Preserve Quality, in West"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
       mtext(side=1, at=.5, line=.001, text="Proportion of National Housing Stock Available", cex=.7, outer=TRUE)
       mtext(side=2, at=.25, line=.001, text="Proportion of Sample Republicans", cex=.7, outer=TRUE)
       mtext(side=2, at=.75, line=.001, text="Proportion of Sample Democrats", cex=.7, outer=TRUE)


dev.off()




###Make additional plots using a loop

cat("Produce Appendix Figures 34-47 ... \n")


names.plots<-c("Sort as Dem.",
"Sort as Dem., Top 25 MSA", "Sort as Dem., South", "Sort as Dem., Northeast",
"Sort as Dem., West","Sort as Dem., Midwest","Sort as Dem., Affordable", "Sort as Dem., 
Preserve Level Owner Occupied", "Sort as Dem., 
Preserve Level Owner Occupied, Affordable", "Sort as Dem., 
Preserve Level Owner Occupied, Affordable, in West", "Sort as Dem.,
Preserve Level Owner Occupied, Affordable, in Midest", "Sort as Dem.,
Preserve Level Owner Occupied, Affordable, in South", "Sort as Dem.,
Preserve Level Owner Occupied, Affordable, in Northeast", 

"Sort as Dem., Higher Density", "Sort as Rep.", 
"Sort as Dem, Top 25 MSA", "Sort as Rep., South", "Sort as Rep., Northeast",
"Sort as Rep., West","Sort as Rep., Midwest","Sort as Rep., Affordable", "Sort as Rep., 
Preserve Level Owner Occupied", "Sort as Rep., 
Preserve Level Owner Occupied, Affordable", "Sort as Rep., 
Preserve Level Owner Occupied, Affordable, in West", "Sort as Rep.,
Preserve Level Owner Occupied, Affordable, in Midest", "Sort as Rep.,
Preserve Level Owner Occupied, Affordable, in South", "Sort as Rep.,
Preserve Level Owner Occupied, Affordable, in Northeast", 

"Sort as Rep., Higher Density")

length(names.plots)==length(plot.obs)


for(i in 1:length(plot.obs)){
	
	
file.name<-paste(output, names(plot.obs)[[i]],".pdf",sep="")

if(i <=length((plot.obs))/2){
pdf(file.name, paper='special', height=5, width=6)

barplot(plot.obs[[i]], main=names.plots[i]
, ylim=c(0,1), axes=FALSE,  cex.main=.9, col="lightskyblue", space=0 , xlim=c(0,20) ,xlab="Proportion of National Housing Stock Available",ylab="Proportion of Sample Democrats")
  axis(1, at=seq(0,20,1), labels=seq(0., 1,.05))
  axis(2, seq(0, 1, .1)) 
    mtext(side=1, at=0, line=2, text="Difficult to Sort", cex=.8)
    mtext(side=1, at=20, line=2, text="Easy to Sort", cex=.8)
    }
    
if(i >length((plot.obs))/2){
pdf(file.name, paper='special', height=5, width=6)

barplot(plot.obs[[i]], main=names.plots[i]
, ylim=c(0,1), axes=FALSE,  cex.main=.9,  col="red", space=0 , xlim=c(0,20) ,xlab="Proportion of National Housing Stock Available",ylab="Proportion of Sample Republicans")

  axis(1, at=seq(0,20,1), labels=seq(0., 1,.05))
  axis(2, seq(0, 1, .1)) 
    mtext(side=1, at=0, line=2, text="Difficult to Sort", cex=.8)
    mtext(side=1, at=20, line=2, text="Easy to Sort", cex=.8)
    
    }
    dev.off()
        
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    ###########################################################
    
    
    
    
    ##########REPEAT WITH WITHIN-MSA CONSTRAINT
cat("Produce Appendix Figure 33\n")


load("cces_2012_income.Rdata") ##cces.income
load("zip_apsa_imputed22.Rdata")
load("dd.final.Rdata")



dim(dd.final)
dd.final$faminc.house<-dd.final$income.num*3*1000
table(dd.final$faminc.house)

dd.final$faminc.rent<-(dd.final$income.num/12)*.3*1000
table(dd.final$faminc.rent)

dd.final$zipnew<-dd.final$zip
dd.final$zipnew[dd.final$zipnew==""]<-NA
table(nchar(dd.final$zipnew))




head(dd.final)


class(dd.final$ownedvaluek)
table(dd.final$ownedvaluek)
dd.final$house.val<-1000*as.numeric(dd.final$ownedvaluek)
table(dd.final$house.val)

class(dd.final$rentmo)
table(dd.final$rentmo)
dd.final$rentmo[dd.final$rentmo==""]<-NA
dd.final$rent.val<-as.numeric(dd.final$rentmo)
table(dd.final$rent.val)


##did they follow the rule of thumb for three times the price of the house?
dd.final$rule.house<-NA
dd.final$rule.house[dd.final$faminc.house>=dd.final$house.val]<-1
dd.final$rule.house[dd.final$faminc.house<dd.final$house.val]<-0
table(dd.final$rule.house, exclude=NULL)

##did they follow the rule of thumb for rent being 30% of income?
dd.final$rule.rent<-NA
dd.final$rule.rent[dd.final$faminc.rent>=dd.final$rent.val]<-1
dd.final$rule.rent[dd.final$faminc.rent<dd.final$rent.val]<-0
table(dd.final$rule.rent, exclude=NULL)

dd.final$afford<-NA
dd.final$afford[dd.final$rule.house==0 | dd.final$rule.rent==0]<-0
dd.final$afford[dd.final$rule.house==1 | dd.final$rule.rent==1]<-1
table(dd.final$afford, exclude=NULL) ##meeting either the rent or housing rule of thumb


##########

zip$in.dd.final<-0
zip$in.dd.final[zip$zipnew%in%dd.final$zipnew]<-1
table(zip$in.dd.final)

zip.dd.final<-zip[zip$in.dd.final==1,]
dim(zip.dd.final)
names(zip.dd.final)

names(zip)
zip.dd.final<-cbind.data.frame(pctr082=zip.dd.final$pctr082, zipnew=zip.dd.final$zipnew, ba=zip.dd.final$ba, own25=zip.dd.final$ownocc.val.q252, rentmed=zip.dd.final$mediangrossrent2, top25=zip.dd.final$top25,pctown=zip.dd.final$pctowned.5yr.20122, rent=zip.dd.final$mediangrossrent2, popdens.5yr.20122=zip.dd.final$popdens.5yr.20122, msa=zip.dd.final$msa)
dim(zip.dd.final)
head(zip.dd.final)




##### merge zip code data to ssi data by current zip code of respondent
dim(dd.final)
dd.final.zip<-merge(dd.final, zip.dd.final, by="zipnew",all.x=TRUE)
dim(dd.final.zip)
names(dd.final.zip)

##merge in current and former msa for movers
msas.old<-cbind.data.frame(msa.old=zip$msa, zipnew=zip$zipnew)##using complete zip code data file
msas.new<-cbind.data.frame(msa.new=zip$msa, zipnew=zip$zipnew)

table(nchar(dd.final$lastzip.text))##previous zip code
table(nchar(dd.final.zip$zipnew))##current zip code
table(nchar(zip$zipnew))
names(zip)

dim(dd.final.zip)
dd.final.zip<-merge(dd.final.zip, msas.new, by="zipnew", all.x=T)##merge by current zip code
dd.final.zip<-merge(dd.final.zip, msas.old, by.x="lastzip.text",by.y="zipnew", all.x=T)##merge by current zip code
dim(dd.final.zip)



##what percentage of movers stayed in the same MSA
table(!is.na(dd.final.zip$lastzip.text))
table(is.na(dd.final.zip$lastzip.text))
table(dd.final.zip$same.msa, exclude=NULL)
mean(dd.final.zip$same.msa, na.rm=T)
mean(dd.final.zip$same.msa[dd.final.zip$lastzip.text!=dd.final.zip$zipnew], na.rm=T)


##are people who move to different MSAs different than those who move within MSA?
vars<-c("income.num", "white2","single","educ2")
var.names<-c("HH Income ($)","Non-Hispanic White","Single","B.A.")
r<-as.data.frame(matrix(nrow=length(vars), ncol=3))
rownames(r)<-var.names
colnames(r)<-c("Mean (Same MSA)","Mean (Different MSA)","p.value of diff.")

for(i in 1:length(vars)){
t<-t.test(dd.final.zip[dd.final.zip$same.msa==1,vars[i]],dd.final.zip[dd.final.zip$same.msa==0,vars[i]], na.rm=T )
r[i,1]<-t$estimate[1]
r[i,2]<-t$estimate[2]
r[i,3]<-t$p.value
}
r<-signif(r, digits=2)
r
library(xtable)
print(xtable(r,digits=c(0,2,2,4)), row.names=F)

##Answer: they are richer, whiter and more educated



##What percentage of people stayed in the same zip?
table(!is.na(dd.final$lastzip.text))
class(dd.final$lastzip.text)
class(dd.final$zipnew)
table(dd.final$lastzip.text==dd.final$zipnew)

dd.final$same.zip<-NA
dd.final$same.zip[!is.na(dd.final$lastzip.text) & dd.final$lastzip.text==dd.final$zipnew]<-1
dd.final$same.zip[!is.na(dd.final$lastzip.text) & dd.final$lastzip.text!=dd.final$zipnew]<-0
table(dd.final$same.zip, exclude=NULL)
mean(dd.final$same.zip, na.rm=T)


dim(dd.final)
dim(dd.final.zip)



dd.final.zip$dem<-0
dd.final.zip$dem[dd.final.zip$partywlean=="dem"]<-1
table(dd.final.zip$dem)

dd.final.zip$rep<-0
dd.final.zip$rep[dd.final.zip$partywlean=="rep"]<-1
table(dd.final.zip$rep)


dim(dd.final.zip) ###final data to create histograms





##make storage objects
dd.final.zip$zip.poss.dems<-NA
dd.final.zip$zip.poss.dems.ed<-NA
dd.final.zip$zip.poss.dems.msa<-NA
dd.final.zip$zip.poss.dems.south<-NA
dd.final.zip$zip.poss.dems.northeast<-NA
dd.final.zip$zip.poss.dems.west<-NA
dd.final.zip$zip.poss.dems.midwest<-NA
dd.final.zip$zip.poss.dems.aff<-NA
dd.final.zip$zip.poss.dems.owner<-NA
dd.final.zip$zip.poss.dems.density<-NA
dd.final.zip$zip.poss.dems.owner.aff<-NA
dd.final.zip$zip.poss.dems.owner.aff.west<-NA
dd.final.zip$zip.poss.dems.owner.aff.midwest<-NA
dd.final.zip$zip.poss.dems.owner.aff.south<-NA
dd.final.zip$zip.poss.dems.owner.aff.northeast<-NA

dd.final.zip$zip.poss.dems.ed.aff<-NA
dd.final.zip$zip.poss.dems.ed.aff.msa<-NA
dd.final.zip$zip.poss.dems.ed.aff.south<-NA
dd.final.zip$zip.poss.dems.ed.aff.west<-NA
dd.final.zip$zip.poss.dems.ed.aff.midwest<-NA
dd.final.zip$zip.poss.dems.ed.aff.northeast<-NA


dd.final.zip$zip.poss.reps<-NA
dd.final.zip$zip.poss.reps.ed<-NA
dd.final.zip$zip.poss.reps.msa<-NA
dd.final.zip$zip.poss.reps.south<-NA
dd.final.zip$zip.poss.reps.northeast<-NA
dd.final.zip$zip.poss.reps.west<-NA
dd.final.zip$zip.poss.reps.midwest<-NA
dd.final.zip$zip.poss.reps.aff<-NA
dd.final.zip$zip.poss.reps.owner<-NA
dd.final.zip$zip.poss.reps.owner.aff<-NA

dd.final.zip$zip.poss.reps.owner.aff.west<-NA
dd.final.zip$zip.poss.reps.owner.aff.midwest<-NA
dd.final.zip$zip.poss.reps.owner.aff.south<-NA
dd.final.zip$zip.poss.reps.owner.aff.northeast<-NA

dd.final.zip$zip.poss.reps.owner.west<-NA
dd.final.zip$zip.poss.reps.density<-NA
dd.final.zip$zip.poss.reps.ed.aff<-NA
dd.final.zip$zip.poss.reps.ed.aff.msa<-NA
dd.final.zip$zip.poss.reps.ed.aff.south<-NA
dd.final.zip$zip.poss.reps.ed.aff.west<-NA
dd.final.zip$zip.poss.reps.ed.aff.midwest<-NA
dd.final.zip$zip.poss.reps.ed.aff.northeast<-NA


dd.final.zip$total.msa.units<-NA

##CONSTRAIN PEOPLE TO MOVE TO SAME MSA, then report percent of within-MSA housing units available

##remove missings on zip code variables
dd.final.zip<-dd.final.zip[ !is.na(dd.final.zip$zipnew) & !is.na(dd.final.zip$faminc.house) & !is.na(dd.final.zip$pctr082) & !is.na(dd.final.zip$popdens.5yr.20122) & !is.na(dd.final.zip$pctown) &  !is.na(dd.final.zip$top25)&!is.na(dd.final.zip$msa), ]

dim(dd.final.zip)

zip<-zip[ !is.na(zip$zipnew)  &  !is.na(zip$ownocc.val.q252) & !is.na(zip$popdens.5yr.20122 ) & !is.na(zip$pctowned.5yr.20122) &  !is.na(zip$top25) & !is.na(zip$south) & !is.na(zip$northeast) & !is.na(zip$west) & !is.na(zip$midwest) &!is.na(zip$msa), ]

dim(zip)





###loop to compute housing options under various constraints
for(i in 1:nrow(dd.final.zip)){


##total units
	dd.final.zip$total.msa.units[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]])


##DEMS	

if(dd.final.zip$dem[i]==1  ) {
	
	
##more dem
dd.final.zip$zip.poss.dems[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]>zip$pctr082  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current)


##more dem + in top 25 msa
dd.final.zip$zip.poss.dems.msa[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]>zip$pctr082 & zip$top25==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in top25 MSA)

##more dem + in south
dd.final.zip$zip.poss.dems.south[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 & zip$south==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in south)

##more dem + in northeast
dd.final.zip$zip.poss.dems.northeast[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 & zip$northeast==1  ] , na.rm=TRUE)##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in northeast)


##more dem + in west
dd.final.zip$zip.poss.dems.west[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 & zip$west==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in west)

##more dem + in midwest
dd.final.zip$zip.poss.dems.midwest[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]>zip$pctr082 & zip$midwest==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in midwest)

##more dem + affordable
dd.final.zip$zip.poss.dems.aff[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


##more dem + at least as high on pct owner occupied
dd.final.zip$zip.poss.dems.owner[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable
dd.final.zip$zip.poss.dems.owner.aff[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 & zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  & dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the West
dd.final.zip$zip.poss.dems.owner.aff.west[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$west==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the midwest
dd.final.zip$zip.poss.dems.owner.aff.midwest[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$midwest==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the south
dd.final.zip$zip.poss.dems.owner.aff.south[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$south==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the northeast
dd.final.zip$zip.poss.dems.owner.aff.northeast[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$northeast==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


##more dem + higher density than current (for dems)
dd.final.zip$zip.poss.dems.density[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$popdens.5yr.20122[i]<zip$popdens.5yr.20122  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


}

##REPS	
	
if(dd.final.zip$rep[i]==1){	

##more rep
dd.final.zip$zip.poss.reps[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]<zip$pctr082  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current)

##more rep + in top 25 msa
dd.final.zip$zip.poss.reps.msa[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]<zip$pctr082 & zip$top25==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in top25 MSA)

##more rep + in south
dd.final.zip$zip.poss.reps.south[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]<zip$pctr082 & zip$south==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in south)

##more rep + in northeast
dd.final.zip$zip.poss.reps.northeast[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]<zip$pctr082 & zip$northeast==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in northeast)


##more rep + in west
dd.final.zip$zip.poss.reps.west[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]<zip$pctr082 & zip$west==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in west)

##more rep + in midwest
dd.final.zip$zip.poss.reps.midwest[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]<zip$pctr082 & zip$midwest==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in midwest)

##more rep + affordable
dd.final.zip$zip.poss.reps.aff[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)


##more rep + at least as high on pct owner occupied
dd.final.zip$zip.poss.reps.owner[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable
dd.final.zip$zip.poss.reps.owner.aff[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the West
dd.final.zip$zip.poss.reps.owner.aff.west[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]& dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$west==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


##more rep + at least as high on pct owner occupied & affordable in the midest
dd.final.zip$zip.poss.reps.owner.aff.midwest[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$midwest==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the south
dd.final.zip$zip.poss.reps.owner.aff.south[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$south==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the northeast
dd.final.zip$zip.poss.reps.owner.aff.northeast[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$northeast==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)




##more rep + lower density than current (for reps)
dd.final.zip$zip.poss.reps.density[i]<-sum(zip$housing[zip$msa==dd.final.zip$msa[i]&dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$popdens.5yr.20122[i]>zip$popdens.5yr.20122  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)

}


}

	

table(nchar(dd.final.zip$zipnew))

dd.final.zip2<-dd.final.zip

dim(dd.final.zip2)
head(dd.final.zip2)

save(dd.final.zip2, file=paste(output,"move_poss_ssi2_same_msa.Rdata",sep="" ))


load(paste(output,"move_poss_ssi2_same_msa.Rdata",sep="" ))


##check for missingness on zipcode covariates
table(!is.na(zip$pctr082))

table(!is.na(zip$zipnew))

table(!is.na(zip$ba))

table(!is.na(zip$ownocc.val.q252))

table(!is.na(zip$popdens.5yr.20122 ))

table(!is.na(zip$pctowned.5yr.20122))

table(!is.na(zip$top25))

table(!is.na(zip$south))

table(!is.na(zip$northeast))

table(!is.na(zip$west))

table(!is.na(zip$midwest))


##about as many missings as members of other party
table(dd.final.zip2$dem)
table(is.na(dd.final.zip2$zip.poss.reps.density))

##about as many missings as members of other party
table(dd.final.zip2$rep)
table(is.na(dd.final.zip2$zip.poss.dems.northeast))


var.names<-c("zip.poss.dems", "zip.poss.dems.msa", "zip.poss.dems.south", "zip.poss.dems.northeast", "zip.poss.dems.west", "zip.poss.dems.midwest", "zip.poss.dems.aff", "zip.poss.dems.owner", "zip.poss.dems.owner.aff", "zip.poss.dems.owner.aff.west", "zip.poss.dems.owner.aff.midwest", "zip.poss.dems.owner.aff.south", "zip.poss.dems.owner.aff.northeast",   
"zip.poss.dems.density","zip.poss.reps",  "zip.poss.reps.msa", "zip.poss.reps.south", "zip.poss.reps.northeast", "zip.poss.reps.west", "zip.poss.reps.midwest", "zip.poss.reps.aff", "zip.poss.reps.owner", "zip.poss.reps.owner.aff", "zip.poss.reps.owner.aff.west", "zip.poss.reps.owner.aff.midwest", "zip.poss.reps.owner.aff.south", "zip.poss.reps.owner.aff.northeast","zip.poss.reps.density")
	
	
for(i in 1:length(var.names)){
	
	print(summary(dd.final.zip2[,var.names[i]]))
	
}



# # # # 

# # # ##analyze
 # # # ##spot check

test<-NA

##take 1000 draws, see if all are successful
for(i in 1:1000){
draw<-sample(1:nrow(dd.final.zip2), size=1)

if(dd.final.zip2$rep[i]==1){
	
test[i]<-I(sum(zip$housing[zip$msa==dd.final.zip2$msa[draw]& dd.final.zip2$pctr082[draw]<zip$pctr082  ] , na.rm=TRUE)==dd.final.zip2$zip.poss.reps[draw])

}

if(dd.final.zip2$dem[i]==1){
	
test[i]<-I(sum(zip$housing[zip$msa==dd.final.zip2$msa[draw]& dd.final.zip2$pctr082[draw]>zip$pctr082  ] , na.rm=TRUE)==dd.final.zip2$zip.poss.dems[draw])

}


}
table(test) ##perfect.




# # # ###end spot check




	##UPDATED TO BE TOTAL HOUSING UNITS WITHIN MSA

	##compute proportions of within-MSA housing stock
	for(i in 1:length(var.names)){
		
		dd.final.zip2[,paste(var.names[i],".prop",sep="")]<-dd.final.zip2[,var.names[i]] / dd.final.zip2$total.msa.units
		
	}
	
	
plot.vars<-c("zip.poss.dems.prop"  ,
 "zip.poss.dems.msa.prop"      ,          "zip.poss.dems.south.prop"      ,        "zip.poss.dems.northeast.prop"         ,
 "zip.poss.dems.west.prop"   ,            "zip.poss.dems.midwest.prop"       ,     "zip.poss.dems.aff.prop"            ,   
"zip.poss.dems.owner.prop"         ,   "zip.poss.dems.owner.aff.prop",  "zip.poss.dems.owner.aff.west.prop"  ,"zip.poss.dems.owner.aff.midwest.prop"  ,"zip.poss.dems.owner.aff.south.prop"  ,"zip.poss.dems.owner.aff.northeast.prop"  ,

 "zip.poss.dems.density.prop"       ,    
 
 "zip.poss.reps.prop"     ,     "zip.poss.reps.msa.prop"        ,        "zip.poss.reps.south.prop"     ,        
 "zip.poss.reps.northeast.prop"      ,    "zip.poss.reps.west.prop"        ,       "zip.poss.reps.midwest.prop"          , 
"zip.poss.reps.aff.prop"         ,       "zip.poss.reps.owner.prop"     ,   "zip.poss.reps.owner.aff.prop",  "zip.poss.reps.owner.aff.west.prop"  ,"zip.poss.reps.owner.aff.midwest.prop"  ,"zip.poss.reps.owner.aff.south.prop"  ,"zip.poss.reps.owner.aff.northeast.prop"  ,"zip.poss.reps.density.prop"          )


##get total number of partisans in sample
total.dems<-dim(dd.final.zip2[dd.final.zip2$dem==1 & !is.na(dd.final.zip2$zipnew)& !is.na(dd.final.zip2$faminc.house) & !is.na(dd.final.zip2$pctr082) & !is.na(dd.final.zip2$popdens.5yr.20122) & !is.na(dd.final.zip2$pctown) &  !is.na(dd.final.zip2$top25) & !is.na(dd.final.zip2$msa),])[1]

total.reps<-dim(dd.final.zip2[dd.final.zip2$rep==1 & !is.na(dd.final.zip2$zipnew)& !is.na(dd.final.zip2$faminc.house) & !is.na(dd.final.zip2$pctr082)  & !is.na(dd.final.zip2$popdens.5yr.20122) & !is.na(dd.final.zip2$pctown) &  !is.na(dd.final.zip2$top25)& !is.na(dd.final.zip2$msa),])[1]


bars<-paste("bar.",var.names,sep="")##make names for bar charts
denoms<-c(rep(total.dems, 16), rep(total.reps, 16)) ##make vector of denominators

plot.obs<-list(NA)


bins<-seq(0,1,.05)

##store proportions in a list for each variable

#dems
for(i in 1:(length(bars)/2)  )  {


	plot.obs[[i]]<-assign(bars[i], hist(dd.final.zip2[dd.final.zip2$dem==1,plot.vars[i]], breaks=bins, plot=F)$counts / total.dems)
}


#reps
for(i in ((length(bars)/2)+1): length(bars) )  {
	plot.obs[[i]]<-assign(bars[i], hist(dd.final.zip2[dd.final.zip2$rep==1,plot.vars[i]], breaks=bins, plot=F)$counts / total.reps)
}

names(plot.obs)<-bars




file.name<-paste(output, "dr.panel_within_msa",".pdf",sep="")
pdf(file.name, paper='special', height=5, width=7)

par(mfrow=c(2,4) , oma=c(1.2,1.2,0,0))

barplot(plot.obs$bar.zip.poss.dems, main="Sort as Dem."
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
  
barplot(plot.obs$bar.zip.poss.dems.aff, main= "Sort as Dem., Affordable"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
   
barplot(plot.obs$bar.zip.poss.dems.owner.aff, main= "Sort as Dem., Affordable
Preserve Quality"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)

barplot(plot.obs$bar.zip.poss.dems.owner.aff.west, main=  "Sort as Dem., Affordable, 
Preserve Quality, in West"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
  
  
   barplot(plot.obs$bar.zip.poss.reps, main="Sort as Rep."
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)

barplot(plot.obs$bar.zip.poss.reps.aff, main= "Sort as Rep., Affordable"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
   
barplot(plot.obs$bar.zip.poss.reps.owner.aff, main= "Sort as Rep., Affordable
Preserve Quality"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)


barplot(plot.obs$bar.zip.poss.reps.owner.aff.west, main=  "Sort as Rep., Affordable, 
Preserve Quality, in West"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
       mtext(side=1, at=.5, line=.001, text="Proportion of Available Housing Stock Within MSA", cex=.7, outer=TRUE)
       mtext(side=2, at=.25, line=.001, text="Proportion of Sample Republicans", cex=.7, outer=TRUE)
       mtext(side=2, at=.75, line=.001, text="Proportion of Sample Democrats", cex=.7, outer=TRUE)


dev.off()


    

    
    
    
    
    
    
    
    
        
        cat("Produce Appendix Figures 48-55 ... \n")

        
    

  ##### MSA FIGURES in Appendix
   #####FIGURES

dim(zip)
zip2<-zip[zip$top25==1 & !is.na(zip$msa), ]
dim(zip2)
table(zip2$msa, exclude=NULL)


msas<-unique(na.omit(zip2$msa))
msas2<-gsub("--","-",msas)



##### ALL ZIPS IN MSA

bin.weighted.freqs<-list(NA)
number.zips<-list(NA)

for(i in 1:length(msas)){
sum<-NA
sum.zips<-NA

for(j in 1:20){

sum[j]<-sum( zip2$share[zip2$msa==msas[i] & !is.na(zip2$pctr082) & zip2$q20==j &   !is.na(zip2$msa)], na.rm=TRUE)
sum.zips[j]<-length(zip2$zipnew[zip2$msa==msas[i]   & !is.na(zip2$pctr082) & zip2$q20==j &   !is.na(zip2$msa)]   )

}
bin.weighted.freqs[[i]]<-sum
number.zips[[i]]<-sum.zips

}

names(bin.weighted.freqs)<-msas2[!is.na(msas2)]
names(number.zips)<-msas2[!is.na(msas2)]

sum(bin.weighted.freqs[[1]][1:20], na.rm=TRUE)

##spot check
bin.weighted.freqs[[2]]




dem.zips1<-NA
mixed.zips<-NA
rep.zips1<-NA
number.zipsreps<-NA
number.zipsdems<-NA

for(i in 1:length(msas)){
	
	##dem zips
dem.zips1[i]<-round( sum(bin.weighted.freqs[[i]][1:8], na.rm=TRUE) , digits=10)
number.zipsdems[i]<-sum(number.zips[[i]][1:8], na.rm=TRUE) 

##mixed zips
mixed.zips[i]<-round( sum(bin.weighted.freqs[[i]][9:12], na.rm=TRUE) , digits=10)

##republican zips
rep.zips1[i]<-round( sum(bin.weighted.freqs[[i]][13:20], na.rm=TRUE) , digits=10)
number.zipsreps[i]<-sum(number.zips[[i]][13:20], na.rm=TRUE) 

}

names(dem.zips1)<-msas2
names(rep.zips1)<-msas2
names(number.zipsdems)<-msas2
names(number.zipsreps)<-msas2


dem.zips2<-dem.zips1[order(dem.zips1, decreasing=TRUE)]
number.zipsdems2<-number.zipsdems[names(dem.zips2)]##put in same order as proportions
zero.dems<-which(number.zipsdems2 ==0)
names(zero.dems)<-names(number.zipsdems2[number.zipsdems2 ==0])
zero.dems.names<-nchar(names(zero.dems))

rep.zips2<-rep.zips1[order(rep.zips1, decreasing=TRUE)]
number.zipsreps2<-number.zipsreps[names(rep.zips2)]
number.zipsreps.labs<-NA
zero.reps<-which(number.zipsreps2 ==0)
names(zero.reps)<-names(number.zipsreps2[number.zipsreps2 ==0])
zero.reps.names<-nchar(names(zero.reps))




dem.zips2_blank<-rep(0, length(dem.zips2))
names(dem.zips2_blank)<-names(dem.zips2)

rep.zips2_blank<-rep(0, length(rep.zips2))
names(rep.zips2_blank)<-names(rep.zips2)


file.name<-paste(output,"top25all_dem.pdf",sep="")
pdf(file.name, paper='special', height=6, width=7)
par(mar=c(3,17.3,3,4), xpd=NA)
bar<-barplot(dem.zips2, horiz=T,  col=c("lightskyblue"), xlim=c(0, 1.5), axes=FALSE, las=2, cex.axis=.5, cex.names=.8, space=.3, main="Proportion of MSA Housing in Landslide Dem. Zip Codes", cex.main=.95)
axis(1, at=seq(0,1,.1))
mtext(side=1, line=2, text="Proportion of MSA's Housing Units")
dev.off()



file.name<-paste(output,"top25all_rep.pdf",sep="")
pdf(file.name, paper='special', height=6, width=7)
par(mar=c(3,17.3,3,4),xpd=NA)
barplot(rep.zips2, horiz=T,  col=c("red"), xlim=c(0, 1), axes=FALSE, las=2, cex.axis=.5, cex.names=.8, space=.3, main="Proportion of MSA Housing in Landslide Rep. Zip Codes", cex.main=.95)
axis(1, at=seq(0,1,.1))
mtext(side=1, line=2, text="Proportion of MSA's Housing Units")
segments(x0=-.05, y0=bar[zero.reps],x1=c(-0.87, -0.92), bar[zero.reps], y1=bar[zero.reps], col="red", lty=1, lwd=1)
dev.off()



##### Owner Occupied restriction 

bin.weighted.freqs<-list(NA)
number.zips<-list(NA)

##define restriction
occ.quants<-quantile(zip$pctowned.5yr.20122, probs=c(0,.25,.5,.75,1))
min.occ<-occ.quants["50%"]

for(i in 1:length(msas)){
sum<-NA
sum.zips<-NA

for(j in 1:20){

sum[j]<-sum( zip2$share[zip2$msa==msas[i] & !is.na(zip2$pctr082) & zip2$q20==j & zip2$pctowned.5yr.20122 > min.occ&  !is.na(zip2$msa)], na.rm=TRUE)
sum.zips[j]<-length(zip2$zipnew[zip2$msa==msas[i]   & !is.na(zip2$pctr082) & zip2$q20==j &  zip2$pctowned.5yr.20122>min.occ & !is.na(zip2$msa)]   )

}
bin.weighted.freqs[[i]]<-sum
number.zips[[i]]<-sum.zips

}

names(bin.weighted.freqs)<-msas
names(number.zips)<-msas

sum(bin.weighted.freqs[[1]][1:20], na.rm=TRUE)

##spot check
bin.weighted.freqs[[2]]




dem.zips1<-NA
mixed.zips<-NA
rep.zips1<-NA
number.zipsreps<-NA
number.zipsdems<-NA

for(i in 1:length(msas)){
	
	##dem zips
dem.zips1[i]<- sum(bin.weighted.freqs[[i]][1:8], na.rm=TRUE) 
number.zipsdems[i]<-sum(number.zips[[i]][1:8], na.rm=TRUE) 

##mixed zips
mixed.zips[i]<-sum(bin.weighted.freqs[[i]][9:12], na.rm=TRUE) 

##republican zips
rep.zips1[i]<- sum(bin.weighted.freqs[[i]][13:20], na.rm=TRUE) 
number.zipsreps[i]<-sum(number.zips[[i]][13:20], na.rm=TRUE) 

}

names(dem.zips1)<-msas2
names(rep.zips1)<-msas2
names(number.zipsdems)<-msas2
names(number.zipsreps)<-msas2



dem.zips2<-dem.zips1[order(dem.zips1, decreasing=TRUE)]
number.zipsdems2<-number.zipsdems[names(dem.zips2)]##put in same order as proportions
zero.dems<-which(number.zipsdems2 ==0)
names(zero.dems)<-names(number.zipsdems2[number.zipsdems2 ==0])
zero.dems.names<-nchar(names(zero.dems))

rep.zips2<-rep.zips1[order(rep.zips1, decreasing=TRUE)]
number.zipsreps2<-number.zipsreps[names(rep.zips2)]
number.zipsreps.labs<-NA
zero.reps<-which(number.zipsreps2 ==0)
names(zero.reps)<-names(number.zipsreps2[number.zipsreps2 ==0])
zero.reps.names<-nchar(names(zero.reps))


dem.zips2_blank<-rep(0, length(dem.zips2))
names(dem.zips2_blank)<-names(dem.zips2)

rep.zips2_blank<-rep(0, length(rep.zips2))
names(rep.zips2_blank)<-names(rep.zips2)



file.name<-paste(output,"top25occ.dem.pdf",sep="")
pdf(file.name, paper='special', height=6, width=7)
par(mar=c(3,17.3,3,4), xpd=NA)
bar<-barplot(dem.zips2, horiz=T,  col=c("lightskyblue"), xlim=c(0, 1.5), axes=FALSE, las=2, cex.axis=.5, cex.names=.8, space=.3, main="Proportion of MSA Housing in Landslide Dem. Zip Codes
>Median %Owner Occupied Housing", cex.main=.95)
axis(1, at=seq(0,1,.1))
mtext(side=1, line=2, text="Proportion of MSA's Housing Units")
segments(x0=-.07, y0=bar[zero.dems],x1=-.042*zero.dems.names,  y1=bar[zero.dems], col="red", lty=1, lwd=1)
dev.off()





file.name<-paste(output,"top25occ.rep.pdf",sep="")
pdf(file.name, paper='special', height=6, width=7)
par(mar=c(3,17.3,3,4),xpd=NA)
barplot(rep.zips2, horiz=T,  col=c("red"), xlim=c(0, 1), axes=FALSE, las=2, cex.axis=.5, cex.names=.8, space=.3, main="Proportion of MSA Housing in Landslide Rep. Zip Codes
>Median %Owner Occupied Housing", cex.main=.95)
axis(1, at=seq(0,1,.1))
mtext(side=1, line=2, text="Proportion of MSA's Housing Units")
#text(x=rep.zips+.15, y=bar, labels= number.zipsreps.labs, cex=.8)
segments(x0=-.05, y0=bar[zero.reps],x1=c(-0.73, -0.87, -0.92),  y1=bar[zero.reps], col="red", lty=1, lwd=1)
dev.off()





##### Owner Occ restriction + affordability
##DEMS
bin.weighted.freqs<-list(NA)
number.zips<-list(NA)

##define restriction
occ.quants<-quantile(zip$pctowned.5yr.20122, probs=c(0,.25,.5,.75,1))
min.occ<-occ.quants["50%"]

for(i in 1:length(msas)){
sum<-NA
sum.zips<-NA

for(j in 1:20){

sum[j]<-sum( zip2$share[zip2$msa==msas[i] & !is.na(zip2$pctr082) & zip2$q20==j & zip2$pctowned.5yr.20122 > min.occ&  !is.na(zip2$msa) & zip2$dem.afford.house25>.2], na.rm=TRUE)
sum.zips[j]<-length(zip2$zipnew[zip2$msa==msas[i]   & !is.na(zip2$pctr082) & zip2$q20==j &  zip2$pctowned.5yr.20122>min.occ & !is.na(zip2$msa) & zip2$dem.afford.house25>.2]   )

}
bin.weighted.freqs[[i]]<-sum
number.zips[[i]]<-sum.zips

}

names(bin.weighted.freqs)<-msas
names(number.zips)<-msas

sum(bin.weighted.freqs[[1]][1:20], na.rm=TRUE)

##spot check
bin.weighted.freqs[[2]]




dem.zips1<-NA
mixed.zips<-NA
rep.zips1<-NA
number.zipsreps<-NA
number.zipsdems<-NA

for(i in 1:length(msas)){
	
	##dem zips
dem.zips1[i]<- sum(bin.weighted.freqs[[i]][1:8], na.rm=TRUE) 
number.zipsdems[i]<-sum(number.zips[[i]][1:8], na.rm=TRUE) 

##mixed zips
mixed.zips[i]<-sum(bin.weighted.freqs[[i]][9:12], na.rm=TRUE) 

##republican zips
rep.zips1[i]<- sum(bin.weighted.freqs[[i]][13:20], na.rm=TRUE) 
number.zipsreps[i]<-sum(number.zips[[i]][13:20], na.rm=TRUE) 

}

names(dem.zips1)<-msas2
names(number.zipsdems)<-msas2



dem.zips2<-dem.zips1[order(dem.zips1, decreasing=TRUE)]
number.zipsdems2<-number.zipsdems[names(dem.zips2)]##put in same order as proportions
zero.dems<-which(number.zipsdems2 ==0)
names(zero.dems)<-names(number.zipsdems2[number.zipsdems2 ==0])
zero.dems.names<-nchar(names(zero.dems))


dem.zips2_blank<-rep(0, length(dem.zips2))
names(dem.zips2_blank)<-names(dem.zips2)



file.name<-paste(output,"top25occ.aff.dem.pdf",sep="")
pdf(file.name, paper='special', height=6, width=7)
par(mar=c(3,17.3,3,4), xpd=NA)
bar<-barplot(dem.zips2, horiz=T,  col=c("lightskyblue"), xlim=c(0, 1.5), axes=FALSE, las=2, cex.axis=.5, cex.names=.8, space=.3, main="Proportion of MSA Housing in Landslide Dem. Zip Codes
>Median %Owner Occupied Housing, \n > 20% Affordable Housing", cex.main=.95)
axis(1, at=seq(0,1,.1))
mtext(side=1, line=2, text="Proportion of MSA's Housing Units")
#text(x=dem.zips+.15, y=bar, labels= number.zipsdems.labs, cex=.8)
segments(x0=-.07, y0=bar[zero.dems],x1=-.042*zero.dems.names,  y1=bar[zero.dems], col="red", lty=1, lwd=1)
dev.off()


##REPS
bin.weighted.freqs<-list(NA)
number.zips<-list(NA)

##define restriction
occ.quants<-quantile(zip$pctowned.5yr.20122, probs=c(0,.25,.5,.75,1))
min.occ<-occ.quants["50%"]

for(i in 1:length(msas)){
sum<-NA
sum.zips<-NA

for(j in 1:20){

sum[j]<-sum( zip2$share[zip2$msa==msas[i] & !is.na(zip2$pctr082) & zip2$q20==j & zip2$pctowned.5yr.20122 > min.occ&  !is.na(zip2$msa) & zip2$rep.afford.house25>.2], na.rm=TRUE)
sum.zips[j]<-length(zip2$zipnew[zip2$msa==msas[i]   & !is.na(zip2$pctr082) & zip2$q20==j &  zip2$pctowned.5yr.20122>min.occ & !is.na(zip2$msa) & zip2$rep.afford.house25>.2]   )

}
bin.weighted.freqs[[i]]<-sum
number.zips[[i]]<-sum.zips

}

names(bin.weighted.freqs)<-msas
names(number.zips)<-msas

sum(bin.weighted.freqs[[1]][1:20], na.rm=TRUE)

##spot check
bin.weighted.freqs[[2]]




dem.zips1<-NA
mixed.zips<-NA
rep.zips1<-NA
number.zipsreps<-NA
number.zipsdems<-NA

for(i in 1:length(msas)){
	
	##dem zips
dem.zips1[i]<- sum(bin.weighted.freqs[[i]][1:8], na.rm=TRUE) 
number.zipsdems[i]<-sum(number.zips[[i]][1:8], na.rm=TRUE) 

##mixed zips
mixed.zips[i]<-sum(bin.weighted.freqs[[i]][9:12], na.rm=TRUE) 

##republican zips
rep.zips1[i]<- sum(bin.weighted.freqs[[i]][13:20], na.rm=TRUE) 
number.zipsreps[i]<-sum(number.zips[[i]][13:20], na.rm=TRUE) 

}

names(rep.zips1)<-msas2
names(number.zipsreps)<-msas2


rep.zips2<-rep.zips1[order(rep.zips1, decreasing=TRUE)]
number.zipsreps2<-number.zipsreps[names(rep.zips2)]
number.zipsreps.labs<-NA
zero.reps<-which(number.zipsreps2 ==0)
names(zero.reps)<-names(number.zipsreps2[number.zipsreps2 ==0])
zero.reps.names<-nchar(names(zero.reps))



rep.zips2_blank<-rep(0, length(rep.zips2))
names(rep.zips2_blank)<-names(rep.zips2)





file.name<-paste(output,"top25occ.aff.rep.pdf",sep="")
pdf(file.name, paper='special', height=6, width=7)
par(mar=c(3,17.3,3,4),xpd=NA)
barplot(rep.zips2, horiz=T,  col=c("red"), xlim=c(0, 1), axes=FALSE, las=2, cex.axis=.5, cex.names=.8, space=.3, main="Proportion of MSA Housing in Landslide Rep. Zip Codes
>Median %Owner Occupied Housing, \n > 20% Affordable Housing", cex.main=.95)
axis(1, at=seq(0,1,.1))
mtext(side=1, line=2, text="Proportion of MSA's Housing Units")
#text(x=rep.zips+.15, y=bar, labels= number.zipsreps.labs, cex=.8)
segments(x0=-.05, y0=bar[zero.reps],x1=-.028*zero.reps.names,  y1=bar[zero.reps], col="red", lty=1, lwd=1)
dev.off()





##### Owner Occ restriction + affordability + density
##DEMS
bin.weighted.freqs<-list(NA)
number.zips<-list(NA)

##define restriction
occ.quants<-quantile(zip$pctowned.5yr.20122, probs=c(0,.25,.5,.75,1))
min.occ<-occ.quants["50%"]

for(i in 1:length(msas)){
sum<-NA
sum.zips<-NA

for(j in 1:20){

sum[j]<-sum( zip2$share[zip2$msa==msas[i] & !is.na(zip2$pctr082) & zip2$q20==j & zip2$pctowned.5yr.20122 > min.occ&  !is.na(zip2$msa) & zip2$dem.afford.house25>.2&zip2$dens5k==1], na.rm=TRUE)
sum.zips[j]<-length(zip2$zipnew[zip2$msa==msas[i]   & !is.na(zip2$pctr082) & zip2$q20==j &  zip2$pctowned.5yr.20122>min.occ & !is.na(zip2$msa) & zip2$dem.afford.house25>.2&zip2$dens5k==1]   )

}
bin.weighted.freqs[[i]]<-sum
number.zips[[i]]<-sum.zips

}

names(bin.weighted.freqs)<-msas
names(number.zips)<-msas

sum(bin.weighted.freqs[[1]][1:20], na.rm=TRUE)

##spot check
bin.weighted.freqs[[2]]




dem.zips1<-NA
mixed.zips<-NA
rep.zips1<-NA
number.zipsreps<-NA
number.zipsdems<-NA

for(i in 1:length(msas)){
	
	##dem zips
dem.zips1[i]<- sum(bin.weighted.freqs[[i]][1:8], na.rm=TRUE) 
number.zipsdems[i]<-sum(number.zips[[i]][1:8], na.rm=TRUE) 

##mixed zips
mixed.zips[i]<-sum(bin.weighted.freqs[[i]][9:12], na.rm=TRUE) 

##republican zips
rep.zips1[i]<- sum(bin.weighted.freqs[[i]][13:20], na.rm=TRUE) 
number.zipsreps[i]<-sum(number.zips[[i]][13:20], na.rm=TRUE) 

}

names(dem.zips1)<-msas2
names(number.zipsdems)<-msas2



dem.zips2<-dem.zips1[order(dem.zips1, decreasing=TRUE)]
number.zipsdems2<-number.zipsdems[names(dem.zips2)]
number.zipsdems.labs<-NA
zero.dems<-which(number.zipsdems2 ==0)
names(zero.dems)<-names(number.zipsdems2[number.zipsdems2 ==0])
zero.dems.names<-nchar(names(zero.dems))


dem.zips2_blank<-rep(0, length(dem.zips2))
names(dem.zips2_blank)<-names(dem.zips2)



file.name<-paste(output,"top25occ.aff.dens.dem.pdf",sep="")
pdf(file.name, paper='special', height=6, width=7)
par(mar=c(3,17.3,3,4), xpd=NA)
bar<-barplot(dem.zips2, horiz=T,  col=c("lightskyblue"), xlim=c(0, 1.5), axes=FALSE, las=2, cex.axis=.5, cex.names=.8, space=.3, main="Proportion of MSA Housing in Landslide Dem. Zip Codes
>Median %Owner Occupied Housing, \n > 20% Affordable Housing, High Density", cex.main=.95)
axis(1, at=seq(0,1,.1))
mtext(side=1, line=2, text="Proportion of MSA's Housing Units")
#text(x=dem.zips+.15, y=bar, labels= number.zipsdems.labs, cex=.8)
segments(x0=-.07, y0=bar[zero.dems],x1=-.042*zero.dems.names,  y1=bar[zero.dems], col="red", lty=1, lwd=1)
dev.off()



##REPS
bin.weighted.freqs<-list(NA)
number.zips<-list(NA)

##define restriction
occ.quants<-quantile(zip$pctowned.5yr.20122, probs=c(0,.25,.5,.75,1))
min.occ<-occ.quants["50%"]

for(i in 1:length(msas)){
sum<-NA
sum.zips<-NA

for(j in 1:20){

sum[j]<-sum( zip2$share[zip2$msa==msas[i] & !is.na(zip2$pctr082) & zip2$q20==j & zip2$pctowned.5yr.20122 > min.occ&  !is.na(zip2$msa) & zip2$rep.afford.house25>.2&zip2$dens5k==0], na.rm=TRUE)
sum.zips[j]<-length(zip2$zipnew[zip2$msa==msas[i]   & !is.na(zip2$pctr082) & zip2$q20==j &  zip2$pctowned.5yr.20122>min.occ & !is.na(zip2$msa) & zip2$rep.afford.house25>.2&zip2$dens5k==0]   )

}
bin.weighted.freqs[[i]]<-sum
number.zips[[i]]<-sum.zips

}

names(bin.weighted.freqs)<-msas
names(number.zips)<-msas

sum(bin.weighted.freqs[[1]][1:20], na.rm=TRUE)

##spot check
bin.weighted.freqs[[2]]




dem.zips1<-NA
mixed.zips<-NA
rep.zips1<-NA
number.zipsreps<-NA
number.zipsdems<-NA

for(i in 1:length(msas)){
	
	##dem zips
dem.zips1[i]<- sum(bin.weighted.freqs[[i]][1:8], na.rm=TRUE) 
number.zipsdems[i]<-sum(number.zips[[i]][1:8], na.rm=TRUE) 

##mixed zips
mixed.zips[i]<-sum(bin.weighted.freqs[[i]][9:12], na.rm=TRUE) 

##republican zips
rep.zips1[i]<- sum(bin.weighted.freqs[[i]][13:20], na.rm=TRUE) 
number.zipsreps[i]<-sum(number.zips[[i]][13:20], na.rm=TRUE) 

}

names(rep.zips1)<-msas2
names(number.zipsreps)<-msas2



rep.zips2<-rep.zips1[order(rep.zips1, decreasing=TRUE)]
number.zipsreps2<-number.zipsreps[names(rep.zips2)]
number.zipsreps.labs<-NA
zero.reps<-which(number.zipsreps2 ==0)
names(zero.reps)<-names(number.zipsreps2[number.zipsreps2 ==0])
zero.reps.names<-nchar(names(zero.reps))



rep.zips2_blank<-rep(0, length(rep.zips2))
names(rep.zips2_blank)<-names(rep.zips2)



file.name<-paste(output,"top25occ.aff.dens.rep.pdf",sep="")
pdf(file.name, paper='special', height=6, width=7)
par(mar=c(3,17.3,3,4),xpd=NA)
barplot(rep.zips2, horiz=T,  col=c("red"), xlim=c(0, 1), axes=FALSE, las=2, cex.axis=.5, cex.names=.8, space=.3, main="Proportion of MSA Housing in Landslide Rep. Zip Codes
>Median %Owner Occupied Housing, \n > 20% Affordable Housing, High Density", cex.main=.95)
axis(1, at=seq(0,1,.1))
mtext(side=1, line=2, text="Proportion of MSA's Housing Units")
#text(x=rep.zips+.15, y=bar, labels= number.zipsreps.labs, cex=.8)
segments(x0=-.05, y0=bar[zero.reps],x1=-.028*zero.reps.names,  y1=bar[zero.reps], col="red", lty=1, lwd=1)
dev.off()



    
    
    
    
           cat("Produce Appendix Figures 32, CCES Results ... \n")
 

    
    ####### REPEAT WITH CCES


    
     
    
load("cces_2012_income.Rdata")
ls()
table(nchar(cces.income$zipnew))

dim(cces.income)
head(cces.income)

zip$in.cces<-0
zip$in.cces[zip$zipnew%in%cces.income$zipnew]<-1
table(zip$in.cces)

zip.dd.final<-zip[zip$in.cces==1,]
dim(zip.dd.final)
names(zip.dd.final)


head(zip.dd.final)
names(zip)
zip.dd.final<-cbind.data.frame(pctr082=zip.dd.final$pctr082, zipnew=zip.dd.final$zipnew, ba=zip.dd.final$ba, own25=zip.dd.final$ownocc.val.q252, rentmed=zip.dd.final$mediangrossrent2, top25=zip.dd.final$top25,pctown=zip.dd.final$pctowned.5yr.20122, rent=zip.dd.final$mediangrossrent2, popdens.5yr.20122=zip.dd.final$popdens.5yr.20122)
dim(zip.dd.final)
head(zip.dd.final)



##merge zip data with cces data by zip of respondent
dim(cces.income)
dd.final.zip <-merge(cces.income, zip.dd.final, by="zipnew",all.x=TRUE)
head(dd.final.zip)
dim(dd.final.zip)

table(nchar(dd.final.zip$zipnew))
table(nchar(zip$zipnew))
names(zip)




##make storage objects
dd.final.zip$zip.poss.dems<-NA
dd.final.zip$zip.poss.dems.ed<-NA
dd.final.zip$zip.poss.dems.msa<-NA
dd.final.zip$zip.poss.dems.south<-NA
dd.final.zip$zip.poss.dems.northeast<-NA
dd.final.zip$zip.poss.dems.west<-NA
dd.final.zip$zip.poss.dems.midwest<-NA
dd.final.zip$zip.poss.dems.aff<-NA
dd.final.zip$zip.poss.dems.owner<-NA
dd.final.zip$zip.poss.dems.density<-NA
dd.final.zip$zip.poss.dems.owner.aff<-NA
dd.final.zip$zip.poss.dems.owner.aff.west<-NA
dd.final.zip$zip.poss.dems.owner.aff.midwest<-NA
dd.final.zip$zip.poss.dems.owner.aff.south<-NA
dd.final.zip$zip.poss.dems.owner.aff.northeast<-NA

dd.final.zip$zip.poss.dems.ed.aff<-NA
dd.final.zip$zip.poss.dems.ed.aff.msa<-NA
dd.final.zip$zip.poss.dems.ed.aff.south<-NA
dd.final.zip$zip.poss.dems.ed.aff.west<-NA
dd.final.zip$zip.poss.dems.ed.aff.midwest<-NA
dd.final.zip$zip.poss.dems.ed.aff.northeast<-NA


dd.final.zip$zip.poss.reps<-NA
dd.final.zip$zip.poss.reps.ed<-NA
dd.final.zip$zip.poss.reps.msa<-NA
dd.final.zip$zip.poss.reps.south<-NA
dd.final.zip$zip.poss.reps.northeast<-NA
dd.final.zip$zip.poss.reps.west<-NA
dd.final.zip$zip.poss.reps.midwest<-NA
dd.final.zip$zip.poss.reps.aff<-NA
dd.final.zip$zip.poss.reps.owner<-NA
dd.final.zip$zip.poss.reps.owner.aff<-NA

dd.final.zip$zip.poss.reps.owner.aff.west<-NA
dd.final.zip$zip.poss.reps.owner.aff.midwest<-NA
dd.final.zip$zip.poss.reps.owner.aff.south<-NA
dd.final.zip$zip.poss.reps.owner.aff.northeast<-NA

dd.final.zip$zip.poss.reps.owner.west<-NA
dd.final.zip$zip.poss.reps.density<-NA
dd.final.zip$zip.poss.reps.ed.aff<-NA
dd.final.zip$zip.poss.reps.ed.aff.msa<-NA
dd.final.zip$zip.poss.reps.ed.aff.south<-NA
dd.final.zip$zip.poss.reps.ed.aff.west<-NA
dd.final.zip$zip.poss.reps.ed.aff.midwest<-NA
dd.final.zip$zip.poss.reps.ed.aff.northeast<-NA



###loop to compute housing options under various constraints

dim(dd.final.zip)
dd.final.zip<-dd.final.zip[!is.na(dd.final.zip$zipnew) & !is.na(dd.final.zip$faminc.house) & !is.na(dd.final.zip$pctr082) &  !is.na(dd.final.zip$popdens.5yr.20122) & !is.na(dd.final.zip$pctown) &  !is.na(dd.final.zip$top25) ,]
dim(dd.final.zip)

dim(zip)
zip<-zip[ !is.na(zip$zipnew) &  !is.na(zip$ownocc.val.q252) & !is.na(zip$popdens.5yr.20122 ) & !is.na(zip$pctowned.5yr.20122) &  !is.na(zip$top25) & !is.na(zip$south) & !is.na(zip$northeast) & !is.na(zip$west) & !is.na(zip$midwest) , ]
dim(zip)


for(i in 1:length(dd.final.zip$zipnew)){

##DEMS	
	
if(dd.final.zip$dem[i]==1  ) {
	
##more dem
dd.final.zip$zip.poss.dems[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current)


##more dem + ed
dd.final.zip$zip.poss.dems.ed[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$ba>=dd.final.zip$ba[i]  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips as high or higher than current zip's BA)


##more dem + in top 25 msa
dd.final.zip$zip.poss.dems.msa[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 & zip$top25==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in top25 MSA)

##more dem + in south
dd.final.zip$zip.poss.dems.south[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$south==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in south)

##more dem + in northeast
dd.final.zip$zip.poss.dems.northeast[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 & zip$northeast==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in northeast)


##more dem + in west
dd.final.zip$zip.poss.dems.west[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$west==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in west)

##more dem + in midwest
dd.final.zip$zip.poss.dems.midwest[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 & zip$midwest==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; possible zips in midwest)

##more dem + affordable
dd.final.zip$zip.poss.dems.aff[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


##more dem + at least as high on pct owner occupied
dd.final.zip$zip.poss.dems.owner[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable
dd.final.zip$zip.poss.dems.owner.aff[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  & dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the West
dd.final.zip$zip.poss.dems.owner.aff.west[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$west==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the midwest
dd.final.zip$zip.poss.dems.owner.aff.midwest[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$midwest==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the south
dd.final.zip$zip.poss.dems.owner.aff.south[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$south==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + at least as high on pct owner occupied & affordable in the northeast
dd.final.zip$zip.poss.dems.owner.aff.northeast[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$northeast==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


##more dem + higher density than current (for dems)
dd.final.zip$zip.poss.dems.density[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$popdens.5yr.20122[i]<zip$popdens.5yr.20122  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more dem + ed + affordable
dd.final.zip$zip.poss.dems.ed.aff[i]<-sum(zip$housing[dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)



dd.final.zip$zip.poss.dems.ed.aff.msa[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i] & zip$top25==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable, in top 25 msa)

##more dem + ed + affordable + south
dd.final.zip$zip.poss.dems.ed.aff.south[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]  &zip$south==1] , na.rm=TRUE)		##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable; as high or higher on BA, in south)

##more dem + ed + affordable + northeast
dd.final.zip$zip.poss.dems.ed.aff.northeast[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i] & zip$northeast==1] , na.rm=TRUE)		##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable; as high or higher on BA, in northeast)

##more dem + ed + affordable + west
dd.final.zip$zip.poss.dems.ed.aff.west[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]  & zip$west==1] , na.rm=TRUE)		##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable; as high or higher on BA, in west)

##more dem + ed + affordable + midwest
dd.final.zip$zip.poss.dems.ed.aff.midwest[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]>zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]  & zip$midwest==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable; as high or higher on BA, in midwest)


}

##REPS	
	
if(dd.final.zip$rep[i]==1 ){	

##more rep
dd.final.zip$zip.poss.reps[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current)


##more rep + ed
dd.final.zip$zip.poss.reps.ed[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 & zip$ba>=dd.final.zip$ba[i]  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips as high or higher than current zip's BA)


##more rep + in top 25 msa
dd.final.zip$zip.poss.reps.msa[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 & zip$top25==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in top25 MSA)

##more rep + in south
dd.final.zip$zip.poss.reps.south[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 & zip$south==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in south)

##more rep + in northeast
dd.final.zip$zip.poss.reps.northeast[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 & zip$northeast==1  ] , na.rm=TRUE)##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in northeast)


##more rep + in west
dd.final.zip$zip.poss.reps.west[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 & zip$west==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in west)

##more rep + in midwest
dd.final.zip$zip.poss.reps.midwest[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 & zip$midwest==1  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; possible zips in midwest)

##more rep + affordable
dd.final.zip$zip.poss.reps.aff[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)


##more rep + at least as high on pct owner occupied
dd.final.zip$zip.poss.reps.owner[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable
dd.final.zip$zip.poss.reps.owner.aff[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the West
dd.final.zip$zip.poss.reps.owner.aff.west[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$west==1] , na.rm=TRUE)##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)


##more rep + at least as high on pct owner occupied & affordable in the midest
dd.final.zip$zip.poss.reps.owner.aff.midwest[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$midwest==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the south
dd.final.zip$zip.poss.reps.owner.aff.south[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$south==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)

##more rep + at least as high on pct owner occupied & affordable in the northeast
dd.final.zip$zip.poss.reps.owner.aff.northeast[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &zip$pctowned.5yr.20122>=dd.final.zip$pctown[i]  &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$northeast==1] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable)




##more rep + lower density than current (for reps)
dd.final.zip$zip.poss.reps.density[i]<-sum(zip$housing[!is.na(zip$pctr082) & !is.na(zip$zipnew) & !is.na(zip$ba) &  !is.na(zip$ownocc.val.q252) & !is.na(zip$popdens.5yr.20122 ) & !is.na(zip$pctowned.5yr.20122) &  !is.na(zip$top25) & !is.na(zip$south) & !is.na(zip$northeast) & !is.na(zip$west) & !is.na(zip$midwest)   & dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$popdens.5yr.20122[i]>zip$popdens.5yr.20122  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)

##more rep + ed + affordable
dd.final.zip$zip.poss.reps.ed.aff[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]  ] , na.rm=TRUE)	##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable)



dd.final.zip$zip.poss.reps.ed.aff.msa[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i] & zip$top25==1  ] , na.rm=TRUE)	##current zip more republcian than possible zips (making possible zips more dem than current; 25th percentile houe affordable, in top 25 msa)

##more rep + ed + affordable + south
dd.final.zip$zip.poss.reps.ed.aff.south[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]  &zip$south==1] , na.rm=TRUE)		##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable; as high or higher on BA, in south)

##more rep + ed + affordable + northeast
dd.final.zip$zip.poss.reps.ed.aff.northeast[i]<-sum(zip$housing[ dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]   & zip$northeast==1] , na.rm=TRUE)		##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable; as high or higher on BA, in northeast)

##more rep + ed + affordable + west
dd.final.zip$zip.poss.reps.ed.aff.west[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]  & zip$west==1] , na.rm=TRUE)		##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable; as high or higher on BA, in west)

##more rep + ed + affordable + midwest
dd.final.zip$zip.poss.reps.ed.aff.midwest[i]<-sum(zip$housing[dd.final.zip$pctr082[i]<zip$pctr082 &dd.final.zip$faminc.house[i]>=zip$ownocc.val.q252 & zip$ba>=dd.final.zip$ba[i]  & zip$midwest==1] , na.rm=TRUE)##current zip less republcian than possible zips (making possible zips more rep than current; 25th percentile houe affordable; as high or higher on BA, in midwest)



}


}

	

table(nchar(dd.final.zip$zipnew))

dd.final.zip2<-dd.final.zip

dim(dd.final.zip2)
head(dd.final.zip2)






save(dd.final.zip2, file=paste(output,"move_poss_cces3.Rdata",sep=""))
load(paste(output,"move_poss_cces3.Rdata",sep=""))


##check for missingness on zipcode covariates
table(!is.na(zip$pctr082))

table(!is.na(zip$zipnew))

table(!is.na(zip$ba))

table(!is.na(zip$ownocc.val.q252))

table(!is.na(zip$popdens.5yr.20122 ))

table(!is.na(zip$pctowned.5yr.20122))

table(!is.na(zip$top25))

table(!is.na(zip$south))

table(!is.na(zip$northeast))

table(!is.na(zip$west))

table(!is.na(zip$midwest))


##about as many missings as members of other party
table(dd.final.zip2$dem)
table(is.na(dd.final.zip2$zip.poss.reps.density))

##about as many missings as members of other party
table(dd.final.zip2$rep)
table(is.na(dd.final.zip2$zip.poss.dems.northeast))


var.names<-c("zip.poss.dems",  "zip.poss.dems.msa", "zip.poss.dems.south", "zip.poss.dems.northeast", "zip.poss.dems.west", "zip.poss.dems.midwest", "zip.poss.dems.aff", "zip.poss.dems.owner", "zip.poss.dems.owner.aff", "zip.poss.dems.owner.aff.west", "zip.poss.dems.owner.aff.midwest", "zip.poss.dems.owner.aff.south", "zip.poss.dems.owner.aff.northeast",   
"zip.poss.dems.density",  "zip.poss.reps", "zip.poss.reps.msa", "zip.poss.reps.south", "zip.poss.reps.northeast", "zip.poss.reps.west", "zip.poss.reps.midwest", "zip.poss.reps.aff", "zip.poss.reps.owner", "zip.poss.reps.owner.aff", "zip.poss.reps.owner.aff.west", "zip.poss.reps.owner.aff.midwest", "zip.poss.reps.owner.aff.south", "zip.poss.reps.owner.aff.northeast","zip.poss.reps.density")
	
	
for(i in 1:length(var.names)){
	
	print(summary(dd.final.zip2[,var.names[i]]))
	
}


test<-NA

##take 1000 draws, see if all are successful
for(i in 1:1000){
draw<-sample(1:nrow(dd.final.zip2), size=1)

if(dd.final.zip2$rep[i]==1){
test[i]<-I(sum(zip$housing[ dd.final.zip2$pctr082[draw]<zip$pctr082  ] , na.rm=TRUE)==dd.final.zip2$zip.poss.reps[draw])
}

if(dd.final.zip2$dem[i]==1){
test[i]<-I(sum(zip$housing[ dd.final.zip2$pctr082[draw]>zip$pctr082  ] , na.rm=TRUE)==dd.final.zip2$zip.poss.dems[draw])
}


}
table(test) 

# # # ###end spot check


	
	total.housing<-as.numeric(sum(zip$housing[!is.na(zip$pctr082) & !is.na(zip$zipnew) & !is.na(zip$ba) &  !is.na(zip$ownocc.val.q252) & !is.na(zip$popdens.5yr.20122 ) & !is.na(zip$pctowned.5yr.20122) &  !is.na(zip$top25) & !is.na(zip$south) & !is.na(zip$northeast) & !is.na(zip$west) & !is.na(zip$midwest)]))

	
	


	
	for(i in 1:length(var.names)){
		
		dd.final.zip2[,paste(var.names[i],".prop",sep="")]<-dd.final.zip2[,var.names[i]] / total.housing
		
	}
	
	
plot.vars<-c("zip.poss.dems.prop"          ,         
 "zip.poss.dems.msa.prop"      ,          "zip.poss.dems.south.prop"      ,        "zip.poss.dems.northeast.prop"         ,
 "zip.poss.dems.west.prop"   ,            "zip.poss.dems.midwest.prop"       ,     "zip.poss.dems.aff.prop"            ,   
"zip.poss.dems.owner.prop"         ,   "zip.poss.dems.owner.aff.prop",  "zip.poss.dems.owner.aff.west.prop"  ,"zip.poss.dems.owner.aff.midwest.prop"  ,"zip.poss.dems.owner.aff.south.prop"  ,"zip.poss.dems.owner.aff.northeast.prop"  ,

 "zip.poss.dems.density.prop"       ,    
 "zip.poss.reps.prop"     ,         "zip.poss.reps.msa.prop"        ,        "zip.poss.reps.south.prop"     ,        
 "zip.poss.reps.northeast.prop"      ,    "zip.poss.reps.west.prop"        ,       "zip.poss.reps.midwest.prop"          , 
"zip.poss.reps.aff.prop"         ,       "zip.poss.reps.owner.prop"     ,   "zip.poss.reps.owner.aff.prop",  "zip.poss.reps.owner.aff.west.prop"  ,"zip.poss.reps.owner.aff.midwest.prop"  ,"zip.poss.reps.owner.aff.south.prop"  ,"zip.poss.reps.owner.aff.northeast.prop"  ,"zip.poss.reps.density.prop"         )


##get total number of partisans in sample
total.dems<-dim(dd.final.zip2[dd.final.zip2$dem==1 & !is.na(dd.final.zip2$zipnew)& !is.na(dd.final.zip2$faminc.house) & !is.na(dd.final.zip2$pctr082)  & !is.na(dd.final.zip2$popdens.5yr.20122) & !is.na(dd.final.zip2$pctown) &  !is.na(dd.final.zip2$top25),])[1]

total.reps<-dim(dd.final.zip2[dd.final.zip2$rep==1 & !is.na(dd.final.zip2$zipnew)& !is.na(dd.final.zip2$faminc.house) & !is.na(dd.final.zip2$pctr082) & !is.na(dd.final.zip2$popdens.5yr.20122) & !is.na(dd.final.zip2$pctown) &  !is.na(dd.final.zip2$top25),])[1]


bars<-paste("bar.",var.names,sep="")##make names for bar charts
denoms<-c(rep(total.dems, 16), rep(total.reps, 16)) ##make vector of denominators

plot.obs<-list(NA)


bins<-seq(0,1,.05)

##store proportions in a list for each variable

#dems
for(i in 1:(length(bars)/2)  )  {


	plot.obs[[i]]<-assign(bars[i], hist(dd.final.zip2[dd.final.zip2$dem==1,plot.vars[i]], breaks=bins, plot=F)$counts / total.dems)
}


#reps
for(i in ((length(bars)/2)+1): length(bars) )  {
	plot.obs[[i]]<-assign(bars[i], hist(dd.final.zip2[dd.final.zip2$rep==1,plot.vars[i]], breaks=bins, plot=F)$counts / total.reps)
}

names(plot.obs)<-bars




file.name<-paste(output, "dr.panel.cces",".pdf",sep="")
pdf(file.name, paper='special', height=5, width=7)
par(mfrow=c(2,4) , oma=c(1.2,1.2,0,0))

barplot(plot.obs$bar.zip.poss.dems, main="Sort as Dem."
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
  
barplot(plot.obs$bar.zip.poss.dems.aff, main= "Sort as Dem., Affordable"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
   
barplot(plot.obs$bar.zip.poss.dems.owner.aff, main= "Sort as Dem., Affordable
Preserve Quality"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)

barplot(plot.obs$bar.zip.poss.dems.owner.aff.west, main=  "Sort as Dem., Affordable, 
Preserve Quality, in West"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="lightskyblue", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
  
  
   barplot(plot.obs$bar.zip.poss.reps, main="Sort as Rep."
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)

barplot(plot.obs$bar.zip.poss.reps.aff, main= "Sort as Rep., Affordable"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
   
barplot(plot.obs$bar.zip.poss.reps.owner.aff, main= "Sort as Rep., Affordable
Preserve Quality"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)


barplot(plot.obs$bar.zip.poss.reps.owner.aff.west, main=  "Sort as Rep., Affordable, 
Preserve Quality, in West"
, ylim=c(0,1), axes=FALSE, xlab="", cex.main=.9, ylab="", col="red", space=0 , xlim=c(0,20) )
  axis(1, at=seq(0,20, 1), labels=F)
  axis(2, seq(0, 1, .05), labels=F)
  mtext(side=1,c("0",".25",".5",".75","1"), at=c(0,5,10,15,20), cex=.7, line=1)
  mtext(side=2,c("0",".25",".5",".75","1"), at=c(0,.25,.5,.75,1), cex=.7, line=1, las=2)
       mtext(side=1, at=.5, line=.001, text="Proportion of National Housing Stock Available", cex=.7, outer=TRUE)
       mtext(side=2, at=.25, line=.001, text="Proportion of Sample Republicans", cex=.7, outer=TRUE)
       mtext(side=2, at=.75, line=.001, text="Proportion of Sample Democrats", cex=.7, outer=TRUE)


dev.off()

        cat("End FEASABILITY ANALYSIS\n")
