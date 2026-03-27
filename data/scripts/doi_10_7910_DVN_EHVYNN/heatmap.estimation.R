################################
################################
################################
################################
####### HEAT MAP
################################
################################
################################
################################


cat("Begin HEAT MAP\n")

load("quickfire.data.final.Rdata")

pair.means.all<-quickfire.data.final
dim(pair.means.all)
pair.means.dems<-subset(pair.means.all, pair.means.all$partywlean=="dem")
dim(pair.means.dems)	
pair.means.reps<-subset(pair.means.all, pair.means.all$partywlean=="rep")
dim(pair.means.reps)	
pair.means.all[1,]

item.names<-c("Commute Time","Sidewalks","Parking Capacity","Amount Daily Driving","School Quality","Property Tax Rates","Crime","Gang Activity","Drug Users","Homeless","Police Quality","Judeo-Christian Neighbors","Distance to Church","Retiree Friendly","Atheist Friendly","Amount of Corruption","Gay Friendly","High-Density Housing","Low-Density Housing","Air Pollution","Access to Highways","Access to Bus/Rail","Neighbors Share Politics","Bike Friendly","Neighbors Educated","Walk to Shopping","Walk to Work/School","Home Values","Home Prices","White Community","Black Community","Asian Community","Hispanic Community","Christian Community","Jewish Community","Muslim Community","Street Life","Restaurants","Privacy","Neighbors Share Religious Values","Kid Friendly","Number of Retirees","Poor Community","Warm Climate","Cold Climate","Bus Child to School","Business Friendly","Government Employees","Wealthy Community","Big Houses/Yards","Friends Nearby","Family Nearby","Lot in Common w/ Neighbors","Republican Community","Democratic Community","Local Sales Tax","Low-Income Services","Public Transit Quality","Road Quality","Parks Quality","Prefer Not to Answer","Region of Country","Close to Major Metro Area")




##make category vectors
disorder<-c("Crime","Gang Activity","Drug Users","Homeless","Air Pollution")
geo<-c("Region of Country","Close to Major Metro Area", "Warm Climate","Cold Climate" )
fam<-c("Family Nearby", "Friends Nearby")
neigh.inc<-c("Home Values","Home Prices","Poor Community","Wealthy Community")
gov<-c("Property Tax Rates","Police Quality","Parks Quality","Local Sales Tax","Business Friendly","Government Employees","Amount of Corruption","Low-Income Services")
trans<-c("Amount Daily Driving","Road Quality","Commute Time","Access to Highways","Bike Friendly","Public Transit Quality","Access to Bus/Rail")
growth.sprawl<-c("Privacy","Big Houses/Yards","Low-Density Housing","High-Density Housing","Walk to Shopping","Walk to Work/School","Parking Capacity","Sidewalks")
children<-c("School Quality","Kid Friendly","Bus Child to School")
social<-c("Restaurants","Retiree Friendly","Number of Retirees","Street Life")
beliefs<-c("Neighbors Share Religious Values","Lot in Common w/ Neighbors","Judeo-Christian Neighbors","Distance to Church","Christian Community","Jewish Community","Muslim Community","Neighbors Share Politics","Republican Community","Democratic Community","Neighbors Educated","Gay Friendly","Atheist Friendly")
race<-c("White Community","Black Community","Asian Community","Hispanic Community")
prefer.not<-c("Prefer Not to Answer")

cats<-list(disorder, geo, fam, neigh.inc, gov, trans, growth.sprawl, children, social, beliefs, race, prefer.not)
names(cats)<-c("disorder","geo","fam","neigh.inc","gov","trans","growth.sprawl","children","social","beliefs","race","prefer.not")
cats


catnames<-names(cats)
dd.pairs<-list(NA)


##list containing the three data frames
pair.data<-list(pair.means.all, pair.means.dems, pair.means.reps)





for(g in 1:length(pair.data)){
	
	colnames(pair.data[[g]])<-c(item.names, "partywlean","white","black","female","age","education","income","libdem","conrep","rich","old","ownrent","educ","homelast5","kidshome","city","black.therm","blakc.hisp.therm","white.black.diff","young1","young2","young3","respid")


pair.means.all.3<-pair.data[[g]][,item.names]
dim(pair.means.all.3)
pair.means.all.3[1,]

##replace all NAs with 0s, 0's with 1s and 1s with 2s to ease computation

pair.means.all.3[pair.means.all.3==1]<-2
pair.means.all.3[pair.means.all.3==0]<-1
pair.means.all.3[is.na(pair.means.all.3)]<-0
pair.means.all.3[1,]


##make a data frame for each category, store in a list
for(i in 1:len(cats)){
dd.pairs[[i]]<-as.data.frame(pair.means.all.3[,cats[[i]]])
	}
names(dd.pairs)<-catnames



##spotcheck
dim(dd.pairs[[1]])
head(dd.pairs[[1]])
dim(dd.pairs[[4]])
head(dd.pairs[[4]])
head(dd.pairs$prefer.not)

head(dd.pairs$race)
dim(dd.pairs$race)
len(dd.pairs)


##take row sums for all but the prefer.not vector, store in a vector in each data frame
for(i in 1:len(dd.pairs)){
	
if(i!=len(dd.pairs)){dd.pairs[[i]]$sum<-apply(dd.pairs[[i]], 1, sum)}

}

head(dd.pairs$disorder)
head(dd.pairs$race)
head(dd.pairs$prefer.not)


##if sum=0 the category was not involved in a faceoff for that observation. if sum=1, the row had a trait that lost to something in abother category. if sum=2, the row had a trait that beat something in another category. if sum=3 the faceoff occurred within the category. 


##make an empty list
sums<-list(NA)

###deposit each vector of sums in an element of a list called "sums"
for(i in 1:len(dd.pairs)){
if(i!=len(dd.pairs)){sums[[i]]<-dd.pairs[[i]]$sum}
}


##combine all the sums data
sum.dta<-cbind.data.frame(sums, dd.pairs$prefer.not )##cbind.data.frame works on lists
names(sum.dta)<-names(cats)
head(sum.dta)
dim(sum.dta)



##make matrix to store results of face-offs

results.faceoff<-matrix(nrow=len(cats), ncol=len(cats))
colnames(results.faceoff)<-names(cats)
rownames(results.faceoff)<-names(cats)
results.faceoff[1:nrow(results.faceoff),1:ncol(results.faceoff)]<-0
results.faceoff

results.faceoff2<-matrix(nrow=len(cats), ncol=len(cats))
colnames(results.faceoff2)<-names(cats)
rownames(results.faceoff2)<-names(cats)
results.faceoff2[1:nrow(results.faceoff2),1:ncol(results.faceoff2)]<-0
results.faceoff2

##tabulate up faceoff results as raw numbers

catnames<-names(cats)


###tabulate results
for(j in 1:(ncol(sum.dta) -1) ){##iterate over everything but the prefer.not column; that will be added in by name below
	
   for(k in 1:(ncol(sum.dta) -1) ){

##make smaller data set of one matchup
pair<-cbind.data.frame(sum.dta[,j],sum.dta[,k], sum.dta$prefer.not )
names(pair)<-c(catnames[j], catnames[k], "prefer.not")
pair<-subset(pair, (pair[,1]!=0 & pair[,2]!=0 & pair$prefer.not!=0)==TRUE & pair[,1]!=3 & pair[,2]!=3)###remove observations where category not involved in a faceoff or where it faced off against something in its own category
pair<-na.omit(pair)

      for(i in 1:nrow(pair)){
	
if( pair[i,1]==2 & pair[i,2]==1 & pair$prefer.not[i]==1  & j!=k ) {
results.faceoff[catnames[j], catnames[k]]<-results.faceoff[catnames[j], catnames[k]]+1
##results where rows in results matrix are winners
}



}
}}

results.faceoff<-results.faceoff[-nrow(results.faceoff),-ncol(results.faceoff)] ##we can get rid of the prefer.not column/row. it's not really comparable since it is in every choice. but we will want to include it in the denominator in the proportions

results.faceoff


results.prefer.not<-matrix(nrow=len(cats)-1, ncol=len(cats)-1)
colnames(results.prefer.not)<-names(cats)[1:(len(cats)-1)]
rownames(results.prefer.not)<-names(cats)[1:(len(cats)-1)]
results.prefer.not[1:nrow(results.prefer.not),1:ncol(results.prefer.not)]<-0
results.prefer.not


###count up number of times prefer.not wins for every combo of two categories that face off
###tabulate results
for(j in 1:(ncol(sum.dta) -1) ){##iterate over everything but the prefer.not column; that will be added in by name below
	
for(k in 1:(ncol(sum.dta) -1) ){

##make smaller data set of one matchup
pair<-cbind.data.frame(sum.dta[,j],sum.dta[,k], sum.dta$prefer.not )
names(pair)<-c(catnames[j], catnames[k], "prefer.not")
pair<-subset(pair, (pair[,1]!=0 & pair[,2]!=0 & pair$prefer.not!=0)==TRUE & pair[,1]!=3 & pair[,2]!=3)
pair<-na.omit(pair)

for(i in 1:nrow(pair)){
	
if( pair[i,1]==1 & pair[i,2]==1 & pair$prefer.not[i]==2 & j!=k ) {
results.prefer.not[catnames[j], catnames[k]]<-results.prefer.not[catnames[j], catnames[k]]+1
##results where rows in results matrix are number of times prefer.not won for that matchup of categories
}

}}}
results.prefer.not
isSymmetric(as.matrix(results.prefer.not))##this is a symmetric matrix

results.faceoff3<-matrix(nrow=len(cats)-1, ncol=len(cats)-1)
colnames(results.faceoff3)<-names(cats)[1:(len(cats)-1)]
rownames(results.faceoff3)<-names(cats)[1:(len(cats)-1)]
results.faceoff3[1:nrow(results.faceoff3),1:ncol(results.faceoff3)]<-0
results.faceoff3




##incorporate the denominator for each faceoff and turn into proportions
for(j in 1: ncol(results.faceoff3)){
for(i in 1: nrow(results.faceoff3)){

results.faceoff3[catnames[i],catnames[j]]<-results.faceoff[i,j] / (results.faceoff[i,j] + results.faceoff[j,i] + results.prefer.not[i,j]) ##proportion of times the row category wins over the column including the prefer not observations


}}

diag(results.faceoff3)<-0##replace NaN's with 0's on the diag

results.faceoff
results.prefer.not
results.faceoff3##this one has the proportion of times the row beats the column

xtable(results.faceoff3, digits=2)##generate table of proportions for latex


##round off to nearest 10th
results.faceoff3.r<-round(results.faceoff3,1)
results.faceoff3.r


barnames<-c("Disorder","Geography","Family/Friends","Neighborhood Income","Government Services","Transportation","Growth v. Sprawl","Children","Social Life","Neighborhood Beliefs/Values","Neighborhood Race")


my_palette <- colorRampPalette(c("white","gray90", "gray30"))(n = 299)

colors<-rev(heat.colors(256))


results.faceoff4<-results.faceoff3.r
colnames(results.faceoff4)<-rep("",len(rownames(results.faceoff3.r)))
results.faceoff4



##GENERATE FIGURE 4 in manuscript
file.names<-paste(output, c("quickfire.heatmap.pdf", "quickfire.heatmap.dems.pdf", "quickfire.heatmap.reps.pdf",sep=""))

titles<-c("Proportion Won by Row Category 
When Facing Column Category","Proportion Won by Row Category 
When Facing Column Category
(Democrats)","Proportion Won by Row Category 
When Facing Column Category
(Republicans)")

pdf(file=file.names[g])

par(cex.main=0.75)
heatmap.2(results.faceoff3.r,Rowv=NA, Colv=NA,  col =my_palette , scale="none", margins=c(15.6,14), symm=TRUE, main=titles[g],cellnote=results.faceoff3.r, notecol="black",trace="none",density.info="none", labRow= barnames, labCol=barnames)

dev.off()



}

cat("End HEAT MAP\n")
